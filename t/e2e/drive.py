#!/usr/bin/env python3
# Two-process witness: `kli install` (process 1) then a fresh `kli mcp-serve`
# (process 2) that rediscovers the placed extension from disk and serves it.
# Distinct processes force a fresh recompile, so a tool folding a value across
# ordered component files proves an ordered cross-file load. Pins are computed
# with real git; each scenario gets its own XDG root, shared only between its
# two processes.
import argparse
import base64
import json
import os
import subprocess
import tempfile
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer


def assert_eq(got, want, msg):
    if got != want:
        raise AssertionError(f"{msg}: expected {want!r}, got {got!r}")


def read_tree(root):
    files = {}
    for dirpath, _dirs, names in os.walk(root):
        for name in names:
            path = os.path.join(dirpath, name)
            rel = os.path.relpath(path, root).replace(os.sep, "/")
            with open(path, "rb") as f:
                files[rel] = f.read()
    return files


def bundle_bytes(files):
    obj = {
        "format": "kli-dir-bundle-v1",
        "files": {rel: base64.b64encode(data).decode("ascii")
                  for rel, data in files.items()},
    }
    return json.dumps(obj).encode("utf-8")


def _git_env(home):
    env = dict(os.environ)
    env["HOME"] = home
    env["GIT_CONFIG_NOSYSTEM"] = "1"
    env["GIT_CONFIG_GLOBAL"] = "/dev/null"
    env["GIT_CONFIG_SYSTEM"] = "/dev/null"
    return env


def _git(args, cwd, env):
    r = subprocess.run(["git", "-c", "core.autocrlf=false", *args],
                       cwd=cwd, env=env, capture_output=True, text=True)
    if r.returncode != 0:
        raise RuntimeError(f"git {args} failed: {r.stderr}")
    return r.stdout


def git_tree_sha(files):
    with tempfile.TemporaryDirectory() as d:
        for rel, data in files.items():
            path = os.path.join(d, *rel.split("/"))
            os.makedirs(os.path.dirname(path), exist_ok=True)
            with open(path, "wb") as f:
                f.write(data)
        env = _git_env(d)
        _git(["init", "-q"], cwd=d, env=env)
        _git(["add", "-A"], cwd=d, env=env)
        return _git(["write-tree"], cwd=d, env=env).strip()


def git_blob_sha(data):
    r = subprocess.run(["git", "hash-object", "--stdin"],
                       input=data, capture_output=True)
    if r.returncode != 0:
        raise RuntimeError(f"git hash-object failed: {r.stderr!r}")
    return r.stdout.decode().strip()


class HttpOrigin:
    def __init__(self, files):
        handler = self._handler(files)
        self.httpd = ThreadingHTTPServer(("127.0.0.1", 0), handler)
        self.port = self.httpd.server_address[1]
        self.thread = threading.Thread(target=self.httpd.serve_forever, daemon=True)
        self.thread.start()

    def url(self, name):
        return f"http://127.0.0.1:{self.port}/{name}"

    @staticmethod
    def _handler(files):
        class H(BaseHTTPRequestHandler):
            protocol_version = "HTTP/1.1"

            def log_message(self, *_a):
                pass

            def do_GET(self):
                body = files.get(self.path.lstrip("/"))
                if body is None:
                    self.send_response(404)
                    self.send_header("Content-Length", "0")
                    self.end_headers()
                    return
                self.send_response(200)
                self.send_header("Content-Type", "application/octet-stream")
                self.send_header("Content-Length", str(len(body)))
                self.end_headers()
                self.wfile.write(body)
        return H

    def stop(self):
        self.httpd.shutdown()


def _result_text(result):
    parts = []
    for block in result.get("content", []) or []:
        if isinstance(block, dict) and block.get("type") == "text":
            parts.append(block.get("text", ""))
    return "\n".join(parts)


class McpServer:
    """A `kli mcp-serve <id>` process, driven over newline-delimited JSON-RPC 2.0
    (initialize -> notifications/initialized -> tools/list|tools/call)."""

    def __init__(self, kli, ext_id, env, cwd):
        self.proc = subprocess.Popen(
            [kli, "mcp-serve", ext_id], cwd=cwd, env=env,
            stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=subprocess.PIPE, text=True, bufsize=1)
        self._id = 0
        self._err = []
        threading.Thread(target=self._drain, daemon=True).start()

    def _drain(self):
        for line in self.proc.stderr:
            self._err.append(line)

    def stderr(self):
        return "".join(self._err)

    def _send(self, obj):
        self.proc.stdin.write(json.dumps(obj) + "\n")
        self.proc.stdin.flush()

    def _rpc(self, method, params=None):
        self._id += 1
        rid = self._id
        msg = {"jsonrpc": "2.0", "id": rid, "method": method}
        if params is not None:
            msg["params"] = params
        self._send(msg)
        while True:
            line = self.proc.stdout.readline()
            if not line:
                raise RuntimeError(
                    f"server closed during {method} (exit {self.proc.poll()})\n"
                    f"stderr:\n{self.stderr()}")
            m = json.loads(line)
            if m.get("id") == rid:
                if "error" in m:
                    raise RuntimeError(f"jsonrpc error on {method}: {m['error']}")
                return m.get("result")

    def _notify(self, method):
        self._send({"jsonrpc": "2.0", "method": method})

    def initialize(self):
        self._rpc("initialize", {
            "protocolVersion": "2025-11-25",
            "capabilities": {},
            "clientInfo": {"name": "e2e", "version": "0"},
        })
        self._notify("notifications/initialized")

    def tool_names(self):
        return [t["name"] for t in self._rpc("tools/list").get("tools", [])]

    def call_text(self, name, arguments=None):
        result = self._rpc("tools/call", {"name": name, "arguments": arguments or {}})
        if result.get("isError"):
            raise RuntimeError(f"tool {name} isError: {_result_text(result)}")
        return _result_text(result)

    def close(self):
        try:
            self.proc.stdin.close()
        except OSError:
            pass
        try:
            self.proc.wait(timeout=60)
        except subprocess.TimeoutExpired:
            self.proc.kill()


def scenario_env(base, name):
    root = os.path.join(base, name)
    dirs = {k: os.path.join(root, k)
            for k in ("home", "config", "cache", "data", "state")}
    for d in dirs.values():
        os.makedirs(d, exist_ok=True)
    env = dict(os.environ)
    env.update({
        "HOME": dirs["home"],
        "XDG_CONFIG_HOME": dirs["config"],
        "XDG_CACHE_HOME": dirs["cache"],
        "XDG_DATA_HOME": dirs["data"],
        "XDG_STATE_HOME": dirs["state"],
    })
    return env, dirs["config"], dirs["home"]


def run_install(kli, env, cwd, url, sha, yes=True, timeout=300):
    argv = [kli, "install", url, sha] + (["--yes"] if yes else [])
    return subprocess.run(argv, cwd=cwd, env=env, stdin=subprocess.DEVNULL,
                          capture_output=True, text=True, timeout=timeout)


def installed_dir(config, ext_id):
    return os.path.join(config, "kli", "extensions", ext_id)


def scenario_two_process(kli, origin, pins, tmp):
    env, _config, cwd = scenario_env(tmp, "two-process")
    r = run_install(kli, env, cwd, origin.url("dir.bundle"), pins["dir"])
    assert_eq(r.returncode, 0, f"install exit (stderr: {r.stderr})")
    ext_id = r.stdout.strip()
    assert_eq(ext_id, "e2e-dir", f"declared id on stdout (stderr: {r.stderr})")

    server = McpServer(kli, ext_id, env, cwd)
    try:
        server.initialize()
        names = server.tool_names()
        assert names, f"empty tool surface\nstderr:\n{server.stderr()}"
        assert_eq(server.call_text(names[0]), "42",
                  "cross-file ordered value across processes")
    finally:
        server.close()
    print("ok: two-process ordered cross-file serve")


def scenario_single_file(kli, origin, pins, tmp):
    env, _config, cwd = scenario_env(tmp, "single")
    r = run_install(kli, env, cwd, origin.url("solo.lisp"), pins["solo"])
    assert_eq(r.returncode, 0, f"single install exit (stderr: {r.stderr})")
    ext_id = r.stdout.strip()
    assert_eq(ext_id, "e2e-solo", "single-file declared id")

    server = McpServer(kli, ext_id, env, cwd)
    try:
        server.initialize()
        names = server.tool_names()
        assert names, f"single: empty surface\nstderr:\n{server.stderr()}"
        assert_eq(server.call_text(names[0]), "solo-ok", "single-file tool value")
    finally:
        server.close()
    print("ok: single-file install serves")


def scenario_wrong_sha(kli, origin, pins, tmp):
    env, config, cwd = scenario_env(tmp, "wrong-sha")
    r = run_install(kli, env, cwd, origin.url("dir.bundle"), "0" * 40)
    assert_eq(r.returncode, 3, f"wrong-sha exit (stderr: {r.stderr})")
    assert not os.path.isdir(installed_dir(config, "e2e-dir")), \
        "a wrong sha placed nothing"
    q = subprocess.run([kli, "mcp-serve", "e2e-dir"], cwd=cwd, env=env,
                       stdin=subprocess.DEVNULL, capture_output=True,
                       text=True, timeout=300)
    assert_eq(q.returncode, 3, f"serve exits 3 when nothing installed (stderr: {q.stderr})")
    print("ok: wrong sha rejected, nothing served")


def scenario_non_tty_refuses(kli, origin, pins, tmp):
    env, config, cwd = scenario_env(tmp, "non-tty")
    try:
        r = run_install(kli, env, cwd, origin.url("dir.bundle"), pins["dir"],
                        yes=False, timeout=120)
    except subprocess.TimeoutExpired:
        raise AssertionError("install without --yes hung on a non-tty")
    assert_eq(r.returncode, 3, f"non-tty refusal exit (stderr: {r.stderr})")
    assert not os.path.isdir(installed_dir(config, "e2e-dir")), \
        "a refusal placed nothing"
    print("ok: non-tty without --yes refuses, no hang")


def scenario_reinstall_wholesale(kli, origin, pins, tmp):
    env, config, cwd = scenario_env(tmp, "reinstall")
    ghost = os.path.join(installed_dir(config, "e2e-dir"), "dropped.lisp")

    r1 = run_install(kli, env, cwd, origin.url("dir-v1.bundle"), pins["dir_v1"])
    assert_eq(r1.returncode, 0, f"v1 install exit (stderr: {r1.stderr})")
    assert os.path.isfile(ghost), "v1 placed the extra file"

    r2 = run_install(kli, env, cwd, origin.url("dir.bundle"), pins["dir"])
    assert_eq(r2.returncode, 0, f"reinstall exit (stderr: {r2.stderr})")
    assert not os.path.exists(ghost), "reinstall left a ghost file"

    server = McpServer(kli, "e2e-dir", env, cwd)
    try:
        server.initialize()
        assert_eq(server.call_text(server.tool_names()[0]), "42",
                  "reinstalled extension still serves")
    finally:
        server.close()
    print("ok: reinstall replaces wholesale, no ghost")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--kli", required=True)
    ap.add_argument("--fixture", required=True)
    ap.add_argument("--single", required=True)
    args = ap.parse_args()

    dir_files = read_tree(args.fixture)
    v1_files = dict(dir_files)
    v1_files["dropped.lisp"] = b";;; Extra file present in v1, dropped by v2.\n"
    with open(args.single, "rb") as f:
        solo_bytes = f.read()

    origin_files = {
        "dir.bundle": bundle_bytes(dir_files),
        "dir-v1.bundle": bundle_bytes(v1_files),
        "solo.lisp": solo_bytes,
    }
    pins = {
        "dir": git_tree_sha(dir_files),
        "dir_v1": git_tree_sha(v1_files),
        "solo": git_blob_sha(solo_bytes),
    }

    with tempfile.TemporaryDirectory() as tmp:
        origin = HttpOrigin(origin_files)
        try:
            scenario_two_process(args.kli, origin, pins, tmp)
            scenario_single_file(args.kli, origin, pins, tmp)
            scenario_wrong_sha(args.kli, origin, pins, tmp)
            scenario_non_tty_refuses(args.kli, origin, pins, tmp)
            scenario_reinstall_wholesale(args.kli, origin, pins, tmp)
        finally:
            origin.stop()
    print("all e2e scenarios passed")


if __name__ == "__main__":
    main()
