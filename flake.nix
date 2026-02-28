{
  description = "kli — event-sourced task graphs for Claude Code";

  inputs = {
    # Pinned to same nixpkgs as core depot — SBCL 2.5.7
    # (SBCL 2.6.0 from newer nixos-unstable breaks fare-quasiquote-readtable)
    nixpkgs.url = "github:NixOS/nixpkgs/88d3861acdd3d2f0e361767018218e51810df8a1";
    cl-deps.url = "github:kleisli-io/cl-deps";
    cl-deps.inputs.nixpkgs.follows = "nixpkgs";
    lol-reactive.url = "github:kleisli-io/lol-reactive";
    lol-reactive.inputs.nixpkgs.follows = "nixpkgs";
    lol-reactive.inputs.cl-deps.follows = "cl-deps";
  };

  outputs = { nixpkgs, cl-deps, lol-reactive, ... }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          inherit (cl-deps.lib.${system}) buildLisp lisp;

          # --- kli internal libraries ---

          kli-crdt = buildLisp.library {
            name = "kli-crdt";
            deps = [ lisp.alexandria ];
            srcs = map (f: ./lib/crdt + "/${f}") [
              "package.lisp"
              "vector-clock.lisp"
              "g-set.lisp"
              "lww-register.lisp"
              "or-set.lisp"
              "lww-map.lisp"
            ];
          };

          kli-task = buildLisp.library {
            name = "kli-task";
            deps = [ kli-crdt lisp.alexandria lisp.bordeaux-threads lisp.cl-ppcre lisp.yason lisp.uiop ];
            srcs = map (f: ./lib/task + "/${f}") [
              "package.lisp"
              "validation.lisp"
              "event.lisp"
              "log.lisp"
              "paths.lisp"
              "state.lisp"
              "graph.lisp"
              "query.lisp"
              "markov.lisp"
            ];
          };

          kli-depot = buildLisp.library {
            name = "depot";
            deps = [ lisp.uiop ];
            srcs = [
              ./lib/depot/package.lisp
              ./lib/depot/discovery.lisp
            ];
          };

          kli-playbook = buildLisp.library {
            name = "kli-playbook";
            deps = [ lisp.uiop ];
            srcs = [ ./lib/playbook/query.lisp ];
          };

          # --- Hook support libraries ---

          kli-claude-hooks = buildLisp.library {
            name = "claude-hooks";
            deps = [ lisp.yason lisp.uiop ];
            srcs = map (f: ./lib/claude-hooks + "/${f}") [
              "package.lisp"
              "src/json.lisp"
              "src/responses.lisp"
              "src/depot.lisp"
              "src/paths.lisp"
              "src/file-io.lisp"
              "src/prompts.lisp"
              "src/hook.lisp"
              "src/testing.lisp"
            ];
          };

          kli-claude-session = buildLisp.library {
            name = "claude-session";
            deps = [ lisp.yason lisp.uiop ];
            srcs = map (f: ./lib/claude-session + "/${f}") [
              "package.lisp"
              "session.lisp"
            ];
          };

          kli-playbook-hooks = buildLisp.library {
            name = "playbook-hooks";
            deps = [ kli-claude-hooks lisp.yason ];
            srcs = map (f: ./lib/playbook-hooks + "/${f}") [
              "package.lisp"
              "src/domains.lisp"
              "src/co-app.lisp"
              "src/session-io.lisp"
            ];
          };

          kli-mcp-framework = buildLisp.library {
            name = "kli-mcp-framework";
            deps = [
              lisp.alexandria lisp.bordeaux-threads lisp.let-over-lambda
              lisp.yason lisp.cl-ppcre
            ];
            srcs = map (f: ./lib/mcp-framework/src + "/${f}") [
              "package.lisp"
              "conditions.lisp"
              "content.lisp"
              "schema.lisp"
              "protocol.lisp"
              "tools/core.lisp"
              "tools/macros.lisp"
              "hooks.lisp"
              "resources.lisp"
              "prompts.lisp"
              "evolution.lisp"
              "transport/protocol.lisp"
              "transport/stdio.lisp"
              "server.lisp"
            ];
          };

          kli-mcp-http = buildLisp.library {
            name = "kli-mcp-http";
            deps = [
              kli-mcp-framework lisp.hunchentoot
              lisp.bordeaux-threads lisp.yason
            ];
            srcs = map (f: ./lib/mcp-http/src + "/${f}") [
              "package.lisp"
              "session.lisp"
              "sse.lisp"
              "transport.lisp"
              "handler.lisp"
            ];
          };

          # --- Web framework (flake input) ---

          kli-lol-reactive = lol-reactive.lib.${system}.library;

          # --- Service modules ---

          # playbook-mcp-lib: the library (no server.lisp) so task-mcp can reference session symbols
          playbook-mcp-lib = buildLisp.library {
            name = "playbook-mcp";
            deps = [
              kli-playbook kli-task kli-mcp-framework kli-mcp-http
              lisp.alexandria lisp.bordeaux-threads lisp.yason
              lisp.cl-ppcre lisp.hunchentoot lisp.dexador lisp.uiop
            ];
            srcs = map (f: ./services/playbook-mcp/lib + "/${f}") [
              "package.lisp"
              "locking.lisp"
              "pattern.lisp"
              "store.lisp"
              "edges.lisp"
              "file-sync.lisp"
              "graph.lisp"
              "activation.lisp"
              "session.lisp"
              "parser.lisp"
              "search.lisp"
              "embeddings.lisp"
              "cleanup.lisp"
            ];
          };

          task-mcp-service = buildLisp.library {
            name = "task-mcp";
            deps = [
              kli-task kli-crdt kli-depot kli-mcp-framework kli-mcp-http
              playbook-mcp-lib
              lisp.alexandria lisp.bordeaux-threads lisp.yason
              lisp.cl-ppcre lisp.hunchentoot lisp.dexador lisp.uiop
            ];
            srcs = map (f: ./services/task-mcp + "/${f}") [
              "package.lisp"
              "embeddings.lisp"
              "obs-index.lisp"
              "retrieval.lisp"
              "session.lisp"
              "format.lisp"
              "macros.lisp"
              "graph.lisp"
              "query.lisp"
              "tools.lisp"
              "server.lisp"
            ];
          };

          playbook-mcp-service = buildLisp.library {
            name = "playbook-mcp-server";
            deps = [
              playbook-mcp-lib
              kli-mcp-framework kli-mcp-http
              lisp.hunchentoot lisp.yason lisp.dexador
            ];
            srcs = map (f: ./services/playbook-mcp/server + "/${f}") [
              "package.lisp"
              "server.lisp"
              "tools.lisp"
              "query.lisp"
              "resources.lisp"
            ];
          };

          dashboard-service = buildLisp.library {
            name = "kli-dashboard";
            deps = [ kli-lol-reactive kli-task lisp.cl-who lisp.parenscript lisp.yason ];
            srcs = map (f: ./services/dashboard + "/${f}") [
              "package.lisp"
              # CSS modules (tokens first — defines design system)
              "css/tokens.lisp"
              "css/base.lisp"
              "css/frontier.lisp"
              "css/health.lisp"
              "css/activity.lisp"
              "css/stats.lisp"
              "css/sessions.lisp"
              "css/task-detail.lisp"
              "css/plan.lisp"
              "css/graph.lisp"
              # Data layer (before components — provides data functions)
              "data/local.lisp"
              # Components
              "components/task-card.lisp"
              "components/nav.lisp"
              "components/metrics.lisp"
              "components/sections.lisp"
              # Scripts (before pages)
              "scripts/reveal.lisp"
              "scripts/activity.lisp"
              "scripts/graph.lisp"
              # Pages
              "pages/frontier.lisp"
              "pages/health.lisp"
              "pages/activity.lisp"
              "pages/task-detail.lisp"
              "pages/plan.lisp"
              "pages/stats.lisp"
              "pages/sessions.lisp"
              "pages/graph.lisp"
              # Routes and server (must be after pages)
              "routes.lisp"
              "server.lisp"
            ];
          };

          # --- kli binary (unwrapped) ---

          kli-binary = buildLisp.program {
            name = "kli";
            main = "kli:main";
            deps = [
              task-mcp-service
              playbook-mcp-service
              dashboard-service
              kli-mcp-framework
              kli-mcp-http
              kli-claude-hooks
              kli-claude-session
              kli-playbook-hooks
              kli-depot
              lisp.alexandria
              lisp.bordeaux-threads
              lisp.cl-ppcre
              lisp.dexador
              lisp.hunchentoot
              lisp.let-over-lambda
              lisp.usocket
              lisp.yason
              lisp.uiop
              lisp.ironclad
              lisp.swank
            ];
            srcs =
              (map (f: ./hooks + "/${f}") [
                "package.lisp"
                "config.lisp"
                "session-start.lisp"
                "session-leave.lisp"
                "tool-call.lisp"
                "session-task-write.lisp"
                "file-conflict.lisp"
                "playbook-activate.lisp"
                "feedback-nudge.lisp"
                "task-complete-reflect.lisp"
                "dispatch.lisp"
              ]) ++ [
              ./src/package.lisp
              ./src/init.lisp
              ./src/update.lisp
              ./src/main.lisp
            ];
            tests = {
              name = "kli-tests";
              srcs =
                (map (f: ./lib/crdt/t + "/${f}") [ "package.lisp" "tests.lisp" ]) ++
                (map (f: ./lib/task/t + "/${f}") [ "package.lisp" "tests.lisp" "test-query.lisp" "test-markov.lisp" ]) ++
                (map (f: ./lib/claude-hooks/t + "/${f}") [ "package.lisp" "suite.lisp" "test-json.lisp" "test-responses.lisp" "test-paths.lisp" "test-file-io.lisp" "test-prompts.lisp" "test-hook.lisp" ]) ++
                (map (f: ./lib/playbook-hooks/t + "/${f}") [ "package.lisp" "suite.lisp" "test-domains.lisp" "test-co-app.lisp" "test-session-io.lisp" ]) ++
                (map (f: ./services/playbook-mcp/t + "/${f}") [ "package.lisp" "suite.lisp" "test-session-state.lisp" "test-http-transport.lisp" "test-session-discovery.lisp" "test-feedback-state.lisp" "test-relevance-feedback.lisp" ]) ++
                (map (f: ./hooks/t + "/${f}") [ "package.lisp" "suite.lisp" "test-task-complete-reflect.lisp" ]);
              deps = [ lisp.fiveam lisp.bordeaux-threads ];
              expression = "(and (fiveam:run! :crdt-tests) (fiveam:run! :task-tests) (claude-hooks.tests:run-all-tests) (playbook-hooks.tests:run-all-tests) (playbook-mcp.tests:run-all-tests) (kli-hook.tests:run-all-tests))";
            };
          };

          # --- kli wrapped with plugin data ---

          kli = pkgs.runCommand "kli" {
            nativeBuildInputs = [ pkgs.makeWrapper ];
          } ''
            mkdir -p $out/bin $out/share/kli
            cp -rT ${./plugin} $out/share/kli/
            makeWrapper ${kli-binary}/bin/kli $out/bin/kli \
              --set KLI_DATA_DIR $out/share/kli \
              --set NIX_BUILDLISP_LISP_ARGS "--dynamic-space-size 4096"
          '';

        in {
          inherit kli-binary;
          default = kli;
        });
    };
}
