#!/usr/bin/env python3
"""Serve the current directory over HTTPS for the install gate.

The installer fetches over HTTPS; serving the freshly built artifact from a
local origin lets install.sh's hardened curl path run verbatim. Usage:

    serve-https.py PORT CERTFILE KEYFILE
"""
import http.server
import ssl
import sys

port = int(sys.argv[1])
certfile = sys.argv[2]
keyfile = sys.argv[3]

class Server(http.server.HTTPServer):
    # A client that rejects the server cert (the update gate's fail-closed leg)
    # tears the connection down mid-handshake; that is the tested behavior, not a
    # server fault, so don't dump a traceback for it.
    def handle_error(self, request, client_address):
        exc = sys.exc_info()[1]
        if isinstance(exc, (ConnectionError, ssl.SSLError)):
            return
        super().handle_error(request, client_address)


httpd = Server(("localhost", port), http.server.SimpleHTTPRequestHandler)
ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
ctx.load_cert_chain(certfile, keyfile)
httpd.socket = ctx.wrap_socket(httpd.socket, server_side=True)
httpd.serve_forever()
