#!/usr/bin/env python3
"""Deterministic fake store.v1.ShimStore Connect server for the doctor harness.

Speaks just enough Connect-over-HTTP to exercise every branch of
agent-shim-doctor.sh's store probe, on a UNIX domain socket, with NO
dependency on the real shim-store binary or on any Go build. One scripted
response mode per process.

    fake-store-fixture.py SOCKET MODE READY_FIFO CALL_LOG SLOW_SECONDS

MODE is one of:
    healthy         HTTP 200, {"success":{}}          (an empty success arm)
    failure         HTTP 200, {"failure":{"detail"…}} (the store refuses the read)
    scoped_refusal  HTTP 200, {"failure":{"detail"…,"invalidRequest":{"field":"session"}}}
                    (the real GetLiveWork refusal for an unscoped request —
                    see shim-store/AGENTS.md "GetLiveWork IS SCOPED TO ONE
                    SESSION"; this is the doctor's expected HEALTHY answer)
    non200          HTTP 503 with a Connect error body
    malformed       HTTP 200 with a body that is not JSON
    slow            HTTP 200 {"success":{}} after SLOW_SECONDS, to trip --max-time

READY_FIFO is the readiness latch: the server opens it for writing only after
bind+listen has succeeded, so the harness's blocking read returns exactly when
the socket is accepting and never races a duration-based startup guess.

CALL_LOG accumulates one TAB-separated line per received request:
    <path>\t<X-Agent-Repl-Request-Id>\t<body>
"""

import os
import socketserver
import sys
import time
from http.server import BaseHTTPRequestHandler

SOCKET, MODE, READY_FIFO, CALL_LOG = sys.argv[1:5]
SLOW_SECONDS = float(sys.argv[5]) if len(sys.argv) > 5 else 5.0

RESPONSES = {
    "healthy": (200, '{"success":{}}'),
    "failure": (200, '{"failure":{"detail":"database is locked"}}'),
    "scoped_refusal": (
        200,
        '{"failure":{"detail":"session: GetLiveWork names no session, and the '
        'store never answers it unscoped","invalidRequest":{"field":"session"}}}',
    ),
    "non200": (503, '{"code":"unavailable","message":"store is shutting down"}'),
    "malformed": (200, '{"success":'),
    "slow": (200, '{"success":{}}'),
}
if MODE not in RESPONSES:
    sys.stderr.write("fake-store-fixture: unknown mode %r\n" % MODE)
    sys.exit(2)


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    # A UNIX-socket peer has no address; the default implementation indexes
    # into the empty client_address and would raise.
    def address_string(self):
        return "unix"

    # The harness asserts on the call log, never on stderr chatter.
    def log_message(self, fmt, *args):
        pass

    def do_POST(self):
        length = int(self.headers.get("Content-Length") or 0)
        body = self.rfile.read(length).decode("utf-8", "replace")
        with open(CALL_LOG, "a") as fh:
            fh.write(
                "%s\t%s\t%s\n"
                % (self.path, self.headers.get("X-Agent-Repl-Request-Id", ""), body)
            )
        if MODE == "slow":
            time.sleep(SLOW_SECONDS)
        status, payload = RESPONSES[MODE]
        raw = payload.encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(raw)))
        self.end_headers()
        self.wfile.write(raw)


class Server(socketserver.ThreadingUnixStreamServer):
    daemon_threads = True
    allow_reuse_address = True


if os.path.exists(SOCKET):
    os.unlink(SOCKET)
server = Server(SOCKET, Handler)
with open(READY_FIFO, "w") as fh:
    fh.write("ready\n")
try:
    server.serve_forever()
except KeyboardInterrupt:
    pass
