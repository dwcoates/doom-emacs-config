# Diagnosis: expected stderr closure is logged as an error

Severity: Low. Confidence: Certain.

The daemon emitted `shim: stderr scan error: read |0: file already closed` for the intentionally stopped `doom` session at `13:39:27.940` and for the cleanly deleted `ceac-sandbox-gns-gate` session at `13:53:36`. Both child processes exited with code 0.

`modules/app/agent-repl/daemon/internal/shim/proc.go:359` scans stderr and emits every non-nil `scanner.Err()` as an error. It has no knowledge of expected pipe closure during the process teardown path.

Root cause: process teardown closes the pipe while the scanner is active, and the scanner classifies the resulting closed-file error without lifecycle context.

Impact: clean termination appears red, obscures real shim failures, and creates false incident signals during session deletion and supersession.
