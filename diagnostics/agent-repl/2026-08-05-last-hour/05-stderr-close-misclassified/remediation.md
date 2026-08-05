# Remediation: make pipe ownership and close state explicit

1. Define one owner for closing the stderr pipe and one completion signal for the scanner.
2. Pass expected-shutdown state into the pump or join the pump before closing its reader.
3. Treat only lifecycle-proven closure as normal. Unexpected read failures remain errors with session and process identity.
4. Add tests for clean exit, SIGTERM, session deletion, scanner failure before shutdown, and malformed stderr followed by clean close.
5. Log shutdown reason, child exit status, pipe close owner, scanner completion, and whether the close was expected.

Success criteria: the two observed code-0 terminations produce no error record, while an actual read failure still does.

Protobuf decision: no change.
