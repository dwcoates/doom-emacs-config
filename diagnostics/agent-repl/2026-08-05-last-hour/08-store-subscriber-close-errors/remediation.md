# Remediation: centralize subscription termination

1. Give each subscriber connection one terminal-state owner with reason codes such as client EOF, client reset, slow consumer, server shutdown, replay failure, and readiness failure.
2. Cancel replay and close the socket exactly once through that owner.
3. Have reader and writer paths report candidate causes to the owner, then suppress secondary closed-connection symptoms after a terminal reason is recorded.
4. Keep unexpected transport loss as an error and expected client cancellation as normal or verbose.
5. Preserve session, Claude session, peer, replay range, delivered count, and terminal owner in one final structured record.
6. Test close before first replay row, close mid-replay, close during tail, simultaneous read and write failure, and store shutdown.

Success criteria: each connection produces one terminal record, no self-close error, and unexpected replay truncation remains loud.

Protobuf decision: no change. Socket close is an adequate cancellation signal.
