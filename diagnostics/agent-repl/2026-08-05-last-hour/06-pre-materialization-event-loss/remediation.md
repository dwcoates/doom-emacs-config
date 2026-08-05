# Remediation: gate publication on host materialization

1. Establish one daemon creation-state transition that publishes `WorkspaceAvailable` before any session-scoped frontend state can be emitted.
2. Hold session startup state behind the existing materialization acknowledgement, or include it in the first post-materialization authoritative snapshot. Do not emit frames the host cannot route.
3. Keep the initial prompt gate and the state-publication gate driven by the same durable job state.
4. Make Emacs reject an impossible pre-available session frame loudly with job, path, and session identity. Do not silently retain or drop it as normal behavior.
5. Add deterministic tests that delay Emacs materialization while the shim becomes ready and produces progress.
6. Verify that canonical workspace logging begins with the first workspace-scoped record and that the global sink receives no name/path warning pair.

Success criteria: zero dropped initial render or context-cost records under arbitrarily delayed materialization, and no workspace records in the global sink.

Protobuf decision: no change. Existing creation and materialization messages contain the required identity and acknowledgement.
