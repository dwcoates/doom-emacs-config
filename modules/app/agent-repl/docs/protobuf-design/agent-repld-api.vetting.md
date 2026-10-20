# Vetting register: agent-repld API contract

Investigations owed once the contract has landed; see the sibling design record
`agent-repld-api.md`.

1. SUBAGENT TRANSCRIPT LOCATION.
   - Assumption: the vendor files a subagent's transcript at
     `<config>/projects/<project>/<session-id>/subagents/agent-<agent_id>.jsonl`
     under the SPAWNING session, so a subagent id resolves to exactly one
     workspace.
   - Affected: `CreateWorkspaceAsyncMergeMakerSubagent`, the
     `async_merge_unknown_subagent` refusal, and the maker resolution that
     decides `async_merge_context_not_maker`.
   - Verify: forward, list `subagents/` under several live sessions and match
     ids to their spawning sessions' Agent tool results; reverse, check whether
     a subagent of a subagent (spawn depth 2) files under the top session or
     its parent, and whether a resumed or forked session's subagents move.
   - Status: OPEN. Observed 2026-10-16 for spawn depth 1 only
     (`agent-a6e95de73147f1ce5.jsonl` with `spawnDepth: 1` in its
     `.meta.json`).
