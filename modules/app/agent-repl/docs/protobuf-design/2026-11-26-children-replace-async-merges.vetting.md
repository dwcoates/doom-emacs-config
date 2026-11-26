# Vetting register: children replace async merges

## 1. A subagent's transcript can be resumed as a top-level session

- **Assumption**: the vendor's CLI can start a new top-level session from a
  subagent's transcript, the `agent-<id>.jsonl` side transcript it writes
  under its spawning session's directory, the way it forks a main session's
  transcript.
- **Affected if false**: `agentrepl.v1.WorkspaceContextOriginSubagent` and
  the `subagent` arm of `agentrepl.v1.WorkspaceContextOrigin`, and
  `agentrepl.v1.CreateWorkspaceUnknownSubagent`.
  - Without it, a child landing a subagent's branch can only fork its
    spawning workspace, as the async merge's subagent maker did.
- **How to verify**: in the shim's fork path
  (`agent-shim/claude/shim`, its `ForkSession` endpoint), copy a subagent's
  side transcript into a new session file. Then resume it with the vendor
  SDK's resume option on the fake SDK and on one real Haiku call, and check
  that the resumed session sees the subagent's turns and none of its
  spawner's.
- **Status**: OPEN.
