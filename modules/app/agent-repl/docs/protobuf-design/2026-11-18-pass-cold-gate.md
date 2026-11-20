# PassColdGate: an agent passes another workspace's cold gate by a Haiku compaction

The owner ruled (2026-11-18) that a workspace may pass a child's cold gate at its own discretion, but only by compacting all of the child's context with Haiku. The agent API must support that one way of passing a gate for another workspace, and never paying the cold read or clearing the context.

Designed autonomously (no co-design was asked for).

## Assumptions

- Objective: an agent can pass another workspace's cold gate, and can only do it by a Haiku compaction of all the context.
- Built: one rpc (`agentrepl.v1.PassColdGate`), its CLI verb `workspace pass-gate WS`, which replaces `workspace gate` (that verb could pay and clear), the metaprompt rule, and the workspaces skill's entry.
- No frontend change: the user still answers the gate card through `agentrepl.v1.AnswerColdGate`, with every choice.
- Systems: the daemon (server, workspace verbs, CLI) and the editor's fake daemon, which implements every rpc.

## Landed changes

### PassColdGate (endpoint_pass_cold_gate.proto, service.proto AGENT API section)

- WHAT: a request naming the gated `workspace.v1.WorkspaceRef` and the passing workspace (`agentrepl.v1.WorkspaceRequesterWorkspace`, required); an empty success; an error whose cause is `unknown_workspace`, `workspace_ref_mismatch`, `transferring_away`, `not_yet_adopted`, `no_cold_gate` or `unserved_remediation`.
- WHY A NEW RPC, NOT A REQUESTER ON `AnswerColdGate`: the request carries no choice at all, so pay and clear cannot be expressed on the agent's path. A requester field on `AnswerColdGate` would leave them one field away, enforced only by a check.
- The requester is the workspace arm alone, not `agentrepl.v1.WorkspaceRequester`: the user never passes a gate this way, so the user arm is unrepresentable.
- The daemon answers the gate `compact` with the default summarizer (`defaultSummarizerAnswer`: Haiku, `SESSION_COMPACT_SCOPE_ALL`), the summarizer a standing deploy check's gate is answered by. It is `compact`, not `compact_and_resume`: the prompts held at a gate are the user's, so they stay held for the user to send.
- `unserved_remediation` is a gate whose menu offers no Haiku compaction of all the context.
- `CompactWorkspaceError.cold_gate`'s comment now points an agent at `PassColdGate`.
