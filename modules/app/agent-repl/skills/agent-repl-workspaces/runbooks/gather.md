# Asking many workspaces one question

- `agent-repld workspace gather -workspaces A,B,C -prompt TEXT [-as-user]`
  prompts every named workspace (by name) with TEXT on this workspace's
  behalf, exactly as `workspace prompt` would, with the same refusals,
  including the fan-out cap, which `-as-user` lifts (only when the user asked).
  Use it whenever this workspace needs the same answer from several
  workspaces (a standup, a status sweep) instead of a `workspace prompt` per
  target that asks each to reply.
- Targets never reply on purpose. A target's ANSWER is the final response of
  the turn the gather's prompt went into, followed even when the prompt joins
  a running turn, is held, or is resumed after a restart. Do not ask targets
  to send a reply.
- The answer prints the gather's ref (`success.gather.id`), the accepted
  targets, and every refused target with its typed cause: an unknown or
  closed name, a name several open workspaces share, this workspace itself, a
  question or permission waiting on the user, a merge in progress, or the
  submission's own `workspace prompt` refusal (a cold gate, the fan-out cap).
  stderr says one sentence per refused target. A refused target is not part
  of the gather. When every target is refused, nothing is made and the
  command exits 6.
- The answers come back to THIS workspace as notices (deferred prompts, like
  subscription notices), never on the command's output:
  - THE FIRST UPDATE, once: the moment every accepted target has answered,
    or 5 minutes after the gather was made, whichever is first. It carries
    every answer so far under its workspace name and every target still
    outstanding with why (working, held, cold_gate, needs_input,
    ended_without_response, faulted).
  - After it, EACH LATER ANSWER as its own notice, naming the workspace and
    how many are still outstanding.
  - A target that closes before answering resolves as `closed`.
  - Each notice carries an `agentrepl.v1.GatherNotice` as JSON in a fenced
    block: `firstUpdate` (`trigger` allResolved|timedOut, `resolved[]` each
    with `target.name` and `answered.text` or `closed`, `outstanding[]` each
    with `target.name` and its reason arm) or `laterAnswer` (`resolved`,
    `outstandingCount`). Read those fields; do not parse the sentences.
- After `workspace gather` returns, end the turn to wait (mark this workspace
  awaiting if `mark-awaiting.md` applies); the first update
  wakes it. Gathers survive daemon restarts and deploys.
- `agent-repld workspace gather-status GATHER` reads a gather's answers and
  outstanding targets at any time.
