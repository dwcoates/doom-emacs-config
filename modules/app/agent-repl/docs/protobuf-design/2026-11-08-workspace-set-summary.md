# Workspaces set their own topbar summary

The topbar's purple summary was distilled by the daemon itself: a headless
Haiku call over every prompt the user sent, made at the first prompt and then
every five prompts. The owner wants the summary to be part of the agent-repld
harness instead: each workspace's own agent sets it, on its first prompt after
creation and again after a /clear, and changes it only when the workspace's
high-level task materially changes. The convention stays the same: one
imperative sentence distilling the workspace's task, and short (ten to fifteen
words at most, shorter being better).

Design mode: autonomous.

## Context (assumptions, from the request and the code)

- Objective: the workspace's agent, not a periodic headless derivation, owns
  the topbar summary.
- What is built: an agent API verb that sets the summary, a prompt note that
  asks for it while none stands, and the retirement of the synthesizer.
- Frontend: the topbar already draws `frontend.v1.TopbarTitle`; only its
  documentation changes, not its shape.
- Systems: the daemon (verb, store, note, topbar install, /clear drop), the
  shim (its digest rpc retires), the proto package, the harness (brief and
  skill).
- Non-additive changes: `titlesynth.Synthesizer` is deleted;
  `shim.v1.GatherTitleDigest` loses its only caller and retires; the store's
  summary row loses its digest columns (they were the synthesizer's cadence).

## Iteration sequence

Default sequence. Transport is the existing ConnectRPC agent API; the
canonical `oneof result { success; error; }` carries forward; one endpoint.

## Landed changes

### `agentrepl.v1.SetWorkspaceSummary` (new endpoint)

- WHAT: a new rpc in the agent API section, beside `SetWorkspaceName`, with
  the standard refusal arms of a workspace-addressed verb plus
  `invalid_summary` (the summary as named and the validator's reason).
  `agentrepl.v1.WorkspaceStatus.summary` (optional) answers the standing
  summary; unset means one is owed.
- WHY: the agent holds the whole conversation, so it is the one that states
  the task; the daemon distills nothing.
- Consequences:
  - The daemon validates the format, so the title line never draws an
    over-long or multi-clause summary. The rule: one line, one sentence, two
    to fifteen words, no comma/semicolon/colon/parenthesis/dash, no
    surrounding whitespace. This shares its one-sentence and word-bound check
    with the link label's (`internal/workspace` `validLinkLabel`), extracted
    into one helper.
  - "Owed" is not its own stored state: it is the absence of a summary row.
    Creation starts with none; /clear drops it. While none stands, every
    user-sent or agent-sent prompt is closed by the meta-wrapped
    `workspace-summary-owed` brief, so a workspace that skipped it is asked
    again rather than left on its vendor title. Daemon-born briefs (merge,
    pull request and queue notices) are not decorated.
  - The vendor's ai-title stays the topbar's fallback while no summary
    stands, exactly as before the first distillation.
  - Why the alternatives lost: a metaprompt rule ("set it when your context
    is empty") relies on the agent noticing its context is empty, and misses
    forks, whose context is the parent's; a stored "owed" flag beside the row
    adds a second fact that can disagree with the first.

### `frontend.v1.TopbarTitle` documentation

- WHAT: the comment now names the agent-set summary instead of the daemon's
  distillation. No shape change.

### `shim.v1.GatherTitleDigest` (retired)

- WHAT: the rpc, its endpoint file and `shim.v1.TitleDigestBoundary` are
  removed; the shim's digest engine and its tests go with it.
- WHY: its only caller was the synthesizer. A fork's naming call reads the
  parent's prompts from the daemon's own store (`RecentConversationPrompts`),
  not this rpc, so nothing else depends on it (verified by grep over the
  daemon and the shim, 2026-11-08).
