# Slack-thread watching in the daemon

The gns-sockets plugin (a separate Go daemon, LaunchAgent
`com.chesscom.gns-sockets`) streamed Slack thread messages into per-session
queue files that Claude Code hooks drained at tool boundaries, with an idle
Haiku subagent bridging idle moments; the subagent died every few minutes and
left gaps. agent-repl's daemon outlives every session and starts turns from
outside, so it takes the whole job over: it follows subscribed threads itself
and tells each workspace as a prompt. The same daemon also takes over the
plugin's reaction initiator (an emoji the user types or reacts creates a
workspace), so the plugin can be deleted.

Design mode: autonomous (the user approved the feature's design in the parent
workspace `gns-sockets-persistent-bridge`; the protobuf shapes were not
co-designed).

## Context (assumptions, recorded per the autonomous flow)

- Objective: deliver Slack-thread activity to workspaces structurally, with no
  hooks, listener subagent, heartbeat or respawn directive, and fold the
  reaction initiator in.
- Built: a daemon package `slackwatch` (stream workers, batching, posting,
  reaction polls), durable `wsm` tables, agent API endpoints and
  `agent-repld` verbs.
- Frontend: none beyond the feed drawing the new prompt origin's bubble as an
  ordinary daemon-sent prompt.
- Systems: daemon (all logic), proto, elisp (`wire-common.el`'s list of
  origins no client sends). No shim, store, sidecar or webapp change.
- Non-additive changes: none. Every message, field, rpc and enum value is new.
  `CreateWorkspaceSlackOrigin`'s comment loses "the daemon itself never posts
  there", which is no longer true for a subscribed workspace.

## Iteration sequence

Default sequence, walked once: transport (ConnectRPC, the existing
`agentrepl.v1.AgentRepl` service); suites (one new "Slack threads" run of
rpcs after the GitHub merge-queue rpcs); inventory; conventions (the standing
`oneof result { success; error; }`, the package's `error` spelling, causes
under `oneof cause`); then shapes one endpoint at a time.

## Landed changes

### Shared types (`slack_thread.proto`, `slack_reactions.proto`)

- WHAT: `SlackThreadRef`, `SlackThreadMode` (listen, respond, interact as a
  oneof of empty arms), `SlackThreadSubscription` (origin, posts, health,
  pending count), `SlackThreadMessage`; `SlackReactionSettings`,
  `SlackReactionBinding`, `SlackReactionAction`.
- WHY: every endpoint names a thread and a mode; the status and the update
  rpc both answer the settings.
- The mode is a oneof, not an enum, per the state-enum rule: it is the
  subscription's autonomy condition.
- `SlackReactionAction.thread` is a oneof (`announced` = the emoji-ack
  originator whose lifecycle hooks post; `subscribed` = a creation-time
  subscription) because the two arms carry different data (only `subscribed`
  has a mode) and the three shipped gns-sockets bindings use exactly these two
  shapes (`agent-ack.sh` and `one-shot.sh` pass `--slack-thread`;
  `sockets-ack.sh` passes `--socket`). `one_shot` stays a bool: it selects no
  interpretation of adjacent data.
- Consequence: the bindings are typed workspace-creation requests, so the
  dispatch shell scripts (`dispatch-workspace.sh` and its wrappers) have no
  successor; a user wanting a custom script binding has none (accepted).

### Endpoints

- `SubscribeSlackThread`, `UnsubscribeSlackThread`, `SetSlackThreadMode`,
  `ListSlackThreadSubscriptions`, `ReadSlackThread`, `PostToSlackThread`,
  `GetSlackStatus`, `UpdateSlackReactionSettings`.
- A thread is named by its Slack link everywhere; a link that is not one is
  Connect InvalidArgument (as `CreateWorkspaceSlackOrigin` already is), not an
  error arm, because it is a malformed request rather than a domain answer.
- "Not subscribed" on unsubscribe is a success arm (the answer the caller
  wanted), mirroring `UnsubscribeGithubMergeQueue`.
- `PostToSlackThread` refusals are typed arms: no emoji prefix, a reserved
  `:peepo_*:` prefix, listen mode, post limit reached, a secret found (the kinds
  only, never the matched text), and Slack unreachable.
- `GetSlackStatus` folds in the reaction settings, so there is no separate
  read of them; its error arm is deliberately empty.
- Rejected: a separate `GetSlackReactionSettings` (duplicated the status);
  per-setting rpcs (`SetWatchUser`, `BindEmoji`, …) — one update rpc with a
  oneof of changes keeps one verb surface and one validation site.

### `CreateWorkspaceSlackOrigin.subscribe`

- WHAT: `optional SlackThreadMode subscribe = 2`.
- WHY: `-slack-thread` on `workspace create` records a subscription when the
  originator intends one (the reaction initiator's `subscribed` bindings), so
  the subscription is made by the create itself instead of being the new
  agent's "first action".
- Consequence: a subscription Slack refuses fails the create, which is
  undone (honest failure over a workspace that silently hears nothing).
  The held-create record (`daemon/internal/workspace/pendingcreates.go`)
  carries the mode, so a fork create waiting for its requester's turn keeps it.

### `conversation.v1.PromptOrigin.PROMPT_ORIGIN_SLACK_THREAD_NOTICE = 54`

- WHAT: the digest's prompt origin.
- WHY: a digest is the daemon's word, never the user's, and it must be told
  apart in records, in the cold gate (it waits the gate out, as every notice
  does: `promptqueue.waitsOutColdGate`) and in the feed label.
- It is NOT `promptorigin.Unseen`: the feed draws it, because what a thread
  said is what the user wants to read beside the agent's reply (the PR and
  merge-queue notices are bookkeeping, a thread's messages are content).
- Delivery: a digest carrying an urgent message is submitted with ordinary
  delivery, which the running turn's classifier may interject (the successor
  of the plugin's PreToolUse interrupt); any other digest is deferred (the
  successor of its Stop-hook drain). Verified in
  `daemon/internal/promptqueue/classify.go` (`deferredVerdict`): deferred
  prompts are never classified and run as their own turn; ordinary prompts are
  classified against the running turn.

## Core decisions recorded for implementers

- SELF-FILTER IS A NONCE, RECORDED BEFORE THE POST. The plugin derived a
  per-session emoji from a 32-bit id modulo a 128-entry palette, so two
  sessions could look like each other. The daemon writes a random nonce row
  (`slack_thread_posts`) before `gns slack send`, and puts the nonce in the
  post's footer; an arriving message carrying one of the subscription's
  nonces is dropped in the same transaction that would store it. A crash
  between the row and the send leaves a nonce no message carries, which is
  harmless. This makes "the workspace is told its own post" impossible rather
  than unlikely, and removes the post-in-flight race a timestamp ledger would
  have.
- PENDING MESSAGES AND THE CURSOR ADVANCE IN ONE TRANSACTION, so a restart
  never loses a message nor re-reads one past its cursor.
- Post limit (20, as the plugin had) and the secret scrub are ported; reaching
  the limit switches the subscription to listen and says so in the thread.

## Not ported (by instruction)

hooks, `wait-for-events`, `sockets-listener`, heartbeat, bridge respawn,
`bridge-lifetime`, `delay`/`undelay`, `ack-required`/`ack-clear`/`interrupt`,
and the PR/git helpers (`git`, `prreact`, `gitops`, `prmanifest`, `prstate`),
superseded by `prwatch`, `-slack-thread` and `-originator emoji-ack`.

## Package-wide sweep

Nothing obviated: every declaration is additive and every new message is
referenced by an rpc or a shared type.
