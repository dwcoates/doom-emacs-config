# Remote control

How a workspace's session, which runs on this machine, is made reachable from
claude.ai and its mobile app. This is the developer reference for the feature
across the daemon, the shim, the webapp and Emacs. Update it in the same change
as any code that changes what it says.

## What it is

The vendor CLI the shim drives has its own Remote Control bridge. In SDK mode
the shim asks for it with the SDK's `Query.enableRemoteControl` (the CLI's
`remote_control` control request, internal to the SDK and typed at the SDK
boundary, `sdk/types.ts`). The CLI then registers the session with claude.ai
and relays it both ways. Nothing runs in the cloud: the agent loop, the tools
and the files stay on this machine.

What the phone sends goes straight into the CLI, past agent-repl's prompt queue
(owner ruling, 2026-10-31: that is accepted):

- **A prompt** starts a turn the shim did not start. The CLI echoes it on its
  stream because every agent session is launched with
  `--replay-user-messages`. The shim stamps a uuid on each prompt it sends, so
  an echo carrying one of those uuids is an acknowledgment and is dropped. An
  echo carrying any other uuid is a remote prompt. The shim adopts its turn as
  it adopts a vendor-started one, with `PROMPT_ORIGIN_REMOTE_CONTROL` and the
  prompt's text as `said`, and the feed draws it as a prompt the user said.
- **A permission answer.** The CLI puts each ask to the shim (`canUseTool`)
  and to claude.ai. When claude.ai answers first, the CLI withdraws the ask
  from the shim (`control_cancel_request`), and the SDK aborts the ask's
  `AbortSignal`. The shim settles the permission as
  `AgentPermissionSuccess.answered_remotely`. The decision is not relayed, so
  the gated call's own outcome says which way it went. When the shim answers
  first, the CLI withdraws claude.ai's prompt itself.
- **An interrupt or a stop** ends the turn inside the CLI. The shim sees the
  turn's interrupted result as it sees one it asked for.

### What was verified against a real bridge

These claims were first read from the SDK's minified code. They were then run
against a live bridge (SDK and CLI 0.3.311, 2026-10-31, from the Claude mobile
app):

- `enableRemoteControl` brings the bridge up with no prompt sent, so a
  workspace needs no first turn before it can be reached. It answered in
  0.6-1.0s with `session_url`, `connect_url`, `environment_id`, `bridge_epoch`
  and `bridge_session_id`.
- A second vendor process passed the first one's `bridge_session_id` as
  `reattachSessionId` got the same bridge session back. claude.ai kept the
  same session.
- A prompt typed on the phone arrived as `{type: "user", isReplay: true}` with
  a uuid the shim never sent, followed by the turn it started.
- A permission answered on the phone aborted `canUseTool`'s `AbortSignal`
  2-3.5s after the ask, and the gated call then ran.
- A stop pressed on the phone ended the running tool call as a rejected tool
  use, then `[Request interrupted by user for tool use]`, then a `result` of
  `error_during_execution`. That is the same shape as a stop asked for here.
- A turn started from the phone runs with the session's own working directory
  and permission mode. It reads and writes this machine's worktree like any
  local turn, including `git commit`.

### Gaps that are known and open

- **The bridge's health.** The CLI states it as `system`/`bridge_state`
  messages: `ready`, `connected` (with `bridge_epoch`), `failed` (with
  `failure_kind`) and `policy_disabled`. No shim converter owns them, so they
  land as residue, and a bridge that fails after it was turned on leaves the
  chip saying on. The contract has no field for the bridge's health.
- **A question answered on claude.ai** has no "answered remotely" arm, so it
  is recorded `unanswered`.
- **A rollback past a remote prompt** is untested and probably refuses
  `unseen_prompt`.

## The mode

The mode belongs to the daemon and is kept per workspace in wsm
(`workspace_remote_control`, which also keeps the bridge's claude.ai session
id).

- **Defaults.** An unset mode is on for a repository's main worktree
  (`wsm.IsMainWorktree`) and off for every other workspace. A toggle records the
  mode it leaves, so it holds across shims and daemons (owner ruling,
  2026-10-31).
- **Changing it.** `agentrepl.v1.UpdateRemoteControlMode` takes `on`, `off` or
  `toggle`. A toggle turns over the mode read at that moment. The topbar chip
  and `agent-repl-remote-control-toggle` both send `toggle`. An agent sends
  `on` or `off` for any workspace with `agent-repld workspace set WS
  -remote-control on|off`.
  - With a shim up, the daemon sends `shim.v1.SetSessionRemoteControl`.
  - Turning it on with no shim starts one.
  - Turning it off with no shim only records it.
  - When the vendor refuses, the mode is still recorded, and the refusal
    rides the warning strip.
- **Every shim start.** If the mode is on, the daemon sends
  `SetSessionRemoteControl` once the session is up, with the bridge session id
  the last shim answered (`reattach_bridge_session_id`). claude.ai then keeps
  the same session instead of listing a new one. The shim puts the state back
  in force on every vendor process it opens after that, and reattaches with the
  id it last learned.

## Keep-alives and the idle cutoff

A workspace in remote control keeps its shim up between keep-alives (owner
ruling, 2026-10-31). Its beats go on as every workspace's do, but no beat is
followed by the keep-alive cycle's park, and no settings visit stands it down.
Since it is never parked, it never needs the restore before a beat. The idle
sweep still parks it once it has been idle past the cutoff, as it does any
workspace, and the bridge goes down with the shim. A workspace parked at its cold gate keeps its bridge (see "A
parked workspace"). The mode is read when each stand-down is decided, so turning it
off lets the next beat park the shim as usual. See `shim-lifecycle.md`,
"Keep-alives".

## A parked workspace

A workspace parked at its cold gate runs no vendor process, so it has no
bridge, and a prompt typed on the phone reached nothing on this machine until
someone answered the gate at the editor. A workspace in remote control
therefore keeps its bridge while it is parked (owner ruling, 2026-11-02, which
overturns "the gate runs no shim" for this case only):

- **The bridge.** Wherever the daemon would stand a gate's shim down, it
  settles it instead (`workspace.Fleet.settleGateShim`). With the mode on, one
  shim holds the parked conversation's bridge (`shim.v1.HoldParkedBridge`):
  it resumes the conversation exactly as the gate's pay answer would, sends no
  prompt, and turns the bridge on, re-binding the kept claude.ai session. A
  gate raised from its record with no shim gets a shim spawned for it, and so
  does turning the mode on while parked. Turning it off stands the shim down.
- **A remote prompt pays.** The vendor runs a prompt typed on claude.ai the
  moment it arrives, so the cold read is spent then. The shim states it on the
  bridge's stream, and the daemon answers the gate pay through the same
  `AnswerColdGate` the editor's button uses, logging the prompt and the gate at
  INFO (`daemon.workspace.parked_bridge`). The answer retires the gate, keeps
  the prompts held at it as pay always does, and marks the cache.
- **The answer promotes the bridge.** The pay answer's `StartSession` is
  answered with the session the bridge already opened, and the remote
  prompt's turn is its turn in flight. Any other answer (clear, compact) stands
  the bridge's shim down and resumes on a new shim.
- **A bridge that cannot be held** is logged at ERROR and stated on the warning
  strip, and the gate keeps no shim.
- **A prompt from anywhere else** meets the gate as before.

Two vendor behaviors are assumed and not yet confirmed against a live bridge.
The shim logs each at INFO, with an `assumption` field, every time it opens a
parked bridge:

- an idle resumed vendor process makes no API request;
- a prompt typed on claude.ai while no bridge was attached is delivered when
  the bridge re-binds the same claude.ai session.

## Surfaces

- **Topbar.** `frontend.v1.TopbarView.remote_control` is always set. The chip
  opens the left group's status run, after the account cell and immediately
  left of the persistent-wifi chip, and is drawn the way that
  chip is: a black phone glyph on a green disc when the mode is on and a white
  disc when it is off. It pulses while a toggle is in flight, and the tooltip
  is worded by the daemon.
- **Emacs.** `agent-repl-remote-control-toggle` (`SPC j C`).
- **Not surfaced.** The bridge's claude.ai URL is an implementation detail, so
  no view shows it (owner ruling, 2026-10-31).
