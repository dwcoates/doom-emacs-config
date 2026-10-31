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
  and `agent-repl-remote-control-toggle` both send `toggle`.
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

## Keep-alives and hibernation

A workspace in remote control keeps its shim up between keep-alives (owner
ruling, 2026-10-31). Its beats go on as every workspace's do, but no beat is
followed by the keep-alive cycle's park, and no settings visit stands it down.
Since it is never parked, it never needs the restore before a beat. The idle
cutoff still hibernates it as it does any workspace, and the bridge goes down
with the shim. The mode is read when each stand-down is decided, so turning it
off lets the next beat park the shim as usual. See `shim-lifecycle.md`,
"Keep-alives".

## Surfaces

- **Topbar.** `frontend.v1.TopbarView.remote_control` is always set. The chip
  sits immediately left of the persistent-wifi chip and is drawn the way that
  chip is: a black phone glyph on a green disc when the mode is on and a white
  disc when it is off. It pulses while a toggle is in flight, and the tooltip
  is worded by the daemon.
- **Emacs.** `agent-repl-remote-control-toggle`.
- **Not surfaced.** The bridge's claude.ai URL is an implementation detail, so
  no view shows it (owner ruling, 2026-10-31).
