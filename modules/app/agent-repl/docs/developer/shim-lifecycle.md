# Shim lifecycle and keep-alives

How the daemon decides when a workspace's shim runs, and how it keeps a
conversation's prompt cache warm. This is the developer reference for both. The
owner rulings it states are dated, and the code that carries each one is named.
Update this file in the same change as any code that changes what it says.

## The rule: a shim runs only when a session must

A shim is the per-workspace process that runs the vendor session (about 350MB).
Since the 2026-10-28 owner ruling, a shim runs only when something needs the
session. Nothing about looking at a workspace needs it, because the daemon
reads every conversation from the store's read-only face (`storereader`,
`docs/protobuf-design/2026-10-27-store-read-history.md`). A workspace with no
shim still draws its whole conversation.

### What starts a shim

| Cause | Where |
|---|---|
| A prompt, from the user or from another workspace (`agent-repld workspace prompt`) | `promptqueue` revival (`queue.reviveInBackground`, `Deps.Revive` = `Fleet.Start`) |
| A cold gate's answer, whichever choice | `workspace.Fleet.ResumeCold` (`heldForAnswer`) |
| A keep-alive restore before the next beat | `cachelapse.Keeper` (`Deps.Restore`) |
| A create with an initial prompt, and every fork | `workspace.verbs.Create`, `CreateThread` |
| Turning remote control on for a workspace with no shim | `UpdateRemoteControlMode` (`remote-control.md`) |
| The restart verb, which starts one at once when none is up | `workspace.verbs.Restart` (`startForRestart`) |
| A rebind to another transcript | `workspace.Fleet.StartRebound` |
| A model, permission-mode or effort pick on a workspace with no shim | `promptqueue` settings revival (`SubmitSessionAct`, `ReviveForSettings`) |
| A boot or a handover, only for a shim that was up when the last daemon stopped | `bringup.OnlyWasUp`, in `boot` and `rollout` |

### What never starts a shim

- Selecting a workspace (`verbs.Select`), or the landing after a hide.
- Opening it (`verbs.Open`, and the editor's `OpenTabs`, `opentabs.Coordinator`).
- Showing a hidden workspace, expanding a collapsed repository, or reopening a
  closed one. A reopen for an owner or a main worktree clears the closed flag,
  and the prompt sent to it starts the shim.
- Emacs announcing a workspace (`verbs.Register`).
- A create with no initial prompt.
- A history read. The store serves it.

### "Was up at the last stop"

A boot or a handover brings back exactly the shims that were up, hidden or
not, and nothing else decides. The state is recorded at every park and unpark,
never only at shutdown, so a crash leaves it true too: the session record in
wsm. Every stand-down the daemon makes on purpose writes a terminal (a
keep-alive cycle's park, a hibernation, a cache lapse, a kill), and every
session coming up clears it. The daemon's own shutdown, restart or hot reload
writes nothing, so the record still says how each shim stood. A session on
record with a vendor session and no terminal was up. A record with no vendor
session is the spawn-facts row a create writes before any start, and it was
never up. A turn a restart cut had its shim up, so it comes back and resumes.

### A settings pick's shim is a visit

The topbar's model, permission-mode and effort controls are drawn while a
workspace has no shim (owner request, 2026-10-29): the last model and mode in
force, the effort last picked, and the catalog the login last served are kept
in wsm (`workspace_selections`, `account_model_catalogs`, layout 89) and seeded
when the workspace registers (`topbar.WithSelectionStore`). Only a standing
cold gate withholds them, because its answer comes first.

A pick with no shim starts one, start cause `settings`:

- A model or permission-mode pick is held as a prompt is, under the reconnect
  hold, and delivered once the session is up.
- An effort pick holds no row: the topbar keeps the pick, and every start puts
  it back in force (`reapplyEffort`).

The switch away from that workspace stands the shim down again, reason
`settings_visit` (`workspace.SettingsVisits`, `drain.VisitDown`), under the
keep-alive cycle's rules: never under a turn, never for the workspace in view,
and never once a real turn has run since the pick. It leaves the keep-alive
cycle's park, so the workspace is drawn as the idle session it was.

## No UX waits on a shim

- An editor's open answers once the conversation's newest page is read from the
  store and the page drew (`opentabs`, `docs/protobuf-design/open-tabs.md`).
- A prompt sent while the shim is coming up is held, never refused. The held
  badge says why: "after loading" for any start, "after reconnect" for a dead
  shim's relaunch, "after reloading" for a stale-build replacement.
- The footer reads "loading" for every shim start (the `starting` status arm,
  worded "loading" by the page, `statusCellWord`), a parked session's restore
  included: the booting rung stands while parked (`ladder.Resolve`).
- The sidebar row's name wears the reviving shimmer (`RosterRow.reviving`) from
  a bring-up's first step to its last (`shimLoading` in `cmd/agent-repld`).

## Book naming

The daemon reads a workspace's conversation from the store under its book. The
book is the original vendor session id, which the shim states on every
`StartSessionResponse.book` and the daemon records in `wsm.workspace_book`
(`Fleet.recordBook`). A workspace with no recorded book takes the one its shim
persisted in `<state>/shim/<workspace key>/agent-id.json` (`shimidentity`) and
records it. One whose shim persisted none has no conversation yet, and its
newest page is empty.

## The cold gate runs no shim

A shim judges a cold gate once, at a start. The fleet records it in
`wsm.cold_gate`, raises it, and stands the shim down. From then on the gate is
raised from the record with no shim, at every start, the boot's included. Only
an answer spawns the shim its resume runs through. See `coldgaterecord.go`.

## Keep-alives

The daemon drives every keep-alive (owner ruling, 2026-10-26). A shim runs no
timer of its own. The details live in `daemon/internal/cachelapse`:

- **Cadence.** One beat every `KeepaliveInterval` (52 minutes), timed from the
  cache clock: the later of the last turn's end and the last beat sent
  (`wsm.Workspace.CacheClock`). The keeper asks the shim for each beat
  (`shim.v1.SendKeepalive`).
- **Parking between beats.** Once a beat has gone out, a session nobody is
  using is stood down (`drain.Controller.CycleDown`, terminal
  `keepalive_cycle`). It is restored `RestoreLead` (one minute) before its next
  beat, at most `MaxRestores` (3) at once. An idle workspace's memory is free
  between beats.
- **Remote control keeps the shim up between beats** (owner ruling,
  2026-10-31). A workspace whose remote-control mode is on still gets every
  beat, but no beat is followed by the park, and no settings visit stands it
  down: claude.ai must always find the session served. Since it is never
  parked it never needs the restore. The cutoff below still hibernates it.
  The mode is read when each stand-down is decided. See `remote-control.md`.
- **Cutoff.** Beats stop once the workspace has been idle for
  `drain.DefaultIdleCutoff` (24 hours, owner ruling 2026-10-28). The idle sweep
  then hibernates the session.
- **Hidden workspaces** beat no more, and their cache lapses on its own clock.
  **Closed workspaces** get the one beat they were owed, then lapse.
- A parked session is never brought back by a read or a look. Only its next
  beat, a prompt, or a restart brings it back.

## Telemetry

The cache and lifecycle series carry a `workspace` label
(`docs/PLAN-runtime-metrics.md`): keep-alive lateness, cache lapses, cold-gate
idle time, gate answers, shim starts by cause, and shim stand-downs by reason.
A cold gate is counted once, when its shim judges it, never on a re-raise from
the record.
