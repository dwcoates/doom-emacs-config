# Remediation changelog

One line per landed remediation, newest first. Kept BRIEF on purpose: it is read
back into context at every compaction, so it must stay cheap to carry.

Its job is regression watch. A perf win or an invariant established while
remediating one realtest must not be quietly undone while remediating a later
one, and this is the only artifact that spans the whole effort. Before landing
anything, scan it for a line the change would reverse.

Format: `- <area>: <what changed> (<finding>)`. One sentence. No rationale — the
commit and docs/REALTEST-JUDGEMENT-CALLS.md hold that.

## Startup and workspace sections (realtests 1-8), 2026-09-12

- daemon: forgetting a workspace stands its live session down first, so a forgotten row no longer leaves a shim holding the directory's workspace lock for the next registration of that directory to mis-adopt (realtest 7)
- daemon: a bring-up whose lock reads held resolves the shim's newest socket generation and refuses at once when nothing is listening, instead of spending the whole adoption bound dialing a path no process ever bound (realtest 7)
- daemon: a failed adoption raises the same shim_start_failed fault and dead link a failed spawn does, so the footer states the cause of a dropped prompt (realtest 7)
- daemon: the bring-up records both kernel facts and the branch it chose at info, so which path a bring-up took is readable without rerunning at debug (realtest 7)
- daemon: a teardown this daemon ordered is an ordinary event on every side that sees it — the watcher and the supervisor both read the shim client's stand-down latch, so a kill or a relaunch bounce no longer records two stream ERRORs, a severed link, its health fault and a clean exit as five failures; an unasked end stays exactly as loud
- daemon/workspace: a resume opens its watch fleet on a context its caller cannot cancel, and a session that displaces a watcher closes it, so a relaunched workspace no longer tears down the fleet it just opened and reads its own cancel back as a severing
- shim: a peer's h2c CANCEL is recorded at info rather than warn — every workspace close cancels two standing watches, so the warning fired on every ordinary departure; a reset this side cannot account for is still an error
- store: a refusal's log level is a property of its refusal class, so an OpenAgentSession refused unknown_agent — the ordinary answer for an agent whose first row has not landed, which the shim already serves as an empty page — records at info while every other class keeps its level
- daemon/workspace+account: a bring-up over a vendor id that never took a turn is the ordinary bounce it is, recorded once at info with no fault, while a conversation that DID take turns and whose transcript vanished stays exactly as loud; the account probe's own empty answer drops to debug, since a layer that cannot tell the two apart must not choose the level
- daemon/roster: a repository's default branch is no longer read as a family link, so a workspace cut from it draws at the top level and priority can reorder the tab bar (realtest 8)
- daemon: every fleet adoption is dialed under the one adoption bound and records both its edges, so a revival can no longer hang forever with a prompt held and no turn recorded (realtest 7)
- elisp: a webview's load report is an info record, so the ONE marker every painted panel leaves survives the default log level and a created workspace stops reading as one that never painted
- elisp: a state save for a workspace whose root is gone is refused instead of re-creating the directory a nuke had just destroyed, so a delete leaves nothing on disk
- elisp: state-save no longer asks a retired local roster-snapshot subsystem to write, ending two `snapshot skipped reason=function-unavailable` records per run that named a function deliberately deleted when the daemon became the source of which workspaces exist
- sidecar: a LOST verdict about startup backlog is recorded on the rung the catch-up policy already chose for it, ending 161 warnings per run about runs that were stale before the process started
- elisp: the composer mounts at a height derived once per frame instead of a fraction of whatever window the mount split, so every workspace's input window is the same fixed height and stays it across remounts
- elisp/realtest: a quit arriving while a guarded section runs is delivered to the command loop afterwards, so C-g reaches the prompt the user aimed it at; a run clears pending input before it measures (DD, EE)
- daemon/elisp: registering a directory re-opens the closed row it names, and the roster stops offering repositories whose worktree is gone (AA, BB, GG)
- daemon: an inert surviving shim is no longer counted as a surviving session, removing all five cold-start harvest records (CC)
- realtest: a run forgets the workspaces it registered, via the command-file ingress (R)
- webview: the precreate hold gates a drain pass instead of each item, so losing focus mid-paint no longer strands the queue; 20.9s to under 1.5s (FF)
- realtest: a realtest reads a snapshot carrying the write-ahead log, never the owner's live database (Y)
- realtest/roster: both never-firing phase markers now fire and carry measured budgets (X)
- realtest: the sweep sequences each test's required world, so all eight can run in one invocation (T)
- daemon/elisp: departure means the boot claim is released, not the address file vanishing; fixes a restart that destroyed the daemon
- daemon: the host session identity is minted at creation, refused empty at the store, and healed in the existing rows (S)
- elisp: workspace switch and composer-landing records persist at info instead of being dropped at debug (U)
- realtest: a posted key is not a delivered one — keydriver holds the target key window open until Emacs's own `(recent-keys)`/`quit-flag` marks account for the press, so a dropped key is reported (and, for `<escape>` and `C-g`, re-posted) instead of being waited out downstream
- realtest: the failed `C-g` was key delivery, not quit handling — keydriver reads whether the target owns a key window before posting, and the dismissal finding now names harness or product from the marks Emacs itself leaves (the reading refused the post when this landed; it is advisory on a confirmed press since the entry below)
- realtest: phase budgets retightened from 43 runs of manifest history — the boot-chain rows drop from 3x to 2x (some to 1.75x/2.5x by per-row spread), the harness-only focus-edge row to 2.5x; two locked-screen runs excluded from every focus/paint row (Q, V)
- logs: compact query modes (tally, sample, fields, timeline) plus stderr and Messages sources
- daemon/elisp: workspace log-sink resolution is total, and reopening a vanished directory is a named refusal (F, I, J)
- daemon/shim: the vendor guard forces a fake vendor instead of refusing the spawn (E, G, L)
- realtest: key delivery fails loudly instead of silently dropping, one shared show phase, realtest 4 bootstraps its third workspace (H, M, K)
- daemon/roster: the daemon outlives Emacs, and "loading workspaces" echoes on a hidden startup (C, D)
- wsm: the forget verb, registration's undo, reachable through the command-file ingress (B)
- daemon/server+health: a fault the `kind' oneof spells no arm for is withheld from the wire instead of published unset, so one abandoned-conversation fault no longer costs the editor the whole WatchHostWorkspace push.
- daemon/rollout: a relaunch whose resume failed records `resume_failed' itself, not a private second spelling that reached no arm.
- daemon/server+promptqueue+prompthandler+health: resolving a named workspace's log sink is total at the last four sites, so a prompt is never lost, and a host view never withheld, over where its narration is written.
- elisp/notifications+session: the desktop-focus check names its scope -- the workspace that asked, or the central sink -- so it no longer earns `log-routing-error' at ERROR.
- daemon/sidebar: roster order is priority then name then id, with the selection instant dropped as an ordering key, so the drawn bar no longer moves under the user and cycling right then left returns where it started.
- realtest: a key window is a reading, not a gate — keydriver stops refusing a confirmed press on `NSRunningApplication.isActive` (the cached answer that refused 40 healthy presses in the 2026-09-12 16:12 sweep), reports the accessibility reading in the receipt instead, and keeps the hard refusal only on an unheld press, where nothing reads Emacs's marks back.
- realtest: a requested activation is not a focus edge — the show phase now reads Emacs's own `frame-focus-state` inside the keydriver hold, and when no press ever produced real focus it names the cause (a locked screen, where the window server activates nobody, versus an activation declined on an unlocked session) in seconds instead of waiting out a two-minute paint ceiling; key delivery, the advisory readiness reading and the product's pre-creation hold are unchanged.
- proto/daemon: the five fault kinds the daemon opens and neither session surface could carry — `conversation_abandoned`, `session_absent`, `watch_open_refused`, `daemon_state_unreadable` and the workspace-scoped `adoption_window_expired` — have arms on `SessionFault` and `HostFault`, so a fresh bring-up's standing fault no longer costs the editor every host view of that workspace.
- proto/daemon: `DaemonFault` spells `daemon_state_unreadable`, retiring the last site (`health.selfCheckFault`) that put a fault on the wire with its `kind` oneof unset.
- proto/daemon: `CreateWorkspaceError.spawn_failed` lands, so the one refusal both a create and an open raise is answered in band by both instead of out of band by one.
- proto/daemon: `ForgetWorkspace` lands whole — endpoint, rpc and handler — for a verb whose refusals and command-file route were already built with no wire to answer on; `server.setArm` learned repeated fields so `has_children` names the children instead of dropping them.

## Standing measurements to protect

- Cold start 1.826s-2.7s from spawn to usable across the current (2026-09-12) regime, historical high 3.479s from an early cold-cache run; every phase measured (n=26).
- Panels paint 22ms-409ms after a single focus edge (n=24), tighter than the previously recorded 216ms-1.5s.
- Full elisp suite 4152 tests, ~36s.
