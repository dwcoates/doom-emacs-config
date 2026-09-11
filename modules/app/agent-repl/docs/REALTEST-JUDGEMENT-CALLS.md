# Realtest Judgement Calls

## Purpose

On 2026-09-11 the owner ruled that the lead runs realtests 1 through 8
(startup and workspaces) alone and asks nothing. Every judgement call the
lead makes during those runs is recorded here instead, so the owner can
review or reverse any of them later.

The owner made two rulings directly on 2026-09-11, not delegated to the
lead:

- The workspace id contract is the lead's pick.
- A workspace whose directory no longer exists is closed automatically.

See `docs/REALTEST-PLAN.md` for run status.

## Realtest 1

| Date | Question | Decision | Why | How to reverse |
| --- | --- | --- | --- | --- |
| 2026-09-11 | Whether to restart the daemon under the vendor guard without asking | Quit the owner's Emacs (idle, no unsaved work known) and let the realtest's Emacs spawn a guarded daemon | The realtest needs a guarded daemon and the owner's session had nothing to lose | None needed, it is per-run |
| 2026-09-11 | An orphan shim (pid 94292, spawned 2026-09-10 without the vendor guard, its daemon gone) was submitting a keepalive prompt to the real vendor every four minutes | Killed it with SIGTERM | An unguarded shim talking to the real vendor is exactly what the guard exists to prevent | Not applicable, the next daemon spawns a fresh guarded shim |
| 2026-09-11 | Workspace id contract (shim's 8-hex hash vs daemon's 16-hex id) | The daemon's 16-hex id is the id on every record and sink; the shim's hash survives as a separate context key | One canonical id avoids two id spaces colliding in logs and sinks | Swap the key names in the shim's logger and sink naming |
| 2026-09-11 | A daemon adopting a surviving shim waited only for `healthy` | Any diagnostics frame counts as answered; unhealthy shims are adopted and their faults surfaced through the workspace-health path | Waiting on healthy-only blocked adoption of a shim that was alive but degraded | Restore the healthy-only wait in awaitHealthy |
| 2026-09-11 | A log call that cannot route its workspace SIGNALED into the caller | It records the routing error and writes the original record globally, never signals | A logging failure must not abort the caller's own work | Restore the signal in agent-repl--emit-log-record (not recommended, it aborted the roster) |
| 2026-09-11 | Roster apply loop aborted on one bad workspace | Per-workspace isolation, failures logged, the rest still opened | One bad workspace should not block every other workspace from opening | None |
| 2026-09-11 | Stale daemon.addr | Removed before spawn; boot wait refuses an address it judged stale; daemon removes its addr on shutdown | A stale address left the boot wait connecting to nothing | None |
| 2026-09-11 | No mode-line feedback during daemon bring-up | New lifecycle states on the existing daemon segment (starting, linking, ready) plus a workspaces-opening n/m count | The owner had no way to tell bring-up was in progress versus stalled | Remove the states |
| 2026-09-11 | Shim `storeUnreachable` fault permanent | Transient store faults resolve on the next success; per-site permanence table kept in the shim commit | A transient fault marked permanent never clears even after the store recovers | Drop the resolveComponent calls |
| 2026-09-11 | Sidecar discover-meta flood | Workflow-agent meta shape is legitimate and attributed to its run; holds are state (warn on first and on reason change, debug on repeat, info on release, periodic summary) | The flood was real traffic, not a bug, so it needed leveling rather than suppression | Revert the two sidecar commits |
| 2026-09-11 | Sidecar forward failure during daemon boot | Six-rung retry ladder, undeliverable records kept in the global sink | A single retry or immediate drop lost records during the boot window | Revert |
| 2026-09-11 | Harvest manifest unreadable (36k lines) | Identical finding classes collapsed to one line with count and sample, full list in HARVEST-FULL.jsonl | 36k lines of duplicate findings made the manifest unusable | Revert harvest.go collapse |
| 2026-09-11 | `daemon-answered` phase keyed on a false `booted` record | Re-keyed to link up plus roster subscribed; new `daemon-spawned` phase | The old key fired before the daemon was actually answering | Revert phases.go |
| 2026-09-11 | Vendor guard checked only the daemon | Preflight also declines on unguarded surviving shims and shim-locks | A guarded daemon paired with an unguarded surviving shim still reached the real vendor | Revert realtest.sh preflight |
| 2026-09-11 | Per-instance daemon workspace log generations hid the previous instance | A new instance appends to the current sink under the cap | Rotating to a new generation on every instance lost the previous instance's tail | Revert dlog change |
| 2026-09-11 | Focus moved to Emacs on launch under `open -g` | The cause is inside our own module: webview pre-creation on link-up (lisp/webview-recovery.el → frontend.el) creates a native WebKit view, which macOS answers by activating Emacs regardless of `open -g`. The pre-creation drain parks while Emacs is known unfocused and resumes on the focus edge (commit 3db3d6271) | The mechanism is inferred from the code path and confirmed only by the rerun | Revert that commit |
| 2026-09-11 | Heartbeat `owner-not-loaded` warns and `link-up-skipped ws=none` warn | The keys `:readiness-poll` and `:workspace-status-export` named arm functions from modules deleted in commit 79faac1c7, so the assertion could never be satisfied. The two keys were removed from the required-timer contract while the assertion and its warning were kept (commit 148e32948) | The assertion was checking for arm functions that no longer exist | Restore the keys only together with owners |

## Pending

Entries 4 through 17 above describe dispatched fixes whose outcome is not
yet known. Their outcome is recorded here once each fix merges.
