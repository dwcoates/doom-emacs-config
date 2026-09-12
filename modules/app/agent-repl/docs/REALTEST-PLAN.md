# Realtests — the plan

The owner's ruling, 2026-09-10. This document is the reference for every
realtest; a realtest that is not in it does not run.

## What a realtest is

A realtest drives the OWNER'S ACTUAL EDITOR: the one Emacs.app process on the
owner's Mac, the real `~/.config/doom` on master (theme, treemacs, dashboard,
personal bindings — the owner's environment is the fixture, never a debugging
target), the real `~/.claude-emacs` state, the real store, sidecar, shim and
daemon deployed from master, and the owner's real `~/.claude` transcripts.
There is NO sandbox and NO image. The one substitution is the vendor: the fake
SDK answers every model call (`AGENT_REPL_FORBID_VENDOR_CALLS=1` on the Emacs
process, inherited by everything it spawns) so no real Claude call can occur;
the owner may lift this per test.

Input is what the owner would send: real key events into the real Emacs
process (`s-}`, `M-2`, `SPC TAB n`, typed prompt text), never an elisp call
that performs the act. Elisp is read-only, for reading state and asserting.

The editor must not disturb the owner: Emacs is launched without taking focus,
and NO pictures are taken at this stage, so Emacs is never brought frontmost.
If key delivery to an unfocused Emacs is impossible on macOS, that is surfaced
to the owner before any alternative is chosen.

There is exactly one launch method, `open -g` (owner ruling, 2026-09-11): it
asks LaunchServices not to bring the application forward, and it never brings
Emacs forward and then corrects for it. An earlier method that spawned the
bundle's executable directly and reactivated the owner's application afterward
was removed once realtest 1's first run traced a focus theft under `open -g`
itself to this module's own webview pre-creation on link-up, not to the launch
method (fixed in commit 3db3d6271; see docs/REALTEST-JUDGEMENT-CALLS.md,
realtest 1, row 24). A realtest starts Emacs once per test.

## The loop, for every realtest

1. Run the realtest.
2. Determine issues and potential issues from the logs: every WARN and ERROR
   in Emacs `*Messages*`, the module log, every workspace's elisp sink,
   `daemon.run.log`, the store, the sidecar and the shim — whether or not it
   relates to the test's subject — plus anything that reads wrong (a slow
   phase, a workspace missing, unexpected modeline text).
3. Surface them to the owner, with evidence, verbatim. Nothing is fixed here.
4. The owner decides what is remediated and how.
5. The lead remediates as ruled, then redeploys master.
6. The owner tests and confirms on their editor.
7. The next realtest runs. Only then.

A realtest is remediated if and only if ALL warnings and errors across ALL logs
are resolved. There is no allowlist.

## Step zero, once, before the first realtest

The previous playtest layer verified the module against a bare sandbox Doom
profile with elisp-driven acts and is retired entirely: the `playtest` build
tag and every `e2e/playtest_*_test.go`, `bin/playtest.sh`, the capture and
settle substrate, `e2e/PLAYTEST-SPEC.md`, `e2e/PLAYTEST-PLAN.md`, the sandbox
playtest self-tests, and every reference in AGENTS.md files and ledgers. The
e2e and Emacs-layer suites stay.

## Before each run

- Back up `~/.claude-emacs/wsm.db` (with `-wal`/`-shm`) and the store's
  `events.db` to timestamped siblings and report the paths.
- Refuse to run unless every deployed system is at master's HEAD (the
  readiness report is the judge) and no human is using Emacs.
- After the run the owner's editor is left working and the stack untouched.

## The set

Startup and shape
1. Start Emacs cold. Time to usable; which workspaces open and when; what the
   modeline and tab bar show while waiting.

   TWO PHASES (owner ruling, 2026-09-11): `open -gj` leaves the frame
   visible-but-unfocused on this machine, not truly hidden, and the settled
   webview invariant PARKS a workspace's pre-creation in exactly that state
   rather than steal focus — so a panel does not paint until the first focus
   edge, no matter how long the hidden window runs. HIDDEN startup is
   therefore judged by a drawn tab and the pre-creation queue armed for it,
   not by a painted panel; a SHOW phase, run right after the realtest's own
   key self-test brings Emacs forward, then asserts every panel paints
   within a generous ceiling. This proves panels DO paint, just on first
   show (see e2e/REALTEST-SPEC.md, "The phases: hidden, then shown", and
   docs/REALTEST-JUDGEMENT-CALLS.md, row 40).
2. Quit and restart Emacs with the daemon still up. Same measurements; the
   daemon is adopted, not rebuilt.
3. Start Emacs with the daemon down. It is built and spawned; time and
   feedback for that path.

Workspaces
4. Switch between workspaces with `s-{`, `s-}` and `M-<n>`. Selection, tab
   highlight, panel and composer all follow.
5. Create a workspace (`SPC TAB n`), work in it, delete it. The tab appears,
   is selected, disappears; the user lands somewhere sensible.
6. Register a directory (`SPC TAB C-n`); re-open a closed workspace
   (`SPC TAB o`).
7. Fork a workspace with its conversation (`SPC TAB f`). The fork carries the
   history.
8. Reorder by priority; close, reopen, kill a workspace.

Conversation
9. Send a prompt, watch it think, read the answer. Tab arm, footer and feed
   through the whole turn.
10. Interrupt a running turn.
11. A permission ask and a multiple-choice question, each answered from the
    card.
12. A shell command; a long one that backgrounds; a detached one.
13. Attach a clipboard image and a region to a prompt; recall history in the
    composer.
14. Slash commands: clear, compact, model change, fast mode.

Panels and windows
15. Hide and reshow the panels (`SPC o c`); fullscreen toggle (`SPC w f`);
    focus the composer (`SPC o v`); rescue the webview.
16. Visit a file from a tool card; open in editor.

Daemon lifecycle
17. Restart the daemon gracefully and forced from inside Emacs. Conversation
    and selection survive.
18. Schedule a drain; shut down now. The banner, the held prompts, the
    handover to a new daemon.
19. Deploy from master while Emacs runs (`bin/deploy-all.sh`). What the user
    sees during the bounce.

Failure
20. Kill the shim under a workspace; the link severs and recovers on the next
    prompt.
21. Vendor errors: rate limit, auth failure, refusal. The footer and the
    terminal rows.
22. Hibernate an idle workspace; revive it with a prompt.

Scale
23. Two workspaces thinking at once; six panels open (the connection cap).
24. A long session: an hour of mixed acts, then the log volume and memory.

## Status

| # | status | ruling |
|---|---|---|
| 0 | done (cc93abb9d) | retire the old playtest layer |
| 1 | DONE — twice-green 2026-09-12, clean harvest | remediated; see judgement ledger rows 1-57 |
| 5 | authored, not yet run | `TestRealtestCreateWorkDeleteAWorkspace`; acts against a dedicated scratch repo under the run directory |
| 6 | authored, not yet run | `TestRealtestRegisterAndReopen`; same scratch-repo rule |

The lead updates this table as each realtest runs, is ruled on, and is
confirmed.

### Realtest 1, run 1 — 2026-09-11 12:13, FAILED

This run predates the owner's 2026-09-11 ruling that a realtest starts Emacs
once per test; it ran three cold starts, rotating between two launch methods to
find one that left focus alone. All three failed the plan's own assertion: no
tab was drawn for any of the three workspaces the state database holds. Run
record: `~/.claude-emacs/realtest/realtest-20260911-121357/MANIFEST.md`.

Phases measured (all three starts agreed to within half a second):

| phase | measured |
|---|---|
| doom-boot | 2.0–2.5 s from spawn |
| daemon spawn to serving | 10.0 s, every time — the adoption bound, not work |
| link up | ~13.5 s from spawn |

One line per distinct finding class:

- the roster push aborted on a stale workspace, so no roster ever reached the
  frontend and no tab was drawn — the run's actual failure;
- `elisp.daemon.booted` fired 3 ms after the spawn against a stale address
  file: the frontend reported a daemon it did not have;
- the 10 s daemon bring-up is an adoption bound waiting on a surviving shim
  that was never healthy, not work the daemon did;
- the shim held a permanent `storeUnreachable` fault for the whole run;
- nothing in the mode line said anything while bring-up ran, so the 13.5 s to a
  link reads to the owner as a hang;
- the vendor guard had a hole: a shim left listening by an earlier UNGUARDED
  daemon was adopted by this run's guarded daemon and submitted a keepalive
  prompt to the real vendor every four minutes (fixed — `bin/realtest.sh` now
  enumerates and refuses on every unguarded listening shim and `shim-lock`);
- two workspace-id schemes in log attribution: the shim stamps its own 8-hex id
  while the daemon-side sink is keyed by the 16-hex id, which the harvester
  reports as a broken routing invariant on every shim record — a CONTRACT
  question for the owner;
- the sidecar wrote 4988 `discover-meta` records inside the run window;
- every launch moved focus (Chrome to Emacs), including under `open -g`;
- the heartbeat warned repeatedly that the owner was not loaded;
- `elisp.host.link-up-skipped ws=none` warned during bring-up.

Harness fixes landed from this run: the vendor-guard hole above; `daemon-answered`
re-keyed off the link and the roster subscription rather than the boot claim,
with `daemon-spawned` split out; and the harvest collapsed per class with the
full record list in `HARVEST-FULL.jsonl`.
