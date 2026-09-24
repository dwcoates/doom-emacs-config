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

The editor is launched without taking focus, and NO pictures are taken at this
stage. If key delivery to an unfocused Emacs is impossible on macOS, that is
surfaced to the owner before any alternative is chosen.

FOCUS IS STOLEN ONCE AND HANDED BACK ONCE (owner ruling, 2026-09-13). It used to
be per press: every keystroke activated Emacs, posted, and reactivated whatever
had been frontmost, so a sweep flickered the owner's desktop dozens of times and
nothing on the screen said whether a run was still going. The sweep now brings
Emacs forward once, before its first realtest, and keeps it there for the whole
run — so the owner can watch it — and the application that was frontmost before
the sweep gets focus back once, at the end. That handback runs from
`bin/realtest.sh`'s EXIT trap, so a failure, a panic and an interrupt all return
the desktop, and the desktop coming back is how the owner knows the sweep is
over. `bin/realtest.sh -run <one realtest>` behaves exactly the same way.

There is exactly one launch method, `open -g` (owner ruling, 2026-09-11): it
asks LaunchServices not to bring the application forward, and it never brings
Emacs forward and then corrects for it. An earlier method that spawned the
bundle's executable directly and reactivated the owner's application afterward
was removed once realtest 1's first run traced a focus theft under `open -g`
itself to this module's own webview pre-creation on link-up, not to the launch
method (fixed in commit 3db3d6271; see docs/REALTEST-JUDGEMENT-CALLS.md,
realtest 1, row 24). A realtest starts Emacs once per test.

## The loop, for every realtest

COLLECT EVERYTHING BEFORE REMEDIATING ANYTHING. The owner's standing rule
(2026-09-12) governs every realtest and every section, not one wave: run all
the acts, and all the realtests of the section, gathering findings, and only
then remediate them together. Fixing one finding before looking at the next
costs a merge, a deploy and a rerun for each, and it hides the shared invariant
that several findings usually have in common.

A realtest therefore COLLECTS its assertion failures and keeps going
(`t.Errorf`) rather than stopping at the first (`t.Fatalf`). A hard stop is
reserved for a precondition whose absence makes everything after it
meaningless.

1. Run every realtest in the section. Each one collects, none of them stops
   early, and a failing one does not stop the next from running.
2. Determine issues and potential issues from the logs: every WARN and ERROR
   in Emacs `*Messages*`, the module log, every workspace's elisp sink,
   `daemon.run.log`, the store, the sidecar and the shim — whether or not it
   relates to the test's subject — plus anything that reads wrong (a slow
   phase, a workspace missing, unexpected modeline text).
3. Surface the WHOLE set, with evidence, verbatim. Nothing is fixed here.
4. The owner decides what is remediated and how (sections 1 and 2 are the
   lead's to decide alone; see the autonomy note below).
5. The lead remediates the whole set in one wave, grouped by invariant, then
   redeploys master.
6. The owner tests and confirms on their editor.
7. The next section runs. Only then.

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
  `events.db` to timestamped siblings and report the paths. ONCE PER RUN, not
  once per realtest: the copy is of the state as it was before the run began.
- Refuse to run unless every deployed system is at master's HEAD (the
  readiness report is the judge) and no human is using Emacs.
- Set BOTH consents. `AGENT_REPL_REALTEST_TAKEOVER=1` lets the run quit the
  standing editor; `AGENT_REPL_REALTEST_STOP_DAEMON=1` lets it stop the
  standing daemon. The second one is needed on EVERY sweep, not only when
  realtest 3 is asked for: the previous sweep's handback deliberately leaves
  the owner a guard-free daemon, so a sweep always opens against a daemon that
  cannot be adopted. With the consent the run quits the editor first and then
  stops that daemon itself through the daemon's own door
  (`UpdateShutdownSchedule{now}`, with SIGTERM as a stated fallback and never
  SIGKILL) and says which pid it stopped; without it the preflight declines and names the variable. THE SHIMS
  THAT DAEMON LEFT LISTENING GO WITH IT, under the same consent: a shim outlives
  the daemon that spawned it and the run's own daemon would adopt it, so after
  the daemon is gone each unguarded shim and `shim-lock` is SIGTERMed and stated
  by pid and socket. Without the consent that refusal stands and names the
  variable as the remedy.
- After the run the owner is left with a GUARD-FREE editor and the stack
  untouched. A run's own editor forbids the vendor; the handback quits it and
  cold-starts a normal one in its place (see "Running a sweep", "The editor the
  owner gets back"). The REGISTRY is untouched with it: every workspace the run created is closed
  and forgotten through the daemon before the run directory goes away, and a
  row that survives fails the run (see "Running a sweep", "The leftovers").
- The DESKTOP is left as the run found it too. The sweep takes focus before its
  first realtest and gives it back from its EXIT trap; expect Emacs to be
  frontmost for the whole run and the previous application to come back when it
  ends. Clicking away mid-run is allowed: the next press re-takes focus and says
  so in its receipt.

## Running a sweep

`bin/realtest.sh` takes realtests by number or by name — `bin/realtest.sh 2 3
4`, `bin/realtest.sh -run TestRealtestForkAWorkspace` — and with no argument
runs the whole set. A sweep is SEQUENCED: one `go test` invocation per
realtest, with the world each one's precondition demands established between
them, from the world table in the script. No test's own refusal is relaxed by
this; the runner is what gets out of their way.

- **The editor.** A cold-start realtest (1, 5, 6, 7, 8) gets a socket with
  nothing answering: the runner quits what the previous realtest left.
  Realtests 2 and 4 keep the standing editor, because a restart and an
  adoption are what they measure. `AGENT_REPL_REALTEST_TAKEOVER=1` is one
  answer for the whole run, and the refusal says how many quits the plan holds
  before any of them happens.
- **The focus.** The sweep steals it ONCE at the start (Emacs frontmost, so the
  owner can watch) and hands it back ONCE at the end, from the EXIT trap, so a
  failed, panicked or interrupted sweep still returns the desktop. No press
  hands focus back on its own any more; a press that finds Emacs not frontmost
  — the owner clicked away, or the realtest just cold-started a new Emacs —
  re-takes it and records that in its receipt. A desktop that will not
  cooperate (a declined activation, a locked screen) is REPORTED and the sweep
  runs anyway.
- **The daemon.** Realtest 3 needs none running, and stopping the owner's
  daemon is a consent of its own: `AGENT_REPL_REALTEST_STOP_DAEMON=1`. Without
  it realtest 3 is SKIPPED with the reason rather than run into its refusal.
  The stop is the daemon's own `UpdateShutdownSchedule{now}`, so it stands its
  sessions down on the way out; SIGTERM is the stated fallback when nothing
  answers that door, and a daemon that ignores both is a skip too.
- **The editor the owner gets back.** Every editor a realtest launches carries
  `AGENT_REPL_FORBID_VENDOR_CALLS`, so the one left standing at the end is never
  the one the owner should keep: until 2026-09-13 it was, and the owner's real
  workspaces spent the hours after a sweep talking to the FAKE vendor through
  the daemon that editor spawned. From the same EXIT trap, after the focus
  handback, the run reads the standing editor's environment from the kernel; a
  GUARDED one is quit (the same consented takeover path), a guarded DAEMON is
  stopped under `AGENT_REPL_REALTEST_STOP_DAEMON=1` — or LEFT and said loudly
  without it — and a normal editor is cold-started with `open -gj -a Emacs` and
  the guard removed from its environment. A guard-free editor is left exactly
  alone. The run names the editor the owner gets back before it starts and
  again at the end: *the owner's editor was restored: guarded Emacs pid N quit,
  guarded daemon pid M stopped, a guard-free Emacs launched*. None of it
  changes the sweep's verdict. A run that quit the owner's editor and then
  ENDED WITHOUT ONE ANSWERING — a preflight decline after the quit, most of all
  — still cold-starts a guard-free editor: "nothing answering, nothing to hand
  back" applies only to a run that never quit one. A deploy cannot carry the
  guard across either: the daemon's own deploy hands over to a successor
  spawned with the INCUMBENT's environment, so a guarded daemon is replaced by
  a guarded one and the owner's daemon by an unguarded one.
- **The leftovers.** A realtest that registers, creates or forks a workspace
  puts a row in the OWNER'S registry naming a directory under the run
  directory, and the row outlives the directory. A sweep therefore DECLINES at
  its start when any row names a directory under `~/.claude-emacs/realtest/`
  (listing them, with the one-line remedy `bin/realtest.sh --clean-leftovers`),
  each act realtest ends by removing its own rows through the daemon and FAILS
  for any that survive, and the sweep does the same over its whole run
  directory from an EXIT trap — so a realtest that failed, a panic and an
  interrupt all reach it. **The owner's state at the end of a run is the state
  it was in at the start**; until 2026-09-13 it was not, and the owner watched
  their editor report a stale registry row for hours afterwards.
- **The gap between sweeps.** Each realtest harvests its own window, so
  anything written while no realtest was running — a deploy restart, a boot
  catch-up, the owner's own use of the editor — was read by nothing. A sweep
  now opens by scanning from the previous sweep's end to now, across every
  source the harvest already knows plus the live editor's `*Messages*` and
  `*Warnings*` buffers, and writes what it finds verbatim into
  `between-sweeps/MANIFEST.md` under "## Between sweeps". Those findings are
  held to the SAME bar as an in-window finding — no allowlist — and make the
  sweep exit non-zero. They do not block it: the realtests still run, so one
  run gathers every finding.
- **What the exit status means.** 0 — everything asked for ran and passed, the
  run left no registry row behind, and nothing was written between the sweeps.
  78 — what ran passed, but at least one realtest never ran (each skip's reason
  is printed). 77 — DECLINED, nothing ran at all. Anything else is a realtest
  failing, a registry row the sweep could not remove, or a between-sweeps
  finding; a failure does not stop the sweep: the rest still run, so one run
  gathers every finding.

## The set

Startup and shape — COMPLETE
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

Workspaces — COMPLETE
4. Switch between workspaces with `s-{`, `s-}` and `M-<n>`. Selection, tab
   highlight, panel and composer all follow.
5. Create a workspace (`SPC TAB n` — it asks "Initial prompt: " and nothing
   else; the repository is the one the current workspace sits in and the daemon
   mints the name), work in it, delete it. The tab appears, is selected,
   disappears; the user lands somewhere sensible.
6. Register a directory (`SPC TAB C-n` — "Add project directory: "); re-open a
   closed workspace (`SPC TAB O` — "Open workspace: ").
   Register a REPOSITORY on its own (`SPC j .` — "Register repository from
   file: ", which takes any file inside it): the rail draws the repository as a
   section with no rows, and `SPC TAB N` can then pick it.
7. Fork a workspace with its conversation (`SPC TAB f` — "Initial prompt: "
   alone, like every dynamic mode). The fork carries the history.
8. Reorder by priority; close, reopen (`SPC TAB O`), kill a workspace.

   The creation keys are the owner's 2026-09-12 ruling, recorded in
   docs/REALTEST-JUDGEMENT-CALLS.md: `n` dynamic create, `N` static create
   (the one mode that asks "Repository: " then "Name: "), `c`/`C` the child
   variants, `f` fork, `o` one-shot ("One-shot commission: "), `O` re-open.
   `lisp/keybindings.el` and `lisp/verbs.el` are the truth.

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
19. Deploy from master while Emacs runs (`daemon/bin/claude-repld deploy`, or
    `M-x agent-repl-deploy`). What the user sees during the bounce.

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

Sweeps are named by their run directory stamp under `~/.claude-emacs/realtest/`.
The three most recent are 2026-09-13 13:31 (rt-run21), 13:4x (rt-run22) and
13:5x (rt-run23), all run under the once-per-sweep focus policy (owner ruling,
landed 2026-09-13). Every sweep opens with the BETWEEN-SWEEPS GAP SCAN, which
holds the window since the previous sweep ended — when no realtest was running
and the editor was the owner's — to the same bar as a run window, with no
allowlist. It reported 0 findings in rt-run22 and rt-run23, and exactly one in
rt-run21: the sidecar's aged-unowned-spool `hold-expired` warning, which awaits
the owner's ruling.

Realtests 1–8 ALL PASSED with a clean harvest in rt-run21. In rt-run22 all
passed except 6, whose chord was lost under a flood of `<triple-wheel-up>`
mouse events in the key ring (the owner scrolling over Emacs mid-run; not a
product finding); 6 then passed alone in rt-run23. ALL EIGHT are TWICE-GREEN as
of 2026-09-13.

| # | status | ruling |
|---|---|---|
| 0 | done (cc93abb9d) | retire the old playtest layer |
| 1 | PASSED 2026-09-13 (rt-run21 and rt-run22), clean harvest both times — TWICE-GREEN | `TestRealtestStartTheEditor` |
| 2 | PASSED 2026-09-13 (rt-run21 and rt-run22), clean harvest both times — TWICE-GREEN | `TestRealtestRestartWithTheDaemonUp` |
| 3 | PASSED 2026-09-13 (rt-run21 and rt-run22), clean harvest both times — TWICE-GREEN | `TestRealtestStartWithTheDaemonDown` |
| 4 | PASSED 2026-09-13 (rt-run21 and rt-run22), clean harvest both times — TWICE-GREEN | `TestRealtestSwitchBetweenWorkspaces`; the three-tab bar the reordering finding asked for |
| 5 | PASSED 2026-09-13 (rt-run21 and rt-run22), clean harvest both times — TWICE-GREEN | `TestRealtestCreateWorkDeleteAWorkspace`; C-g resolved by the once-per-sweep focus policy (docs/REALTEST-JUDGEMENT-CALLS.md, "The vanishing C-g, resolved (2026-09-13 13:31, lead)") |
| 6 | PASSED 2026-09-13 (rt-run21 and rt-run23) — TWICE-GREEN; rt-run22's chord was lost to a `<triple-wheel-up>` flood in the key ring, not a product finding | `TestRealtestRegisterAndReopen`; same C-g resolution |
| 7 | PASSED 2026-09-13 (rt-run21 and rt-run22), clean harvest both times — TWICE-GREEN | `TestRealtestForkAWorkspace`; the phantom shim adoption is gone; same C-g resolution |
| 8 | PASSED 2026-09-13 (rt-run21 and rt-run22), clean harvest both times — TWICE-GREEN | `TestRealtestPriorityCloseReopenKill`; same C-g resolution |
| 9 | TWICE-GREEN (rt-run40 00:2x, rt-run41 00:3x, 2026-09-14; gap scan clean in both; section gate `bin/test-all.sh` passed 00:47 on every layer incl. the sandbox Emacs layer) | `TestRealtestSendAPrompt`; the send is real keys throughout — `SPC o v` (a toggle: the composer is auto-selected on arrival), the prompt typed character by character, `RET` in the composer. Measured phases: send key → submit 85ms, submit → delivered 160ms, submit → concluded 1.15s, answer drawn before the terminal row (−0.7 to −1.0s). Remediated on the way: the eval transport's temp buffer in the probe, the pre-registration source set, `elisp.input.send` and feed row draws at DEBUG, the response renderer recording nothing, sidecar discovery by rescan only, the sidecar's 2m24s boot walk and 100% steady-state CPU, the daemon's closed-sink poison, the successor spawning over a starting shim |
| 10 | AWAITING A RULING | no Emacs key interrupts a turn without restarting the session (`C-u SPC o C-c` is a forced restart); the footer stop control is a webview button the key driver cannot press. The owner names the real-key path (docs/REALTEST-JUDGEMENT-CALLS.md, "Realtest 10, the interrupt key") |
| 10–14 | not yet authored; the lead's to run and remediate alone (owner ruling, 2026-09-13 evening) | Conversation — the rest of the section |
| 15–16 | not yet authored | Panels and windows |
| 17–19 | not yet authored | Daemon lifecycle |
| 20–22 | not yet authored | Failure |
| 23–24 | not yet authored | Scale |

The startup section (1–3) and the workspaces section (4–8) are COMPLETE.

The lead updates this table as each realtest runs, is ruled on, and is
confirmed.

### Running realtest 7

It is the first realtest that needs a prompt to be ANSWERED. The daemon refuses
a fork whose parent has no conversation, so realtest 7 gives its parent one
before it forks, and a submitted prompt reaches the shim's `createRealQuery`,
which is where `AGENT_REPL_FORBID_VENDOR_CALLS=1` makes the shim's own guard
throw.

NOTHING EXTRA IS STATED ON THE LAUNCH. A daemon that is itself under the guard
spawns every shim with `--fake` and never a real-vendor one
(`daemon/internal/shimclient/supervisor.go`, `fakeMode`), so the prompt is
answered from the offline scripted SDK and `createRealQuery` is never reached.
The guard used to REFUSE that spawn instead, which is what made realtests 4, 5
and 7 impossible to run: creating or forking a workspace brings a session up,
and the refusal cascaded to "the shim did not come up".

`AGENT_REPL_FAKE_SHIMS=1` remains a separate daemon test hook — it turns fake
shims on WITHOUT the guard, so a suite can exercise a real vendor call site
against a live session — and it is not needed here.

Realtest 8 needs nothing extra either: nothing in it submits a prompt, and its
reopen's bring-up spawns a fake shim for the same reason.

### Running realtest 9

It is the first realtest whose SUBJECT is a turn, and the first where text is
TYPED rather than supplied. Three things about it are worth having in hand
before a run.

- **The whole act is real keys.** `SPC o v` selects the workspace's composer,
  the prompt goes in one key event per character, and `RET` — the composer's
  own send key (`lisp/input.el`, `agent-repl-input-mode-map`, `:ni "RET"`) — is
  the act. Nothing about the send is supplied through the probe transport:
  `agent-repl-send` reads the COMPOSER rather than taking an argument, so a
  prompt inserted through a probe would be testing the send with the composer
  removed from it. The composer is read back before the send, so a dropped
  character is a stated harness finding and the turn is still judged against
  the prompt the editor actually holds.
- **The vendor guard alone answers the prompt, exactly as it does for realtest
  7.** Nothing extra is stated on the launch. The prompt names no `!scenario`,
  so the fake answers from the DEFAULT PROSE SCENARIO
  (`agent-shim/claude/shim/src/fake/scenarios/prose.ts`), which thinks on both
  arms, opens with a fixed sentence and concludes by echoing the prompt
  verbatim — a turn that thinks briefly and answers with a known text, in
  microseconds. No scenario was added for this realtest.
- **Four of the facts item 9 names have no log edge behind them**, and the
  realtest asserts what exists rather than faking them: the queue's submit door
  writes only DEBUG, the footer's resolved status is neither logged at INFO nor
  readable from elisp, nothing records the tab arm's transitions, and no log
  anywhere carries a feed row's prose. Each is recorded as a LOGGING DEFECT in
  docs/REALTEST-JUDGEMENT-CALLS.md for the lead to dispatch, and the run's own
  manifest says which assertion each one cost. THE TURN'S PHASES SHIP
  UNBUDGETED for the standing reason (`AGENTS.md`, "Test wait/timeout bounds
  are measured, not guessed"): they are measured from real log edges and
  reported, and become budgets — a small multiple of the observed healthy
  maximum, with the runs behind it named — once accumulated runs have sized
  them.

### Running realtests 2 and 3

Each has a precondition the tests themselves will not establish, and the runner
establishes both (see "Running a sweep"):

- **2 — restart with the daemon up** needs a daemon already serving. Realtest 1
  leaves one behind, and so does starting Emacs by hand; the runner cannot
  start one, so with no daemon running it SKIPS realtest 2 and says so. The
  test quits the standing editor itself — the quit is the restart it measures,
  so the runner leaves the editor alone for it — which needs
  `AGENT_REPL_REALTEST_TAKEOVER=1` exactly as the script's second refusal does.
- **3 — start with the daemon down** needs NO daemon running. The test still
  refuses and names the pid rather than stopping the owner's daemon. What
  changed is who may: with `AGENT_REPL_REALTEST_STOP_DAEMON=1` the runner quits
  the editor, stops the daemon through its own door and waits for it to go
  before realtest 3 starts, so it can be the third test of a sweep after all. Without that
  consent it is skipped, and the manual route is unchanged — stop the daemon
  deliberately (`SPC o C-d`, or kill the pid) and run realtest 3 alone.

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
