---
name: profile-agent-repl
description: Profile a live agent-repl across every runtime at once — CPU and memory of the daemon, store, sidecar, every shim, Emacs and the webapp's pages and WebKit processes — and rank the user-visible latencies (workspace open, switch, first switch to a never-shown workspace, page load, prompt echo) against how perceptible each is; run the startup assessment; and keep the living list of perf-sensitive areas. Use when the user asks why agent-repl is slow, hot, heavy on memory or battery, wants a profile or a perf assessment, cares about how fast something feels, or invokes /profile-agent-repl.
argument-hint: "<start [--log-level LEVEL]|stop|status|report [--startup RFC3339] [--level LEVEL]|startup-assessment|switch-bench|list> or a plain-English request"
lineage_root: user.dodge.skills.profile-agent-repl
---

# Profile agent-repl

## What This Skill Does

One profiling session covers every runtime, and lands in one directory:
`~/.cache/agent-repl/profiles/<session>/` (`AGENT_REPL_PROFILE_ROOT`
overrides the root). `bin/profile-agent-repl.sh` opens and closes sessions and
reports on them; each runtime profiles itself without a restart.

| Runtime | How it is reached | What it writes |
|---|---|---|
| daemon | SIGWINCH, to the pid in `armed/daemon-<pid>` | `daemon-<pid>.cpu.pprof`, `.heap.pprof` |
| store | SIGWINCH, `armed/store-<pid>` | `store-<pid>.cpu.pprof`, `.heap.pprof` |
| sidecar | SIGWINCH, `armed/sidecar-<pid>` | `sidecar-<pid>.cpu.pprof`, `.heap.pprof` |
| every shim | SIGWINCH, `armed/shim-<pid>` | `shim-<pid>.cpuprofile` (Chrome DevTools), `.heapprofile` |
| Emacs | `emacsclient --eval (agent-repl-profiling-reconcile)` | `emacs-<pid>.{cpu,mem}.elprof` (`M-x profiler-find-profile`), `.{cpu,mem}.txt` (reversed call tree) |
| webapp pages | injected by Emacs's reconcile through the webview read channel | `webapp-<workspace>.lag.json` (main-thread stalls) |
| every process | the collector's sampler | `samples.jsonl` (CPU-seconds and RSS every 2s, WebKit included) |

A session opened with `--log-level debug` (or `warn`, `error`) also raises
every runtime above to that level for the session: the session's `log-level`
file is read with `active`, the Go runtimes and every shim open a log level
window live, and Emacs opens its own and tells the webhost, which tells every
page (`logLevel`, replayed to a page that says hello later). The window is
`proto/vocab/log-level-window.json`'s: at most five minutes, ended by the stop
or by itself, whichever comes first. Each runtime records the window opening
and closing (`*.profiling.level_opened` / `level_closed`,
`webapp.log.level-window`).

The contract every runtime answers identically is
`proto/vocab/profile-session.json`: the root, the `active` file naming the open
session, the `armed/` markers, the signal, and the 30-minute window after
which every runtime stops by itself.

## Arguments

| Argument | Behaviour |
|---|---|
| `start` | Open a session: `bin/profile-agent-repl.sh start [--interval S] [--log-level debug\|warn\|error]`. |
| `stop` | Close it; every runtime writes its profiles: `bin/profile-agent-repl.sh stop`. |
| `status` | The open session, armed runtimes, stale markers: `bin/profile-agent-repl.sh status`. |
| `report [SESSION] [--startup RFC3339] [--level LEVEL]` | Summarize a session (newest by default) into `<session>/summary.md`, from its logs at `--level` up (info by default), kept beside it in `logs.jsonl`. |
| `startup-assessment` | The startup procedure below. LEAD SESSION ONLY. |
| `switch-bench` | The switch bench below: a slow switch tour and a fast one, at debug, measured phase by phase in one run. LEAD SESSION ONLY, and only with the owner's agreement to that run. |
| `list` | Read or update the perf-sensitive areas below. |

## Steps

1. Prepare.
  - a. Hold merge-queue rollouts for the whole session (`/merge-queue hold-rollouts`, released with `/merge-queue release-rollouts`; being added on a separate branch). A landing would roll out a new build mid-session and replace the processes being profiled.
  - b. Profile at `info` unless the diagnosis needs the debug records themselves. The latency table reads INFO edges only, so a latency that seems to need debug is a finding: find out why the INFO record is missing and fix the logging. Open the session with `--log-level debug` when the records around the edges are the evidence (the switch bench does), and read its report with `--level debug`.
  - c. Check `bin/profile-agent-repl.sh status`. Every runtime you expect should be `armed`. A missing one predates profiling or is not running; a `stale` one is a process that died without removing its marker.

2. Profile.
  - a. `bin/profile-agent-repl.sh start`. Every line of its output says what each runtime did (`signaled`, `skipped ... and why`, `emacs: started <session> pages=N`).
  - b. Have the user do the interaction being measured, or reproduce it. Keep a session to the interaction; minutes, not hours.
  - c. `bin/profile-agent-repl.sh stop`, then give the runtimes a second to write their profiles.
  - d. NEVER LEAVE A SESSION OPEN. A profile costs CPU on a battery. Every runtime stops by itself after 30 minutes, but that is a backstop for a dead collector, not a way to end a session.
  - e. A shim that exits mid-session writes no profile. Close the session before standing a shim down.

3. Read the results: `bin/profile-agent-repl.sh report`.
  - a. **Processes** ranks every process by CPU-seconds with its average and peak RSS, and sums per runtime. This is the battery view. WebKit processes are every app's (`webkit:*`); close Safari for a clean reading.
  - b. **Hottest frames** per runtime: `go tool pprof -top` for the Go runtimes (`go tool pprof -http=: <file>` to explore), self samples and self bytes for the shim (load `.cpuprofile` into Chrome DevTools for a flame chart), Emacs's reversed call tree, and each page's main-thread stalls.
  - c. **User-visible latency** pairs real INFO log edges per interaction, ranked by how far its p90 is over the budget a user perceives:

    | Interaction | Start edge | End edge | Budget |
    |---|---|---|---|
    | prompt echo | `elisp.input.submit` | webapp `feed.draw-user-prompt` | 100ms |
    | switch to a shown workspace | `elisp.host.editor-switch` | webapp `page.shown-frame`, after `elisp.panels.webview-on-switch` (view `same`/`fresh`) | 100ms |
    | first switch to a never-shown workspace | `elisp.host.editor-switch` | the later of `elisp.panels.webview-on-switch` (view `first`) and webapp `feed.page-placed` | 100ms |
    | page boot to first feed paint | webapp `main.boot` | webapp `feed.page-placed`, same connection | 300ms |
    | workspace open | `elisp.roster.tab-open` | daemon `daemon.workspace.bring_up` "the session is up" | 300ms |
    | prompt to first response text | `elisp.input.submit` | webapp `feed.draw-text-block` / `feed.draw-response` | 1s (vendor-bound) |

    A short latency on an interaction the user is waiting on matters more than a long one in the background: rank by the ratio, never by raw milliseconds. An interaction the session did not exercise is named as such.
  - d. **Switch to a shown workspace, by phase** splits each such switch at its edges, so a slow switch names its owner:

    | Phase | Ends at |
    |---|---|
    | Emacs, before its switch pass runs (its redisplay and hooks) | `elisp.panels.switch-activation-won` |
    | Emacs's switch pass, to the webhost told to show the page | `elisp.webhost.show` (recorded at the call) |
    | the webhost, to the page told it is shown | webapp `page.switched-to` |
    | the page, to its first shown frame | webapp `page.shown-frame` |
  - e. Write what the session found into the perf-sensitive list (step 6).

4. Startup assessment (`startup-assessment`). LEAD SESSION ONLY: a subagent never launches, quits or drives Emacs.
  - a. Hold rollouts (step 1a). Note the instant, then quit and launch the real Emacs.app the user uses, the way they launch it.
  - b. Let every workspace load; do not touch anything.
  - c. Switch through every workspace in turn, once each (each is a first switch), then once more each (repeat switches).
  - d. `bin/profile-agent-repl.sh report --startup <the launch instant, RFC3339>`: it adds launch to the daemon's first record, each workspace's session up and page painted, launch to every workspace loaded, and every switch with first/repeat marked. A profiling session open across the launch adds the processes and frames.
  - e. All timings come from real log edges. Never time with the harness, a stopwatch or the tool-call clock.

5. Switch bench (`switch-bench`). LEAD SESSION ONLY: a subagent never drives Emacs. It takes over the owner's live editor while it runs, so it runs only when the owner has agreed to that run.
  - a. Hold rollouts (step 1a). The bench does not hold them itself, since a script releasing a hold could release someone else's; it detects a rollout instead (the daemon or Emacs replaced) and fails the run as INVALID.
  - b. `bin/profile-agent-repl.sh switch-bench [--slow N:SECONDS] [--fast N:SECONDS]`. Defaults: slow is 8 switches each way 1.5s apart, a pace where each switch settles; fast is 40 each way 0.25s apart, a held key's pace, where switches overlap and pressure shows. The two together must fit the log level window; the bench refuses tours that would outlast it.
  - c. The bench opens a session at debug, drives each tour through Emacs's own key loop (`(agent-repl-profiling-switch-tour N INTERVAL LABEL)`, answered by `(agent-repl-profiling-switch-tour-state)`), cancels a tour past twice its planned length plus a margin, closes the session, and writes the report from the session's debug logs. Exit 0 is a complete, valid run; it prints the summary's and the logs' paths.
  - d. Read the report's **Switch tour slow** and **Switch tour fast** sections. Each gives keys queued, editor switches, switch passes run and superseded (a pass a later key replaced before it ran), pages told shown and first shown frames, then the tour's own phase table (step 3d), its main-thread stalls (`webhost.stall`) and its rail draws (`sidebar.roster-draw`). The fast tour's superseded passes and stalls against the slow tour's show where pressure builds.
  - e. One tour on its own, outside a bench: `(agent-repl-profiling-switch-tour N INTERVAL LABEL)` through `/runtime-eval-code` in an open session; `(agent-repl-profiling-switch-tour-cancel)` stops it early.
  - f. Record the result in the perf-sensitive list (step 6).

6. Keep the perf-sensitive list current.
  - a. WHENEVER AN AREA BELOW COMES UP WITH THE USER, OR THE USER SHOWS A VESTED INTEREST IN HOW FAST SOMETHING IS (or in optimizing it or code like it), add an entry or update the existing one in the same turn. Measurements replace estimates as soon as they exist; keep the old number with its date when it moved.
  - b. Every entry has every field of the standard below; `unknown` is an honest value, a missing field is not.

## Perf-sensitive areas (living list)

**Entry standard.**
- **Area**: the operation, named as the user would.
- **Interaction**: what the user does and sees, start to end.
- **Budget**: the perceptibility budget and why (keystroke 50-100ms, navigation 100ms, open 300ms, background work: seconds).
- **Measured**: the latest number, its date, and its evidence (a report section, log edges, a profile file); `owner-observed` when it is the user's own report.
- **Code paths**: where the time is spent or owned.
- **Contributors**: known causes, measured or suspected (say which).
- **Status / next step**: open, in progress, fixed (with the commit), or ruled out.
- **Flagged by**: who raised it, when, and why it matters to them.

### First switch to a never-shown workspace
- **Interaction**: switch keystroke to that workspace's page painted and interactive.
- **Budget**: 100ms (a navigation the user waits on; a blank page is its worst case).
- **Measured**: 1s+, owner-observed 2026-10-12. The collector's "first switch" row measures it from now on.
- **Code paths**: Emacs webview creation (frontend.el), the page's boot (webapp `main.boot`), the first feed page (daemon OpenFeed, webapp `feed.page-placed`).
- **Contributors**: the page loads every page of feed history instead of the most recent one (owner, suspected); other webapp boot work and daemon-side waits on first load are unmeasured.
- **Status / next step**: open, HIGH PRIORITY. Owner's direction: (1) preload a workspace's page before the user switches to it, scheduled so Emacs never locks up preloading many at once (one at a time, at idle, likely-next first); (2) load only the most recent feed page per workspace, and find any other boot work or daemon waits that block the first paint.
- **Flagged by**: the owner, 2026-10-12, as the switch they feel most.

### Switch to an already-shown workspace
- **Interaction**: switch keystroke to the workspace's view visible.
- **Budget**: 100ms.
- **Measured**: 2026-10-15, from 132 switches' log edges (nearest-event joined, before the phase table existed), switch request to the page's first shown frame: 170ms median, 308ms p90. By phase, medians: Emacs before its switch pass about 88ms (57ms of it Emacs's own redisplay and hooks, `waited-ms`), the webhost to the page told about 37ms, the page to its first frame 58ms (201ms p90).
- **Code paths**: host.el's switch, panels.el's deferred switch pass (it tells the webhost to show the page), the webhost's show, the page's shown handlers.
- **Contributors**: measured: the switch pass waits on Emacs's redisplay before the webhost hears of the switch; main-thread stalls overlap about 20% of switches (web process memory pressure, investigated separately). Suspected: the page's shown handlers (animations resumed, repaint measuring).
- **Status / next step**: open. Run the switch bench for an exact phase baseline at a settled pace and at a held key's pace, then tell the webhost at the switch itself rather than after Emacs's redisplay.
- **Flagged by**: the owner, 2026-10-12 (workspace switching UX).

### Workspace open
- **Interaction**: opening a workspace (tab) to its session up and usable.
- **Budget**: 300ms.
- **Measured**: test fixture register+open 47ms of daemon wall time at info, 2026-10-12 (scratch profile); the user-visible span is unmeasured.
- **Code paths**: daemon RegisterWorkspace/OpenWorkspace, Fleet.start, the shim spawn, sessionwatcher.Start.
- **Contributors**: a git subprocess on register; the shim's ~70ms start; the sidebar and footer re-render.
- **Status / next step**: open; measure with the collector's "workspace open" row.
- **Flagged by**: the owner, 2026-10-12 ("how quickly workspaces are opened").

### Startup to every workspace loaded
- **Interaction**: launching Emacs.app to every workspace's session up and page painted.
- **Budget**: seconds, but every workspace the user reaches for before it is loaded is a first-switch wait.
- **Measured**: unknown; run the startup assessment (step 4).
- **Code paths**: config.el load, the daemon boot and bring-up, each workspace's session and page.
- **Contributors**: unknown.
- **Status / next step**: open; run the startup assessment with rollouts held.
- **Flagged by**: the owner, 2026-10-12.

### Sidebar re-render per state change
- **Interaction**: every state change of any workspace re-renders every roster row.
- **Budget**: background, but it is CPU on every change and grows with the number of workspaces.
- **Measured**: 4 debug records per row per render; sidebar records were 26% of a daemon's records in a 58-test sample, 2026-10-12.
- **Code paths**: daemon/internal/resolve/sidebar (render, sectionRows, row, statusArm).
- **Contributors**: whole-roster re-render and per-row decision logging on unchanged rows.
- **Status / next step**: proposed: log a row's decisions only when its result changes; awaiting the owner.
- **Flagged by**: found by profiling, 2026-10-12.

### Log volume at info
- **Interaction**: none directly; CPU and disk on every record, on battery.
- **Budget**: background.
- **Measured**: in 3h of the owner's use, 2026-10-12: webapp `sidebar.drawn-status` 8,931 records, store `store.rpc.open-agent-session` 7,865, daemon `daemon.feed.row_placed` 6,399 and `row_republished` 5,479.
- **Code paths**: webapp sidebar drawn-status logging, store RPC logging, daemon feed resolver.
- **Contributors**: per-event records at info that are hot-path diagnostics (the logging contract puts those at debug).
- **Status / next step**: open; re-level the hot per-event records to debug where they are hot-path diagnostics.
- **Flagged by**: found by profiling, 2026-10-12.

### Debug-level log volume
- **Interaction**: none; a debug window's cost.
- **Budget**: background.
- **Measured**: at debug, logging was 56% of a daemon's CPU over 30 register+open cycles (1.07s against 0.47s at info), 2026-10-12.
- **Code paths**: daemon/internal/dlog (one write(2) per record, plus the terminal mirror).
- **Contributors**: record count (the sidebar's repeats above); one synchronous write per record.
- **Status / next step**: open; the sidebar fix above cuts most of it.
- **Flagged by**: found by profiling, 2026-10-12.

### Daemon resident memory
- **Interaction**: none; memory pressure and battery.
- **Budget**: unknown; it looked large for the workload.
- **Measured**: 509MB RSS for the owner's live daemon, 2026-10-12.
- **Code paths**: unknown; take a heap profile with this skill.
- **Contributors**: unknown.
- **Status / next step**: open; profile a session and read `daemon-<pid>.heap.pprof`.
- **Flagged by**: the owner, 2026-10-12 (production memory todo).

### Shim start
- **Interaction**: every shim spawn: opening or reviving a workspace.
- **Budget**: part of workspace open (300ms).
- **Measured**: 70ms wall, 2026-10-12 (17ms compiling the bundle, the rest node built-ins); 108ms before the protobuf registry guard.
- **Code paths**: agent-shim/claude/shim/dist/main.js load.
- **Contributors**: bundle compile; node built-in loads.
- **Status / next step**: little left; NODE_COMPILE_CACHE trims the compile.
- **Flagged by**: found by profiling, 2026-10-12.

### Test runs on the RAM disk
- **Interaction**: none for the user; wall time and CPU of every full test run.
- **Budget**: as fast as the machine allows.
- **Measured**: utility-tier fsync on the run's RAM disk p90 166ms, p99 330ms during a run, against 0.5ms at the default tier, 2026-10-12.
- **Code paths**: testrun/internal/ramdisk (UnitIOTier), bin/background.sh.
- **Contributors**: macOS throttles utility-tier I/O whenever higher-priority I/O hits the same device; the higher-priority writer on the RAM disk is unidentified.
- **Status / next step**: open; identify the writer with root `fs_usage` (owner keeps the utility tier and the RAM disk).
- **Flagged by**: the owner, 2026-10-12.

## Notes

- **CRITICAL: Never leave a session open.** Stop it when the interaction is done.
- **CRITICAL: Only the lead session runs the startup assessment** or otherwise launches, quits or drives Emacs.
- **IMPORTANT: Profile with rollouts held,** so a landing cannot replace the profiled build mid-session.
- **IMPORTANT: Read latencies only from real log edges** (the table above). Harness overhead, tool-call timing and estimates are never evidence.
- **NOTE: Tests never write into your profiles root.** `bin/background.sh` points a test's runtimes at `$TMPDIR/agent-repl-test-profiles`.
- **NOTE: SIGWINCH, not SIGUSR1/SIGUSR2.** Its default action is to ignore, so an old build is never killed by it; node's SIGUSR1 opens its inspector and the shim's SIGUSR2 is its keep-alive reset.
