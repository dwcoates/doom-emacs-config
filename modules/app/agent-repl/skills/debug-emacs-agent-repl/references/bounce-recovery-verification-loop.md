# Bounce recovery-verification loop

Use this runbook when the question is "does the stack actually RECOVER from a
backend bounce, for every workspace, with real data on the wire?" It gates on a
green suite, deploys, forces a bounce of the backend services, and then renders
a verdict against fixed criteria — each with an exact probe — before looping
into remediation.

This runbook owns the bounce-and-verdict discipline and nothing else. The
iteration mechanics belong to `iterative-fix-verify-loop.md`, the log routing
and JSONL shape belong to `../../logging-contract.md`, the deploy ordering
belongs to the daemon's own deploy (`daemon/internal/deploy`), the suite invocations belong to the component
`AGENTS.md` files, and the pre-conclusion audit belongs to
`observability-gaps.md`.

## When to select it

Select this runbook when:

- A change touches recovery, resync, reconnect, restart announcement,
  hibernation, or the shim attach path, and the question is whether a bounce
  survives it.
- Workspaces come back after a restart looking alive — socket open, page
  mounted, spinner turning — and the doubt is whether anything real is
  flowing.
- A fix has landed for a recovery defect and the bounce must be re-run to prove
  it, repeatedly, until an iteration is clean.

Do not select it for a single non-recovery bug with a known symptom; that is
`critical-path-observability-loop.md`. Do not select it for a general
drive-to-healthy sweep with plural unrelated causes; that is
`iterative-fix-verify-loop.md`, which this runbook plugs into as the
verification half. Do not select it for a read-only diagnosis: it deploys,
truncates logs, and kickstarts services from step 2 onward, and it requires the
user to have asked for that.

## 1. Gate: never deploy on a red suite

Nothing below this line is meaningful on an untested tree. A bounce verdict
computed over a build with known-failing tests attributes to the runtime what
the suite already named.

Run, and require green:

```sh
# Daemon — the whole thing, no cache. Long-running; budget for it.
cd modules/app/agent-repl/daemon && go test ./... -count=1

# Webapp
cd modules/app/agent-repl/webapp && npm test && npm run typecheck

# Shim
cd modules/app/agent-repl/agent-shim/claude/shim && npm test

# ERT, per touched module (see the root AGENTS.md for the module list)
cd modules/app/agent-repl && \
  /path/to/emacs -batch -Q -l ert -l lisp/test-<module>.el \
  -f ert-run-tests-batch-and-exit
```

**THE E2E SKIP FOOTGUN — read this before trusting a green daemon run.** The
`daemon/e2e` package builds the shim bundle per run, and it NEVER installs
anything. When `agent-shim/claude/shim/node_modules` is absent, `buildShim`
calls `t.Skipf("shim deps not installed …: run \`npm ci\` in …")` and the
entire e2e surface reports as SKIP inside an otherwise-green `go test ./...`.
A green summary therefore does not mean e2e ran.

Before the daemon suite, install the deps and confirm afterwards that e2e did
not skip:

```sh
cd modules/app/agent-repl/agent-shim/claude/shim && npm ci
cd modules/app/agent-repl/daemon && go test ./... -count=1 2>&1 | grep -E '^(ok|FAIL|---? SKIP).*e2e'
```

Treat any `SKIP` line naming a missing `node_modules` as a RED suite, not as a
pass. Do not proceed to step 2 until e2e has actually executed.

`daemon/e2e` carries other legitimate skips (no `node`, no `go` toolchain, no
`shim-store` source, harness gaps). Read each one; the deps skip is the one
that silently hides real product failures, and it is the one this gate exists
for.

## 2. Deploy

```sh
modules/app/agent-repl/daemon/bin/claude-repld deploy
```

THE DAEMON OWNS THE DEPLOY: it builds into staging, judges every component by
content hash against what each running process reported, installs, and puts
what is out of date into service — the store and sidecar in the recorded safe
order, stale shims through the bounce registry, a stale daemon by handover,
and `reload_elisp` / `reload_webapp` pushed to the stale Emacs and webviews.
The verb prints one line per component decision; a `build_failed` answer
deployed NOTHING. Do not reorder the store and sidecar by hand (see the
restart-safety section of `health-and-readiness.md`).

An UNFORCED deploy ends no turn, so a registered shim bounce or a handover may
still be waiting on a busy workspace when the verb returns. Read its record
(`daemon.promptqueue.bounce`, `daemon.rollout.handover`) before measuring a
component the deploy has not yet put into service.

The elisp reload is pushed to the Emacs that reported older elisp and is
loaded there (the whole module set in `config.el` order, then the heartbeat
assertion). Verify a new symbol is actually bound
(`/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -e '(bound-and-true-p <new-var>)'`)
before believing any result.

Then force page convergence when the deploy did not push a webapp reload (a
page whose build already matched is left alone). Clear the debounce stamp and
sweep explicitly:

```sh
/Applications/Emacs.app/Contents/MacOS/bin/emacsclient \
  -e '(progn (setq agent-repl--webview-recovery-last-sweep nil)
             (agent-repl--webview-recovery-sweep "<reason>"))'
```

Name the reason for what provoked it; it lands in the sweep's own records.

## 3. Force the bounce

1. Truncate both observation sinks so the window observed below has nothing
   before it. Resolve the paths through
   `modules/app/agent-repl/scripts/agent-repl-log-discovery.sh` per
   `structured-logs.md` rather than hardcoding them; today they resolve to the
   daemon's `~/.claude-emacs/claude-repld.log` and the Emacs-side
   `doom-agent-repl.log` under the UID-qualified `$TMPDIR/doom-agent-repl-<uid>/`.

   ```sh
   truncate -s 0 "$DAEMON_LOG" "$EMACS_LOG"
   ```

   Clearing logs is the user-directed iteration-boundary exception the Safety
   rules carve out for the remediation loops; the terms and the STOP condition
   are owned by "Clear the observation logs first" in
   `iterative-fix-verify-loop.md` step 1 and apply here unchanged.

2. Kickstart the two backend services, store first:

   ```sh
   launchctl kickstart -k gui/$(id -u)/com.agentrepl.shim-store
   launchctl kickstart -k gui/$(id -u)/com.agentrepl.shim-claude-sidecar
   ```

   In this harness `launchctl kickstart` requires the sandbox override to run
   at all; expect to pass it, and never work around a refusal by editing the
   plists.

3. Bounce the daemon through Emacs and wait for its terminal result:

   ```sh
   /Applications/Emacs.app/Contents/MacOS/bin/emacsclient \
     -e '(agent-repl-frontend-daemon-restart-await)'
   ```

   This blocks for the whole coordinated restart and records itself as
   deploy-driven, which is what separates "the deploy is driving" from a frame
   that has stopped responding.

## 4. Render the verdict

All seven criteria, each with its probe. A criterion with no probe run is not
met; it is unmeasured.

1. **The announcement was delivered and the quiet window opened, with no warns
   INSIDE the window.**
   - Probe: in the webapp records, find the restart announcement and the quiet
     window it opens (`webapp/src/restart-window.ts`), then grep warn-level
     records whose timestamps fall between the window's open and close.
   - Warns outside the window are a different finding; do not fold them in.

2. **Zero failure-local CREATED cards of the `daemonUnreachable` / severed
   class.**
   - Probe: count CREATION records only — `failure-local: CREATED` lines whose
     `type` names a kind in the severed class (`daemonUnreachable` and
     siblings; the class list lives in `lisp/failure.el`).
   - Counting every occurrence of the kind name double-counts: a card is
     mentioned again on render, on resolve, and in the webapp's own records.
     Count creations.

3. **The shims were preserved across the bounce.**
   - Probe: process count of the shim processes before and after. Equal counts
     with the same PIDs is preservation; a changed count is a finding even when
     everything else is green.

4. **Every pre-bounce in-flight turn either resumed with REAL shim SDK activity
   or was closed loudly by the undriven-turn watchdog.**
   - Probe: enumerate the in-flight turns before the bounce, then for each one
     find either shim SDK activity after the bounce or a `turnUndriven`
     failure (`FailureTurnUndriven`,
     `daemon/internal/sessioncontroller/undriventurn*`).
   - A turn that is neither is the failure this criterion exists to catch: a
     workspace thinking forever in silence.

5. **Per-workspace REAL DATA over the wire after the bounce.**
   - Probe: count rendered feed elements inside each live page and sample the
     count TWICE, about sixty seconds apart. Growth proves data is landing. A
     socket being open proves nothing.
   - Use the live widget directly. `agent-repl--frontend-webview-execute-script`
     takes only `(buf script)` and RETURNS NOTHING, so it cannot carry a count
     back. Get the widget with `agent-repl--frontend-webview-live-widget` and
     call `xwidget-webkit-execute-script` with a callback:

     ```elisp
     (let ((xw (agent-repl--frontend-webview-live-widget BUF)))
       (xwidget-webkit-execute-script
        xw "document.querySelectorAll('<feed-item-selector>').length"
        (lambda (n) (agent-repl--log nil "feed-count: buf=%s n=%S" BUF n))))
     ```

   - A dead widget (`nil` from the live-widget call) is a finding, not a zero.

6. **Per-workspace recovery within the 3s SLO.**
   - The canonical record is `recovery-slo:` in each workspace's `emacs.log`,
     one per workspace per outage, owned by `lisp/recovery-slo.el`. Do not
     invent a second timing.
   - **`outcome=recovered` IS THE ONLY PASS.** The vocabulary is closed
     (`agent-repl-recovery-slo-outcomes`) and each value means one thing:
     - `recovered` — the conjunction completed inside the budget with nothing
       having touched the workspace. The only outcome that satisfies this
       criterion.
     - `budget-breach` — the SLO verdict: the deadline passed with a signal
       outstanding. This criterion FAILED for that workspace. It is emitted at
       the deadline and does NOT end the measurement.
     - `not-measured` — the measurement was invalidated, and `reason=` says by
       what: `slo-force` (the instrument's own forced repair), a sweep reason
       such as `deploy_refresh` (the harness reloaded the page mid-window), a
       scope refusal, or `superseded`. NOT a pass and NOT a failure — an
       unmeasured criterion, which `observability-gaps.md` governs.
     - `unrecovered` — a signal became definitively unobtainable
       (`reason=no-page`, `probe-absent`, `workspace-gone`). A real failure.
   - There is deliberately no `forced-recovered`. A conjunction satisfied after
     the instrument reloaded the page is the repair working, not the budget
     being met; historical records carrying it, and any `outstanding=none` read
     as a pass under `forced=yes`, are worthless for this criterion.
   - `scope=` names the conjunction actually applied. A workspace with no page
     when the outage began is measured on `emacs,wire` only; it is not owed a
     page signal and its absence is not a failure.
   - **A deploy that reloads the webviews cannot measure this criterion
     cleanly.** The deploy's own `reload_webapp` lands inside the window and the affected
     workspaces come back `not-measured reason=deploy_refresh`, correctly.
     Measure criterion 6 across a `launchctl kickstart` /
     `agent-repl-frontend-daemon-restart-await` bounce — the same restriction
     criterion 7 already carries.
   - **`emacs_ms` is not comparable across single bounces.** It has been seen
     at 5ms and at 2320ms on identical code, because the anchor differed: an
     attempt armed while the link was still up dated recovery from the arming,
     one armed from a genuine announcement dated it from the link reopening.
     Outage-scoped arming removes that particular split, but `webapp_ms`
     remains a DETECTION time quantized to `agent-repl-recovery-slo-poll-ms`
     (500ms), not an arrival time — identical `webapp_ms` across several
     workspaces means one tick observed them all, not that they converged.
     Compare distributions across several bounces; never conclude from one.

7. **NO SDK QUERY WAS TERMINATED BY THE BOUNCE.**
   - This is the requirement in its most direct form: a session must not die
     because a SEPARATE process restarted. A store bounce reaching the SDK is
     the loop failing, not a symptom of it.
   - Probe: count, in each live page's rendered text, `unexpected_query_termination`
     and the human-facing `query ended unexpectedly`. Both, because the card's
     prose and its reason field are different strings and either may be present.
   - MEASURE ACROSS A BOUNCE THAT DOES NOT REFRESH THE PAGES.
     A deploy that pushes `reload_webapp` reloads every stale webview, which
     WIPES the very cards this criterion counts — a post-deploy zero means the
     pages were reset, not that nothing died. Bounce with `launchctl kickstart` alone when measuring
     this, or read a durable sink instead.
   - The durable sinks do NOT currently capture it: the workspace
     `emacs.log`s read zero while the card is on screen, because the card is
     produced on the daemon's translate path
     (`daemon/internal/frontend/translate.go`, reason
     `unexpected_query_termination`) rather than written to the workspace log.
     Treat that gap as a finding in its own right — a criterion whose only
     evidence is a DOM node is one page-reload away from being unmeasurable.
   - Known origin: `agent-shim/claude/shim/src/uds/uds-session.ts`
     (`UNEXPECTED_QUERY_TERMINATION_REASON`), reached from a store-client
     write rejection (`store-client: write on a down connection`).

## 5. Remediate and loop

Root-cause every criterion that failed, from the two sinks — not from the
symptom. Then dispatch fixes, merge, redeploy, re-bounce, and re-render the
verdict. Loop until ONE iteration is clean on all the criteria at once; an
iteration that fixes criterion 2 while criterion 5 regresses has not exited.

### Restart what the bounce interrupted, then bounce again

An interrupted SDK query does not resume itself, so a remediation verified only
against workspaces that were already idle proves nothing about the case the fix
exists for. After the fixes land and are deployed:

1. Tell every SDK query the previous bounce interrupted to START AGAIN, so the
   next bounce has live work to interrupt.
2. Wait long enough for those queries to be genuinely in flight — verify it,
   do not assume it, the same way the async probes are verified by a growing
   tick file rather than a pid.
3. Bounce the backend again and re-render the verdict, criterion 7 included.
4. If anything was interrupted again, remediate and repeat from step 1.

The loop exits only when a bounce lands on genuinely live work and interrupts
none of it. A clean verdict over idle workspaces is not an exit.

Fanout mechanics, gating on loop-critical fixes only, and the merge-then-redeploy
discipline are owned by `iterative-fix-verify-loop.md` steps 5 through 8. Use
them as written; this runbook adds only the verdict.

## Hard-won gotchas

- **e2e skips silently without shim deps.** Covered in step 1. It has hidden
  twenty real failures inside a green summary. Always confirm e2e ran.

- **A fresh worktree needs BOTH the deps and a BUILT shim.** Two different
  masks on the same lie — "the suite did not actually test your change":

  ```bash
  ln -sfn <main-repo>/modules/app/agent-repl/agent-shim/claude/shim/node_modules \
          <worktree>/modules/app/agent-repl/agent-shim/claude/shim/node_modules
  bin/build-frontend.sh --force shim
  ```

  Without `node_modules` the suite SKIPS and reports `ok` in under a second.
  With deps but no built `dist/main.js`, every spawned shim exits 1 and tests
  fail for a reason that has nothing to do with the change. Both have already
  cost a full revert: an agent read a 11.9s skip as a pass, its branch merged,
  and it broke thirty e2e tests.

- **Wall-clock is a FLOOR test, not a range.** A real e2e run is ~250s idle and
  ~440s under load on this machine. Under ~30s means it skipped. A long run is
  contention, not a hang — do not kill it and do not read the duration as a
  failure signal.

- **A deploy's `reload_webapp` reloads the stale webviews, destroying DOM
  evidence.** Any
  criterion measured by reading the live page (criterion 7, feed counts, badge
  states) must be sampled across a `launchctl kickstart` bounce rather than a
  deploy, or the measurement records the reload instead of the bounce.

- **An empty task list does NOT mean prior agents died.** After a compaction the
  list can read empty while agents are still running. ALWAYS run `ListAgents`
  before re-dispatching work believed lost. Skipping this put two agents in one
  worktree, which forced a rewind-and-rebuild of the branch's history.

- **Count shim survival by PID IDENTITY, never by process count.** A complete
  kill-and-respawn satisfies an equal count. Classify with `ps -o command=`
  first: `pgrep -f 'shim/dist/main.js'` also matches the daemon.

- **PID identity means DIFFING THE TWO PID SETS, not comparing start times to
  the bounce boundary.** A shim that died at 11:56:29 and respawned at 11:56:30
  has a start time before an 11:58 bounce and passes a "started before the
  boundary" test — while being a different process. This exact substitution
  reported "all 7 shims preserved, zero died" for a bounce that had in fact
  replaced one, and the process count stayed 7 the whole time so nothing looked
  wrong. Snapshot `pid ws` pairs to a file before and after, and `diff` them.
  Anything but an empty diff is a finding.

- **Probe the page with `textContent`, NEVER `innerText`, and sample more than
  once.** `innerText` returns only RENDERED text, so a failure card inside a
  collapsed or hidden bubble reads as absent — a criterion-7 sweep reported
  zero terminations on a page that was carrying one. Worse, the same page read
  0 at 12:02 and 1 at 12:08 with no bounce in between: content enters the DOM
  late, so **a single post-bounce sample is not a measurement**. Take at least
  two samples spaced ~25s and require them to agree.

- **A termination card on screen is not evidence the termination happened NOW.**
  Cards are REPLAYED from history on every page load. Convert the card's own
  `observed_at_ms` to wall clock (`date -r $((ms/1000))`) before attributing it
  to the bounce under test. One card read as a live bounce casualty was
  timestamped ~13 hours earlier; the daemon logs the replay explicitly as
  `decision=retain_history_no_bring_up_fault` / `single_card_per_replayed_pair`.

- **A deploy's elisp reload is loaded by Emacs, not by the deploy.** The daemon
  PUSHES `reload_elisp` to an Emacs that reported older elisp and answers
  `reload_pushed`; the load itself, and any refusal (a reload naming another
  checkout's root), is Emacs's own record. A measurement taken against stale
  elisp looks like the fix failing: verify a new symbol is bound before
  believing any result. Note `load` does not unbind variables the change
  DELETED, so a stale binding lingering is not proof of stale code.

- **`emacsclient` is not on PATH.** Use
  `/Applications/Emacs.app/Contents/MacOS/bin/emacsclient`, or
  `$AGENT_REPL_EMACSCLIENT`. A "command not found" here reads exactly like a
  dead Emacs if you are not watching for it.

- **`agent-repl-refresh-webviews` returns an INTEGER.** The function coerces
  the sweep's internal nil (a debounced sweep) to `0`; do not "simplify" that
  coercion away, and do not make the sweep itself return an integer instead:
  the nil-for-debounced distinction is load-bearing internally.

- **Prompt sends require an explicit `PROMPT_ORIGIN_*` value.** The client
  rejects anything that is not `PROMPT_ORIGIN_`-prefixed and rejects
  `PROMPT_ORIGIN_UNSPECIFIED`. A probe prompt with no origin does not fail
  visibly at the call site; it fails at the boundary.

- **Merge-completed workspaces are excluded from page pre-creation.** The
  `:merge-completed` refusal in `agent-repl--frontend-precreate-refusal` is
  deliberate — a merged workspace is CLOSED, and an automatic page would
  resurrect a presentation the user is done with. Their absent pages are not a
  recovery failure, and criterion 5 must not count them as missing data.

- **"Thinking" is only trustworthy when corroborated by shim SDK activity.** A
  workspace rendering a thinking state proves the page received a state, not
  that a turn is being driven. Always pair it with criterion 4's probe.

## Composition

- `iterative-fix-verify-loop.md` for the iteration mechanics, the log-clearing
  carve-out, the fanout rules, and the merge-then-redeploy order.
- `health-and-readiness.md` for the pre-bounce baseline sweep and the
  store-before-sidecar restart order.
- `structured-logs.md` for resolving the sinks and mining the bounce window,
  and `identity-correlation.md` for tying a turn to its workspace, session, and
  process across the bounce.
- `critical-path-observability-loop.md` when a criterion fails and the sinks
  cannot name why.
- `observability-gaps.md` before declaring any iteration clean — an unmeasured
  criterion is a blind spot, not a pass.
- `performance-investigation.md` when criterion 6 is the surviving failure and
  the SLO record cannot localize the time.
