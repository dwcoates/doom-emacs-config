# Bounce recovery-verification loop

Use this runbook when the question is "does the stack actually RECOVER from a
backend bounce, for every workspace, with real data on the wire?" It gates on a
green suite, deploys, forces a bounce of the backend services, and then renders
a verdict against fixed criteria — each with an exact probe — before looping
into remediation.

This runbook owns the bounce-and-verdict discipline and nothing else. The
iteration mechanics belong to `iterative-fix-verify-loop.md`, the log routing
and JSONL shape belong to `../../logging-contract.md`, the deploy ordering
belongs to `bin/deploy-all.sh`, the suite invocations belong to the component
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
modules/app/agent-repl/bin/deploy-all.sh
```

The script owns its ordering and its failure semantics; do not reorder the
store and sidecar by hand (see the restart-safety section of
`health-and-readiness.md`).

Then hot-load every touched non-test elisp file into the live Emacs. **`emacsclient`
is not on PATH in this harness** — use the absolute path `deploy-all.sh` itself
resolves, `/Applications/Emacs.app/Contents/MacOS/bin/emacsclient` (overridable
via `AGENT_REPL_EMACSCLIENT`):

```sh
/Applications/Emacs.app/Contents/MacOS/bin/emacsclient \
  -e '(load-file "/abs/path/modules/app/agent-repl/lisp/<module>.el")'
```

Then force page convergence. `deploy-all.sh` already refreshes the webviews, but
its sweep debounces, so a hot-load that lands after it leaves pages on the old
bundle. Clear the debounce stamp and sweep explicitly:

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

All six criteria, each with its probe. A criterion with no probe run is not
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
   - The canonical SLO record and the forced re-hydration behind it are owned
     by the `feat/workspace-recovery-slo` work; read that branch for the record
     name and the measurement, and do not restate its internals here. Until it
     is in master, measure against whatever it defines rather than inventing a
     second timing.

## 5. Remediate and loop

Root-cause every criterion that failed, from the two sinks — not from the
symptom. Then dispatch fixes, merge, redeploy, re-bounce, and re-render the
verdict. Loop until ONE iteration is clean on all six criteria at once; an
iteration that fixes criterion 2 while criterion 5 regresses has not exited.

Fanout mechanics, gating on loop-critical fixes only, and the merge-then-redeploy
discipline are owned by `iterative-fix-verify-loop.md` steps 5 through 8. Use
them as written; this runbook adds only the verdict.

## Hard-won gotchas

- **e2e skips silently without shim deps.** Covered in step 1. It has hidden
  twenty real failures inside a green summary. Always confirm e2e ran.

- **`emacsclient` is not on PATH.** Use
  `/Applications/Emacs.app/Contents/MacOS/bin/emacsclient`, or
  `$AGENT_REPL_EMACSCLIENT`. A "command not found" here reads exactly like a
  dead Emacs if you are not watching for it.

- **`agent-repl-refresh-webviews` must return an INTEGER.** `deploy-all.sh`
  formats its answer with `%d`; a debounced sweep returning `nil` crashed the
  deploy with `Format specifier doesn't match argument type`. The function now
  coerces the sweep's internal nil to `0` — do not "simplify" that coercion
  away, and do not make the sweep itself return an integer instead: the
  nil-for-debounced distinction is load-bearing internally.

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
