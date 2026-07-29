# daemon/

## Logging

- The daemon owns one canonical JSON logging API in `internal/dlog`, divided
  between normal and verbose emission functions. New or changed daemon code
  uses that API only.
- Workspace-bound records persist through the canonical
  `<workspace>/.claude/emacs/daemon.log` symlink. The daemon's global service
  log is only for events that are conceptually unrelated to every workspace
  and agent session. Difficulty resolving a known workspace is an invariant
  violation, never a reason to write its record globally.
- Every new or materially changed nontrivial function logs its entry. Every
  meaningful branch that selects a different nontrivial block, call, state
  transition, or outcome logs its selection.
- The daemon's normal helper persists and emits to the terminal. The verbose
  helper always persists and gates terminal output through the daemon's
  established verbose setting.
- Each error is logged exactly once by its owning layer with session, workspace,
  operation, resolved inputs, branch outcome, and cause. Error-path tests assert
  the canonical record and its context.
- Frequent or hot diagnostics use the verbose helper. Do not bypass logging.
  Direct diagnostic output through `fmt`, `log`, `slog`, or an ad hoc logger is
  forbidden except a documented pre-logger bootstrap failure or logger-sink
  emergency path.

## Verification

- Run `make coverage` after every daemon Go change. It runs all `cmd`, `e2e`,
  and `internal` packages with `-coverpkg=./...` and reports `go tool
  cover -func` output. The command must pass.
- Before handoff, run `modules/app/agent-repl/bin/test-all.sh` from the
  repository root. Every tracked suite must pass.
- Maintain at least 90% statement coverage. Until the measured daemon baseline
  reaches that target, never reduce it, report the gap explicitly, and add
  focused tests for every critical branch and every error path changed.
- Run `modules/app/agent-repl/bin/report-logging-density.sh daemon` and report
  its source-line and canonical-call counts as a rough review aid. It is not
  semantic logging coverage, so directly audit all critical branches and
  errors even when the ratio rises.
- After a commit lands on `master`, run
  `modules/app/agent-repl/bin/test-all.sh --record`, inspect
  `modules/app/agent-repl/test_time.csv`, and surface every reported timing
  regression.
