# Webapp

## Logging

- The webapp owns one canonical logging API with normal and verbose emission
  functions in `src/wslog.ts`. New or changed webapp code uses that API only.
- Canonical workspace records are persisted by the daemon at
  `<workspace>/.claude/emacs/webapp.log`. The webapp is always attached to a
  session and workspace, so it has no global durable log. Missing workspace
  association is an invariant violation.
- Every record is JSON-shaped and carries a stable `operation`, `connection_id`,
  and every known session, request, workspace directory, and workspace ID.
  Never create human-formatted durable records or legacy logging identities.
- Every new or materially changed nontrivial function logs its entry. Every
  meaningful branch that selects a different nontrivial block, call, state
  transition, or outcome logs its selection.
- The normal helper persists through the daemon and emits to the browser
  console. The verbose helper always persists and gates browser-console output
  through the webapp verbose setting.
- Each error is logged exactly once by its owning layer with session, workspace,
  connection or request, operation, resolved inputs, branch outcome, and cause.
  Error-path tests assert the canonical record and its context.
- Log every critical state transition and branch that selects a materially
  different outcome. Errors use explicit `error` severity.
- Frequent or hot diagnostics use the verbose helper. Do not bypass logging.
  Direct `console` calls or ad hoc logger aliases are forbidden except a
  documented pre-logger bootstrap failure or logger-sink emergency path.

- Before finishing a webapp change, run `npm run typecheck` and
  `npm run coverage`. Both commands must pass. Coverage measures authored
  `src/**/*.ts`, including branch data, and excludes declarations and generated
  sources.
- Before handoff, run `modules/app/agent-repl/bin/test-all.sh` from the
  repository root. Every tracked suite must pass.
- Maintain at least 90% statement coverage. Never reduce the measured baseline,
  and add focused tests for every critical branch and every error path changed.
- After a commit lands on `master`, run
  `modules/app/agent-repl/bin/test-all.sh --record`, inspect the canonical
  `modules/app/agent-repl/test_time.csv`, and surface every reported timing
  regression.

## Logging-density audit

Before handoff, run
`modules/app/agent-repl/bin/report-logging-density.sh webapp` and report its
source-line and canonical-call counts. This is a rough syntactic review aid,
not semantic logging coverage. Directly audit every critical branch and error
path even when the ratio rises.
