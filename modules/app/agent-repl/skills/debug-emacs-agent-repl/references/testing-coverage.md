# Testing and coverage

Use this playbook to determine what repository-tracked tests exercise a path,
measure code coverage, identify coverage gaps, and distinguish code coverage
from logging coverage.

## Full local verification

Run from the repository root:

```sh
modules/app/agent-repl/bin/test-all.sh
```

The unified runner executes:

- Shell harnesses for the orchestrator and local infrastructure.
- The complete ERT safety wrapper.
- Every authored Go module with coverage.
- Both TypeScript projects with typechecking and coverage.
- Proto validation and generated-artifact checks.
- Logging-density reporting.
- Per-suite wall-clock timing.

Every tracked failure must be fixed. Do not investigate whether a tracked
failure predates the current work.

After a successful commit lands on `master`, record canonical timing history:

```sh
modules/app/agent-repl/bin/test-all.sh --record
```

The record mode updates `modules/app/agent-repl/test_time.csv` only after every
suite passes and the branch and commit remain unchanged. Inspect reported
timing regressions and the newest rows.

## Focused non-Lisp coverage

Run all non-Lisp coverage:

```sh
modules/app/agent-repl/bin/report-nonlisp-coverage.sh
```

Select one or more components:

```sh
modules/app/agent-repl/bin/report-nonlisp-coverage.sh daemon webapp
```

Accepted components are:

- `daemon`
- `sidecar`
- `store`
- `wire`
- `webapp`
- `shim`
- `proto`

Go coverage is block-based and reported as aggregate statements plus
per-function results. TypeScript reports statements, branches, functions, and
lines. Generated protobuf artifacts are dependencies rather than coverage
subjects.

Component-local commands:

- Go runtimes: `make coverage`.
- Shim and webapp: `npm run typecheck` then `npm run coverage`.
- Proto: `make coverage`.

The repository target is at least 90 percent statement coverage for each
authored non-Lisp component. Until a component reaches the target, never lower
its measured baseline and add focused tests for every changed critical branch
and error path.

The reporter currently measures rather than enforcing the 90 percent
threshold. A passing command proves that tests and instrumentation ran. It
does not by itself prove the target was reached.

## Elisp and infrastructure coverage

ERT currently provides test counts and pass or fail status rather than a
percentage-based source-coverage report. Assess changed Elisp through focused
unit tests for:

- Successful behavior.
- Meaningful branches.
- Boundaries and state transitions.
- Every error path.
- Canonical error logging.
- Absence of partial state mutation.

Shell harnesses and proto validation are also pass or fail surfaces rather
than source-coverage percentages.

## Logging coverage

Code coverage and logging coverage answer different questions.

Run the rough logging-density report:

```sh
modules/app/agent-repl/bin/report-logging-density.sh
```

Select a component when needed:

```sh
modules/app/agent-repl/bin/report-logging-density.sh daemon
```

The report counts authored source lines and canonical logging call sites. It
cannot prove semantic coverage. Audit directly whether:

- Every materially changed nontrivial function logs entry.
- Every meaningful branch logs its selected outcome.
- Every state transition is attributable.
- Every error is logged exactly once by its owning layer.
- Error tests assert canonical structured context.
- Hot paths use the canonical verbose helper rather than bypassing logging.

`wire` is intentionally sink-free. Its callers own contextual logging.

## Investigating a path

For a function or behavior under investigation:

1. Identify its runtime and nearest scoped `AGENTS.md`.
2. Find focused unit tests and error-path tests.
3. Run the component coverage command.
4. Inspect per-function or file coverage.
5. Run logging density as a rough locator.
6. Audit critical branches manually.
7. Record missing tests separately from missing telemetry.
8. Run the full unified suite before handoff after any implementation.

Do not claim semantic coverage solely from a percentage or logging-call count.
