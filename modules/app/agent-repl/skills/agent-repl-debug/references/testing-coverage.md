# Testing and coverage

Use this playbook to determine what repository-tracked tests exercise a path,
measure code coverage, identify coverage gaps, and distinguish code coverage
from logging coverage.

## Running tests and coverage

The daemon runs every test run (`bin/background.sh` refuses any other); the
`agent-repl-tests` skill says how to name the tests. The full suite is the
merge queue's gate, never a hand run:

```sh
agent-repld test run -every-suite              # only with a stated systemic reason
agent-repld test run -suite daemon -coverage   # one system's coverage
agent-repld test run -every-suite -coverage    # every system's, and the all-tier baseline
```

A coverage run instruments every Go and vitest suite it selects, the systems
the integration and e2e tiers spawn included, and its last unit
(`testrun coverage-baseline`) merges every tier into each system's statement
coverage and writes `coverage-baseline.json` beside the run's log. It also
reports the shim's e2e reach (c8 over the profiles the e2e suite's shims
wrote), never merged into the baseline. A coverage run of every suite also
runs the istanbul honesty probes (`webapp-coverage-honesty`,
`shim-coverage-honesty`); after a change to the coverage provider, its
version, or the isolation the coverage script buys, run them by name with
`-coverage`.

Canonical timing history (`test_time.csv`) is recorded only when the owner
asks: `agent-repld test run -every-suite -record -as-user` from the main
checkout's workspace. Each row carries its `measure`, and regressions compare
only rows of the same measure (see `modules/app/agent-repl/AGENTS.md`, "Suite
timings: what a row measures").

Go coverage is block-based and reported as aggregate statements plus
per-function results. TypeScript reports statements, branches, functions, and
lines. Generated protobuf artifacts are dependencies rather than coverage
subjects.

The repository target is at least 90 percent statement coverage for each
authored non-Lisp component. Until a component reaches the target, never lower
its measured baseline and add focused tests for every changed critical branch
and error path.

Both TypeScript packages measure with istanbul, never `@vitest/coverage-v8`.
That provider merges each test-file window's raw V8 coverage before remapping
it, so a module compiled in more than one window loses one window's counts and
the surviving one depends on which test files shared a process; webapp
`src/scroll.ts` read 47.71 percent of statements in the package report against
100 percent with only its own test file running, and the figure moved between
runs as test files were added. Treat a v8-provider number in an older report as
unusable rather than merely stale. The honesty probes are the check that
catches a regression to it.

A coverage run measures rather than enforcing the 90 percent threshold. A
passing run proves that tests and instrumentation ran. It
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
