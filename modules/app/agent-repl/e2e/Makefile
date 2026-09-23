# The cross-system e2e suite. Follows daemon/Makefile's conventions.
#
# GOTEST_PARALLEL bounds how many worlds stand at once. SPEC.md ("Running the
# suite — parallelism, and every bound it rests on") settles it at 8 on
# measured grounds: every test declares t.Parallel() and each world is a
# daemon, a store, a sidecar and a node shim.
GOTEST_PARALLEL ?= 8
GOTEST_TIMEOUT ?= 45m

# EVERY TEST RECIPE RUNS UNDER $(BACKGROUND), bin/background.sh: the run and
# every process it starts go to background priority, so test load cannot
# starve the owner's live runtime. bin/test-background.sh's source scan fails
# a test recipe that does not.
BACKGROUND := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))../bin/background.sh)

.PHONY: test coverage perf perf-only perf-enforce perf-baseline

# TMPDIR=/tmp IS REQUIRED on macOS -- the suite's unix socket paths do not fit
# the 103-byte cap beneath the default per-user temp root.
test:
	TMPDIR=/tmp $(BACKGROUND) go test ./... -count=1 -timeout $(GOTEST_TIMEOUT) -parallel $(GOTEST_PARALLEL)

# The same run with coverage collection for the SPAWNED systems, merged and
# reported per module. See ../bin/e2e-coverage.sh.
coverage:
	E2E_COVERAGE_PARALLEL=$(GOTEST_PARALLEL) E2E_COVERAGE_TIMEOUT=$(GOTEST_TIMEOUT) \
		$(BACKGROUND) ../bin/e2e-coverage.sh

# ---------------------------------------------------------------------------
# The perf phase — PERF-SPEC.md section D4.
#
# SERIAL, AND AFTER THE FUNCTIONAL SUITE. `-parallel 1` is stated explicitly
# rather than inherited, so a future edit to GOTEST_PARALLEL cannot silently
# parallelize a latency measurement; SPEC.md's own numbers settle why (a run
# went 20.7s -> 38.4s at -parallel 12 and lost a test to a bound). It runs
# after `test` because a red functional run makes every perf number
# meaningless, and measuring first only wastes the time it takes to find out.
#
# `-v` is not optional: PerfRecorder.Report writes through t.Logf and the whole
# discipline depends on a passing run printing its numbers. They land in
# `go test -json` output as ordinary Output actions.
#
# The build tag keeps every perf file out of the functional binary; the env var
# is the second gate, so a hand-rolled `-tags perf` sweep skips loudly instead
# of measuring latency under whatever load it happened to run at.
perf: test perf-only

# The perf phase alone, for a re-measurement on an already-green tree.
#
# The baseline drift check REPORTS here rather than failing; see PERF-SPEC.md
# section I5 finding 17 for the measurement that settled that, and use
# `perf-enforce` on a host quiet enough to hold it.
perf-only:
	TMPDIR=/tmp AGENT_REPL_E2E_PERF=1 $(BACKGROUND) go test ./... -tags perf -run 'TestPerf' -count=1 \
		-timeout $(GOTEST_TIMEOUT) -parallel 1 -v

# REWRITE the committed baselines from this run's measurement. Deliberate, by
# name, and never reachable from a failing run: a baseline a red run can
# rewrite is not a baseline (PERF-SPEC.md section D3).
#
# -count=5, and the recorded number is the HIGH-WATER p50/p95 across the five:
# run-to-run spread is real and unequal between rows (measured, one row's p95
# varied 6% across runs and another's 75%), so a baseline from a single
# repetition puts the 20% regression check inside the noise. THREE WAS NOT
# ENOUGH — a baseline recorded over three runs put perf-response-bubble's
# p95 at 1.539ms and the very next run measured 2.036ms, a false 32%
# regression against a row whose real spread is 1.4-2.0ms. See PERF-SPEC.md
# section I5 finding 17.
perf-baseline:
	TMPDIR=/tmp AGENT_REPL_E2E_PERF=1 AGENT_REPL_E2E_PERF_BASELINE=write \
		$(BACKGROUND) go test ./... -tags perf -run 'TestPerf' -count=5 \
		-timeout $(GOTEST_TIMEOUT) -parallel 1 -v

# The perf phase with the >20% baseline regression check FAILING rather than
# reporting. For a quiet or dedicated host; on a box shared with other suites
# it fails on load, not on regressions.
perf-enforce:
	TMPDIR=/tmp AGENT_REPL_E2E_PERF=1 AGENT_REPL_E2E_PERF_BASELINE=enforce \
		$(BACKGROUND) go test ./... -tags perf -run 'TestPerf' -count=1 \
		-timeout $(GOTEST_TIMEOUT) -parallel 1 -v
