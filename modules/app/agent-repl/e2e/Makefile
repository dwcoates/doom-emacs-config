# The cross-system e2e suite. Follows daemon/Makefile's conventions.
#
# GOTEST_PARALLEL bounds how many worlds stand at once. SPEC.md ("Running the
# suite — parallelism, and every bound it rests on") settles it at 8 on
# measured grounds: every test declares t.Parallel() and each world is a
# daemon, a store, a sidecar and a node shim.
GOTEST_PARALLEL ?= 8
GOTEST_TIMEOUT ?= 45m

.PHONY: test coverage perf perf-only perf-baseline

# TMPDIR=/tmp IS REQUIRED on macOS -- the suite's unix socket paths do not fit
# the 103-byte cap beneath the default per-user temp root.
test:
	TMPDIR=/tmp go test ./... -count=1 -timeout $(GOTEST_TIMEOUT) -parallel $(GOTEST_PARALLEL)

# The same run with coverage collection for the SPAWNED systems, merged and
# reported per module. See ../bin/e2e-coverage.sh.
coverage:
	E2E_COVERAGE_PARALLEL=$(GOTEST_PARALLEL) E2E_COVERAGE_TIMEOUT=$(GOTEST_TIMEOUT) \
		../bin/e2e-coverage.sh

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
perf-only:
	TMPDIR=/tmp AGENT_REPL_E2E_PERF=1 go test ./... -tags perf -run 'TestPerf' -count=1 \
		-timeout $(GOTEST_TIMEOUT) -parallel 1 -v

# REWRITE the committed baselines from this run's measurement. Deliberate, by
# name, and never reachable from a failing run: a baseline a red run can
# rewrite is not a baseline (PERF-SPEC.md section D3).
perf-baseline:
	TMPDIR=/tmp AGENT_REPL_E2E_PERF=1 AGENT_REPL_E2E_PERF_BASELINE=write \
		go test ./... -tags perf -run 'TestPerf' -count=1 \
		-timeout $(GOTEST_TIMEOUT) -parallel 1 -v
