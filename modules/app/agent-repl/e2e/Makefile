# The cross-system e2e suite. Follows daemon/Makefile's conventions.
#
# GOTEST_PARALLEL bounds how many worlds stand at once. SPEC.md ("Running the
# suite — parallelism, and every bound it rests on") settles it at 8 on
# measured grounds: every test declares t.Parallel() and each world is a
# daemon, a store, a sidecar and a node shim.
GOTEST_PARALLEL ?= 8
GOTEST_TIMEOUT ?= 45m

.PHONY: test coverage

# TMPDIR=/tmp IS REQUIRED on macOS -- the suite's unix socket paths do not fit
# the 103-byte cap beneath the default per-user temp root.
test:
	TMPDIR=/tmp go test ./... -count=1 -timeout $(GOTEST_TIMEOUT) -parallel $(GOTEST_PARALLEL)

# The same run with coverage collection for the SPAWNED systems, merged and
# reported per module. See ../bin/e2e-coverage.sh.
coverage:
	E2E_COVERAGE_PARALLEL=$(GOTEST_PARALLEL) E2E_COVERAGE_TIMEOUT=$(GOTEST_TIMEOUT) \
		../bin/e2e-coverage.sh
