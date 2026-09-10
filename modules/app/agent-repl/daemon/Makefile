# GOTEST_PARALLEL bounds how many tests run at once.
#
# MEASURED BASIS (16-core host, `-count=3`, all green): the integration suite
# runs 118.3s at -parallel 1, 32.5s at 4, 18.4s at 8, 13.8s at 12 and 13.3s at
# 16. The curve is flat past 8 while the per-test load keeps climbing: every
# test owns a real claude-repld process plus a fake shim, and at 16 that load
# pushed ordinary daemon steps past harness.DefaultTimeout in two tests that are
# green at 8. Eight is the last value that buys most of the speed and none of
# the oversubscription. Override it on the command line for a bigger host.
GOTEST_PARALLEL ?= 8

.PHONY: test integration coverage

# The unit suite. It is NOT parallelized within a package: it runs in 2.2s wall
# with a 1.21s slowest package and a 0.19s slowest test, so per-test parallelism
# would buy nothing measurable.
test:
	go build ./... && go vet ./... && go test ./... -count=1

# The integration suite. TMPDIR=/tmp IS REQUIRED on macOS -- see AGENTS.md.
integration:
	TMPDIR=/tmp go test -tags integration ./integration/... -timeout 180s -count=1 -parallel $(GOTEST_PARALLEL)

coverage:
	../bin/report-nonlisp-coverage.sh daemon
