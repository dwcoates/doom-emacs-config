# EVERY TEST RECIPE RUNS UNDER $(BACKGROUND), bin/background.sh: the run and
# every process it starts go to background priority, so test load cannot
# starve the owner's live runtime. bin/test-background.sh's source scan fails
# a test recipe that does not.
BACKGROUND := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))../../bin/background.sh)

.PHONY: coverage shapes

coverage:
	$(BACKGROUND) ../../bin/report-nonlisp-coverage.sh store

# shapes prints the residue shape catalog: one row per distinct recursive key
# structure observed on a line no producer stored. ARGS passes flags through,
# e.g. `make shapes ARGS="--kind unparsed --example"`.
shapes:
	go run ./cmd/shapes $(ARGS)
