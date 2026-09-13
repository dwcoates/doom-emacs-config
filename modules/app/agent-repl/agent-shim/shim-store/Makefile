.PHONY: coverage shapes

coverage:
	../../bin/report-nonlisp-coverage.sh store

# shapes prints the residue shape catalog: one row per distinct recursive key
# structure observed on a line no producer stored. ARGS passes flags through,
# e.g. `make shapes ARGS="--kind unparsed --example"`.
shapes:
	go run ./cmd/shapes $(ARGS)
