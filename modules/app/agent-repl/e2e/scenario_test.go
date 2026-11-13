package e2e

import (
	"testing"

	scenariokit "agentrepl/e2e/kit/scenario"
)

// A SCENARIO IS ONE TOP-LEVEL TEST WHOSE NAMED STEPS RUN IN ORDER
// (kit/scenario). The scans that keep the inventory whole, and the scenario
// matrix that names a step by its path, read this package's source from
// kit/scans.

// scenario runs steps as t's named cases, in order, beside the other
// scenarios.
func scenario(t *testing.T, steps ...func(*testing.T)) {
	t.Helper()
	scenariokit.Run(t, steps)
}
