package integration

import "testing"

// SUBJECT — a tool_result that never found its call.
//
// An ORPHAN tool_result is not a withholding class. A withholding class is a
// record we understood and deliberately chose not to carry; an orphan is a
// SETTLE THE CONVERTER FAILED TO PERFORM, so the unit it belonged to stays open
// in every reader downstream and the run draws as still running.
//
// THE MOCK HAS NO EXCUSE TO PRODUCE ONE. Every scenario's transcript is written
// from the top by the mocked vendor and read from offset zero by a fresh
// sidecar, so every call is read before its result and every join is available.
// The one production case an orphan legitimately names — a call left behind a
// cursor by a restart — does not arise here.
//
// It was on the documented-kinds allowlist, which made the entire mock suite
// blind to a join the converter had stopped performing.

// TestMockScenariosProduceNoOrphanToolResults asserts zero orphans across every
// scenario the mocked vendor declares.
func TestMockScenariosProduceNoOrphanToolResults(t *testing.T) {
	for _, tc := range mockScenarios {
		t.Run(tc.Prompt, func(t *testing.T) {
			// Arrange, Act: the scenario, generated fresh and ingested whole.
			tree := generateMock(t, tc.Prompt, tc.Wait)
			in := ingestMock(t, tree)

			// Assert.
			requireNoOrphanToolResults(t, tc.Prompt, in.Entries())
		})
	}
}
