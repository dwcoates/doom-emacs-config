package integration

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
//
// WHERE THE SUBJECT IS ASSERTED. It is asserted for EVERY scenario the mocked
// vendor declares — the blocked rows included — inside `TestMockScenarios`
// (`mock_scenarios_test.go`), on the tree that test already generated and
// ingested. It used to live in a second test function of its own, and that
// function re-ran the whole mocked vendor over all 133 rows — four real
// processes per row — to reach one call to `requireNoOrphanToolResults`. The
// assertion is unchanged and its coverage is unchanged; only the second
// generation of the identical fixture is gone.
//
// The invariant's own helper is `requireNoOrphanToolResults`
// (`mock_helpers_test.go`), which states what an orphan is in the failure it
// prints.
