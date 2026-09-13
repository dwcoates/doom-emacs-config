//go:build race

package db

// raceEnabled says this test binary is instrumented by the race detector.
//
// A LATENCY BOUND IS NOT MEASURABLE IN AN INSTRUMENTED BINARY. The detector
// shadows every memory access, and the same 30-row batch behind the same sweep
// batch that measures 5ms + 3ms uninstrumented measured 50ms + 413ms under it —
// a number about the detector, not about the store, and one that would make the
// production budget fail for a healthy write path. The bounds are asserted in
// the uninstrumented binary; what the race build is here to prove about these
// paths is the absence of a data race, and it still runs everything else.
//
// THE PLAN ASSERTIONS RUN IN BOTH. They are what actually guards a seek turning
// into a scan (see TestTheSweepsDeleteSeeksTheLedgerRatherThanScanningIt), and
// they cost nothing and say the same thing on every box.
const raceEnabled = true
