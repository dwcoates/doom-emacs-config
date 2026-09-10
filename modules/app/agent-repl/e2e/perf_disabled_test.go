//go:build !perf

// perf_disabled_test.go — the perf phase's absence, made explicit.
//
// The perf files are compiled only under `-tags perf` (PERF-SPEC.md §D4), so
// the functional suite's binary contains none of them. runSuite still calls
// perfReportSummary after m.Run; this is what that call resolves to when the
// tag is absent, which is why main_test.go needs no build tag of its own.
package e2e

// perfReportSummary writes nothing: there is no perf phase in this build.
func perfReportSummary() {}
