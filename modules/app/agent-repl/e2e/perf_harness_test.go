//go:build perf

// perf_harness_test.go — the perf phase's instrument, per PERF-SPEC.md §D.
//
// THIS FILE IS COMPILED ONLY UNDER `-tags perf`. The functional suite's binary
// does not contain a byte of it, so `make test` is unchanged by construction
// rather than by a runtime branch that could rot. The env gate
// AGENT_REPL_E2E_PERF=1 sits on top of the tag for the second reader: someone
// who builds with the tag by hand (an IDE, a `go test -tags perf ./...` sweep)
// gets a loud skip naming the Makefile target instead of a latency measurement
// taken under whatever load that invocation happened to run at.
//
// Three things live here, and nothing else:
//
//   - PerfRecorder (§D1): N samples in, p50/p95 out, printed on every outcome.
//   - the calibration guard (§D2): DECLINED is neither pass nor fail.
//   - baselines and the >20% regression check (§D3).
package e2e

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"sync"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"connectrpc.com/connect"
)

// ===========================================================================
// The gate.
// ===========================================================================

// PerfEnabledEnv gates the phase. The Makefile's `perf` target sets it; a
// hand-rolled `-tags perf` run does not, and is skipped loudly.
const PerfEnabledEnv = "AGENT_REPL_E2E_PERF"

// PerfBaselineEnv, set to "write", makes the phase REWRITE its baselines
// instead of enforcing them. `make -C e2e perf-baseline` is the only thing
// that sets it: a baseline a red run can rewrite is not a baseline (§D3).
const PerfBaselineEnv = "AGENT_REPL_E2E_PERF_BASELINE"

// PerfSamples is N for every assertion in this phase. §B fixes it at 20 and
// §D1 makes a short sample a FAILURE rather than a percentile over fewer.
const PerfSamples = 20

// perfRequire skips the calling test unless the phase is enabled.
func perfRequire(t *testing.T) {
	t.Helper()
	if os.Getenv(PerfEnabledEnv) != "1" {
		t.Skipf("perf: %s is not 1; the perf phase runs serially through `make -C e2e perf`", PerfEnabledEnv)
	}
	// SERIAL, NEVER PARALLEL (§D4). No perf test calls t.Parallel(), and this
	// is where that is stated rather than left to each file's discipline: a
	// latency taken beside seven other worlds measures the scheduler.
}

// perfWritingBaselines answers whether this run rewrites baselines.
func perfWritingBaselines() bool { return os.Getenv(PerfBaselineEnv) == "write" }

// ===========================================================================
// D1. The recorder.
// ===========================================================================

// PerfRecorder gathers a fixed number of samples and answers percentiles over
// them. Modeled on emacs_test.go's phase/record/reportPhases trio, which
// already implements this discipline: measure on every run, print on a
// PASSING run, derive the bound from the print.
type PerfRecorder struct {
	name    string
	samples []time.Duration
}

// NewPerfRecorder names one assertion. The name is the baseline file's stem
// and the summary line's key, so it is a stable identifier, not a sentence.
func NewPerfRecorder(name string) *PerfRecorder { return &PerfRecorder{name: name} }

// Record adds one sample. Nothing is ever discarded: no trimming, no outlier
// rejection, no best-of. The p95 exists to hold the tail (§D1).
func (r *PerfRecorder) Record(d time.Duration) { r.samples = append(r.samples, d) }

// Len answers how many samples have been recorded.
func (r *PerfRecorder) Len() int { return len(r.samples) }

// sorted answers the samples in ascending order, leaving the field untouched.
func (r *PerfRecorder) sorted() []time.Duration {
	out := append([]time.Duration(nil), r.samples...)
	sort.Slice(out, func(i, j int) bool { return out[i] < out[j] })
	return out
}

// percentile answers the NEAREST-RANK percentile: sample ceil(q*n) of the
// sorted n, 1-indexed. NO INTERPOLATION (§B): with N=20 an interpolated
// percentile is a value no sample took, and a budget must be violated by a
// real observation.
func (r *PerfRecorder) percentile(q float64) time.Duration {
	s := r.sorted()
	if len(s) == 0 {
		return 0
	}
	rank := int(float64(len(s))*q + 0.9999999)
	if rank < 1 {
		rank = 1
	}
	if rank > len(s) {
		rank = len(s)
	}
	return s[rank-1]
}

// P50 is the lower median — sample 10 of a sorted 20.
func (r *PerfRecorder) P50() time.Duration { return r.percentile(0.50) }

// P95 is sample 19 of a sorted 20 (ceil(0.95*20) = 19).
func (r *PerfRecorder) P95() time.Duration { return r.percentile(0.95) }

// Min and Max bound the observed distribution.
func (r *PerfRecorder) Min() time.Duration { return r.sorted()[0] }
func (r *PerfRecorder) Max() time.Duration { s := r.sorted(); return s[len(s)-1] }

// Report prints the distribution on EVERY outcome, pass or fail.
//
// This is not a convenience. A budget in this suite is a stated multiple of an
// observed healthy maximum, and the only run that produces that observation is
// a run that passed — verbatim the reason emacs_test.go's reportPhases gives.
func (r *PerfRecorder) Report(t *testing.T) {
	t.Helper()
	if len(r.samples) == 0 {
		t.Logf("perf %s: NO SAMPLES", r.name)
		return
	}
	t.Logf("perf %s n=%d min=%s p50=%s p95=%s max=%s",
		r.name, len(r.samples), r.Min(), r.P50(), r.P95(), r.Max())
	// The duration table's own shape, so `go test -v`'s existing reader
	// surfaces the headline number with no new tooling (§D4).
	t.Logf("perf phase %s took %s", r.name, r.P50())
}

// Assert reports, then holds the assertion to its absolute budget (§C) and to
// its committed baseline (§D3) — unless the calibration guard DECLINED, in
// which case it reports and asserts nothing.
func (r *PerfRecorder) Assert(t *testing.T, p50Budget, p95Budget time.Duration) {
	t.Helper()
	r.Report(t)

	verdict := perfVerdictPass
	switch {
	case len(r.samples) != PerfSamples:
		// A percentile over a short sample is not the percentile it claims to
		// be, so this is a failure and never a quiet report (§D1).
		verdict = perfVerdictFail
		t.Errorf("perf %s: n = %d, want exactly %d samples", r.name, len(r.samples), PerfSamples)
	case perfDeclined():
		verdict = perfVerdictDeclined
	default:
		if r.P50() > p50Budget {
			verdict = perfVerdictFail
			t.Errorf("perf %s: p50 = %s, want at most the budget %s", r.name, r.P50(), p50Budget)
		}
		if r.P95() > p95Budget {
			verdict = perfVerdictFail
			t.Errorf("perf %s: p95 = %s, want at most the budget %s", r.name, r.P95(), p95Budget)
		}
		if !r.assertBaseline(t) {
			verdict = perfVerdictFail
		}
	}

	perfRecordSummary(perfSummaryRow{
		Name:      r.name,
		N:         len(r.samples),
		P50:       r.P50(),
		P95:       r.P95(),
		P50Budget: p50Budget,
		P95Budget: p95Budget,
		Verdict:   verdict,
	})

	if perfDeclined() {
		// A skip, but never a silent one: the reason carries the calibration
		// numbers so a reader's eye cannot pass over it (§D2).
		t.Skipf("PERF DECLINED: %s — %s", r.name, perfCalibrationSummary())
	}
}

// ===========================================================================
// D3. Baselines and the regression check.
// ===========================================================================

// PerfRegressionFactor is the tolerated drift from a committed baseline. §D3
// fixes it at 20%: the absolute budget catches "this was always too slow", the
// baseline catches "this got slower", which a budget with headroom never will.
const PerfRegressionFactor = 1.20

// PerfBaselineDir holds one file per assertion, committed.
const PerfBaselineDir = "perf-baselines"

// perfBaseline is one committed measurement.
type perfBaseline struct {
	Assertion string  `json:"assertion"`
	N         int     `json:"n"`
	P50Ms     float64 `json:"p50_ms"`
	P95Ms     float64 `json:"p95_ms"`
	Cores     int     `json:"cores"`
	Tip       string  `json:"tip"`
	Measured  string  `json:"measured"`
	Note      string  `json:"note,omitempty"`
}

func perfBaselinePath(name string) string {
	return filepath.Join(PerfBaselineDir, name+".json")
}

// assertBaseline holds the recorder to its committed baseline, and answers
// whether it held.
func (r *PerfRecorder) assertBaseline(t *testing.T) bool {
	t.Helper()
	return perfAssertBaseline(t, r.name, len(r.samples), r.P50(), r.P95())
}

// perfAssertBaseline holds one measurement to its committed baseline, and
// answers whether it held. A missing baseline is reported, not failed: the
// first run of a new assertion has nothing to compare to, and
// `make -C e2e perf-baseline` writes it.
//
// A FREE FUNCTION rather than a method, because a measurement taken in ANOTHER
// RUNTIME reaches this phase as finished percentiles (§D1's TypeScript twin
// ships its p50/p95 through one ClientLog record) and is held to exactly the
// same baseline by exactly the same code.
func perfAssertBaseline(t *testing.T, name string, n int, p50, p95 time.Duration) bool {
	t.Helper()
	if perfWritingBaselines() {
		perfWriteBaseline(t, name, n, p50, p95)
		return true
	}
	body, err := os.ReadFile(perfBaselinePath(name))
	if err != nil {
		t.Logf("perf %s: no committed baseline at %s (%v); the regression check is NOT enforced for this assertion — run `make -C e2e perf-baseline` to record one",
			name, perfBaselinePath(name), err)
		return true
	}
	var base perfBaseline
	if err := json.Unmarshal(body, &base); err != nil {
		t.Errorf("perf %s: reading the baseline %s: %v", name, perfBaselinePath(name), err)
		return false
	}
	if base.Cores != runtime.NumCPU() {
		// A baseline whose core count does not match the running host is
		// REPORTED and not enforced (§D3); the calibration guard covers that
		// case instead.
		t.Logf("perf %s: the baseline was recorded on a %d-core host and this one has %d, so the regression check is NOT enforced; baseline p50=%.3fms p95=%.3fms",
			name, base.Cores, runtime.NumCPU(), base.P50Ms, base.P95Ms)
		return true
	}
	ok := true
	if got, limit := msOf(p50), base.P50Ms*PerfRegressionFactor; base.P50Ms > 0 && got > limit {
		t.Errorf("perf %s: p50 = %.3fms, want at most %.3fms — a %.0f%% regression on the baseline %.3fms (%s, tip %s)",
			name, got, limit, (got/base.P50Ms-1)*100, base.P50Ms, base.Measured, base.Tip)
		ok = false
	}
	if got, limit := msOf(p95), base.P95Ms*PerfRegressionFactor; base.P95Ms > 0 && got > limit {
		t.Errorf("perf %s: p95 = %.3fms, want at most %.3fms — a %.0f%% regression on the baseline %.3fms (%s, tip %s)",
			name, got, limit, (got/base.P95Ms-1)*100, base.P95Ms, base.Measured, base.Tip)
		ok = false
	}
	return ok
}

// perfWriteBaseline records one measurement as an assertion's baseline.
// Reached ONLY from `make -C e2e perf-baseline`.
func perfWriteBaseline(t *testing.T, name string, n int, p50, p95 time.Duration) {
	t.Helper()
	base := perfBaseline{
		Assertion: name,
		N:         n,
		P50Ms:     msOf(p50),
		P95Ms:     msOf(p95),
		Cores:     runtime.NumCPU(),
		Tip:       perfTip(),
		Measured:  time.Now().Format("2006-01-02"),
	}
	body, err := json.MarshalIndent(base, "", "  ")
	if err != nil {
		t.Fatalf("perf %s: encoding the baseline: %v", name, err)
	}
	if err := os.MkdirAll(PerfBaselineDir, 0o755); err != nil {
		t.Fatalf("perf %s: mkdir %s: %v", name, PerfBaselineDir, err)
	}
	if err := os.WriteFile(perfBaselinePath(name), append(body, '\n'), 0o644); err != nil {
		t.Fatalf("perf %s: writing %s: %v", name, perfBaselinePath(name), err)
	}
	t.Logf("perf %s: baseline written to %s", name, perfBaselinePath(name))
}

// ===========================================================================
// Measurements taken in ANOTHER RUNTIME.
// ===========================================================================

// PerfShipped is one assertion measured page-side and shipped here as finished
// percentiles.
//
// §A4 is why this shape exists at all: the webapp layer runs on faked timers,
// so only DURATIONS computed page-side mean anything and no page instant can
// be subtracted from a daemon one. The page therefore computes its own
// percentiles with the identical nearest-rank rule
// (`webapp/test/webapp-layer/perf.ts`) and ships them in ONE ClientLog record;
// this is where they meet their budget, their baseline and the summary.
type PerfShipped struct {
	Name string
	N    int
	Min  time.Duration
	P50  time.Duration
	P95  time.Duration
	Max  time.Duration
}

// Report prints the shipped distribution, in PerfRecorder.Report's own shape.
func (s PerfShipped) Report(t *testing.T) {
	t.Helper()
	t.Logf("perf %s n=%d min=%s p50=%s p95=%s max=%s (measured page-side)",
		s.Name, s.N, s.Min, s.P50, s.P95, s.Max)
	t.Logf("perf phase %s took %s", s.Name, s.P50)
}

// Assert holds a shipped measurement to its budget and its baseline, exactly
// as PerfRecorder.Assert does for a locally measured one.
func (s PerfShipped) Assert(t *testing.T, p50Budget, p95Budget time.Duration) {
	t.Helper()
	s.Report(t)

	verdict := perfVerdictPass
	switch {
	case s.N != PerfSamples:
		verdict = perfVerdictFail
		t.Errorf("perf %s: n = %d, want exactly %d samples", s.Name, s.N, PerfSamples)
	case perfDeclined():
		verdict = perfVerdictDeclined
	default:
		if s.P50 > p50Budget {
			verdict = perfVerdictFail
			t.Errorf("perf %s: p50 = %s, want at most the budget %s", s.Name, s.P50, p50Budget)
		}
		if s.P95 > p95Budget {
			verdict = perfVerdictFail
			t.Errorf("perf %s: p95 = %s, want at most the budget %s", s.Name, s.P95, p95Budget)
		}
		if !perfAssertBaseline(t, s.Name, s.N, s.P50, s.P95) {
			verdict = perfVerdictFail
		}
	}

	perfRecordSummary(perfSummaryRow{
		Name:      s.Name,
		N:         s.N,
		P50:       s.P50,
		P95:       s.P95,
		P50Budget: p50Budget,
		P95Budget: p95Budget,
		Verdict:   verdict,
	})

	if perfDeclined() {
		t.Skipf("PERF DECLINED: %s — %s", s.Name, perfCalibrationSummary())
	}
}

// msOf renders a duration in milliseconds, the unit every budget in
// PERF-SPEC.md is stated in.
func msOf(d time.Duration) float64 { return float64(d) / float64(time.Millisecond) }

// perfTip answers the commit a baseline was measured at, or "unknown". It
// shells out to the repo's git ONCE per process — this is the harness's own
// bookkeeping, not a system under test, and it reads nothing the suite mocks.
var perfTipOnce = sync.OnceValue(func() string {
	cmd := exec.Command("git", "-C", repo.repoDir, "rev-parse", "--short", "HEAD")
	out, err := cmd.Output()
	if err != nil {
		return "unknown"
	}
	return strings.TrimSpace(string(out))
})

func perfTip() string { return perfTipOnce() }

// ===========================================================================
// D2. The calibration guard.
// ===========================================================================

// PerfCalibrationLoopbackProbes is how many DaemonHealth round trips the
// loopback probe makes. 100 sequential calls of the cheapest real rpc in the
// service, over the exact transport every measured hop rides.
const PerfCalibrationLoopbackProbes = 100

// PerfCalibrationCPUIterations is the fixed iteration count of the CPU probe.
// FIXED so the result is comparable across runs of the same machine, which a
// wall-clock-only probe is not.
const PerfCalibrationCPUIterations = 20_000_000

// PerfCalibrationFactor is how far above its recorded baseline either probe
// may run before the phase DECLINES.
const PerfCalibrationFactor = 2.5

// The calibration baselines, MEASURED ON THIS BOX and recorded in
// PERF-SPEC.md §D2. They are constants rather than a baseline file because
// they gate the baseline machinery itself: a guard that reads the artifact it
// exists to protect has nothing to fall back on when that artifact is absent.
const (
	// PerfCalibrationLoopbackBaseline is the p50 of one DaemonHealth round
	// trip on an unloaded host.
	PerfCalibrationLoopbackBaseline = 700 * time.Microsecond
	// PerfCalibrationCPUBaseline is the fixed CPU probe's duration on an
	// unloaded host.
	PerfCalibrationCPUBaseline = 12 * time.Millisecond
)

type perfCalibration struct {
	Loopback time.Duration
	CPU      time.Duration
	Declined bool
	Reason   string
}

var (
	perfCalibrationOnce  sync.Once
	perfCalibrationState perfCalibration
)

// perfCalibrate runs the guard ONCE per perf phase, on the first world that
// asks for it, and caches the verdict for every later assertion.
//
// It runs on the CALLER'S world rather than building one of its own: the probe
// is meant to measure the box these assertions actually run on, and a
// dedicated world would both cost a bring-up and measure a different moment.
func perfCalibrate(t *testing.T, w *World) {
	t.Helper()
	perfCalibrationOnce.Do(func() {
		perfCalibrationState = perfRunCalibration(t, w)
		if perfCalibrationState.Declined {
			t.Logf("PERF DECLINED: %s", perfCalibrationSummary())
		} else {
			t.Logf("perf calibration: %s", perfCalibrationSummary())
		}
	})
}

func perfRunCalibration(t *testing.T, w *World) perfCalibration {
	t.Helper()
	// The loopback probe: 100 sequential DaemonHealth calls, timed as one
	// block and reported as the per-call mean. DaemonHealth has no refusal
	// site, allocates nothing interesting, and rides the same transport as
	// every measured hop.
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	start := time.Now()
	for i := 0; i < PerfCalibrationLoopbackProbes; i++ {
		if _, err := w.Client().DaemonHealth(ctx, connect.NewRequest(&agentreplv1.DaemonHealthRequest{})); err != nil {
			t.Fatalf("perf calibration: DaemonHealth probe %d: %v", i, err)
		}
	}
	loopback := time.Since(start) / PerfCalibrationLoopbackProbes

	cpu := perfCPUProbe()

	cal := perfCalibration{Loopback: loopback, CPU: cpu}
	var reasons []string
	if ratio := float64(loopback) / float64(PerfCalibrationLoopbackBaseline); ratio > PerfCalibrationFactor {
		reasons = append(reasons, fmt.Sprintf("loopback %s is %.1fx its baseline %s (limit %.1fx)",
			loopback, ratio, PerfCalibrationLoopbackBaseline, PerfCalibrationFactor))
	}
	if ratio := float64(cpu) / float64(PerfCalibrationCPUBaseline); ratio > PerfCalibrationFactor {
		reasons = append(reasons, fmt.Sprintf("cpu %s is %.1fx its baseline %s (limit %.1fx)",
			cpu, ratio, PerfCalibrationCPUBaseline, PerfCalibrationFactor))
	}
	if len(reasons) > 0 {
		cal.Declined = true
		cal.Reason = strings.Join(reasons, "; ")
	}
	return cal
}

// perfCPUProbe times a deterministic, allocation-free integer loop.
//
// The accumulator is returned into a package variable so the compiler cannot
// delete the loop; without that sink this probe measures nothing at all.
func perfCPUProbe() time.Duration {
	start := time.Now()
	acc := uint64(1)
	for i := uint64(0); i < PerfCalibrationCPUIterations; i++ {
		acc = acc*6364136223846793005 + 1442695040888963407
		acc ^= acc >> 33
	}
	elapsed := time.Since(start)
	perfCPUProbeSink = acc
	return elapsed
}

// perfCPUProbeSink keeps the CPU probe's loop alive against dead-store
// elimination. Read by nothing; that is the point.
var perfCPUProbeSink uint64

func perfDeclined() bool { return perfCalibrationState.Declined }

func perfCalibrationSummary() string {
	c := perfCalibrationState
	s := fmt.Sprintf("loopback p/call %s (baseline %s), cpu probe %s (baseline %s), factor %.1f, cores %d",
		c.Loopback, PerfCalibrationLoopbackBaseline, c.CPU, PerfCalibrationCPUBaseline,
		PerfCalibrationFactor, runtime.NumCPU())
	if c.Reason != "" {
		s += " — DECLINED: " + c.Reason
	}
	return s
}

// ===========================================================================
// The phase summary (§D4).
// ===========================================================================

type perfVerdict string

const (
	perfVerdictPass     perfVerdict = "PASS"
	perfVerdictFail     perfVerdict = "FAIL"
	perfVerdictDeclined perfVerdict = "DECLINED"
)

type perfSummaryRow struct {
	Name      string
	N         int
	P50       time.Duration
	P95       time.Duration
	P50Budget time.Duration
	P95Budget time.Duration
	Verdict   perfVerdict
}

var (
	perfSummaryMu   sync.Mutex
	perfSummaryRows []perfSummaryRow
)

func perfRecordSummary(row perfSummaryRow) {
	perfSummaryMu.Lock()
	defer perfSummaryMu.Unlock()
	perfSummaryRows = append(perfSummaryRows, row)
}

// perfReportSummary writes the phase's final block. Called from runSuite after
// m.Run, so it lands whatever the outcome; the !perf build defines it as a
// no-op, which is why main_test.go needs no build tag of its own.
func perfReportSummary() {
	perfSummaryMu.Lock()
	defer perfSummaryMu.Unlock()
	if len(perfSummaryRows) == 0 {
		return
	}
	var b strings.Builder
	b.WriteString("\n=== PERF PHASE SUMMARY ===\n")
	b.WriteString("calibration: " + perfCalibrationSummary() + "\n")
	if perfDeclined() {
		b.WriteString("PERF DECLINED: every assertion below reported its samples and asserted nothing.\n")
	}
	fmt.Fprintf(&b, "%-44s %3s %10s %10s %10s %10s  %s\n",
		"assertion", "n", "p50", "p95", "p50 budget", "p95 budget", "verdict")
	for _, row := range perfSummaryRows {
		fmt.Fprintf(&b, "%-44s %3d %10s %10s %10s %10s  %s\n",
			row.Name, row.N, row.P50, row.P95, row.P50Budget, row.P95Budget, row.Verdict)
	}
	fmt.Fprint(os.Stderr, b.String())
}
