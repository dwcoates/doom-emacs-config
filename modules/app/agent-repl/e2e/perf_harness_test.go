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

// PerfBaselineEnv selects what this run does with the committed baselines.
//
//	"write"   — REWRITE them from this run's measurement. `make -C e2e
//	            perf-baseline` is the only thing that sets it: a baseline a red
//	            run can rewrite is not a baseline (§D3), so only a repetition
//	            whose budgets held records anything.
//	"enforce" — FAIL on a >20% regression (§D3).
//	unset     — REPORT a >20% regression, loudly, and do not fail. THE DEFAULT,
//	            for the reason PERF-SPEC.md §I5 finding 17 records: on the
//	            shared host this phase was built on, three rows' honest
//	            run-to-run spread exceeds the 20% tolerance, so enforcing it
//	            there produces false regressions against measurements that are
//	            nowhere near their absolute budget. The absolute budgets still
//	            fail hard on every run; it is only the drift check that waits
//	            on a calibration guard strong enough to tell a loaded box from
//	            a slow one (finding 13).
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

// perfEnforcingBaselines answers whether a regression FAILS this run.
func perfEnforcingBaselines() bool { return os.Getenv(PerfBaselineEnv) == "enforce" }

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
		if !perfBaselineStep(t, r.name, verdict, len(r.samples), r.P50(), r.P95()) {
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

// perfBaselineStep is the baseline half of an assertion: it RECORDS on a
// baseline run and ENFORCES on every other run.
//
// A RED REPETITION RECORDS NOTHING. §D3's rule is that "a baseline a red run
// can rewrite is not a baseline", and an earlier draft broke it: the write
// happened unconditionally, so a `make perf-baseline` taken while sibling
// suites saturated the box recorded the numbers of a run whose budgets it had
// just failed. The verdict so far is therefore the gate, and a skipped
// repetition says so rather than passing quietly.
func perfBaselineStep(t *testing.T, name string, verdict perfVerdict, n int, p50, p95 time.Duration) bool {
	t.Helper()
	if !perfWritingBaselines() {
		return perfAssertBaseline(t, name, n, p50, p95)
	}
	if verdict != perfVerdictPass {
		t.Logf("perf %s: this repetition FAILED its budget, so it is NOT recorded as a baseline (n=%d p50=%s p95=%s)",
			name, n, p50, p95)
		return true
	}
	perfWriteBaseline(t, name, n, p50, p95)
	return true
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
	report := func(percentile string, got, baseline float64) {
		if baseline <= 0 {
			return
		}
		limit := baseline * PerfRegressionFactor
		if got <= limit {
			return
		}
		drift := (got/baseline - 1) * 100
		if perfEnforcingBaselines() {
			t.Errorf("perf %s: %s = %.3fms, want at most %.3fms — a %.0f%% regression on the baseline %.3fms (%s, tip %s)",
				name, percentile, got, limit, drift, baseline, base.Measured, base.Tip)
			ok = false
			return
		}
		t.Logf("perf %s: BASELINE DRIFT — %s = %.3fms against the baseline %.3fms (%s, tip %s), a %.0f%% rise past the %.0f%% tolerance. "+
			"REPORTED, NOT FAILED: see PERF-SPEC.md §I5 finding 17; set %s=enforce to make it fail.",
			name, percentile, got, baseline, base.Measured, base.Tip, drift,
			(PerfRegressionFactor-1)*100, PerfBaselineEnv)
	}
	report("p50", msOf(p50), base.P50Ms)
	report("p95", msOf(p95), base.P95Ms)
	return ok
}

// perfBaselineHighWater keeps the largest p50/p95 this process has seen per
// assertion, so a `-count=N` baseline run records the OBSERVED MAXIMUM rather
// than whichever repetition happened to write last.
var (
	perfBaselineHighWaterMu sync.Mutex
	perfBaselineHighWater   = map[string][2]time.Duration{}
)

// perfWriteBaseline records one measurement as an assertion's baseline.
// Reached ONLY from `make -C e2e perf-baseline`.
//
// THE RECORDED NUMBER IS THE HIGH-WATER MARK ACROSS THE RUN'S REPETITIONS, and
// the target runs `-count=5` for exactly that reason. Run-to-run spread is
// real and unequal between rows — measured, submit-prompt-ack's p95 varied 6%
// across runs while select-workspace-ack's varied 75% — so a baseline taken
// from one repetition puts the >20% regression check inside the noise for the
// noisy rows and fails them at random. A baseline at the observed healthy
// maximum is this suite's own convention for every other bound it carries.
//
// FIVE, NOT THREE: a three-run baseline put perf-response-bubble's p95 at
// 1.539ms and the next run measured 2.036ms — a false 32% regression against a
// row whose real spread is 1.4-2.0ms, and whose BUDGET (5ms) it never came
// near. The observation window has to be wider than the spread it is meant to
// bound.
func perfWriteBaseline(t *testing.T, name string, n int, p50, p95 time.Duration) {
	t.Helper()
	perfBaselineHighWaterMu.Lock()
	if seen, ok := perfBaselineHighWater[name]; ok {
		if seen[0] > p50 {
			p50 = seen[0]
		}
		if seen[1] > p95 {
			p95 = seen[1]
		}
	}
	perfBaselineHighWater[name] = [2]time.Duration{p50, p95}
	perfBaselineHighWaterMu.Unlock()

	base := perfBaseline{
		Assertion: name,
		N:         n,
		P50Ms:     msOf(p50),
		P95Ms:     msOf(p95),
		Cores:     runtime.NumCPU(),
		Tip:       perfTip(),
		Measured:  time.Now().Format("2006-01-02"),
		Note:      "the high-water p50/p95 across the baseline run's repetitions",
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
		if !perfBaselineStep(t, s.Name, verdict, s.N, s.P50, s.P95) {
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
//
// 1.5, NOT THE 2.5 AN EARLIER DRAFT CARRIED, and the reason is a measurement
// this guard's own bring-up produced: THE CPU PROBE IS BARELY SENSITIVE TO
// LOAD ON A 16-CORE HOST. Sampled 25 times at load average 3.9 it ran
// 42.0-43.0 ms, and inside a perf run at load average 10.6 it ran 42.0 ms —
// a single-threaded loop does not slow down while free cores remain, so on
// this box the probe only moves once the load average passes the core count.
// A 2.5x gate on a probe with that little travel is a gate that never closes.
const PerfCalibrationFactor = 1.5

// The calibration baselines, MEASURED ON THIS BOX and recorded in
// PERF-SPEC.md §D2. They are constants rather than a baseline file because
// they gate the baseline machinery itself: a guard that reads the artifact it
// exists to protect has nothing to fall back on when that artifact is absent.
const (
	// PerfCalibrationLoopbackBaseline is the p50 of one DaemonHealth round
	// trip. MEASURED — see PERF-SPEC.md §I2 for the readings; set at a
	// round-up of the observed max, so the gate closes at 1.5x it.
	//
	// THIS IS THE PROBE THAT ACTUALLY DISCRIMINATES: unlike the CPU loop it
	// crosses a socket and schedules two processes, so it degrades as soon as
	// the box has more runnable work than it can dispatch promptly.
	PerfCalibrationLoopbackBaseline = 230 * time.Microsecond
	// PerfCalibrationCPUBaseline is the fixed CPU probe's duration, MEASURED:
	// 42.0 ms as the minimum of 25 samples at load average 3.9, on the same
	// host. An earlier draft guessed 12 ms and DECLINED every run — the guess
	// was wrong by 3.5x, which is exactly why a threshold in this suite is a
	// measurement and never an estimate.
	PerfCalibrationCPUBaseline = 42 * time.Millisecond
)

type perfCalibration struct {
	Loopback time.Duration
	CPU      time.Duration
	Declined bool
	Reason   string
}

var (
	perfCalibrationMu sync.Mutex
	// perfCalibrationState is the CURRENT assertion's calibration.
	perfCalibrationState perfCalibration
	// perfPhaseHadDecline records whether ANY assertion in this phase was
	// declined, for the summary block.
	perfPhaseHadDecline bool
)

// perfCalibrate runs the guard for the assertion the caller is about to take,
// on the caller's world.
//
// PER-ASSERTION, WHICH IS A DELIBERATE DEVIATION FROM §D2's "ONCE PER PHASE",
// and PERF-SPEC.md §I5 finding 13 records the two measurements that forced it:
//
//  1. A reading taken once at the start of a phase cannot see load that arrives
//     during it, and on a box shared with sibling suites that is the normal
//     case, not the exception: a whole `-count=3` baseline run measured 2-4x
//     its quiet figures while the start-of-phase probe read normal.
//  2. Made sticky for the phase, the converse happens: ONE spike declines every
//     assertion after it, including the ~2.5 minutes of a `-count=3` run whose
//     own samples came back at their quiet figures. Observed twice.
//
// §D2's intent — never assert a latency budget against a box that was saturated
// when the sample was taken — is served exactly by probing at each assertion
// and declining that assertion. The PHASE still reports DECLINED in its summary
// if any assertion was declined, so a green run that measured nothing still
// cannot be mistaken for a green run that measured something.
//
// The CPU probe is fixed work and runs once per process; the loopback probe is
// re-taken here, at ~20ms per assertion.
//
// It runs on the CALLER'S world rather than building one of its own: the probe
// is meant to measure the box this assertion actually runs on, and a dedicated
// world would both cost a bring-up and measure a different moment.
func perfCalibrate(t *testing.T, w *World) {
	t.Helper()
	loopback := perfLoopbackProbe(t, w)
	cpu := perfCalibrationCPUOnce()
	declined, reason := perfCalibrationVerdict(loopback, cpu)

	perfCalibrationMu.Lock()
	defer perfCalibrationMu.Unlock()
	perfCalibrationState = perfCalibration{Loopback: loopback, CPU: cpu, Declined: declined, Reason: reason}
	if declined {
		perfPhaseHadDecline = true
		t.Logf("PERF DECLINED: %s", perfCalibrationSummaryLocked())
		return
	}
	t.Logf("perf calibration: %s", perfCalibrationSummaryLocked())
}

// perfLoopbackProbe times PerfCalibrationLoopbackProbes sequential DaemonHealth
// calls and answers their p50. DaemonHealth has no refusal site, allocates
// nothing interesting, and rides the same transport as every measured hop.
//
// THE p50 OF THE HUNDRED, NOT THEIR MEAN, and this cost a whole baseline run to
// learn: the mean is one stall away from anything, and a probe that reads
// 600 µs because ONE of a hundred calls took 40 ms declines a phase that is
// perfectly measurable. Observed exactly that — a single early probe declined
// three whole runs whose own samples came in at their quiet figures. The
// percentile rule is also what §D1 already requires of every other number in
// this phase; the probe had no business being the exception.
func perfLoopbackProbe(t *testing.T, w *World) time.Duration {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	samples := make([]time.Duration, 0, PerfCalibrationLoopbackProbes)
	for i := 0; i < PerfCalibrationLoopbackProbes; i++ {
		start := time.Now()
		if _, err := w.Client().DaemonHealth(ctx, connect.NewRequest(&agentreplv1.DaemonHealthRequest{})); err != nil {
			t.Fatalf("perf calibration: DaemonHealth probe %d: %v", i, err)
		}
		samples = append(samples, time.Since(start))
	}
	sort.Slice(samples, func(i, j int) bool { return samples[i] < samples[j] })
	return samples[len(samples)/2-1]
}

// perfCalibrationCPUOnce times the fixed CPU loop once per process.
var perfCalibrationCPUOnce = sync.OnceValue(perfCPUProbe)

// perfCalibrationVerdict answers whether these probe readings decline the
// phase, and why.
func perfCalibrationVerdict(loopback, cpu time.Duration) (bool, string) {
	var reasons []string
	if ratio := float64(loopback) / float64(PerfCalibrationLoopbackBaseline); ratio > PerfCalibrationFactor {
		reasons = append(reasons, fmt.Sprintf("loopback %s is %.1fx its baseline %s (limit %.1fx)",
			loopback, ratio, PerfCalibrationLoopbackBaseline, PerfCalibrationFactor))
	}
	if ratio := float64(cpu) / float64(PerfCalibrationCPUBaseline); ratio > PerfCalibrationFactor {
		reasons = append(reasons, fmt.Sprintf("cpu %s is %.1fx its baseline %s (limit %.1fx)",
			cpu, ratio, PerfCalibrationCPUBaseline, PerfCalibrationFactor))
	}
	return len(reasons) > 0, strings.Join(reasons, "; ")
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

func perfDeclined() bool {
	perfCalibrationMu.Lock()
	defer perfCalibrationMu.Unlock()
	return perfCalibrationState.Declined
}

func perfCalibrationSummary() string {
	perfCalibrationMu.Lock()
	defer perfCalibrationMu.Unlock()
	return perfCalibrationSummaryLocked()
}

// perfCalibrationSummaryLocked is perfCalibrationSummary with the lock already
// held by the caller.
func perfCalibrationSummaryLocked() string {
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
	b.WriteString("last calibration: " + perfCalibrationSummary() + "\n")
	perfCalibrationMu.Lock()
	hadDecline := perfPhaseHadDecline
	perfCalibrationMu.Unlock()
	if hadDecline {
		b.WriteString("PERF DECLINED: at least one assertion below reported its samples and asserted nothing; " +
			"a DECLINED row is neither a pass nor a failure.\n")
	}
	fmt.Fprintf(&b, "%-44s %3s %10s %10s %10s %10s  %s\n",
		"assertion", "n", "p50", "p95", "p50 budget", "p95 budget", "verdict")
	for _, row := range perfSummaryRows {
		fmt.Fprintf(&b, "%-44s %3d %10s %10s %10s %10s  %s\n",
			row.Name, row.N, row.P50, row.P95, row.P50Budget, row.P95Budget, row.Verdict)
	}
	fmt.Fprint(os.Stderr, b.String())
}
