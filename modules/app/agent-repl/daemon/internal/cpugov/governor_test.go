package cpugov

import (
	"context"
	"errors"
	"math"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// fakeSampler answers scripted ticks and a scripted power source, or their
// failures.
type fakeSampler struct {
	mu       sync.Mutex
	ticks    Ticks
	err      error
	power    Power
	powerErr error
}

func (f *fakeSampler) Ticks() (Ticks, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.ticks, f.err
}

func (f *fakeSampler) Power() (Power, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.power, f.powerErr
}

// tick advances the scripted counters by an interval busy for fraction of its
// ticks.
func (f *fakeSampler) tick(fraction float64) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.ticks.Busy += uint64(fraction * 1000)
	f.ticks.Total += 1000
	f.ticks.CPUs = 16
}

// fakeClock is a settable clock.
type fakeClock struct {
	mu  sync.Mutex
	now time.Time
}

func (c *fakeClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *fakeClock) advance(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.now = c.now.Add(d)
}

type rig struct {
	g       *Governor
	sampler *fakeSampler
	clock   *fakeClock
	log     *dlog.TestLogger
}

func newRig(t *testing.T) *rig {
	t.Helper()
	return newRigWith(t, DefaultConfig())
}

// newRigWith is newRig under cfg, for a test whose subject is not the
// default bounds (a share-accounting test needs room for more gates than the
// queue window holds).
func newRigWith(t *testing.T, cfg Config) *rig {
	t.Helper()
	r := &rig{sampler: &fakeSampler{}, clock: &fakeClock{now: time.Unix(1700000000, 0)}, log: dlog.NewTestLogger()}
	g, err := New(cfg, r.sampler, r.sampler, r.clock.Now, r.log)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	r.g = g
	return r
}

// at is a 16-processor host at a fraction of its processors.
func at(fraction float64) Sample { return Sample{Utilization: fraction, CPUs: 16} }

// roomFor is the default config with room for n concurrent gates.
func roomFor(n int) Config {
	cfg := DefaultConfig()
	cfg.MaxGates = n
	return cfg
}

func ready(w *waiter) bool {
	select {
	case <-w.ready:
		return true
	default:
		return false
	}
}

func speculative(rank int) Request { return Request{Workspace: "ws", Repo: "/repo/.git", Rank: rank} }

// recordsAt answers every record the governor logged at level under op, in
// order.
func (r *rig) recordsAt(level, op string) []dlog.Record {
	return dlog.FilterRecords(r.log.Records(), dlog.RecordQuery{Level: level, Operation: op})
}

func (r *rig) recordAt(level, op string) (dlog.Record, bool) {
	return dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: level, Operation: op})
}

func lastDecision(log *dlog.TestLogger) (string, string) {
	records := log.Records()
	for i := len(records) - 1; i >= 0; i-- {
		if records[i].Operation == "daemon.cpugov.admit" {
			return records[i].Context["decision"].(string), records[i].Context["reason"].(string)
		}
	}
	return "", ""
}

func TestASpeculativeGateIsDecidedByTheBand(t *testing.T) {
	tests := []struct {
		name         string
		utilization  float64
		wantAdmitted bool
		wantReason   string
	}{
		{name: "below the band", utilization: 0.20, wantAdmitted: true, wantReason: "below_band"},
		{name: "inside the band", utilization: 0.60, wantAdmitted: false, wantReason: "load_in_or_above_band"},
		{name: "above the band", utilization: 0.80, wantAdmitted: false, wantReason: "load_in_or_above_band"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r := newRig(t)
			r.g.Observe(at(tt.utilization))

			// Act.
			w := r.g.enqueue(speculative(1))

			// Assert.
			if ready(w) != tt.wantAdmitted {
				t.Fatalf("admitted = %v, want %v", ready(w), tt.wantAdmitted)
			}
			if _, reason := lastDecision(r.log); reason != tt.wantReason {
				t.Fatalf("decision reason = %q, want %q", reason, tt.wantReason)
			}
		})
	}
}

func TestAGateWhoseCostWouldPassTheCeilingIsDeferredBelowTheBand(t *testing.T) {
	// Arrange: a gate learned to cost four processors, a quarter of the host.
	r := newRig(t)
	r.g.mean = 4
	r.g.Observe(at(0.47))

	// Act.
	w := r.g.enqueue(speculative(1))

	// Assert.
	if ready(w) {
		t.Fatal("a gate whose cost passes the ceiling was admitted")
	}
	if _, reason := lastDecision(r.log); reason != "projected_above_ceiling" {
		t.Fatalf("decision reason = %q, want projected_above_ceiling", reason)
	}
}

func TestAJustStartedGatesShareIsCountedBeforeTheLoadShowsIt(t *testing.T) {
	// Arrange: two head gates just started, not yet in the load average.
	r := newRigWith(t, roomFor(3))
	r.g.Observe(at(0.35))
	for i := 0; i < 2; i++ {
		r.g.enqueue(Request{Workspace: "head", Rank: 0, Head: true})
	}

	// Act.
	w := r.g.enqueue(speculative(1))

	// Assert.
	if ready(w) {
		t.Fatal("a gate was admitted on a load that does not show the gates just started")
	}
	if _, reason := lastDecision(r.log); reason != "load_in_or_above_band" {
		t.Fatalf("decision reason = %q, want load_in_or_above_band", reason)
	}
}

func TestASettledGatesShareIsReadOutOfTheSample(t *testing.T) {
	// Arrange: the same two gates, long enough ago that the load average
	// holds their whole share.
	r := newRigWith(t, roomFor(3))
	for i := 0; i < 2; i++ {
		r.g.enqueue(Request{Workspace: "head", Rank: 0, Head: true})
	}
	r.clock.advance(10 * time.Minute)
	r.g.Observe(at(0.35))

	// Act.
	w := r.g.enqueue(speculative(1))

	// Assert.
	if !ready(w) {
		_, reason := lastDecision(r.log)
		t.Fatalf("deferred (%s); want the settled gates' share read out of the sample, not counted twice", reason)
	}
}

func TestNoGateIsAdmittedPastTheConcurrentGateBound(t *testing.T) {
	// Arrange: an idle host already running the bound's worth of gates,
	// long enough ago that the load holds them.
	r := newRig(t)
	for i := 0; i < QueueWindow; i++ {
		r.g.enqueue(Request{Workspace: "head", Head: true})
	}
	r.clock.advance(10 * time.Minute)
	r.g.Observe(at(0))

	// Act.
	w := r.g.enqueue(speculative(1))

	// Assert.
	if ready(w) {
		t.Fatal("a gate was admitted past the concurrent gate bound")
	}
	if _, reason := lastDecision(r.log); reason != "gate_limit" {
		t.Fatalf("decision reason = %q, want gate_limit", reason)
	}
}

func TestTheHeadsGateIsNeverGoverned(t *testing.T) {
	// Arrange: the host is saturated.
	r := newRig(t)
	r.g.Observe(at(1))

	// Act.
	w := r.g.enqueue(Request{Workspace: "head", Rank: 0, Head: true})

	// Assert.
	if !ready(w) {
		t.Fatal("the head's gate waited on the governor")
	}
}

func TestTheGateClosestToItsHeadIsAdmittedFirst(t *testing.T) {
	// Arrange: rank 2 asks first, then rank 1, before any sample; the band
	// then has room for exactly one.
	r := newRig(t)
	far := r.g.enqueue(speculative(2))
	near := r.g.enqueue(speculative(1))

	// Act.
	r.g.Observe(at(0.42))

	// Assert.
	if !ready(near) || ready(far) {
		t.Fatalf("admitted near=%v far=%v, want only the gate closest to its head", ready(near), ready(far))
	}
}

func TestNoGateIsAdmittedBeforeTheHostIsSampled(t *testing.T) {
	// Arrange.
	r := newRig(t)

	// Act.
	w := r.g.enqueue(speculative(1))

	// Assert.
	if ready(w) {
		t.Fatal("a gate was admitted with no sample of the host")
	}
	if _, reason := lastDecision(r.log); reason != "no_sample" {
		t.Fatalf("decision reason = %q, want no_sample", reason)
	}
}

func TestAnUnchangedDeferralIsRecordedOnce(t *testing.T) {
	// Arrange.
	r := newRig(t)
	r.g.Observe(at(0.6))
	r.g.enqueue(speculative(1))

	// Act.
	r.g.Observe(at(0.62))

	// Assert.
	deferrals := 0
	for _, rec := range r.recordsAt("info", "daemon.cpugov.admit") {
		if rec.Context["decision"] == "defer" {
			deferrals++
		}
	}
	if deferrals != 1 {
		t.Fatalf("deferral records = %d, want 1 for one standing decision", deferrals)
	}
}

func TestAWaitingGateIsAdmittedWhenTheLoadFalls(t *testing.T) {
	// Arrange.
	r := newRig(t)
	r.g.Observe(at(0.6))
	w := r.g.enqueue(speculative(1))

	// Act.
	r.g.Observe(at(0.2))

	// Assert.
	if !ready(w) {
		t.Fatal("the gate was not admitted once the load fell below the band")
	}
}

func TestAReleasedGateMakesRoomForTheNext(t *testing.T) {
	// Arrange: two head gates leave no room.
	r := newRig(t)
	r.g.Observe(at(0.35))
	var heads []*waiter
	for i := 0; i < 2; i++ {
		heads = append(heads, r.g.enqueue(Request{Workspace: "head", Head: true}))
	}
	w := r.g.enqueue(speculative(1))

	// Act.
	heads[0].permit.Release(0, 0, false)

	// Assert.
	if !ready(w) {
		t.Fatal("the gate was not admitted once a running gate ended")
	}
}

func TestAcquireWithdrawsAWaiterWhoseContextEnds(t *testing.T) {
	// Arrange.
	r := newRig(t)
	ctx, cancel := context.WithCancelCause(context.Background())
	cause := errors.New("the base moved")
	cancel(cause)

	// Act.
	_, err := r.g.Acquire(ctx, speculative(1))

	// Assert.
	if !errors.Is(err, cause) {
		t.Fatalf("Acquire = %v, want the context's cause", err)
	}
	if len(r.g.waiting) != 0 {
		t.Fatalf("waiting = %d, want the withdrawn gate gone", len(r.g.waiting))
	}
}

func TestReleaseLearnsTheMeanCost(t *testing.T) {
	tests := []struct {
		name     string
		cores    float64
		wall     time.Duration
		measured bool
		want     float64
	}{
		{name: "a finished run", cores: 3, wall: time.Minute, measured: true, want: 0.7*DefaultMeanCores + 0.3*3},
		{name: "a cut run", cores: 3, wall: time.Minute, measured: false, want: DefaultMeanCores},
		{name: "a run too short to say", cores: 3, wall: time.Second, measured: true, want: DefaultMeanCores},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r := newRig(t)
			w := r.g.enqueue(Request{Workspace: "head", Head: true})

			// Act.
			w.permit.Release(tt.cores, tt.wall, tt.measured)

			// Assert.
			if math.Abs(r.g.mean-tt.want) > 1e-9 {
				t.Fatalf("mean = %v, want %v", r.g.mean, tt.want)
			}
		})
	}
}

func TestASamplerFailureAdmitsNothingAndIsRecordedOnce(t *testing.T) {
	// Arrange.
	r := newRig(t)
	r.sampler.err = errors.New("sysctl refused")
	w := r.g.enqueue(speculative(1))

	// Act.
	r.g.observe()
	r.g.observe()

	// Assert.
	if ready(w) {
		t.Fatal("a gate was admitted with the host's load unread")
	}
	if errorsSeen := len(r.recordsAt("error", "daemon.cpugov.sample")); errorsSeen != 1 {
		t.Fatalf("error records = %d, want one for one standing cause", errorsSeen)
	}
}

func TestTheLoadIsTheSmoothedBusyShareOfTheHostsTicks(t *testing.T) {
	tests := []struct {
		name      string
		intervals []float64
		want      float64
	}{
		{name: "the first interval stands whole", intervals: []float64{0.4}, want: 0.4},
		{name: "a later interval moves it by its weight", intervals: []float64{0.4, 1.0}, want: 0.4 + (1-math.Exp(-5.0/30))*0.6},
		{name: "no interval yet reads nothing", intervals: nil, want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the first reading primes.
			r := newRig(t)
			r.sampler.tick(0)
			r.g.observe()

			// Act.
			for _, fraction := range tt.intervals {
				r.clock.advance(5 * time.Second)
				r.sampler.tick(fraction)
				r.g.observe()
			}

			// Assert.
			if got := r.g.sample.Utilization; math.Abs(got-tt.want) > 1e-9 {
				t.Fatalf("smoothed load = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestTicksThatGoBackwardsAdmitNothing(t *testing.T) {
	// Arrange: a primed governor whose host reports fewer ticks than before.
	r := newRig(t)
	r.sampler.tick(0)
	r.g.observe()
	r.sampler.tick(0.1)
	r.g.observe()
	r.sampler.ticks.Total = 500

	// Act.
	r.g.observe()
	r.g.observe()
	w := r.g.enqueue(speculative(1))

	// Assert.
	if r.g.sampled {
		t.Fatal("the governor kept a load across ticks that went backwards")
	}
	if _, found := r.recordAt("error", "daemon.cpugov.sample"); !found || ready(w) {
		t.Fatalf("recorded=%v admitted=%v, want an error and no admission", found, ready(w))
	}
}

func TestNewRefusesAConfigurationThatIsNoBand(t *testing.T) {
	// Arrange.
	cfg := DefaultConfig()
	cfg.Low, cfg.High = 0.8, 0.7

	// Act.
	_, err := New(cfg, &fakeSampler{}, &fakeSampler{}, time.Now, dlog.NewTestLogger())

	// Assert.
	if err == nil {
		t.Fatal("New accepted a floor above its ceiling")
	}
}

func TestNewRefusesAMissingPowerSource(t *testing.T) {
	// Act.
	_, err := New(DefaultConfig(), &fakeSampler{}, nil, time.Now, dlog.NewTestLogger())

	// Assert.
	if err == nil {
		t.Fatal("New accepted no power source")
	}
}

func TestTheBandDecidesUnlessTheHostIsOnACPower(t *testing.T) {
	tests := []struct {
		name         string
		power        Power
		wantAdmitted bool
		wantReason   string
	}{
		{name: "on AC power", power: PowerAC, wantAdmitted: true, wantReason: "on_ac_power"},
		{name: "on battery", power: PowerBattery, wantAdmitted: false, wantReason: "load_in_or_above_band"},
		{name: "power unread", power: PowerUnknown, wantAdmitted: false, wantReason: "load_in_or_above_band"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the host is saturated.
			r := newRig(t)
			r.g.Observe(at(1))
			r.g.ObservePower(tt.power)

			// Act.
			w := r.g.enqueue(speculative(1))

			// Assert.
			if ready(w) != tt.wantAdmitted {
				t.Fatalf("admitted = %v, want %v", ready(w), tt.wantAdmitted)
			}
			if _, reason := lastDecision(r.log); reason != tt.wantReason {
				t.Fatalf("decision reason = %q, want %q", reason, tt.wantReason)
			}
		})
	}
}

func TestOnACPowerAGateIsAdmittedBeforeTheHostIsSampled(t *testing.T) {
	// Arrange.
	r := newRig(t)
	r.g.ObservePower(PowerAC)

	// Act.
	w := r.g.enqueue(speculative(1))

	// Assert.
	if !ready(w) {
		t.Fatal("a gate on AC power waited for a sample of the host's load")
	}
}

func TestOnACPowerTheConcurrentGateBoundStillHolds(t *testing.T) {
	// Arrange: the bound's worth of gates already runs.
	r := newRig(t)
	r.g.ObservePower(PowerAC)
	for i := 0; i < QueueWindow; i++ {
		r.g.enqueue(Request{Workspace: "head", Head: true})
	}

	// Act.
	w := r.g.enqueue(speculative(1))

	// Assert.
	if ready(w) {
		t.Fatal("a gate on AC power was admitted past the concurrent gate bound")
	}
	if _, reason := lastDecision(r.log); reason != "gate_limit" {
		t.Fatalf("decision reason = %q, want gate_limit", reason)
	}
}

func TestPluggingInAdmitsAGateTheBandDeferred(t *testing.T) {
	// Arrange: on battery, a saturated host defers the gate.
	r := newRig(t)
	r.g.Observe(at(1))
	r.g.ObservePower(PowerBattery)
	w := r.g.enqueue(speculative(1))

	// Act.
	r.g.ObservePower(PowerAC)

	// Assert.
	if !ready(w) {
		t.Fatal("the deferred gate was not admitted once the host ran on AC power")
	}
	found := false
	for _, rec := range r.recordsAt("info", "daemon.cpugov.power") {
		if rec.Context["was"] == "battery" && rec.Context["power"] == "ac" {
			found = true
		}
	}
	if !found {
		t.Fatal("the change from battery to AC power was not recorded at INFO")
	}
}

func TestUnpluggingReturnsTheNextGateToTheBand(t *testing.T) {
	// Arrange: on AC power the first gate runs; then the host is unplugged.
	r := newRig(t)
	r.g.Observe(at(1))
	r.g.ObservePower(PowerAC)
	first := r.g.enqueue(speculative(1))

	// Act.
	r.g.ObservePower(PowerBattery)
	first.permit.Release(0, 0, false)
	next := r.g.enqueue(speculative(1))

	// Assert.
	if !ready(first) || ready(next) {
		t.Fatalf("admitted first=%v next=%v, want only the gate asked on AC power", ready(first), ready(next))
	}
}

func TestTheSamplingLoopReadsThePowerSource(t *testing.T) {
	// Arrange: the host runs on AC power and its ticks have not primed.
	r := newRig(t)
	r.sampler.power = PowerAC
	r.sampler.tick(1)
	w := r.g.enqueue(speculative(1))

	// Act.
	r.g.observe()

	// Assert.
	if !ready(w) {
		t.Fatal("the sampling loop did not lift the band on AC power")
	}
}

func TestAPowerReadFailureLeavesTheBandDecidingAndIsRecordedOnce(t *testing.T) {
	// Arrange: the host was on AC power, its load saturated; the power
	// source then cannot be read.
	r := newRig(t)
	r.sampler.power = PowerAC
	r.sampler.tick(1)
	r.g.observe()
	r.sampler.tick(1)
	r.g.observe()
	r.sampler.powerErr = errors.New("IOPSCopyPowerSourcesInfo answered nothing")

	// Act.
	r.sampler.tick(1)
	r.g.observe()
	r.sampler.tick(1)
	r.g.observe()
	w := r.g.enqueue(speculative(1))

	// Assert.
	if ready(w) {
		t.Fatal("a gate was admitted past the band with the power source unread")
	}
	errs := r.recordsAt("error", "daemon.cpugov.power")
	if len(errs) != 1 {
		t.Fatalf("error records = %d, want one for one standing cause", len(errs))
	}
	if errs[0].Context["error"] != "IOPSCopyPowerSourcesInfo answered nothing" || errs[0].Context["was"] != "ac" {
		t.Fatalf("error context = %v, want the cause and the source it was on", errs[0].Context)
	}
}

func TestRecordsAtAnswersOnlyTheLevelAndOperationAskedForInOrder(t *testing.T) {
	// Arrange.
	r := newRig(t)
	r.log.Info("daemon.cpugov.power", "first", dlog.Context{"n": 1})
	r.log.Error("daemon.cpugov.power", "an error", nil)
	r.log.Info("daemon.cpugov.sample", "another operation", nil)
	r.log.Info("daemon.cpugov.power", "second", dlog.Context{"n": 2})

	// Act.
	got := r.recordsAt("info", "daemon.cpugov.power")

	// Assert.
	if len(got) != 2 || got[0].Message != "first" || got[1].Message != "second" {
		t.Fatalf("recordsAt = %v, want the two info power records in order", got)
	}
}
