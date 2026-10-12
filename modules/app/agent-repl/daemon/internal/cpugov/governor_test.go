package cpugov

import (
	"context"
	"errors"
	"math"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/dlog"
)

// fakeSampler answers a scripted sample or failure.
type fakeSampler struct {
	mu     sync.Mutex
	sample Sample
	err    error
}

func (f *fakeSampler) Sample() (Sample, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.sample, f.err
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
	r := &rig{sampler: &fakeSampler{}, clock: &fakeClock{now: time.Unix(1700000000, 0)}, log: dlog.NewTestLogger()}
	g, err := New(DefaultConfig(), r.sampler, r.clock.Now, r.log)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	r.g = g
	return r
}

// at is a 16-processor host at a fraction of its processors.
func at(fraction float64) Sample { return Sample{Load1: fraction * 16, CPUs: 16} }

func ready(w *waiter) bool {
	select {
	case <-w.ready:
		return true
	default:
		return false
	}
}

func speculative(rank int) Request { return Request{Workspace: "ws", Repo: "/repo/.git", Rank: rank} }

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
	// Arrange: three head gates just started, not yet in the load average.
	r := newRig(t)
	r.g.Observe(at(0.25))
	for i := 0; i < 3; i++ {
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
	// Arrange: the same three gates, long enough ago that the load average
	// holds their whole share.
	r := newRig(t)
	for i := 0; i < 3; i++ {
		r.g.enqueue(Request{Workspace: "head", Rank: 0, Head: true})
	}
	r.clock.advance(10 * time.Minute)
	r.g.Observe(at(0.25))

	// Act.
	w := r.g.enqueue(speculative(1))

	// Assert.
	if !ready(w) {
		_, reason := lastDecision(r.log)
		t.Fatalf("deferred (%s); want the settled gates' share read out of the sample, not counted twice", reason)
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
	for _, rec := range r.log.Records() {
		if rec.Operation == "daemon.cpugov.admit" && rec.Context["decision"] == "defer" {
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
	// Arrange: three head gates leave no room.
	r := newRig(t)
	r.g.Observe(at(0.25))
	var heads []*waiter
	for i := 0; i < 3; i++ {
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
	errorsSeen := 0
	for _, rec := range r.log.Records() {
		if rec.Level == "error" && rec.Operation == "daemon.cpugov.sample" {
			errorsSeen++
		}
	}
	if errorsSeen != 1 {
		t.Fatalf("error records = %d, want one for one standing cause", errorsSeen)
	}
}

func TestSampleUtilization(t *testing.T) {
	tests := []struct {
		name   string
		sample Sample
		want   float64
	}{
		{name: "a fraction", sample: Sample{Load1: 4, CPUs: 16}, want: 0.25},
		{name: "saturated is capped", sample: Sample{Load1: 40, CPUs: 16}, want: 1},
		{name: "no processors reads as saturated", sample: Sample{Load1: 1}, want: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := tt.sample.Utilization()

			// Assert.
			if got != tt.want {
				t.Fatalf("Utilization = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestNewRefusesAConfigurationThatIsNoBand(t *testing.T) {
	// Arrange.
	cfg := DefaultConfig()
	cfg.Low, cfg.High = 0.8, 0.7

	// Act.
	_, err := New(cfg, &fakeSampler{}, time.Now, dlog.NewTestLogger())

	// Assert.
	if err == nil {
		t.Fatal("New accepted a floor above its ceiling")
	}
}
