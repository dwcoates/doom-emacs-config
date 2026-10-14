package cpugov

import (
	"context"
	"errors"
	"slices"
	"sync"
	"testing"

	"agent-repld/internal/dlog"
)

// fakeSource answers a scripted power source, or its failure.
type fakeSource struct {
	mu    sync.Mutex
	power Power
	err   error
}

func (f *fakeSource) Power() (Power, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.power, f.err
}

type rig struct {
	g      *Governor
	source *fakeSource
	log    *dlog.TestLogger
}

func newRig(t *testing.T) *rig {
	t.Helper()
	return newRigWith(t, DefaultConfig())
}

func newRigWith(t *testing.T, cfg Config) *rig {
	t.Helper()
	return newRigBorn(t, cfg, false)
}

// newRigBorn builds a rig whose governor is born with racingDisabled.
func newRigBorn(t *testing.T, cfg Config, racingDisabled bool) *rig {
	t.Helper()
	r := &rig{source: &fakeSource{}, log: dlog.NewTestLogger()}
	g, err := New(cfg, r.source, racingDisabled, r.log)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	r.g = g
	return r
}

// recordsAt answers every record the governor logged at level under op, in
// order.
func (r *rig) recordsAt(level, op string) []dlog.Record {
	return dlog.FilterRecords(r.log.Records(), dlog.RecordQuery{Level: level, Operation: op})
}

func ready(w *waiter) bool {
	select {
	case <-w.ready:
		return true
	default:
		return false
	}
}

func behind(rank int) Request { return Request{Workspace: "ws", Repo: "/repo/.git", Rank: rank} }

func head() Request { return Request{Workspace: "head", Repo: "/repo/.git", Head: true} }

func TestNewRefusesAMissingCollaboratorOrBound(t *testing.T) {
	tests := []struct {
		name   string
		cfg    Config
		source PowerSource
		log    dlog.Logger
	}{
		{name: "no power source", cfg: DefaultConfig(), log: dlog.NewTestLogger()},
		{name: "no logger", cfg: DefaultConfig(), source: &fakeSource{}},
		{name: "no gate bound", cfg: Config{}, source: &fakeSource{}, log: dlog.NewTestLogger()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			_, err := New(tt.cfg, tt.source, false, tt.log)

			// Assert.
			if err == nil {
				t.Fatal("New accepted it")
			}
		})
	}
}

func TestTheWindowFollowsThePowerSource(t *testing.T) {
	tests := []struct {
		name  string
		power Power
		want  int
	}{
		{name: "on AC power the top two race", power: PowerAC, want: QueueWindow},
		{name: "on battery the head alone", power: PowerBattery, want: BatteryWindow},
		{name: "unread, the head alone", power: PowerUnknown, want: BatteryWindow},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r := newRig(t)

			// Act.
			r.g.ObservePower(tt.power)

			// Assert.
			if got := r.g.Window(); got != tt.want {
				t.Fatalf("Window = %d, want %d", got, tt.want)
			}
		})
	}
}

func TestTheWindowFollowsTheRacingSwitch(t *testing.T) {
	tests := []struct {
		name     string
		power    Power
		disabled bool
		want     int
	}{
		{name: "on AC power with racing enabled the top two race", power: PowerAC, want: QueueWindow},
		{name: "on AC power with racing disabled the head alone", power: PowerAC, disabled: true, want: BatteryWindow},
		{name: "on battery with racing disabled the head alone", power: PowerBattery, disabled: true, want: BatteryWindow},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r := newRig(t)
			r.g.ObservePower(tt.power)

			// Act.
			r.g.SetRacingDisabled(tt.disabled)

			// Assert.
			if got := r.g.Window(); got != tt.want {
				t.Fatalf("Window = %d, want %d", got, tt.want)
			}
		})
	}
}

func TestAGovernorBornWithRacingDisabledNeverRacesOnACPower(t *testing.T) {
	// Arrange.
	r := newRigBorn(t, DefaultConfig(), true)

	// Act.
	r.g.ObservePower(PowerAC)

	// Assert.
	if got := r.g.Window(); got != BatteryWindow {
		t.Fatalf("Window = %d, want the head alone", got)
	}
	if !r.g.Mode().RacingDisabled {
		t.Fatal("the governor forgot the racing switch it was born with")
	}
}

func TestARacingSwitchChangeTellsEverySubscriberAndIsRecorded(t *testing.T) {
	// Arrange: on AC power, racing enabled.
	r := newRig(t)
	r.g.ObservePower(PowerAC)
	var told []Mode
	r.g.OnModeChange(func(m Mode) { told = append(told, m) })

	// Act.
	r.g.SetRacingDisabled(true)
	r.g.SetRacingDisabled(false)

	// Assert.
	want := []Mode{{Power: PowerAC, RacingDisabled: true}, {Power: PowerAC}}
	if !slices.Equal(told, want) {
		t.Fatalf("told = %v, want %v", told, want)
	}
	records := r.recordsAt("info", "daemon.cpugov.racing")
	if len(records) != 2 {
		t.Fatalf("racing records = %d, want one per change", len(records))
	}
	if records[0].Context["racing_disabled"] != true || records[0].Context["window"] != BatteryWindow {
		t.Fatalf("record context = %v, want racing disabled with the head-alone window", records[0].Context)
	}
}

func TestAnUnchangedRacingSwitchTellsNoSubscriber(t *testing.T) {
	// Arrange.
	r := newRigBorn(t, DefaultConfig(), true)
	told := 0
	r.g.OnModeChange(func(Mode) { told++ })

	// Act.
	r.g.SetRacingDisabled(true)

	// Assert.
	if told != 0 {
		t.Fatalf("told %d times, want none for an unchanged switch", told)
	}
	if n := len(r.recordsAt("info", "daemon.cpugov.racing")); n != 0 {
		t.Fatalf("racing records = %d, want none for an unchanged switch", n)
	}
}

func TestAPowerChangeTellsEverySubscriberInOrder(t *testing.T) {
	// Arrange.
	r := newRig(t)
	var told []string
	r.g.OnModeChange(func(m Mode) { told = append(told, "first:"+m.Power.String()) })
	r.g.OnModeChange(func(m Mode) { told = append(told, "second:"+m.Power.String()) })

	// Act.
	r.g.ObservePower(PowerAC)

	// Assert.
	if want := []string{"first:ac", "second:ac"}; !slices.Equal(told, want) {
		t.Fatalf("told = %v, want %v", told, want)
	}
	if len(r.recordsAt("info", "daemon.cpugov.power")) != 1 {
		t.Fatal("the change was not recorded at INFO")
	}
}

func TestAnUnchangedPowerSourceTellsNoSubscriber(t *testing.T) {
	// Arrange.
	r := newRig(t)
	r.g.ObservePower(PowerBattery)
	told := 0
	r.g.OnModeChange(func(Mode) { told++ })

	// Act.
	r.g.ObservePower(PowerBattery)

	// Assert.
	if told != 0 {
		t.Fatalf("told %d times, want none for an unchanged source", told)
	}
}

func TestTheReadingLoopReadsThePowerSource(t *testing.T) {
	// Arrange.
	r := newRig(t)
	r.source.power = PowerAC

	// Act.
	r.g.observe()

	// Assert.
	if got := r.g.Mode().Power; got != PowerAC {
		t.Fatalf("Power = %v, want ac", got)
	}
}

func TestAnUnreadablePowerSourceWorksTheHeadAloneAndIsRecordedOnce(t *testing.T) {
	// Arrange: the host was on AC power.
	r := newRig(t)
	r.source.power = PowerAC
	r.g.observe()
	var told []Power
	r.g.OnModeChange(func(m Mode) { told = append(told, m.Power) })
	r.source.err = errors.New("IOPSCopyPowerSourcesInfo answered nothing")

	// Act.
	r.g.observe()
	r.g.observe()

	// Assert.
	if got := r.g.Window(); got != BatteryWindow {
		t.Fatalf("Window = %d, want the head alone", got)
	}
	if !slices.Equal(told, []Power{PowerUnknown}) {
		t.Fatalf("told = %v, want one change to unknown", told)
	}
	errs := r.recordsAt("error", "daemon.cpugov.power")
	if len(errs) != 1 {
		t.Fatalf("error records = %d, want one for one standing cause", len(errs))
	}
	if errs[0].Context["error"] != "IOPSCopyPowerSourcesInfo answered nothing" || errs[0].Context["was"] != "ac" {
		t.Fatalf("error context = %v, want the cause and the source it was on", errs[0].Context)
	}
}

func TestTheHeadsGateIsNeverGoverned(t *testing.T) {
	// Arrange: the bound is already full.
	r := newRig(t)
	for i := 0; i < QueueWindow; i++ {
		r.g.enqueue(head())
	}

	// Act.
	w := r.g.enqueue(head())

	// Assert.
	if !ready(w) {
		t.Fatal("the head's gate waited on the governor")
	}
}

func TestAGateBehindItsHeadIsAdmittedWhileTheBoundHasRoom(t *testing.T) {
	// Arrange.
	r := newRig(t)
	r.g.enqueue(head())

	// Act.
	w := r.g.enqueue(behind(1))

	// Assert.
	if !ready(w) {
		t.Fatal("a gate the bound had room for waited")
	}
}

func TestNoGateIsAdmittedPastTheConcurrentGateBound(t *testing.T) {
	// Arrange.
	r := newRig(t)
	for i := 0; i < QueueWindow; i++ {
		r.g.enqueue(head())
	}

	// Act.
	w := r.g.enqueue(behind(1))

	// Assert.
	if ready(w) {
		t.Fatal("a gate was admitted past the concurrent gate bound")
	}
	if got := r.recordsAt("info", "daemon.cpugov.admit"); got[len(got)-1].Context["reason"] != "gate_limit" {
		t.Fatalf("last decision = %v, want a gate_limit deferral", got[len(got)-1].Context)
	}
}

func TestAnUnchangedDeferralIsRecordedOnce(t *testing.T) {
	// Arrange.
	r := newRig(t)
	for i := 0; i < QueueWindow; i++ {
		r.g.enqueue(head())
	}
	r.g.enqueue(behind(1))

	// Act.
	r.g.enqueue(behind(2))

	// Assert.
	deferrals := 0
	for _, rec := range r.recordsAt("info", "daemon.cpugov.admit") {
		if rec.Context["decision"] == "defer" {
			deferrals++
		}
	}
	if deferrals != 1 {
		t.Fatalf("deferral records = %d, want 1 for the one gate at the front of the line", deferrals)
	}
}

func TestTheGateClosestToItsHeadIsAdmittedFirst(t *testing.T) {
	// Arrange: the bound is full; rank 2 asks first, then rank 1.
	r := newRigWith(t, Config{MaxGates: 1})
	first := r.g.enqueue(head())
	far := r.g.enqueue(behind(2))
	near := r.g.enqueue(behind(1))

	// Act.
	first.permit.Release()

	// Assert.
	if !ready(near) || ready(far) {
		t.Fatalf("admitted near=%v far=%v, want only the gate closest to its head", ready(near), ready(far))
	}
}

func TestAReleasedGateMakesRoomForTheNext(t *testing.T) {
	// Arrange.
	r := newRigWith(t, Config{MaxGates: 1})
	first := r.g.enqueue(head())
	next := r.g.enqueue(behind(1))

	// Act.
	first.permit.Release()
	first.permit.Release()

	// Assert.
	if !ready(next) || len(r.g.active) != 1 {
		t.Fatalf("admitted=%v active=%d, want the next gate admitted and one released once", ready(next), len(r.g.active))
	}
}

func TestAcquireWithdrawsAWaiterWhoseContextEnds(t *testing.T) {
	// Arrange: the bound is full, so the gate waits.
	r := newRigWith(t, Config{MaxGates: 1})
	r.g.enqueue(head())
	ctx, cancel := context.WithCancelCause(context.Background())
	cause := errors.New("the base moved")
	cancel(cause)

	// Act.
	_, err := r.g.Acquire(ctx, behind(1))

	// Assert.
	if !errors.Is(err, cause) {
		t.Fatalf("Acquire = %v, want the context's cause", err)
	}
	if len(r.g.waiting) != 0 {
		t.Fatalf("waiting = %d, want the withdrawn gate gone", len(r.g.waiting))
	}
}

func TestHostPowerSourceReadsTheFixedOverride(t *testing.T) {
	tests := []struct {
		fixed string
		want  PowerSource
	}{
		{fixed: "", want: &Host{}},
		{fixed: "ac", want: Fixed{Source: PowerAC}},
		{fixed: "battery", want: Fixed{Source: PowerBattery}},
	}
	for _, tt := range tests {
		t.Run(tt.fixed, func(t *testing.T) {
			// Act.
			got, err := HostPowerSource(tt.fixed)

			// Assert.
			if err != nil {
				t.Fatalf("HostPowerSource(%q): %v", tt.fixed, err)
			}
			switch want := tt.want.(type) {
			case *Host:
				if _, ok := got.(*Host); !ok {
					t.Fatalf("HostPowerSource(%q) = %T, want the host's own", tt.fixed, got)
				}
			case Fixed:
				if got != want {
					t.Fatalf("HostPowerSource(%q) = %v, want %v", tt.fixed, got, want)
				}
			}
		})
	}
}

func TestHostPowerSourceRefusesAnUnknownOverride(t *testing.T) {
	for _, fixed := range []string{"AC", "plugged", "1"} {
		t.Run(fixed, func(t *testing.T) {
			// Act.
			got, err := HostPowerSource(fixed)

			// Assert.
			if err == nil || got != nil {
				t.Fatalf("HostPowerSource(%q) = %v, %v; want the refusal", fixed, got, err)
			}
		})
	}
}

func TestAFixedSourceAlwaysAnswersItsSource(t *testing.T) {
	// Act.
	got, err := Fixed{Source: PowerBattery}.Power()

	// Assert.
	if err != nil || got != PowerBattery {
		t.Fatalf("Power = %v, %v; want battery", got, err)
	}
}
