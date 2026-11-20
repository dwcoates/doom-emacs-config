package cores

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	"agentrepl/logging/hostcpu"
	"agentrepl/logging/hostpower"
	"agentrepl/testrun/internal/seats"
)

func TestTargetAndCeilingCores(t *testing.T) {
	cases := []struct {
		name                    string
		limits                  Limits
		wantTarget, wantCeiling float64
	}{
		{name: "the base limits are 35% and 50% of sixteen cores", limits: BaseLimits, wantTarget: 5.6, wantCeiling: 8},
		{name: "the powered limits are 50% and 55% of sixteen cores", limits: PoweredLimits, wantTarget: 8, wantCeiling: 8.8},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			target, ceiling := tc.limits.TargetCores(16), tc.limits.CeilingCores(16)

			// Assert
			if target != tc.wantTarget || ceiling != tc.wantCeiling {
				t.Fatalf("target %v, ceiling %v; want %v and %v", target, ceiling, tc.wantTarget, tc.wantCeiling)
			}
		})
	}
}

func TestHostPowerLimits(t *testing.T) {
	cases := []struct {
		name  string
		power HostPower
		want  Limits
	}{
		{name: "AC power in High Power Mode allows the powered limits", power: HostPower{Source: hostpower.PowerAC, EnergyMode: hostpower.EnergyModeHighPower}, want: PoweredLimits},
		{name: "AC power in Low Power Mode keeps the base limits", power: HostPower{Source: hostpower.PowerAC, EnergyMode: hostpower.EnergyModeLowPower}, want: BaseLimits},
		{name: "AC power in the automatic mode keeps the base limits", power: HostPower{Source: hostpower.PowerAC, EnergyMode: "0"}, want: BaseLimits},
		{name: "AC power on a host with no energy mode keeps the base limits", power: HostPower{Source: hostpower.PowerAC}, want: BaseLimits},
		{name: "the battery in High Power Mode keeps the base limits", power: HostPower{Source: hostpower.PowerBattery, EnergyMode: hostpower.EnergyModeHighPower}, want: BaseLimits},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.power.Limits()

			// Assert
			if got != tc.want {
				t.Fatalf("Limits() = %+v; want %+v", got, tc.want)
			}
		})
	}
}

// powerSource is a hostpower.PowerSource answering one reading.
type powerSource struct {
	power hostpower.Power
	err   error
}

func (p powerSource) Power() (hostpower.Power, error) { return p.power, p.err }

func TestReadHostPower(t *testing.T) {
	noSource, noMode := errors.New("IOPSCopyPowerSourcesInfo answered nothing"), errors.New("pmset -g exited 1")
	cases := []struct {
		name    string
		source  powerSource
		mode    string
		modeErr error
		want    HostPower
		wantErr error
	}{
		{name: "the source and the energy mode together", source: powerSource{power: hostpower.PowerAC}, mode: hostpower.EnergyModeHighPower,
			want: HostPower{Source: hostpower.PowerAC, EnergyMode: hostpower.EnergyModeHighPower}},
		{name: "a source that cannot be read answers its error", source: powerSource{err: noSource}, mode: hostpower.EnergyModeHighPower, wantErr: noSource},
		{name: "an energy mode that cannot be read answers its error", source: powerSource{power: hostpower.PowerAC}, modeErr: noMode, wantErr: noMode},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := ReadHostPower(tc.source, func() (string, error) { return tc.mode, tc.modeErr })

			// Assert
			if !errors.Is(err, tc.wantErr) || got != tc.want {
				t.Fatalf("ReadHostPower = %+v, %v; want %+v, %v", got, err, tc.want, tc.wantErr)
			}
		})
	}
}

func TestExpectedCores(t *testing.T) {
	cases := []struct {
		name     string
		width    int
		measured float64
		known    bool
		want     float64
	}{
		{name: "a unit with history is costed by it", width: 2, measured: 6.1, known: true, want: 6.1},
		{name: "a light unit with history is costed by it", width: 1, measured: 1.1, known: true, want: 1.1},
		{name: "a unit with no history is costed the default a slot", width: 2, want: 4},
		{name: "a one-slot unit with no history is costed the default", width: 1, want: DefaultCoresPerSlot},
		{name: "a history of no CPU is no history", width: 1, measured: 0, known: true, want: DefaultCoresPerSlot},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := ExpectedCores(tc.width, tc.measured, tc.known)

			// Assert
			if got != tc.want {
				t.Fatalf("ExpectedCores(%d, %v, %v) = %v; want %v", tc.width, tc.measured, tc.known, got, tc.want)
			}
		})
	}
}

func TestAdmits(t *testing.T) {
	// Sixteen cores: the tests' target is 5.6, the host's ceiling 8, under
	// the base limits; 8 and 8.8 under the powered ones.
	cases := []struct {
		name   string
		limits Limits
		load   Load
		cores  float64
		want   bool
	}{
		{name: "tests under their target but the host at its ceiling admit nothing",
			load: Load{HostCores: 8, TestsCores: 1, Running: true}, cores: 1, want: false},
		{name: "an otherwise idle host with the tests at their target admits nothing",
			load: Load{HostCores: 5.6, TestsCores: 5.6, Running: true}, cores: 0.5, want: false},
		{name: "a heavy unit that would take the tests past their target waits",
			load: Load{HostCores: 2, TestsCores: 2, Running: true}, cores: 6.1, want: false},
		{name: "a light unit within the target and the ceiling starts",
			load: Load{HostCores: 2, TestsCores: 2, Running: true}, cores: 1.1, want: true},
		{name: "a unit that brings the tests exactly to their target starts",
			load: Load{HostCores: 3, TestsCores: 3, Running: true}, cores: 2.6, want: true},
		{name: "the rest of the host's load does not eat the tests' target",
			load: Load{HostCores: 5, TestsCores: 1, Running: true}, cores: 2, want: true},
		{name: "the rest of the host's load still bounds the host at its ceiling",
			load: Load{HostCores: 6, TestsCores: 1, Running: true}, cores: 2.5, want: false},
		{name: "units that just started count toward the tests",
			load: Load{HostCores: 1, TestsCores: 1, RampCores: 4, Running: true}, cores: 1, want: false},
		{name: "the first unit starts whatever it costs while the rest of the host is under the ceiling",
			load: Load{HostCores: 7.9}, cores: 6.1, want: true},
		{name: "the first unit waits while the rest of the host is at the ceiling",
			load: Load{HostCores: 8}, cores: 1, want: false},
		{name: "lingering test processes do not hold the first unit back",
			load: Load{HostCores: 9, TestsCores: 6}, cores: 1, want: true},
		{name: "the powered target admits a unit the base target holds back",
			limits: PoweredLimits, load: Load{HostCores: 5, TestsCores: 5, Running: true}, cores: 2, want: true},
		{name: "a unit that would take the tests past the powered target waits",
			limits: PoweredLimits, load: Load{HostCores: 8, TestsCores: 8, Running: true}, cores: 1, want: false},
		{name: "the powered ceiling still bounds the host",
			limits: PoweredLimits, load: Load{HostCores: 10, TestsCores: 2, Running: true}, cores: 2, want: false},
		{name: "the first unit starts while the rest of the host is under the powered ceiling",
			limits: PoweredLimits, load: Load{HostCores: 8.5}, cores: 6.1, want: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			limits := tc.limits
			if limits == (Limits{}) {
				limits = BaseLimits
			}

			// Act
			got := Admits(16, limits, tc.load, tc.cores)

			// Assert
			if got != tc.want {
				t.Fatalf("Admits(16, %+v, %+v, %v) = %v; want %v", limits, tc.load, tc.cores, got, tc.want)
			}
		})
	}
}

func TestReadState(t *testing.T) {
	cases := []struct {
		name    string
		file    string
		write   bool
		want    CPUState
		wantErr string
	}{
		{name: "no governor has stated anything: nothing waits for the CPU", want: CPUState{CapPercent: 35, CeilingPercent: 50}},
		{name: "a stated wait", write: true, file: `{"waiting_for_cpu":true,"busy_percent":71.5,"tests_percent":30,"cap_percent":35,"ceiling_percent":50}`, want: CPUState{WaitingForCPU: true, BusyPercent: 71.5, TestsPercent: 30, CapPercent: 35, CeilingPercent: 50}},
		{name: "a corrupt state is refused", write: true, file: `{"waiting_for_cpu":`, wantErr: "read the CPU governor's state"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			if tc.write {
				if err := os.WriteFile(filepath.Join(dir, StateFile), []byte(tc.file), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, err := ReadState(dir)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("ReadState = %+v, %v; want an error containing %q", got, err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("ReadState = %+v, %v; want %+v", got, err, tc.want)
			}
		})
	}
}

func TestWriteStateReadsBackWhole(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	want := CPUState{WaitingForCPU: true, BusyPercent: 80, CapPercent: BaseLimits.Target}

	// Act
	if err := writeState(dir, want); err != nil {
		t.Fatal(err)
	}
	got, err := ReadState(dir)

	// Assert: the state, and no temp file left beside it.
	if err != nil || got != want {
		t.Fatalf("ReadState = %+v, %v; want %+v", got, err, want)
	}
	entries, err := os.ReadDir(dir)
	if err != nil || len(entries) != 1 {
		t.Fatalf("dir holds %v, %v; want the state file alone", entries, err)
	}
}

func TestWriteStateSurfacesAnUnwritableDirectory(t *testing.T) {
	// Act
	err := writeState(filepath.Join(t.TempDir(), "missing"), CPUState{})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "write the CPU governor's state") {
		t.Fatalf("writeState = %v; want the write's failure", err)
	}
}

func TestHoldThatWaitsForTheCPUSaysSoAndWhenItEnds(t *testing.T) {
	// Arrange: the governor holds every unit back, and another unit holds
	// the only seat.
	log := &recLog{waits: make(chan struct{}, 1)}
	h := NewHost(t.TempDir(), 1, 19, log)
	if err := writeState(h.Pool.Dir, CPUState{WaitingForCPU: true, BusyPercent: 72, TestsPercent: 30, CapPercent: BaseLimits.Target, CeilingPercent: BaseLimits.Ceiling}); err != nil {
		t.Fatal(err)
	}
	var mu sync.Mutex
	var waited []string
	var ended []string
	h.WaitingForCPU = func(id string, s CPUState) {
		mu.Lock()
		waited = append(waited, fmt.Sprintf("%s %.0f/%d", id, s.BusyPercent, s.CapPercent))
		mu.Unlock()
	}
	h.CPUWaitEnded = func(id string, d time.Duration) {
		mu.Lock()
		ended = append(ended, id)
		mu.Unlock()
	}
	other, err := h.Hold("other", 1, 1)
	if err != nil {
		t.Fatal(err)
	}
	done := make(chan error, 1)
	go func() {
		release, err := h.Hold("u1", 1, 1)
		if err == nil {
			err = release()
		}
		done <- err
	}()

	// Act
	<-log.waits
	if err := other(); err != nil {
		t.Fatal(err)
	}

	// Assert
	if err := <-done; err != nil {
		t.Fatalf("Hold = %v", err)
	}
	mu.Lock()
	defer mu.Unlock()
	if len(waited) != 1 || waited[0] != "u1 72/35" || len(ended) != 1 || ended[0] != "u1" {
		t.Fatalf("told waiting %v and ended %v; want u1 once each at 72/35", waited, ended)
	}
	if !strings.Contains(log.text(), "core seats: unit u1 is waiting for CPU; the tests use 30% of the host against their 35% target and the host is 72% busy against its 50% ceiling") {
		t.Fatalf("log = %q; want the CPU wait said", log.text())
	}
}

func TestHoldThatCannotTellWhyItWaitsFailsAndGivesItsSeatsBack(t *testing.T) {
	// Arrange: a corrupt state, and another unit holding the only seat.
	log := &recLog{waits: make(chan struct{}, 1)}
	h := NewHost(t.TempDir(), 1, 19, log)
	other, err := h.Hold("other", 1, 1)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(h.Pool.Dir, StateFile), []byte("{"), 0o644); err != nil {
		t.Fatal(err)
	}
	done := make(chan error, 1)
	go func() {
		_, err := h.Hold("u1", 1, 1)
		done <- err
	}()

	// Act: once u1 says it waits, the other unit lets its seat go.
	<-log.waits
	if err := other(); err != nil {
		t.Fatal(err)
	}
	err = <-done

	// Assert
	if err == nil || !strings.Contains(err.Error(), "unit u1 waited for core seats but could not tell why") {
		t.Fatalf("Hold = %v; want the unreadable state surfaced", err)
	}
	if !free(t, h.Pool, 0) {
		t.Fatal("the failed hold kept its seat")
	}
}

// govLog records a governor's lines by level.
type govLog struct {
	mu    sync.Mutex
	infos []string
	errs  []string
}

func (l *govLog) Infof(format string, args ...any) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.infos = append(l.infos, fmt.Sprintf(format, args...))
}

func (l *govLog) Errorf(format string, args ...any) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.errs = append(l.errs, fmt.Sprintf(format, args...))
}

func (l *govLog) lines() (infos, errs []string) {
	l.mu.Lock()
	defer l.mu.Unlock()
	return append([]string(nil), l.infos...), append([]string(nil), l.errs...)
}

// host scripts the host's and the tests' CPU reads: each read moves the
// counters by one second of sixteen cores, busy on the cores set before it,
// the tests on theirs.
type host struct {
	mu       sync.Mutex
	now      hostcpu.Times
	busy     float64
	tests    float64
	testsCPU float64
	err      error
	testsErr error
	// power is the host's power; powerErr fails its read.
	power    HostPower
	powerErr error
}

// plug sets the host's power.
func (h *host) plug(power HostPower, err error) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.power, h.powerErr = power, err
}

func (h *host) readPower() (HostPower, error) {
	h.mu.Lock()
	defer h.mu.Unlock()
	return h.power, h.powerErr
}

func (h *host) set(busy, tests float64) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.busy, h.tests = busy, tests
}

func (h *host) fail(err, testsErr error) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.err, h.testsErr = err, testsErr
}

func (h *host) read() (hostcpu.Times, error) {
	h.mu.Lock()
	defer h.mu.Unlock()
	if h.err != nil {
		return hostcpu.Times{}, h.err
	}
	h.now.User += h.busy
	h.now.Idle += 16 - h.busy
	return h.now, nil
}

func (h *host) readTests() (float64, error) {
	h.mu.Lock()
	defer h.mu.Unlock()
	if h.testsErr != nil {
		return 0, h.testsErr
	}
	h.testsCPU += h.tests
	return h.testsCPU, nil
}

// clock is the governor's settable clock.
type clock struct {
	mu  sync.Mutex
	now time.Time
}

func (c *clock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *clock) advance(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.now = c.now.Add(d)
}

// govRig runs a governor over a ten-seat pool of a sixteen-core host.
type govRig struct {
	pool   seats.Pool
	host   *host
	clock  *clock
	ledger *ledger
	log    *govLog
	ticks  chan time.Time
	steps  chan Decision
	stop   context.CancelFunc
	done   chan struct{}
}

func newGovRig(t *testing.T, dir string) *govRig {
	t.Helper()
	return &govRig{
		pool:   seats.Pool{Dir: dir, Prefix: "core", N: 10, What: "core"},
		host:   &host{power: onBattery},
		clock:  &clock{now: time.Date(2026, 11, 13, 9, 0, 0, 0, time.UTC)},
		ledger: &ledger{dir: dir},
		log:    &govLog{},
		ticks:  make(chan time.Time),
		steps:  make(chan Decision),
		done:   make(chan struct{}),
	}
}

func (r *govRig) start() {
	ctx, stop := context.WithCancel(context.Background())
	r.stop = stop
	g := &Governor{Pool: r.pool, NumCPU: 16, Read: r.host.read, Tests: r.host.readTests, Power: r.host.readPower, Now: r.clock.Now, Ticks: r.ticks, Log: r.log, Stepped: func(d Decision) { r.steps <- d }}
	go func() {
		g.Run(ctx)
		close(r.done)
	}()
}

// measured starts the governor and reads the host twice at busy and tests
// cores, so its next decision is measured.
func (r *govRig) measured(busy, tests float64) Decision {
	r.host.set(busy, tests)
	r.start()
	<-r.steps
	return r.tick(busy, tests)
}

// tick reads the host once more and answers what the governor decided.
func (r *govRig) tick(busy, tests float64) Decision {
	r.host.set(busy, tests)
	r.ticks <- time.Time{}
	return <-r.steps
}

// ask posts a waiting unit on the ledger, as Host.Hold does before it asks
// for seats.
func (r *govRig) ask(t *testing.T, rec UnitRecord) string {
	t.Helper()
	path, err := r.ledger.post(rec)
	if err != nil {
		t.Fatal(err)
	}
	return path
}

// running posts a unit that holds its seats, started at started.
func (r *govRig) running(t *testing.T, rec UnitRecord, started time.Time) seats.Held {
	t.Helper()
	held, err := r.pool.TakeN(rec.Width, 0, func() { t.Fatal("a running unit's seats waited") })
	if err != nil {
		t.Fatal(err)
	}
	rec.Started = started
	if err := writeRecord(r.ask(t, rec), rec); err != nil {
		t.Fatal(err)
	}
	return held
}

// flockShared holds f shared, as a waiting taker holds its rank's lane.
func flockShared(f *os.File) error { return syscall.Flock(int(f.Fd()), syscall.LOCK_SH) }

func (r *govRig) halt() {
	r.stop()
	<-r.done
}

// turnstileFree reports whether a taker could begin to gather right now.
func turnstileFree(t *testing.T, p seats.Pool) bool {
	t.Helper()
	f, err := p.TryTurnstile()
	if err != nil {
		t.Fatal(err)
	}
	if f == nil {
		return false
	}
	f.Close()
	return true
}

func freeSeats(t *testing.T, p seats.Pool) int {
	t.Helper()
	n := 0
	for i := range p.N {
		if free(t, p, i) {
			n++
		}
	}
	return n
}

func TestGovernorHoldsEveryUnitBackUntilItHasReadTheHost(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())
	r.ask(t, UnitRecord{Unit: "u1", Rank: 19, Width: 1, Cores: 1})

	// Act: the first read has nothing to compare with.
	r.start()
	defer r.halt()
	d := <-r.steps

	// Assert
	if d.Measured || d.Admit || !d.Gated || d.Withheld != 10 {
		t.Fatalf("first decision = %+v; want unmeasured, gated, every seat withheld", d)
	}
	if turnstileFree(t, r.pool) || freeSeats(t, r.pool) != 0 {
		t.Fatal("a taker could gather before the governor read the host")
	}
	if s, err := ReadState(r.pool.Dir); err != nil || !s.WaitingForCPU {
		t.Fatalf("state = %+v, %v; want waiting for CPU", s, err)
	}
}

func TestGovernorWithNothingWaitingHoldsTheGateWithoutWaitingForCPU(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())

	// Act
	d := r.measured(0, 0)
	defer r.halt()

	// Assert
	if !d.Measured || d.Waiting || d.Admit || !d.Gated || freeSeats(t, r.pool) != 0 {
		t.Fatalf("decision = %+v; want measured, nothing waiting, gated, nothing free", d)
	}
	if s, err := ReadState(r.pool.Dir); err != nil || s.WaitingForCPU {
		t.Fatalf("state = %+v, %v; want no wait for the CPU", s, err)
	}
}

func TestGovernorLetsTheNextUnitsWidthGoWhenItFits(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())
	r.ask(t, UnitRecord{Unit: "u1", Rank: 19, Width: 2, Cores: 1.4})

	// Act
	d := r.measured(0, 0)
	defer r.halt()

	// Assert
	if !d.Admit || d.Gated || d.Next.Unit != "u1" || d.Next.Cores != 1.4 {
		t.Fatalf("decision = %+v; want u1 admitted", d)
	}
	if got := freeSeats(t, r.pool); got != 2 || !turnstileFree(t, r.pool) {
		t.Fatalf("%d seats free, turnstile free %v; want u1's two and the turnstile", got, turnstileFree(t, r.pool))
	}
}

func TestGovernorDecides(t *testing.T) {
	// Sixteen cores: the tests' target is 5.6, the host's ceiling 8. A
	// running unit started long ago holds one seat, its use in the reads.
	cases := []struct {
		name      string
		busy      float64
		tests     float64
		next      UnitRecord
		wantAdmit bool
	}{
		{name: "tests under their target but the host at its ceiling admit nothing",
			busy: 8, tests: 1, next: UnitRecord{Unit: "u", Rank: 19, Width: 1, Cores: 1}},
		{name: "an otherwise idle host with the tests at their target admits nothing",
			busy: 5.6, tests: 5.6, next: UnitRecord{Unit: "u", Rank: 19, Width: 1, Cores: 0.5}},
		{name: "a heavy unit whose history says 6 cores waits",
			busy: 2, tests: 2, next: UnitRecord{Unit: "heavy", Rank: 19, Width: 2, Cores: 6}},
		{name: "a light unit whose history says 1 core starts",
			busy: 2, tests: 2, next: UnitRecord{Unit: "light", Rank: 19, Width: 1, Cores: 1}, wantAdmit: true},
		{name: "a unit with no history, costed the default, waits where its history would have let it start",
			busy: 2, tests: 2, next: UnitRecord{Unit: "new", Rank: 19, Width: 2, Cores: ExpectedCores(2, 0, false)}},
		{name: "the rest of the host's load does not eat the tests' share",
			busy: 6, tests: 1, next: UnitRecord{Unit: "u", Rank: 19, Width: 1, Cores: 1}, wantAdmit: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r := newGovRig(t, t.TempDir())
			held := r.running(t, UnitRecord{Unit: "old", Rank: 19, Width: 1, Cores: 1}, r.clock.Now().Add(-time.Minute))
			defer held.Release()
			r.ask(t, tc.next)

			// Act
			r.measured(tc.busy, tc.tests)
			defer r.halt()
			d := r.tick(tc.busy, tc.tests)

			// Assert
			if d.Admit != tc.wantAdmit || d.Next.Unit != tc.next.Unit || d.Gated == tc.wantAdmit {
				t.Fatalf("decision = %+v; want %s admitted %v", d, tc.next.Unit, tc.wantAdmit)
			}
			if s, err := ReadState(r.pool.Dir); err != nil || s.WaitingForCPU == tc.wantAdmit {
				t.Fatalf("state = %+v, %v; want waiting for CPU %v", s, err, !tc.wantAdmit)
			}
		})
	}
}

func TestGovernorCountsTheAverageOfRecentReadsNotTheBusiest(t *testing.T) {
	// Arrange: nothing runs, so the next unit waits only while the rest of
	// the host is at its 8-core ceiling. One read 14.4 cores busy, then idle.
	r := newGovRig(t, t.TempDir())
	r.ask(t, UnitRecord{Unit: "u1", Rank: 19, Width: 1, Cores: 1})
	busy := r.measured(14.4, 0)
	defer r.halt()

	// Act
	idle := r.tick(0, 0)

	// Assert: the average of 14.4 and 0 is 7.2, under the ceiling; the
	// busiest read alone would have held the unit back.
	if busy.Admit || !idle.Admit || idle.Load.HostCores != 7.2 {
		t.Fatalf("decisions = %+v then %+v; want held at 14.4, then admitted at an average of 7.2", busy, idle)
	}
}

func TestGovernorAveragesOverTheLastAverageTicksReads(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())
	r.measured(12, 0)
	defer r.halt()

	// Act: three more reads push the 12-core read out of the window.
	r.tick(3, 0)
	r.tick(3, 0)
	d := r.tick(3, 0)

	// Assert
	if d.Load.HostCores != 3 {
		t.Fatalf("host cores = %v; want 3, the average of the last %d reads", d.Load.HostCores, AverageTicks)
	}
}

func TestGovernorCountsTheTestsOwnCPUApartFromTheHost(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())

	// Act
	d := r.measured(7, 2.5)
	defer r.halt()

	// Assert
	if d.Load.TestsCores != 2.5 || d.Load.Other() != 4.5 {
		t.Fatalf("load = %+v; want the tests at 2.5 cores and the rest of the host at 4.5", d.Load)
	}
}

func TestGovernorCountsAJustStartedUnitAtItsExpectedCoresUntilTheReadsHoldIt(t *testing.T) {
	// Arrange: a unit expected to use 3 cores just started; the reads do not
	// show it yet. The next unit is 3 cores too: 6 would pass the target.
	r := newGovRig(t, t.TempDir())
	held := r.running(t, UnitRecord{Unit: "started", Rank: 19, Width: 1, Cores: 3}, r.clock.Now())
	defer held.Release()
	r.ask(t, UnitRecord{Unit: "next", Rank: 19, Width: 1, Cores: 3})
	ramping := r.measured(0, 0)
	defer r.halt()

	// Act: once RampWindow has passed, its measured use stands for it.
	r.clock.advance(RampWindow)
	settled := r.tick(0, 0)

	// Assert
	if ramping.Admit || ramping.Load.RampCores != 3 || !settled.Admit || settled.Load.RampCores != 0 {
		t.Fatalf("decisions = %+v then %+v; want held while it ramps at 3 cores, then admitted", ramping, settled)
	}
}

func TestGovernorCostsSeatsTheLedgerDoesNotAccountFor(t *testing.T) {
	// Arrange: once the governor lets a seat go for a waiting unit, a run
	// that states nothing takes it.
	r := newGovRig(t, t.TempDir())
	r.ask(t, UnitRecord{Unit: "u1", Rank: 19, Width: 1, Cores: 1})
	r.measured(0, 0)
	defer r.halt()
	held, err := r.pool.TakeN(1, 0, func() { t.Fatal("a free seat waited") })
	if err != nil {
		t.Fatal(err)
	}
	defer held.Release()

	// Act
	d := r.tick(0, 0)

	// Assert
	if d.TestSeats != 1 || d.Load.RampCores != DefaultCoresPerSlot || !d.Load.Running {
		t.Fatalf("decision = %+v; want the unstated seat costed %v cores", d, DefaultCoresPerSlot)
	}
}

func TestGovernorCostsAWaiterThatStatesNothingAsTheWidestUnit(t *testing.T) {
	// Arrange: a run that states nothing waits on its rank's lane, while
	// the tests use 2 cores: the widest unit at the default, 4, would pass
	// the target.
	r := newGovRig(t, t.TempDir())
	held := r.running(t, UnitRecord{Unit: "old", Rank: 19, Width: 1, Cores: 1}, r.clock.Now().Add(-time.Minute))
	defer held.Release()
	if err := os.MkdirAll(r.pool.Dir, 0o755); err != nil {
		t.Fatal(err)
	}
	lane, err := os.OpenFile(filepath.Join(r.pool.Dir, "core.lane-15"), os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		t.Fatal(err)
	}
	defer lane.Close()
	if err := flockShared(lane); err != nil {
		t.Fatal(err)
	}

	// Act
	r.measured(2, 2)
	defer r.halt()
	d := r.tick(2, 2)

	// Assert
	if !d.Waiting || d.Next.Recorded || d.Next.Rank != 15 || d.Next.Width != FirstUnitSeats || d.Next.Cores != DefaultCoresPerSlot*FirstUnitSeats || d.Admit {
		t.Fatalf("decision = %+v; want the unstated rank-15 waiter costed as the widest unit and held", d)
	}
}

func TestGovernorDecidesOnTheBestRanksCostliestUnit(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())
	r.ask(t, UnitRecord{Unit: "ordinary", Rank: 19, Width: 2, Cores: 5})
	r.ask(t, UnitRecord{Unit: "head-light", Rank: 10, Width: 1, Cores: 1})
	r.ask(t, UnitRecord{Unit: "head-heavy", Rank: 10, Width: 2, Cores: 3})

	// Act
	d := r.measured(0, 0)
	defer r.halt()

	// Assert: whichever of the head's units gathers first starts, so the
	// costlier is decided on, and the widest's seats go.
	if d.Next.Unit != "head-heavy" || d.Next.Rank != 10 || d.Next.Width != 2 {
		t.Fatalf("decided on %+v; want the head's costliest unit", d.Next)
	}
}

func TestGovernorNeverStarvesTheFirstUnit(t *testing.T) {
	// Arrange: nothing runs, the rest of the host uses 7 cores, and the next
	// unit's history says 6.1, past the tests' whole target.
	r := newGovRig(t, t.TempDir())
	r.ask(t, UnitRecord{Unit: "heavy", Rank: 19, Width: 2, Cores: 6.1})

	// Act
	d := r.measured(7, 0)
	defer r.halt()

	// Assert
	if !d.Admit || d.Load.Running {
		t.Fatalf("decision = %+v; want the first unit admitted alone", d)
	}
}

func TestGovernorDecidesTheMomentAUnitWakesIt(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())
	r.measured(0, 0)
	defer r.halt()

	// Act: a unit states itself and wakes the governor; no read follows.
	r.ask(t, UnitRecord{Unit: "u1", Rank: 19, Width: 1, Cores: 1})
	if err := wakeGovernor(r.pool.Dir); err != nil {
		t.Fatal(err)
	}
	d := <-r.steps

	// Assert
	if !d.Admit || d.Next.Unit != "u1" || freeSeats(t, r.pool) != 1 {
		t.Fatalf("decision = %+v; want u1 admitted on the wake", d)
	}
}

func TestGovernorSaysEachChangeOnce(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())
	r.ask(t, UnitRecord{Unit: "u1", Rank: 19, Width: 1, Cores: 1})
	r.measured(0, 0)
	defer r.halt()

	// Act
	r.tick(0, 0)
	r.tick(14.4, 0)
	r.tick(14.4, 0)

	// Assert
	infos, errs := r.log.lines()
	var said []string
	for _, l := range infos {
		switch {
		case strings.Contains(l, "until the host's power and the host's and the tests' load are read"):
			said = append(said, "unread")
		case strings.Contains(l, "CPU available"):
			said = append(said, "available")
		case strings.Contains(l, "waiting for CPU;"):
			said = append(said, "waiting")
		}
	}
	if strings.Join(said, ",") != "unread,available,waiting" || len(errs) != 0 {
		t.Fatalf("said %v (errors %v); want unread, available, waiting once each", said, errs)
	}
	if !strings.Contains(strings.Join(infos, "\n"), "the next unit u1 costs 1.0 cores") {
		t.Fatalf("infos = %v; want the held unit and its cost said", infos)
	}
	if s, err := ReadState(r.pool.Dir); err != nil || !s.WaitingForCPU || s.BusyPercent != 60 {
		t.Fatalf("state = %+v, %v; want waiting at 60%%, the average of 0, 14.4 and 14.4 cores", s, err)
	}
}

func TestGovernorThatCannotReadHoldsEveryUnitBackAndSaysSoOnce(t *testing.T) {
	cases := []struct {
		name     string
		hostErr  error
		testsErr error
		want     string
	}{
		{name: "the host's load", hostErr: errors.New("host_statistics answered 5"),
			want: "cannot read the host's CPU load, so no test unit starts until it can: host_statistics answered 5"},
		{name: "the tests' own CPU", testsErr: errors.New("proc_info answered EIO"),
			want: "cannot read the tests' own CPU, so no test unit starts until it can: proc_info answered EIO"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r := newGovRig(t, t.TempDir())
			r.ask(t, UnitRecord{Unit: "u1", Rank: 19, Width: 1, Cores: 1})
			r.measured(0, 0)
			defer r.halt()

			// Act
			r.host.fail(tc.hostErr, tc.testsErr)
			first := r.tick(0, 0)
			second := r.tick(0, 0)

			// Assert
			if first.Measured || first.Admit || !first.Gated || second.Admit {
				t.Fatalf("decisions = %+v, %+v; want unmeasured and gated", first, second)
			}
			_, errs := r.log.lines()
			if len(errs) != 1 || !strings.Contains(errs[0], tc.want) {
				t.Fatalf("errors = %v; want the read's failure said once", errs)
			}
		})
	}
}

func TestGovernorThatCannotReadTheLedgerHoldsEveryUnitBack(t *testing.T) {
	// Arrange: a live run's record does not read.
	r := newGovRig(t, t.TempDir())
	path := r.ask(t, UnitRecord{Unit: "u1", Rank: 19, Width: 1, Cores: 1})
	if err := os.WriteFile(path, []byte("{"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	d := r.measured(0, 0)
	defer r.halt()

	// Assert
	if d.Admit || !d.Gated {
		t.Fatalf("decision = %+v; want every unit held back", d)
	}
	_, errs := r.log.lines()
	if len(errs) == 0 || !strings.Contains(errs[0], "cannot read what the units on the host cost") {
		t.Fatalf("errors = %v; want the ledger's failure said", errs)
	}
}

func TestGovernorThatCannotProbeTheSeatsHoldsEveryUnitBack(t *testing.T) {
	// Arrange: seat 9's path is a directory, so it cannot be probed.
	dir := t.TempDir()
	r := newGovRig(t, dir)
	if err := os.Mkdir(r.pool.Path(9), 0o755); err != nil {
		t.Fatal(err)
	}
	r.ask(t, UnitRecord{Unit: "u1", Rank: 19, Width: 1, Cores: 1})

	// Act
	d := r.measured(0, 0)
	defer r.halt()

	// Assert
	if d.Admit || !d.Gated {
		t.Fatalf("decision = %+v; want every unit held back", d)
	}
	_, errs := r.log.lines()
	if len(errs) == 0 || !strings.Contains(errs[0], "cannot tell which seats the tests hold") {
		t.Fatalf("errors = %v; want the probe's failure said", errs)
	}
}

func TestGovernorWaitsForAGatherUnderWayBeforeHoldingEveryUnitBack(t *testing.T) {
	// Arrange: a taker is mid-gather when the host passes the ceiling.
	r := newGovRig(t, t.TempDir())
	r.ask(t, UnitRecord{Unit: "u1", Rank: 19, Width: 1, Cores: 1})
	r.measured(0, 0)
	defer r.halt()
	gatherer, err := r.pool.Turnstile()
	if err != nil {
		t.Fatal(err)
	}

	// Act
	busy := r.tick(16, 0)
	if busy.Gated {
		t.Fatalf("decision = %+v; the governor cannot hold the turnstile a gatherer holds", busy)
	}
	if err := gatherer.Close(); err != nil {
		t.Fatal(err)
	}
	after := <-r.steps

	// Assert: once the gather ended, the governor holds the turnstile.
	if !after.Gated || turnstileFree(t, r.pool) {
		t.Fatalf("decision = %+v; want the turnstile held once the gather ended", after)
	}
}

func TestOnlyOneRunGovernsAndTheNextTakesOverWhenItExits(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	first := newGovRig(t, dir)
	first.start()
	<-first.steps
	second := newGovRig(t, dir)
	second.start()
	defer second.halt()

	// Act: the first governs a read; the second does not step until the
	// first exits.
	first.tick(0, 0)
	select {
	case d := <-second.steps:
		t.Fatalf("a second governor stepped while the first governed: %+v", d)
	default:
	}
	first.halt()
	d := <-second.steps

	// Assert
	if d.Measured || !d.Gated {
		t.Fatalf("the new governor's first decision = %+v; want it unmeasured and gated", d)
	}
}

func TestGovernorLetsEverythingGoWhenItsRunEnds(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())
	r.start()
	<-r.steps

	// Act
	r.halt()

	// Assert
	if !turnstileFree(t, r.pool) || freeSeats(t, r.pool) != 10 {
		t.Fatal("the stopped governor still holds the turnstile or seats")
	}
}

func TestGovernorStoppedBeforeItGovernsLeavesTheRoleFree(t *testing.T) {
	// Arrange: another run holds the role.
	dir := t.TempDir()
	pool := seats.Pool{Dir: dir, Prefix: "core", N: 10, What: "core"}
	role, err := pool.Exclusive("governor")
	if err != nil {
		t.Fatal(err)
	}
	r := newGovRig(t, dir)
	r.start()

	// Act: this run stops while still waiting for the role.
	r.halt()
	if err := role.Close(); err != nil {
		t.Fatal(err)
	}

	// Assert: a later run takes the role.
	next := newGovRig(t, dir)
	next.start()
	defer next.halt()
	if d := <-next.steps; !d.Gated {
		t.Fatalf("the next governor's first decision = %+v", d)
	}
}

func TestAwaitLockHandsOverTheLockOrItsFailure(t *testing.T) {
	cases := []struct {
		name    string
		err     error
		wantErr bool
	}{
		{name: "a held lock", err: nil},
		{name: "a lock that failed", err: errors.New("flock: bad file"), wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f, err := os.CreateTemp(t.TempDir(), "lock")
			if err != nil {
				t.Fatal(err)
			}

			// Act
			r := <-awaitLock(func() (*os.File, error) {
				if tc.err != nil {
					return nil, tc.err
				}
				return f, nil
			})

			// Assert
			if (r.err != nil) != tc.wantErr || (r.f != nil) == tc.wantErr {
				t.Fatalf("awaitLock = %+v; want error %v", r, tc.wantErr)
			}
			f.Close()
		})
	}
}

func TestDropOnArrivalLetsTheLockGo(t *testing.T) {
	// Arrange: the role is held elsewhere, so the wait for it is abandoned.
	p := seats.Pool{Dir: t.TempDir(), Prefix: "core", N: 1, What: "core"}
	holder, err := p.Exclusive("governor")
	if err != nil {
		t.Fatal(err)
	}
	abandoned := awaitLock(func() (*os.File, error) { return p.Exclusive("governor") })
	dropOnArrival(abandoned)

	// Act
	if err := holder.Close(); err != nil {
		t.Fatal(err)
	}

	// Assert: the abandoned wait got the role and let it go, so it is free.
	next, err := p.Exclusive("governor")
	if err != nil {
		t.Fatal(err)
	}
	next.Close()
}

var (
	// onBattery is a host on its battery in Low Power Mode, under the base
	// limits.
	onBattery = HostPower{Source: hostpower.PowerBattery, EnergyMode: hostpower.EnergyModeLowPower}
	// pluggedIn is a host on AC power in High Power Mode, under the powered
	// limits: a closed lid under persistent Wi-Fi keeps High Power Mode on AC
	// power, so it reads the same.
	pluggedIn = HostPower{Source: hostpower.PowerAC, EnergyMode: hostpower.EnergyModeHighPower}
)

func TestGovernorHoldsTheTestsToTheLimitsTheHostsPowerAllows(t *testing.T) {
	// Sixteen cores, the tests using 6 of them: over the base target of 5.6,
	// under the powered one of 8.
	cases := []struct {
		name        string
		power       HostPower
		wantAdmit   bool
		wantLimits  Limits
		wantPercent [2]int
	}{
		{name: "on the battery the base target holds the next unit back", power: onBattery, wantLimits: BaseLimits, wantPercent: [2]int{35, 50}},
		{name: "on AC power in High Power Mode the powered target lets it start", power: pluggedIn, wantAdmit: true, wantLimits: PoweredLimits, wantPercent: [2]int{50, 55}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r := newGovRig(t, t.TempDir())
			r.host.plug(tc.power, nil)
			held := r.running(t, UnitRecord{Unit: "old", Rank: 19, Width: 1, Cores: 6}, r.clock.Now().Add(-time.Minute))
			defer held.Release()
			r.ask(t, UnitRecord{Unit: "u", Rank: 19, Width: 1, Cores: 1})

			// Act
			d := r.measured(6, 6)
			defer r.halt()

			// Assert
			if d.Admit != tc.wantAdmit || d.Limits != tc.wantLimits {
				t.Fatalf("decision = %+v; want admitted %v under %+v", d, tc.wantAdmit, tc.wantLimits)
			}
			if s, err := ReadState(r.pool.Dir); err != nil || [2]int{s.CapPercent, s.CeilingPercent} != tc.wantPercent {
				t.Fatalf("state = %+v, %v; want the limits %v stated", s, err, tc.wantPercent)
			}
		})
	}
}

func TestGovernorReturnsToTheBaseLimitsWhenTheHostIsUnplugged(t *testing.T) {
	// Arrange: plugged in, the next unit fits the powered target.
	r := newGovRig(t, t.TempDir())
	r.host.plug(pluggedIn, nil)
	held := r.running(t, UnitRecord{Unit: "old", Rank: 19, Width: 1, Cores: 6}, r.clock.Now().Add(-time.Minute))
	defer held.Release()
	r.ask(t, UnitRecord{Unit: "u", Rank: 19, Width: 1, Cores: 1})
	before := r.measured(6, 6)
	defer r.halt()

	// Act
	r.host.plug(onBattery, nil)
	after := r.tick(6, 6)

	// Assert
	if !before.Admit || after.Admit || after.Limits != BaseLimits {
		t.Fatalf("decisions = %+v, then %+v; want admitted plugged in, held back under the base limits once unplugged", before, after)
	}
}

func TestGovernorSaysEachChangeOfItsLimitsOnce(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())
	r.host.plug(pluggedIn, nil)
	r.measured(0, 0)
	defer r.halt()

	// Act: the same limits again, then unplugged twice over.
	r.tick(0, 0)
	r.host.plug(onBattery, nil)
	r.tick(0, 0)
	r.host.plug(HostPower{Source: hostpower.PowerBattery, EnergyMode: hostpower.EnergyModeHighPower}, nil)
	r.tick(0, 0)

	// Assert
	infos, _ := r.log.lines()
	var said []string
	for _, l := range infos {
		if strings.Contains(l, "CPU governor: the host runs on") {
			said = append(said, l)
		}
	}
	want := []string{
		"CPU governor: the host runs on ac power in High Power Mode, so the tests average at most 50% (8.0 cores) and the host reaches at most 55% (8.8 cores)",
		"CPU governor: the host runs on battery power in Low Power Mode, so the tests average at most 35% (5.6 cores) and the host reaches at most 50% (8.0 cores)",
	}
	if strings.Join(said, "\n") != strings.Join(want, "\n") {
		t.Fatalf("said %q; want %q", said, want)
	}
}

func TestGovernorThatCannotReadThePowerHoldsEveryUnitBackAndSaysSoOnce(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())
	r.ask(t, UnitRecord{Unit: "u1", Rank: 19, Width: 1, Cores: 1})
	r.measured(0, 0)
	defer r.halt()

	// Act
	r.host.plug(HostPower{}, errors.New("pmset -g did not answer within 150ms"))
	first := r.tick(0, 0)
	second := r.tick(0, 0)

	// Assert
	if first.Measured || first.Admit || !first.Gated || second.Admit {
		t.Fatalf("decisions = %+v, %+v; want unmeasured and gated", first, second)
	}
	_, errs := r.log.lines()
	want := "cannot read the host's power, so no test unit starts until it can: pmset -g did not answer within 150ms"
	if len(errs) != 1 || !strings.Contains(errs[0], want) {
		t.Fatalf("errors = %v; want the power read's failure said once", errs)
	}
}

func TestLimitsSaidNamesBothLimits(t *testing.T) {
	// Act
	got := LimitsSaid()

	// Assert
	want := "the tests to an average of 50% of the host and the host to 55% on AC power in High Power Mode, and to 35% and 50% otherwise"
	if got != want {
		t.Fatalf("LimitsSaid() = %q; want %q", got, want)
	}
}

func TestTheGovernorSaysItsLimitsWithLimitsSaid(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())

	// Act
	r.measured(0, 0)
	r.halt()

	// Assert
	infos, _ := r.log.lines()
	if len(infos) == 0 || !strings.HasSuffix(infos[0], "holding "+LimitsSaid()) {
		t.Fatalf("infos = %v; want the governor's first line to say LimitsSaid()", infos)
	}
}
