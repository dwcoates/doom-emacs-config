package cores

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"agentrepl/logging/hostcpu"
	"agentrepl/testrun/internal/seats"
)

func TestAllowance(t *testing.T) {
	// Sixteen cores: the cap is 10.4 cores, 9.4 once the margin is kept.
	cases := []struct {
		name      string
		busyCores float64
		rampSeats int
		want      int
	}{
		{name: "an idle host hands out what the cap leaves, two cores a seat", busyCores: 0, want: 4},
		{name: "a half-busy host leaves the tests a seat", busyCores: 7, want: 1},
		{name: "a host at the cap hands out nothing", busyCores: 10.4, want: 0},
		{name: "a host past the cap hands out nothing", busyCores: 14, want: 0},
		{name: "headroom short of a whole seat hands out nothing", busyCores: 8, want: 0},
		{name: "seats taken too recently to show count against the headroom", busyCores: 1, rampSeats: 2, want: 2},
		{name: "recent seats can use up the whole headroom", busyCores: 1, rampSeats: 5, want: 0},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Allowance(16, tc.busyCores, tc.rampSeats)

			// Assert
			if got != tc.want {
				t.Fatalf("Allowance(16, %v, %d) = %d; want %d", tc.busyCores, tc.rampSeats, got, tc.want)
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
		{name: "no governor has stated anything: nothing waits for the CPU", want: CPUState{CapPercent: CapPercent}},
		{name: "a stated wait", write: true, file: `{"waiting_for_cpu":true,"busy_percent":71.5,"cap_percent":65}`, want: CPUState{WaitingForCPU: true, BusyPercent: 71.5, CapPercent: 65}},
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
	want := CPUState{WaitingForCPU: true, BusyPercent: 80, CapPercent: CapPercent}

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
	if err := writeState(h.Pool.Dir, CPUState{WaitingForCPU: true, BusyPercent: 72, CapPercent: CapPercent}); err != nil {
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
	other, err := h.Hold("other", 1)
	if err != nil {
		t.Fatal(err)
	}
	done := make(chan error, 1)
	go func() {
		release, err := h.Hold("u1", 1)
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
	if len(waited) != 1 || waited[0] != "u1 72/65" || len(ended) != 1 || ended[0] != "u1" {
		t.Fatalf("told waiting %v and ended %v; want u1 once each at 72/65", waited, ended)
	}
	if !strings.Contains(log.text(), "core seats: unit u1 is waiting for CPU; the host is 72% busy against the 65% cap") {
		t.Fatalf("log = %q; want the CPU wait said", log.text())
	}
}

func TestHoldThatCannotTellWhyItWaitsFailsAndGivesItsSeatsBack(t *testing.T) {
	// Arrange: a corrupt state, and another unit holding the only seat.
	log := &recLog{waits: make(chan struct{}, 1)}
	h := NewHost(t.TempDir(), 1, 19, log)
	other, err := h.Hold("other", 1)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(h.Pool.Dir, StateFile), []byte("{"), 0o644); err != nil {
		t.Fatal(err)
	}
	done := make(chan error, 1)
	go func() {
		_, err := h.Hold("u1", 1)
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

// host scripts the host's CPU reads: each read moves the counters by one
// second of every core, busy for the share set before it.
type host struct {
	mu    sync.Mutex
	now   hostcpu.Times
	share float64
	err   error
}

func (h *host) set(share float64, err error) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.share, h.err = share, err
}

func (h *host) read() (hostcpu.Times, error) {
	h.mu.Lock()
	defer h.mu.Unlock()
	if h.err != nil {
		return hostcpu.Times{}, h.err
	}
	h.now.User += 16 * h.share
	h.now.Idle += 16 * (1 - h.share)
	return h.now, nil
}

// govRig runs a governor over a ten-seat pool of a sixteen-core host.
type govRig struct {
	pool  seats.Pool
	host  *host
	log   *govLog
	ticks chan time.Time
	steps chan Decision
	stop  context.CancelFunc
	done  chan struct{}
}

func newGovRig(t *testing.T, dir string) *govRig {
	t.Helper()
	r := &govRig{
		pool:  seats.Pool{Dir: dir, Prefix: "core", N: 10, What: "core"},
		host:  &host{},
		log:   &govLog{},
		ticks: make(chan time.Time),
		steps: make(chan Decision),
		done:  make(chan struct{}),
	}
	return r
}

func (r *govRig) start() {
	ctx, stop := context.WithCancel(context.Background())
	r.stop = stop
	g := &Governor{Pool: r.pool, NumCPU: 16, Read: r.host.read, Ticks: r.ticks, Log: r.log, Stepped: func(d Decision) { r.steps <- d }}
	go func() {
		g.Run(ctx)
		close(r.done)
	}()
}

// tick reads the host once more and answers what the governor decided.
func (r *govRig) tick(share float64) Decision {
	r.host.set(share, nil)
	r.ticks <- time.Time{}
	return <-r.steps
}

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

	// Act: the first read has nothing to compare with.
	r.start()
	defer r.halt()
	d := <-r.steps

	// Assert
	if d.Measured || d.Allow != 0 || !d.Gated || d.Withheld != 10 {
		t.Fatalf("first decision = %+v; want unmeasured, gated, every seat withheld", d)
	}
	if turnstileFree(t, r.pool) || freeSeats(t, r.pool) != 0 {
		t.Fatal("a taker could gather before the governor read the host")
	}
	if s, err := ReadState(r.pool.Dir); err != nil || !s.WaitingForCPU {
		t.Fatalf("state = %+v, %v; want waiting for CPU", s, err)
	}
}

func TestGovernorHandsOutWhatTheCapLeaves(t *testing.T) {
	cases := []struct {
		name      string
		share     float64
		wantAllow int
		wantFree  int
		wantGated bool
	}{
		{name: "an idle host", share: 0, wantAllow: 4, wantFree: 4},
		{name: "a host already half busy", share: 0.45, wantAllow: 1, wantFree: 1},
		{name: "a host at the cap", share: 0.65, wantAllow: 0, wantFree: 0, wantGated: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r := newGovRig(t, t.TempDir())
			r.host.set(tc.share, nil)
			r.start()
			defer r.halt()
			<-r.steps

			// Act
			d := r.tick(tc.share)

			// Assert
			if !d.Measured || d.Allow != tc.wantAllow || d.Gated != tc.wantGated {
				t.Fatalf("decision = %+v; want allow %d, gated %v", d, tc.wantAllow, tc.wantGated)
			}
			if got := freeSeats(t, r.pool); got != tc.wantFree {
				t.Fatalf("%d seats free; want %d", got, tc.wantFree)
			}
			if turnstileFree(t, r.pool) == tc.wantGated {
				t.Fatalf("turnstile free = %v; want %v", !tc.wantGated, !tc.wantGated)
			}
		})
	}
}

func TestGovernorCountsTheBusiestRecentRead(t *testing.T) {
	// Arrange: one busy read, then idle ones.
	r := newGovRig(t, t.TempDir())
	r.start()
	defer r.halt()
	<-r.steps
	r.tick(0.65)

	// Act
	first, second, third := r.tick(0), r.tick(0), r.tick(0)

	// Assert: the busy read holds units back for PeakTicks reads.
	if first.Allow != 0 || second.Allow != 0 || third.Allow != 4 {
		t.Fatalf("allowances = %d, %d, %d; want 0, 0, 4", first.Allow, second.Allow, third.Allow)
	}
}

func TestGovernorCostsSeatsTheTestsJustTook(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())
	r.start()
	defer r.halt()
	<-r.steps
	r.tick(0)

	// Act: the tests take two of the four seats handed out; the host's reads
	// do not show them yet.
	held, err := r.pool.TakeN(2, 0, func() { t.Fatal("a handed-out seat waited") })
	if err != nil {
		t.Fatal(err)
	}
	defer held.Release()
	d := r.tick(0)

	// Assert: 9.4 cores of headroom less 4 for the two new seats leaves 2.
	if d.TestSeats != 2 || d.RampSeats != 2 || d.Allow != 2 {
		t.Fatalf("decision = %+v; want 2 test seats, 2 of them recent, 2 more allowed", d)
	}
}

func TestGovernorSaysEachChangeOnce(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())
	r.start()
	defer r.halt()
	<-r.steps

	// Act
	r.tick(0)
	r.tick(0)
	r.tick(0.9)
	r.tick(0.9)

	// Assert
	infos, errs := r.log.lines()
	var said []string
	for _, l := range infos {
		switch {
		case strings.Contains(l, "until the host's load is read"):
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
	if !strings.Contains(strings.Join(infos, "\n"), "the host is 90% busy") {
		t.Fatalf("infos = %v; want the busyness said", infos)
	}
	if s, err := ReadState(r.pool.Dir); err != nil || !s.WaitingForCPU || s.BusyPercent != 90 {
		t.Fatalf("state = %+v, %v; want waiting at 90%%", s, err)
	}
}

func TestGovernorThatCannotReadTheHostHoldsEveryUnitBackAndSaysSoOnce(t *testing.T) {
	// Arrange
	r := newGovRig(t, t.TempDir())
	r.start()
	defer r.halt()
	<-r.steps
	r.tick(0)

	// Act
	r.host.set(0, errors.New("host_statistics answered 5"))
	r.ticks <- time.Time{}
	first := <-r.steps
	r.ticks <- time.Time{}
	second := <-r.steps

	// Assert
	if first.Measured || first.Allow != 0 || !first.Gated || second.Allow != 0 {
		t.Fatalf("decisions = %+v, %+v; want unmeasured and gated", first, second)
	}
	_, errs := r.log.lines()
	if len(errs) != 1 || !strings.Contains(errs[0], "cannot read the host's CPU load, so no test unit starts until it can: host_statistics answered 5") {
		t.Fatalf("errors = %v; want the read's failure said once", errs)
	}
}

func TestGovernorThatCannotProbeTheSeatsHoldsEveryUnitBack(t *testing.T) {
	// Arrange: seat 9's path is a directory, so it cannot be probed.
	dir := t.TempDir()
	r := newGovRig(t, dir)
	if err := os.Mkdir(r.pool.Path(9), 0o755); err != nil {
		t.Fatal(err)
	}
	r.start()
	defer r.halt()
	<-r.steps

	// Act
	d := r.tick(0)

	// Assert
	if d.Allow != 0 || !d.Gated {
		t.Fatalf("decision = %+v; want every unit held back", d)
	}
	_, errs := r.log.lines()
	if len(errs) == 0 || !strings.Contains(errs[0], "cannot tell which seats the tests hold") {
		t.Fatalf("errors = %v; want the probe's failure said", errs)
	}
}

func TestGovernorWaitsForAGatherUnderWayBeforeHoldingEveryUnitBack(t *testing.T) {
	// Arrange: a taker is mid-gather when the host passes the cap.
	r := newGovRig(t, t.TempDir())
	r.start()
	defer r.halt()
	<-r.steps
	r.tick(0)
	gatherer, err := r.pool.Turnstile()
	if err != nil {
		t.Fatal(err)
	}

	// Act
	busy := r.tick(0.9)
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
	first.tick(0)
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
