package cores

import (
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	"agentrepl/testrun/internal/seats"
)

type recLog struct {
	mu    sync.Mutex
	lines []string
	// waits, when set, receives once per line that says a unit waits, for
	// seats or for the CPU.
	waits chan struct{}
}

func (l *recLog) Infof(format string, args ...any) {
	line := fmt.Sprintf(format, args...)
	l.mu.Lock()
	l.lines = append(l.lines, line)
	l.mu.Unlock()
	if l.waits != nil && strings.Contains(line, " wait") {
		l.waits <- struct{}{}
	}
}

func (l *recLog) Errorf(format string, args ...any) {
	l.Infof("ERROR "+format, args...)
}

func (l *recLog) text() string {
	l.mu.Lock()
	defer l.mu.Unlock()
	return strings.Join(l.lines, "\n")
}

// free reports whether seat i of p can be taken right now.
func free(t *testing.T, p seats.Pool, i int) bool {
	t.Helper()
	held, err := p.Held(i)
	if err != nil {
		t.Fatal(err)
	}
	return !held
}

func TestBudget(t *testing.T) {
	cases := []struct {
		name string
		cpus int
		want int
	}{
		{name: "sixteen cores budget eight, for the powered target", cpus: 16, want: 8},
		{name: "ten cores budget five", cpus: 10, want: 5},
		{name: "two cores budget one", cpus: 2, want: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := Budget(tc.cpus)

			// Assert
			if err != nil || got != tc.want {
				t.Fatalf("Budget(%d) = %d, %v; want %d", tc.cpus, got, err, tc.want)
			}
		})
	}
}

func TestBudgetRefusesAHostWithoutOneWholeCore(t *testing.T) {
	// Act
	got, err := Budget(1)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "not one whole core") {
		t.Fatalf("Budget(1) = %d, %v; want it refused", got, err)
	}
}

func TestCheck(t *testing.T) {
	cases := []struct {
		name    string
		env     map[string]string
		wantErr bool
	}{
		{name: "a top-level run passes", env: map[string]string{}},
		{name: "a run inside a unit is refused", env: map[string]string{NestedEnv: "1"}, wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := Check(func(k string) string { return tc.env[k] })

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("Check = %v, want error %v", err, tc.wantErr)
			}
		})
	}
}

func TestHoldTakesTheUnitsWidthAndGivesItBack(t *testing.T) {
	// Arrange
	log := &recLog{}
	h := NewHost(t.TempDir(), 3, 19, log)

	// Act
	release, err := h.Hold("u1", 2, 1)

	// Assert
	if err != nil {
		t.Fatalf("Hold = %v", err)
	}
	if free(t, h.Pool, 0) || free(t, h.Pool, 1) || !free(t, h.Pool, 2) {
		t.Fatal("want seats 0 and 1 held for the two-wide unit and seat 2 free")
	}
	if err := release(); err != nil {
		t.Fatal(err)
	}
	if !free(t, h.Pool, 0) || !free(t, h.Pool, 1) {
		t.Fatal("the unit's seats are still held after release")
	}
	if log.text() != "" {
		t.Fatalf("an unwaited hold logged %q, want nothing", log.text())
	}
}

func TestHoldStatesTheUnitOnTheLedgerUntilItGivesItsSeatsBack(t *testing.T) {
	// Arrange
	h := NewHost(t.TempDir(), 3, 12, &recLog{})
	before := time.Now()

	// Act
	release, err := h.Hold("u1", 2, 1.4)
	if err != nil {
		t.Fatal(err)
	}
	held, err := readLedger(h.Pool.Dir)
	if err != nil {
		t.Fatal(err)
	}
	if err := release(); err != nil {
		t.Fatal(err)
	}
	after, err := readLedger(h.Pool.Dir)

	// Assert
	if len(held) != 1 || held[0].Unit != "u1" || held[0].Rank != 12 || held[0].Width != 2 || held[0].Cores != 1.4 || held[0].Started.Before(before) {
		t.Fatalf("ledger while held = %+v; want u1 at rank 12, two wide, 1.4 cores, started", held)
	}
	if err != nil || len(after) != 0 {
		t.Fatalf("ledger after release = %+v, %v; want it empty", after, err)
	}
}

func TestHoldStatesTheUnitWhileItWaits(t *testing.T) {
	// Arrange: another unit holds the only seat.
	log := &recLog{waits: make(chan struct{}, 1)}
	h := NewHost(t.TempDir(), 1, 19, log)
	other, err := h.Hold("other", 1, 1)
	if err != nil {
		t.Fatal(err)
	}
	done := make(chan error, 1)
	go func() {
		release, err := h.Hold("u1", 1, 3)
		if err == nil {
			err = release()
		}
		done <- err
	}()
	<-log.waits

	// Act
	recs, err := readLedger(h.Pool.Dir)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	waiting := 0
	for _, r := range recs {
		if r.Unit == "u1" && r.Started.IsZero() && r.Cores == 3 {
			waiting++
		}
	}
	if waiting != 1 {
		t.Fatalf("ledger = %+v; want u1 waiting at 3 cores", recs)
	}
	if err := other(); err != nil {
		t.Fatal(err)
	}
	if err := <-done; err != nil {
		t.Fatal(err)
	}
}

func TestHoldThatFailsTakesTheUnitOffTheLedger(t *testing.T) {
	// Arrange: a unit wider than the pool.
	h := NewHost(t.TempDir(), 1, 19, &recLog{})

	// Act
	_, err := h.Hold("wide", 2, 4)

	// Assert
	recs, lerr := readLedger(h.Pool.Dir)
	if err == nil || lerr != nil || len(recs) != 0 {
		t.Fatalf("Hold = %v; ledger %+v, %v; want the hold refused and nothing left on the ledger", err, recs, lerr)
	}
}

func TestHoldWakesTheGovernorAtEachChange(t *testing.T) {
	// Arrange: the governor's wake FIFO, read without blocking, so each
	// read answers every wake written before it.
	h := NewHost(t.TempDir(), 1, 19, &recLog{})
	path := filepath.Join(h.Pool.Dir, WakeFile)
	if err := syscall.Mkfifo(path, 0o644); err != nil {
		t.Fatal(err)
	}
	fd, err := syscall.Open(path, syscall.O_RDWR|syscall.O_NONBLOCK, 0)
	if err != nil {
		t.Fatal(err)
	}
	defer syscall.Close(fd)
	wakes := func() int {
		buf := make([]byte, 16)
		n, err := syscall.Read(fd, buf)
		if errors.Is(err, syscall.EAGAIN) {
			return 0
		}
		if err != nil {
			t.Fatal(err)
		}
		return n
	}

	// Act
	release, err := h.Hold("u1", 1, 1)
	if err != nil {
		t.Fatal(err)
	}
	asked := wakes()
	if err := release(); err != nil {
		t.Fatal(err)
	}
	ended := wakes()

	// Assert: once on asking, once on starting, once on letting go.
	if asked != 2 || ended != 1 {
		t.Fatalf("wakes = %d while held, %d on release; want 2 and 1", asked, ended)
	}
}

func TestHoldWaitsForAnotherRunsSeatAndSaysSo(t *testing.T) {
	// Arrange: another run's unit holds the host's only seat.
	log := &recLog{waits: make(chan struct{}, 1)}
	h := NewHost(t.TempDir(), 1, 19, log)
	other, err := h.Hold("other", 1, 1)
	if err != nil {
		t.Fatal(err)
	}
	type held struct {
		release func() error
		err     error
	}
	done := make(chan held, 1)
	go func() {
		r, err := h.Hold("u1", 1, 1)
		done <- held{r, err}
	}()

	// Act: once u1 says it waits, the other run's unit exits.
	<-log.waits
	if err := other(); err != nil {
		t.Fatal(err)
	}
	got := <-done

	// Assert
	if got.err != nil {
		t.Fatalf("Hold = %v", got.err)
	}
	if err := got.release(); err != nil {
		t.Fatal(err)
	}
	want := []string{
		"core seats: unit u1 waits for 1 of the host's 1, for the CPU governor to admit it, for seats test runs hold, or behind a run that outranks this one (rank 19)",
		"core seats: unit u1 took 1 after",
	}
	for _, w := range want {
		if !strings.Contains(log.text(), w) {
			t.Fatalf("log = %q, want it to contain %q", log.text(), w)
		}
	}
}

func TestHoldRefusesAUnitWiderThanTheHostsBudget(t *testing.T) {
	// Arrange
	h := NewHost(t.TempDir(), 2, 19, &recLog{})

	// Act
	release, err := h.Hold("wide", 3, 1)

	// Assert
	if err == nil || release != nil || !strings.Contains(err.Error(), "hold 3 core seat(s) for unit wide") {
		t.Fatalf("Hold = %v; want the too-wide unit refused", err)
	}
}

func TestRank(t *testing.T) {
	cases := []struct {
		name    string
		marker  string
		want    int
		wantErr bool
	}{
		{name: "the merge queue's head", marker: "nice-10", want: 10},
		{name: "an ordinary run", marker: "nice-19", want: 19},
		{name: "no marker: not started through background.sh", marker: "", wantErr: true},
		{name: "a marker that names no niceness", marker: "1", wantErr: true},
		{name: "a niceness that is not a number", marker: "nice-x", wantErr: true},
		{name: "a negative niceness", marker: "nice--1", wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := Rank(func(k string) string {
				if k == EnvBackground {
					return tc.marker
				}
				return ""
			})

			// Assert
			if (err != nil) != tc.wantErr || got != tc.want {
				t.Fatalf("Rank(%q) = %d, %v; want %d, error %v", tc.marker, got, err, tc.want, tc.wantErr)
			}
		})
	}
}

// The rank's effect on the order of takers is seats.Pool.TakeN's (its lane
// tests); what Host owns is passing the run's rank through to it.
func TestHoldTakesSeatsAtTheRunsRank(t *testing.T) {
	// Arrange: a rank the pool refuses, so only the rank reaching TakeN fails.
	h := NewHost(t.TempDir(), 1, -1, &recLog{})

	// Act
	release, err := h.Hold("u1", 1, 1)

	// Assert
	if err == nil || release != nil || !strings.Contains(err.Error(), "at rank -1") {
		t.Fatalf("Hold = %v; want the pool to see the run's rank", err)
	}
}

func TestPlanForGivesEachRunItsBudgetPoolAndGovernor(t *testing.T) {
	tests := []struct {
		name       string
		ungoverned string
		numCPU     int
		want       Plan
		wantErr    string
	}{
		{name: "an ordinary run shares the host's governed pool", numCPU: 16,
			want: Plan{Budget: 16 * PoweredLimits.Target / 100, Dir: "/shared", Governed: true}},
		{name: "an ungoverned run holds the whole host from its own pool, ungoverned, a seat per UngovernedSeatCores cores", ungoverned: "1", numCPU: 16,
			want: Plan{Budget: 8, Dir: "/own"}},
		{name: "an ungoverned run on a host of one core is refused", ungoverned: "1", numCPU: 1,
			wantErr: "which is not one seat"},
		{name: "a marker that is not 1 is refused", ungoverned: "yes", numCPU: 16,
			wantErr: `AGENT_REPL_TEST_UNGOVERNED="yes" is not the "1"`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			getenv := func(name string) string {
				if name == UngovernedEnv {
					return tt.ungoverned
				}
				return ""
			}

			// Act
			got, err := PlanFor(getenv, tt.numCPU, "/shared", "/own")

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("PlanFor err = %v, want one containing %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("PlanFor = %+v, %v; want %+v", got, err, tt.want)
			}
		})
	}
}
