package cores

import (
	"fmt"
	"strings"
	"sync"
	"testing"

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
		{name: "sixteen cores budget five", cpus: 16, want: 5},
		{name: "ten cores budget three", cpus: 10, want: 3},
		{name: "three cores budget one", cpus: 3, want: 1},
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
	release, err := h.Hold("u1", 2)

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

func TestHoldWaitsForAnotherRunsSeatAndSaysSo(t *testing.T) {
	// Arrange: another run's unit holds the host's only seat.
	log := &recLog{waits: make(chan struct{}, 1)}
	h := NewHost(t.TempDir(), 1, 19, log)
	other, err := h.Hold("other", 1)
	if err != nil {
		t.Fatal(err)
	}
	type held struct {
		release func() error
		err     error
	}
	done := make(chan held, 1)
	go func() {
		r, err := h.Hold("u1", 1)
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
		"core seats: unit u1 waits for 1 of the host's 1, held by test runs or promised to a run that outranks this one (rank 19)",
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
	release, err := h.Hold("wide", 3)

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
	release, err := h.Hold("u1", 1)

	// Assert
	if err == nil || release != nil || !strings.Contains(err.Error(), "at rank -1") {
		t.Fatalf("Hold = %v; want the pool to see the run's rank", err)
	}
}
