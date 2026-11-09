package ramdisk

import (
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"
	"time"

	"agentrepl/testrun/internal/seats"
	"agentrepl/testrun/seatwait"
)

// free reports whether seat i of p can be taken right now.
func free(t *testing.T, p seats.Pool, i int) bool {
	t.Helper()
	held, err := p.Held(i)
	if err != nil {
		t.Fatal(err)
	}
	return !held
}

const gib = uint64(1) << 30

func TestCeilingAndMemorySeats(t *testing.T) {
	cases := []struct {
		name        string
		total       uint64
		sizeMiB     int
		wantCeiling uint64
		wantSeats   int
	}{
		{name: "the 48 GiB host the ruling was made on seats five 4 GiB disks under 20 GiB", total: 48 * gib, sizeMiB: 4096, wantCeiling: 20480, wantSeats: 5},
		{name: "a 64 GiB host's ceiling grows with it", total: 64 * gib, sizeMiB: 4096, wantCeiling: 27306, wantSeats: 6},
		{name: "a ceiling that does not divide evenly rounds down", total: 16 * gib, sizeMiB: 4096, wantCeiling: 6826, wantSeats: 1},
		{name: "a host whose ceiling is under one disk seats none", total: 8 * gib, sizeMiB: 4096, wantCeiling: 3413, wantSeats: 0},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			ceiling, n := CeilingMiB(tc.total), MemorySeats(tc.total, tc.sizeMiB)

			// Assert
			if ceiling != tc.wantCeiling || n != tc.wantSeats {
				t.Fatalf("CeilingMiB, MemorySeats = %d, %d; want %d, %d", ceiling, n, tc.wantCeiling, tc.wantSeats)
			}
		})
	}
}

func TestAdmitsKeepsTheFloorAfterEverySeatedDiskFills(t *testing.T) {
	cases := []struct {
		name      string
		available uint64
		wired     uint64
		want      bool
	}{
		{name: "30 GiB free seats the first disk", available: 30720, wired: 0, want: true},
		{name: "30 GiB free seats a fifth disk to exactly the 10 GiB floor", available: 30720, wired: 16384, want: true},
		{name: "one MiB short of the floor is refused", available: 30719, wired: 16384, want: false},
		{name: "a host already under the floor is refused with nothing seated", available: 10239, wired: 0, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Admits(tc.available, tc.wired, 4096)

			// Assert
			if got != tc.want {
				t.Fatalf("Admits(%d, %d, 4096) = %v, want %v", tc.available, tc.wired, got, tc.want)
			}
		})
	}
}

func TestParseMemory(t *testing.T) {
	cases := []struct {
		name    string
		out     string
		want    Memory
		wantErr string
	}{
		{name: "sysctl's bytes and level", out: "51539607552\n55\n", want: Memory{TotalBytes: 51539607552, AvailableBytes: 51539607552 / 100 * 55}},
		{name: "one answer only", out: "51539607552\n", wantErr: "not hw.memsize and kern.memorystatus_level"},
		{name: "a total that is not a number", out: "oid?\n55\n", wantErr: "hw.memsize answered"},
		{name: "a total of zero", out: "0\n55\n", wantErr: "hw.memsize answered"},
		{name: "a level over 100", out: "51539607552\n101\n", wantErr: "not a percentage"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := parseMemory(tc.out)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("parseMemory(%q) = %+v, %v; want an error with %q", tc.out, got, err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("parseMemory(%q) = %+v, %v; want %+v", tc.out, got, err, tc.want)
			}
		})
	}
}

// The memory seats live in the registry's seats directory under the "ram"
// prefix, apart from the single "seat" seat the runners before 2026-11-09
// took, whose holders name nobody and keep no watch.
func TestTheMemorySeatsLiveInTheRegistrysSeatsDirectory(t *testing.T) {
	// Act
	got := seatPool("/reg", 4).Path(3)

	// Assert
	if got != "/reg/seats/ram-3.lock" {
		t.Fatalf("seatPool(/reg, 4).Path(3) = %q, want /reg/seats/ram-3.lock", got)
	}
}

func TestAReleasedRAMDiskGivesItsSeatBack(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	if free(t, seatPool(m.Registry, 1), 0) {
		t.Fatal("seat 0 reads free while the run's disk holds it")
	}

	// Act
	err = v.Release()

	// Assert
	if err != nil || !free(t, seatPool(m.Registry, 1), 0) {
		t.Fatalf("Release = %v, seat free %v; want the seat given back", err, free(t, seatPool(m.Registry, 1), 0))
	}
}

func TestAcquireRefusesAHostThatCannotSeatOneDisk(t *testing.T) {
	cases := []struct {
		name string
		mem  func() (Memory, error)
		want string
	}{
		{name: "the memory read fails", mem: func() (Memory, error) { return Memory{}, errors.New("sysctl is gone") }, want: "size the RAM disk seats: sysctl is gone"},
		{name: "the ceiling is under one disk", mem: func() (Memory, error) { return Memory{TotalBytes: 8 * gib, AvailableBytes: 8 * gib}, nil }, want: "less than one 4096 MiB disk"},
		{name: "the floor can never be kept", mem: func() (Memory, error) { return Memory{TotalBytes: 12 * gib, AvailableBytes: 12 * gib}, nil }, want: "can never seat a 4096 MiB RAM disk and keep 10240 MiB available"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := &fakeHost{}
			m := newManager(t, h)
			m.Memory = tc.mem

			// Act
			v, err := m.Acquire(4096)

			// Assert
			if err == nil || v != nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Acquire = %v, %v; want %q and no volume", v, err, tc.want)
			}
			if len(h.calls) != 0 {
				t.Fatalf("the host was asked %v, want nothing attached", h.calls)
			}
		})
	}
}

func TestAnUnrankedManagerTakesNoSeat(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	m.Rank = NoRank

	// Act
	v, err := m.Acquire(16)

	// Assert
	if err == nil || v != nil || !strings.Contains(err.Error(), "a rank is zero or more") {
		t.Fatalf("Acquire = %v, %v; want the unranked seat refused", v, err)
	}
}

// lineLog is a run log safe across goroutines that hands every line on.
type lineLog struct {
	mu    sync.Mutex
	lines []string
	each  chan string
}

func newLineLog() *lineLog { return &lineLog{each: make(chan string, 16)} }

func (l *lineLog) add(line string) {
	l.mu.Lock()
	l.lines = append(l.lines, line)
	l.mu.Unlock()
	l.each <- line
}

func (l *lineLog) Infof(format string, args ...any)  { l.add("INFO " + fmt.Sprintf(format, args...)) }
func (l *lineLog) Errorf(format string, args ...any) { l.add("ERROR " + fmt.Sprintf(format, args...)) }

// seated is one seat() answer.
type seated struct {
	s   *seats.Seat
	err error
}

// heldBy takes a seat of m's pool as another run of the host would.
func heldBy(t *testing.T, m Manager, n int, h seats.Holder) *seats.Seat {
	t.Helper()
	s, err := seatPool(m.Registry, n).Admit(seats.Admission{
		Me:      h,
		Check:   func([]seats.Holder) (bool, error) { return true, nil },
		Waiting: func(w seats.Wait) error { return fmt.Errorf("the other run waited: %+v", w) },
		Recheck: func() <-chan time.Time { return nil },
	})
	if err != nil {
		t.Fatalf("hold a seat for %+v: %v", h, err)
	}
	return s
}

func TestSeatWaitsStatesItsWaitAndTakesTheSeat(t *testing.T) {
	other := seats.Holder{Pid: 777, Workspace: "fix-footer", Rank: 19, Size: 4096}
	cases := []struct {
		name string
		// total and the available readings, in GiB, in the order the seat
		// reads them.
		total     uint64
		available []uint64
		wantWait  string
		// wake is how the wait ends: the other run lets go, or the recheck
		// fires.
		wake     string
		wantTook string
	}{
		{
			name: "every seat under the ceiling held, woken by the holder letting go", total: 16, available: []uint64{15, 15, 15},
			wantWait: `INFO RAM disk seat: waiting, reason=ceiling (every seat under the ceiling is held); holders=[workspace "fix-footer"]; available=15360MiB wired=4096MiB ceiling=6826MiB floor=10240MiB rank=10`,
			wake:     "release",
			wantTook: `INFO RAM disk seat: took seat 0 of 1 after `,
		},
		{
			name: "refused for memory, woken by the recheck once memory frees", total: 48, available: []uint64{20, 17, 30},
			wantWait: `INFO RAM disk seat: waiting, reason=memory (taking a seat would leave less than the floor available); holders=[workspace "fix-footer"]; available=17408MiB wired=4096MiB ceiling=20480MiB floor=10240MiB rank=10`,
			wake:     "recheck",
			wantTook: `INFO RAM disk seat: took seat 1 of 5 after `,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the other run holds seat 0; this run is the gate.
			h := &fakeHost{}
			m := newManager(t, h)
			log := newLineLog()
			m.Log, m.Rank, m.Workspace = log, 10, "gate-ws"
			reads := 0
			m.Memory = func() (Memory, error) {
				gb := tc.available[min(reads, len(tc.available)-1)]
				reads++
				return Memory{TotalBytes: tc.total * gib, AvailableBytes: gb * gib}, nil
			}
			recheck := make(chan time.Time, 1)
			m.Recheck = func() <-chan time.Time { return recheck }
			n := MemorySeats(tc.total*gib, 4096)
			holder := heldBy(t, m, n, other)
			defer holder.Release()
			done := make(chan seated, 1)

			// Act
			go func() {
				s, err := m.seat(4096)
				done <- seated{s, err}
			}()
			wait := <-log.each
			if tc.wake == "release" {
				if err := holder.Release(); err != nil {
					t.Fatal(err)
				}
			} else {
				recheck <- time.Time{}
			}
			got := <-done
			took := <-log.each

			// Assert
			if got.err != nil {
				t.Fatalf("seat = %v", got.err)
			}
			defer got.s.Release()
			if wait != tc.wantWait {
				t.Fatalf("the wait line =\n%s\nwant\n%s", wait, tc.wantWait)
			}
			if !strings.HasPrefix(took, tc.wantTook) || !strings.Contains(took, `workspace "gate-ws"`) {
				t.Fatalf("the took line = %s, want it to begin %q and name the gate", took, tc.wantTook)
			}
			if _, ok, err := seatwait.Parse(wait); !ok || err != nil {
				t.Fatalf("the wait line does not read back: %v, %v", ok, err)
			}
		})
	}
}

func TestSeatStatesAWaitOnceWhileNothingChanges(t *testing.T) {
	// Arrange: refused twice on the same holders, then admitted.
	h := &fakeHost{}
	m := newManager(t, h)
	log := newLineLog()
	m.Log, m.Rank = log, 10
	avail := []uint64{12, 12, 12, 30}
	reads := 0
	m.Memory = func() (Memory, error) {
		gb := avail[min(reads, len(avail)-1)]
		reads++
		return Memory{TotalBytes: 48 * gib, AvailableBytes: gb * gib}, nil
	}
	recheck := make(chan time.Time, 2)
	recheck <- time.Time{}
	recheck <- time.Time{}
	m.Recheck = func() <-chan time.Time { return recheck }

	// Act
	s, err := m.seat(4096)

	// Assert
	if err != nil {
		t.Fatalf("seat = %v", err)
	}
	defer s.Release()
	close(log.each)
	var waits int
	for line := range log.each {
		if strings.Contains(line, "waiting, reason=") {
			waits++
		}
	}
	if waits != 1 {
		t.Fatalf("the wait was stated %d times, want once: %v", waits, log.lines)
	}
}

func TestSeatSaysNoSeatWasTakenWhenAWaitFails(t *testing.T) {
	// Arrange: refused once, then the memory read fails.
	h := &fakeHost{}
	m := newManager(t, h)
	log := newLineLog()
	m.Log, m.Rank = log, 10
	reads := 0
	m.Memory = func() (Memory, error) {
		reads++
		if reads > 2 {
			return Memory{}, errors.New("sysctl is gone")
		}
		return Memory{TotalBytes: 48 * gib, AvailableBytes: 12 * gib}, nil
	}
	recheck := make(chan time.Time, 1)
	recheck <- time.Time{}
	m.Recheck = func() <-chan time.Time { return recheck }

	// Act
	s, err := m.seat(4096)

	// Assert
	if err == nil || s != nil || !strings.Contains(err.Error(), "admit a RAM disk seat: sysctl is gone") {
		t.Fatalf("seat = %v, %v; want the memory read's failure", s, err)
	}
	last := log.lines[len(log.lines)-1]
	if !strings.HasPrefix(last, "INFO RAM disk seat: no seat was taken: ") || !seatwait.Ended(last) {
		t.Fatalf("the last line = %q; want the wait's end", last)
	}
	if !free(t, seatPool(m.Registry, 5), 0) {
		t.Fatal("the failed seat kept a seat")
	}
}

func TestASeatWaitTheRunCutAttachesNothing(t *testing.T) {
	// Arrange: the host's one memory seat is another run's, and this run's
	// caller is already gone.
	h := &fakeHost{}
	m := newManager(t, h)
	m.Memory = func() (Memory, error) { return Memory{TotalBytes: 16 * gib, AvailableBytes: 15 * gib}, nil }
	other := heldBy(t, m, MemorySeats(16*gib, 4096), seats.Holder{Pid: 777, Workspace: "fix-footer", Rank: 19, Size: 4096})
	defer other.Release()
	cut := make(chan struct{})
	close(cut)
	m.Cut = cut
	log := m.Log.(*fakeLog)

	// Act
	v, err := m.Acquire(4096)

	// Assert
	if !errors.Is(err, seats.ErrCut) || v != nil {
		t.Fatalf("Acquire = %v, %v; want the cut and no volume", v, err)
	}
	if len(h.calls) != 0 {
		t.Fatalf("the host was asked %v, want nothing attached", h.calls)
	}
	if !log.has("INFO RAM disk seat: the run was cut after waiting") {
		t.Fatalf("log = %q, want the cut wait said", log.lines)
	}
}
