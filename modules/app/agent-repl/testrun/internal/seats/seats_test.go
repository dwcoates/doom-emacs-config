package seats

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
)

// hold locks seat i of p for the test, as another run would.
func hold(t *testing.T, p Pool, i int) *os.File {
	t.Helper()
	f, err := p.Open(i)
	if err != nil {
		t.Fatal(err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		t.Fatalf("hold seat %d: %v", i, err)
	}
	return f
}

// Free reports whether seat i of p can be locked right now.
func free(t *testing.T, p Pool, i int) bool {
	t.Helper()
	f, err := p.Open(i)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB)
	if errors.Is(err, syscall.EWOULDBLOCK) {
		return false
	}
	if err != nil {
		t.Fatal(err)
	}
	return true
}

func TestPathNamesTheSeatByPrefixAndIndex(t *testing.T) {
	// Arrange
	p := Pool{Dir: "/r/seats", Prefix: "core", N: 3, What: "core"}

	// Act
	got := p.Path(2)

	// Assert
	if got != "/r/seats/core-2.lock" {
		t.Fatalf("Path(2) = %q, want /r/seats/core-2.lock", got)
	}
}

func TestTakeTakesTheFirstFreeSeat(t *testing.T) {
	cases := []struct {
		name string
		held []int
		want int
	}{
		{name: "an empty pool gives seat 0", want: 0},
		{name: "a held seat is passed over", held: []int{0}, want: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			p := Pool{Dir: t.TempDir(), Prefix: "seat", N: 2, What: "test"}
			for _, i := range tc.held {
				defer hold(t, p, i).Close()
			}
			waited := false

			// Act
			f, got, err := p.Take(func(int) { waited = true })

			// Assert
			if err != nil || got != tc.want || waited {
				t.Fatalf("Take = seat %d, %v, waited %v; want seat %d at once", got, err, waited, tc.want)
			}
			if free(t, p, got) {
				t.Fatalf("seat %d reads free while it is held", got)
			}
			if err := f.Close(); err != nil {
				t.Fatal(err)
			}
		})
	}
}

func TestTakeWaitsForTheFirstSeatLetGo(t *testing.T) {
	// Arrange: both seats held by other runs.
	p := Pool{Dir: t.TempDir(), Prefix: "seat", N: 2, What: "test"}
	seat0 := hold(t, p, 0)
	defer seat0.Close()
	seat1 := hold(t, p, 1)
	waiting := make(chan int, 1)
	type taken struct {
		f   *os.File
		i   int
		err error
	}
	done := make(chan taken, 1)

	// Act: the run waits, and seat 1's holder lets go.
	go func() {
		f, i, err := p.Take(func(held int) { waiting <- held })
		done <- taken{f, i, err}
	}()
	if held := <-waiting; held != 2 {
		t.Fatalf("the wait was told %d seats are held, want 2", held)
	}
	if err := seat1.Close(); err != nil {
		t.Fatal(err)
	}
	got := <-done

	// Assert
	if got.err != nil || got.i != 1 {
		t.Fatalf("Take = seat %d, %v; want seat 1, the one let go", got.i, got.err)
	}
	if err := got.f.Close(); err != nil {
		t.Fatal(err)
	}
}

func TestTakeRefusesAnEmptyPool(t *testing.T) {
	// Arrange
	p := Pool{Dir: t.TempDir(), Prefix: "seat", N: 0, What: "test"}

	// Act
	f, _, err := p.Take(func(int) { t.Fatal("an empty pool must never wait") })

	// Assert
	if err == nil || f != nil || !strings.Contains(err.Error(), "has 0 seats") {
		t.Fatalf("Take = %v, %v; want the empty pool refused", f, err)
	}
}

func TestTakeSurfacesAnUnopenableSeat(t *testing.T) {
	// Arrange: the pool's directory is a file, so no seat can be made.
	file := filepath.Join(t.TempDir(), "not-a-dir")
	if err := os.WriteFile(file, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	p := Pool{Dir: file, Prefix: "seat", N: 1, What: "test"}

	// Act
	_, _, err := p.Take(func(int) {})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "make the test seats directory") {
		t.Fatalf("Take = %v; want the seats directory's failure", err)
	}
}

func TestTakeNHoldsEverySeatItAskedFor(t *testing.T) {
	// Arrange
	p := Pool{Dir: t.TempDir(), Prefix: "core", N: 3, What: "test"}

	// Act
	held, err := p.TakeN(2, 0, func() { t.Fatal("a pool with free seats must not wait") })

	// Assert
	if err != nil || len(held) != 2 {
		t.Fatalf("TakeN(2) = %d seats, %v; want 2", len(held), err)
	}
	if free(t, p, 0) || free(t, p, 1) || !free(t, p, 2) {
		t.Fatal("want seats 0 and 1 held and seat 2 free")
	}
	if err := held.Release(); err != nil {
		t.Fatal(err)
	}
	for i := range 3 {
		if !free(t, p, i) {
			t.Fatalf("seat %d is still held after Release", i)
		}
	}
}

func TestTakeNRefusesACountOutsideThePool(t *testing.T) {
	cases := []struct {
		name string
		n    int
	}{
		{name: "zero seats", n: 0},
		{name: "more seats than the pool has", n: 3},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			p := Pool{Dir: t.TempDir(), Prefix: "core", N: 2, What: "test"}

			// Act
			held, err := p.TakeN(tc.n, 0, func() { t.Fatal("a refused count must never wait") })

			// Assert
			if err == nil || held != nil || !strings.Contains(err.Error(), "from one to all of them") {
				t.Fatalf("TakeN(%d) = %v, %v; want it refused", tc.n, held, err)
			}
		})
	}
}

func TestTakeNWaitsOnceForSeatsLetGo(t *testing.T) {
	// Arrange: both seats held by another run.
	p := Pool{Dir: t.TempDir(), Prefix: "core", N: 2, What: "test"}
	seat0, seat1 := hold(t, p, 0), hold(t, p, 1)
	waits := make(chan struct{}, 2)
	type taken struct {
		h   Held
		err error
	}
	done := make(chan taken, 1)

	// Act: the taker waits, and the other run lets both seats go.
	go func() {
		h, err := p.TakeN(2, 0, func() { waits <- struct{}{} })
		done <- taken{h, err}
	}()
	<-waits
	if err := seat0.Close(); err != nil {
		t.Fatal(err)
	}
	if err := seat1.Close(); err != nil {
		t.Fatal(err)
	}
	got := <-done

	// Assert
	if got.err != nil || len(got.h) != 2 {
		t.Fatalf("TakeN(2) = %d seats, %v; want both", len(got.h), got.err)
	}
	if len(waits) != 0 {
		t.Fatal("waiting was called more than once")
	}
	if err := got.h.Release(); err != nil {
		t.Fatal(err)
	}
}

func TestTakeNWaitsForTheGathererAhead(t *testing.T) {
	// Arrange: another taker holds the turnstile, mid-gather.
	p := Pool{Dir: t.TempDir(), Prefix: "core", N: 2, What: "test"}
	if err := os.MkdirAll(p.Dir, 0o755); err != nil {
		t.Fatal(err)
	}
	turnstile, err := p.lock(p.turnstilePath(), syscall.LOCK_EX)
	if err != nil {
		t.Fatal(err)
	}
	waits := make(chan struct{}, 1)
	done := make(chan error, 1)

	// Act: the taker waits on the turnstile even though seats are free, and
	// the gatherer ahead lets it go.
	go func() {
		h, err := p.TakeN(1, 0, func() { waits <- struct{}{} })
		if err == nil {
			err = h.Release()
		}
		done <- err
	}()
	<-waits
	if err := turnstile.Close(); err != nil {
		t.Fatal(err)
	}

	// Assert
	if err := <-done; err != nil {
		t.Fatalf("TakeN after the turnstile freed = %v", err)
	}
}

func TestTakeNGivesBackWhatItGatheredWhenASeatFails(t *testing.T) {
	// Arrange: seat 0 can be made, seat 1's path is a directory, so opening
	// it fails after seat 0 is already held.
	p := Pool{Dir: t.TempDir(), Prefix: "core", N: 2, What: "test"}
	if err := os.Mkdir(p.Path(1), 0o755); err != nil {
		t.Fatal(err)
	}

	// Act
	held, err := p.TakeN(2, 0, func() {})

	// Assert
	if err == nil || held != nil || !strings.Contains(err.Error(), "open the test seat") {
		t.Fatalf("TakeN(2) = %v, %v; want seat 1's failure", held, err)
	}
	if !free(t, p, 0) {
		t.Fatal("seat 0 is still held after the failed gather")
	}
}

func TestTakeNRefusesANegativeRank(t *testing.T) {
	// Arrange
	p := Pool{Dir: t.TempDir(), Prefix: "core", N: 1, What: "test"}

	// Act
	held, err := p.TakeN(1, -1, func() { t.Fatal("a refused rank must never wait") })

	// Assert
	if err == nil || held != nil || !strings.Contains(err.Error(), "a rank is zero or more") {
		t.Fatalf("TakeN at rank -1 = %v, %v; want it refused", held, err)
	}
}

func TestTakeNYieldsToAHigherRankWaiting(t *testing.T) {
	// Arrange: a higher-priority taker (rank 1) is waiting, holding its lane.
	p := Pool{Dir: t.TempDir(), Prefix: "core", N: 1, What: "test"}
	if err := os.MkdirAll(p.Dir, 0o755); err != nil {
		t.Fatal(err)
	}
	higher, err := p.lock(p.lanePath(1), syscall.LOCK_SH)
	if err != nil {
		t.Fatal(err)
	}
	waits := make(chan struct{}, 1)
	done := make(chan error, 1)

	// Act: a rank-2 taker asks while the seat is free; it must wait until the
	// higher waiter leaves its lane.
	go func() {
		h, err := p.TakeN(1, 2, func() { waits <- struct{}{} })
		if err == nil {
			err = h.Release()
		}
		done <- err
	}()
	<-waits
	if !free(t, p, 0) {
		t.Fatal("the lower-ranked taker took the seat while a higher rank waited")
	}
	if err := higher.Close(); err != nil {
		t.Fatal(err)
	}

	// Assert
	if err := <-done; err != nil {
		t.Fatalf("TakeN after the higher rank left = %v", err)
	}
}

func TestTakeNIgnoresWaitersOfLowerOrEqualRank(t *testing.T) {
	cases := []struct {
		name string
		lane int
	}{
		{name: "a lower-priority waiter", lane: 3},
		{name: "a waiter of the same rank", lane: 2},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			p := Pool{Dir: t.TempDir(), Prefix: "core", N: 1, What: "test"}
			if err := os.MkdirAll(p.Dir, 0o755); err != nil {
				t.Fatal(err)
			}
			other, err := p.lock(p.lanePath(tc.lane), syscall.LOCK_SH)
			if err != nil {
				t.Fatal(err)
			}
			defer other.Close()

			// Act
			held, err := p.TakeN(1, 2, func() { t.Fatal("must not wait on a waiter that does not outrank it") })

			// Assert
			if err != nil || len(held) != 1 {
				t.Fatalf("TakeN = %v, %v; want the seat at once", held, err)
			}
			if err := held.Release(); err != nil {
				t.Fatal(err)
			}
		})
	}
}

func TestTakeNLetsItsLaneGoOnceItHasItsSeats(t *testing.T) {
	// Arrange
	p := Pool{Dir: t.TempDir(), Prefix: "core", N: 1, What: "test"}

	// Act
	held, err := p.TakeN(1, 1, func() {})
	if err != nil {
		t.Fatal(err)
	}
	defer held.Release()

	// Assert: a lower rank probing lane 1 finds nobody waiting there.
	above, err := p.waiterAbove(2)
	if err != nil || above != -1 {
		t.Fatalf("waiterAbove(2) = %d, %v; want -1: the holder left its lane", above, err)
	}
}
