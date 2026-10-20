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
