package ramdisk

import (
	"errors"
	"os"
	"strings"
	"syscall"
	"testing"
)

func TestMemorySeats(t *testing.T) {
	cases := []struct {
		name    string
		mem     uint64
		sizeMiB int
		want    int
	}{
		{name: "half of 64 GiB holds four 8 GiB disks", mem: 64 << 30, sizeMiB: 8192, want: 4},
		{name: "a share smaller than one disk still seats one run", mem: 8 << 30, sizeMiB: 8192, want: 1},
		{name: "a share that does not divide evenly rounds down", mem: 40 << 30, sizeMiB: 8192, want: 2},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := MemorySeats(tc.mem, tc.sizeMiB)

			// Assert
			if got != tc.want {
				t.Fatalf("MemorySeats(%d, %d) = %d, want %d", tc.mem, tc.sizeMiB, got, tc.want)
			}
		})
	}
}

func TestParseMemsize(t *testing.T) {
	cases := []struct {
		name    string
		out     string
		want    uint64
		wantErr bool
	}{
		{name: "sysctl's count of bytes", out: "68719476736\n", want: 68719476736},
		{name: "an answer that is not a number", out: "unknown oid\n", wantErr: true},
		{name: "a count of zero", out: "0\n", wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := parseMemsize(tc.out)

			// Assert
			if (err != nil) != tc.wantErr || got != tc.want {
				t.Fatalf("parseMemsize(%q) = %d, %v; want %d, error %v", tc.out, got, err, tc.want, tc.wantErr)
			}
		})
	}
}

// holdSeat locks seat i under registry for the test, as another run would.
func holdSeat(t *testing.T, registry string, i int) *os.File {
	t.Helper()
	f, err := openSeat(registry, i)
	if err != nil {
		t.Fatal(err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		t.Fatalf("hold seat %d: %v", i, err)
	}
	return f
}

// seatFree reports whether seat i can be locked right now.
func seatFree(t *testing.T, registry string, i int) bool {
	t.Helper()
	f, err := openSeat(registry, i)
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

func TestTakeSeatTakesTheFirstFreeSeat(t *testing.T) {
	cases := []struct {
		name string
		held []int
		want int
	}{
		{name: "an empty registry gives seat 0", want: 0},
		{name: "a held seat is passed over", held: []int{0}, want: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			registry := t.TempDir()
			for _, i := range tc.held {
				defer holdSeat(t, registry, i).Close()
			}
			waited := false

			// Act
			f, got, err := takeSeat(registry, 2, func(int) { waited = true })

			// Assert
			if err != nil || got != tc.want || waited {
				t.Fatalf("takeSeat = seat %d, %v, waited %v; want seat %d at once", got, err, waited, tc.want)
			}
			if seatFree(t, registry, got) {
				t.Fatalf("seat %d reads free while it is held", got)
			}
			if err := f.Close(); err != nil {
				t.Fatal(err)
			}
		})
	}
}

func TestTakeSeatWaitsForTheFirstSeatLetGo(t *testing.T) {
	// Arrange: both seats held by other runs.
	registry := t.TempDir()
	seat0 := holdSeat(t, registry, 0)
	defer seat0.Close()
	seat1 := holdSeat(t, registry, 1)
	waiting := make(chan int, 1)
	type taken struct {
		f   *os.File
		i   int
		err error
	}
	done := make(chan taken, 1)

	// Act: the run waits, and seat 1's holder lets go.
	go func() {
		f, i, err := takeSeat(registry, 2, func(held int) { waiting <- held })
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
		t.Fatalf("takeSeat = seat %d, %v; want seat 1, the one let go", got.i, got.err)
	}
	if err := got.f.Close(); err != nil {
		t.Fatal(err)
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
	if seatFree(t, m.Registry, 0) {
		t.Fatal("seat 0 reads free while the run's disk holds it")
	}

	// Act
	err = v.Release()

	// Assert
	if err != nil || !seatFree(t, m.Registry, 0) {
		t.Fatalf("Release = %v, seat free %v; want the seat given back", err, seatFree(t, m.Registry, 0))
	}
}

func TestAcquireWithoutTheHostsMemoryAttachesNothing(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	m.Memory = func() (uint64, error) { return 0, errors.New("sysctl is gone") }

	// Act
	v, err := m.Acquire(16)

	// Assert
	if err == nil || v != nil || !strings.Contains(err.Error(), "size the RAM disk seats: sysctl is gone") {
		t.Fatalf("Acquire = %v, %v; want the memory read's failure and no volume", v, err)
	}
	if len(h.calls) != 0 {
		t.Fatalf("the host was asked %v, want nothing attached", h.calls)
	}
}
