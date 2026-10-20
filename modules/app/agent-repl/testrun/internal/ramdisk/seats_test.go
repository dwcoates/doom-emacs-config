package ramdisk

import (
	"errors"
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

// seatFree reports whether memory seat i under registry can be locked now.
func seatFree(t *testing.T, registry string, i int) bool {
	t.Helper()
	f, err := seatPool(registry, i+1).Open(i)
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

// The memory seats keep the paths every run on the host already shares: a
// run built before the seats were shared with the core pool and one built
// after must still contend for the same files.
func TestTheMemorySeatsLiveInTheRegistrysSeatsDirectory(t *testing.T) {
	// Act
	got := seatPool("/reg", 4).Path(3)

	// Assert
	if got != "/reg/seats/seat-3.lock" {
		t.Fatalf("seatPool(/reg, 4).Path(3) = %q, want /reg/seats/seat-3.lock", got)
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
