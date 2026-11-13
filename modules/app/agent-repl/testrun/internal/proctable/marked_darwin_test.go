package proctable

import (
	"math"
	"os"
	"os/exec"
	"slices"
	"syscall"
	"testing"
)

// The kernel's own reads: this process carries HOME from its start, so a
// scan for HOME finds it, with the CPU getrusage says it used, and a child it
// reaped shows in its children's CPU.
func TestNewMarkedReadsThisProcessesCPUAndItsReapedChildren(t *testing.T) {
	// Arrange
	m, err := NewMarked("HOME")
	if err != nil {
		t.Fatal(err)
	}
	if err := exec.Command("/usr/bin/true").Run(); err != nil {
		t.Fatal(err)
	}
	var self, children syscall.Rusage
	if err := syscall.Getrusage(syscall.RUSAGE_SELF, &self); err != nil {
		t.Fatal(err)
	}
	if err := syscall.Getrusage(syscall.RUSAGE_CHILDREN, &children); err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := m.Scan()

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	i := slices.IndexFunc(got, func(u Usage) bool { return u.Pid == os.Getpid() })
	if i < 0 {
		t.Fatal("this process is not scanned as marked")
	}
	seconds := func(tv syscall.Timeval) float64 { return float64(tv.Sec) + float64(tv.Usec)/1e6 }
	wantOwn := seconds(self.Utime) + seconds(self.Stime)
	// The scan ran after getrusage, so it reads at least as much, and the
	// scan itself costs well under a second.
	if u := got[i]; u.Own < wantOwn || u.Own > wantOwn+1 || u.Ppid != os.Getppid() || u.Start.IsZero() {
		t.Fatalf("this process scanned as %+v; want own CPU from %.3fs, ppid %d and a start", u, wantOwn, os.Getppid())
	}
	wantChildren := seconds(children.Utime) + seconds(children.Stime)
	if c := got[i].Children; math.Abs(c-wantChildren) > 0.01 || c <= 0 {
		t.Fatalf("children's CPU = %.4fs; want getrusage's %.4fs, above zero", c, wantChildren)
	}
}
