package reap

import (
	"os"
	"os/exec"
	"strings"
	"syscall"
	"testing"
	"time"
)

// exitWait bounds a wait the test expects to end at once.
const exitWait = 10 * time.Second

func TestOSTableListsThisProcessInItsSession(t *testing.T) {
	// Arrange.
	sid, err := syscall.Getsid(os.Getpid())
	if err != nil {
		t.Fatal(err)
	}

	// Act.
	procs, err := OSTable{}.List()

	// Assert.
	if err != nil {
		t.Fatalf("List() error = %v", err)
	}
	for _, p := range procs {
		if p.Pid == os.Getpid() {
			if p.Sid != sid || p.Start.IsZero() {
				t.Fatalf("this process is listed as %+v, want session %d and a start", p, sid)
			}
			return
		}
	}
	t.Fatal("this process is not in the listing")
}

// TestReapLeavesNoChildOfAForkingLoop is the leak itself: a parent forking
// children as fast as it can, as the build-frontend harness's shells did. One
// snapshot and one kill pass left hundreds alive; Reap leaves none.
func TestReapLeavesNoChildOfAForkingLoop(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	loop := exec.Command("/bin/bash", "-c", `while :; do /bin/sleep 300 & done`)
	loop.Env = append(os.Environ(), "TMPDIR="+dir+"/tr-1")
	if err := loop.Start(); err != nil {
		t.Fatal(err)
	}
	exited := make(chan error, 1)
	go func() { exited <- loop.Wait() }()
	log := &recLog{}

	// Act.
	res, err := Under(OSTable{}, dir, log)

	// Assert.
	if err != nil {
		t.Fatalf("Under() error = %v; log %q", err, log.errs)
	}
	if len(res.Killed) == 0 {
		t.Fatal("Under() killed nothing")
	}
	t.Logf("the reap: %s", strings.Join(log.info, "; "))
	select {
	case <-exited:
	case <-time.After(exitWait):
		t.Fatal("the forking parent outlived the reap")
	}
	procs, err := OSTable{}.List()
	if err != nil {
		t.Fatal(err)
	}
	dirs, err := Spellings(dir)
	if err != nil {
		t.Fatal(err)
	}
	for _, p := range procs {
		for _, d := range dirs {
			if p.State != 'Z' && names(p.Line, d) {
				t.Errorf("pid %d (%s) survived the reap", p.Pid, p.Line)
			}
		}
	}
}

func TestAwaitExitAnswersAGoneProcessAtOnce(t *testing.T) {
	// Arrange.
	cmd := exec.Command("/usr/bin/true")
	if err := cmd.Run(); err != nil {
		t.Fatal(err)
	}

	// Act.
	left, err := OSTable{}.AwaitExit([]int{cmd.Process.Pid}, exitWait)

	// Assert.
	if err != nil || len(left) != 0 {
		t.Fatalf("AwaitExit() = %v, %v, want nothing left", left, err)
	}
}

func TestAwaitExitWaitsForAKilledProcess(t *testing.T) {
	// Arrange.
	cmd := exec.Command("/bin/sleep", "300")
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	go func() { _ = cmd.Wait() }()
	if err := cmd.Process.Kill(); err != nil {
		t.Fatal(err)
	}

	// Act.
	left, err := OSTable{}.AwaitExit([]int{cmd.Process.Pid}, exitWait)

	// Assert.
	if err != nil || len(left) != 0 {
		t.Fatalf("AwaitExit() = %v, %v, want the killed process exited", left, err)
	}
}

func TestAwaitExitAnswersAProcessStillRunningAtItsBound(t *testing.T) {
	// Arrange.
	cmd := exec.Command("/bin/sleep", "300")
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = cmd.Process.Kill(); _ = cmd.Wait() })

	// Act.
	left, err := OSTable{}.AwaitExit([]int{cmd.Process.Pid}, time.Millisecond)

	// Assert.
	if err != nil || len(left) != 1 || left[0] != cmd.Process.Pid {
		t.Fatalf("AwaitExit() = %v, %v, want the running pid answered", left, err)
	}
}
