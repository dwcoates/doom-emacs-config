//go:build darwin

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

func TestOutput(t *testing.T) {
	tests := []struct {
		name    string
		bound   time.Duration
		command []string
		want    string
		wantErr string
	}{
		{name: "answers what the command printed to stdout", bound: exitWait, command: []string{"/bin/sh", "-c", "echo $LC_ALL; echo noise >&2"}, want: "C\n"},
		{name: "fails a command that fails", bound: exitWait, command: []string{"/usr/bin/false"}, wantErr: "/usr/bin/false: exit status 1"},
		{name: "fails a command that fails with what it said", bound: exitWait, command: []string{"/bin/sh", "-c", "echo refused >&2; exit 3"}, wantErr: "/bin/sh -c echo refused >&2; exit 3: exit status 3: refused"},
		{name: "fails a command that does not answer within its bound", bound: 50 * time.Millisecond, command: []string{"/bin/sleep", "300"}, wantErr: "/bin/sleep 300 did not answer within 50ms: context deadline exceeded"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := output(tt.bound, tt.command[0], tt.command[1:]...)

			// Assert.
			if tt.wantErr != "" {
				if err == nil || err.Error() != tt.wantErr || got != "" {
					t.Fatalf("output() = %q, %v, want error %q", got, err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("output() = %q, %v, want %q", got, err, tt.want)
			}
		})
	}
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

func TestOSReapReapsOverTheHostsTable(t *testing.T) {
	// Arrange.
	log := &recLog{}

	// Act.
	res, err := OSReap(log)(Members{What: "nothing"})

	// Assert.
	if err != nil || res.Passes != 1 || !log.infoHas("reap nothing: freeze pass 1 found no member left running") {
		t.Fatalf("OSReap() = %+v, %v, log %q", res, err, log.info)
	}
}

func TestEveryCallerTakesOSReap(t *testing.T) {
	// Arrange: every place a reap over the host's table is built.
	sites := []string{"../main.go", "../internal/ramdisk/ramdisk.go"}

	for _, site := range sites {
		// Act.
		src, err := os.ReadFile(site)
		if err != nil {
			t.Fatal(err)
		}

		// Assert.
		if !strings.Contains(string(src), "reap.OSReap(") || strings.Contains(string(src), "reap.Reap(reap.OSTable") {
			t.Errorf("%s builds its own reap over the host's table; want reap.OSReap", site)
		}
	}
}
