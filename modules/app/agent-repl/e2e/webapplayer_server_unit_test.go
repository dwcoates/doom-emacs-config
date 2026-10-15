package e2e

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"regexp"
	"strings"
	"syscall"
	"testing"
	"time"
)

func TestKillProcessGroup(t *testing.T) {
	cases := []struct {
		name  string
		reap  bool // the group has exited and been reaped before the kill
		alive bool // the group is running when killed
	}{
		{name: "a running group is killed", alive: true},
		{name: "a group already gone is no failure", reap: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a process leading its own group.
			cmd := exec.Command("/bin/sleep", "60")
			cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
			if err := cmd.Start(); err != nil {
				t.Fatalf("start: %v", err)
			}
			pgid := cmd.Process.Pid
			if tc.reap {
				if err := cmd.Process.Kill(); err != nil {
					t.Fatalf("kill: %v", err)
				}
				_ = cmd.Wait() // a killed process's Wait answers its signal
			}

			// Act
			err := killProcessGroup(pgid)

			// Assert
			if err != nil {
				t.Fatalf("killProcessGroup = %v, want nil", err)
			}
			if tc.alive {
				err := cmd.Wait()
				ws, ok := cmd.ProcessState.Sys().(syscall.WaitStatus)
				if err == nil || !ok || !ws.Signaled() || ws.Signal() != syscall.SIGKILL {
					t.Fatalf("the group's leader ended with %v (%v), want SIGKILL", err, cmd.ProcessState)
				}
			}
		})
	}
}

// TestTheLayerDriverIsKilledOnlyThroughKillGroup pins that every kill of the
// driver's group goes through killGroup, so none drops the kill's error.
func TestTheLayerDriverIsKilledOnlyThroughKillGroup(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("webapplayer_server_test.go")
	if err != nil {
		t.Fatalf("read the driver's source: %v", err)
	}

	// Act
	raw := regexp.MustCompile(`syscall\.Kill\(`).FindAllIndex(src, -1)
	group := regexp.MustCompile(`harness\.KillGroup\(`).FindAllIndex(src, -1)
	viaHelper := regexp.MustCompile(`s\.killGroup\(\)`).FindAllIndex(src, -1)

	// Assert: no raw kill, and the one group kill is killProcessGroup's own.
	if len(raw) != 0 {
		t.Fatalf("webapplayer_server_test.go calls syscall.Kill %d time(s), want none: the group kill is harness.KillGroup's", len(raw))
	}
	if len(group) != 1 {
		t.Fatalf("webapplayer_server_test.go calls harness.KillGroup %d time(s), want only killProcessGroup's one", len(group))
	}
	if len(viaHelper) < 3 {
		t.Fatalf("webapplayer_server_test.go calls s.killGroup() %d time(s), want the warm-up, run and stop sites", len(viaHelper))
	}
}

// wlFakeTimer is a silence bound the test fires itself; every restart is
// signalled on resets.
type wlFakeTimer struct {
	c      chan time.Time
	resets chan struct{}
}

func (f *wlFakeTimer) C() <-chan time.Time { return f.c }
func (f *wlFakeTimer) Reset(time.Duration) { f.resets <- struct{}{} }
func (f *wlFakeTimer) Stop()               {}

// wlFakeServer is a wlServer over a driver this test plays: requests are
// read off stdin, and the lines are whatever the test sends. Its process is a
// group-leading sleep, so a kill has a real group to end.
func wlFakeServer(t *testing.T) (*wlServer, chan<- string, *wlFakeTimer) {
	t.Helper()
	cmd := exec.Command("/bin/sleep", "60")
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start: %v", err)
	}
	// THE CLEANUP KILLS THE PROCESS, NOT ITS GROUP: a run this test makes give
	// up has killed the group already, and darwin answers a group holding only
	// its zombie with EPERM. A process that is a zombie takes the kill as done.
	t.Cleanup(func() {
		if err := cmd.Process.Kill(); err != nil {
			t.Errorf("kill the fake driver %d: %v", cmd.Process.Pid, err)
		}
		_ = cmd.Wait() // a killed process's Wait answers its signal
	})
	r, w := io.Pipe()
	go func() { _, _ = io.Copy(io.Discard, r) }() // the driver's stdin, drained
	lines := make(chan string, 64)
	timer := &wlFakeTimer{c: make(chan time.Time, 1), resets: make(chan struct{}, 64)}
	s := &wlServer{cmd: cmd, stdin: w, lines: lines, silence: WebappLayerSilence,
		newTimer: func(time.Duration) wlTimer { return timer }}
	return s, lines, timer
}

func TestWlServerRunRestartsTheBoundOnEveryFinishedTest(t *testing.T) {
	// Arrange: four finished tests, then the answer.
	s, lines, timer := wlFakeServer(t)
	for range 4 {
		lines <- wlProgressPrefix + "pass"
	}
	lines <- wlResultPrefix + `{"file":"a.layer.test.ts","ok":true,"tests":4}`

	// Act
	err := s.Run(t, "a.layer.test.ts", wlWorld{})

	// Assert
	if err != nil || s.broken != nil {
		t.Fatalf("Run = %v (broken %v), want the run answered", err, s.broken)
	}
	if got := len(timer.resets); got != 4 {
		t.Fatalf("the bound was restarted %d time(s), want once per finished test", got)
	}
}

func TestWlServerRunThatFinishesNoTestWithinTheBoundIsBroken(t *testing.T) {
	// Arrange: one finished test, then silence: the bound fires once the run
	// has taken that test's restart.
	s, lines, timer := wlFakeServer(t)
	lines <- wlProgressPrefix + "pass"
	go func() {
		<-timer.resets
		timer.c <- time.Now()
	}()

	// Act
	err := s.Run(t, "a.layer.test.ts", wlWorld{})

	// Assert
	want := fmt.Sprintf("a.layer.test.ts finished no test for %s (1 finished before)", WebappLayerSilence)
	if err == nil || !strings.Contains(err.Error(), want) {
		t.Fatalf("Run = %v, want an error naming %q", err, want)
	}
	if s.broken == nil {
		t.Fatal("the driver was left usable after a run it gave up on")
	}
}
