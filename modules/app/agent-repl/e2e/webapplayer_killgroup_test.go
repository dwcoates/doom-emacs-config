package e2e

import (
	"os"
	"os/exec"
	"regexp"
	"syscall"
	"testing"
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
	viaHelper := regexp.MustCompile(`s\.killGroup\(\)`).FindAllIndex(src, -1)

	// Assert: the one raw kill is killProcessGroup's own.
	if len(raw) != 1 {
		t.Fatalf("webapplayer_server_test.go calls syscall.Kill %d time(s), want only killProcessGroup's one", len(raw))
	}
	if len(viaHelper) < 3 {
		t.Fatalf("webapplayer_server_test.go calls s.killGroup() %d time(s), want the warm-up, run and stop sites", len(viaHelper))
	}
}
