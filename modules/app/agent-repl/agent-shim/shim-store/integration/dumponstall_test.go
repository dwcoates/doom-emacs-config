package integration

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"os/signal"
	"strings"
	"syscall"
	"testing"
	"time"
)

// startStalledStore starts a stand-in store process whose SIGQUIT handling is
// the script given, returning once it has said "ready" (every script says so
// after its trap is set, so no signal lands before the disposition under
// test).
func startStalledStore(t *testing.T, cmd *exec.Cmd) *storeProcess {
	t.Helper()
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	out, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatalf("stdout pipe: %v", err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start the stand-in store: %v", err)
	}
	line, err := bufio.NewReader(out).ReadString('\n')
	if err != nil || line != "ready\n" {
		t.Fatalf("stand-in readiness line = %q, %v; want \"ready\"", line, err)
	}
	s := &storeProcess{t: t, cmd: cmd, done: make(chan struct{})}
	go func() {
		_ = cmd.Wait()
		close(s.done)
	}()
	t.Cleanup(func() {
		_ = syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
		<-s.done
	})
	return s
}

// exitOnQuitStandIn makes this test binary, run with it set, a stand-in that
// exits 2 on SIGQUIT (TestMain, runExitOnQuitStandIn).
const exitOnQuitStandIn = "AGENT_REPL_TEST_EXIT_ON_SIGQUIT"

// exitOnQuitCommand is this test binary as that stand-in.
func exitOnQuitCommand() *exec.Cmd {
	cmd := exec.Command(os.Args[0])
	cmd.Env = append(os.Environ(), exitOnQuitStandIn+"=1")
	return cmd
}

// runExitOnQuitStandIn is the stand-in for a process that exits on SIGQUIT,
// as the Go runtime's own handler does.
//
// IT IS SUBSCRIBED BEFORE IT SAYS READY, so no SIGQUIT can arrive before the
// handler that answers it. A shell's trap could not promise that: a trapped
// signal landing just before the shell entered its wait stayed pending until
// the 100s sleep it waited on ended, and the test's bound ran out first.
func runExitOnQuitStandIn() int {
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGQUIT)
	fmt.Println("ready")
	<-quit
	return 2
}

func TestDumpOnStall(t *testing.T) {
	tests := []struct {
		name    string
		standIn func() *exec.Cmd
		bound   time.Duration
		want    string
	}{
		{
			name:    "a store that dumps and exits on SIGQUIT is reported as dumped",
			standIn: exitOnQuitCommand,
			bound:   dumpBound,
			want:    "goroutine dump is in the stderr below",
		},
		{
			name: "a store that ignores SIGQUIT is reported as not having exited",
			standIn: func() *exec.Cmd {
				return exec.Command("/bin/sh", "-c", `trap '' QUIT; echo ready; while :; do sleep 100 & wait $!; done`)
			},
			bound: 50 * time.Millisecond,
			want:  "did not exit within 50ms",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s := startStalledStore(t, tc.standIn())

			// Act
			got := s.dumpOnStall(tc.bound)

			// Assert
			if !strings.Contains(got, tc.want) {
				t.Fatalf("dumpOnStall = %q, want it to say %q", got, tc.want)
			}
		})
	}
}

func TestDumpOnStallOfAStoreAlreadyGone(t *testing.T) {
	// Arrange: the store has exited and been reaped.
	s := startStalledStore(t, exec.Command("/bin/sh", "-c", `echo ready; exit 0`))
	<-s.done

	// Act
	got := s.dumpOnStall(dumpBound)

	// Assert
	if !strings.Contains(got, "already gone") {
		t.Fatalf("dumpOnStall = %q, want the already-gone answer", got)
	}
}
