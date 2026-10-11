package e2e

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

// envStandInStore selects the stand-in store a re-exec of this test binary
// runs instead of the suite (TestMain): one of the standIn* modes below.
const envStandInStore = "AGENT_REPL_E2E_STANDIN_STORE"

// The stand-in store's SIGQUIT dispositions.
const (
	standInExitsOnQuit = "exits-on-quit"
	standInIgnoresQuit = "ignores-quit"
	standInExitsAtOnce = "exits-at-once"
)

// runStandInStore is a stand-in store's whole life: it puts its SIGQUIT
// disposition in force, says "ready", and acts on it.
//
// WHY A GO PROCESS AND NOT A SHELL. A shell runs a trap only between
// commands: a SIGQUIT landing after `sleep 100 &` forks but before `wait`
// blocks stays pending until the wait returns, 100s later, so the stand-in
// "did not exit" for no reason of the code under test. The Go runtime takes
// a signal at any instant, so once "ready" is printed there is no moment the
// disposition is not in force.
func runStandInStore(mode string) int {
	switch mode {
	case standInExitsOnQuit:
		quit := make(chan os.Signal, 1)
		signal.Notify(quit, syscall.SIGQUIT)
		fmt.Println("ready")
		<-quit
		return 2
	case standInIgnoresQuit:
		signal.Ignore(syscall.SIGQUIT)
		never := make(chan os.Signal, 1)
		signal.Notify(never, syscall.SIGTERM)
		fmt.Println("ready")
		<-never
		return 0
	case standInExitsAtOnce:
		fmt.Println("ready")
		return 0
	}
	fmt.Fprintf(os.Stderr, "e2e: unknown stand-in store mode %q\n", mode)
	return 1
}

// startStalledStore starts a stand-in store process with the SIGQUIT
// disposition MODE, returning once it has said "ready", which it says only
// once that disposition is in force.
func startStalledStore(t *testing.T, mode string) *Store {
	t.Helper()
	self, err := os.Executable()
	if err != nil {
		t.Fatalf("locate this test binary to re-exec as the stand-in store: %v", err)
	}
	cmd := exec.Command(self)
	cmd.Env = append(os.Environ(), envStandInStore+"="+mode)
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
	s := &Store{t: t, cmd: cmd, exit: watchProcess(cmd)}
	t.Cleanup(func() {
		_ = syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
		<-s.exit.done
	})
	return s
}

func TestStoreDumpOnStall(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name  string
		mode  string
		bound time.Duration
		want  string
	}{
		{
			name:  "a store that dumps and exits on SIGQUIT is reported as dumped",
			mode:  standInExitsOnQuit,
			bound: storeDumpBound,
			want:  "goroutine dump of the store is on stderr above",
		},
		{
			name:  "a store that ignores SIGQUIT is reported as not having exited",
			mode:  standInIgnoresQuit,
			bound: 50 * time.Millisecond,
			want:  "did not exit within 50ms",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			s := startStalledStore(t, tc.mode)

			// Act
			got := s.dumpOnStall(tc.bound)

			// Assert
			if !strings.Contains(got, tc.want) {
				t.Fatalf("dumpOnStall = %q, want it to say %q", got, tc.want)
			}
		})
	}
}

func TestStoreDumpOnStallOfAStoreAlreadyGone(t *testing.T) {
	t.Parallel()
	// Arrange: the store has exited and been reaped.
	s := startStalledStore(t, standInExitsAtOnce)
	<-s.exit.done

	// Act
	got := s.dumpOnStall(storeDumpBound)

	// Assert
	if !strings.Contains(got, "already gone") {
		t.Fatalf("dumpOnStall = %q, want the already-gone answer", got)
	}
}
