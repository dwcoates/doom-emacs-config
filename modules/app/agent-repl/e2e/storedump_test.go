package e2e

import (
	"bufio"
	"os/exec"
	"strings"
	"syscall"
	"testing"
	"time"
)

// startStalledStore starts a stand-in store process whose SIGQUIT handling is
// the script given, returning once it has said "ready" (every script says so
// after its trap is set, so no signal lands before the disposition under
// test).
func startStalledStore(t *testing.T, script string) *Store {
	t.Helper()
	cmd := exec.Command("/bin/sh", "-c", script)
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
		name   string
		script string
		bound  time.Duration
		want   string
	}{
		{
			name:   "a store that dumps and exits on SIGQUIT is reported as dumped",
			script: `trap 'exit 2' QUIT; echo ready; while :; do sleep 100 & wait $!; done`,
			bound:  storeDumpBound,
			want:   "goroutine dump of the store is on stderr above",
		},
		{
			name:   "a store that ignores SIGQUIT is reported as not having exited",
			script: `trap '' QUIT; echo ready; while :; do sleep 100 & wait $!; done`,
			bound:  50 * time.Millisecond,
			want:   "did not exit within 50ms",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			s := startStalledStore(t, tc.script)

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
	s := startStalledStore(t, `echo ready; exit 0`)
	<-s.exit.done

	// Act
	got := s.dumpOnStall(storeDumpBound)

	// Assert
	if !strings.Contains(got, "already gone") {
		t.Fatalf("dumpOnStall = %q, want the already-gone answer", got)
	}
}
