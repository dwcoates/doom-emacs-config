package harness

import (
	"bufio"
	"os/exec"
	"strings"
	"syscall"
	"testing"
	"time"
)

// startQuitChild starts a child in a group of its own whose SIGQUIT handling
// is the script given, standing in for a daemon that stalled. It returns once
// the child has said "ready", which every script does after its trap is set,
// so no signal can land before the disposition it is testing.
func startQuitChild(t *testing.T, script string) *exec.Cmd {
	t.Helper()
	cmd := exec.Command("/bin/sh", "-c", script)
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	out, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatalf("stdout pipe: %v", err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start child: %v", err)
	}
	line, err := bufio.NewReader(out).ReadString('\n')
	if err != nil || line != "ready\n" {
		t.Fatalf("child readiness line = %q, %v; want \"ready\"", line, err)
	}
	t.Cleanup(func() {
		_ = syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
		_ = cmd.Wait()
	})
	return cmd
}

func TestDumpOnStall(t *testing.T) {
	tests := []struct {
		name   string
		script string
		bound  time.Duration
		want   string
	}{
		{
			name:   "a daemon that dumps and exits on SIGQUIT is reported as dumped",
			script: `trap 'exit 2' QUIT; echo ready; while :; do sleep 100 & wait $!; done`,
			bound:  DefaultTimeout,
			want:   "the daemon.cmd.sigquit record below holds its goroutine dump",
		},
		{
			name:   "a daemon that ignores SIGQUIT is reported as not having exited",
			script: `trap '' QUIT; echo ready; while :; do sleep 100 & wait $!; done`,
			bound:  50 * time.Millisecond,
			want:   "did not exit within 50ms",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a bare Daemon carrying only what the dump touches.
			cmd := startQuitChild(t, tc.script)
			d := &Daemon{t: t, cmd: cmd}

			// Act
			got := d.dumpOnStall(tc.bound)

			// Assert
			if !strings.Contains(got, tc.want) {
				t.Fatalf("dumpOnStall = %q, want it to say %q", got, tc.want)
			}
		})
	}
}

func TestDumpOnStallNeverSignalsAReapedDaemon(t *testing.T) {
	// Arrange: the reap has begun, so the pid may already name a stranger.
	cmd := startQuitChild(t, `echo ready; while :; do sleep 100 & wait $!; done`)
	d := &Daemon{t: t, cmd: cmd}
	d.sigMu.Lock()
	d.reapBegun = true
	d.sigMu.Unlock()

	// Act
	got := d.dumpOnStall(DefaultTimeout)

	// Assert: refused, and the child was left alone.
	if !strings.Contains(got, "already reaped") {
		t.Fatalf("dumpOnStall = %q, want the reaped refusal", got)
	}
	if err := cmd.Process.Signal(syscall.Signal(0)); err != nil {
		t.Fatalf("the child is gone after dumpOnStall on a reaped daemon: %v", err)
	}
}
