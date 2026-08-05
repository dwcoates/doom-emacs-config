package e2e

import (
	"fmt"
	"os/exec"
	"syscall"
	"testing"
	"time"
)

// spawnSleeper starts a long-lived child in its own group and registers it,
// mirroring exactly what startShimStore does.
func spawnSleeper(t *testing.T) *exec.Cmd {
	t.Helper()
	cmd := exec.Command("sleep", "300")
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start sleeper: %v", err)
	}
	registerChild(cmd.Process.Pid)
	return cmd
}

// alive reports whether PID still exists. Signal 0 performs the permission and
// existence checks without delivering anything.
func alive(pid int) bool {
	return syscall.Kill(pid, 0) == nil
}

// THE REGRESSION THIS PACKAGE ACTUALLY SUFFERED: children outliving the binary
// because only t.Cleanup reaped them. reapChildren is what the signal handler
// and the post-run backstop both call, so proving it kills a registered child
// proves the path that was missing.
func TestReaperKillsARegisteredChild(t *testing.T) {
	// Arrange
	cmd := spawnSleeper(t)
	pid := cmd.Process.Pid
	t.Cleanup(func() { _ = syscall.Kill(-pid, syscall.SIGKILL); _ = cmd.Wait() })

	// Act
	reapChildren()

	// Assert — reaped, not merely signalled: Wait reaps the zombie so the
	// liveness check below cannot be satisfied by an unreaped entry.
	_ = cmd.Wait()
	if alive(pid) {
		t.Fatalf("pid %d survived reapChildren; a killed test binary would have stranded it", pid)
	}
}

// A child whose own cleanup already reaped it must be unregistered, or the
// end-of-run sweep signals a pid the OS may have recycled onto something else.
func TestReaperDoesNotSignalAnUnregisteredChild(t *testing.T) {
	// Arrange
	cmd := spawnSleeper(t)
	pid := cmd.Process.Pid
	_ = syscall.Kill(-pid, syscall.SIGKILL)
	_ = cmd.Wait()

	// Act
	unregisterChild(pid)

	// Assert — nothing is left for the sweep to address.
	reaper.mu.Lock()
	_, tracked := reaper.groups[pid]
	reaper.mu.Unlock()
	if tracked {
		t.Fatalf("pid %d still registered after unregisterChild", pid)
	}
}

// The group, not just the leader: a store that spawned its own children would
// otherwise leak them one level down.
func TestReaperKillsTheWholeProcessGroup(t *testing.T) {
	// Arrange — a shell that forks a grandchild and then waits.
	cmd := exec.Command("sh", "-c", "sleep 300 & echo $!; wait")
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	out, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatalf("stdout pipe: %v", err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start group leader: %v", err)
	}
	registerChild(cmd.Process.Pid)
	var grandchild int
	if _, err := fmt.Fscan(out, &grandchild); err != nil {
		t.Fatalf("reading grandchild pid: %v", err)
	}
	t.Cleanup(func() { _ = syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL); _ = cmd.Wait() })

	// Act
	reapChildren()
	_ = cmd.Wait()

	// Assert — the grandchild went with the group.
	deadline := time.Now().Add(3 * time.Second)
	for alive(grandchild) && time.Now().Before(deadline) {
		time.Sleep(10 * time.Millisecond)
	}
	if alive(grandchild) {
		t.Fatalf("grandchild %d survived the group kill", grandchild)
	}
}
