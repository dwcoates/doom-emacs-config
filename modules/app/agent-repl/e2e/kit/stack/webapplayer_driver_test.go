package stack

import (
	"agent-repld/integration/grantedclock"
	"agent-repld/integration/harness"
	"context"
	"fmt"
	"os/exec"
	"syscall"
	"testing"
	"time"
)

// TestWebappLayerParticipantHoldOutlivesTheWaitBound pins the bound the host
// participant hold runs on.
//
// The hold exists so the footer reads CONNECTED for the whole vitest child
// (internal/resolve/footer/status.go refuses `connected` while either
// participant stream is down). Bounding it by harness.DefaultTimeout — which
// is what `Daemon.WatchHost` does, correctly, for a WAIT — expired the hold
// five seconds into a child that runs far longer: the footer dropped to
// `disconnected`, the real page closed its composer gate, and every later
// submission was swallowed by the app itself with nothing reaching the daemon.
// A hold is bounded by the thing it is held FOR, so this asserts the two
// bounds are the ones the arrangement needs and not each other.
func TestWebappLayerParticipantHoldOutlivesTheWaitBound(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name  string
		bound time.Duration
		floor time.Duration
	}{
		{
			name:  "a functional area's bound outlives the harness's wait bound",
			bound: WebappLayerTimeout,
			floor: harness.DefaultTimeout,
		},
		{
			// ONE BOUND PER AREA, since 2026-09-04: WebappLayerTimeout used to
			// carry 300s so that the handover area fitted under it too, which
			// made every OTHER area's hang bound thirty times its slowest
			// observed child (PERF-SPEC.md §H finding 2). The areas whose child
			// is structurally longer now pass their own bound to
			// wlChild.WaitFor, and each such bound is pinned here.
			name:  "the handover area's bound outlives the handover chain it drives",
			bound: WebappLayerHandoverTimeout,
			floor: harness.HandoverChainTimeout,
		},
		{
			name:  "a cold child's mount bound outlives the warm driver's warm-up bound",
			bound: WebappLayerColdMountTimeout,
			floor: wlWarmupBound,
		},
		{
			name:  "a cold child's mount bound outlives a functional area's bound",
			bound: WebappLayerColdMountTimeout,
			floor: WebappLayerTimeout,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act: the constants themselves are the subject.
			got := tc.bound

			// Assert.
			if got <= tc.floor {
				t.Errorf("the participant hold's bound = %s, want more than %s", got, tc.floor)
			}
		})
	}
}

// TestWlKillTreeReapsAGrandchild pins the reason wlStartVitest puts its child
// in its own process group.
//
// `npm run` is a wrapper: the process the Go side holds is not the one doing
// the work, and vitest's own worker pool is a further generation down. A kill
// aimed at cmd.Process alone left every worker alive and reparented to init,
// spinning on the box for the rest of the suite. This drives the exact shape
// with a shell standing in for npm: the parent exits immediately, and only a
// group-wide kill can reach the grandchild it left behind.
func TestWlKillTreeReapsAGrandchild(t *testing.T) {
	t.Parallel()
	// Arrange: a shell that spawns a long-lived grandchild, reports its pid,
	// and waits — the npm/vitest shape.
	report := reportFIFO(t, t.TempDir(), "grandchild.pid")
	cmd := exec.Command("/bin/sh", "-c",
		fmt.Sprintf("sleep 600 & echo $! > %s; wait", report))
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start the stand-in child: %v", err)
	}
	child := &wlChild{t: t, cmd: cmd, done: make(chan error, 1)}
	exited := make(chan struct{})
	go func() {
		err := cmd.Wait()
		close(exited)
		child.done <- err
	}()

	grandchild := readReportedPID(t, report, exited)
	if err := syscall.Kill(grandchild, 0); err != nil {
		t.Fatalf("the grandchild %d is not running before the kill: %v", grandchild, err)
	}

	// Act.
	child.killTree()

	// Assert: the grandchild is gone, not merely its parent.
	gone, cancel := grantedclock.WithBound(context.Background(), DefaultTimeout)
	defer cancel()
	if err := AwaitPIDsGone(gone, []int{grandchild}); err != nil {
		_ = syscall.Kill(grandchild, syscall.SIGKILL)
		t.Fatalf("the grandchild survived killTree; the kill reached the parent only: %v", err)
	}
}
