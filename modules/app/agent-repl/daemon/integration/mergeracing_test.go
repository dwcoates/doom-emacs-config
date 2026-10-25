//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/integration/harness"
)

// switchRacing asks d to disable, or (enable) to enable, the queue's racing.
func switchRacing(t *testing.T, d *harness.Daemon, enable bool) *agentreplv1.UpdateMergeQueueResponse {
	t.Helper()
	req := &agentreplv1.UpdateMergeQueueRequest{Requester: harness.User(),
		Action: &agentreplv1.UpdateMergeQueueRequest_DisableRacing{DisableRacing: &agentreplv1.UpdateMergeQueueDisableRacing{}},
	}
	if enable {
		req.Action = &agentreplv1.UpdateMergeQueueRequest_EnableRacing{EnableRacing: &agentreplv1.UpdateMergeQueueEnableRacing{}}
	}
	return askMergeQueue(t, d, req)
}

// TestRacingDisabledBeforeARestartStaysDisabledInTheSuccessor covers the
// switch's durability end to end: disabled on one daemon, it survives that
// daemon's stop, the successor states it at boot, and the successor answers
// a second disable as racing_already_disabled.
func TestRacingDisabledBeforeARestartStaysDisabledInTheSuccessor(t *testing.T) {
	t.Parallel()
	// Arrange: disable racing, then stop the daemon that disabled it.
	d1 := harness.StartDaemon(t, harness.Opts{DurableState: true})
	if resp := switchRacing(t, d1, false); resp.GetSuccess() == nil {
		t.Fatalf("disable_racing = %v, want a success", resp)
	}
	d1.Stop()

	// Act
	d2 := harness.StartDaemon(t, harness.Opts{StateDir: d1.StateDir, ExtraEnv: []string{"AGENT_REPL_LOCK_DIR=" + d1.LockDir}})
	d2.ExpectWarnings("daemon.merge.disable_racing")
	again := switchRacing(t, d2, false)

	// Assert
	if r := d2.AwaitRunLogOperation("daemon.merge.disable_racing"); r.Level != "info" || r.Context["disabled_since"] == nil {
		t.Fatalf("the successor's boot record = %+v, want the inherited switch stated at INFO with its start", r)
	}
	if again.GetError().GetRacingAlreadyDisabled() == nil {
		t.Fatalf("a disable_racing on the successor = %v, want racing_already_disabled", again)
	}
}
