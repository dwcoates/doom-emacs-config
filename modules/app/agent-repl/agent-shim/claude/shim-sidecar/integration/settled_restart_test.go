package integration

import (
	"context"
	"path/filepath"
	"strings"
	"testing"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
)

// SUBJECT — A RUN THAT SETTLED IS NEVER TRACKED AGAIN, OR CONCLUDED LOST, BY A
// SIDECAR STARTED AFTER THE SETTLE.
//
// On 2026-09-30 a background shell's spool ended `EXIT=0`, the sidecar settled
// its run, and two deploys restarted the sidecar. Each new process resumed the
// spool at its committed cursor — past the marker — so nothing it read said the
// run had ended, it tracked the quiet spool, and the silence window later
// concluded the finished run LOST over its real terminal.
//
// THE STORE IS REAL, so the settle the first process made durable is exactly
// what GetRunSettlements answers the second one. The fence is a second run in
// the same session that goes silent and IS concluded LOST by the second
// process, so the subject's having no conclusion is a decision of a sweep that
// ran, not the absence of one.
func TestARestartedSidecarNeverTracksOrLosesARunThatSettledBeforeIt(t *testing.T) {
	t.Parallel()
	// Arrange: the first process reads the run's own EXIT marker and settles it.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/work/settled-restart-probe", "e7e7e7e7-e7e7-4e7e-8e7e-e7e7e7e7e7e7")
	opts := lostOptions(t, store.Socket, tree)
	opts.StaleShellSilence = shortSilence
	first := startSidecar(t, opts)
	awaitCursorAtLeast(ctx, t, store.Client, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("work\nEXIT=0\n"))
	awaitCursorAtLeast(ctx, t, store.Client, fx.SpoolPath, spool.Offset())
	awaitLog(ctx, t, opts.LogPath, "the first process settling the run", func(r logRecord) bool {
		return r.Operation == "run-settled" && samePathAny(r.Context["path"], fx.SpoolPath)
	})
	first.Stop()
	// THE SHIM'S CLAIM IS HOW A RESTARTED READER RE-CLAIMS THE SPOOL: the launch
	// line sits behind the transcript's cursor and is not read again, and the
	// shim states the pairing from the vendor's task stream (2026-09-30 the
	// owner's store holds exactly this row for the run in question).
	claimSpoolAsTheShimDoes(ctx, t, store.Client, fx.TaskID, fx.CallID)

	// Act: a fresh process over the same store and files, with a silent fence.
	fencePath := appendDetachedLaunch(t, fx, "b0fence", capturedBashCall2)
	restarted := opts
	restarted.LogPath = filepath.Join(t.TempDir(), "sidecar-restarted.log")
	// THE SETTLE IS STATED AT EITHER LEVEL: a restarted reader meets the spool
	// as boot backlog, so whether its settle lands inside the startup catch-up
	// (stated at DEBUG) or just after it (INFO) is a matter of scheduling, not
	// of the subject. The restarted process records both.
	restarted.ExtraEnv = append(append([]string(nil), opts.ExtraEnv...), "AGENT_REPL_LOG_LEVEL=debug")
	startSidecar(t, restarted)
	fence := newGrowingFile(t, fencePath)
	fence.AppendRaw([]byte("said once and never again\n"))
	awaitLostConclusion(ctx, t, restarted.LogPath, fencePath, "went_silent")

	// Assert: settled per the store, never tracked, never concluded.
	awaitLog(ctx, t, restarted.LogPath, "the restarted process settling the run by the record", func(r logRecord) bool {
		return r.Operation == "lost-policy" && samePathAny(r.Context["path"], fx.SpoolPath) &&
			strings.Contains(r.Message, "run already settled per the store")
	})
	for _, r := range readLog(t, restarted.LogPath) {
		if r.Operation == "lost-policy" && samePathAny(r.Context["path"], fx.SpoolPath) &&
			strings.Contains(r.Message, "tracking detached run") {
			t.Fatalf("the restarted process tracked the settled run: %q", r.Message)
		}
	}
	if stated := lostConclusions(t, restarted.LogPath, fx.SpoolPath); len(stated) != 0 {
		t.Fatalf("the restarted process concluded the settled run LOST: %+v", stated)
	}
}

// claimSpoolAsTheShimDoes writes the shim's claim pairing a spool's task id
// with its run, exactly as EntryBatch.shell_run_claims carries it.
func claimSpoolAsTheShimDoes(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, taskID, run string) {
	t.Helper()
	res, err := c.WriteBatch(ctx, connect.NewRequest(&storev1.WriteBatchRequest{
		WriteClass: &storev1.WriteClass{WriteClass: &storev1.WriteClass_Interactive{Interactive: &storev1.WriteClassInteractive{}}},
		Producer:   "shim-claude",
		Batch: &storev1.EntryBatch{ShellRunClaims: []*storev1.ShellRunClaim{{
			VendorTaskId: taskID, Run: &conversationv1.AgentActivityId{Value: run},
		}}},
	}))
	if err != nil {
		t.Fatalf("writing the shim's claim: %v", err)
	}
	if res.Msg.GetSuccess() == nil {
		t.Fatalf("the store refused the shim's claim: %v", res.Msg.GetResult())
	}
}
