package integration

import (
	"path/filepath"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

func TestAFileScopedDiagnosticReachesClientLogWithItsCompleteShape(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, store.Socket, tree)
	opts.ExtraEnv = []string{"AGENT_REPL_LOG_LEVEL=debug"}
	process := startSidecar(t, opts)
	transcript := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))

	// Act.
	for _, line := range captured.Lines {
		transcript.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, store, transcript.Path(), transcript.Offset())
	request := process.Daemon.awaitRequest(ctx, func(req *agentreplv1.ClientLogRequest) bool {
		return req.GetRecord().GetOperation() == "convert-line"
	})

	// Assert.
	record := request.GetRecord()
	if record.GetSidecar() == nil || record.GetDebug() == nil {
		t.Fatalf("ClientLog arms = runtime %T level %T, want sidecar/debug", record.GetRuntime(), record.GetLevel())
	}
	if !record.GetVerbose() {
		t.Fatal("ClientLog verbose = false, want the sidecar's verbose class")
	}
	if _, err := time.Parse(sharedlogging.TimestampLayout, record.GetTimestamp()); err != nil {
		t.Fatalf("ClientLog timestamp = %q, want the sidecar's contract timestamp: %v", record.GetTimestamp(), err)
	}
	if request.GetWorkspace().GetId() == "" || request.GetWorkspace().GetDir() == "" {
		t.Fatalf("ClientLog workspace ref = %v, want id and dir", request.GetWorkspace())
	}
	context := record.GetContext().AsMap()
	if context["claude_session_id"] != captured.Session || context["pid"] == nil {
		t.Fatalf("ClientLog context = %v, want claude_session_id and sidecar pid", context)
	}
	wantPath, err := filepath.EvalSymlinks(transcript.Path())
	if err != nil {
		t.Fatalf("normalize transcript path: %v", err)
	}
	if context["path"] != wantPath {
		t.Fatalf("ClientLog context.path = %v, want %q", context["path"], wantPath)
	}
}
