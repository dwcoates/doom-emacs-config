package integration

import (
	"strings"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — a TaskStop RESULT naming an AGENT task, and one naming a task no
// launch opened.
//
// DELIBERATELY-STOPPED WORK MUST NEVER RESOLVE LOST. A shell task's cancelled
// terminal is minted by the spool's reader (it owes the output the spool holds);
// an AGENT task settles in the converter, because the spawn unit is a line in
// this stream's own book. And a stop naming a task NOTHING launched cannot be
// keyed on a guess: it is stored whole, loudly, so the stop is not lost and no
// row is settled against an invented owner.

// TestATaskStopOnAnAgentTaskCancelsItsSpawnUnit asserts the agent arm of the
// carve-out, with the vendor's own `local_agent` task type.
func TestATaskStopOnAnAgentTaskCancelsItsSpawnUnit(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/taskstop-agent-probe"
	session := "6a6a6a6a-6a6a-46a6-86a6-6a6a6a6a6a6a"
	launch := corpusRecord(t, "tool-results/agent_async_launch.jsonl", 0)
	vendorTask, _ := launch["toolUseResult"].(map[string]any)["agentId"].(string)
	if vendorTask == "" {
		t.Fatalf("the async-launch fixture names no vendor agentId: %v", launch)
	}

	stop := retargetTaskStop(t,
		retargetSession(t, decodeRecord(t, corpusLine(t, "tool-results/task_stop.jsonl", 0)), session, cwd),
		vendorTask, "local_agent")
	stopCall := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	stopCall = renameToolUse(t, stopCall, "TaskStop")
	stopCall = setToolUseID(t, stopCall, toolUseIDOfResult(t, stop))

	// Act: the launch first, because the spawn unit the stop settles is the CALL
	// that opened it and only the launch says which call that was.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := seedAgentSpawn(t, tree, cwd, session)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	g.AppendLine(encodeRecord(t, stopCall))
	g.AppendLine(encodeRecord(t, stop))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	wantKey := "activity:" + corpusSubagentAgentID
	e := fake.awaitEntry(ctx, t, "the cancelled spawn unit", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == wantKey &&
			activityOf(e.GetAgentUpdate().GetServeableFrame()).GetSubagent().GetFailure() != nil
	})
	failure := activityOf(e.GetAgentUpdate().GetServeableFrame()).GetSubagent().GetFailure()
	if failure.GetStoppedByUser() == nil {
		t.Errorf("a stopped subagent must resolve stopped_by_user; the failure states %v", failure.GetCause())
	}
	for _, entry := range fake.Entries() {
		if strings.HasPrefix(entry.GetUpsertKey(), "bash:") {
			t.Errorf("an AGENT stop minted the shell row %q; it settles its spawn unit and no run", entry.GetUpsertKey())
		}
	}
}

// TestATaskStopForATaskNoLaunchOpenedIsClassifiedWholeAndLoudly asserts the
// other edge: an unlaunched task's stop is carried whole under the residue kind
// that names WHY, rather than keyed on a guess, and is never quietly dropped
// into the LOST policy's hands.
//
// THAT KIND IS RESIDUE, AND RESIDUE IS NEVER PERSISTED. The reader still frames
// the stop and still says `task_stop/unlaunched` about it — that account is the
// coverage — and it announces no row, so nothing is keyed on an invented owner
// and nothing is stored.
func TestATaskStopForATaskNoLaunchOpenedIsClassifiedWholeAndLoudly(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/taskstop-unlaunched-probe"
	slug := cwdSlug(cwd)
	session := "6b6b6b6b-6b6b-46b6-86b6-6b6b6b6b6b6b"
	// The per-record withholding statement is DEBUG, and it is this subject's
	// evidence that the stop was framed rather than dropped.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))
	unlaunched := "nolaunchtask00001"

	stop := retargetTaskStop(t,
		retargetSession(t, decodeRecord(t, corpusLine(t, "tool-results/task_stop.jsonl", 0)), session, cwd),
		unlaunched, "local_agent")
	stopCall := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	stopCall = renameToolUse(t, stopCall, "TaskStop")
	stopCall = setToolUseID(t, stopCall, toolUseIDOfResult(t, stop))

	// Act: no launch of any kind precedes the stop.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	g.AppendLine(encodeRecord(t, stopCall))
	g.AppendLine(encodeRecord(t, stop))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: framed whole under the residue kind that names WHY…
	rec := awaitResidueWithheld(ctx, t, opts.LogPath, "vendor_specific/task_stop/unlaunched")
	// …announcing no row, because nothing was stored and a key nobody can look
	// up is an untraceable announcement…
	if key, ok := rec.Context["upsert_key"]; ok && key != "" {
		t.Errorf("the withholding record names upsert_key %v; a withheld stop announces no row", key)
	}
	requireNoResidueStored(t, fake.Entries())
	// …and said so.
	awaitLog(ctx, t, opts.LogPath, "the unlaunched-stop warning", func(r logRecord) bool {
		return r.Level == "warn" && r.Operation == "task-stop" && r.Context["task_id"] == unlaunched
	})
	// A stop is never keyed onto a run nobody launched.
	for _, e := range fake.Entries() {
		if strings.HasPrefix(e.GetUpsertKey(), "bash:"+unlaunched) {
			t.Errorf("the stop minted run rows under the vendor task id: %q", e.GetUpsertKey())
		}
	}
}
