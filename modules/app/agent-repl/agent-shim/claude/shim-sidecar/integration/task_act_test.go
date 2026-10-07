package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// SUBJECT — the task tracker's acts, read off a REAL transcript pair.
//
// `AgentTaskAct.state` is "where the act LEFT the task, resolved by the
// producer", and the `rejected` arm says outright that nothing was added and
// nothing changed. The file plane resolved it from the CALL'S OWN INPUT
// instead, so a `TaskUpdate(9, completed)` the board REFUSED came out of here
// as `completed` — and this act is re-delivered after the stream plane's own
// refusal, so it won: the footer's checklist drew a TICKED row for an update
// the tracker had rejected. Observed in the G50–52 playbook against the running
// application.
//
// The other half is the same mistake in the other direction: an update that
// moved only an edge names no status at all, and resolving `pending` for it
// invents one. `AgentTaskState.status` is a oneof precisely so "this act said
// nothing about where the task stands" is representable, and the shim's own
// TypeScript converter leaves it unset. A CREATE keeps the default, because the
// create tool takes no status and a new entry IS recorded and not begun.

// seedTaskUpdate writes one `TaskUpdate` call and its result into TREE's
// session file, built from the captured transcript's own tool_use/tool_result
// pair so the record SHAPE is the vendor's rather than this test's invention.
// Only the tool name, the call id, the arguments and the result body are this
// test's, because those are what the subject is about.
func seedTaskUpdate(
	t *testing.T,
	tree *vendorTree,
	cwd, session, callID string,
	input map[string]any,
	structured map[string]any,
	failed bool,
) int64 {
	t.Helper()
	captured := loadCapturedSession(t)
	slug := cwdSlug(cwd)

	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	call = renameToolUse(t, call, "TaskUpdate")
	call = setToolUseID(t, call, callID)
	call = setToolUseInput(t, call, input)

	result := retargetSession(t, decodeRecord(t, captured.Lines[10]), session, cwd)
	result = setToolUseID(t, result, callID)
	result = setToolResultText(t, result, "the tracker's prose, never read for a status")
	result = withFields(t, result, map[string]any{"toolUseResult": structured})
	if failed {
		result = setToolResultError(t, result)
	}

	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, call))
	g.AppendLine(encodeRecord(t, result))
	// THE WHOLE FILE'S LENGTH, which is what a caller awaits: a cursor at 0 is
	// durable the moment the file is discovered, before either line is read.
	return g.Offset()
}

// setToolResultError marks the record's tool_result block an error, which is
// what the vendor writes for a refused call and what the sidecar reads as the
// failure.
func setToolResultError(t *testing.T, obj map[string]any) map[string]any {
	t.Helper()
	msg, ok := obj["message"].(map[string]any)
	if !ok {
		t.Fatalf("record carries no message object: %v", obj)
	}
	blocks, ok := msg["content"].([]any)
	if !ok || len(blocks) == 0 {
		t.Fatalf("record's message carries no content blocks: %v", msg)
	}
	newBlocks := make([]any, 0, len(blocks))
	var marked bool
	for _, raw := range blocks {
		b, ok := raw.(map[string]any)
		if !ok {
			newBlocks = append(newBlocks, raw)
			continue
		}
		nb := make(map[string]any, len(b))
		for k, v := range b {
			nb[k] = v
		}
		if b["type"] == "tool_result" {
			nb["is_error"] = true
			marked = true
		}
		newBlocks = append(newBlocks, nb)
	}
	if !marked {
		t.Fatalf("record carries no tool_result block to mark an error: %v", msg)
	}
	newMsg := make(map[string]any, len(msg))
	for k, v := range msg {
		newMsg[k] = v
	}
	newMsg["content"] = newBlocks
	return withFields(t, obj, map[string]any{"message": newMsg})
}

// TestARejectedTaskUpdateNeverResolvesTheStatusItAskedFor is the defect's own
// subject: the board refused the update, so the act may not report the status
// the call asked for.
func TestARejectedTaskUpdateNeverResolvesTheStatusItAskedFor(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/task-reject-probe"
	session := "5a5a5a5a-5a5a-45a5-85a5-5a5a5a5a5a5a"
	callID := "toolu_task_update_rejected"

	// Act: the tracker refused `TaskUpdate(9, completed)` and echoed no task.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	end := seedTaskUpdate(t, tree, cwd, session, callID,
		map[string]any{"taskId": "9", "status": "completed"},
		map[string]any{"success": false, "taskId": "9", "error": "no task with id 9"},
		true)
	awaitCursorInBatches(ctx, t, fake, tree.sessionPath(cwdSlug(cwd), session), end)

	// Assert.
	act := lastTaskAct(t, fake, callID)
	if act.GetRejected() == nil {
		t.Fatalf("Act = %T, want the rejected arm", act.GetAct())
	}
	if act.GetState().GetStatus() != nil {
		t.Errorf("a refused update resolved status %+v; it must state NONE, and above all not the "+
			"`completed` it asked for", act.GetState().GetStatus())
	}
}

// TestAnEdgeOnlyTaskUpdateLeavesTheStatusUnset is the other half: nothing was
// said about where the task stands, so nothing may be resolved for it.
func TestAnEdgeOnlyTaskUpdateLeavesTheStatusUnset(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/task-edge-probe"
	session := "5b5b5b5b-5b5b-45b5-85b5-5b5b5b5b5b5b"
	callID := "toolu_task_update_edge_only"

	// Act: an update that links one task behind another and says nothing else.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	end := seedTaskUpdate(t, tree, cwd, session, callID,
		map[string]any{"taskId": "2", "blockedBy": []any{"1"}},
		map[string]any{"success": true, "taskId": "2", "updatedFields": []any{"blockedBy"}},
		false)
	awaitCursorInBatches(ctx, t, fake, tree.sessionPath(cwdSlug(cwd), session), end)

	// Assert.
	act := lastTaskAct(t, fake, callID)
	if act.GetState().GetStatus() != nil {
		t.Errorf("an edge-only update resolved status %+v; it must state NONE", act.GetState().GetStatus())
	}
}

// lastTaskAct answers the task act the unit finally settled as, failing loudly
// when the unit produced none.
func lastTaskAct(t *testing.T, fake *fakeStore, unit string) *conversationv1.AgentTaskAct {
	t.Helper()
	entries := unitEntries(fake.Entries(), unit)
	if len(entries) == 0 {
		t.Fatalf("the call produced no unit under %q; keys were %v",
			"activity:"+unit, upsertKeysOf(fake.Entries()))
	}
	for i := len(entries) - 1; i >= 0; i-- {
		if act := activityOf(entries[i].GetAgentUpdate().GetServeableFrame()).GetTaskAct(); act != nil {
			return act
		}
	}
	t.Fatalf("unit %q wrote %d entries and none of them was a task act", unit, len(entries))
	return nil
}
