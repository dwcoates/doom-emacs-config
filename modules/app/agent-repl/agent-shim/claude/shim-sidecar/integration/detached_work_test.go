package integration

import (
	"encoding/json"
	"fmt"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT 3 — the detached-shell lifecycle, read entirely out of files.
//
// A b* spool is a byte stream terminated by its `EXIT=<code>` line. Its bytes
// become StoreAgentUpdate.bash entries whose run is the SPAWNING CALL's
// tool_use_id (agent_activity.proto: a detached command is announced under one
// identity on both streams), each an AgentBash.tail holding the rendered
// window so far — one row superseded per batch, never the whole spool.

// detachedFixture seeds a parent transcript that spawns a background shell
// command, and answers the spool the vendor would be writing for it.
type detachedFixture struct {
	Tree      *vendorTree
	Cwd       string
	Slug      string
	Session   string
	CallID    string
	TaskID    string
	SpoolPath string
	Parent    *growingFile
}

// seedDetachedShell writes the captured transcript's background-launch pair —
// the assistant's `run_in_background` Bash call and the tool_result naming the
// spool — into a fresh session, re-pointed at this test's spool path.
//
// THE PAIR IS GENUINE, and requireCapturedBackgroundLaunchPair below pins that:
// captured line 8 is a real `Bash` tool_use whose input carries
// `run_in_background: true`, and captured line 10 is its real background
// tool_result — the one naming a `backgroundTaskId` and carrying the vendor's
// launch prose. Its `stdout`/`stderr`/`interrupted` fields are present and
// empty because that is what the VENDOR writes for a backgrounded launch (the
// corpus's own tool-results/bash-background.jsonl carries the identical shape),
// not because a foreground result was mutated into one. Only the task id and
// the spool PATH are re-pointed, because those are this test's, and nothing
// else about the record is touched.
func seedDetachedShell(t *testing.T, tree *vendorTree, cwd, session string) detachedFixture {
	t.Helper()
	captured := loadCapturedSession(t)
	slug := cwdSlug(cwd)
	taskID := capturedSpoolTask1
	spool := tree.spoolPath(slug, session, taskID)

	requireCapturedBackgroundLaunchPair(t, captured)
	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	result := retargetSession(t, decodeRecord(t, captured.Lines[10]), session, cwd)
	result = setToolResultText(t, result, backgroundLaunchText(taskID, spool))
	result = setNested(t, result, "toolUseResult", "backgroundTaskId", taskID)

	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, call))
	g.AppendLine(encodeRecord(t, result))
	return detachedFixture{
		Tree: tree, Cwd: cwd, Slug: slug, Session: session,
		CallID: capturedBashCall1, TaskID: taskID, SpoolPath: spool, Parent: g,
	}
}

// appendDetachedLaunch writes a SECOND background-launch pair into a fixture's
// session — the same captured pair, re-pointed at another call id and another
// task — and answers the second run's spool path. It is what gives a subject a
// claimed FENCE run: only a claimed spool is ever read, so only a claimed spool
// can go silent and be concluded under the same window as the subject's own.
func appendDetachedLaunch(t *testing.T, fx detachedFixture, taskID, callID string) string {
	t.Helper()
	captured := loadCapturedSession(t)
	spool := fx.Tree.spoolPath(fx.Slug, fx.Session, taskID)
	cwd := fx.Cwd
	call := setToolUseID(t, retargetSession(t, decodeRecord(t, captured.Lines[8]), fx.Session, cwd), callID)
	result := retargetSession(t, decodeRecord(t, captured.Lines[10]), fx.Session, cwd)
	result = setToolUseID(t, setToolResultText(t, result, backgroundLaunchText(taskID, spool)), callID)
	result = setNested(t, result, "toolUseResult", "backgroundTaskId", taskID)
	fx.Parent.AppendLine(encodeRecord(t, call))
	fx.Parent.AppendLine(encodeRecord(t, result))
	return spool
}

// requireCapturedBackgroundLaunchPair guards the provenance every detached
// subject rests on. A re-capture that moved or changed those two lines must
// fail HERE, saying exactly what it lost, rather than as a mystifying "no bash
// row was ever written" in eight separate subjects.
func requireCapturedBackgroundLaunchPair(t *testing.T, captured capturedSession) {
	t.Helper()
	call := decodeRecord(t, captured.Lines[8])
	if kind, _ := call["type"].(string); kind != "assistant" {
		t.Fatalf("captured line 8 is a %q record, wanted the assistant's background Bash call", kind)
	}
	block := soleToolUseBlock(t, call)
	if name, _ := block["name"].(string); name != "Bash" {
		t.Fatalf("captured line 8 calls %q, wanted Bash", name)
	}
	input, ok := block["input"].(map[string]any)
	if !ok {
		t.Fatalf("captured line 8's Bash call carries no input object: %v", block)
	}
	if background, _ := input["run_in_background"].(bool); !background {
		t.Fatalf("captured line 8's Bash call is not backgrounded (run_in_background=%v); the detached subjects need a real LAUNCH, not a foreground call dressed as one", input["run_in_background"])
	}

	result := decodeRecord(t, captured.Lines[10])
	if id := toolUseIDOfResult(t, result); id != capturedBashCall1 {
		t.Fatalf("captured line 10 answers call %q, wanted line 8's %q", id, capturedBashCall1)
	}
	inner, ok := result["toolUseResult"].(map[string]any)
	if !ok {
		t.Fatalf("captured line 10 carries no toolUseResult: %v", result)
	}
	if got, _ := inner["backgroundTaskId"].(string); got != capturedSpoolTask1 {
		t.Fatalf("captured line 10 names background task %q, wanted %q; a result with no backgroundTaskId is a FOREGROUND result and announces no spool",
			got, capturedSpoolTask1)
	}
}

// soleToolUseBlock answers the one tool_use block of an assistant record.
func soleToolUseBlock(t *testing.T, obj map[string]any) map[string]any {
	t.Helper()
	msg, ok := obj["message"].(map[string]any)
	if !ok {
		t.Fatalf("record carries no message object: %v", obj)
	}
	blocks, _ := msg["content"].([]any)
	for _, raw := range blocks {
		b, ok := raw.(map[string]any)
		if ok && b["type"] == "tool_use" {
			return b
		}
	}
	t.Fatalf("record carries no tool_use block: %v", msg)
	return nil
}

// TestTaskStopResultCancelsTheOwningTask asserts the one EXEMPT-SET carve-out:
// the TaskStop CALL is dropped, but its RESULT is consumed as the owning task's
// cancelled terminal — deliberately-stopped work must never resolve LOST.
func TestTaskStopResultCancelsTheOwningTask(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	fx := seedDetachedShell(t, tree, "/work/detached-stop-probe",
		"dddddddd-dddd-4ddd-8ddd-dddddddddddd")

	stop := decodeRecord(t, corpusLine(t, "tool-results/task_stop.jsonl", 0))
	stop = retargetSession(t, stop, fx.Session, "/work/detached-stop-probe")
	stop = retargetTaskStop(t, stop, fx.TaskID, "local_bash")
	// THE CALL COMES FIRST, as the vendor writes it. An exempt tool's call is
	// DROPPED but still remembered, because this one result has to find the call
	// it belongs to; a fixture that supplies only the result is a transcript no
	// vendor ever wrote, and it exercises the orphan path instead of the
	// carve-out.
	stopCall := retargetSession(t, decodeRecord(t, captured.Lines[8]), fx.Session, "/work/detached-stop-probe")
	stopCall = renameToolUse(t, stopCall, "TaskStop")
	stopCall = setToolUseID(t, stopCall, toolUseIDOfResult(t, stop))

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("partial work\n"))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())
	fx.Parent.AppendLine(encodeRecord(t, stopCall))
	fx.Parent.AppendLine(encodeRecord(t, stop))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())

	// Assert.
	rows := awaitBashRunTerminal(ctx, t, storeClient(fake.Socket), fx.CallID)
	requireBashReplayOrder(t, fx.CallID, rows)
	interrupted := rows[len(rows)-1].GetSuccess().GetInterrupted()
	if interrupted == nil {
		t.Fatalf("a stopped task must settle as interrupted; run %q ended on %v",
			fx.CallID, describeBashRows(rows))
	}
	if interrupted.GetByUser() == nil {
		t.Errorf("a TaskStop result is a person's decision and must state by_user: %v", interrupted.GetCause())
	}
	// THE CANCELLED TERMINAL OWES THE OUTPUT THE SPOOL HELD: the run said
	// something before it was stopped, and the terminal is the last thing any
	// reader sees of it.
	if want := requireLatestTail(t, fx.CallID, rows); interrupted.GetOutput().GetText().GetStdout() != want {
		t.Errorf("the cancelled terminal carries stdout %q, wanted exactly the run's joined deltas %q",
			interrupted.GetOutput().GetText().GetStdout(), want)
	}
}

// retargetTaskStop re-points a TaskStop result at this test's task, in BOTH
// places the vendor states it.
//
// THE VENDOR WRITES THE FACT TWICE: once as the structured `toolUseResult`
// object, and once as the tool_result BLOCK's content, which is that same
// object serialized to a JSON string. Re-pointing only `toolUseResult` left the
// block's embedded JSON still naming the corpus's task (`abaa339795d28ab75`,
// type `local_agent`) — an incoherent record no vendor ever wrote, and one that
// would let a converter reading the block instead of the object pass this
// subject while failing in production on every real stop.
func retargetTaskStop(t *testing.T, obj map[string]any, taskID, taskType string) map[string]any {
	t.Helper()
	inner, ok := obj["toolUseResult"].(map[string]any)
	if !ok {
		t.Fatalf("record carries no toolUseResult: %v", obj)
	}
	retargeted := make(map[string]any, len(inner))
	for k, v := range inner {
		retargeted[k] = v
	}
	retargeted["task_id"] = taskID
	retargeted["task_type"] = taskType
	command, _ := retargeted["command"].(string)
	retargeted["message"] = fmt.Sprintf("Successfully stopped task: %s (%s)", taskID, command)

	encoded, err := json.Marshal(retargeted)
	if err != nil {
		t.Fatalf("encode the retargeted stop result: %v", err)
	}
	out := withFields(t, obj, map[string]any{"toolUseResult": retargeted})
	return setToolResultText(t, out, string(encoded))
}

func runsSeen(entries []*storev1.StoreEntry) []string {
	var out []string
	for _, b := range bashFramesOf(entries) {
		out = append(out, b.GetRun().GetValue())
	}
	return sortedStrings(out)
}

// renameToolUse re-points a record's tool_use block at a different tool name,
// so an exempt-set call can be built from a real assistant line.
func renameToolUse(t *testing.T, obj map[string]any, name string) map[string]any {
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
		if b["type"] == "tool_use" {
			nb["name"] = name
		}
		newBlocks = append(newBlocks, nb)
	}
	newMsg := make(map[string]any, len(msg))
	for k, v := range msg {
		newMsg[k] = v
	}
	newMsg["content"] = newBlocks
	return withFields(t, obj, map[string]any{"message": newMsg})
}

// toolUseIDOfResult reads the call id a tool_result record names, so a fixture's
// call can be built to match a fixture's result rather than guessed.
func toolUseIDOfResult(t *testing.T, obj map[string]any) string {
	t.Helper()
	msg, ok := obj["message"].(map[string]any)
	if !ok {
		t.Fatalf("record carries no message object: %v", obj)
	}
	blocks, ok := msg["content"].([]any)
	if !ok {
		t.Fatalf("record's message carries no content blocks: %v", msg)
	}
	for _, raw := range blocks {
		b, ok := raw.(map[string]any)
		if !ok {
			continue
		}
		if b["type"] == "tool_result" {
			if id, ok := b["tool_use_id"].(string); ok && id != "" {
				return id
			}
		}
	}
	t.Fatalf("record carries no tool_result naming a call: %v", msg)
	return ""
}
