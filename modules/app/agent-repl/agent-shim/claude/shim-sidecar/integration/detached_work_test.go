package integration

import (
	"strconv"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT 3 — the detached-shell lifecycle, read entirely out of files.
//
// A b* spool is a DELTA STREAM terminated by its `EXIT=<code>` line. Its bytes
// become StoreAgentUpdate.bash entries whose run is the SPAWNING CALL's
// tool_use_id (agent_activity.proto: a detached command is announced under one
// identity on both streams), each frame an AgentBash.update carrying
// new_output and the from_offset that must equal the bytes the consumer has
// already accumulated — a gap detector, so contiguity is the contract.

// detachedFixture seeds a parent transcript that spawns a background shell
// command, and answers the spool the vendor would be writing for it.
type detachedFixture struct {
	Tree      *vendorTree
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
func seedDetachedShell(t *testing.T, tree *vendorTree, cwd, session string) detachedFixture {
	t.Helper()
	captured := loadCapturedSession(t)
	slug := cwdSlug(cwd)
	taskID := capturedSpoolTask1
	spool := tree.spoolPath(slug, session, taskID)

	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	result := retargetSession(t, decodeRecord(t, captured.Lines[10]), session, cwd)
	result = setToolResultText(t, result, backgroundLaunchText(taskID, spool))
	result = setNested(t, result, "toolUseResult", "backgroundTaskId", taskID)

	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, call))
	g.AppendLine(encodeRecord(t, result))
	return detachedFixture{
		Tree: tree, Slug: slug, Session: session,
		CallID: capturedBashCall1, TaskID: taskID, SpoolPath: spool, Parent: g,
	}
}

// TestSpoolBytesBecomeBashUpdatesUnderTheSpawningCallsIdentity asserts the run
// identity: the deltas are keyed by the tool_use_id of the call that spawned
// them, never by the vendor's task id.
func TestSpoolBytesBecomeBashUpdatesUnderTheSpawningCallsIdentity(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/detached-run-probe",
		"88888888-8888-4888-8888-888888888888")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("first chunk of output\n"))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())

	// Assert.
	frames := bashFramesForRun(fake.Entries(), fx.CallID)
	if len(frames) == 0 {
		t.Fatalf("no bash frame was written for run %q; runs seen: %v",
			fx.CallID, runsSeen(fake.Entries()))
	}
}

// TestBashDeltasCarryContiguousOffsets asserts every update's from_offset
// equals the bytes already accumulated — no gap and no overlap.
func TestBashDeltasCarryContiguousOffsets(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/detached-offset-probe",
		"99999999-9999-4999-8999-999999999999")
	chunks := [][]byte{
		[]byte("chunk one\n"),
		[]byte("chunk two, a little longer\n"),
		[]byte("chunk three\n"),
	}

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	for _, c := range chunks {
		spool.AppendRaw(c)
		awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())
	}

	// Assert.
	var accumulated uint64
	var joined strings.Builder
	var updates int
	for _, frame := range bashFramesForRun(fake.Entries(), fx.CallID) {
		up := frame.GetUpdate()
		if up == nil {
			continue
		}
		updates++
		if up.GetFromOffset() != accumulated {
			t.Fatalf("update %d states from_offset %d, wanted %d — from_offset is a gap detector and must equal the bytes already accumulated",
				updates, up.GetFromOffset(), accumulated)
		}
		joined.WriteString(up.GetNewOutput())
		accumulated += uint64(len(up.GetNewOutput()))
	}
	if updates == 0 {
		t.Fatalf("the spool grew three times and produced no update frame")
	}
	want := string(chunks[0]) + string(chunks[1]) + string(chunks[2])
	if joined.String() != want {
		t.Errorf("the deltas concatenate to %q, wanted the spool's bytes %q", joined.String(), want)
	}
}

// TestTheExitMarkerSettlesTheRunAsCompleted asserts the `EXIT=<code>` line ends
// the run as COMPLETED with the shell's own verdict — a nonzero exit is still
// the success arm.
func TestTheExitMarkerSettlesTheRunAsCompleted(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/detached-exit-probe",
		"aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
	// The corpus's CLEAN spool ends with the terminal marker; its last line is
	// EXIT=1, so the code the shell reported is 1.
	clean := corpusBytes(t, "spools/bash-clean.output")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw(clean)
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())

	// Assert.
	var settled *conversationv1.AgentBashSuccess
	for _, frame := range bashFramesForRun(fake.Entries(), fx.CallID) {
		if s := frame.GetSuccess(); s != nil {
			settled = s
		}
	}
	if settled == nil {
		t.Fatalf("the EXIT marker produced no terminal frame for run %q", fx.CallID)
	}
	completed := settled.GetCompleted()
	if completed == nil {
		t.Fatalf("a spool that ran to its EXIT marker must settle as completed, not interrupted")
	}
	exited := completed.GetTermination().GetExited()
	if exited == nil {
		t.Fatalf("a detached shell's terminal must state the shell's termination: %v", completed.GetTermination())
	}
	if exited.GetCode() != int32(exitCodeOf(t, clean)) {
		t.Errorf("terminal states exit code %d, wanted the marker's %d", exited.GetCode(), exitCodeOf(t, clean))
	}
	if completed.GetOutput().GetText().GetWhole() == nil {
		t.Errorf("a completed detached shell carries its whole output: %v", completed.GetOutput())
	}
}

// TestASplitSpoolLineConvertsOnceAndWhole asserts the carry: a spool cut
// mid-line and then completed yields the line once, entire.
func TestASplitSpoolLineConvertsOnceAndWhole(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/detached-carry-probe",
		"bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
	head := "a line that will be cut in "
	tail := "half\nEXIT=0\n"

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte(head))
	awaitAnyCursorFor(ctx, t, fake, fx.SpoolPath)
	spool.AppendRaw([]byte(tail))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())

	// Assert.
	var joined strings.Builder
	for _, frame := range bashFramesForRun(fake.Entries(), fx.CallID) {
		if up := frame.GetUpdate(); up != nil {
			joined.WriteString(up.GetNewOutput())
		}
	}
	whole := joined.String()
	if strings.Count(whole, "a line that will be cut in half") != 1 {
		t.Fatalf("the split line converted %d times, wanted exactly once; deltas joined to %q",
			strings.Count(whole, "a line that will be cut in half"), whole)
	}
}

// TestDetachedRunFramesAreNeverPageLines asserts a bash run's frames are
// routed to the lifecycle record and never to a book — the spawning CALL is
// already the page line.
func TestDetachedRunFramesAreNeverPageLines(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/detached-page-probe",
		"cccccccc-cccc-4ccc-8ccc-cccccccccccc")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("some output\nEXIT=0\n"))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())

	// Assert.
	for _, e := range fake.Entries() {
		if e.GetAgentUpdate().GetBash() == nil {
			continue
		}
		if e.GetAgentUpdate().GetServeableFrame() != nil {
			t.Errorf("entry %q is both a bash frame and a page line", e.GetUpsertKey())
		}
		if want := "bash:" + fx.CallID; e.GetUpsertKey() != want {
			t.Errorf("bash frame keyed %q, wanted %q", e.GetUpsertKey(), want)
		}
	}
}

// TestTaskStopResultCancelsTheOwningTask asserts the one EXEMPT-SET carve-out:
// the TaskStop CALL is dropped, but its RESULT is consumed as the owning task's
// cancelled terminal — deliberately-stopped work must never resolve LOST.
func TestTaskStopResultCancelsTheOwningTask(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/detached-stop-probe",
		"dddddddd-dddd-4ddd-8ddd-dddddddddddd")

	stop := decodeRecord(t, corpusLine(t, "tool-results/task_stop.jsonl", 0))
	stop = retargetSession(t, stop, fx.Session, "/Users/dodgecoates/detached-stop-probe")
	stop = setNested(t, stop, "toolUseResult", "task_id", fx.TaskID)
	stop = setNested(t, stop, "toolUseResult", "task_type", "local_bash")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("partial work\n"))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())
	fx.Parent.AppendLine(encodeRecord(t, stop))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())

	// Assert.
	var interrupted *conversationv1.AgentBashInterrupted
	for _, frame := range bashFramesForRun(fake.Entries(), fx.CallID) {
		if i := frame.GetSuccess().GetInterrupted(); i != nil {
			interrupted = i
		}
	}
	if interrupted == nil {
		t.Fatalf("a stopped task must settle as interrupted; run %q never did", fx.CallID)
	}
	if interrupted.GetByUser() == nil {
		t.Errorf("a TaskStop result is a person's decision and must state by_user: %v", interrupted.GetCause())
	}
}

// TestTaskStopCallItselfIsDropped asserts the exempt-set half of the carve-out:
// the CALL produces nothing at all — not a page line, not residue.
func TestTaskStopCallItselfIsDropped(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/taskstop-call-probe"
	slug := cwdSlug(cwd)
	session := "eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee"
	callID := "toolu_taskstop_call_0001"

	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	call = setToolUseID(t, call, callID)
	call = renameToolUse(t, call, "TaskStop")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, call))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	for _, e := range fake.Entries() {
		if strings.Contains(e.GetUpsertKey(), callID) {
			t.Errorf("an exempt-set call produced entry %q; exempt calls are dropped entirely", e.GetUpsertKey())
		}
	}
}

// exitCodeOf reads the code from a spool's terminal EXIT marker.
func exitCodeOf(t *testing.T, spool []byte) int {
	t.Helper()
	for _, line := range strings.Split(strings.TrimRight(string(spool), "\n"), "\n") {
		if strings.HasPrefix(line, "EXIT=") {
			code, err := strconv.Atoi(strings.TrimSpace(strings.TrimPrefix(line, "EXIT=")))
			if err != nil {
				t.Fatalf("spool EXIT marker %q is not a code: %v", line, err)
			}
			return code
		}
	}
	t.Fatalf("spool carries no EXIT marker")
	return 0
}

// runsSeen lists every detached run the sidecar named, for a failure message.
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
