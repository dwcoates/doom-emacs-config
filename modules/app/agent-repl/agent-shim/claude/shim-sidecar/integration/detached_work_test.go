package integration

import (
	"strconv"
	"strings"
	"testing"

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

	// Assert: the run is READABLE under the spawning call's identity...
	rows, ok := watchBashRun(ctx, t, storeClient(fake.Socket), fx.CallID)
	if !ok || len(rows) == 0 {
		t.Fatalf("no bash row was readable for run %q; runs seen: %v",
			fx.CallID, runsSeen(fake.Entries()))
	}
	// ...and under nothing else. The vendor task id must never reach the run's
	// identity space: a row keyed by it can be joined to no call in the book.
	if _, found := watchBashRun(ctx, t, storeClient(fake.Socket), fx.TaskID); found {
		t.Fatalf("the run was also readable under the vendor task id %q", fx.TaskID)
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

	// Assert: read the run back and walk its deltas in the order the store
	// replays them, which is the order a consumer accumulates them in.
	rows, ok := watchBashRun(ctx, t, storeClient(fake.Socket), fx.CallID)
	if !ok {
		t.Fatalf("the run %q was not readable at all; runs seen: %v", fx.CallID, runsSeen(fake.Entries()))
	}
	var updates int
	for _, row := range rows {
		if row.GetUpdate() != nil {
			updates++
		}
	}
	if updates == 0 {
		t.Fatalf("the spool grew three times and produced no update row: %v", describeBashRows(rows))
	}
	joined := requireContiguousDeltas(t, fx.CallID, rows)
	want := string(chunks[0]) + string(chunks[1]) + string(chunks[2])
	if joined != want {
		t.Errorf("the deltas concatenate to %q, wanted the spool's bytes %q", joined, want)
	}
}

// TestEachSpoolDeltaIsItsOwnRowSoNoneErasesAnother asserts the per-row key
// space where it is observable: the store holds one row per write, so a run that
// grew three times replays THREE deltas. One key for the run would leave only
// the last, and the output before it would be gone.
func TestEachSpoolDeltaIsItsOwnRowSoNoneErasesAnother(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/detached-rows-probe",
		"a5a5a5a5-a5a5-4a5a-8a5a-a5a5a5a5a5a5")
	chunks := []string{"one\n", "two\n", "three\n"}

	// Act: each chunk is a separate poll, so each is a separate write.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	for _, chunk := range chunks {
		spool.AppendRaw([]byte(chunk))
		awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())
	}

	// Assert.
	rows, ok := watchBashRun(ctx, t, storeClient(fake.Socket), fx.CallID)
	if !ok {
		t.Fatalf("the run %q was not readable at all", fx.CallID)
	}
	var deltas int
	for _, row := range rows {
		if row.GetUpdate() != nil {
			deltas++
		}
	}
	if deltas != len(chunks) {
		t.Fatalf("the run replayed %d delta rows for %d writes: %v; each write is its own row",
			deltas, len(chunks), describeBashRows(rows))
	}
	if got := requireContiguousDeltas(t, fx.CallID, rows); got != strings.Join(chunks, "") {
		t.Errorf("the replayed deltas concatenate to %q, wanted every chunk", got)
	}
}

// TestAnUnknownRunIsARefusedOpen asserts the endpoint's own convention: a run
// the store holds no row for is refused at the transport, never answered with an
// empty stream that reads as "the run produced nothing".
func TestAnUnknownRunIsARefusedOpen(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)

	// Act.
	_, ok := watchBashRun(ctx, t, storeClient(fake.Socket), "toolu_no_such_run")

	// Assert.
	if ok {
		t.Fatal("a run with no stored row must be a refused open, not an empty stream")
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

	// Assert: read the run through to its terminal row.
	rows := awaitBashRunTerminal(ctx, t, storeClient(fake.Socket), fx.CallID)
	requireBashReplayOrder(t, fx.CallID, rows)
	settled := rows[len(rows)-1].GetSuccess()
	if settled == nil {
		t.Fatalf("the EXIT marker produced no terminal row for run %q: %v", fx.CallID, describeBashRows(rows))
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
	rows := awaitBashRunTerminal(ctx, t, storeClient(fake.Socket), fx.CallID)
	whole := requireContiguousDeltas(t, fx.CallID, rows)
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
		if prefix := "bash:" + fx.CallID + ":"; !strings.HasPrefix(e.GetUpsertKey(), prefix) {
			t.Errorf("bash frame keyed %q, wanted a row of %q", e.GetUpsertKey(), prefix)
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
	captured := loadCapturedSession(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/detached-stop-probe",
		"dddddddd-dddd-4ddd-8ddd-dddddddddddd")

	stop := decodeRecord(t, corpusLine(t, "tool-results/task_stop.jsonl", 0))
	stop = retargetSession(t, stop, fx.Session, "/Users/dodgecoates/detached-stop-probe")
	stop = setNested(t, stop, "toolUseResult", "task_id", fx.TaskID)
	stop = setNested(t, stop, "toolUseResult", "task_type", "local_bash")
	// THE CALL COMES FIRST, as the vendor writes it. An exempt tool's call is
	// DROPPED but still remembered, because this one result has to find the call
	// it belongs to; a fixture that supplies only the result is a transcript no
	// vendor ever wrote, and it exercises the orphan path instead of the
	// carve-out.
	stopCall := retargetSession(t, decodeRecord(t, captured.Lines[8]), fx.Session, "/Users/dodgecoates/detached-stop-probe")
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
	if got := interrupted.GetOutput().GetText().GetStdout(); !strings.Contains(got, "partial work") {
		t.Errorf("the cancelled terminal carries stdout %q, wanted the output the spool held", got)
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
