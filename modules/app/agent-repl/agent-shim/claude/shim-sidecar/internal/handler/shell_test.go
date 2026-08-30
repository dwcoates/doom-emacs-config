package handler

// shell_test.go — the detached shell spool: deltas, the one structured byte it
// carries, and the LOST terminal the reader asks for.

import (
	"testing"

	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// spoolContext names the two identities a spool carries SEPARATELY, because
// the whole contract here is that they are not interchangeable: `task` is the
// vendor's runtime bookkeeping id, `run` is the spawning call's tool_use_id and
// the only thing a bash frame may be keyed by.
func spoolContext(path, task, run string) *Context {
	return &Context{
		Path: path, SessionID: "s", MainAgentID: "s", AgentID: "s",
		TaskID: task, RunActivityID: run, Kind: tail.KindShellSpool, FileID: "dev:7",
	}
}

func spoolFrames(text string, offset int64) []tail.Frame {
	return []tail.Frame{{Raw: []byte(text), Offset: offset}}
}

func TestSpoolBytesBecomeADeltaCarryingTheirStartOffset(t *testing.T) {
	// Arrange. from_offset is a GAP DETECTOR: it must equal the bytes the
	// consumer already holds, so a hole is refused rather than concatenated over.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("second chunk", 512), spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	delta := entryByKey(t, entries, convert.BashDeltaKey("toolu_run1", 512))
	update := delta.GetAgentUpdate().GetBash().GetFrame().GetUpdate()
	if update == nil {
		t.Fatal("spool bytes must land on the bash update arm")
	}
	if got := update.GetNewOutput(); got != "second chunk" {
		t.Fatalf("new_output = %q, want the appended bytes verbatim", got)
	}
	if got := update.GetFromOffset(); got != 512 {
		t.Fatalf("from_offset = %d, want 512", got)
	}
}

func TestSpoolDeltaNamesTheRunAndIsNotPaginatable(t *testing.T) {
	// Arrange.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("output", 0), spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	delta := entryByKey(t, entries, convert.BashDeltaKey("toolu_run1", 0))
	if got := delta.GetAgentUpdate().GetBash().GetRun().GetValue(); got != "toolu_run1" {
		t.Fatalf("run = %q, want the spawning call's unit id", got)
	}
	if delta.GetAgentUpdate().GetServeableFrame() != nil {
		t.Fatal("a run frame must not be a page line: the spawning CALL is already one")
	}
}

func TestExitMarkerEndsTheRunAsCompleted(t *testing.T) {
	// Arrange. A NON-ZERO EXIT IS STILL COMPLETED: the command ran, and the code
	// is its own verdict on itself, not a failure of the call.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("boom\nEXIT=17\n", 0), spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	var completed bool
	for _, e := range entries {
		success := e.GetAgentUpdate().GetBash().GetFrame().GetSuccess()
		if success == nil {
			continue
		}
		completed = true
		if got := success.GetCompleted().GetTermination().GetExited().GetCode(); got != 17 {
			t.Fatalf("exit code = %d, want 17", got)
		}
		if success.GetInterrupted() != nil {
			t.Fatal("a command that exited must not be reported as interrupted")
		}
	}
	if !completed {
		t.Fatalf("the EXIT marker did not end the run: keys=%v", allKeys(entries))
	}
}

func TestMidLineExitIsNotReadAsTheMarker(t *testing.T) {
	// Arrange. `EXIT=` is COMMON as ordinary output — 23 of 44 real spools
	// carrying it have it only mid-line — and a loose match would end those runs
	// early and wrongly.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("BUILD_EXIT=0\n", 0), spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	for _, e := range entries {
		if e.GetAgentUpdate().GetBash().GetFrame().GetSuccess() != nil {
			t.Fatal("mid-line EXIT= must not terminate the run")
		}
	}
}

func TestNonNumericExitIsNotReadAsTheMarker(t *testing.T) {
	// Arrange.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("EXIT=abc\n", 0), spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	for _, e := range entries {
		if e.GetAgentUpdate().GetBash().GetFrame().GetSuccess() != nil {
			t.Fatal("a non-numeric EXIT= must not terminate the run")
		}
	}
}

func TestUnownedSpoolBytesLandAsResidueRatherThanBeingDropped(t *testing.T) {
	// Arrange. A spool with no spawning call names no run — but its bytes are
	// NEVER discarded; the aged-unowned-spool policy requires them to be ingested
	// attributed to the residue path.
	h := NewShellOutputHandler(testLogger(t))
	ctx := spoolContext("/t/orphan.output", "bbkq1", "")

	// Act.
	entries := h.Handle(spoolFrames("orphaned bytes", 0), ctx)

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1: an unowned spool's bytes must still be ingested", len(entries))
	}
	v := entries[0].GetAgentUpdate().GetUnservedItem().GetVendorSpecific()
	if v == nil || v.GetKind() != "unowned_spool" {
		t.Fatalf("kind = %v, want unowned_spool", v.GetKind())
	}
}

func TestLostTerminalResolvesInterruptedWithNoCause(t *testing.T) {
	// Arrange. LOST means "we stopped seeing it", not "known failed". The wire
	// carries no DetachedLost, and by_user/timed_out would be accusations with no
	// evidence — so the cause arm must stay UNSET.
	h := NewShellOutputHandler(testLogger(t))
	// The handler must have READ the spool before its terminal can be stated:
	// the terminal owes the run's output, and its write identity is digested
	// from the file coordinates this batch establishes (R-S1).
	h.Handle(spoolFrames("some output\n", 0), spoolContext("/private/tmp/b1.output", "b1", "toolu_run"))

	// Act.
	entries := h.LostTerminal("b1", "toolu_run", "owner-agent", string(convert.LostWentSilent))

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	interrupted := entries[0].GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetInterrupted()
	if interrupted == nil {
		t.Fatal("a LOST run must resolve on the interrupted arm")
	}
	if interrupted.GetByUser() != nil || interrupted.GetTimedOut() != nil {
		t.Fatal("a LOST run must state NO cause: neither a user stop nor a timeout was observed")
	}
	if got := entries[0].GetUpsertKey(); got != convert.BashTerminalKey("toolu_run") {
		t.Fatalf("upsert_key = %q, want the run's bash key so it upserts the run's own row", got)
	}
}

func TestLostTerminalIsRefusedWhenNothingNamesTheRun(t *testing.T) {
	// Arrange. A terminal keyed on an invented identity would upsert no real row.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.LostTerminal("", "", "owner", "went_silent")

	// Assert.
	if entries != nil {
		t.Fatalf("entries = %v, want nil: a run with no identity must be refused, not keyed on a guess", allKeys(entries))
	}
}

func TestTaskObserverReceivesALaunchReadOffAToolResult(t *testing.T) {
	// Arrange. The launch result is the ONLY place the vendor states which call
	// opened which spool, and only the conversion side reads tool results.
	h := NewSessionTranscriptHandler(testLogger(t))
	type spawn struct{ task, call, agent, output string }
	var seen []spawn
	h.SetTaskObserver(func(taskID, toolUseID, agentID, outputPath string) {
		seen = append(seen, spawn{taskID, toolUseID, agentID, outputPath})
	})
	lines := `{"type":"assistant","uuid":"a1","isSidechain":false,"timestamp":"2026-07-21T15:36:10.000Z","message":{"id":"m1","role":"assistant","content":[{"type":"tool_use","id":"toolu_spawn","name":"Agent","input":{"description":"d","prompt":"p"}}]}}
{"type":"user","uuid":"u1","isSidechain":false,"timestamp":"2026-07-21T15:36:13.295Z","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"toolu_spawn","content":[{"type":"text","text":"launched"}]}]},"toolUseResult":{"isAsync":true,"agentId":"a15b5267244c1360e","outputFile":"/tmp/a15.output","description":"d","prompt":"p"}}`

	// Act.
	h.Handle(framesFrom(t, lines), sessionContext("/p/s.jsonl", "s"))

	// Assert.
	if len(seen) != 1 {
		t.Fatalf("observations = %d, want 1", len(seen))
	}
	if seen[0].task != "a15b5267244c1360e" {
		t.Fatalf("task = %q, want the vendor agent id", seen[0].task)
	}
	if seen[0].call != "toolu_spawn" {
		t.Fatalf("call = %q, want the spawning tool_use_id", seen[0].call)
	}
	if seen[0].output != "/tmp/a15.output" {
		t.Fatalf("output = %q, want the spool path the vendor named", seen[0].output)
	}
}

func TestSpoolFramesAreKeyedByTheSpawningCallRatherThanTheVendorTaskId(t *testing.T) {
	// Arrange. A detached command is announced under ONE identity on both planes
	// — the tool_use_id of the call that launched it — so the vendor's task id
	// must never reach the key space.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("output", 0), spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	for _, e := range entries {
		if e.GetUpsertKey() == convert.BashDeltaKey("bbkq1", 0) {
			t.Fatalf("entry keyed by the vendor task id %q; the run is the spawning call", "bbkq1")
		}
	}
	entryByKey(t, entries, convert.BashDeltaKey("toolu_run1", 0))
}

func TestATerminalCarriesTheWholeRunsOutputRatherThanTheLastBatch(t *testing.T) {
	// Arrange. A spool whose EXIT marker arrives on a LATER poll than its output
	// would otherwise settle carrying only the final chunk while claiming to
	// carry the whole — erasing everything the run actually said.
	h := NewShellOutputHandler(testLogger(t))
	ctx := spoolContext("/t/b1.output", "bbkq1", "toolu_run1")

	// Act.
	h.Handle(spoolFrames("first chunk\n", 0), ctx)
	entries := h.Handle(spoolFrames("EXIT=0\n", 12), ctx)

	// Assert.
	var settled bool
	for _, e := range entries {
		success := e.GetAgentUpdate().GetBash().GetFrame().GetSuccess()
		if success == nil {
			continue
		}
		settled = true
		if got := success.GetCompleted().GetOutput().GetText().GetStdout(); got != "first chunk\nEXIT=0\n" {
			t.Fatalf("terminal stdout = %q, want everything the run said", got)
		}
	}
	if !settled {
		t.Fatal("the EXIT marker produced no terminal")
	}
}

func TestALostTerminalCarriesTheOutputTheRunHadProduced(t *testing.T) {
	// Arrange. A LOST run's terminal is the last thing any reader will see of
	// it, so settling it with an empty output would throw away the only account
	// of what it managed to do.
	h := NewShellOutputHandler(testLogger(t))
	ctx := spoolContext("/t/b1.output", "bbkq1", "toolu_run1")
	h.Handle(spoolFrames("all it managed to say\n", 0), ctx)

	// Act.
	entries := h.LostTerminal("bbkq1", "toolu_run1", "owner-agent", "went_silent")

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	got := entries[0].GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetInterrupted().GetOutput().GetText().GetStdout()
	if got != "all it managed to say\n" {
		t.Fatalf("LOST terminal stdout = %q, want the output the run had produced", got)
	}
}

func TestAMarkerOnItsOwnPollIsRecognizedAfterANewlineTerminatedBatch(t *testing.T) {
	// Arrange. The raw spool codec carries nothing, so a batch may begin
	// mid-line and a marker at its very start cannot be trusted on its own. It
	// CAN be trusted when the previous batch ended on a newline — which is the
	// ordinary shape of a command that finishes between two polls.
	h := NewShellOutputHandler(testLogger(t))
	ctx := spoolContext("/t/b1.output", "bbkq1", "toolu_run1")
	h.Handle(spoolFrames("first chunk\n", 0), ctx)

	// Act.
	entries := h.Handle(spoolFrames("EXIT=0\n", 12), ctx)

	// Assert.
	var settled bool
	for _, e := range entries {
		if e.GetAgentUpdate().GetBash().GetFrame().GetSuccess() != nil {
			settled = true
		}
	}
	if !settled {
		t.Fatal("a marker arriving on its own poll after a newline-terminated batch must end the run")
	}
}

func TestAMarkerOpeningAMidLineBatchIsNotTrusted(t *testing.T) {
	// Arrange. `EXIT=` is common as ordinary output, and a batch that continues
	// an unterminated line may be carrying its tail — so the marker is refused
	// and the staleness policy owns the outcome, exactly as before.
	h := NewShellOutputHandler(testLogger(t))
	ctx := spoolContext("/t/b1.output", "bbkq1", "toolu_run1")
	h.Handle(spoolFrames("BUILD_", 0), ctx)

	// Act.
	entries := h.Handle(spoolFrames("EXIT=0\n", 6), ctx)

	// Assert.
	for _, e := range entries {
		if e.GetAgentUpdate().GetBash().GetFrame().GetSuccess() != nil {
			t.Fatal("a marker continuing an unterminated line must not end the run")
		}
	}
}
