package handler

// shell_test.go — the detached shell spool: deltas, the one structured byte it
// carries, and the LOST terminal the reader asks for.

import (
	"testing"

	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

func spoolContext(path, task string) *Context {
	return &Context{
		Path: path, SessionID: "s", MainAgentID: "s", AgentID: "s",
		TaskID: task, Kind: tail.KindShellSpool, FileID: "dev:7",
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
	entries := h.Handle(spoolFrames("second chunk", 512), spoolContext("/t/b1.output", "b1"))

	// Assert.
	delta := entryByKey(t, entries, convert.BashKey("b1"))
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
	entries := h.Handle(spoolFrames("output", 0), spoolContext("/t/b1.output", "b1"))

	// Assert.
	delta := entryByKey(t, entries, convert.BashKey("b1"))
	if got := delta.GetAgentUpdate().GetBash().GetRun().GetValue(); got != "b1" {
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
	entries := h.Handle(spoolFrames("boom\nEXIT=17\n", 0), spoolContext("/t/b1.output", "b1"))

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
	entries := h.Handle(spoolFrames("BUILD_EXIT=0\n", 0), spoolContext("/t/b1.output", "b1"))

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
	entries := h.Handle(spoolFrames("EXIT=abc\n", 0), spoolContext("/t/b1.output", "b1"))

	// Assert.
	for _, e := range entries {
		if e.GetAgentUpdate().GetBash().GetFrame().GetSuccess() != nil {
			t.Fatal("a non-numeric EXIT= must not terminate the run")
		}
	}
}

func TestUnownedSpoolBytesLandAsResidueRatherThanBeingDropped(t *testing.T) {
	// Arrange. A spool with no task identity names no run — but its bytes are
	// NEVER discarded; the aged-unowned-spool policy requires them to be ingested
	// attributed to the residue path.
	h := NewShellOutputHandler(testLogger(t))
	ctx := spoolContext("/t/orphan.output", "")

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
	if got := entries[0].GetUpsertKey(); got != convert.BashKey("toolu_run") {
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
