package handler

// shell_test.go — the detached shell spool: its rendered tail, the three terminators it
// can carry, and the LOST terminal the reader asks for.

import (
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
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

// prefixOf answers a readPrefix that serves the first bytes of file, so a
// batch that does not continue the window reseeds from a known spool without
// touching the filesystem.
func prefixOf(file string) func(string, int64, func([]byte)) error {
	return func(_ string, upTo int64, sink func([]byte)) error {
		sink([]byte(file[:upTo]))
		return nil
	}
}

func TestSpoolBytesBecomeTheRunsTail(t *testing.T) {
	// Arrange. Output past what is rendered is not stored, so a batch lands as
	// the run's one tail row, carrying the window as it is drawn.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("first chunk\n", 0), spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	got := entryByKey(t, entries, convert.BashTailKey("toolu_run1")).GetAgentUpdate().GetBash().GetFrame().GetTail()
	if got == nil {
		t.Fatal("spool bytes must land on the bash tail arm")
	}
	if got.GetText() != "first chunk\n" || got.GetBytesOmitted() != 0 || got.GetLinesOmitted() != 0 {
		t.Fatalf("tail = {%q, %d, %d}, want the whole output and nothing omitted", got.GetText(), got.GetBytesOmitted(), got.GetLinesOmitted())
	}
}

func TestALaterBatchSupersedesTheTailWithTheWholeWindow(t *testing.T) {
	// Arrange. The tail is a snapshot of the run, not of the batch: the second
	// batch's row states everything drawn so far.
	h := NewShellOutputHandler(testLogger(t))
	ctx := spoolContext("/t/b1.output", "bbkq1", "toolu_run1")
	h.Handle(spoolFrames("first\n", 0), ctx)

	// Act.
	entries := h.Handle(spoolFrames("second\n", 6), ctx)

	// Assert.
	got := entryByKey(t, entries, convert.BashTailKey("toolu_run1")).GetAgentUpdate().GetBash().GetFrame().GetTail()
	if got.GetText() != "first\nsecond\n" {
		t.Fatalf("tail text = %q, want the run's whole window", got.GetText())
	}
}

func TestTheTailIsCappedAtTheRenderersBoundAndCountsWhatItOmits(t *testing.T) {
	// Arrange. A run far past the cap: the row holds at most the cap and states
	// how many bytes and lines came before it.
	h := NewShellOutputHandler(testLogger(t))
	line := strings.Repeat("y", 99) + "\n"
	spool := strings.Repeat(line, 1000)

	// Act.
	entries := h.Handle(spoolFrames(spool, 0), spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	got := entryByKey(t, entries, convert.BashTailKey("toolu_run1")).GetAgentUpdate().GetBash().GetFrame().GetTail()
	if len(got.GetText()) > maxRememberedOutput {
		t.Fatalf("tail text = %d bytes, want at most the cap %d", len(got.GetText()), maxRememberedOutput)
	}
	if got.GetBytesOmitted()+uint64(len(got.GetText())) != uint64(len(spool)) {
		t.Fatalf("bytes_omitted %d + text %d != the %d bytes written", got.GetBytesOmitted(), len(got.GetText()), len(spool))
	}
	if want := uint64(1000 - strings.Count(got.GetText(), "\n")); got.GetLinesOmitted() != want {
		t.Fatalf("lines_omitted = %d, want %d", got.GetLinesOmitted(), want)
	}
}

func TestAResumedSpoolReseedsItsTailFromTheFile(t *testing.T) {
	// Arrange. A restarted sidecar resumes mid-file; the tail it writes must
	// supersede the stored one with the SAME window, not with only the bytes
	// read since the restart.
	file := "before the restart\nafter\n"
	h := NewShellOutputHandler(testLogger(t))
	h.readPrefix = prefixOf(file)

	// Act.
	entries := h.Handle(spoolFrames("after\n", 19), spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	got := entryByKey(t, entries, convert.BashTailKey("toolu_run1")).GetAgentUpdate().GetBash().GetFrame().GetTail()
	if got.GetText() != file {
		t.Fatalf("tail text = %q, want the whole file %q", got.GetText(), file)
	}
}

func TestAReReadBatchDoesNotCountItsBytesTwice(t *testing.T) {
	// Arrange. A batch whose write was never acknowledged is polled again from
	// the committed cursor; the window must not hold its bytes twice.
	file := "one\ntwo\n"
	h := NewShellOutputHandler(testLogger(t))
	h.readPrefix = prefixOf(file)
	ctx := spoolContext("/t/b1.output", "bbkq1", "toolu_run1")
	h.Handle(spoolFrames("one\n", 0), ctx)
	h.Handle(spoolFrames("two\n", 4), ctx)

	// Act.
	entries := h.Handle(spoolFrames("two\n", 4), ctx)

	// Assert.
	got := entryByKey(t, entries, convert.BashTailKey("toolu_run1")).GetAgentUpdate().GetBash().GetFrame().GetTail()
	if got.GetText() != file {
		t.Fatalf("tail text = %q, want %q exactly once", got.GetText(), file)
	}
}

func TestATailThatCannotBeReseededIsWithheldAndLoggedAsAnError(t *testing.T) {
	// Arrange. A window missing the file's prefix would state wrong omitted
	// counts for the rest of the run, so it is withheld rather than guessed.
	sink, log := capturingLogger()
	h := NewShellOutputHandler(log)
	h.readPrefix = func(string, int64, func([]byte)) error { return errors.New("spool unreadable") }

	// Act.
	entries := h.Handle(spoolFrames("after\n", 19), spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	for _, e := range entries {
		if e.GetUpsertKey() == convert.BashTailKey("toolu_run1") {
			t.Fatal("a tail was written from a window missing the file's prefix")
		}
	}
	if got := levelForMessage(t, sink, "could not be rebuilt from its spool"); got != "error" {
		t.Fatalf("the withheld tail was recorded at %q, want error", got)
	}
}

func TestTheBatchAfterAFailedReseedReseedsAgain(t *testing.T) {
	// Arrange. The withheld batch's bytes are in the prefix the next reseed
	// reads, so nothing is lost once the file can be read.
	file := "before\nfailed\nnext\n"
	h := NewShellOutputHandler(testLogger(t))
	ctx := spoolContext("/t/b1.output", "bbkq1", "toolu_run1")
	h.readPrefix = func(string, int64, func([]byte)) error { return errors.New("spool unreadable") }
	h.Handle(spoolFrames("failed\n", 7), ctx)
	h.readPrefix = prefixOf(file)

	// Act.
	entries := h.Handle(spoolFrames("next\n", 14), ctx)

	// Assert.
	got := entryByKey(t, entries, convert.BashTailKey("toolu_run1")).GetAgentUpdate().GetBash().GetFrame().GetTail()
	if got.GetText() != file {
		t.Fatalf("tail text = %q, want the whole file %q", got.GetText(), file)
	}
}

func TestSpoolTailNamesTheRunAndIsNotPaginatable(t *testing.T) {
	// Arrange.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("output", 0), spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	row := entryByKey(t, entries, convert.BashTailKey("toolu_run1"))
	if got := row.GetAgentUpdate().GetBash().GetRun().GetValue(); got != "toolu_run1" {
		t.Fatalf("run = %q, want the spawning call's unit id", got)
	}
	if row.GetAgentUpdate().GetServeableFrame() != nil {
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
	entries := h.LostTerminal("b1", "toolu_run", "owner-agent", string(convert.LostWentSilent), false)

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
	entries := h.LostTerminal("", "", "owner", "went_silent", false)

	// Assert.
	if entries != nil {
		t.Fatalf("entries = %v, want nil: a run with no identity must be refused, not keyed on a guess", allKeys(entries))
	}
}

func TestTaskObserverReceivesALaunchReadOffAToolResult(t *testing.T) {
	// Arrange. The launch result is the ONLY place the vendor states which call
	// opened which spool, and only the conversion side reads tool results.
	h := NewSessionTranscriptHandler(testLogger(t))
	type spawn struct{ task, call, agent, output, workspaceDir, workspaceID, claudeSessionID string }
	var seen []spawn
	h.SetTaskObserver(func(taskID, toolUseID, agentID, outputPath string, backgrounded bool, workspaceDir, workspaceID, claudeSessionID string) {
		seen = append(seen, spawn{taskID, toolUseID, agentID, outputPath, workspaceDir, workspaceID, claudeSessionID})
	})
	lines := `{"type":"assistant","uuid":"a1","isSidechain":false,"timestamp":"2026-07-21T15:36:10.000Z","message":{"id":"m1","role":"assistant","content":[{"type":"tool_use","id":"toolu_spawn","name":"Agent","input":{"description":"d","prompt":"p"}}]}}
{"type":"user","uuid":"u1","isSidechain":false,"timestamp":"2026-07-21T15:36:13.295Z","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"toolu_spawn","content":[{"type":"text","text":"launched"}]}]},"toolUseResult":{"isAsync":true,"agentId":"a15b5267244c1360e","outputFile":"/tmp/a15.output","description":"d","prompt":"p"}}`

	// Act.
	ctx := sessionContext("/p/s.jsonl", "s")
	ctx.WorkspaceDir = "/workspace"
	ctx.WorkspaceID = "workspace-id"
	ctx.ClaudeSessionID = "session-1"
	h.Handle(framesFrom(t, lines), ctx)

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
	if seen[0].workspaceDir != "/workspace" || seen[0].workspaceID != "workspace-id" || seen[0].claudeSessionID != "session-1" {
		t.Fatalf("workspace attribution = %+v, want the handler's file scope", seen[0])
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
		if e.GetUpsertKey() == convert.BashTailKey("bbkq1") {
			t.Fatalf("entry keyed by the vendor task id %q; the run is the spawning call", "bbkq1")
		}
	}
	entryByKey(t, entries, convert.BashTailKey("toolu_run1"))
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
	entries := h.LostTerminal("bbkq1", "toolu_run1", "owner-agent", "went_silent", false)

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

// ---- a terminal for a spool this handler never read ----

func TestATerminalForANeverReadSpoolIsStillMinted(t *testing.T) {
	// Arrange. A REFUSED TERMINAL IS A RUN LEFT OPEN FOREVER in every reader
	// downstream, which is strictly worse than a terminal that honestly says it
	// saw nothing — and refusing is what made the swept_up conclusion, whose
	// whole premise is a spool nobody ever read, unstatable.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.LostTerminal("b1", "toolu_run", "owner-agent", string(convert.LostSweptUp), false)

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1: a run concluded LOST owes a terminal even when its spool was never read", len(entries))
	}
}

func TestATerminalForANeverReadSpoolStatesNotObserved(t *testing.T) {
	// Arrange. The handler holds no bytes, so it may not claim the command
	// printed nothing.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.LostTerminal("b1", "toolu_run", "owner-agent", string(convert.LostSweptUp), false)

	// Assert.
	output := entries[0].GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetInterrupted().GetOutput()
	if output.GetNotObserved() == nil {
		t.Fatalf("output = %v, want not_observed for a spool this handler never read", output)
	}
}

func TestTwoNeverReadSpoolsMintDistinctTerminalWriteIds(t *testing.T) {
	// Arrange. Both handlers have read nothing, so neither has a file position;
	// their terminals are identified by their RUNS, or the store absorbs the
	// second as a replay of the first and one run loses its terminal entirely.
	first := NewShellOutputHandler(testLogger(t))
	second := NewShellOutputHandler(testLogger(t))

	// Act.
	a := first.LostTerminal("b1", "toolu_run_one", "owner-agent", string(convert.LostSweptUp), false)
	b := second.LostTerminal("b2", "toolu_run_two", "owner-agent", string(convert.LostSweptUp), false)

	// Assert.
	if a[0].GetWriteId() == b[0].GetWriteId() {
		t.Fatalf("two never-read runs minted the same terminal write id %q", a[0].GetWriteId())
	}
}

func TestATerminalForAReadSpoolIsIdentifiedByTheFileItWasReadFrom(t *testing.T) {
	// Arrange. A handler that DID read states the terminal at those coordinates,
	// which are the same ones the cursor is stated in — so a re-read after a
	// restart mints the identical id and the store absorbs the replay.
	h := NewShellOutputHandler(testLogger(t))
	ctx := spoolContext("/private/tmp/b1.output", "b1", "toolu_run")
	h.Handle(spoolFrames("some output\n", 0), ctx)

	// Act.
	entries := h.LostTerminal("b1", "toolu_run", "owner-agent", string(convert.LostWentSilent), false)

	// Assert.
	want := convert.WriteID(convert.Attribution{
		FileID: ctx.FileID, Offset: ctx.BytesObserved,
	}, "bash_terminal")
	if got := entries[0].GetWriteId(); got != want {
		t.Fatalf("write id = %q, want the digest of the coordinates it was read at (%q)", got, want)
	}
}

func TestAFourDigitExitIsNotReadAsTheMarker(t *testing.T) {
	// Arrange. A shell exit code is 0-255, so maxExitMarkerDigits is 3 and a
	// longer run of digits is ORDINARY OUTPUT — a script echoing a build id, a
	// line-start `EXIT=1234`. Reading it as the terminator would end a run early
	// and wrongly, and the run would then never carry what it said afterwards.
	h := NewShellOutputHandler(testLogger(t))

	// Act: the marker-shaped line begins the file, so it genuinely starts a line.
	entries := h.Handle(spoolFrames("EXIT=1234\n", 0), spoolContext("/t/b4.output", "b4", "toolu_run4"))

	// Assert: the bytes land as the tail and NOTHING settles the run.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want exactly the tail: a four-digit EXIT= is output, not the terminator (keys: %v)", len(entries), allKeys(entries))
	}
	if got := entries[0].GetUpsertKey(); got != convert.BashTailKey("toolu_run4") {
		t.Fatalf("upsert_key = %q, want the run's tail key", got)
	}
}

// exitCodeOf returns the code of the completed terminal in entries, and whether
// any terminal was minted at all.
func exitCodeOf(t *testing.T, entries []*storev1.StoreEntry) (int32, bool) {
	t.Helper()
	for _, e := range entries {
		success := e.GetAgentUpdate().GetBash().GetFrame().GetSuccess()
		if success == nil || success.GetCompleted() == nil {
			continue
		}
		return success.GetCompleted().GetTermination().GetExited().GetCode(), true
	}
	return 0, false
}

func TestTheWrapperExitLineEndsTheRunAsCompleted(t *testing.T) {
	// Arrange. The vendor's own background-shell wrapper terminates a spool with
	// `[exited with code N]`, and no harness `EXIT=` line is present at all. This
	// reader used to see nothing there and let a silence window call the run LOST.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("kern.num_files: 11173\n\n[exited with code 0]\n", 0),
		spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	code, ok := exitCodeOf(t, entries)
	if !ok {
		t.Fatalf("the wrapper's exit line did not end the run: keys=%v", allKeys(entries))
	}
	if code != 0 {
		t.Fatalf("exit code = %d, want 0", code)
	}
}

func TestANonZeroWrapperExitIsStillCompleted(t *testing.T) {
	// Arrange. The code is the wrapper's verdict on the run, never a failure of
	// this call, so it takes the completed arm exactly as `EXIT=` does.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("work\n[exited with code 144]\n", 0),
		spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	code, ok := exitCodeOf(t, entries)
	if !ok {
		t.Fatalf("a non-zero wrapper exit did not end the run: keys=%v", allKeys(entries))
	}
	if code != 144 {
		t.Fatalf("exit code = %d, want 144", code)
	}
}

func TestTheHarnessMarkerBeatsTheWrapperLineWhenBothArePresent(t *testing.T) {
	// Arrange. The bracket reports the WRAPPER's exit and reads 0 above a harness
	// `EXIT=77`, because the wrapper ran fine and the command did not. Taking the
	// bracket's code there would report a failed command as a clean one.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("EXIT=77\n\n[exited with code 0]\n", 0),
		spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	code, ok := exitCodeOf(t, entries)
	if !ok {
		t.Fatalf("the run did not end: keys=%v", allKeys(entries))
	}
	if code != 77 {
		t.Fatalf("exit code = %d, want the command's own 77 rather than the wrapper's 0", code)
	}
}

func TestALowercaseExitLineAboveTheWrapperIsNotTheHarnessMarker(t *testing.T) {
	// Arrange. A real spool prints `exit=1` as ordinary script output above the
	// wrapper's line. It is not the harness marker, so the wrapper's own code
	// stands.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("exit=1\n\n[exited with code 0]\n", 0),
		spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	code, ok := exitCodeOf(t, entries)
	if !ok {
		t.Fatalf("the run did not end: keys=%v", allKeys(entries))
	}
	if code != 0 {
		t.Fatalf("exit code = %d, want the wrapper's 0: `exit=1` is output, not the marker", code)
	}
}

func TestAMidLineWrapperSentenceIsNotReadAsTheTerminator(t *testing.T) {
	// Arrange. A run that PRINTS the wrapper's sentence mid-line must not be
	// ended by it, exactly as `BUILD_EXIT=0` does not end one.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("saw [exited with code 0]\n", 0),
		spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	if _, ok := exitCodeOf(t, entries); ok {
		t.Fatalf("a mid-line wrapper sentence must not terminate the run: keys=%v", allKeys(entries))
	}
}

func TestAnUnclosedWrapperLineIsNotReadAsTheTerminator(t *testing.T) {
	// Arrange. Without the closing bracket the line is prose, not the wrapper's
	// terminator.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("[exited with code 0\n", 0),
		spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	if _, ok := exitCodeOf(t, entries); ok {
		t.Fatalf("an unclosed wrapper line must not terminate the run: keys=%v", allKeys(entries))
	}
}

func TestANonNumericWrapperCodeIsNotReadAsTheTerminator(t *testing.T) {
	// Arrange. `[exited with code oops]` is not a code, and guessing one would
	// put a number in the run's mouth.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("[exited with code oops]\n", 0),
		spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	if _, ok := exitCodeOf(t, entries); ok {
		t.Fatalf("a non-numeric wrapper code must not terminate the run: keys=%v", allKeys(entries))
	}
}

func TestAFourDigitWrapperCodeIsNotReadAsTheTerminator(t *testing.T) {
	// Arrange. The digits are bounded for the same reason `EXIT=1234` is refused:
	// past three digits it is not an exit code being reported.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("[exited with code 1234]\n", 0),
		spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	if _, ok := exitCodeOf(t, entries); ok {
		t.Fatalf("a four-digit wrapper code must not terminate the run: keys=%v", allKeys(entries))
	}
}

func TestAWrapperLineOpeningAMidLineBatchIsNotTrusted(t *testing.T) {
	// Arrange. A batch resuming mid-line may be carrying the tail of a line that
	// began in an earlier poll, so a wrapper line at its very start proves
	// nothing.
	h := NewShellOutputHandler(testLogger(t))
	ctx := spoolContext("/t/b1.output", "bbkq1", "toolu_run1")
	h.Handle(spoolFrames("no newline here", 0), ctx)

	// Act.
	entries := h.Handle(spoolFrames("[exited with code 0]\n", 15), ctx)

	// Assert.
	if _, ok := exitCodeOf(t, entries); ok {
		t.Fatalf("a wrapper line opening a mid-line batch must not terminate the run: keys=%v", allKeys(entries))
	}
}

func TestAWrapperLineEndingTheRunTellsTheReaderItCannotBeLost(t *testing.T) {
	// Arrange. A run that ended on its own evidence must be taken out of the
	// staleness policy's hands, or the finished spool going quiet is written up
	// as LOST — which is exactly what realtest 2026-09-13 harvested.
	h := NewShellOutputHandler(testLogger(t))
	var toldPath, toldRun string
	h.onTerminal = func(path, run string) { toldPath, toldRun = path, run }

	// Act.
	h.Handle(spoolFrames("done\n[exited with code 0]\n", 0),
		spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	if toldPath != "/t/b1.output" || toldRun != "toolu_run1" {
		t.Fatalf("onTerminal saw (%q, %q), want the spool and its run", toldPath, toldRun)
	}
}

// terminationOf returns the completed terminal's termination arm, so a test can
// claim which of the two endings the reader spelled.
func terminationOf(t *testing.T, entries []*storev1.StoreEntry) (*conversationv1.AgentBashTermination, bool) {
	t.Helper()
	for _, e := range entries {
		success := e.GetAgentUpdate().GetBash().GetFrame().GetSuccess()
		if success == nil || success.GetCompleted() == nil {
			continue
		}
		return success.GetCompleted().GetTermination(), true
	}
	return nil, false
}

// TestTheWrapperKilledLineEndsTheRunOnTheKilledArm is the third terminator the
// vendor writes and the one this reader did not know. 121 of one machine's
// shell spools end on it, and every one of them was left open for a silence
// window to conclude LOST — `bpth8pp8m.output`, written 16:21:33 and concluded
// `went_silent` at 16:51:47, is the harvested case.
func TestTheWrapperKilledLineEndsTheRunOnTheKilledArm(t *testing.T) {
	// Arrange.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("PACKAGE_COMPILES\n\n[killed]\n", 0),
		spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	termination, ok := terminationOf(t, entries)
	if !ok {
		t.Fatalf("the wrapper's killed line did not end the run: keys=%v", allKeys(entries))
	}
	if termination.GetKilled() == nil {
		t.Fatalf("termination = %v, want the killed arm: a kill reports no status", termination)
	}
}

// TestAKilledRunIsReportedAsATerminalRead pins what stops the LOST policy
// restating a killed run as `went_silent`: the handler must tell the reader it
// READ the run's own terminal, exactly as it does for an exit.
func TestAKilledRunIsReportedAsATerminalRead(t *testing.T) {
	// Arrange.
	h := NewShellOutputHandler(testLogger(t))
	var gotPath, gotRun string
	h.SetTerminalObserver(func(path, run string) { gotPath, gotRun = path, run })

	// Act.
	h.Handle(spoolFrames("[killed]\n", 0), spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	if gotPath != "/t/b1.output" || gotRun != "toolu_run1" {
		t.Fatalf("terminal reported for (%q, %q), want (/t/b1.output, toolu_run1)", gotPath, gotRun)
	}
}

// TestTheHarnessMarkerBeatsTheKilledLineWhenBothArePresent applies the existing
// precedence to the new line: a kill reports no status, so a harness `EXIT=`
// recorded right above it is the only verdict the command gave.
func TestTheHarnessMarkerBeatsTheKilledLineWhenBothArePresent(t *testing.T) {
	// Arrange.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("EXIT=77\n\n[killed]\n", 0),
		spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	code, ok := exitCodeOf(t, entries)
	if !ok {
		t.Fatalf("the run did not end: keys=%v", allKeys(entries))
	}
	if code != 77 {
		t.Fatalf("exit code = %d, want the command's own 77 rather than an unqualified kill", code)
	}
}

// TestAMidLineKilledSentenceIsNotReadAsTheTerminator holds the new line to the
// same strictness as the other two: a run that PRINTS the word must not end.
func TestAMidLineKilledSentenceIsNotReadAsTheTerminator(t *testing.T) {
	// Arrange.
	h := NewShellOutputHandler(testLogger(t))

	// Act.
	entries := h.Handle(spoolFrames("the child said [killed] and carried on\n", 0),
		spoolContext("/t/b1.output", "bbkq1", "toolu_run1"))

	// Assert.
	if _, ok := terminationOf(t, entries); ok {
		t.Fatalf("a mid-line killed sentence must not terminate the run: keys=%v", allKeys(entries))
	}
}
