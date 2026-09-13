package handler

// runoutput_test.go — the accumulator every seam-minted terminal is spelled
// from. The handlers that embed it have their own suites; what is tested here
// is the shared machinery itself.

import (
	"strings"
	"testing"
)

func runOutputCtx(path, fileID string, observed int64) *Context {
	return &Context{Path: path, FileID: fileID, BytesObserved: observed}
}

func TestRunOutputStartsUnread(t *testing.T) {
	// Arrange, Act: nothing has been accumulated.
	r := NewRunOutput(testLogger(t))

	// Assert: a terminal minted now must state not_observed, never claim it
	// carries a run's whole output.
	if r.Read() {
		t.Fatal("a run output that has accumulated nothing reports itself as read")
	}
}

func TestRunOutputRemembersTheBytesVerbatim(t *testing.T) {
	// Arrange.
	r := NewRunOutput(testLogger(t))

	// Act: two batches, as two polls would deliver them.
	r.Remember(runOutputCtx("/tmp/b1.output", "1:2", 6), []byte("one\n"))
	r.Remember(runOutputCtx("/tmp/b1.output", "1:2", 12), []byte("two\n"))

	// Assert: the terminal states the RUN's output, not the last batch's.
	seen, omitted := r.Seen()
	if seen != "one\ntwo\n" || omitted != 0 {
		t.Fatalf("seen = %q omitted = %d, want both batches whole and nothing omitted", seen, omitted)
	}
}

func TestRunOutputReportsWhatItOmittedPastTheBound(t *testing.T) {
	// Arrange. Something must be held for the terminal to carry, so the cost is
	// bounded — and past the bound the extent is STATED rather than the terminal
	// misreporting a truncated output as whole.
	r := NewRunOutput(testLogger(t))

	// Act.
	r.Remember(runOutputCtx("/tmp/b1.output", "1:2", 0), []byte(strings.Repeat("x", maxRememberedOutput+64)))

	// Assert.
	seen, omitted := r.Seen()
	if len(seen) != maxRememberedOutput || omitted != 64 {
		t.Fatalf("seen = %d bytes omitted = %d, want the bound held and the rest counted", len(seen), omitted)
	}
}

func TestRunOutputAttributionFallsBackToTheRunScopeBeforeAnyRead(t *testing.T) {
	// Arrange. A handler asked for a terminal before it read a batch has no file
	// coordinates, and an attribution with neither those nor a run scope digests
	// ONE write id for every such terminal in the process.
	r := NewRunOutput(testLogger(t))

	// Act.
	at := r.TerminalAttribution("b1", "agent-1", "toolu_run")

	// Assert.
	if at.WriteScope == "" {
		t.Fatalf("attribution = %+v, want the run scope standing in for the file coordinates", at)
	}
}

func TestRunOutputAttributionIsTheFileItLastRead(t *testing.T) {
	// Arrange.
	r := NewRunOutput(testLogger(t))
	r.RememberCoords(runOutputCtx("/tmp/b1.output", "16777232:777", 128))

	// Act.
	at := r.TerminalAttribution("b1", "agent-1", "toolu_run")

	// Assert: the terminal is stated at a real position in a real file.
	if at.FileID != "16777232:777" || at.Offset != 128 || at.Path != "/tmp/b1.output" {
		t.Fatalf("attribution = %+v, want the coordinates of the last batch read", at)
	}
}

func TestRunOutputCancelledRefusesWithoutASpawningCall(t *testing.T) {
	// Arrange. A terminal keyed on nothing upserts no row.
	r := NewRunOutput(testLogger(t))

	// Act.
	got := r.Cancelled("b1", "", "agent-1", 1700000000000)

	// Assert.
	if got != nil {
		t.Fatalf("entries = %v, want the terminal refused rather than keyed on a guess", got)
	}
}

func TestRunOutputLostRefusesWithoutASpawningCall(t *testing.T) {
	// Arrange. Same rule as the stop: the vendor task id is not a row a reader
	// of the conversation can join to the call.
	r := NewRunOutput(testLogger(t))

	// Act.
	got := r.Lost("b1", "", "agent-1", "went_silent", false)

	// Assert.
	if got != nil {
		t.Fatalf("entries = %v, want the terminal refused rather than keyed on a guess", got)
	}
}

func TestRunOutputCancelledStatesThePersonsDecision(t *testing.T) {
	// Arrange.
	r := NewRunOutput(testLogger(t))
	r.RememberCoords(runOutputCtx("/tmp/b1.output", "16777232:777", 4))
	r.Remember(runOutputCtx("/tmp/b1.output", "16777232:777", 4), []byte("out\n"))

	// Act.
	got := r.Cancelled("b1", "toolu_run", "agent-1", 1700000000000)

	// Assert.
	cut := got[0].GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetInterrupted()
	if cut.GetByUser() == nil {
		t.Fatalf("cause = %v, want by_user rather than an accusation with no evidence", cut.GetCause())
	}
}

func TestRunOutputLostStatesHowItStoppedBeingSeen(t *testing.T) {
	// Arrange.
	r := NewRunOutput(testLogger(t))
	r.RememberCoords(runOutputCtx("/tmp/b1.output", "16777232:777", 4))

	// Act.
	got := r.Lost("b1", "toolu_run", "agent-1", "went_silent", false)

	// Assert: the arm is on the wire, not only in this process's log.
	cut := got[0].GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetInterrupted()
	if cut.GetLost().GetWentSilent() == nil {
		t.Fatalf("cause = %v, want the went_silent arm the reader concluded", cut.GetCause())
	}
}
