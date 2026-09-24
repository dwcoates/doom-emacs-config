package handler

// runoutput_test.go — the accumulator every seam-minted terminal is spelled
// from. The handlers that embed it have their own suites; what is tested here
// is the shared machinery itself.

import (
	"bytes"
	"encoding/json"
	"strings"
	"testing"

	"agentrepl/shim-claude-sidecar/internal/logging"
)

// capturingLogger is testLogger with the file sink kept, so a test can pin the
// LEVEL a record was written at rather than only its text.
func capturingLogger() (*bytes.Buffer, *logging.Bound) {
	// BOTH SINKS ARE SUPPLIED, as testLogger's comment requires, and they are
	// DIFFERENT buffers: one buffer behind both would record every line twice
	// and no "exactly one record" assertion could hold.
	sink := &bytes.Buffer{}
	stderr := &bytes.Buffer{}
	return sink, logging.New(sink, stderr).With(logging.Context{Component: "test"})
}

// levelForMessage returns the level of the ONE captured record whose message
// contains substring, failing unless exactly one does.
func levelForMessage(t *testing.T, sink *bytes.Buffer, substring string) string {
	t.Helper()
	var level string
	count := 0
	for _, line := range strings.Split(strings.TrimSpace(sink.String()), "\n") {
		if line == "" {
			continue
		}
		var rec map[string]any
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("captured log line is not JSON: %v: %q", err, line)
		}
		if msg, _ := rec["message"].(string); strings.Contains(msg, substring) {
			level, _ = rec["level"].(string)
			count++
		}
	}
	if count != 1 {
		t.Fatalf("message mentioning %q: got %d records, want exactly 1; log:\n%s", substring, count, sink.String())
	}
	return level
}

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

func TestRunOutputKeepsTheTailTheRendererShows(t *testing.T) {
	// Arrange. The daemon draws a shell's LAST 16 KiB, so the run's most recent
	// bytes are what a terminal keeps — across batches, not only within one.
	r := NewRunOutput(testLogger(t))
	ctx := runOutputCtx("/tmp/b1.output", "1:2", 0)
	r.Remember(ctx, []byte(strings.Repeat("a", maxRememberedOutput)))

	// Act.
	r.Remember(ctx, []byte("the latest line\n"))

	// Assert.
	seen, omitted := r.Seen()
	if !strings.HasSuffix(seen, "the latest line\n") || len(seen) != maxRememberedOutput {
		t.Fatalf("seen = %d bytes ending %q, want the bound held on the run's tail", len(seen), seen[len(seen)-16:])
	}
	if omitted != uint64(len("the latest line\n")) {
		t.Fatalf("omitted = %d, want the earlier bytes the tail dropped", omitted)
	}
}

func TestRunOutputStatesTheBoundOnlyOnTheBatchThatCrossesIt(t *testing.T) {
	// Arrange. Every later batch of a talkative run drops more of its head; the
	// crossing is the one fact worth a record.
	sink, log := capturingLogger()
	r := NewRunOutput(log)
	ctx := runOutputCtx("/tmp/b1.output", "1:2", 0)
	r.Remember(ctx, []byte(strings.Repeat("x", maxRememberedOutput+1)))

	// Act.
	r.Remember(ctx, []byte("more\n"))

	// Assert.
	if got := strings.Count(sink.String(), "the run has said more than"); got != 1 {
		t.Fatalf("the bound was recorded %d times, want once; log:\n%s", got, sink.String())
	}
}

func TestRunOutputRecordsTheBoundAtInfo(t *testing.T) {
	// Arrange. A run saying more than the bound is ordinary — three did it in
	// one session on 2026-09-13 — and the terminal carries the omitted count,
	// so the reader is told rather than misled. Held at warn it was a defect
	// report for something that is not a defect.
	sink, log := capturingLogger()
	r := NewRunOutput(log)

	// Act.
	r.Remember(runOutputCtx("/tmp/b1.output", "1:2", 0), []byte(strings.Repeat("x", maxRememberedOutput+64)))

	// Assert.
	if got := levelForMessage(t, sink, "the run has said more than"); got != "info" {
		t.Fatalf("the run-output bound was recorded at %q, want info", got)
	}
}

func TestRunOutputStatesBothCountsWhenItPassesTheBound(t *testing.T) {
	// Arrange. "How much was kept, how much was dropped" is the whole question
	// the record answers.
	sink, log := capturingLogger()
	r := NewRunOutput(log)

	// Act.
	r.Remember(runOutputCtx("/tmp/b1.output", "1:2", 0), []byte(strings.Repeat("x", maxRememberedOutput+64)))

	// Assert.
	logged := sink.String()
	if !strings.Contains(logged, "16384") {
		t.Errorf("the run-output bound record does not state the bound it held; log:\n%s", logged)
	}
	if !strings.Contains(logged, "64 omitted") {
		t.Errorf("the run-output bound record does not state what it omitted; log:\n%s", logged)
	}
}

func TestRunOutputSaysNothingWhileItIsInsideTheBound(t *testing.T) {
	// Arrange. The record belongs to the ONE batch that crosses the bound; a
	// run that stays inside it is not worth a line.
	sink, log := capturingLogger()
	r := NewRunOutput(log)

	// Act.
	r.Remember(runOutputCtx("/tmp/b1.output", "1:2", 0), []byte("a short run\n"))

	// Assert.
	if strings.Contains(sink.String(), "the run has said more than") {
		t.Fatalf("a run inside the bound recorded the bound; log:\n%s", sink.String())
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
