package handler

// runoutput_test.go — the accumulator every seam-minted terminal is spelled
// from. The handlers that embed it have their own suites; what is tested here
// is the shared machinery itself.

import (
	"bytes"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
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

// ---- the rendered tail ----

func TestTheWindowIsBoundedByTheContractsCap(t *testing.T) {
	// Arrange. The writer and the renderer read ONE number, the contract's; a
	// local copy of 16 KiB here is how the two would drift apart.
	want := int(conversationv1.AgentBashTailCap_AGENT_BASH_TAIL_CAP_BYTES)

	// Act.
	got := maxRememberedOutput

	// Assert.
	if got != want {
		t.Fatalf("maxRememberedOutput = %d, want the contract's AGENT_BASH_TAIL_CAP_BYTES %d", got, want)
	}
}

func TestRenderedCutsTheWindowAsTheRendererDraws(t *testing.T) {
	// Arrange. One case per shape of cut: nothing omitted, a window holding a
	// line break, one holding none after a mid-line cut, one holding none
	// after a cut on a line end, a window opening mid-character, and a window
	// whose only line break is its last byte.
	limit := maxRememberedOutput
	cases := []struct {
		name      string
		output    string
		wantText  string
		wantBytes uint64
		wantLines uint64
	}{
		{name: "inside the cap", output: "a\nb\n", wantText: "a\nb\n"},
		{
			name:      "a line break in the window",
			output:    strings.Repeat("x", 10) + "\n" + strings.Repeat("z", limit-6) + "\nlast\n",
			wantText:  "last\n",
			wantBytes: uint64(10 + 1 + limit - 6 + 1),
			wantLines: 2,
		},
		{
			name:      "no line break after a mid-line cut",
			output:    "one\ntwo" + strings.Repeat("z", limit),
			wantText:  strings.Repeat("z", limit),
			wantBytes: 7,
			wantLines: 2,
		},
		{
			name:      "no line break after a cut on a line end",
			output:    "one\ntwo\n" + strings.Repeat("z", limit),
			wantText:  strings.Repeat("z", limit),
			wantBytes: 8,
			wantLines: 2,
		},
		{
			name:      "a window opening mid-character",
			output:    "é" + strings.Repeat("z", limit-1),
			wantText:  strings.Repeat("z", limit-1),
			wantBytes: 2,
			wantLines: 1,
		},
		{
			name:      "the only line break is the last byte",
			output:    strings.Repeat("a", limit) + "tail\n",
			wantText:  "",
			wantBytes: uint64(limit + 5),
			wantLines: 1,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			r := NewRunOutput(testLogger(t))
			r.Remember(runOutputCtx("/tmp/b1.output", "1:2", 0), []byte(tc.output))

			// Act.
			text, bytesOmitted, linesOmitted := r.Rendered()

			// Assert.
			if text != tc.wantText || bytesOmitted != tc.wantBytes || linesOmitted != tc.wantLines {
				t.Fatalf("rendered = {%d bytes, %d, %d}, want {%d bytes, %d, %d}",
					len(text), bytesOmitted, linesOmitted, len(tc.wantText), tc.wantBytes, tc.wantLines)
			}
		})
	}
}

// capSpoolOracle is the daemon's retired capSpool over a WHOLE spool, kept as
// the oracle the window must agree with: the window never holds the whole
// spool, and it still has to draw exactly what the whole spool drew.
func capSpoolOracle(spool string) (string, uint64) {
	if len(spool) <= maxRememberedOutput {
		return spool, 0
	}
	dropped := spool[:len(spool)-maxRememberedOutput]
	tail := spool[len(spool)-maxRememberedOutput:]
	if i := strings.IndexByte(tail, '\n'); i >= 0 {
		dropped = spool[:len(spool)-maxRememberedOutput+i+1]
		tail = tail[i+1:]
	}
	lines := uint64(strings.Count(dropped, "\n"))
	if dropped != "" && !strings.HasSuffix(dropped, "\n") {
		lines++
	}
	return tail, lines
}

func TestRenderedDrawsWhatTheWholeSpoolDrew(t *testing.T) {
	// Arrange. Deterministic spools of varied line lengths, fed in varied
	// batch sizes, so the window's accounting is exercised across every cut.
	lineLengths := []int{0, 1, 17, 200, 5000, 20000}
	batchSizes := []int{1, 7, 4096, 1 << 20}
	for _, lineLen := range lineLengths {
		for _, batch := range batchSizes {
			var spool strings.Builder
			for spool.Len() < 3*maxRememberedOutput {
				spool.WriteString(strings.Repeat("q", lineLen))
				spool.WriteByte('\n')
			}
			spool.WriteString("unterminated")
			whole := spool.String()
			r := NewRunOutput(testLogger(t))
			ctx := runOutputCtx("/tmp/b1.output", "1:2", 0)

			// Act.
			for i := 0; i < len(whole); i += batch {
				end := i + batch
				if end > len(whole) {
					end = len(whole)
				}
				r.Remember(ctx, []byte(whole[i:end]))
			}
			text, bytesOmitted, linesOmitted := r.Rendered()

			// Assert.
			wantText, wantLines := capSpoolOracle(whole)
			if text != wantText || linesOmitted != wantLines || bytesOmitted+uint64(len(text)) != uint64(len(whole)) {
				t.Fatalf("line=%d batch=%d: rendered {%d bytes, lines %d, omitted %d}, want {%d bytes, lines %d} over %d written",
					lineLen, batch, len(text), linesOmitted, bytesOmitted, len(wantText), wantLines, len(whole))
			}
		}
	}
}

func TestAbsorbAtTheFilesStartResetsWithoutReadingIt(t *testing.T) {
	// Arrange. A rotated or truncated spool is re-read from offset 0, and the
	// window it had is not the new file's.
	r := NewRunOutput(testLogger(t))
	r.readPrefix = func(string, int64, func([]byte)) error {
		t.Fatal("a batch at offset 0 read the file's prefix")
		return nil
	}
	ctx := runOutputCtx("/tmp/b1.output", "1:2", 0)
	if err := r.Absorb(ctx, 0, []byte("old file\n")); err != nil {
		t.Fatalf("absorb: %v", err)
	}

	// Act.
	err := r.Absorb(ctx, 0, []byte("new\n"))

	// Assert.
	text, _, _ := r.Rendered()
	if err != nil || text != "new\n" {
		t.Fatalf("absorb = %v, text = %q, want the new file alone", err, text)
	}
}

func TestAbsorbOfAContinuingBatchNeverReadsTheFile(t *testing.T) {
	// Arrange. The ordinary poll continues the window; reseeding it would read
	// the whole spool on every batch.
	r := NewRunOutput(testLogger(t))
	r.readPrefix = func(string, int64, func([]byte)) error {
		t.Fatal("a continuing batch read the file's prefix")
		return nil
	}
	ctx := runOutputCtx("/tmp/b1.output", "1:2", 0)
	if err := r.Absorb(ctx, 0, []byte("one\n")); err != nil {
		t.Fatalf("absorb: %v", err)
	}

	// Act.
	err := r.Absorb(ctx, 4, []byte("two\n"))

	// Assert.
	text, _, _ := r.Rendered()
	if err != nil || text != "one\ntwo\n" {
		t.Fatalf("absorb = %v, text = %q, want both batches", err, text)
	}
}

func TestAbsorbAnswersTheReseedsFailure(t *testing.T) {
	// Arrange.
	r := NewRunOutput(testLogger(t))
	r.readPrefix = func(string, int64, func([]byte)) error { return errors.New("disk said no") }

	// Act.
	err := r.Absorb(runOutputCtx("/tmp/b1.output", "1:2", 9), 5, []byte("tail\n"))

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "disk said no") {
		t.Fatalf("absorb = %v, want the reseed's failure", err)
	}
}

func TestReadFilePrefixStreamsExactlyThePrefix(t *testing.T) {
	// Arrange. A prefix longer than one chunk, so the loop is exercised.
	path := filepath.Join(t.TempDir(), "b1.output")
	content := strings.Repeat("0123456789", prefixChunk/5)
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatalf("write fixture: %v", err)
	}
	var got strings.Builder

	// Act.
	err := readFilePrefix(path, int64(len(content)-3), func(chunk []byte) { got.Write(chunk) })

	// Assert.
	if err != nil || got.String() != content[:len(content)-3] {
		t.Fatalf("read = %v, %d bytes, want the first %d bytes", err, got.Len(), len(content)-3)
	}
}

func TestReadFilePrefixRefusesAFileShorterThanThePrefix(t *testing.T) {
	// Arrange. A prefix the file no longer holds cannot rebuild the window.
	path := filepath.Join(t.TempDir(), "b1.output")
	if err := os.WriteFile(path, []byte("short"), 0o600); err != nil {
		t.Fatalf("write fixture: %v", err)
	}

	// Act.
	err := readFilePrefix(path, 64, func([]byte) {})

	// Assert.
	if err == nil {
		t.Fatal("a prefix longer than the file was read without error")
	}
}

func TestReadFilePrefixRefusesAMissingFile(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "gone.output")

	// Act.
	err := readFilePrefix(path, 1, func([]byte) {})

	// Assert.
	if err == nil {
		t.Fatal("a missing spool was read without error")
	}
}
