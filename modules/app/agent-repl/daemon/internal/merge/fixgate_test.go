package merge

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/testcases"
	"agent-repld/internal/wsm"
)

// fixGateCards answers every FeedFixGate WS's feed was drawn, in order, with
// their row ids.
func fixGateCards(h *harness, ws ids.WorkspaceID) ([]*frontendv1.FeedFixGate, []string) {
	h.feed.mu.Lock()
	defer h.feed.mu.Unlock()
	var cards []*frontendv1.FeedFixGate
	var rows []string
	for _, r := range h.feed.rows {
		if card := r.Row.GetActivity().GetFixGate(); r.WS == ws && card != nil {
			cards = append(cards, card)
			rows = append(rows, r.Row.GetId().GetValue())
		}
	}
	return cards, rows
}

const fixGateOutput = "[agent-repl-tests] daemon: starting\n[agent-repl-tests] daemon: passed in 2s\n"

func TestTheMediatorsGateIsDrawnInItsFeedAsItRuns(t *testing.T) {
	tests := []struct {
		name        string
		run         scriptedRun
		wantOutcome string
		wantSummary string
	}{
		{name: "a passing gate settles its card passed", run: scriptedRun{Output: fixGateOutput, Code: 0}, wantOutcome: "succeeded"},
		{name: "a failing gate settles its card failed, saying the fix goes back to work",
			run: scriptedRun{Output: "[agent-repl-tests] daemon: starting\n[agent-repl-tests] daemon: FAILED in 2s\n", Code: 1}, wantOutcome: "failed",
			wantSummary: "the suite failed (exit 1); the fix goes back to work"},
		{name: "a gate that could not run settles its card failed, saying so", run: scriptedRun{Code: 127}, wantOutcome: "failed",
			wantSummary: "the gate did not run"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			h.runner.runs = []scriptedRun{tt.run}

			// Act
			_ = h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

			// Assert: drawn live first, settled last, on one row of attempt 1.
			cards, rows := fixGateCards(h, "ws-fixer")
			if len(cards) < 2 || cards[0].GetLive() == nil || rows[0] != rows[len(rows)-1] {
				t.Fatalf("cards %v on rows %v; want a live card, then its settling, on one row", cards, rows)
			}
			last := cards[len(cards)-1]
			if last.GetHead().GetLabel().GetText() != fixGateLabel || last.GetHead().GetAttempt() != 1 || last.GetHead().GetMaxAttempts() != MaxFixAttempts {
				t.Fatalf("head %v; want %q, attempt 1 of %d", last.GetHead(), fixGateLabel, MaxFixAttempts)
			}
			settled := last.GetSettled()
			if settled == nil || (tt.wantOutcome == "succeeded") != (settled.GetSucceeded() != nil) ||
				!strings.HasPrefix(settled.GetFailed().GetSummary(), tt.wantSummary) {
				t.Fatalf("settled %v; want %s with %q", settled, tt.wantOutcome, tt.wantSummary)
			}
		})
	}
}

func TestTheMediatorsGateCardDrawsTheRunsSuitesAndLinksItsLog(t *testing.T) {
	// Arrange
	h := finishing(t)
	h.runner.runs = []scriptedRun{{Output: fixGateOutput, Code: 0}}

	// Act
	if err := h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch); err != nil {
		t.Fatal(err)
	}

	// Assert
	cards, _ := fixGateCards(h, "ws-fixer")
	last := cards[len(cards)-1].GetOutput()
	if len(last.GetSuites()) != 1 || last.GetSuites()[0].GetName() != "daemon" || last.GetSuites()[0].GetPassed() == nil {
		t.Fatalf("suites %v; want the daemon suite passed", last.GetSuites())
	}
	if last.GetLog().GetToken().GetValue() != "ws-fixer/1" {
		t.Fatalf("log %v; want the settled card linking the attempt's log", last.GetLog())
	}
}

// THE TWO GATES READ A RUN ALIKE: the same lines draw the same suites in a
// queue merge's tests tab and in a mediator's fix gate card, since both read
// them through suiteTracker.
func TestBothGatesReadARunsLinesThroughOneTracker(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queue := &gateRun{r: &run{o: h.o, ws: theWorkspace}, tracker: newSuiteTracker(h.o.nowMS)}
	card := &fixGateRun{o: h.o, tracker: newSuiteTracker(h.o.nowMS), fields: dlog.Context{}}
	card.f.Workspace = "ws-fixer"

	// Act
	for _, line := range strings.Split(strings.TrimSuffix(fixGateOutput, "\n"), "\n") {
		queue.tracker.take(line)
		card.line(line)
	}

	// Assert
	if !proto.Equal(queue.tracker.view(nil), card.tracker.view(nil)) {
		t.Fatalf("queue gate %v, fix gate %v; want one view of one run", queue.tracker.view(nil), card.tracker.view(nil))
	}
}

func TestALineTheFixGateCannotReadIsRecordedUnderTheFinish(t *testing.T) {
	// Arrange
	h := newHarness(t)
	card := &fixGateRun{o: h.o, tracker: newSuiteTracker(h.o.nowMS), fields: dlog.Context{"workspace": "ws-fixer"}}

	// Act
	card.line(strings.TrimSuffix(unreadableTotal, "\n"))

	// Assert
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "warn", Operation: opRemediationFinish, Message: errCountLine.Error()})
	if !ok || rec.Context["workspace"] != "ws-fixer" || rec.Context["line"] == "" {
		t.Fatalf("records = %v; want the line recorded at WARN under the finish", h.logs.Records())
	}
}

func TestRecordUnreadLine(t *testing.T) {
	tests := []struct {
		name      string
		err       error
		wantLevel string
	}{
		{name: "an unreadable seat line is an ERROR", err: errors.Join(errSeatLine, errors.New("gremlins")), wantLevel: "error"},
		{name: "an uncountable line is a WARN", err: errors.Join(errCountLine, errors.New("too big")), wantLevel: "warn"},
		{name: "a line read is no record"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			recordUnreadLine(h.logs.Global(), "op.test", "the line", tt.err, dlog.Context{"round": 2})

			// Assert
			records := h.logs.Records()
			if tt.wantLevel == "" {
				if len(records) != 0 {
					t.Fatalf("records = %v; want none", records)
				}
				return
			}
			rec, ok := dlog.FindRecord(records, dlog.RecordQuery{Level: tt.wantLevel, Operation: "op.test"})
			if !ok || rec.Context["line"] != "the line" || rec.Context["round"] != 2 || rec.Context["error"] != tt.err.Error() {
				t.Fatalf("records = %v; want one %s carrying the line, the cause and the caller's fields", records, tt.wantLevel)
			}
		})
	}
}

func TestAMediatorsGateLogResolvesInItsOwnFeed(t *testing.T) {
	tests := []struct {
		name      string
		ws        ids.WorkspaceID
		write     bool
		wantErr   bool
		wantFound bool
	}{
		{name: "the mediator's own log, on disk", ws: "ws-fixer", write: true, wantFound: true},
		{name: "the mediator's own log, aged out", ws: "ws-fixer", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			path := h.o.fixGateLog(tt.ws, 1).path
			if tt.write {
				if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
					t.Fatal(err)
				}
				if err := os.WriteFile(path, []byte("ok\n"), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, err := h.o.TestLogPath(context.Background(), tt.ws, string(tt.ws)+"/1")

			// Assert
			if (err != nil) != tt.wantErr || (tt.wantFound && got != path) {
				t.Fatalf("TestLogPath = (%q, %v); want %q, error %v", got, err, path, tt.wantErr)
			}
		})
	}
}

func TestAWorkspaceThatIsNoMediatorCannotResolveATokenNamingItself(t *testing.T) {
	// Arrange
	h := finishing(t)

	// Act
	_, err := h.o.TestLogPath(context.Background(), theWorkspace, string(theWorkspace)+"/1")

	// Assert
	var refusal *RefusalError
	if !errors.As(err, &refusal) || refusal.Arm != ArmUnknownMergeTestLog {
		t.Fatalf("TestLogPath = %v; want the token refused as no merge this feed draws", err)
	}
}

// EVERY GATE'S LOG HAS ONE SHAPE: a queue merge's and a mediator's are named
// by testsLogOf, so each is resolved and pruned alike.
func TestEveryGatesLogIsNamedByOneShape(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	merge, mediator := h.o.testLog("abc123", 2), h.o.fixGateLog("def456", 3)

	// Assert
	if merge != h.o.testsLogOf("abc123", 2) || mediator != h.o.testsLogOf("def456", 3) {
		t.Fatalf("merge %+v, mediator %+v; want both named by testsLogOf", merge, mediator)
	}
	if !IsLog(filepath.Base(merge.path)) || !IsLog(filepath.Base(mediator.path)) || mediator.token != "def456/3" {
		t.Fatalf("merge %+v, mediator %+v; want logs the retention pass knows, tokens key/round", merge, mediator)
	}
}

func TestAFixGateRowIsARootCardRow(t *testing.T) {
	// Arrange
	f := wsm.RemediationFinish{Workspace: "mediator", StartedAt: time.UnixMilli(5)}
	card := &frontendv1.FeedFixGate{}

	// Act
	row := fixGateRow(f, 2, card)

	// Assert
	want := feedid.RootCardRow("mediator", feedid.RowKey{Kind: feedid.KindFixGate, ID: "5-2"},
		&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_FixGate{FixGate: card}})
	if !proto.Equal(row, want) {
		t.Fatalf("row = %v, want the shared root card row %v", row, want)
	}
}

func TestAMediatorsFailingGateRecordsItsCasesForTheMediatorsReruns(t *testing.T) {
	// Arrange
	h := finishing(t)
	h.runner.runs = []scriptedRun{{Output: failedUnit("daemon:internal/merge#00", "daemon", "--- FAIL: TestA (0.01s)") + "[agent-repl-tests] daemon: FAILED in 2s\n", Code: 1}}

	// Act
	_ = h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

	// Assert
	got := h.db.testCases["ws-fixer"]
	if len(got) != 1 || testcases.Key(got[0]) != testcases.Key(testcases.GoPackage(moduleRoot+"daemon/internal/merge", []string{"TestA"})) {
		t.Fatalf("recorded %v for the mediator, want its gate's failing TestA", got)
	}
}
