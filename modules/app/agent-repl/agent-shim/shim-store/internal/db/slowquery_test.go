package db

import (
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"

	"agentrepl/shim-store/internal/logging"
)

func TestSlowQueryFromEnvUsesTheShippedDefaultWhenUnset(t *testing.T) {
	// Arrange
	t.Setenv(EnvSlowQueryMs, "")

	// Act
	got, err := SlowQueryFromEnv()

	// Assert
	if err != nil {
		t.Fatalf("SlowQueryFromEnv: %v", err)
	}
	if got != DefaultSlowQuery {
		t.Fatalf("threshold = %v, want %v", got, DefaultSlowQuery)
	}
}

func TestSlowQueryFromEnvReadsAnExplicitThreshold(t *testing.T) {
	// Arrange
	t.Setenv(EnvSlowQueryMs, "50")

	// Act
	got, err := SlowQueryFromEnv()

	// Assert
	if err != nil {
		t.Fatalf("SlowQueryFromEnv: %v", err)
	}
	if got != 50*time.Millisecond {
		t.Fatalf("threshold = %v, want 50ms", got)
	}
}

func TestSlowQueryFromEnvRefusesAMalformedValue(t *testing.T) {
	// Arrange
	t.Setenv(EnvSlowQueryMs, "soon")

	// Act
	_, err := SlowQueryFromEnv()

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}

func TestSlowQueryFromEnvRefusesANonPositiveValue(t *testing.T) {
	// Arrange: an operator who set it to zero meant something by it, and
	// running the shipped default underneath them is the failure the loud
	// refusal exists to prevent.
	t.Setenv(EnvSlowQueryMs, "0")

	// Act
	_, err := SlowQueryFromEnv()

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}

// sustainOverBudget drives one statement family over its budget enough times
// that the NEXT observation is reported as a persistent defect rather than an
// isolated spike. It is what a lost index looks like: every statement of the
// family, not one unlucky one.
func sustainOverBudget(d *DB, statement string) {
	for i := 0; i < BudgetWarnAt-1; i++ {
		d.observeBudget(statement, true)
	}
}

func TestObserveQueryReportsAStatementOverTheThreshold(t *testing.T) {
	// Arrange
	s, log := newSink(t)
	d := &DB{log: log, slowQuery: time.Nanosecond}
	sustainOverBudget(d, StatementOpenPage)

	// Act
	d.observeQuery(StatementOpenPage, "entry", logging.Fields{BookAgentID: "agent-1"}, time.Now().Add(-time.Second), 12)

	// Assert
	s.assertLogged(t, "warn", "persistently over its budget")
	s.assertContext(t, "statement", StatementOpenPage)
	s.assertContext(t, "book_agent_id", "agent-1")
	s.assertContext(t, "rows", float64(12))
}

func TestObserveQueryReportsTheLockWaitApartFromTheTotal(t *testing.T) {
	// Arrange: a batch that queued behind another writer for nearly all of its
	// measured duration. Reported as one number it reads as a slow statement;
	// the two numbers together say it never got to run.
	s, log := newSink(t)
	d := &DB{log: log, slowQuery: time.Nanosecond}
	sustainOverBudget(d, StatementWriteBatch)

	// Act
	d.observeQuery(StatementWriteBatch, "entry",
		logging.Fields{LockWait: 3800 * time.Millisecond}, time.Now().Add(-3822*time.Millisecond), 6)

	// Assert
	s.assertLogged(t, "warn", "lock_wait_ms=3800")
	s.assertContext(t, "lock_wait_ms", float64(3800))
}

func TestObserveQueryReportsAZeroLockWaitRatherThanOmittingIt(t *testing.T) {
	// Arrange: a statement with no wait to measure. Zero is a fact — "this one
	// really did spend its time running" — and an omitted key would leave a
	// reader unable to tell it from an older record that never measured.
	s, log := newSink(t)
	d := &DB{log: log, slowQuery: time.Nanosecond}

	// Act
	d.observeQuery(StatementOpenPage, "entry", logging.Fields{}, time.Now().Add(-time.Second), 3)

	// Assert
	s.assertContext(t, "lock_wait_ms", float64(0))
}

func TestObserveQuerySaysNothingAboutAFastStatement(t *testing.T) {
	// Arrange: successful query timing is exactly the high-volume narration
	// the verbose gate exists to keep out of a singleton global log.
	s, log := newSink(t)
	d := &DB{log: log, slowQuery: time.Hour}

	// Act
	d.observeQuery(StatementWriteBatch, "entry", logging.Fields{}, time.Now(), 1)

	// Assert
	if len(s.records(t)) != 0 {
		t.Fatalf("a fast statement was reported: %s", s.file.String())
	}
}

func TestObserveQuerySaysNothingWhenReportingIsDisabled(t *testing.T) {
	// Arrange
	s, log := newSink(t)
	d := &DB{log: log, slowQuery: 0}

	// Act
	d.observeQuery(StatementWriteBatch, "entry", logging.Fields{}, time.Now().Add(-time.Hour), 1)

	// Assert
	if len(s.records(t)) != 0 {
		t.Fatalf("reporting was disabled but a record was written: %s", s.file.String())
	}
}

func TestBulkBudgetFromEnvUsesTheShippedDefaultsWhenUnset(t *testing.T) {
	// Arrange
	t.Setenv(EnvBulkBaseMs, "")
	t.Setenv(EnvBulkPerRowMs, "")

	// Act
	base, perRow, err := BulkBudgetFromEnv()

	// Assert
	if err != nil {
		t.Fatalf("BulkBudgetFromEnv: %v", err)
	}
	if base != DefaultBulkBase || perRow != DefaultBulkPerRow {
		t.Fatalf("budget = (%v, %v), want (%v, %v)", base, perRow, DefaultBulkBase, DefaultBulkPerRow)
	}
}

func TestBulkBudgetFromEnvReadsExplicitValues(t *testing.T) {
	// Arrange
	t.Setenv(EnvBulkBaseMs, "100")
	t.Setenv(EnvBulkPerRowMs, "7")

	// Act
	base, perRow, err := BulkBudgetFromEnv()

	// Assert
	if err != nil {
		t.Fatalf("BulkBudgetFromEnv: %v", err)
	}
	if base != 100*time.Millisecond || perRow != 7*time.Millisecond {
		t.Fatalf("budget = (%v, %v), want (100ms, 7ms)", base, perRow)
	}
}

func TestBulkBudgetFromEnvAllowsAZeroBase(t *testing.T) {
	// Arrange: a zero base budgets purely per row, which is legitimate.
	t.Setenv(EnvBulkBaseMs, "0")

	// Act
	base, _, err := BulkBudgetFromEnv()

	// Assert
	if err != nil {
		t.Fatalf("BulkBudgetFromEnv: %v", err)
	}
	if base != 0 {
		t.Fatalf("base = %v, want 0", base)
	}
}

func TestBulkBudgetFromEnvRefusesANegativeBase(t *testing.T) {
	// Arrange
	t.Setenv(EnvBulkBaseMs, "-1")

	// Act
	_, _, err := BulkBudgetFromEnv()

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}

func TestBulkBudgetFromEnvRefusesAMalformedPerRow(t *testing.T) {
	// Arrange
	t.Setenv(EnvBulkPerRowMs, "later")

	// Act
	_, _, err := BulkBudgetFromEnv()

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}

func TestBulkBudgetFromEnvRefusesANonPositivePerRow(t *testing.T) {
	// Arrange: a zero per-row budget would collapse the bulk budget back to a
	// fixed base, hiding a large batch's growing cost; that is the mistake the
	// refusal exists to prevent.
	t.Setenv(EnvBulkPerRowMs, "0")

	// Act
	_, _, err := BulkBudgetFromEnv()

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}

func TestBudgetForScalesWriteBatchWithRows(t *testing.T) {
	// Arrange
	d := &DB{slowQuery: 250 * time.Millisecond, bulkBase: 250 * time.Millisecond, bulkPerRow: 5 * time.Millisecond}

	// Act + Assert: base + perRow*rows.
	if got, want := d.budgetFor(StatementWriteBatch, 575), 250*time.Millisecond+575*5*time.Millisecond; got != want {
		t.Fatalf("budget(write_batch, 575) = %v, want %v", got, want)
	}
}

func TestBudgetForLeavesAPointQueryFixed(t *testing.T) {
	// Arrange
	d := &DB{slowQuery: 250 * time.Millisecond, bulkBase: 250 * time.Millisecond, bulkPerRow: 5 * time.Millisecond}

	// Act + Assert: a point query is never scaled by the answer's size.
	if got := d.budgetFor(StatementOpenPage, 100000); got != 250*time.Millisecond {
		t.Fatalf("budget(open_page) = %v, want the fixed interactive threshold 250ms", got)
	}
}

func TestBudgetForFloorsAtTheInteractiveThreshold(t *testing.T) {
	// Arrange: an empty or tiny batch whose fixed cost still blows the point
	// threshold is a real stall, so the bulk budget never drops below it.
	d := &DB{slowQuery: 250 * time.Millisecond, bulkBase: 0, bulkPerRow: time.Millisecond}

	// Act + Assert.
	if got := d.budgetFor(StatementWriteBatch, 0); got != 250*time.Millisecond {
		t.Fatalf("budget(write_batch, 0) = %v, want the interactive floor 250ms", got)
	}
}

func TestBudgetForIsDisabledWhenReportingIsOff(t *testing.T) {
	// Arrange
	d := &DB{slowQuery: 0, bulkBase: 250 * time.Millisecond, bulkPerRow: 5 * time.Millisecond}

	// Act + Assert: a non-positive slowQuery is the master switch.
	if got := d.budgetFor(StatementWriteBatch, 1000); got != 0 {
		t.Fatalf("budget with reporting disabled = %v, want 0", got)
	}
}

func TestObserveQueryDoesNotWarnOnAHealthyBulkWrite(t *testing.T) {
	// Arrange: a large, healthy batch on a large database — 575 rows in ~2s,
	// the owner's observed healthy write. The bulk budget (250ms + 5ms/row =
	// 3125ms) covers it, so it must not flood the strict all-logs harvest even
	// though it far exceeds the 250ms interactive point-query threshold.
	s, log := newSink(t)
	d := &DB{log: log, slowQuery: 250 * time.Millisecond, bulkBase: 250 * time.Millisecond, bulkPerRow: 5 * time.Millisecond}

	// Act
	d.observeQuery(StatementWriteBatch, "entry", logging.Fields{}, time.Now().Add(-2033*time.Millisecond), 575)

	// Assert
	if len(s.records(t)) != 0 {
		t.Fatalf("a healthy bulk write was reported as slow: %s", s.file.String())
	}
}

func TestObserveQueryStillWarnsOnAPathologicalBulkWrite(t *testing.T) {
	// Arrange: the same 575-row batch, but taking far longer than the per-row
	// budget allows — a reintroduced scan or a lost index. This must still warn,
	// and the reported threshold is the row-scaled budget, not the fixed one.
	s, log := newSink(t)
	d := &DB{log: log, slowQuery: 250 * time.Millisecond, bulkBase: 250 * time.Millisecond, bulkPerRow: 5 * time.Millisecond}
	sustainOverBudget(d, StatementWriteBatch)

	// Act: budget is 3125ms; ten seconds blows past it.
	d.observeQuery(StatementWriteBatch, "entry", logging.Fields{}, time.Now().Add(-10*time.Second), 575)

	// Assert
	s.assertLogged(t, "warn", "persistently over its budget")
	s.assertContext(t, "statement", StatementWriteBatch)
	s.assertContext(t, "threshold_ms", float64((250*time.Millisecond + 575*5*time.Millisecond).Milliseconds()))
}

// TestObserveQueryLevelsAnOverBudgetSampleByItsFamilysWindow pins the boundary
// between the two causes of a long wall clock. A lost index or a reintroduced
// scan is a property of the STATEMENT and fills the family's window; a loaded
// host takes whichever statement was unlucky.
func TestObserveQueryLevelsAnOverBudgetSampleByItsFamilysWindow(t *testing.T) {
	tests := []struct {
		name      string
		priorOver int
		wantLevel string
		wantText  string
	}{
		{
			name:      "the first sample of a healthy family is an isolated spike",
			priorOver: 0,
			wantLevel: "info",
			wantText:  "isolated sample",
		},
		{
			name:      "one short of the bar is still an isolated spike",
			priorOver: BudgetWarnAt - 2,
			wantLevel: "info",
			wantText:  "isolated sample",
		},
		{
			name:      "the sample that reaches the bar is a persistent defect",
			priorOver: BudgetWarnAt - 1,
			wantLevel: "warn",
			wantText:  "persistently over its budget",
		},
		{
			name:      "a family that is over budget throughout stays a defect",
			priorOver: BudgetWindow - 1,
			wantLevel: "warn",
			wantText:  "persistently over its budget",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			s, log := newSink(t)
			d := &DB{log: log, slowQuery: time.Nanosecond}
			for i := 0; i < test.priorOver; i++ {
				d.observeBudget(StatementOpenPage, true)
			}

			// Act
			d.observeQuery(StatementOpenPage, "entry", logging.Fields{}, time.Now().Add(-time.Second), 1)

			// Assert
			s.assertLogged(t, test.wantLevel, test.wantText)
			s.assertContext(t, "over_budget_recent", float64(test.priorOver+1))
		})
	}
}

// TestObserveQueryKeepsAWindowPerStatementFamily pins that one family's spikes
// never promote another's. The budget answers a question about a statement, so
// its evidence is that statement's.
func TestObserveQueryKeepsAWindowPerStatementFamily(t *testing.T) {
	// Arrange: write_batch is persistently over budget; open_page is not.
	s, log := newSink(t)
	d := &DB{log: log, slowQuery: time.Nanosecond}
	sustainOverBudget(d, StatementWriteBatch)

	// Act
	d.observeQuery(StatementOpenPage, "entry", logging.Fields{}, time.Now().Add(-time.Second), 1)

	// Assert
	s.assertLogged(t, "info", "isolated sample")
	s.assertContext(t, "over_budget_recent", float64(1))
}

// TestObserveQueryForgetsAFamilyThatRecovered pins that the window SLIDES: a
// family whose statements went back within budget is no longer a defect, so a
// later spike reads as the spike it is.
func TestObserveQueryForgetsAFamilyThatRecovered(t *testing.T) {
	// Arrange: over budget throughout one whole window, then a full window of
	// healthy statements. An under-budget statement writes no record, but its
	// verdict is still what the window is made of.
	s, log := newSink(t)
	d := &DB{log: log, slowQuery: time.Hour}
	for i := 0; i < BudgetWindow; i++ {
		d.observeBudget(StatementOpenPage, true)
	}
	for i := 0; i < BudgetWindow; i++ {
		d.observeQuery(StatementOpenPage, "entry", logging.Fields{}, time.Now(), 1)
	}
	if len(s.records(t)) != 0 {
		t.Fatalf("a statement within budget was reported: %s", s.file.String())
	}

	// Act: one spike, against a family that has been healthy since.
	d.slowQuery = time.Nanosecond
	d.observeQuery(StatementOpenPage, "entry", logging.Fields{}, time.Now().Add(-time.Second), 1)

	// Assert
	s.assertLogged(t, "info", "isolated sample")
	s.assertContext(t, "over_budget_recent", float64(1))
}

// ---- per-class write timing ----

// queuedWrite holds the slot, queues one single-entry batch of `class` behind
// it, moves the clock by `wait` while it is queued and by `exec` once it has
// committed, and returns when it is done. The numbers are therefore exact.
func queuedWrite(t *testing.T, d *DB, clock *fakeClock, class WriteClass, id string, wait, exec time.Duration) {
	t.Helper()
	queued := make(chan struct{})
	d.queuedForWrite = func(WriteClass) {
		clock.advance(wait)
		close(queued)
	}
	d.transactionCommitted = func(WriteClass) { clock.advance(exec) }
	release, err := d.acquireWrite(ctx(), WriteInteractive)
	if err != nil {
		t.Fatalf("acquireWrite: %v", err)
	}
	done := make(chan error, 1)
	go func() {
		_, err := d.WriteBatch(ctx(), "test-producer", class,
			batch(pageEntry("w-"+id, "u-"+id, "agent-1", frameItem(activityFrame("agent-1", "act-"+id, prose())))), nil)
		done <- err
	}()
	<-queued
	release()
	if err := <-done; err != nil {
		t.Fatalf("WriteBatch: %v", err)
	}
	d.queuedForWrite = nil
	d.transactionCommitted = nil
}

// recordFor returns the context of the first record with this operation and
// statement.
func recordFor(t *testing.T, s *sink, operation, statement string) (map[string]any, string) {
	t.Helper()
	for _, record := range s.records(t) {
		context, _ := record["context"].(map[string]any)
		if record["operation"] == operation && context["statement"] == statement {
			message, _ := record["message"].(string)
			return context, message
		}
	}
	t.Fatalf("no %s record for %s; log was:\n%s", operation, statement, s.file.String())
	return nil, ""
}

// TestEveryWriteIsTimedByClass pins the per-write timing record: every write,
// healthy or not, reports its class, its queue wait and its execution time
// apart.
func TestEveryWriteIsTimedByClass(t *testing.T) {
	tests := []struct {
		name  string
		class WriteClass
		want  string
	}{
		{name: "interactive", class: WriteInteractive, want: "interactive"},
		{name: "bulk", class: WriteBulk, want: "bulk"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			clock := &fakeClock{now: time.Unix(0, 0)}
			d, s := newBoundedStore(t, clock, 0, 0, 0)

			// Act
			queuedWrite(t, d, clock, test.class, "a", 40*time.Millisecond, 7*time.Millisecond)

			// Assert
			context, _ := recordFor(t, s, WriteTimingOperation, StatementWriteBatch)
			if context["write_class"] != test.want || context["lock_wait_ms"] != float64(40) || context["exec_ms"] != float64(7) {
				t.Fatalf("timing record write_class=%v lock_wait_ms=%v exec_ms=%v, want %s, 40, 7",
					context["write_class"], context["lock_wait_ms"], context["exec_ms"], test.want)
			}
		})
	}
}

// TestASlowWriteRecordNamesItsClass pins that the slow-query record of a write
// says which queue it took and splits its wait from its execution.
func TestASlowWriteRecordNamesItsClass(t *testing.T) {
	tests := []struct {
		name  string
		class WriteClass
		want  string
	}{
		{name: "interactive", class: WriteInteractive, want: "interactive"},
		{name: "bulk", class: WriteBulk, want: "bulk"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			clock := &fakeClock{now: time.Unix(0, 0)}
			d, s := newReportingStore(t, clock)

			// Act
			queuedWrite(t, d, clock, test.class, "a", 40*time.Millisecond, 7*time.Millisecond)

			// Assert
			context, message := recordFor(t, s, SlowQueryOperation, StatementWriteBatch)
			if context["write_class"] != test.want || context["exec_ms"] != float64(7) || context["lock_wait_ms"] != float64(40) {
				t.Fatalf("slow record write_class=%v lock_wait_ms=%v exec_ms=%v, want %s, 40, 7",
					context["write_class"], context["lock_wait_ms"], context["exec_ms"], test.want)
			}
			if !strings.Contains(message, "write_class="+test.want) {
				t.Fatalf("slow record message %q does not name write_class=%s", message, test.want)
			}
		})
	}
}

// TestTheBudgetWindowIsKeptPerClass: a backlog of slow bulk writes does not
// make an interactive write's first slow sample read as a persistent defect.
func TestTheBudgetWindowIsKeptPerClass(t *testing.T) {
	tests := []struct {
		name      string
		slowBulk  int
		wantLevel string
	}{
		{name: "bulk persistently over budget leaves interactive isolated", slowBulk: BudgetWarnAt + 2, wantLevel: "info"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			clock := &fakeClock{now: time.Unix(0, 0)}
			d, s := newReportingStore(t, clock)
			for i := 0; i < test.slowBulk; i++ {
				queuedWrite(t, d, clock, WriteBulk, fmt.Sprintf("b%d", i), time.Millisecond, time.Millisecond)
			}

			// Act
			queuedWrite(t, d, clock, WriteInteractive, "live", time.Millisecond, time.Millisecond)

			// Assert
			var last, lastBulk map[string]any
			for _, record := range s.records(t) {
				context, _ := record["context"].(map[string]any)
				if record["operation"] == SlowQueryOperation && context["write_class"] == "interactive" {
					last = record
				}
				if record["operation"] == SlowQueryOperation && context["write_class"] == "bulk" {
					lastBulk = record
				}
			}
			if lastBulk == nil || lastBulk["level"] != "warn" {
				t.Fatalf("the arranged bulk backlog did not reach the persistent warning: %v", lastBulk)
			}
			if last == nil || last["level"] != test.wantLevel {
				t.Fatalf("interactive slow record = %v, want level %s", last, test.wantLevel)
			}
		})
	}
}
