package db

import (
	"errors"
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

func TestObserveQueryReportsAStatementOverTheThreshold(t *testing.T) {
	// Arrange
	s, log := newSink(t)
	d := &DB{log: log, slowQuery: time.Nanosecond}

	// Act
	d.observeQuery(StatementOpenPage, "entry", logging.Fields{BookAgentID: "agent-1"}, time.Now().Add(-time.Second), 12)

	// Assert
	s.assertLogged(t, "warn", "exceeded the slow-query threshold")
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

	// Act: budget is 3125ms; ten seconds blows past it.
	d.observeQuery(StatementWriteBatch, "entry", logging.Fields{}, time.Now().Add(-10*time.Second), 575)

	// Assert
	s.assertLogged(t, "warn", "exceeded the slow-query threshold")
	s.assertContext(t, "statement", StatementWriteBatch)
	s.assertContext(t, "threshold_ms", float64((250*time.Millisecond + 575*5*time.Millisecond).Milliseconds()))
}
