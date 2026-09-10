package integration

import (
	"context"
	"testing"
)

// CRITIQUE 6 — SIGTERM with a batch in flight.
//
// The sidecar holds no retry buffer and spills nothing, so a signal that arrives
// mid-ingest can only leave the store in one of two states for the batch that
// was in flight: the records AND the cursor advance committed together (they ride
// one transaction), or NEITHER did. What must never appear is a partial — records
// the cursor does not cover, or a cursor past records that were never stored.
//
// The two directions are separate edge cases and get a subject each, over the
// same observation: where the store's cursor for the file ended up, and which of
// the transcript's units its book holds.

// terminatedIngest is what one SIGTERM-during-ingest left behind: the store's
// committed cursor for the file, the units its book holds, and the file offset
// each unit's own record ended at.
type terminatedIngest struct {
	CursorOffset int64
	InBook       map[string]int
	RecordEnd    map[string]int64
}

// capturedUnitLines maps each unit of the captured transcript to the file line
// that mints it: the two responses' thinking blocks and their Bash calls.
var capturedUnitLines = map[string]int{
	capturedThinking1: 7,
	capturedBashCall1: 8,
	capturedThinking2: 12,
	capturedBashCall2: 13,
}

// terminateMidIngest grows the captured transcript, signals the sidecar the
// instant the rest of the file is on disk, and reports what the store kept.
//
// THE SIGNAL LANDS WHILE THERE IS WORK OUTSTANDING: the first cursor advance is
// waited on (a real event, not a duration), so production is demonstrably under
// way, and the remaining lines are written immediately before the SIGTERM — so
// the process is asked to leave with bytes it has not committed.
func terminateMidIngest(ctx context.Context, t *testing.T, tag string) terminatedIngest {
	t.Helper()
	store := startRealStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, store.Socket, tree)

	proc := startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	ends := make([]int64, 0, len(captured.Lines))
	for _, line := range captured.Lines[:9] {
		g.AppendLine(line)
		ends = append(ends, g.Offset())
	}
	awaitCursorAtLeast(ctx, t, store.Client, g.Path(), 1)
	for _, line := range captured.Lines[9:] {
		g.AppendLine(line)
		ends = append(ends, g.Offset())
	}
	proc.Stop()

	out := terminatedIngest{
		InBook:    unitsInBook(ctx, t, store.Client, captured.Session),
		RecordEnd: map[string]int64{},
	}
	cs := cursorByPath(ctx, t, store.Client, g.Path())
	if cs == nil {
		t.Fatalf("%s: the store holds no cursor for %s, though one was observed before the signal", tag, g.Path())
	}
	out.CursorOffset = cs.GetOffset()
	for unit, line := range capturedUnitLines {
		out.RecordEnd[unit] = ends[line]
	}
	return out
}

// TestSigtermCommitsNoRecordItsCursorDoesNotCover asserts the direction that
// catches a batch whose entries were written without their cursor advance: every
// unit the book holds lies wholly behind the committed cursor.
func TestSigtermCommitsNoRecordItsCursorDoesNotCover(t *testing.T) {
	t.Parallel()
	// Arrange & Act.
	ctx, cancel := testContext(t)
	defer cancel()
	got := terminateMidIngest(ctx, t, "records-behind-the-cursor")

	// Assert.
	for unit, n := range got.InBook {
		end, known := got.RecordEnd[unit]
		if !known || n == 0 {
			continue
		}
		if got.CursorOffset < end {
			t.Errorf("unit %q is in the book but its record ends at %d, past the committed cursor %d; the records and the cursor ride ONE transaction",
				unit, end, got.CursorOffset)
		}
	}
}

// TestSigtermCommitsNoCursorPastRecordsItDidNotStore asserts the other
// direction: nothing the cursor claims to have read is missing from the book.
func TestSigtermCommitsNoCursorPastRecordsItDidNotStore(t *testing.T) {
	t.Parallel()
	// Arrange & Act.
	ctx, cancel := testContext(t)
	defer cancel()
	got := terminateMidIngest(ctx, t, "cursor-past-the-records")

	// Assert.
	for unit, end := range got.RecordEnd {
		if got.CursorOffset < end {
			continue
		}
		if got.InBook[unit] == 0 {
			t.Errorf("the cursor stands at %d, past unit %q's record end %d, but the book does not hold it; a cursor never advances past records the same transaction did not store",
				got.CursorOffset, unit, end)
		}
	}
}
