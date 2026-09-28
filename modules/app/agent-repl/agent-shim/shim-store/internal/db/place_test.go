package db

import (
	"database/sql"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// placed states a conversation place on an entry, as a producer does at the
// one place it builds the envelope.
func placed(entry *storev1.StoreEntry, atMs int64, ordinal uint32) *storev1.StoreEntry {
	entry.Place = &conversationv1.ConversationPlace{AtMs: atMs, Ordinal: ordinal}
	return entry
}

// placedLine is one line of a book at a stated place.
func placedLine(writeID, upsertKey, book string, atMs int64, ordinal uint32) *storev1.StoreEntry {
	return placed(pageEntry(writeID, upsertKey, book, frameItem(activityFrame(book, "act-"+upsertKey, prose()))), atMs, ordinal)
}

// servedPointers renders a page's pointers in the order served.
func servedPointers(lines []*storev1.StoreLineAt) []string {
	out := make([]string, 0, len(lines))
	for _, line := range lines {
		out = append(out, line.GetAt().GetValue())
	}
	return out
}

func TestLineAtServesAStatedPlaceOnTheRecordedArm(t *testing.T) {
	// Arrange
	place := servedPlace{atMs: 42, ordinal: 3, recorded: true}

	// Act
	line := lineAt(7, &storev1.StorePageLine{}, nil, place)

	// Assert
	if got := line.GetRecordedPlace(); got.GetAtMs() != 42 || got.GetOrdinal() != 3 {
		t.Fatalf("place = %v, want recorded 42.3", line.GetPlace())
	}
}

func TestLineAtServesAReceiptInstantOnTheReceivedArm(t *testing.T) {
	// Arrange
	place := servedPlace{atMs: 42}

	// Act
	line := lineAt(7, &storev1.StorePageLine{}, nil, place)

	// Assert
	if got := line.GetReceivedPlace(); got.GetAtMs() != 42 || got.GetOrdinal() != 0 {
		t.Fatalf("place = %v, want received 42.0", line.GetPlace())
	}
}

func TestPlaceOfAStatedPlaceIsRecorded(t *testing.T) {
	// Arrange
	entry := placed(&storev1.StoreEntry{}, 500, 2)

	// Act
	place := placeOf(entry, 900)

	// Assert
	if place != (servedPlace{atMs: 500, ordinal: 2, recorded: true}) {
		t.Fatalf("place = %+v, want the stated place, recorded", place)
	}
}

func TestPlaceOfAnUnplacedEntryIsItsReceiptInstant(t *testing.T) {
	// Arrange
	entry := &storev1.StoreEntry{}

	// Act
	place := placeOf(entry, 900)

	// Assert
	if place != (servedPlace{atMs: 900}) {
		t.Fatalf("place = %+v, want the receipt instant at ordinal 0, received", place)
	}
}

func TestScanPlaceRefusesABookedRowWithNoPlaceRow(t *testing.T) {
	// Arrange: the columns a LEFT JOIN leaves NULL for a missing place row.
	var none sql.NullInt64

	// Act
	_, err := scanPlace(7, none, none, none)

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("scanPlace = %v, want a storage failure", err)
	}
}

func TestEveryBookedWriteIsPlacedInTheOrderIndex(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	writeOK(t, d, placedLine("w1", "u1", "agent-1", 500, 2))

	// Assert
	if got := scalar[int64](t, d, `SELECT at_ms * 10 + ordinal FROM entry_place WHERE book_agent_id = 'agent-1'`); got != 5002 {
		t.Fatalf("placed key = %d, want 500.2", got)
	}
}

func TestAnUnbookedWriteIsNotPlacedInTheOrderIndex(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	entry := placed(sessionUpdateEntry("w1", "u1"), 500, 0)

	// Act
	writeOK(t, d, entry)

	// Assert
	if got := scalar[int64](t, d, `SELECT COUNT(*) FROM entry_place`); got != 0 {
		t.Fatalf("place rows = %d, want none for a row no page can serve", got)
	}
}
