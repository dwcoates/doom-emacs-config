package convert

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

const (
	callAt   = "2026-09-01T10:00:00.000Z"
	resultAt = "2026-09-01T10:00:05.000Z"
)

// placeOf renders an entry's stated place as (at_ms, ordinal), or (0, 0) when
// the entry states none.
func placeOf(e *storev1.StoreEntry) (int64, uint32) {
	return e.GetPlace().GetAtMs(), e.GetPlace().GetOrdinal()
}

// readFrameEntry is a page line carrying one read unit's frame.
func readFrameEntry(read *conversationv1.AgentRead) *storev1.StoreEntry {
	return PageLine(testAttribution(0), "0", "activity:x", "session-uuid", &conversationv1.AgentFrame{
		AgentId: agentID("session-uuid"),
		Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{Activity: &conversationv1.AgentActivity{
				Item: &conversationv1.AgentActivity_Read{Read: read},
			}},
		}},
	})
}

// startFrameEntry is a page line whose frame states a start instant.
func startFrameEntry(startMs int64) *storev1.StoreEntry {
	return readFrameEntry(&conversationv1.AgentRead{Result: &conversationv1.AgentRead_Start{
		Start: &conversationv1.AgentReadStart{StartedAt: startedAt(startMs)},
	}})
}

func TestStampPlacePlacesAnEntryAtItsRecordsTimestamp(t *testing.T) {
	// Arrange
	entries := []*storev1.StoreEntry{VendorSpecificEntry(testAttribution(0), "k", map[string]any{"a": "b"})}

	// Act
	stampPlace(entries, 1000)

	// Assert
	if at, ordinal := placeOf(entries[0]); at != 1000 || ordinal != 0 {
		t.Fatalf("place = %d.%d, want 1000.0", at, ordinal)
	}
}

func TestStampPlaceRanksAnEntryByItsIndexAmongTheRecordsEntries(t *testing.T) {
	// Arrange
	entries := []*storev1.StoreEntry{
		VendorSpecificEntry(testAttribution(0), "a", map[string]any{"a": "b"}),
		VendorSpecificEntry(testAttribution(0), "b", map[string]any{"a": "b"}),
	}

	// Act
	stampPlace(entries, 1000)

	// Assert
	if _, ordinal := placeOf(entries[1]); ordinal != 1 {
		t.Fatalf("ordinal = %d, want 1", ordinal)
	}
}

func TestStampPlacePlacesAUnitThatStatesItsStartAtTheStart(t *testing.T) {
	// Arrange: the record carrying the entry is the unit's result, later than
	// the call that opened it.
	entries := []*storev1.StoreEntry{startFrameEntry(400)}

	// Act
	stampPlace(entries, 1000)

	// Assert
	if at, _ := placeOf(entries[0]); at != 400 {
		t.Fatalf("at_ms = %d, want the unit's start 400", at)
	}
}

func TestStampPlaceLeavesThePlaceUnsetWhenNoInstantIsKnown(t *testing.T) {
	// Arrange
	entries := []*storev1.StoreEntry{VendorSpecificEntry(testAttribution(0), "k", map[string]any{"a": "b"})}

	// Act
	stampPlace(entries, 0)

	// Assert
	if entries[0].Place != nil {
		t.Fatalf("place = %v, want unset", entries[0].GetPlace())
	}
}

func TestOpeningInstantReadsTheStartASettleRestates(t *testing.T) {
	// Arrange: a settled frame stands alone and restates its start.
	entry := readFrameEntry(&conversationv1.AgentRead{Result: &conversationv1.AgentRead_Success{
		Success: &conversationv1.AgentReadSuccess{SettledAt: settledAt(900, 300)},
	}})

	// Act
	got := openingInstant(entry)

	// Assert
	if got != 300 {
		t.Fatalf("opening instant = %d, want the restated start 300", got)
	}
}

func TestOpeningInstantOfAnEntryStatingNoStartIsZero(t *testing.T) {
	// Arrange
	entry := VendorSpecificEntry(testAttribution(0), "k", map[string]any{"startedAt": map[string]any{"atMs": 5}})

	// Act
	got := openingInstant(entry)

	// Assert
	if got != 0 {
		t.Fatalf("opening instant = %d, want 0", got)
	}
}

func TestLinePlacesAResultsSettleAtItsCall(t *testing.T) {
	// Arrange
	c := newTestConverter(t)
	convertLines(t, c, assistantWith("a1", "msg_1", callAt, toolCall("toolu_1", "Read", `{"file_path":"/p/f.go"}`)))

	// Act
	settle := entryByKey(t, convertLines(t, c, toolResultLine("u1", "toolu_1", resultAt, `"one"`, `{"type":"text","file":{"content":"one","numLines":1,"totalLines":1}}`)), "activity:toolu_1")

	// Assert
	if at, _ := placeOf(settle); at != parseInstant(callAt) {
		t.Fatalf("at_ms = %d, want the call's %d", at, parseInstant(callAt))
	}
}

func TestLineMintsTheIdenticalPlaceOnAReRead(t *testing.T) {
	// Arrange
	line := assistantWith("a1", "msg_1", callAt, `{"type":"text","text":"hello"}`)
	first := convertLines(t, newTestConverter(t), line)

	// Act
	again := convertLines(t, newTestConverter(t), line)

	// Assert
	firstAt, firstOrdinal := placeOf(first[0])
	againAt, againOrdinal := placeOf(again[0])
	if firstAt == 0 || firstAt != againAt || firstOrdinal != againOrdinal {
		t.Fatalf("places = %d.%d then %d.%d, want one identical stated place", firstAt, firstOrdinal, againAt, againOrdinal)
	}
}
