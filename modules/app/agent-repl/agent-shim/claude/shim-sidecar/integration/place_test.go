package integration

import (
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — the conversation place.
//
// Every entry a transcript record converts to reaches the store stating where
// it sits in its conversation: the timestamp of the record that opened its unit
// and its rank among the record's entries, read from the bytes.

// TestATranscriptRecordReachesTheStorePlacedAtItsOwnTimestamp asserts the
// place a record's entry carries on the wire.
func TestATranscriptRecordReachesTheStorePlacedAtItsOwnTimestamp(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	session := "e0e0e0e0-e0e0-40e0-80e0-e0e0e0e0e0e0"
	g, uuid := seedApiError(t, tree, "/work/place-probe", session)
	rec := decodeRecord(t, corpusLine(t, "transcript-lines/system-api_error.jsonl", 0))
	stamp, _ := rec["timestamp"].(string)
	recorded, err := time.Parse(time.RFC3339Nano, stamp)
	if err != nil {
		t.Fatalf("the api_error fixture's timestamp %q does not parse: %v", stamp, err)
	}

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	wantKey := "session:api_error:" + uuid
	fake.awaitEntry(ctx, t, "the api_error record", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == wantKey
	})
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	place := entryByUpsertKey(fake.Entries(), wantKey).GetPlace()
	if place.GetAtMs() != recorded.UnixMilli() || place.GetOrdinal() != 0 {
		t.Fatalf("place = %v, want %d.0 (the record's own timestamp)", place, recorded.UnixMilli())
	}
}
