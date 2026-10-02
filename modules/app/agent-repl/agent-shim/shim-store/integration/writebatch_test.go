// writebatch_test.go — SUBJECT 1: WriteBatch's durable-ack semantics.
//
// Success means DURABLE: the records and the cursor advance committed as ONE
// transaction, which is why every assertion here reads the state back AFTER a
// store restart — an in-memory answer cannot pass. Failure means NOTHING
// committed, so the same restart shows neither the records nor the cursor.
package integration

import (
	"strings"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// TestWriteBatchCommitsRecordsAndCursorInOneTransaction is the durable-ack
// arm: after a restart on the same database, both halves of the batch are
// there.
func TestWriteBatchCommitsRecordsAndCursorInOneTransaction(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	sidecar := fileProducer(store.client())
	cursor := cursorState("16777232:900001", "/transcripts/a.jsonl", 4096, []byte(`{"partial":`))

	// Act.
	sidecar.writeWithCursor(ctx, t, cursor,
		sidecar.agentEntry("w-durable-1", "u-main-line-1", frameLine(agentID("main"), responseFrame("main", "act-1", "first"))),
	)
	store.restart()

	// Assert.
	after, cancelAfter := callContext(t)
	defer cancelAfter()
	cli := store.client()

	got := sidecarCursors(after, t, cli, nil)
	if len(got) != 1 {
		t.Fatalf("after restart the store holds %d cursors, want 1", len(got))
	}
	if got[0].GetFileId() != cursor.GetFileId() || got[0].GetOffset() != cursor.GetOffset() {
		t.Fatalf("cursor survived as %v, want file_id=%q offset=%d", got[0], cursor.GetFileId(), cursor.GetOffset())
	}
	if string(got[0].GetCarry()) != string(cursor.GetCarry()) {
		t.Fatalf("cursor carry survived as %q, want %q", got[0].GetCarry(), cursor.GetCarry())
	}

	page := openSession(after, t, cli, "main", nil)
	assertTexts(t, "the main agent's book after restart", pageTexts(page.GetPage()), []string{"first"})
	store.assertNoErrorRecords()
}

// TestWriteBatchReplayIsAbsorbedAsSuccess is the replay arm: the same
// write_ids sent twice are the SAME success arm and duplicate no lines.
func TestWriteBatchReplayIsAbsorbedAsSuccess(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	batch := []*storev1.StoreEntry{
		shim.agentEntry("w-replay-1", "u-main-line-1", frameLine(agentID("main"), responseFrame("main", "act-1", "alpha"))),
		shim.agentEntry("w-replay-2", "u-main-line-2", frameLine(agentID("main"), responseFrame("main", "act-2", "beta"))),
	}

	// Act.
	shim.write(ctx, t, batch...)
	shim.write(ctx, t, batch...)

	// Assert.
	page := openSession(ctx, t, store.client(), "main", nil)
	assertTexts(t, "the book after a replayed batch", pageTexts(page.GetPage()), []string{"beta", "alpha"})
	store.assertNoErrorRecords()
}

// TestWriteBatchWithOneInvalidEntryCommitsNothing is the all-or-nothing arm:
// a single bad entry rolls the whole transaction back, records AND cursor.
func TestWriteBatchWithOneInvalidEntryCommitsNothing(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	sidecar := fileProducer(store.client())
	cursor := cursorState("16777232:900002", "/transcripts/b.jsonl", 8192, nil)
	good := sidecar.agentEntry("w-partial-1", "u-main-line-1", frameLine(agentID("main"), responseFrame("main", "act-1", "would-be-durable")))
	// THE OFFENDING ENTRY IS ONE ONLY THE STORAGE LAYER CAN CLASSIFY, and it is
	// LAST. The server's envelope validation runs before any transaction opens,
	// so a bad envelope proves nothing about the transaction — it proves the
	// request never reached one. A frame whose agent_id is empty passes the
	// envelope check and is refused inside the routing, with the good entry
	// already written in the same transaction: only a rollback keeps it out.
	bad := sidecar.agentEntry("w-partial-2", "u-main-line-2", &storev1.StoreAgentUpdate{
		AgentInfo: &storev1.StoreAgentUpdate_ServeableFrame{
			ServeableFrame: &storev1.StorePageLine{
				Book:      &storev1.StorePageLine_PageAgentId{PageAgentId: agentID("main")},
				AgentItem: frameItem(responseFrame("", "act-2", "invalid")),
			},
		},
	})

	// Act.
	failure := sidecar.writeExpectingFailure(ctx, t, cursor, good, bad)

	// Assert: the arm names WHICH entry and which field, so the producer's own
	// logs can say what it sent wrong without parsing prose.
	assertWriteInvalidRequest(t, failure, "entries[1].agent_update.serveable_frame.agent_item.agent_frame.agent_id")
	if !strings.Contains(failure.GetDetail(), "entries[1]") {
		t.Errorf("the detail %q does not name the offending entry index", failure.GetDetail())
	}
	if !strings.Contains(failure.GetDetail(), "w-partial-2") && !strings.Contains(failure.GetDetail(), "entries[1]") {
		t.Errorf("the detail %q identifies neither the entry nor its write_id", failure.GetDetail())
	}
	store.restart()

	after, cancelAfter := callContext(t)
	defer cancelAfter()
	cli := store.client()

	if cursors := sidecarCursors(after, t, cli, nil); len(cursors) != 0 {
		t.Errorf("a refused batch advanced the cursor to %v; a failed transaction commits nothing", cursors)
	}
	// Nothing committed at all — not even the agent row the good entry's first
	// sight would have created, which is a stronger statement than an empty page.
	openUnknownAgent(after, t, cli, "main")
}

// TestWriteBatchWithoutCursorAdvanceLeavesCursorsUntouched is the stream-plane
// arm: a producer with no file to be positioned in never moves a cursor.
func TestWriteBatchWithoutCursorAdvanceLeavesCursorsUntouched(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	shim := streamProducer(cli)
	cursor := cursorState("16777232:900003", "/transcripts/c.jsonl", 128, nil)
	sidecar.writeWithCursor(ctx, t, cursor,
		sidecar.agentEntry("w-file-1", "u-main-line-1", frameLine(agentID("main"), responseFrame("main", "act-1", "from-file"))),
	)

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-stream-1", "u-main-line-2", frameLine(agentID("main"), responseFrame("main", "act-2", "from-stream"))),
	)

	// Assert.
	got := sidecarCursors(ctx, t, cli, nil)
	if len(got) != 1 {
		t.Fatalf("the store holds %d cursors after a stream-plane write, want the sidecar's 1", len(got))
	}
	if got[0].GetOffset() != cursor.GetOffset() {
		t.Errorf("a stream-plane write moved the cursor to %d, want it left at %d", got[0].GetOffset(), cursor.GetOffset())
	}
	store.assertNoErrorRecords()
}

// TestACursorOnlyBatchIsDurablySuccessful: a sidecar that read bytes yielding
// no entries — a partial line, a block of records it had already absorbed —
// must still make its file position durable. Refusing the batch would leave the
// reader re-reading the same bytes forever, so the cursor alone is a legal
// batch, and its success means the same durable thing every other success does.
func TestACursorOnlyBatchIsDurablySuccessful(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	sidecar := fileProducer(store.client())
	cursor := cursorState("16777232:900010", "/transcripts/partial.jsonl", 2048, []byte(`{"partial":`))

	// Act: no entries at all.
	sidecar.writeWithCursor(ctx, t, cursor)
	store.restart()

	// Assert.
	after, cancelAfter := callContext(t)
	defer cancelAfter()
	got := sidecarCursors(after, t, store.client(), nil)
	if len(got) != 1 {
		t.Fatalf("after restart the store holds %d cursors, want the cursor-only batch's 1", len(got))
	}
	if got[0].GetOffset() != cursor.GetOffset() || string(got[0].GetCarry()) != string(cursor.GetCarry()) {
		t.Fatalf("the cursor-only batch survived as %v, want offset=%d carry=%q", got[0], cursor.GetOffset(), cursor.GetCarry())
	}
	store.assertNoErrorRecords()
}

// TestCursorsPerFileLatestWins: the cursor table is keyed by the file's stable
// identity, so one file has exactly one position and the newest advance is it.
// Two files are two rows, and the file_id filter answers about one of them.
func TestCursorsPerFileLatestWins(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	const fileA = "16777232:900020"
	const fileB = "16777232:900021"
	sidecar.writeWithCursor(ctx, t, cursorState(fileA, "/transcripts/a.jsonl", 100, nil),
		sidecar.agentEntry("w-cur-a1", "u-cur-a1", frameLine(agentID("main"), responseFrame("main", "act-a1", "A1"))))
	sidecar.writeWithCursor(ctx, t, cursorState(fileB, "/transcripts/b.jsonl", 700, nil),
		sidecar.agentEntry("w-cur-b1", "u-cur-b1", frameLine(agentID("main"), responseFrame("main", "act-b1", "B1"))))

	// Act: file A advances again.
	sidecar.writeWithCursor(ctx, t, cursorState(fileA, "/transcripts/a.jsonl", 900, []byte("tail")),
		sidecar.agentEntry("w-cur-a2", "u-cur-a2", frameLine(agentID("main"), responseFrame("main", "act-a2", "A2"))))

	// Assert: two rows, A at its newest position.
	all := sidecarCursors(ctx, t, cli, nil)
	if len(all) != 2 {
		t.Fatalf("the store holds %d cursors, want one per file (2)", len(all))
	}
	filter := fileA
	only := sidecarCursors(ctx, t, cli, &filter)
	if len(only) != 1 {
		t.Fatalf("the file_id filter answered %d cursors, want exactly the one file's", len(only))
	}
	if only[0].GetFileId() != fileA {
		t.Fatalf("the file_id filter answered file %q, want %q", only[0].GetFileId(), fileA)
	}
	if only[0].GetOffset() != 900 {
		t.Errorf("the file's cursor is at offset %d, want the latest advance's 900", only[0].GetOffset())
	}
	if string(only[0].GetCarry()) != "tail" {
		t.Errorf("the file's carry is %q, want the latest advance's %q", only[0].GetCarry(), "tail")
	}
	store.assertNoErrorRecords()
}

// TestTheFileIdFilterAnswersEmptyForAFileWithNoCursor: an unknown file is an
// empty answer, never a refusal — the sidecar asks about a file precisely
// because it does not know whether it has a position in it yet.
func TestTheFileIdFilterAnswersEmptyForAFileWithNoCursor(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	sidecar.writeWithCursor(ctx, t, cursorState("16777232:900030", "/transcripts/known.jsonl", 10, nil))

	// Act.
	unknown := "16777232:900031"
	got := sidecarCursors(ctx, t, cli, &unknown)

	// Assert.
	if len(got) != 0 {
		t.Fatalf("the file_id filter answered %v for a file with no cursor, want nothing", got)
	}
	store.assertNoErrorRecords()
}
