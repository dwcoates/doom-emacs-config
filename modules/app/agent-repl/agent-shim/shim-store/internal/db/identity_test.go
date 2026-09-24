package db

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// ---- R-A5: an upsert supersedes content, never identity ----

// TestWriteBatchSkipsAnUpsertThatMovesTheRowToAnotherBook is the re-ingest
// idempotency boundary (ledger row 52, superseding row 51): a corrected
// converter re-reading the corpus writes an already-stored key under its
// now-right book. Moving the row would teleport every pointer already handed out
// for it, so the store KEEPS the stored row and SKIPS this entry — reported in
// the WriteResult, never a batch-fatal refusal.
func TestWriteBatchSkipsAnUpsertThatMovesTheRowToAnotherBook(t *testing.T) {
	// Arrange: every pointer already served for this row names a line of
	// agent-1's page.
	d, s := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))

	// Act
	result, err := d.WriteBatch(ctx(), "producer", WriteInteractive, batch(
		pageEntry("w2", "u1", "agent-2", frameItem(activityFrame("agent-2", "act-1", prose())))), nil)

	// Assert: no refusal, the entry is skipped and reported, nothing was written.
	if err != nil {
		t.Fatalf("error = %v, want nil — a legacy book-conflict is skipped, not refused", err)
	}
	if result.Written != 0 {
		t.Fatalf("written = %d, want 0 — the conflicting entry was skipped", result.Written)
	}
	if len(result.Skipped) != 1 {
		t.Fatalf("skipped = %d, want 1", len(result.Skipped))
	}
	if got := result.Skipped[0]; got.UpsertKey != "u1" || got.FromBook != "agent-1" || got.ToBook != "agent-2" {
		t.Fatalf("skipped[0] = %+v, want {u1 agent-1 agent-2}", got)
	}
	if got := scalar[string](t, d, `SELECT book_agent_id FROM entry WHERE upsert_key = 'u1'`); got != "agent-1" {
		t.Fatalf("book = %q, want agent-1 — the stored row is kept unchanged", got)
	}
	// The store logs the per-entry skip at DEBUG — a benign idempotency outcome
	// it cannot contextualize; the skip is still RETURNED in result.Skipped
	// (asserted above) for the sidecar to summarize.
	s.assertLogged(t, "debug", "the stored row is kept")
}

// TestABookConflictEntryDoesNotLoseItsLegitimateSiblings is the no-data-loss
// boundary (ledger row 52, superseding the row-51 boundary this replaces): a
// batch mixing one legacy book-conflict entry with genuinely-new entries now
// COMMITS the new entries — previously the atomic batch rolled them all back.
func TestABookConflictEntryDoesNotLoseItsLegitimateSiblings(t *testing.T) {
	// Arrange: an existing row under agent-1, so a later move to agent-2 is the
	// legacy book-conflict.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))

	// Act: one batch carries a brand-new, perfectly legitimate line AND the
	// legacy book-conflict entry.
	result, err := d.WriteBatch(ctx(), "producer", WriteInteractive, batch(
		pageEntry("w2", "u2", "agent-1", frameItem(activityFrame("agent-1", "act-2", prose()))),
		pageEntry("w3", "u1", "agent-2", frameItem(activityFrame("agent-2", "act-1", prose())))), nil)

	// Assert: the batch committed the sibling and only skipped the conflict.
	if err != nil {
		t.Fatalf("error = %v, want nil", err)
	}
	if result.Written != 1 || len(result.Skipped) != 1 {
		t.Fatalf("written=%d skipped=%d, want written=1 skipped=1", result.Written, len(result.Skipped))
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry WHERE upsert_key = 'u2'`); got != 1 {
		t.Fatalf("the legitimate sibling produced %d rows, want 1 — a skipped conflict must not lose it", got)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM write_ledger WHERE write_id = 'w2'`); got != 1 {
		t.Fatalf("ledger rows for the committed sibling = %d, want 1", got)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM write_ledger WHERE write_id = 'w3'`); got != 0 {
		t.Fatalf("ledger rows for the skipped conflict = %d, want 0 — a skip records no applied write", got)
	}
}

// TestReIngestingTheSameCorpusTwiceIsANoOp asserts idempotency: re-writing the
// exact same entry under its already-stored book absorbs (its write_id landed
// before), and re-writing it under a DIFFERENT book skips — either way the
// stored row is untouched and no refusal occurs.
func TestReIngestingTheSameCorpusTwiceIsANoOp(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))
	before := scalar[int](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'u1'`)

	// Act: the same write_id replays (absorbed), and a re-booked re-ingest skips.
	absorb, err := d.WriteBatch(ctx(), "producer", WriteInteractive, batch(
		pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose())))), nil)
	if err != nil {
		t.Fatalf("replay error = %v, want nil", err)
	}
	skip, err := d.WriteBatch(ctx(), "producer", WriteInteractive, batch(
		pageEntry("w2", "u1", "agent-2", frameItem(activityFrame("agent-2", "act-1", prose())))), nil)
	if err != nil {
		t.Fatalf("re-book error = %v, want nil", err)
	}

	// Assert: nothing changed on the stored row.
	if absorb.Absorbed != 1 || absorb.Written != 0 {
		t.Fatalf("replay result = %+v, want absorbed=1 written=0", absorb)
	}
	if skip.Written != 0 || len(skip.Skipped) != 1 {
		t.Fatalf("re-book result = %+v, want written=0 skipped=1", skip)
	}
	if got := scalar[int](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'u1'`); got != before {
		t.Fatalf("write_seq moved from %d to %d — a no-op re-ingest must not touch the row", before, got)
	}
}

func TestWriteBatchRefusesAnUpsertThatChangesTheRowsKind(t *testing.T) {
	// Arrange: a served page line becoming an unservable residue row under a
	// pointer that still exists is not a supersession — it is a different thing
	// wearing the same key.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))

	// Act
	_, err := d.WriteBatch(ctx(), "producer", WriteInteractive, batch(unservedEntry("w2", "u1",
		&storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unknown{
			Unknown: &storev1.StoreUnknown{Discriminator: "widget", Raw: rawRecord("widget")},
		}})), nil)

	// Assert
	if got := RefusalSite(err); got != SiteUpsertChangesIdentity {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SiteUpsertChangesIdentity)
	}
}

func TestWriteBatchAcceptsAnUpsertThatKeepsTheRowsIdentity(t *testing.T) {
	// Arrange: the ordinary settling of a unit, which must stay legal.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", proseSaying("A")))))

	// Act
	writeOK(t, d, pageEntry("w2", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", proseSaying("A settled")))))

	// Assert
	if got := scalar[string](t, d, `SELECT write_id FROM entry WHERE upsert_key = 'u1'`); got != "w2" {
		t.Fatalf("write_id = %q, want the supersession to have landed", got)
	}
}

// ---- R-A6: the envelope and the frame must agree ----

func TestWriteBatchRefusesAPageLineWhoseBookDisagreesWithItsFrame(t *testing.T) {
	// Arrange: filing an agent's own words under another agent's name produces
	// a book that reads as a conversation that never happened.
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "producer", WriteInteractive, batch(
		pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-2", "act-1", prose())))), nil)

	// Assert
	if got := RefusalSite(err); got != SitePageBookMismatch {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SitePageBookMismatch)
	}
}

func TestWriteBatchRefusesAPromptWhoseBookDisagreesWithItsRecipient(t *testing.T) {
	// Arrange: a prompt's book IS its one recipient.
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "producer", WriteInteractive, batch(pageEntry("w1", "u1", "agent-1", promptItem("agent-2"))), nil)

	// Assert
	if got := RefusalSite(err); got != SitePageBookMismatch {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SitePageBookMismatch)
	}
}

func TestWriteBatchRefusesAPeerMessageWhoseBookDisagreesWithItsRecipient(t *testing.T) {
	// Arrange: a peer message's book IS its one recipient, exactly like a prompt.
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "producer", WriteInteractive, batch(pageEntry("w1", "u1", "agent-1", peerItem("agent-2"))), nil)

	// Assert
	if got := RefusalSite(err); got != SitePageBookMismatch {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SitePageBookMismatch)
	}
}

func TestAMismatchedPageLineCommitsNothing(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	if _, err := d.WriteBatch(ctx(), "producer", WriteInteractive, batch(
		pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-2", "act-1", prose())))), nil); err == nil {
		t.Fatal("WriteBatch accepted a page line whose envelope and frame disagree")
	}

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry`); got != 0 {
		t.Fatalf("entry rows = %d, want 0", got)
	}
}

func TestWriteBatchRefusesVendorSpecificResidueWithNoRawRecord(t *testing.T) {
	// Arrange: a row saying only "there was something here" IS the drop residue
	// exists to prevent, dressed up as durability.
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "producer", WriteInteractive, batch(unservedEntry("w1", "u1",
		&storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_VendorSpecific{
			VendorSpecific: &storev1.StoreVendorSpecific{Kind: "hook"},
		}})), nil)

	// Assert
	if got := RefusalSite(err); got != SiteResidueRawUnset {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SiteResidueRawUnset)
	}
}

func TestWriteBatchRefusesUnknownResidueWithNoRawRecord(t *testing.T) {
	// Arrange: a record we do not model is worth keeping only verbatim.
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "producer", WriteInteractive, batch(unservedEntry("w1", "u1",
		&storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unknown{
			Unknown: &storev1.StoreUnknown{Discriminator: "widget"},
		}})), nil)

	// Assert
	if got := RefusalSite(err); got != SiteResidueRawUnset {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SiteResidueRawUnset)
	}
}

func TestWriteBatchRefusesUnparsedResidueWithEmptyRawBytes(t *testing.T) {
	// Arrange: an unreadable record is investigable only through its bytes.
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "producer", WriteInteractive, batch(unservedEntry("w1", "u1",
		&storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unparsed{
			Unparsed: &storev1.StoreUnparsed{Source: "t.jsonl", ParseError: "unexpected EOF"},
		}})), nil)

	// Assert
	if got := RefusalSite(err); got != SiteResidueRawUnset {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SiteResidueRawUnset)
	}
}

func TestKeepaliveResidueNeedsNoRawRecord(t *testing.T) {
	// Arrange: a keep-alive is a WELL-FORMED fact with no book, not material
	// that failed to convert — there is nothing verbatim to preserve.
	d, _ := newStore(t)

	// Act
	writeOK(t, d, unservedEntry("w1", "u1", &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_Keepalive{Keepalive: promptItem("agent-1")},
	}))

	// Assert
	if got := scalar[string](t, d, `SELECT kind FROM entry WHERE upsert_key = 'u1'`); got != kindKeepalive {
		t.Fatalf("kind = %q, want %q", got, kindKeepalive)
	}
}

// ---- landing 4: the two identities are minted to the same bytes ----

func TestARunAnnouncedUnderItsOwnUnitIdConvergesOnOneRow(t *testing.T) {
	// Arrange: landing 4 mints DetachedWorkId.value AS the unit's
	// AgentActivityId, so the announcement's handle and the run frame's run id
	// are the same bytes. The origin-unit lookup still runs first and must not
	// produce a second row when the two identities coincide.
	d, _ := newStore(t)
	const shared = "run-and-handle"
	writeOK(t, d, pageEntry("w1", "detached:"+shared, "agent-1",
		frameItem(detachedFrame("agent-1", detachedWork(shared, shared,
			&conversationv1.DetachedWorkDetached_Requested{Requested: &conversationv1.DetachedCauseRequested{}})))))

	// Act
	writeOK(t, d, bashEntry("w2", "bash:"+shared+":start", shared, bashStart()))

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM detached_work`); got != 1 {
		t.Fatalf("detached_work rows = %d, want 1", got)
	}
	if got := scalar[string](t, d, `SELECT work_id FROM detached_work`); got != shared {
		t.Fatalf("work_id = %q, want %q", got, shared)
	}
}
