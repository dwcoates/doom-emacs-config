package db

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// ---- R-A5: an upsert supersedes content, never identity ----

func TestWriteBatchRefusesAnUpsertThatMovesTheRowToAnotherBook(t *testing.T) {
	// Arrange: every pointer already served for this row names a line of
	// agent-1's page. Moving the row would leave those pointers naming a row in
	// a book the caller never asked about.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))

	// Act
	_, err := d.WriteBatch(ctx(), "producer", batch(
		pageEntry("w2", "u1", "agent-2", frameItem(activityFrame("agent-2", "act-1", prose())))))

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	if got := RefusalSite(err); got != SiteUpsertChangesIdentity {
		t.Fatalf("site = %q, want %q", got, SiteUpsertChangesIdentity)
	}
}

// TestABookMoveEntryRollsBackEveryLegitimateSiblingInItsBatch is the
// data-loss boundary the sidecar's quoted-context fix rests on (ledger row 51):
// a WriteBatch is atomic, so ONE refused entry rolls back the WHOLE transaction
// and every LEGITIMATE new entry beside it is lost too. This is why a producer
// must never PUT a book-moving entry in a batch — "log the refusal softer" would
// still discard the batch's good rows — and why the fix removes the bad entry at
// the producer instead.
func TestABookMoveEntryRollsBackEveryLegitimateSiblingInItsBatch(t *testing.T) {
	// Arrange: an existing row under agent-1, so a later move to agent-2 is a
	// genuine identity change.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))

	// Act: one batch carries a brand-new, perfectly legitimate line AND the
	// offending book-move entry.
	_, err := d.WriteBatch(ctx(), "producer", batch(
		pageEntry("w2", "u2", "agent-1", frameItem(activityFrame("agent-1", "act-2", prose()))),
		pageEntry("w3", "u1", "agent-2", frameItem(activityFrame("agent-2", "act-1", prose())))))

	// Assert: the batch was refused, and the legitimate sibling committed
	// NOTHING — the whole transaction rolled back.
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry WHERE upsert_key = 'u2'`); got != 0 {
		t.Fatalf("the legitimate sibling produced %d rows, want 0 — a refused batch commits nothing", got)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM write_ledger WHERE write_id IN ('w2','w3')`); got != 0 {
		t.Fatalf("ledger rows for the refused batch = %d, want 0", got)
	}
}

func TestAnIdentityChangingUpsertCommitsNothing(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))

	// Act
	if _, err := d.WriteBatch(ctx(), "producer", batch(
		pageEntry("w2", "u1", "agent-2", frameItem(activityFrame("agent-2", "act-1", prose()))))); err == nil {
		t.Fatal("WriteBatch accepted a write that changes a row's book")
	}

	// Assert
	if got := scalar[string](t, d, `SELECT book_agent_id FROM entry WHERE upsert_key = 'u1'`); got != "agent-1" {
		t.Fatalf("book = %q, want agent-1 — nothing may have been committed", got)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM write_ledger WHERE write_id = 'w2'`); got != 0 {
		t.Fatalf("ledger rows for the refused write = %d, want 0", got)
	}
}

func TestWriteBatchRefusesAnUpsertThatChangesTheRowsKind(t *testing.T) {
	// Arrange: a served page line becoming an unservable residue row under a
	// pointer that still exists is not a supersession — it is a different thing
	// wearing the same key.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))

	// Act
	_, err := d.WriteBatch(ctx(), "producer", batch(unservedEntry("w2", "u1",
		&storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unknown{
			Unknown: &storev1.StoreUnknown{Discriminator: "widget", Raw: rawRecord("widget")},
		}})))

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
	_, err := d.WriteBatch(ctx(), "producer", batch(
		pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-2", "act-1", prose())))))

	// Assert
	if got := RefusalSite(err); got != SitePageBookMismatch {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SitePageBookMismatch)
	}
}

func TestWriteBatchRefusesAPromptWhoseBookDisagreesWithItsRecipient(t *testing.T) {
	// Arrange: a prompt's book IS its one recipient.
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "producer", batch(pageEntry("w1", "u1", "agent-1", promptItem("agent-2"))))

	// Assert
	if got := RefusalSite(err); got != SitePageBookMismatch {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SitePageBookMismatch)
	}
}

func TestAMismatchedPageLineCommitsNothing(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	if _, err := d.WriteBatch(ctx(), "producer", batch(
		pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-2", "act-1", prose()))))); err == nil {
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
	_, err := d.WriteBatch(ctx(), "producer", batch(unservedEntry("w1", "u1",
		&storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_VendorSpecific{
			VendorSpecific: &storev1.StoreVendorSpecific{Kind: "hook"},
		}})))

	// Assert
	if got := RefusalSite(err); got != SiteResidueRawUnset {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SiteResidueRawUnset)
	}
}

func TestWriteBatchRefusesUnknownResidueWithNoRawRecord(t *testing.T) {
	// Arrange: a record we do not model is worth keeping only verbatim.
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "producer", batch(unservedEntry("w1", "u1",
		&storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unknown{
			Unknown: &storev1.StoreUnknown{Discriminator: "widget"},
		}})))

	// Assert
	if got := RefusalSite(err); got != SiteResidueRawUnset {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SiteResidueRawUnset)
	}
}

func TestWriteBatchRefusesUnparsedResidueWithEmptyRawBytes(t *testing.T) {
	// Arrange: an unreadable record is investigable only through its bytes.
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "producer", batch(unservedEntry("w1", "u1",
		&storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unparsed{
			Unparsed: &storev1.StoreUnparsed{Source: "t.jsonl", ParseError: "unexpected EOF"},
		}})))

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
