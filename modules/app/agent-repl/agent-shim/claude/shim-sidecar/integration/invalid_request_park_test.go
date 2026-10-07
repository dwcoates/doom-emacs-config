package integration

import (
	"context"
	"testing"
)

// CRITIQUE 11 — a batch the REAL store refuses as invalid_request parks the file.
//
// Ruling R-S2's two failure arms drive opposite reactions, and the parking half
// has only ever been driven by the fake store's scripted refusal. The real store
// refuses a batch as invalid_request on its own account when an upsert would
// change a row's IDENTITY: `upsert_key` names ONE thing, so a write that would
// move a row into another book, or turn a served page line into an unservable
// residue row, is a different thing wearing the same key.
//
// So the subject CLAIMS the key first. A row is seeded on the sidecar's own
// upsert key — as the other plane's producer, and as an unserved residue row —
// and the batch carrying that key is then a genuine producer defect that this
// store can never accept, however many times it is offered.

// parkingFixture is a real store already holding a row on the key the sidecar's
// first batch for its transcript will carry.
type parkingFixture struct {
	Store   *realStore
	Tree    *vendorTree
	Opts    sidecarOptions
	Session string
	Cwd     string
	File    *growingFile
}

// seedContestedUpsertKey claims `activity:<unit>` for an unservable row, then
// writes the transcript lines that mint that same unit.
func seedContestedUpsertKey(ctx context.Context, t *testing.T, cwd, session string) parkingFixture {
	t.Helper()
	store := startRealStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	slug := cwdSlug(cwd)

	seedDecoyRow(ctx, t, store.Client, "claude-shim:"+session,
		decoyEntry("activity:"+capturedThinking1, "decoy-"+capturedThinking1, "seeded-by-the-suite"))

	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	return parkingFixture{
		Store: store, Tree: tree, Opts: defaultSidecarOptions(t, store.Socket, tree),
		Session: session, Cwd: cwd, File: g,
	}
}

// TestAnInvalidRequestFromTheRealStoreParksTheFileAndStatesTheDefect asserts the
// ONE error record the parking owes: the store's field, the write ids of the
// whole refused batch, and the file position they were read at.
func TestAnInvalidRequestFromTheRealStoreParksTheFileAndStatesTheDefect(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fx := seedContestedUpsertKey(ctx, t, "/work/park-defect-probe",
		"e1e1e1e1-e1e1-4e1e-8e1e-e1e1e1e1e1e1")

	// Act.
	startSidecar(t, fx.Opts)
	rec := awaitLog(ctx, t, fx.Opts.LogPath, "the producer-defect record", func(r logRecord) bool {
		return r.Operation == "producer-defect" && r.Level == "error" && samePathAny(r.Context["path"], fx.File.Path())
	})

	// Assert: the record names WHY, WHAT and WHERE.
	if got, _ := rec.Context["refusal_kind"].(string); got != "invalid_request" {
		t.Errorf("the producer-defect record names refusal_kind %q, wanted invalid_request", got)
	}
	if got, _ := rec.Context["field"].(string); got == "" {
		t.Errorf("the producer-defect record names no field, so the defect is not investigable; its context was %v", rec.Context)
	}
	if ids := contextStrings(rec.Context["write_ids"]); len(ids) == 0 {
		t.Errorf("the producer-defect record names no write_ids; a batch is refused WHOLE and every record of it must be named")
	}
	if _, ok := rec.Context["offset"]; !ok {
		t.Errorf("the producer-defect record names no offset; its context was %v", rec.Context)
	}
}

// TestAParkedFileIsStatedOnceAndReadNoFurther asserts the two consequences: the
// defect is stated ONCE rather than on every re-read, and the file's cursor never
// moves again.
func TestAParkedFileIsStatedOnceAndReadNoFurther(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fx := seedContestedUpsertKey(ctx, t, "/work/park-once-probe",
		"e2e2e2e2-e2e2-4e2e-8e2e-e2e2e2e2e2e2")
	captured := loadCapturedSession(t)

	// Act: after the park, the vendor keeps writing to the parked file, and a
	// SECOND file appears — whose ingest is the signal that many more cycles
	// have run since the refusal.
	startSidecar(t, fx.Opts)
	awaitLog(ctx, t, fx.Opts.LogPath, "the producer-defect record", func(r logRecord) bool {
		return r.Operation == "producer-defect" && samePathAny(r.Context["path"], fx.File.Path())
	})
	fx.File.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[8]), fx.Session, fx.Cwd)))

	otherCwd := "/work/park-once-other-probe"
	otherSession := "e3e3e3e3-e3e3-4e3e-8e3e-e3e3e3e3e3e3"
	// Its line is the SECOND response's, not the first's: the first's unit is the
	// key the decoy row claims, so seeding it here would park this file too and
	// the subject would prove nothing about the files that keep being read.
	other := newGrowingFile(t, fx.Tree.sessionPath(cwdSlug(otherCwd), otherSession))
	other.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[12]), otherSession, otherCwd)))
	awaitCursorAtLeast(ctx, t, fx.Store.Client, other.Path(), other.Offset())

	// Assert: one statement, and no cursor for the parked file at all — its
	// batch never committed, so the store holds nothing for it.
	var stated int
	for _, r := range logsForOperation(readLog(t, fx.Opts.LogPath), "producer-defect") {
		if samePathAny(r.Context["path"], fx.File.Path()) {
			stated++
		}
	}
	if stated != 1 {
		t.Errorf("the producer defect was stated %d times; a parked file states it once, not on every re-read", stated)
	}
	if cs := cursorByPath(ctx, t, fx.Store.Client, fx.File.Path()); cs != nil {
		t.Errorf("the parked file advanced a cursor to %d; nothing more is read from it and its cursor stays where the store has it",
			cs.GetOffset())
	}
}

// TestAnInvalidRequestDoesNotSuspendTheOtherFiles asserts the half that
// separates a producer defect from an outage: the store is reachable and
// answering, so every OTHER file keeps being read.
func TestAnInvalidRequestDoesNotSuspendTheOtherFiles(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fx := seedContestedUpsertKey(ctx, t, "/work/park-other-file-probe",
		"e4e4e4e4-e4e4-4e4e-8e4e-e4e4e4e4e4e4")
	captured := loadCapturedSession(t)
	otherCwd := "/work/park-other-file-second-probe"
	otherSession := "e5e5e5e5-e5e5-4e5e-8e5e-e5e5e5e5e5e5"

	// Act: the second file appears only AFTER the refusal.
	startSidecar(t, fx.Opts)
	awaitLog(ctx, t, fx.Opts.LogPath, "the producer-defect record", func(r logRecord) bool {
		return r.Operation == "producer-defect" && samePathAny(r.Context["path"], fx.File.Path())
	})
	other := newGrowingFile(t, fx.Tree.sessionPath(cwdSlug(otherCwd), otherSession))
	other.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[12]), otherSession, otherCwd)))

	// Assert: it was ingested, and its unit reached its book.
	awaitCursorAtLeast(ctx, t, fx.Store.Client, other.Path(), other.Offset())
	lines := awaitBookUnits(ctx, t, fx.Store.Client, otherSession, capturedThinking2)
	if len(lines) == 0 {
		t.Fatalf("the second file produced no page line; a producer defect in one file must not suspend the others")
	}
	// And production was never suspended over it: no suspension warning names
	// the store as unreachable in this run.
	for _, r := range logsForOperation(readLog(t, fx.Opts.LogPath), "recover-cursors") {
		if r.Level == "warn" {
			t.Errorf("a producer defect opened a store suspension: %v", r.Message)
		}
	}
}
