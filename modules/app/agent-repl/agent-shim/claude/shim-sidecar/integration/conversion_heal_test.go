package integration

// conversion_heal_test.go — SUBJECT: a conversion change heals the sidecar's
// rows.
//
// A store a pre-versioning sidecar wrote holds cursors with no conversion
// bookkeeping, and among its rows `prompt:<uuid>` rows the old conversion
// minted for records no person typed (task notifications above all). The
// current sidecar meets such a cursor, re-reads the transcript from its start
// under the current conversion, and names in each batch the rows a record no
// longer converts to (EntryBatch.retirements), while a row a record still
// converts to is written again under its own key.
//
// What the store then does with those batches -- takes a retired line out of
// every page, tells a standing watch on the `retired` arm, and restamps an
// unchanged row in place without publishing it -- is the store's own, tested
// in shim-store's suite (integration/heal_test.go, internal/db/write_test.go).
// Here the store is the fake, holding the pre-versioning cursor.

import (
	"context"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
)

const (
	healCwd          = "/work/conversion-heal-probe"
	healSession      = "c1c1c1c1-c1c1-4c1c-8c1c-c1c1c1c1c1c1"
	healTypedUUID    = "c1c1c1c1-0000-4000-8000-00000000c0p1"
	healNoticeUUID   = "c1c1c1c1-0000-4000-8000-00000000c0n1"
	healSentinelUUID = "c1c1c1c1-0000-4000-8000-00000000c0s1"
)

// healTypedPrompt is a user record a person typed, parentless so it is its
// own prompt row.
func healTypedPrompt(uuid, text string) string {
	return `{"type":"user","uuid":"` + uuid + `","sessionId":"` + healSession + `","cwd":"` + healCwd + `","isSidechain":false,"entrypoint":"cli","timestamp":"2026-09-27T12:00:00.000Z",` +
		`"message":{"role":"user","content":[{"type":"text","text":"` + text + `"}]}}`
}

// healNotice is a task notification: the old conversion minted a prompt for
// it, and the current one never does.
func healNotice() string {
	return `{"type":"user","uuid":"` + healNoticeUUID + `","sessionId":"` + healSession + `","cwd":"` + healCwd + `","isSidechain":false,"entrypoint":"cli","origin":{"kind":"task-notification"},` +
		`"timestamp":"2026-09-27T12:00:02.000Z","message":{"role":"user","content":"<task-notification>\n<task-id>bsh1</task-id>\n<status>completed</status>\n</task-notification>"}}`
}

// healFixture is a store holding one transcript's cursor as a pre-versioning
// sidecar left it, and the transcript itself.
type healFixture struct {
	store  *fakeStore
	file   *growingFile
	tree   *vendorTree
	socket string
}

// seedPreVersioningStore writes the transcript (a typed prompt and a task
// notification) and seeds the store with the cursor a pre-versioning sidecar
// left for it: at the file's end, with no conversion stated. No sidecar is
// running when it returns.
func seedPreVersioningStore(t *testing.T) *healFixture {
	t.Helper()
	fx := &healFixture{tree: newVendorTree(t), socket: shortSocketPath(t, "store")}
	fx.store = startFakeStoreAt(t, fx.socket)
	fx.file = newGrowingFile(t, fx.tree.sessionPath(cwdSlug(healCwd), healSession))
	fx.file.AppendLine(healTypedPrompt(healTypedUUID, "fix the build"))
	fx.file.AppendLine(healNotice())
	fx.store.SeedCursors(&storev1.CursorState{
		FileId: fileID(t, fx.file.Path()),
		Path:   fx.file.Path(),
		Offset: fx.file.Offset(),
	})
	return fx
}

// awaitHealed waits for the transcript's cursor to reach its end under the
// current conversion, with nothing left to re-derive. The pre-versioning
// cursor already stands at the end, so the offset alone would be satisfied
// before the heal read a byte: the conversion it states is the signal.
func awaitHealed(ctx context.Context, t *testing.T, fx *healFixture) {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		cs := cursorByPath(ctx, t, fx.store.Client, fx.file.Path())
		conv := cs.GetConversion()
		if cs.GetOffset() >= fx.file.Offset() && conv.GetCurrent() != nil && conv.GetVersion() == convert.ConversionVersion {
			return
		}
		select {
		case <-ctx.Done():
			t.Fatalf("the transcript's cursor never stood at its end under conversion %d; last %v", convert.ConversionVersion, cs)
		case <-tick.C:
		}
	}
}

// retirementsOf answers every retirement the sidecar wrote, by upsert key.
func retirementsOf(batches []*storev1.WriteBatchRequest) map[string]*storev1.StoreRetirement {
	out := map[string]*storev1.StoreRetirement{}
	for _, b := range batches {
		for _, r := range b.GetBatch().GetRetirements() {
			out[r.GetUpsertKey()] = r
		}
	}
	return out
}

func TestAHealRetiresThePromptTheOldConversionMintedForTheNotification(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fx := seedPreVersioningStore(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fx.socket, fx.tree))
	awaitHealed(ctx, t, fx)

	// Assert: retired under the version that no longer converts it.
	retired, ok := retirementsOf(fx.store.Batches())[convert.PromptKey(healNoticeUUID)]
	if !ok {
		t.Fatalf("the heal never retired the notification's prompt row; retirements: %v", retirementsOf(fx.store.Batches()))
	}
	if retired.GetConversionVersion() != convert.ConversionVersion {
		t.Fatalf("the retirement states conversion %d, want %d", retired.GetConversionVersion(), convert.ConversionVersion)
	}
}

func TestAHealRewritesAStillValidPromptUnderItsOwnKeyAndNeverRetiresIt(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fx := seedPreVersioningStore(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fx.socket, fx.tree))
	awaitHealed(ctx, t, fx)

	// Assert: never retired, and written again under its own key at the current
	// version, which the store restamps in place.
	key := convert.PromptKey(healTypedUUID)
	if _, retired := retirementsOf(fx.store.Batches())[key]; retired {
		t.Fatalf("the heal retired the typed prompt %q, which the current conversion still produces", key)
	}
	rewritten := false
	for _, e := range entriesOf(fx.store.Batches()) {
		rewritten = rewritten || (e.GetUpsertKey() == key && e.GetConversionVersion() == convert.ConversionVersion)
	}
	if !rewritten {
		t.Fatalf("the heal never re-wrote the typed prompt %q under conversion %d", key, convert.ConversionVersion)
	}
}

func TestASecondRunAfterAHealReDerivesNothing(t *testing.T) {
	t.Parallel()
	// Arrange: the heal ran to completion and the sidecar stopped.
	ctx, cancel := testContext(t)
	defer cancel()
	fx := seedPreVersioningStore(t)
	healer := startSidecar(t, defaultSidecarOptions(t, fx.socket, fx.tree))
	awaitHealed(ctx, t, fx)
	healer.Stop()
	opts := defaultSidecarOptions(t, fx.socket, fx.tree)

	// Act.
	startSidecar(t, opts)
	fx.file.AppendLine(healTypedPrompt(healSentinelUUID, "the sentinel"))
	awaitCursorAtLeast(ctx, t, fx.store.Client, fx.file.Path(), fx.file.Offset())

	// Assert.
	for _, rec := range readLog(t, opts.LogPath) {
		if rec.Operation == "conversion-heal" {
			t.Fatalf("the second run re-derived again: %v", rec)
		}
	}
}
