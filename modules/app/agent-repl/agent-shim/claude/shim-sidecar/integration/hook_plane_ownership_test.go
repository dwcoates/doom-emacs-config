package integration

import (
	"strings"
	"testing"
)

// SUBJECT — THE STREAM PLANE OWNS THE SERVED HOOK ROW (ruling 2026-09-04).
//
// A hook reaches this system on BOTH planes by vendor design, and the vendor
// hands them DISJOINT identity material: the stream's `hook_started` /
// `hook_response` pair carries a `hook_id` and never a `tool_use_id`, the
// transcript's attachment carries a `toolUseID` and never a `hook_id`, and the
// two records' uuids differ. No upsert key can name one firing on both, so a
// hook converted on both planes drew TWO feed rows nothing downstream could
// reconcile — the file plane's the poorer of the two, since it writes no start
// frame and so carries neither the hook's name nor its event.
//
// The ruling is R15's precedent applied to hooks: the shim's row is the one
// SERVED form, and this reader still READS the transcript record and states
// what it was. Under the 2026-09-13 residue ruling that classification is where
// the record ends — it is named and withheld rather than stored as an unserved
// item — so the reader's own account of it is what these subjects pin, against
// the real reader driving the real capture's own hook attachments.

// TestAHookOnTheFilePlaneServesNoActivityRow is the ruling's own assertion: the
// sidecar read the hook, said what it was, and served NOTHING.
//
// RE-AIMED: it asserted the record landed under `residue:<uuid>` carrying the
// vendor's attachment kind. No residue row is written, so the kind is observable
// as the withheld record's own label instead.
func TestAHookOnTheFilePlaneServesNoActivityRow(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/hook-plane-ownership-probe"
	slug := cwdSlug(cwd)
	session := "8d8d8d8d-8d8d-48d8-88d8-8d8d8d8d8d8d"
	// The capture's own hook attachment, not a hand-authored one.
	hook := retargetSession(t, decodeRecord(t, captured.Lines[2]), session, cwd)
	uuid, _ := hook["uuid"].(string)
	if uuid == "" {
		t.Fatalf("the capture's hook attachment carries no uuid")
	}
	// The withheld-record accounts are verbose, so the subject asks for them.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	g.AppendLine(encodeRecord(t, hook))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the reader kept the vendor's own attachment type whole in what it
	// said it withheld, and the hook reached no row of any sort.
	awaitResidueWithheld(ctx, t, opts.LogPath, "vendor_specific/attachment/hook_non_blocking_error")
	requireNoResidueStored(t, fake.Entries())
	for _, e := range fake.Entries() {
		if strings.Contains(e.GetUpsertKey(), uuid) {
			t.Fatalf("the hook attachment produced a row keyed %q", e.GetUpsertKey())
		}
	}
}

// TestNoFilePlaneEntryCarriesAHookActivity is the stronger half: not merely
// "this record is unserved" but "this reader mints no hook activity AT ALL", so
// no key spelling anywhere in the converter can regrow the duplicate row.
func TestNoFilePlaneEntryCarriesAHookActivity(t *testing.T) {
	t.Parallel()
	// Arrange: the WHOLE capture, which carries several hook attachments of
	// three different outcome kinds.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert. The served count is checked FIRST so "no hook was served" cannot
	// pass by the reader having served nothing at all.
	var served int
	for _, e := range fake.Entries() {
		if e.GetAgentUpdate().GetServeableFrame() == nil {
			continue
		}
		served++
		if hook := activityOf(e.GetAgentUpdate().GetServeableFrame()).GetHook(); hook != nil {
			t.Fatalf("the file plane served a hook activity under key %q: %v", e.GetUpsertKey(), hook)
		}
	}
	if served == 0 {
		t.Fatalf("the reader served no page line at all over the whole capture, so the subject asserted nothing")
	}
}
