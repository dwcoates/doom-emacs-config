package integration

import "testing"

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
// SERVED form, and this reader keeps the transcript record durable and
// investigable as an UNSERVED item. These subjects pin both halves against the
// real reader driving the real capture's own hook attachments.

// TestAHookOnTheFilePlaneServesNoActivityRow is the ruling's own assertion: the
// sidecar wrote the hook down, and wrote NOTHING any page will serve.
func TestAHookOnTheFilePlaneServesNoActivityRow(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/hook-plane-ownership-probe"
	slug := cwdSlug(cwd)
	session := "8d8d8d8d-8d8d-48d8-88d8-8d8d8d8d8d8d"
	// The capture's own hook attachment, not a hand-authored one.
	hook := retargetSession(t, decodeRecord(t, captured.Lines[2]), session, cwd)
	uuid, _ := hook["uuid"].(string)
	if uuid == "" {
		t.Fatalf("the capture's hook attachment carries no uuid")
	}

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	g.AppendLine(encodeRecord(t, hook))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the record landed, and it landed UNSERVED.
	e := entryByUpsertKey(fake.Entries(), "residue:"+uuid)
	if e == nil {
		t.Fatalf("the hook attachment was not stored at all; keys were %v", upsertKeysOf(fake.Entries()))
	}
	if e.GetAgentUpdate().GetServeableFrame() != nil {
		t.Fatalf("a hook attachment reached a page: %v", e.GetAgentUpdate().GetServeableFrame())
	}
	if got := e.GetAgentUpdate().GetUnservedItem().GetVendorSpecific().GetKind(); got != "attachment/hook_non_blocking_error" {
		t.Fatalf("the hook landed as kind %q, want the vendor's own attachment type kept whole", got)
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
