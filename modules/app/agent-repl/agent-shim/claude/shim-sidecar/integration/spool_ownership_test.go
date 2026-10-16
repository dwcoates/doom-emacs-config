package integration

import (
	"context"
	"testing"
	"time"
)

// SUBJECT 4 — who a spool belongs to, and what happens when nobody claims it.
//
// ONLY WHAT IS RENDERED IS READ. A spool is read for the run a spawning call
// claimed and for nothing else: one that appears BEFORE the transcript line
// naming it is held and re-checked, never read, and stays unread after its
// window lapses — until a launch claims it. A spool whose task id has no a/b/w
// prefix is refused loudly and never read. The /tmp versus /private/tmp
// spellings of one file are one file.
//
// "NEVER READ" IS ASSERTED ON A POSITIVE SIGNAL: the reader re-resolves every
// UNWATCHED spool on every rescan and says so at debug, so a later record naming
// the spool is proof it is still unwatched — a watched file is never resolved
// again. The store then holds no cursor for it and the log no residue.

// awaitRestatedAfter waits for a record of `operation` naming path that was
// written AFTER log index `after`: the next rescan re-resolving a spool it still
// does not watch.
func awaitRestatedAfter(ctx context.Context, t *testing.T, logPath, path, operation string, after int) {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		for i, r := range readLog(t, logPath) {
			if i > after && r.Operation == operation && samePathAny(r.Context["path"], path) {
				return
			}
		}
		select {
		case <-ctx.Done():
			t.Fatalf("no %s record for %s after log index %d: the spool was not re-resolved, so it is being watched", operation, path, after)
		case <-tick.C:
		}
	}
}

// requireNeverRead asserts a spool left no trace of a read: no cursor offered
// for it, and no residue classified out of it.
func requireNeverRead(t *testing.T, fake *fakeStore, logPath, path string) {
	t.Helper()
	if cs := latestCursorFor(fake.Batches(), path); cs != nil {
		t.Fatalf("a spool nothing renders was read: the store was offered a cursor for %s at offset %d", path, cs.GetOffset())
	}
	id := fileID(t, path)
	for _, r := range readLog(t, logPath) {
		if r.Operation == "residue-drop" && r.Context["file_id"] == id {
			t.Fatalf("a spool nothing renders was read: its bytes were classified as residue: %v", r.Context)
		}
	}
}

// logIndexOf answers where in the log a record matching `match` first appears,
// or -1.
func logIndexOf(t *testing.T, logPath string, match func(logRecord) bool) int {
	t.Helper()
	for i, r := range readLog(t, logPath) {
		if match(r) {
			return i
		}
	}
	return -1
}

// TestAnUnclassifiableSpoolPrefixIsRefusedLoudly asserts a task id with no
// known kind prefix is an ERROR — a total-ingestion violation stated out loud.
func TestAnUnclassifiableSpoolPrefixIsRefusedLoudly(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/spool-prefix-probe"
	slug := cwdSlug(cwd)
	session := "30303030-3030-4030-8030-303030303030"
	opts := defaultSidecarOptions(t, fake.Socket, tree)
	// z* is none of b* (shell), a* (agent transcript) or w* (workflow journal).
	spoolPath := tree.spoolPath(slug, session, "z0uncla551f1able")

	// Act: the spool appears after catch-up, so its skip is a steady-state
	// decision stated at INFO rather than a demoted catch-up record.
	startSidecar(t, opts)
	awaitCatchupEnd(ctx, t, opts.LogPath)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("bytes nobody can classify\n"))

	// Assert.
	awaitLog(ctx, t, opts.LogPath, "the unclassifiable-prefix refusal", func(r logRecord) bool {
		return r.Level == "error" && samePathAny(r.Context["path"], spoolPath)
	})
}
