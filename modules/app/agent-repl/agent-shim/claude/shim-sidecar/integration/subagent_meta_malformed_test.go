package integration

import (
	"os"
	"path/filepath"
	"testing"
)

// SUBJECT — a meta.json that is PRESENT but does not parse.
//
// The two neighbouring subjects cover the meta that is ABSENT (a warning; it is
// expected to appear a moment later) and the one that parses but states no
// `toolUseId` (an error; it will not fix itself). The third shape is a meta
// caught HALF-WRITTEN or corrupted — bytes that are not JSON at all — and it is
// the one a reader is most likely to get wrong, because "it did not parse" is
// exactly the situation in which inventing an identity from the file name feels
// harmless. It is not: the file name is a locator, and naming the agent by it
// mints a second book for one agent that no consumer can reconcile.

// TestAMalformedMetaHoldsTheTranscriptLoudly asserts a truncated meta is
// reported at error and its transcript is held, never tailed under a
// locator-derived identity.
func TestAMalformedMetaHoldsTheTranscriptLoudly(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/subagent-meta-malformed-probe"
	slug := cwdSlug(cwd)
	session := "3d3d3d3d-3d3d-43d3-83d3-3d3d3d3d3d3d"
	opts := defaultSidecarOptions(t, fake.Socket, tree)

	// Act: the REAL meta fixture, cut off mid-object — the shape a reader sees
	// when it looks between the vendor's write and its rename.
	g := writeSubagentTranscript(t, tree, slug, session, corpusSubagentID)
	writeTruncatedSubagentMeta(t, tree, slug, session, corpusSubagentID)
	// THE WHOLE FIXTURE IS ON DISK BEFORE THE READER IS: the subject is a
	// meta that is PRESENT and unusable, and starting the sidecar first
	// leaves a window in which it scans between the transcript's write
	// and the meta's — the ABSENT-meta case, which is a WARNING and is a
	// different subject. Writing both first closes the window with an
	// ordering rather than with a hope about scheduling.
	startSidecar(t, opts)
	rec := awaitLog(ctx, t, opts.LogPath, "the unparsable-meta refusal", func(r logRecord) bool {
		return r.Operation == "discover-meta" && samePathAny(r.Context["path"], g.Path())
	})

	// Assert.
	if rec.Level != "error" {
		t.Errorf("a meta whose bytes are not JSON was reported at %q; a present-and-unusable meta is an error", rec.Level)
	}
	if latestCursorFor(fake.Batches(), g.Path()) != nil {
		t.Errorf("a sidechain whose meta does not parse advanced a cursor; it must not be tailed at all")
	}
	for _, book := range []string{corpusSubagentAgentID, corpusSubagentID} {
		if len(linesForBook(fake.Entries(), book)) != 0 {
			t.Errorf("an unidentifiable sidechain produced page lines in book %q", book)
		}
	}
}

// writeTruncatedSubagentMeta drops the FIRST HALF of the fixture's meta.json
// beside the transcript, so the malformed case is a real meta cut short rather
// than invented garbage.
func writeTruncatedSubagentMeta(t *testing.T, tree *vendorTree, slug, session, agent string) string {
	t.Helper()
	whole := corpusBytes(t, "sidechain/agent-"+corpusSubagentID+".meta.json")
	if len(whole) < 4 {
		t.Fatalf("the meta fixture is %d bytes, too short to truncate meaningfully", len(whole))
	}
	path := tree.subagentMetaPath(slug, session, agent)
	mustMkdirAll(t, filepath.Dir(path))
	if err := os.WriteFile(path, whole[:len(whole)/2], 0o644); err != nil {
		t.Fatalf("write truncated subagent meta %s: %v", path, err)
	}
	return path
}
