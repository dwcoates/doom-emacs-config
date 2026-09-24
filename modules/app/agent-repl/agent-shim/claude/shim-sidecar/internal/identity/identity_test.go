package identity

import (
	"io"
	"os"
	"path/filepath"
	"testing"

	"agentrepl/shim-claude-sidecar/internal/logging"
)

func TestMain(m *testing.M) {
	if err := os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1"); err != nil {
		panic(err)
	}
	os.Exit(m.Run())
}

type sliceWriter struct{ lines *[]string }

func (w sliceWriter) Write(p []byte) (int, error) {
	*w.lines = append(*w.lines, string(p))
	return len(p), nil
}

func index(t *testing.T, stateDir string) (*Index, *[]string) {
	t.Helper()
	var logs []string
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "identity-test"})
	return New(stateDir, log), &logs
}

// The three fixture ids. They are spelled as real uuids because that is what
// the vendor mints and what the file names carry.
const (
	original = "11111111-1111-4111-8111-111111111111"
	rotated  = "22222222-2222-4222-8222-222222222222"
	stranger = "33333333-3333-4333-8333-333333333333"
	wsKey    = "0a1b2c3d"
)

// writeAgentID writes the shim's agent-id.json, in engine/identity.ts's own
// field names — the on-disk contract this package is a reader of.
func writeAgentID(t *testing.T, stateDir, workspaceKey, originalID string) {
	t.Helper()
	dir := filepath.Join(stateDir, "shim", workspaceKey)
	mustMkdir(t, dir)
	mustWrite(t, filepath.Join(dir, "agent-id.json"), `{
  "original_vendor_session_id": "`+originalID+`",
  "workspace_key": "`+workspaceKey+`",
  "minted_at_ms": 1735689600000
}
`)
}

// writeVendorLink writes the pointer file a rotation leaves behind.
func writeVendorLink(t *testing.T, stateDir, workspaceKey, vendorID, originalID string) {
	t.Helper()
	dir := filepath.Join(stateDir, "shim", workspaceKey, "vendor-id")
	mustMkdir(t, dir)
	mustWrite(t, filepath.Join(dir, vendorID+".json"), `{
  "vendor_session_id": "`+vendorID+`",
  "original_vendor_session_id": "`+originalID+`",
  "linked_at_ms": 1735689700000
}
`)
}

func mustMkdir(t *testing.T, dir string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("creating %s: %v", dir, err)
	}
}

func mustWrite(t *testing.T, path, content string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("writing %s: %v", path, err)
	}
}

// TestResolveAnswersEveryRecordShape is the whole resolution table: which book
// an id names, and on whose authority.
func TestResolveAnswersEveryRecordShape(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name       string
		arrange    func(t *testing.T, stateDir string)
		ask        string
		wantBook   string
		wantSource Source
		wantKey    string
	}{
		{
			name: "a rotated id is booked under the original its link file names",
			arrange: func(t *testing.T, stateDir string) {
				writeAgentID(t, stateDir, wsKey, original)
				writeVendorLink(t, stateDir, wsKey, rotated, original)
			},
			ask: rotated, wantBook: original, wantSource: SourceVendorLink, wantKey: wsKey,
		},
		{
			name: "an id that IS an original is its own book, on the shim's word",
			arrange: func(t *testing.T, stateDir string) {
				writeAgentID(t, stateDir, wsKey, original)
			},
			ask: original, wantBook: original, wantSource: SourceAgentIDRecord, wantKey: wsKey,
		},
		{
			name: "an id no record names is its own book, by the resume rule",
			arrange: func(t *testing.T, stateDir string) {
				writeAgentID(t, stateDir, wsKey, original)
			},
			ask: stranger, wantBook: stranger, wantSource: SourceUnrecorded,
		},
		{
			name: "a link written by ANOTHER workspace's shim still resolves",
			arrange: func(t *testing.T, stateDir string) {
				writeAgentID(t, stateDir, wsKey, original)
				writeAgentID(t, stateDir, "deadbeef", stranger)
				writeVendorLink(t, stateDir, "deadbeef", rotated, stranger)
			},
			ask: rotated, wantBook: stranger, wantSource: SourceVendorLink, wantKey: "deadbeef",
		},
		{
			name:    "an empty id resolves to nothing rather than to some workspace's book",
			arrange: func(t *testing.T, stateDir string) { writeAgentID(t, stateDir, wsKey, original) },
			ask:     "", wantBook: "", wantSource: SourceUnrecorded,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange.
			stateDir := t.TempDir()
			tc.arrange(t, stateDir)
			idx, _ := index(t, stateDir)
			idx.Refresh()

			// Act.
			got := idx.Resolve(tc.ask)

			// Assert.
			if got.Original != tc.wantBook {
				t.Errorf("Resolve(%q) booked %q, want %q", tc.ask, got.Original, tc.wantBook)
			}
			if got.Source != tc.wantSource {
				t.Errorf("Resolve(%q) came from %q, want %q", tc.ask, got.Source, tc.wantSource)
			}
			if got.WorkspaceKey != tc.wantKey {
				t.Errorf("Resolve(%q) named workspace %q, want %q", tc.ask, got.WorkspaceKey, tc.wantKey)
			}
		})
	}
}

// TestAnIndexWithNoStateRootResolvesEveryIdToItself pins the disabled shape: no
// state root is not a reason to guess, it is a reason to keep the pre-existing
// behavior.
func TestAnIndexWithNoStateRootResolvesEveryIdToItself(t *testing.T) {
	t.Parallel()
	// Arrange.
	idx, _ := index(t, "")

	// Act.
	got := idx.Resolve(rotated)

	// Assert.
	if got.Original != rotated || got.Source != SourceUnrecorded {
		t.Errorf("a stateless index resolved %q to %+v, want the id itself as unrecorded", rotated, got)
	}
}

// TestALinkThatAppearsAfterARefreshIsFoundWithoutOne is the mid-tail rotation's
// window: the link file lands between two rescans, and the id is asked about in
// between. A purely in-memory index would answer with the stale miss it cached.
func TestALinkThatAppearsAfterARefreshIsFoundWithoutOne(t *testing.T) {
	t.Parallel()
	// Arrange: a refresh that saw no link at all.
	stateDir := t.TempDir()
	writeAgentID(t, stateDir, wsKey, original)
	idx, _ := index(t, stateDir)
	idx.Refresh()

	// The id is deliberately NOT asked about first. A miss is remembered until
	// the next refresh (see TestAMissIsGlobbedOnceUntilARefresh), so a
	// precondition lookup here would be asserting the cache rather than the
	// fallback this subject is about.

	// Act: the shim rotates, and the id is asked about before the next refresh.
	writeVendorLink(t, stateDir, wsKey, rotated, original)
	got := idx.Resolve(rotated)

	// Assert.
	if got.Original != original || got.Source != SourceVendorLink {
		t.Errorf("the link that appeared since the last refresh resolved to %+v, want %q from its link file", got, original)
	}
}

// TestARemovedLinkStopsAnsweringAfterARefresh is the other half of the cache:
// a record that is gone must stop deciding where rows land.
func TestARemovedLinkStopsAnsweringAfterARefresh(t *testing.T) {
	t.Parallel()
	// Arrange.
	stateDir := t.TempDir()
	writeAgentID(t, stateDir, wsKey, original)
	writeVendorLink(t, stateDir, wsKey, rotated, original)
	idx, _ := index(t, stateDir)
	idx.Refresh()
	if got := idx.Resolve(rotated); got.Original != original {
		t.Fatalf("precondition: the linked id resolved to %q, want %q", got.Original, original)
	}

	// Act.
	if err := os.Remove(filepath.Join(stateDir, "shim", wsKey, "vendor-id", rotated+".json")); err != nil {
		t.Fatalf("removing the link: %v", err)
	}
	idx.Refresh()

	// Assert.
	if got := idx.Resolve(rotated); got.Original != rotated || got.Source != SourceUnrecorded {
		t.Errorf("the removed link still answered %+v, want the id itself as unrecorded", got)
	}
}

// TestAMalformedRecordIsRefusedAndStated covers every unusable record shape.
// NONE of them may resolve, and every one of them must be stated: a record that
// exists and cannot be read is exactly the shape that silently splits a book.
func TestAMalformedRecordIsRefusedAndStated(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name    string
		arrange func(t *testing.T, stateDir string)
	}{
		{
			name: "a link whose json does not parse",
			arrange: func(t *testing.T, stateDir string) {
				dir := filepath.Join(stateDir, "shim", wsKey, "vendor-id")
				mustMkdir(t, dir)
				mustWrite(t, filepath.Join(dir, rotated+".json"), "{not json")
			},
		},
		{
			name: "a link that names no original",
			arrange: func(t *testing.T, stateDir string) {
				dir := filepath.Join(stateDir, "shim", wsKey, "vendor-id")
				mustMkdir(t, dir)
				mustWrite(t, filepath.Join(dir, rotated+".json"),
					`{"vendor_session_id":"`+rotated+`","linked_at_ms":1}`)
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange.
			stateDir := t.TempDir()
			tc.arrange(t, stateDir)
			idx, logs := index(t, stateDir)

			// Act.
			idx.Refresh()
			got := idx.Resolve(rotated)

			// Assert: the id keeps its own book, and the defect was stated once.
			if got.Original != rotated {
				t.Errorf("an unusable record resolved %q to %q; an unreadable record must never decide a book", rotated, got.Original)
			}
			requireOnceIn(t, parseLogLines(t, *logs), "identity-record", "warn")
		})
	}
}

// TestALinkMissingItsVendorFieldFallsBackToItsFileName keeps the resolution the
// file name already carries, and still states the writer's defect.
func TestALinkMissingItsVendorFieldFallsBackToItsFileName(t *testing.T) {
	t.Parallel()
	// Arrange.
	stateDir := t.TempDir()
	dir := filepath.Join(stateDir, "shim", wsKey, "vendor-id")
	mustMkdir(t, dir)
	mustWrite(t, filepath.Join(dir, rotated+".json"),
		`{"original_vendor_session_id":"`+original+`","linked_at_ms":1}`)
	idx, logs := index(t, stateDir)

	// Act.
	idx.Refresh()
	got := idx.Resolve(rotated)

	// Assert.
	if got.Original != original {
		t.Errorf("the link resolved %q to %q, want %q from its file name", rotated, got.Original, original)
	}
	requireOnceIn(t, parseLogLines(t, *logs), "identity-record", "warn")
}

// TestAnAgentIdRecordWithNoOriginalIsRefused: the shim treats such a file as a
// defect rather than an absence, and so does its reader.
func TestAnAgentIdRecordWithNoOriginalIsRefused(t *testing.T) {
	t.Parallel()
	// Arrange.
	stateDir := t.TempDir()
	dir := filepath.Join(stateDir, "shim", wsKey)
	mustMkdir(t, dir)
	mustWrite(t, filepath.Join(dir, "agent-id.json"), `{"workspace_key":"`+wsKey+`","minted_at_ms":1}`)
	idx, logs := index(t, stateDir)

	// Act.
	idx.Refresh()

	// Assert.
	if got := idx.Resolve(original); got.Source != SourceUnrecorded {
		t.Errorf("an agent-id.json naming no original still answered %+v", got)
	}
	requireOnceIn(t, parseLogLines(t, *logs), "identity-record", "warn")
}

// TestRefreshOverAMissingStateRootIsSilent: the sidecar may run beside a daemon
// that has not started a shim yet, and that is not a fault to report.
func TestRefreshOverAMissingStateRootIsSilent(t *testing.T) {
	t.Parallel()
	// Arrange.
	idx, logs := index(t, filepath.Join(t.TempDir(), "never-created"))

	// Act.
	idx.Refresh()

	// Assert.
	for _, rec := range parseLogLines(t, *logs) {
		if rec.Level == "warn" || rec.Level == "error" {
			t.Errorf("an absent state root produced %q at %q: %s", rec.Operation, rec.Level, rec.Message)
		}
	}
}

// TestRotatedNamesOnlyAMovedBook pins the predicate the reader's remap turns on.
func TestRotatedNamesOnlyAMovedBook(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name       string
		resolution Resolution
		asked      string
		want       bool
	}{
		{name: "an id that resolves to another book", resolution: Resolution{Original: original}, asked: rotated, want: true},
		{name: "an id that resolves to itself", resolution: Resolution{Original: original}, asked: original, want: false},
		{name: "an empty resolution", resolution: Resolution{}, asked: rotated, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			if got := tc.resolution.Rotated(tc.asked); got != tc.want {
				t.Errorf("Rotated(%q) = %t, want %t", tc.asked, got, tc.want)
			}
		})
	}
}

// TestAMissIsGlobbedOnceUntilARefresh pins the negative cache, which is what
// makes Resolve an in-memory lookup on the poll path.
//
// `rekeyRotations` resolves EVERY WATCHER ON EVERY POLL TICK. Re-globbing each
// miss made a steady-state sidecar (pid 96084, a 10s `sample`) spend 76-101% of
// a core inside filepath.Glob for ~2900 ids a second, ~1100 of which belonged to
// cold runs that would never resolve, and stretched poll ticks to seconds.
func TestAMissIsGlobbedOnceUntilARefresh(t *testing.T) {
	t.Parallel()
	// Arrange: a refresh that saw no link, and one lookup that globbed for it.
	stateDir := t.TempDir()
	writeAgentID(t, stateDir, wsKey, original)
	idx, _ := index(t, stateDir)
	idx.Refresh()
	if got := idx.Resolve(rotated); got.Source != SourceUnrecorded {
		t.Fatalf("precondition: the unlinked id resolved as %+v, want unrecorded", got)
	}

	// Act: a link appears and the id is asked about again, with no refresh in
	// between. The answer is the remembered miss, which is the PROOF no second
	// glob ran — a glob would have found the file that is now on disk.
	writeVendorLink(t, stateDir, wsKey, rotated, original)
	got := idx.Resolve(rotated)

	// Assert.
	if got.Original != rotated || got.Source != SourceUnrecorded {
		t.Errorf("the second lookup of a missing id resolved to %+v, want the remembered miss; it globbed the disk again", got)
	}
}

// TestARefreshClearsTheRememberedMiss is the bound on the cache: the one event
// that can make yesterday's miss wrong is a refresh, and a refresh clears it.
// Refresh runs at every rescan AND on every poll tick where the change probe
// found something, which is exactly when a new link can have been written.
func TestARefreshClearsTheRememberedMiss(t *testing.T) {
	t.Parallel()
	// Arrange: a remembered miss.
	stateDir := t.TempDir()
	writeAgentID(t, stateDir, wsKey, original)
	idx, _ := index(t, stateDir)
	idx.Refresh()
	if got := idx.Resolve(rotated); got.Source != SourceUnrecorded {
		t.Fatalf("precondition: the unlinked id resolved as %+v, want unrecorded", got)
	}
	writeVendorLink(t, stateDir, wsKey, rotated, original)

	// Act.
	idx.Refresh()

	// Assert.
	if got := idx.Resolve(rotated); got.Original != original || got.Source != SourceVendorLink {
		t.Errorf("after a refresh the link written since the miss resolved to %+v, want %q from its link file", got, original)
	}
}

// TestRecheckLinksDropsTheMissWhenALinkAppears is what keeps the negative cache
// from costing a guarantee: "a link that appears mid-tail moves the book on the
// very next poll" turns on the link being WRITTEN, not on the rescan cadence,
// and the poll path asks the link directories that question once per tick.
func TestRecheckLinksDropsTheMissWhenALinkAppears(t *testing.T) {
	t.Parallel()
	// Arrange: a remembered miss, and a link written after it.
	stateDir := t.TempDir()
	writeAgentID(t, stateDir, wsKey, original)
	idx, _ := index(t, stateDir)
	idx.Refresh()
	if got := idx.Resolve(rotated); got.Source != SourceUnrecorded {
		t.Fatalf("precondition: the unlinked id resolved as %+v, want unrecorded", got)
	}
	writeVendorLink(t, stateDir, wsKey, rotated, original)

	// Act: one poll tick's recheck, with no full refresh.
	idx.RecheckLinks()

	// Assert.
	if got := idx.Resolve(rotated); got.Original != original || got.Source != SourceVendorLink {
		t.Errorf("after the recheck the link resolved to %+v, want %q from its link file", got, original)
	}
}

// TestRecheckLinksKeepsTheMissWhenNothingChanged is the other half: the recheck
// exists to make the miss CHEAP, so a tree nobody wrote to must leave it alone.
func TestRecheckLinksKeepsTheMissWhenNothingChanged(t *testing.T) {
	t.Parallel()
	// Arrange: a remembered miss over a tree that holds a link directory.
	stateDir := t.TempDir()
	writeAgentID(t, stateDir, wsKey, original)
	writeVendorLink(t, stateDir, wsKey, stranger, original)
	idx, _ := index(t, stateDir)
	idx.Refresh()
	if got := idx.Resolve(rotated); got.Source != SourceUnrecorded {
		t.Fatalf("precondition: the unlinked id resolved as %+v, want unrecorded", got)
	}

	// Act: a tick's recheck over an untouched tree, then the link appears
	// WITHOUT another recheck.
	idx.RecheckLinks()
	writeVendorLink(t, stateDir, wsKey, rotated, original)

	// Assert: the recheck that ran saw nothing and kept the miss, which is the
	// proof it did not re-glob.
	if got := idx.Resolve(rotated); got.Source != SourceUnrecorded {
		t.Errorf("a recheck over an unchanged tree dropped the remembered miss: %+v", got)
	}
}

// TestAPollRefreshKeepsTheMissWhenNoLinkMoved pins the poll path's cost: the
// change probe refreshes on nearly every tick of an active session, and a
// refresh that dropped every miss sent each watcher back to a glob over every
// shim directory (74% of the sidecar's CPU, 2026-09-24).
func TestAPollRefreshKeepsTheMissWhenNoLinkMoved(t *testing.T) {
	t.Parallel()
	// Arrange: a remembered miss over a tree that holds a link directory.
	stateDir := t.TempDir()
	writeAgentID(t, stateDir, wsKey, original)
	writeVendorLink(t, stateDir, wsKey, stranger, original)
	idx, _ := index(t, stateDir)
	idx.Refresh()
	if got := idx.Resolve(rotated); got.Source != SourceUnrecorded {
		t.Fatalf("precondition: the unlinked id resolved as %+v, want unrecorded", got)
	}
	globs := idx.globs

	// Act: a poll-path refresh over an untouched link tree, then the lookup.
	idx.RefreshKeepingMisses()
	before := idx.globs
	got := idx.Resolve(rotated)

	// Assert: the refresh read the records but the lookup ran no glob.
	if got.Source != SourceUnrecorded {
		t.Fatalf("the miss resolved as %+v, want the remembered miss", got)
	}
	if idx.globs != before {
		t.Errorf("the lookup after a poll refresh ran %d glob(s), want none (refresh itself ran %d)", idx.globs-before, before-globs)
	}
}

// TestAPollRefreshDropsTheMissWhenALinkAppeared keeps the guarantee: a link
// written since the miss moves its directory, and the poll refresh then finds
// it.
func TestAPollRefreshDropsTheMissWhenALinkAppeared(t *testing.T) {
	t.Parallel()
	// Arrange: a remembered miss, and a link written after it.
	stateDir := t.TempDir()
	writeAgentID(t, stateDir, wsKey, original)
	idx, _ := index(t, stateDir)
	idx.Refresh()
	if got := idx.Resolve(rotated); got.Source != SourceUnrecorded {
		t.Fatalf("precondition: the unlinked id resolved as %+v, want unrecorded", got)
	}
	writeVendorLink(t, stateDir, wsKey, rotated, original)

	// Act.
	idx.RefreshKeepingMisses()

	// Assert.
	if got := idx.Resolve(rotated); got.Original != original || got.Source != SourceVendorLink {
		t.Errorf("after a poll refresh the new link resolved to %+v, want %q from its link file", got, original)
	}
}
