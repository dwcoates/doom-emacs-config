package main

import (
	"io"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/storeclient"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

func TestUnownedSpoolIsHeldNotTailed(t *testing.T) {
	// Arrange: a spool nobody has claimed.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert: tailing it would mean inventing an owner or reading the spool
	// path's runtime id as an identity.
	if _, watched := h.sc.watchers[spool]; watched {
		t.Fatal("an unclaimed spool was tailed")
	}
}

func TestUnownedSpoolIsRetainedAcrossRescans(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act: the spawning call arrives on a later pass.
	h.sc.TaskSpawned("b1", "call-1", "", "", false, "/workspace", "workspace-id", "session-1")
	h.sc.rescan()

	// Assert: it was held, never dropped, so it is tailed the moment it is
	// claimed.
	watched, ok := h.sc.watchers[spool]
	if !ok {
		t.Fatal("a held spool was not picked up once its owner arrived")
	}
	if watched.target.WorkspaceDir != "/workspace" || watched.target.WorkspaceID != "workspace-id" || watched.target.ClaudeSessionID != "session-1" {
		t.Fatalf("spool attribution = %+v, want the spawning transcript's workspace and session", watched.target)
	}
}

func TestAgedUnownedSpoolIsIngestedAsResidue(t *testing.T) {
	// Arrange: a spool whose owner never arrives.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()

	// Assert: an aged unowned spool is never dropped; its bytes go to residue.
	watched, ok := h.sc.watchers[spool]
	if !ok {
		t.Fatal("an aged unclaimed spool was dropped instead of ingested as residue")
	}
	if watched.target.Kind != tail.KindResidueSpool {
		t.Fatalf("kind = %s, want the residue spool kind", watched.target.Kind)
	}
}

func TestAgedUnownedSpoolKeepsBeingTailed(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()

	// Act: bytes appended after the demotion.
	h.sc.pollAll()
	before := h.sc.watchers[spool].tailer.Offset()
	h.write(t, spool, "hello\nmore\n")
	h.sc.pollAll()

	// Assert: nothing appended later is lost either.
	if got := h.sc.watchers[spool].tailer.Offset(); got <= before {
		t.Fatalf("offset = %d, want it past %d (a demoted spool keeps being tailed)", got, before)
	}
}

func TestAgedUnownedSpoolIsWarnedAboutOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.spoolFile(t, "b1", "hello\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.advance(UnownedSpoolWindow)

	// Act.
	h.sc.rescan()
	h.sc.rescan()

	// Assert.
	if got := strings.Count(h.logText(), "spool unclaimed after"); got != 1 {
		t.Fatalf("the demotion was stated %d times, want once", got)
	}
}

func TestAResidueSpoolNeedsNoOwner(t *testing.T) {
	// Arrange: a spool whose task-id prefix already failed classification.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "q1", "hello\n")

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert: no owner would change what happens to it, so it is not held.
	if _, watched := h.sc.watchers[spool]; !watched {
		t.Fatal("an unclassifiable spool was held instead of ingested")
	}
}

func TestAConfigRootTargetNeedsNoOwner(t *testing.T) {
	// Arrange: the transcript IS its session's record.
	h := newHarness(t, &fakeStore{})
	path := h.transcript(t, "sess-1", promptLine)

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if _, watched := h.sc.watchers[path]; !watched {
		t.Fatal("a session transcript was held for an owner it names itself")
	}
}

func TestAZeroHoldWindowKeepsTheDefault(t *testing.T) {
	// Arrange: zero is how the caller says "unset".
	var logs []string
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "held-test"})

	// Act.
	held := newHeldSpools(0, log)

	// Assert.
	if held.window != UnownedSpoolWindow {
		t.Fatalf("hold window = %s, want the default %s", held.window, UnownedSpoolWindow)
	}
}

func TestAConfiguredHoldWindowReplacesTheDefault(t *testing.T) {
	// Arrange.
	var logs []string
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "held-test"})

	// Act.
	held := newHeldSpools(15*time.Millisecond, log)

	// Assert.
	if held.window != 15*time.Millisecond {
		t.Fatalf("hold window = %s, want the configured 15ms", held.window)
	}
}

// backlogGap is how far the fake clock is advanced between writing a fixture and
// the first production cycle, so the fixture's mtime is comfortably before the
// process-start boundary and reads as pre-existing backlog.
const backlogGap = 5 * time.Minute

func TestStartupCatchUpSummarizesABacklogOfUnclaimedSpools(t *testing.T) {
	// Arrange: three spools already on disk before the sidecar starts, none of
	// which any transcript will ever claim.
	h := newHarness(t, &fakeStore{})
	for _, task := range []string{"b1", "b2", "b3"} {
		h.spoolFile(t, task, "orphaned\n")
	}
	h.advance(backlogGap)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act: the hold window lapses and the whole backlog demotes on one rescan.
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()

	// Assert: one summary at info naming the count, not three per-spool warnings.
	rec := h.requireOnce(t, "catchup-summary", "info")
	if got := ctxString(t, rec, "reason"); got != "spool_unclaimed" {
		t.Fatalf("summary reason = %q, want spool_unclaimed", got)
	}
	if got := ctxInt(t, rec, "repeat_count"); got != 3 {
		t.Fatalf("summary repeat_count = %d, want 3", got)
	}
	if got := len(h.opsAt(t, "hold-expired", "info")); got != 0 {
		t.Fatalf("catch-up stated %d per-spool hold-expiry records, want none", got)
	}
}

func TestABacklogSpoolIsDemotedAtDebugNotWarn(t *testing.T) {
	// Arrange: one pre-existing unclaimed spool.
	h := newHarness(t, &fakeStore{})
	h.spoolFile(t, "b1", "orphaned\n")
	h.advance(backlogGap)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()

	// Assert: nothing is silenced — the demotion is still stated, at debug.
	if got := len(h.opsAt(t, "hold-expired", "debug")); got != 1 {
		t.Fatalf("the backlog demotion was stated at debug %d times, want once", got)
	}
}

func TestASpoolThatAppearsAfterCatchUpIsStatedPerItem(t *testing.T) {
	// Arrange: the sidecar is already running when the spool appears.
	h := newHarness(t, &fakeStore{})
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.advance(time.Second)
	h.spoolFile(t, "b1", "appeared while running\n")
	h.sc.rescan()

	// Act: its window lapses.
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()

	// Assert: a newly-arising unclaimed spool is stated per file, at INFO —
	// the mandated ingest-as-residue-and-keep-tailing behavior working.
	h.requireOnce(t, "hold-expired", "info")
	if got := len(h.opsAt(t, "catchup-summary", "")); got != 0 {
		t.Fatalf("a steady-state spool produced %d catch-up summaries, want none", got)
	}
}

func TestAnEmptySpoolBacklogEmitsNoSummary(t *testing.T) {
	// Arrange: nothing unclaimed on disk.
	h := newHarness(t, &fakeStore{})
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()

	// Assert.
	if got := len(h.opsAt(t, "catchup-summary", "")); got != 0 {
		t.Fatalf("an empty spool backlog emitted %d summaries, want none", got)
	}
}

// unresolvableTranscript writes a session transcript whose only line carries no
// cwd, so workspace attribution cannot be resolved and the transcript is held.
func (h *harness) unresolvableTranscript(t *testing.T, session string) string {
	t.Helper()
	path := filepath.Join(h.rootA, "projects", "proj", session+".jsonl")
	h.write(t, path, promptLine+"\n")
	return normalized(path)
}

func TestStartupCatchUpSummarizesUnattributableTranscripts(t *testing.T) {
	// Arrange: two pre-existing transcripts whose workspace cannot be resolved.
	h := newHarness(t, &fakeStore{})
	h.unresolvableTranscript(t, "70000000-0000-4000-8000-000000000001")
	h.unresolvableTranscript(t, "70000000-0000-4000-8000-000000000002")
	h.advance(backlogGap)

	// Act: the first cycle's rescan catches up on the whole backlog at once.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert: one summary at info, not one warning per transcript.
	rec := h.requireOnce(t, "catchup-summary", "info")
	if got := ctxString(t, rec, "reason"); got != "workspace_unattributed" {
		t.Fatalf("summary reason = %q, want workspace_unattributed", got)
	}
	if got := ctxInt(t, rec, "repeat_count"); got != 2 {
		t.Fatalf("summary repeat_count = %d, want 2", got)
	}
	if got := len(h.opsAt(t, "resolve-transcript-workspace", "warn")); got != 0 {
		t.Fatalf("catch-up stated %d per-transcript warnings, want none", got)
	}
}

func TestAnUnattributableTranscriptAfterCatchUpWarnsPerItem(t *testing.T) {
	// Arrange: the sidecar is already running when the transcript appears.
	h := newHarness(t, &fakeStore{})
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.advance(time.Second)
	h.unresolvableTranscript(t, "80000000-0000-4000-8000-000000000001")

	// Act.
	h.sc.rescan()

	// Assert.
	h.requireOnce(t, "resolve-transcript-workspace", "warn")
	if got := len(h.opsAt(t, "catchup-summary", "")); got != 0 {
		t.Fatalf("a steady-state transcript produced %d catch-up summaries, want none", got)
	}
}

func TestStartupCatchUpSummarizesABacklogOfLegacyBookConflicts(t *testing.T) {
	// Arrange: production began AFTER these records were written, so a corrected
	// re-ingest of them is startup catch-up rather than steady state.
	h := newHarness(t, &fakeStore{})
	nowMs := h.clock.UnixMilli()
	h.sc.processStartMs = nowMs + int64(time.Hour/time.Millisecond)

	// Act: the store skipped two legacy book-conflicts, folded across the pass.
	h.sc.noteSkips("/nonexistent/session.jsonl", []storeclient.SkippedEntry{
		{UpsertKey: "activity:msg_1:0", FromBook: "toolu_A", ToBook: "toolu_B"},
		{UpsertKey: "activity:msg_2:0", FromBook: "toolu_A", ToBook: "toolu_B"},
	}, nowMs)
	h.sc.flushCatchupSummaries(nowMs)

	// Assert: one INFO summary naming the count, not two per-entry warnings.
	rec := h.requireOnce(t, "catchup-summary", "info")
	if got := ctxString(t, rec, "reason"); got != "legacy_book_conflict" {
		t.Fatalf("summary reason = %q, want legacy_book_conflict", got)
	}
	if got := ctxInt(t, rec, "repeat_count"); got != 2 {
		t.Fatalf("summary repeat_count = %d, want 2", got)
	}
	if got := len(h.opsAt(t, "book-conflict-skip", "warn")); got != 0 {
		t.Fatalf("catch-up stated %d per-entry skip warnings, want none", got)
	}
}
