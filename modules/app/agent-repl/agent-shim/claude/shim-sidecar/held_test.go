package main

import (
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agentrepl/shim-claude-sidecar/internal/discover"
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

func TestAnAgedUnownedSpoolIsNotRead(t *testing.T) {
	// Arrange: a spool whose owner never arrives.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()

	// Assert: nothing renders an unclaimed spool, so nothing reads it.
	if _, watched := h.sc.watchers[spool]; watched {
		t.Fatal("an aged unclaimed spool was tailed; nothing renders it")
	}
}

func TestAnAgedUnownedSpoolWritesNothingToTheStore(t *testing.T) {
	// Arrange: the spool keeps growing after its window lapsed, as a test log
	// does.
	store := &fakeStore{}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1", "hello\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()

	// Act.
	h.write(t, spool, "hello\nmore\n")
	h.sc.pollAll()

	// Assert: not a residue row, and not a cursor either.
	if store.writeCalls != 0 {
		t.Fatalf("an unclaimed spool cost %d store write(s), want none", store.writeCalls)
	}
}

func TestALapsedSpoolIsReadFromItsStartOnceClaimed(t *testing.T) {
	// Arrange: the window lapses first; the launch is read afterwards, as it
	// is when a restart catches up on a backlog.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()

	// Act.
	h.sc.TaskSpawned("b1", "call-1", "agent-1", "", false, "/workspace", "workspace-id", "session-1")
	h.sc.rescan()

	// Assert: it is claimed as the shell run it is, not left to residue.
	watched, ok := h.sc.watchers[spool]
	if !ok {
		t.Fatal("a lapsed spool was not read once a launch claimed it")
	}
	if watched.target.Kind != tail.KindShellSpool {
		t.Fatalf("kind = %s, want the shell spool it was claimed as", watched.target.Kind)
	}
}

func TestAnAgedUnownedSpoolIsStatedOnce(t *testing.T) {
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
		t.Fatalf("the lapse was stated %d times, want once", got)
	}
}

func TestAnAgedUnownedSpoolStatesItsPathAndReason(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.advance(time.Second)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.advance(time.Second)
	spool := h.spoolFile(t, "b1", "hello\n")
	h.sc.rescan()
	h.advance(UnownedSpoolWindow)

	// Act.
	h.sc.rescan()

	// Assert.
	rec := h.requireOnce(t, "hold-expired", "info")
	if got := ctxString(t, rec, "path"); got != spool {
		t.Fatalf("path = %q, want %q", got, spool)
	}
	if got := ctxString(t, rec, "reason"); got != reasonSpoolUnclaimed {
		t.Fatalf("reason = %q, want %q", got, reasonSpoolUnclaimed)
	}
}

func TestAClaimedSpoolStatesTheClaim(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	h.sc.TaskSpawned("b1", "call-1", "agent-1", "", false, "/workspace", "workspace-id", "session-1")

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert: the decision to read it names the file and the call.
	rec := h.requireOnce(t, "spool-claim", "info")
	if got := ctxString(t, rec, "path"); got != spool {
		t.Fatalf("path = %q, want %q", got, spool)
	}
	if got := ctxString(t, rec, "activity_id"); got != "call-1" {
		t.Fatalf("activity_id = %q, want call-1", got)
	}
}

func TestSpoolsNothingRendersAreNeverRead(t *testing.T) {
	tests := []struct {
		name       string
		arrange    func(t *testing.T, h *harness) string
		wantReason string
	}{
		{
			name: "an unrecognized task-id prefix",
			arrange: func(t *testing.T, h *harness) string {
				return h.spoolFile(t, "q1", "hello\n")
			},
			wantReason: reasonUnrecognizedPrefix,
		},
		{
			name: "an agent spool linking a transcript not written yet",
			arrange: func(t *testing.T, h *harness) string {
				return h.danglingAgentSpool(t, "a1")
			},
			wantReason: reasonTranscriptSymlink,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: claimed, so only the skip rule can keep it unread.
			h := newHarness(t, &fakeStore{})
			spool := tc.arrange(t, h)
			h.sc.TaskSpawned(strings.TrimSuffix(filepath.Base(spool), ".output"), "call-1", "agent-1", "", true, "/workspace", "workspace-id", "session-1")

			// Act.
			if err := h.sc.beginCycle(); err != nil {
				t.Fatalf("beginCycle: %v", err)
			}

			// Assert.
			if _, watched := h.sc.watchers[spool]; watched {
				t.Fatal("a spool nothing renders was tailed")
			}
			rec := h.requireOnce(t, "spool-skip", "info")
			if got := ctxString(t, rec, "reason"); got != tc.wantReason {
				t.Fatalf("reason = %q, want %q", got, tc.wantReason)
			}
			if got := ctxString(t, rec, "path"); got != spool {
				t.Fatalf("path = %q, want %q", got, spool)
			}
		})
	}
}

func TestASkippedSpoolIsStatedOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.spoolFile(t, "q1", "hello\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act: every rescan re-resolves every unwatched spool.
	h.sc.rescan()

	// Assert.
	h.requireOnce(t, "spool-skip", "info")
}

func TestAnAgentSpoolThatIsItsOwnFileIsStillRead(t *testing.T) {
	// Arrange: an a* spool that is NOT a link holds the only copy the reader
	// can prove, so the skip rule leaves it to its claim.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "a1", promptLine+"\n")
	h.sc.TaskSpawned("a1", "call-1", "agent-1", "", true, "/workspace", "workspace-id", "session-1")

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if _, watched := h.sc.watchers[spool]; !watched {
		t.Fatal("a claimed agent spool that is a regular file was not read")
	}
}

func TestAnAgentSpoolThatCannotBeExaminedIsNotRead(t *testing.T) {
	// Arrange: an agent spool target whose parent directory cannot be searched,
	// so whether it is a link cannot be established.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "a1", "x\n")
	dir := filepath.Dir(spool)
	if err := os.Chmod(dir, 0o000); err != nil {
		t.Fatalf("chmod: %v", err)
	}
	t.Cleanup(func() { _ = os.Chmod(dir, 0o755) })
	target := discover.Target{Path: spool, Kind: tail.KindAgentTranscript, TaskID: "a1"}

	// Act.
	_, _, skip, ok := h.sc.unrenderedSpool(target)

	// Assert: not read this pass, and said so at warn.
	if ok || skip {
		t.Fatalf("ok=%t skip=%t, want the examination refused", ok, skip)
	}
	h.requireOnce(t, "spool-skip", "warn")
}

func TestAnAgentSpoolGoneBeforeItsExaminationIsNotRead(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	target := discover.Target{Path: filepath.Join(h.spool, "claude-501", "proj", "runtime-sess", "tasks", "a9.output"), Kind: tail.KindAgentTranscript, TaskID: "a9"}

	// Act.
	reason, _, skip, ok := h.sc.unrenderedSpool(target)

	// Assert.
	if !ok || !skip || reason != reasonSpoolVanished {
		t.Fatalf("reason=%q skip=%t ok=%t, want a vanished spool skipped", reason, skip, ok)
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

	// Act: the hold window lapses for the whole backlog on one rescan.
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

func TestABacklogSpoolsLapseIsStatedAtDebugNotInfo(t *testing.T) {
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

	// Assert: nothing is silenced — the lapse is still stated, at debug.
	if got := len(h.opsAt(t, "hold-expired", "debug")); got != 1 {
		t.Fatalf("the backlog lapse was stated at debug %d times, want once", got)
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
	// the not-rendered-so-not-read rule working.
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
	h.activate(t, session)
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

// --- a transcript that was gone before its first byte -----------------------

// vanishedTranscript names a session transcript that is NOT on disk: the vendor
// session directory was removed between the scan that listed it and the
// attribution read, which is what deleting a session does to every file under it
// at once.
func (h *harness) vanishedTranscript(session string) discover.Target {
	return discover.Target{
		Path:       filepath.Join(h.rootA, "projects", "proj", session+".jsonl"),
		Kind:       tail.KindSessionTranscript,
		ConfigRoot: h.rootA,
		ProjectKey: "proj",
		SessionID:  session,
	}
}

func TestATranscriptGoneBeforeItsFirstByteIsStatedNotWarned(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	target := h.vanishedTranscript("90000000-0000-4000-8000-000000000001")

	// Act.
	if _, ok := h.sc.resolveTranscriptWorkspace(target); ok {
		t.Fatal("a transcript that is not on disk was attributed")
	}

	// Assert.
	h.requireNone(t, "resolve-transcript-workspace", "warn")
	rec := h.requireOnce(t, "resolve-transcript-workspace", "info")
	if got := ctxString(t, rec, "reason"); got != reasonTranscriptVanished {
		t.Fatalf("the record's reason = %q, want %q", got, reasonTranscriptVanished)
	}
}

func TestATranscriptGoneBeforeItsFirstByteIsStatedOncePerFile(t *testing.T) {
	// Arrange: a rescan re-checks every discovered transcript each pass.
	h := newHarness(t, &fakeStore{})
	target := h.vanishedTranscript("90000000-0000-4000-8000-000000000002")

	// Act.
	h.sc.resolveTranscriptWorkspace(target)
	h.sc.resolveTranscriptWorkspace(target)

	// Assert.
	h.requireOnce(t, "resolve-transcript-workspace", "info")
}

func TestAPresentTranscriptThatCannotBeAttributedStillWarns(t *testing.T) {
	// Arrange: the file is there and carries no cwd, which is the condition an
	// operator must look at.
	h := newHarness(t, &fakeStore{})
	h.unresolvableTranscript(t, "90000000-0000-4000-8000-000000000003")
	target := discover.Target{
		Path:       filepath.Join(h.rootA, "projects", "proj", "90000000-0000-4000-8000-000000000003.jsonl"),
		Kind:       tail.KindSessionTranscript,
		ConfigRoot: h.rootA,
		ProjectKey: "proj",
		SessionID:  "90000000-0000-4000-8000-000000000003",
	}

	// Act.
	h.sc.resolveTranscriptWorkspace(target)

	// Assert.
	h.requireOnce(t, "resolve-transcript-workspace", "warn")
}
