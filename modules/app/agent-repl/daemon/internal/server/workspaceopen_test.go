package server

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/workspace/merge"
)

// fakeEnsurer records the workspaces it was asked to bring up.
//
// It is mutex-guarded because the open path DETACHES its bring-up: the
// recording now happens on a goroutine the test does not own, so the plain
// slices this started as were a data race the moment the ack came off the
// bring-up (openbringup.go).
type fakeEnsurer struct {
	mu        sync.Mutex
	calls     []string
	driveable []string
	merge     []string
	err       error
	// gate, when non-nil, holds EnsureDriveable until the test closes it. It
	// is how a test observes an ack that was written while the bring-up was
	// still in progress, with no timing assumption whatsoever.
	gate chan struct{}
	// entered, when non-nil, receives the workspace as EnsureDriveable begins,
	// before the gate is consulted.
	entered chan string
}

func (f *fakeEnsurer) Ensure(workspace string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, workspace)
	return f.err
}

// driveable records the workspaces the DRIVEABLE bring-up was asked for, kept
// apart from calls so a test can tell which of the two an opener used.
func (f *fakeEnsurer) EnsureDriveable(ctx context.Context, workspace string) error {
	f.mu.Lock()
	f.driveable = append(f.driveable, workspace)
	gate, entered, err := f.gate, f.entered, f.err
	f.mu.Unlock()
	if entered != nil {
		entered <- workspace
	}
	if gate != nil {
		select {
		case <-gate:
		case <-ctx.Done():
			return ctx.Err()
		}
	}
	return err
}

func (f *fakeEnsurer) ReviveForMerge(_ context.Context, workspace string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.merge = append(f.merge, workspace)
	return f.err
}

// driveableCalls is the recorded DRIVEABLE bring-ups, copied under the lock.
func (f *fakeEnsurer) driveableCalls() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return slices.Clone(f.driveable)
}

// nonWaitingCalls is the recorded non-waiting bring-ups, copied under the lock.
func (f *fakeEnsurer) nonWaitingCalls() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return slices.Clone(f.calls)
}

// openFailure is one bring-up failure that landed after its open was acked.
type openFailure struct {
	workspace string
	err       error
}

// fakeOpenFailures is the pushed failure surface an early-acked open reports
// into. Its channel is the test's synchronization point, never a poll.
type fakeOpenFailures struct {
	recorded chan openFailure
}

func newFakeOpenFailures() *fakeOpenFailures {
	return &fakeOpenFailures{recorded: make(chan openFailure, 8)}
}

func (f *fakeOpenFailures) RecordOpenFailure(workspace string, err error) {
	f.recorded <- openFailure{workspace: workspace, err: err}
}

// openSettlements installs the detached-bring-up settle seam and returns the
// channel each settlement is announced on. Restored on cleanup so one test's
// seam can never answer another's.
func openSettlements(t *testing.T) chan string {
	t.Helper()
	settled := make(chan string, 8)
	onOpenSettled = func(workspace string) { settled <- workspace }
	t.Cleanup(func() { onOpenSettled = nil })
	return settled
}

// writeTranscript creates <configDir>/projects/<slug(cwd)>/<uuid>.jsonl.
func writeProjectTranscript(t *testing.T, configDir, cwd, uuid string) {
	t.Helper()
	dir := session.ProjectDir(configDir, cwd)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", dir, err)
	}
	path := filepath.Join(dir, uuid+".jsonl")
	if err := os.WriteFile(path, []byte("{}\n"), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

// openerRig builds a WorkspaceOpener over a temp registry, capturing its log.
func openerRig(t *testing.T, dirs ...string) (*WorkspaceOpener, *registry.Registry, *fakeEnsurer, *[]string) {
	t.Helper()
	reg := openTestRegistry(t)
	ens := &fakeEnsurer{}
	var lines []string
	o := &WorkspaceOpener{
		Reg:        reg,
		Ensurer:    ens,
		ConfigDirs: func() []string { return dirs },
		Failures:   newFakeOpenFailures(),
		Logf:       func(f string, a ...any) { lines = append(lines, fmt.Sprintf(f, a...)) },
	}
	return o, reg, ens, &lines
}

// openFailuresOf is the rig's failure sink, typed.
func openFailuresOf(t *testing.T, o *WorkspaceOpener) *fakeOpenFailures {
	t.Helper()
	sink, ok := o.Failures.(*fakeOpenFailures)
	if !ok {
		t.Fatalf("opener failure sink is %T, want *fakeOpenFailures", o.Failures)
	}
	return sink
}

func TestBindWorkspaceBindsTheNewestOnDiskTranscript(t *testing.T) {
	// Arrange — a record with no vendor session id and a transcript on disk.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	writeProjectTranscript(t, cfg, "/w", "uuid-disk")

	// Act
	bound := o.BindWorkspace("/w")

	// Assert
	rec, _ := reg.Get("s_1")
	if !bound || rec.ClaudeSessionID != "uuid-disk" {
		t.Fatalf("bound=%v ClaudeSessionID=%q; want true,uuid-disk", bound, rec.ClaudeSessionID)
	}
}

func TestBindWorkspaceLeavesAnAlreadyBoundRecordAlone(t *testing.T) {
	// Arrange — the record already names its conversation.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, ClaudeSessionID: "uuid-live", CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	writeProjectTranscript(t, cfg, "/w", "uuid-disk")

	// Act
	o.BindWorkspace("/w")

	// Assert
	rec, _ := reg.Get("s_1")
	if rec.ClaudeSessionID != "uuid-live" {
		t.Fatalf("ClaudeSessionID = %q; want the live id uuid-live to survive", rec.ClaudeSessionID)
	}
}

func TestBindWorkspaceNeverAdoptsATranscriptFromAnotherConfigDir(t *testing.T) {
	// Arrange — the only transcript lives under a DIFFERENT account root.
	own, other := t.TempDir(), t.TempDir()
	o, reg, _, _ := openerRig(t, own, other)
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: own, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	writeProjectTranscript(t, other, "/w", "uuid-elsewhere")

	// Act
	o.BindWorkspace("/w")

	// Assert
	rec, _ := reg.Get("s_1")
	if rec.ClaudeSessionID != "" {
		t.Fatalf("ClaudeSessionID = %q; a foreign-config-dir transcript must never be adopted", rec.ClaudeSessionID)
	}
}

func TestBindWorkspaceLoudLogsAForeignTranscriptAsAMigrationCandidate(t *testing.T) {
	// Arrange
	own, other := t.TempDir(), t.TempDir()
	o, reg, _, lines := openerRig(t, own, other)
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: own, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	writeProjectTranscript(t, other, "/w", "uuid-elsewhere")

	// Act
	o.BindWorkspace("/w")

	// Assert
	if !containsSubstring(*lines, "MIGRATION CANDIDATE") {
		t.Fatalf("no migration-candidate log line; got %v", *lines)
	}
}

func TestBindWorkspaceLoudLogsAWorkspaceWithNoRegistryRecord(t *testing.T) {
	// Arrange — a workspace the registry has never heard of.
	cfg := t.TempDir()
	o, _, _, lines := openerRig(t, cfg)

	// Act
	bound := o.BindWorkspace("/unknown")

	// Assert
	if bound {
		t.Fatalf("BindWorkspace bound something for a workspace with no record")
	}
	if !containsSubstring(*lines, "no registry record") {
		t.Fatalf("no loud line for the recordless workspace; got %v", *lines)
	}
}

// openedRecord seeds a workspace whose session already exists and is bound, so
// an open of it is the pure reattach the congestion collapse was made of.
func openedRecord(t *testing.T, reg *registry.Registry, cfg string) {
	t.Helper()
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, ClaudeSessionID: "uuid-live", CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
}

func TestOpenAcksWithoutWaitingForTheBringUpToFinish(t *testing.T) {
	// Arrange — a bring-up that is held open for as long as the test wants it.
	// This is the whole defect: acking on completion made an open cost seconds,
	// and the editor's re-send of every unacked open outran that service rate.
	cfg := t.TempDir()
	o, reg, ens, _ := openerRig(t, cfg)
	settled := openSettlements(t)
	ens.gate = make(chan struct{})
	ens.entered = make(chan string, 1)
	openedRecord(t, reg, cfg)

	// Act — the open returns while the bring-up is still held.
	err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{})

	// Assert — the bring-up really did start and really is still running, so
	// the ack above was written against an unfinished one.
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	if got := <-ens.entered; got != "/w" {
		t.Fatalf("bring-up entered for %q, want /w", got)
	}
	if !o.openInFlight("/w") {
		t.Fatal("the open acked and left no bring-up in flight; the ack must mean accepted, with the bring-up still running")
	}
	close(ens.gate)
	<-settled
}

func TestOpenStartsTheDriveableBringUpAndNotTheNonWaitingOne(t *testing.T) {
	// Arrange — moving the WAIT off the command must not downgrade WHICH
	// bring-up an open asks for: the driveable one is what resolves the
	// workspace, and the non-waiting one would leave the handshake unwatched.
	cfg := t.TempDir()
	o, reg, ens, _ := openerRig(t, cfg)
	settled := openSettlements(t)
	openedRecord(t, reg, cfg)

	// Act
	if err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{}); err != nil {
		t.Fatalf("Open: %v", err)
	}
	<-settled

	// Assert
	if !slices.Equal(ens.driveableCalls(), []string{"/w"}) || len(ens.nonWaitingCalls()) != 0 {
		t.Fatalf("driveable=%v non-waiting=%v; want [/w] and none", ens.driveableCalls(), ens.nonWaitingCalls())
	}
}

func TestADuplicateOpenCoalescesOntoTheBringUpAlreadyInFlight(t *testing.T) {
	// Arrange — the retry storm is a generator of duplicate opens, and two
	// concurrent bring-ups for one workspace would supersede each other.
	cfg := t.TempDir()
	o, reg, ens, _ := openerRig(t, cfg)
	settled := openSettlements(t)
	ens.gate = make(chan struct{})
	ens.entered = make(chan string, 1)
	openedRecord(t, reg, cfg)
	if err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{}); err != nil {
		t.Fatalf("first Open: %v", err)
	}
	<-ens.entered

	// Act — a second open arrives while the first bring-up is still held.
	err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{})

	// Assert — accepted, and against the SAME bring-up.
	if err != nil {
		t.Fatalf("duplicate Open: %v", err)
	}
	close(ens.gate)
	<-settled
	if got := ens.driveableCalls(); !slices.Equal(got, []string{"/w"}) {
		t.Fatalf("bring-ups = %v; want exactly one — the duplicate open must coalesce, not enqueue a second", got)
	}
}

func TestOpenRefusesWhenNoBringUpFailureSinkIsWired(t *testing.T) {
	// Arrange — an early ack with no late-failure surface would swallow every
	// bring-up failure the ack used to carry.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	o.Failures = nil
	openedRecord(t, reg, cfg)

	// Act
	err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no bring-up failure sink wired") {
		t.Fatalf("Open err = %v, want a loud missing-sink refusal", err)
	}
}

func TestRepeatedOpensAreIdempotent(t *testing.T) {
	// Arrange — Emacs sends `openWorkspace' on every workspace SWITCH (the
	// never-blue switch half), so repeat opens are the STEADY STATE, not an
	// edge case. This pins the property that decision rests on: open means
	// "ensure this workspace", so sending it again is safe.
	cfg := t.TempDir()
	o, reg, ens, _ := openerRig(t, cfg)
	settled := openSettlements(t)
	writeProjectTranscript(t, cfg, "/w", "uuid-disk")
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act — three switches to the same workspace, each settled before the next
	// so this measures repetition rather than the coalescing of concurrent
	// opens (which has its own test).
	for i := 0; i < 3; i++ {
		if err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{}); err != nil {
			t.Fatalf("Open #%d: %v", i+1, err)
		}
		<-settled
	}

	// Assert — the bind happened exactly once (the first open discovered the
	// transcript; the rest found the record already bound and left it alone),
	// and each open started the authoritative bring-up, which collapses to the
	// established controller once the session is live.
	rec, _ := reg.Get("s_1")
	if rec.ClaudeSessionID != "uuid-disk" {
		t.Fatalf("ClaudeSessionID = %q; want the once-bound uuid-disk", rec.ClaudeSessionID)
	}
	if len(ens.driveableCalls()) != 3 || len(ens.nonWaitingCalls()) != 0 {
		t.Fatalf("driveable=%v non-waiting=%v; want three authoritative opens and no racing calls", ens.driveableCalls(), ens.nonWaitingCalls())
	}
}

func TestRepeatedOpensNeverRebindADiscoveredTranscript(t *testing.T) {
	// Arrange — a NEWER transcript appears after the first open bound an
	// older one. A re-open must not silently move the session onto it: the
	// binding is the conversation the user is in.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	settled := openSettlements(t)
	writeProjectTranscript(t, cfg, "/w", "uuid-first")
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	if err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{}); err != nil {
		t.Fatalf("first Open: %v", err)
	}
	<-settled
	writeProjectTranscript(t, cfg, "/w", "uuid-second")

	// Act
	if err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{}); err != nil {
		t.Fatalf("second Open: %v", err)
	}
	<-settled

	// Assert
	rec, _ := reg.Get("s_1")
	if rec.ClaudeSessionID != "uuid-first" {
		t.Fatalf("ClaudeSessionID = %q; want the original binding kept", rec.ClaudeSessionID)
	}
}

func TestDiscoveryMarksAWorkspaceWithHistoryBackfillPending(t *testing.T) {
	// Arrange — a transcript on disk means this session HAS history owed.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	writeProjectTranscript(t, cfg, "/w", "uuid-disk")
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	o.BindWorkspace("/w")

	// Assert — without this the record reads UNSPECIFIED ("nothing to
	// backfill") until the first line lands, which is the blue window itself.
	rec, _ := reg.Get("s_1")
	if rec.BackfillState != sessioncontroller.BackfillPending {
		t.Fatalf("BackfillState = %q; want pending", rec.BackfillState)
	}
}

func TestDiscoveryLeavesAWorkspaceWithNoTranscriptUnmarked(t *testing.T) {
	// Arrange — a genuinely fresh workspace owes no backfill.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	o.BindWorkspace("/w")

	// Assert — empty is a real, correct answer here, not "unknown".
	rec, _ := reg.Get("s_1")
	if rec.BackfillState != "" {
		t.Fatalf("BackfillState = %q; want empty for a workspace with no history", rec.BackfillState)
	}
}

func TestDiscoveryNeverDowngradesASettledBackfillState(t *testing.T) {
	// Arrange — a session whose backfill already completed. A later switch
	// re-runs discovery, which must not walk it back to pending.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	writeProjectTranscript(t, cfg, "/w", "uuid-disk")
	if err := reg.Put(registry.Record{
		SessionID: "s_1", CWD: "/w", ConfigDir: cfg, CreatedAt: "2026-07-25T10:00:00Z",
		BackfillState: sessioncontroller.BackfillDone,
	}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	o.BindWorkspace("/w")

	// Assert
	rec, _ := reg.Get("s_1")
	if rec.BackfillState != sessioncontroller.BackfillDone {
		t.Fatalf("BackfillState = %q; want done to survive re-discovery", rec.BackfillState)
	}
}

func TestABringUpFailureAfterTheAckLandsInThePushedFailureSurface(t *testing.T) {
	// Arrange — the ack is no longer the failure surface for the bring-up half
	// of an open, so the failure must reach the user by the surface that
	// replaced it. Anything less would be an early ack that swallows failures.
	cfg := t.TempDir()
	o, reg, ens, _ := openerRig(t, cfg)
	ens.err = errBringUp
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act — the open is accepted, and the failure arrives behind it.
	if err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{}); err != nil {
		t.Fatalf("Open refused an acceptable request: %v", err)
	}
	got := <-openFailuresOf(t, o).recorded

	// Assert
	if got.workspace != "/w" || !errors.Is(got.err, errBringUp) {
		t.Fatalf("published failure = %+v, want /w carrying %v", got, errBringUp)
	}
}

func TestOpenPreservesTypedAutomaticRestoreEvidence(t *testing.T) {
	// Arrange — a durable record names a Claude conversation whose transcript
	// disappeared before the workspace was reopened. The evidence rode the nack
	// before the ack moved; it must still be intact where it lands now.
	cfg := t.TempDir()
	o, reg, ens, _ := openerRig(t, cfg)
	missing := &ResumeTranscriptMissingError{
		ResumeID: "claude-lost", CWD: "/w", ConfigDir: cfg,
		ResolvedConfigDir: cfg, SearchedPaths: []string{cfg + "/projects/w/claude-lost.jsonl"},
	}
	ens.err = missing
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, ClaudeSessionID: "claude-lost"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act.
	if err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{}); err != nil {
		t.Fatalf("Open refused an acceptable request: %v", err)
	}
	published := (<-openFailuresOf(t, o).recorded).err
	failure := errclass.Command(nil, published).GetKind().GetSessionResumeFailed().GetDetail()

	// Assert.
	if !errors.Is(published, missing) {
		t.Fatalf("published error = %v, want original continuity error preserved", published)
	}
	if failure == nil || failure.GetAutomaticRestore() == nil || failure.GetTranscriptUnavailable() == nil {
		t.Fatalf("failure = %v, want automatic_restore + transcript_unavailable", errclass.Command(nil, published))
	}
	if failure.GetClaudeSessionId() != "claude-lost" || failure.GetCwd() != "/w" {
		t.Fatalf("failure evidence = %v", failure)
	}
}

func TestOpenDriveablePreservesAutomaticRestoreEvidence(t *testing.T) {
	// Arrange — merge bring-up drives the same exact resume as workspace open
	// and must report the same structured continuity failure.
	cfg := t.TempDir()
	o, reg, ens, _ := openerRig(t, cfg)
	ens.err = errBringUp
	if err := reg.Put(registry.Record{
		SessionID: "s_1", CWD: "/w", ConfigDir: cfg, ClaudeSessionID: "claude-resume",
	}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act.
	err := o.OpenDriveable(context.Background(), "/w")
	failure := errclass.Command(nil, err)
	detail := failure.GetKind().GetSessionResumeFailed().GetDetail()

	// Assert.
	if !errors.Is(err, errBringUp) {
		t.Fatalf("OpenDriveable error = %v, want original bring-up chain preserved", err)
	}
	if errclass.TypeName(failure) != string(errclass.TypeSessionResumeFailed) || detail == nil || detail.GetAutomaticRestore() == nil {
		t.Fatalf("failure = %v, want typed automatic restore", failure)
	}
	if detail.GetClaudeSessionId() != "claude-resume" || detail.GetCwd() != "/w" || detail.GetConfigDir() != cfg || detail.GetResolvedConfigDir() != cfg {
		t.Fatalf("failure evidence = %v", detail)
	}
	if got := detail.GetBringUpFailure(); got == nil || got.GetCause() != errBringUp.Error() {
		t.Fatalf("bring-up failure = %v, want cause %q", got, errBringUp)
	}
}

// --- the driveable bring-up a merge takes --------------------------------

// THE MATRIX A MERGE RUN DECIDES ON, in one table so the three answers cannot
// drift apart: a workspace that never had a session, one whose session is
// merely asleep, and one whose bring-up genuinely fails.
func TestOpenDriveableForEachSessionDisposition(t *testing.T) {
	tests := []struct {
		name string
		// record is the registry record to seed, if any. A zero SessionID
		// seeds nothing, which is the "never had a session" row.
		record registry.Record
		// ensureErr is what the bring-up reports for a workspace that HAS one.
		ensureErr error
		// wantNoSession is whether the call must report merge.ErrNoSession.
		wantNoSession bool
		// wantErr is whether the call must fail at all.
		wantErr bool
		// wantDriveable is the workspaces the DRIVEABLE bring-up was asked for.
		wantDriveable []string
	}{
		{
			name:          "no session record at all: reported as ErrNoSession, nothing brought up",
			wantNoSession: true,
			wantErr:       true,
		},
		{
			name:          "a hibernated (non-terminal) record: brought up and waited for",
			record:        registry.Record{SessionID: "s_1", CWD: "/w", CreatedAt: "2026-07-25T10:00:00Z"},
			wantDriveable: []string{"/w"},
		},
		{
			name:          "a bring-up that fails: surfaced loudly, and NOT as an absent session",
			record:        registry.Record{SessionID: "s_1", CWD: "/w", CreatedAt: "2026-07-25T10:00:00Z"},
			ensureErr:     errBringUp,
			wantErr:       true,
			wantDriveable: []string{"/w"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			cfg := t.TempDir()
			o, reg, ens, _ := openerRig(t, cfg)
			ens.err = tc.ensureErr
			if tc.record.SessionID != "" {
				tc.record.ConfigDir = cfg
				if err := reg.Put(tc.record); err != nil {
					t.Fatalf("put: %v", err)
				}
			}

			// Act.
			err := o.OpenDriveable(context.Background(), "/w")

			// Assert.
			if (err != nil) != tc.wantErr {
				t.Fatalf("OpenDriveable() error = %v, want error = %v", err, tc.wantErr)
			}
			if got := errors.Is(err, merge.ErrNoSession); got != tc.wantNoSession {
				t.Fatalf("errors.Is(%v, merge.ErrNoSession) = %v, want %v: a workspace that never had a session and one whose session would not start must stay distinguishable",
					err, got, tc.wantNoSession)
			}
			if !slices.Equal(ens.driveable, tc.wantDriveable) {
				t.Fatalf("driveable bring-ups = %v, want %v", ens.driveable, tc.wantDriveable)
			}
		})
	}
}

// THE WAITING BRING-UP IS THE ONE A MERGE GETS: Open's non-waiting Ensure would
// hand the run a session whose shim is still handshaking, and the run's next act
// is a send.
func TestOpenDriveableNeverUsesTheNonWaitingEnsure(t *testing.T) {
	// Arrange.
	cfg := t.TempDir()
	o, reg, ens, _ := openerRig(t, cfg)
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act.
	if err := o.OpenDriveable(context.Background(), "/w"); err != nil {
		t.Fatalf("OpenDriveable: %v", err)
	}

	// Assert.
	if len(ens.calls) != 0 {
		t.Fatalf("the non-waiting Ensure was called for %v: a merge's bring-up must wait for the handshake", ens.calls)
	}
}

func TestOpenForMergeUsesTheExplicitMergeRevivalBoundary(t *testing.T) {
	o, reg, ens, logs := openerRig(t)
	if err := reg.Put(registry.Record{SessionID: "s_merge", CWD: "/w", CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	if err := o.OpenForMerge(context.Background(), "/w"); err != nil {
		t.Fatalf("OpenForMerge: %v", err)
	}
	if !slices.Equal(ens.merge, []string{"/w"}) || len(ens.driveable) != 0 || len(ens.calls) != 0 {
		t.Fatalf("ensurer calls merge=%v driveable=%v ordinary=%v, want only merge revival", ens.merge, ens.driveable, ens.calls)
	}
	if !strings.Contains(strings.Join(*logs, "\n"), "merge session revival COMPLETE") {
		t.Fatalf("logs = %v, want merge-revival completion", *logs)
	}
}

func TestCloseStillFailsLoudlyBecauseItIsNotExposedDaemonSide(t *testing.T) {
	// Arrange
	o, _, _, _ := openerRig(t)

	// Act
	err := o.Close(context.Background(), "/w")

	// Assert
	if err == nil {
		t.Fatalf("Close returned nil; the unexposed verb must fail loudly, never no-op")
	}
}

func TestBindAllSkipsTerminalRecords(t *testing.T) {
	// Arrange — a dead conversation must not be re-bound at boot.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	if err := reg.Put(registry.Record{SessionID: "s_dead", CWD: "/w", ConfigDir: cfg, Terminal: true, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	writeProjectTranscript(t, cfg, "/w", "uuid-disk")

	// Act
	o.BindAll()

	// Assert
	rec, _ := reg.Get("s_dead")
	if rec.ClaudeSessionID != "" {
		t.Fatalf("ClaudeSessionID = %q; a terminal record must stay unbound", rec.ClaudeSessionID)
	}
}

func TestBindAllBindsEveryUnboundRegisteredWorkspace(t *testing.T) {
	// Arrange — the boot sweep, which runs before any frontend connects.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	for _, n := range []string{"1", "2"} {
		if err := reg.Put(registry.Record{SessionID: "s_" + n, CWD: "/w" + n, ConfigDir: cfg, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
			t.Fatalf("put %s: %v", n, err)
		}
		writeProjectTranscript(t, cfg, "/w"+n, "uuid-"+n)
	}

	// Act
	o.BindAll()

	// Assert
	for _, n := range []string{"1", "2"} {
		rec, _ := reg.Get("s_" + n)
		if rec.ClaudeSessionID != "uuid-"+n {
			t.Fatalf("s_%s ClaudeSessionID = %q; want uuid-%s", n, rec.ClaudeSessionID, n)
		}
	}
}

func TestBindWorkspaceIgnoresATranscriptForADifferentCWD(t *testing.T) {
	// Arrange — encoding collisions would be catastrophic, so pin the miss.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	writeProjectTranscript(t, cfg, "/other", "uuid-other")

	// Act
	o.BindWorkspace("/w")

	// Assert
	rec, _ := reg.Get("s_1")
	if rec.ClaudeSessionID != "" {
		t.Fatalf("ClaudeSessionID = %q; want empty (no transcript for /w)", rec.ClaudeSessionID)
	}
}

// --- helpers --------------------------------------------------------------

var errBringUp = errors.New("bring-up failed")

func containsSubstring(lines []string, want string) bool {
	for _, l := range lines {
		if strings.Contains(l, want) {
			return true
		}
	}
	return false
}

// --- Open creates the workspace's first session ----------------------------

// recordingCreator is a WorkspaceSessionCreator that registers a record the way
// the real creation entry point does, so the open path's bind + bring-up run
// against a session that actually exists.
type recordingCreator struct {
	reg   *registry.Registry
	opts  []CreateOpts
	id    string
	err   error
	calls int
}

func (c *recordingCreator) CreateSession(_ context.Context, opts CreateOpts) (string, error) {
	c.calls++
	c.opts = append(c.opts, opts)
	if c.err != nil {
		return "", c.err
	}
	if err := c.reg.Put(registry.Record{SessionID: c.id, CWD: opts.CWD}); err != nil {
		return "", err
	}
	return c.id, nil
}

func TestOpenCreatesASessionForAWorkspaceThatHasNone(t *testing.T) {
	// Arrange — a workspace the daemon has never seen. Open is the single
	// establishment path, so it must not report success having done nothing.
	o, reg, ens, _ := openerRig(t)
	settled := openSettlements(t)
	creator := &recordingCreator{reg: reg, id: "s_new"}
	o.Creator = creator

	// Act.
	err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{
		PermissionMode: "acceptEdits",
		ConfigDir:      "/cfg",
		Fake:           true,
	})

	// Assert — created once, for this cwd, carrying the run preferences, and
	// then brought up driveable like any reattach.
	if err != nil {
		t.Fatalf("Open errored: %v", err)
	}
	<-settled
	if creator.calls != 1 {
		t.Fatalf("CreateSession calls = %d, want 1", creator.calls)
	}
	want := CreateOpts{CWD: "/w", PermissionMode: "acceptEdits", ConfigDir: "/cfg", Fake: true}
	if creator.opts[0] != want {
		t.Fatalf("CreateOpts = %+v, want %+v", creator.opts[0], want)
	}
	if !slices.Contains(ens.driveableCalls(), "/w") {
		t.Fatalf("driveable bring-up = %v, want it to include /w", ens.driveableCalls())
	}
}

func TestOpenNeverCreatesWhenTheWorkspaceAlreadyHasASession(t *testing.T) {
	// Arrange — a reattach. Creating here would mint a rival session for a
	// workspace that already has one.
	o, reg, ens, _ := openerRig(t)
	settled := openSettlements(t)
	creator := &recordingCreator{reg: reg, id: "s_rival"}
	o.Creator = creator
	if err := reg.Put(registry.Record{SessionID: "s_existing", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act.
	if err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{}); err != nil {
		t.Fatalf("Open errored: %v", err)
	}
	<-settled

	// Assert.
	if creator.calls != 0 {
		t.Fatalf("CreateSession was called %d times for a workspace with a session", creator.calls)
	}
	if !slices.Contains(ens.driveableCalls(), "/w") {
		t.Fatalf("driveable bring-up = %v, want it to include /w", ens.driveableCalls())
	}
}

func TestOpenSurfacesACreateFailureRatherThanProceeding(t *testing.T) {
	// Arrange — a refused create leaves no session, so continuing into the
	// bind and bring-up would be the open forming its own opinion.
	o, reg, ens, _ := openerRig(t)
	boom := errors.New("create refused")
	o.Creator = &recordingCreator{reg: reg, id: "s_new", err: boom}

	// Act.
	err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{})

	// Assert.
	if !errors.Is(err, boom) {
		t.Fatalf("Open err = %v, want it to wrap %v", err, boom)
	}
	if len(ens.driveableCalls()) != 0 {
		t.Fatalf("a refused create still brought up %v", ens.driveableCalls())
	}
}

func TestOpenFailsLoudlyWithNoCreatorWired(t *testing.T) {
	// Arrange — a missing dependency is a construction error, never a silent
	// return to the old do-nothing behavior.
	o, _, _, _ := openerRig(t)

	// Act.
	err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no session creator wired") {
		t.Fatalf("Open err = %v, want a loud missing-creator failure", err)
	}
}

func TestOnlyOpenCreates(t *testing.T) {
	// Arrange — the other two entry points report a sessionless workspace so
	// their callers can decide what it means. Merging a plain worktree must
	// not conjure a session for it.
	cases := []struct {
		name string
		open func(*WorkspaceOpener) error
	}{
		{"OpenDriveable", func(o *WorkspaceOpener) error {
			return o.OpenDriveable(context.Background(), "/w")
		}},
		{"OpenForMerge", func(o *WorkspaceOpener) error {
			return o.OpenForMerge(context.Background(), "/w")
		}},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			o, reg, _, _ := openerRig(t)
			creator := &recordingCreator{reg: reg, id: "s_new"}
			o.Creator = creator

			// Act.
			err := c.open(o)

			// Assert.
			if !errors.Is(err, merge.ErrNoSession) {
				t.Fatalf("%s err = %v, want merge.ErrNoSession", c.name, err)
			}
			if creator.calls != 0 {
				t.Fatalf("%s created %d sessions; only Open creates", c.name, creator.calls)
			}
		})
	}
}
