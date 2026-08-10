package create

import (
	"context"
	"errors"
	"path/filepath"
	"testing"
)

// The publication gate holds a worktree's session frames until its creation job
// is materialized, and "not materialized" used to have no terminating event of
// its own. These tests pin the terminal disposition that ends every hold: the
// job is acknowledged, or the daemon abandons the wait, and there is no third
// outcome in which the gate simply keeps holding.

// failBeforeMaterialization drives one job to StateFailed the way the observed
// zombies got there: the worktree and the session are both real, the health
// probe never came back, and the job died before its WorkspaceAvailable was ever
// published — so no host acknowledgement can ever be written against it.
func failBeforeMaterialization(t *testing.T, f *fixture, id string) Job {
	t.Helper()
	f.health.err = errors.New("the shim connection never completed its handshake")
	if _, _, err := f.store.Enqueue(Job{ID: id, Request: Request{Name: "DWC/" + id, GitRoot: "/repo"}, State: StateQueued}); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	err := f.manager.Process(context.Background(), id)
	if !errors.Is(err, ErrJobFailed) {
		t.Fatalf("Process error = %v, want a contained job failure", err)
	}
	got := job(t, f.store, id)
	if got.State != StateFailed {
		t.Fatalf("job state = %s, want failed", got.State)
	}
	return got
}

func TestTerminalFailureBeforeMaterializationAbandonsTheHold(t *testing.T) {
	// Arrange & Act.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	failed := failBeforeMaterialization(t, f, "zombie")

	// Assert: the disposition is durable and names its reason.
	if !failed.PublicationAbandoned {
		t.Fatal("a job that failed before materialization is still holding its publication")
	}
	if failed.PublicationAbandonedReason != string(AbandonTerminalFailure) {
		t.Fatalf("abandon reason = %q, want %q", failed.PublicationAbandonedReason, AbandonTerminalFailure)
	}
}

func TestAbandonedHoldStopsGatingItsWorktreesFrames(t *testing.T) {
	// Arrange.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	failed := failBeforeMaterialization(t, f, "zombie")

	// Act: the gate is asked the same question every frame for that worktree
	// asks.
	decision, err := SessionPublicationDecision(f.store, failed.WorktreePath, failed.SessionID)
	if err != nil {
		t.Fatalf("SessionPublicationDecision: %v", err)
	}

	// Assert: the frames pass, and the gate can still say WHY they pass.
	if !decision.Materialized {
		t.Fatal("an abandoned hold is still gating its worktree's frames")
	}
	if !decision.Abandoned {
		t.Fatal("the gate reports an abandoned hold as an ordinary acknowledgement")
	}
	if decision.AbandonedReason != string(AbandonTerminalFailure) {
		t.Fatalf("decision reason = %q, want %q", decision.AbandonedReason, AbandonTerminalFailure)
	}
}

func TestAbandonmentIsReportedAsAFaultExactlyOnce(t *testing.T) {
	// Arrange: a job whose hold has already been disposed of.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	failBeforeMaterialization(t, f, "zombie")
	if got := f.countErrorRecords("HELD PUBLICATION ABANDONED"); got != 1 {
		t.Fatalf("abandonment records after the failure = %d, want 1", got)
	}

	// Act: every later sweep sees the same terminal job.
	for range 3 {
		if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
			t.Fatalf("SweepAwaitingHost: %v", err)
		}
	}

	// Assert: the durable latch is what makes the report exactly once, so a
	// sweep that runs every few seconds forever cannot turn it into spam.
	if got := f.countErrorRecords("HELD PUBLICATION ABANDONED"); got != 1 {
		t.Fatalf("abandonment records after three sweeps = %d, want 1", got)
	}
}

func TestSweepAbandonsAHoldLeftBehindByAPreviousDaemon(t *testing.T) {
	// Arrange: the store shape the observed incident had at boot — a job
	// already failed before materialization, persisted by a daemon that is
	// gone, with nothing in this process that ever ran its failure path.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	if _, _, err := f.store.Enqueue(Job{
		ID:           "inherited",
		Request:      Request{Name: "DWC/inherited", GitRoot: "/repo"},
		State:        StateFailed,
		WorktreePath: "/worktrees/inherited",
		SessionID:    "s_inherited",
		LastError:    "await session health: the shim never dialled in",
	}); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act: the first sweep after boot.
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert.
	got := job(t, f.store, "inherited")
	if !got.PublicationAbandoned {
		t.Fatal("a hold inherited from a previous daemon survived the boot sweep")
	}
	if got.PublicationAbandonedReason != string(AbandonTerminalFailure) {
		t.Fatalf("abandon reason = %q, want %q", got.PublicationAbandonedReason, AbandonTerminalFailure)
	}
}

func TestAbandonmentReopensTheGateThroughThePublicationPort(t *testing.T) {
	// Arrange & Act: the gate's memo is invalidated through the one port that
	// already tells it a worktree's verdict changed, so a live daemon does not
	// have to be restarted to observe the disposition.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	failed := failBeforeMaterialization(t, f, "zombie")

	// Assert: the last preparation the gate was handed carries the abandonment.
	if len(f.publication.jobs) == 0 {
		t.Fatal("the publication gate was never told about the abandonment")
	}
	last := f.publication.jobs[len(f.publication.jobs)-1]
	if last.ID != failed.ID || !last.PublicationAbandoned {
		t.Fatalf("last publication preparation = %+v, want the abandoned job %s", last, failed.ID)
	}
}

func TestAbandonmentThatCannotReopenTheGateIsReportedAsAFault(t *testing.T) {
	// Arrange: the durable disposition is taken, the in-memory gate refuses it.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	f.publication.err = errors.New("gate is gone")

	// Act.
	if _, _, err := f.store.Enqueue(Job{
		ID: "inherited", Request: Request{Name: "DWC/inherited", GitRoot: "/repo"},
		State: StateFailed, WorktreePath: "/worktrees/inherited", SessionID: "s_inherited",
	}); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert: the durable record still stands, and the failure to act on it is
	// loud rather than swallowed.
	if got := job(t, f.store, "inherited"); !got.PublicationAbandoned {
		t.Fatal("the durable disposition was rolled back by a gate failure")
	}
	if !f.loggedErrorFormat("ABANDONED PUBLICATION GATE NOT REOPENED") {
		t.Fatal("a gate that refused the abandonment was not reported as a fault")
	}
}

func TestAbandonmentThatCannotLatchIsReportedAndRetried(t *testing.T) {
	// Arrange: an unwritable store must not produce a report the next sweep
	// can never repeat.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	failing := &unwritableStore{JobStore: f.store}
	f.manager.cfg.Store = failing
	if _, _, err := f.store.Enqueue(Job{
		ID: "inherited", Request: Request{Name: "DWC/inherited", GitRoot: "/repo"},
		State: StateFailed, WorktreePath: "/worktrees/inherited", SessionID: "s_inherited",
	}); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act.
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert.
	if !f.loggedErrorFormat("HELD PUBLICATION ABANDONMENT COULD NOT LATCH") {
		t.Fatal("a latch failure was not reported as a fault")
	}
	if f.countErrorRecords("HELD PUBLICATION ABANDONED id=") != 0 {
		t.Fatal("an abandonment was reported without its durable latch")
	}
}

func TestParkedJobWhoseWorktreeVanishedIsAbandonedRatherThanReRequested(t *testing.T) {
	// Arrange: a job still parked on the host, whose worktree was deleted while
	// the daemon was down. Boot replay would otherwise ask the editor to
	// materialize a directory that is not there, on every host connect forever.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "gone")
	f.removeWorktree(job(t, f.store, "gone").WorktreePath)
	before := f.available.calls

	// Act: the interval elapses, so a live job WOULD be re-requested here.
	f.advance(defaultMaterializationRequestInterval)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert.
	got := job(t, f.store, "gone")
	if !got.PublicationAbandoned {
		t.Fatal("a parked job whose worktree is gone is still waiting on the host")
	}
	if got.PublicationAbandonedReason != string(AbandonWorktreeGone) {
		t.Fatalf("abandon reason = %q, want %q", got.PublicationAbandonedReason, AbandonWorktreeGone)
	}
	if f.available.calls != before {
		t.Fatalf("available publishes = %d, want %d (a dead worktree is never re-requested)", f.available.calls, before)
	}
}

func TestParkedJobWithALiveWorktreeIsStillReRequested(t *testing.T) {
	// Arrange: the liveness check must not abandon the jobs it exists to
	// protect — a workspace that is really there and really waiting.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "live")
	before := f.available.calls

	// Act.
	f.advance(defaultMaterializationRequestInterval)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert.
	if got := job(t, f.store, "live"); got.PublicationAbandoned {
		t.Fatal("a live parked job was abandoned by the liveness check")
	}
	if f.available.calls != before+1 {
		t.Fatalf("available publishes = %d, want %d", f.available.calls, before+1)
	}
}

func TestAbandonedJobIsNeverReRequestedAgain(t *testing.T) {
	// Arrange: an already-abandoned parked job.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "gone")
	f.removeWorktree(job(t, f.store, "gone").WorktreePath)
	f.advance(defaultMaterializationRequestInterval)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}
	before := f.available.calls

	// Act: the worktree comes back, but the disposition is terminal.
	f.advance(10 * defaultMaterializationRequestInterval)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert.
	if f.available.calls != before {
		t.Fatalf("available publishes = %d, want %d (an abandoned job is never re-asked)", f.available.calls, before)
	}
}

// unwritableStore is a JobStore whose Update always fails, standing in for the
// durable latch that could not be taken.
type unwritableStore struct {
	JobStore
}

func (s *unwritableStore) Update(string, func(*Job) error) (Job, error) {
	return Job{}, errors.New("store is unwritable")
}
