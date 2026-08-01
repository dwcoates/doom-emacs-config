package ssm

import (
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// openMergedTest arranges a wired manager whose merge lease (and therefore
// whose merged-teardown authority) is bound to a fake.
func openMergedTest(t *testing.T, ws string) (*Manager, *fakeSessionAuthority, *capLog, string) {
	t.Helper()
	m, cl, path := openTest(t, fakeResolver{"s1": ws})
	auth := &fakeSessionAuthority{}
	if _, err := NewMergeLease(MergeLeaseConfig{Manager: m, Queue: &fakeQueue{}, Interrupter: auth}); err != nil {
		t.Fatalf("NewMergeLease: %v", err)
	}
	return m, auth, cl, path
}

// the merged-at fact -------------------------------------------------------

func TestMergedTransitionEstablishesTheMergedAtFact(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, _, _, _ := openMergedTest(t, ws)

	// Act.
	if err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}

	// Assert.
	got := mustCurrent(t, m, ws).GetMergedAtMs()
	if got <= 0 {
		t.Fatalf("merged_at_ms = %d, want a positive instant", got)
	}
}

func TestUnmergedWorkspaceCarriesNoMergedAt(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, _, _, _ := openMergedTest(t, ws)

	// Act.
	if err := m.ApplyMergeTransition(ws, "merging", "cherry-pick started"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, ws).GetMergedAtMs(); got != 0 {
		t.Fatalf("merged_at_ms = %d for a workspace that never merged, want 0", got)
	}
}

func TestMergedAtSurvivesAReopen(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, _, cl, path := openMergedTest(t, ws)
	if err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}
	want := mustCurrent(t, m, ws).GetMergedAtMs()
	m.Close()

	// Act.
	reopened, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: fakeResolver{"s1": ws}})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { reopened.Close() })

	// Assert.
	if got := mustCurrent(t, reopened, ws).GetMergedAtMs(); got != want {
		t.Fatalf("merged_at_ms after reopen = %d, want %d", got, want)
	}
}

func TestALaterNonMergeTransitionDoesNotMoveMergedAt(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, _, _, _ := openMergedTest(t, ws)
	if err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}
	want := mustCurrent(t, m, ws).GetMergedAtMs()

	// Act.
	if err := m.Apply(evTurnStarted("s1", 10)); err != nil {
		t.Fatalf("Apply(TurnStarted): %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, ws).GetMergedAtMs(); got != want {
		t.Fatalf("merged_at_ms after a later non-merge transition = %d, want the original %d", got, want)
	}
}

func TestASecondMergedTransitionKeepsTheFirstInstant(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, _, _, _ := openMergedTest(t, ws)
	if err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}
	want := mustCurrent(t, m, ws).GetMergedAtMs()

	// Act.
	if err := m.ApplyMergeTransition(ws, "merged", "second cherry-pick landed"); err != nil {
		t.Fatalf("second ApplyMergeTransition: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, ws).GetMergedAtMs(); got != want {
		t.Fatalf("merged_at_ms after a second merge = %d, want the first landing %d", got, want)
	}
}

func TestASecondMergedTransitionLogsThatTheFirstInstantIsKept(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, _, cl, _ := openMergedTest(t, ws)
	if err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}

	// Act.
	if err := m.ApplyMergeTransition(ws, "merged", "second cherry-pick landed"); err != nil {
		t.Fatalf("second ApplyMergeTransition: %v", err)
	}

	// Assert.
	if !cl.contains("merged-at ws=" + ws + " decision=keep") {
		t.Fatalf("no canonical log line for the kept merged-at; lines=%v", cl.lines)
	}
}

func TestMergedAtRecordFailureAbortsTheTransition(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, _, _, _ := openMergedTest(t, ws)
	if _, err := m.db.Exec(`DROP TABLE workspace_merged`); err != nil {
		t.Fatalf("drop workspace_merged: %v", err)
	}

	// Act.
	err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "record merged-at") {
		t.Fatalf("ApplyMergeTransition error = %v, want a merged-at record failure", err)
	}
}

func TestMergedAtRecordFailureIsLoggedCanonically(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, _, cl, _ := openMergedTest(t, ws)
	if _, err := m.db.Exec(`DROP TABLE workspace_merged`); err != nil {
		t.Fatalf("drop workspace_merged: %v", err)
	}

	// Act.
	if err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed"); err == nil {
		t.Fatal("ApplyMergeTransition succeeded over a missing workspace_merged table")
	}

	// Assert.
	if !cl.contains("merged-at record FAILED workspace=" + ws) {
		t.Fatalf("no canonical log line for the merged-at record failure; lines=%v", cl.lines)
	}
}

// the pushed and snapshotted surface --------------------------------------

func TestTheMergedPushCarriesMergedAt(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, _, _, _ := openMergedTest(t, ws)
	states, cancel := m.Subscribe()
	t.Cleanup(cancel)

	// Act.
	if err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}

	// Assert.
	msg := firstStateFor(t, states, ws, frontendv1.RenderState_RENDER_STATE_MERGED)
	if msg.GetMergedAtMs() <= 0 {
		t.Fatalf("pushed merged state carries merged_at_ms=%d, want a positive instant", msg.GetMergedAtMs())
	}
}

func TestSnapshotIncludesTheMergedWorkspaceWithItsMergedAt(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, _, _, _ := openMergedTest(t, ws)
	if err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}

	// Act.
	snap, err := m.Snapshot()
	if err != nil {
		t.Fatalf("Snapshot: %v", err)
	}

	// Assert.
	var found *frontendv1.WorkspaceState
	for _, s := range snap {
		if s.GetWorkspace() == ws {
			found = s
		}
	}
	if found == nil {
		t.Fatalf("Snapshot omitted the merged workspace %s (%d entries)", ws, len(snap))
	}
	if found.GetMergedAtMs() <= 0 {
		t.Fatalf("snapshotted merged workspace carries merged_at_ms=%d, want a positive instant", found.GetMergedAtMs())
	}
}

func TestSnapshotAfterAReopenStillCarriesMergedAt(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, _, cl, path := openMergedTest(t, ws)
	if err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}
	want := mustCurrent(t, m, ws).GetMergedAtMs()
	m.Close()
	reopened, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: fakeResolver{"s1": ws}})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { reopened.Close() })

	// Act.
	snap, err := reopened.Snapshot()
	if err != nil {
		t.Fatalf("Snapshot: %v", err)
	}

	// Assert.
	for _, s := range snap {
		if s.GetWorkspace() != ws {
			continue
		}
		if s.GetMergedAtMs() != want {
			t.Fatalf("snapshotted merged_at_ms after reopen = %d, want %d", s.GetMergedAtMs(), want)
		}
		return
	}
	t.Fatalf("Snapshot after reopen omitted the merged workspace %s", ws)
}

// the teardown -------------------------------------------------------------

func TestMergedTransitionStandsTheSessionDown(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, auth, _, _ := openMergedTest(t, ws)

	// Act.
	if err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}

	// Assert.
	if len(auth.tornDown) != 1 || auth.tornDown[0] != ws {
		t.Fatalf("teardowns = %v, want exactly [%s]", auth.tornDown, ws)
	}
}

func TestANonMergedTransitionStandsNothingDown(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, auth, _, _ := openMergedTest(t, ws)

	// Act.
	if err := m.ApplyMergeTransition(ws, "merge_conflict", "conflict left in tree"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}

	// Assert.
	if len(auth.tornDown) != 0 {
		t.Fatalf("teardowns = %v, want none for a non-merged transition", auth.tornDown)
	}
}

func TestTheMergedStateIsPushedBeforeTheTeardownRuns(t *testing.T) {
	// Arrange. The authority reads the subscriber channel from inside its own
	// teardown, which can only succeed if the merged frame was already queued.
	const ws = "/ws/alpha"
	m, auth, _, _ := openMergedTest(t, ws)
	states, cancel := m.Subscribe()
	t.Cleanup(cancel)
	var seen frontendv1.RenderState
	auth.teardownHook = func(string) {
		seen = firstStateFor(t, states, ws, frontendv1.RenderState_RENDER_STATE_MERGED).GetState()
	}

	// Act.
	if err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}

	// Assert.
	if seen != frontendv1.RenderState_RENDER_STATE_MERGED {
		t.Fatalf("state visible to the teardown = %s, want MERGED already pushed", seen)
	}
}

func TestATeardownFailureIsLoggedAndDoesNotFailTheMerge(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, auth, cl, _ := openMergedTest(t, ws)
	auth.teardownErr = errors.New("shim would not stop")

	// Act.
	err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed")

	// Assert.
	if err != nil {
		t.Fatalf("ApplyMergeTransition = %v, want nil: a teardown failure must not report a landed merge as failed", err)
	}
	if !cl.contains("merged teardown FAILED ws=" + ws) {
		t.Fatalf("no canonical log line for the teardown failure; lines=%v", cl.lines)
	}
}

func TestATeardownFailureLeavesTheMergedFactIntact(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, auth, _, _ := openMergedTest(t, ws)
	auth.teardownErr = errors.New("shim would not stop")

	// Act.
	if err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}

	// Assert.
	cur := mustCurrent(t, m, ws)
	if cur.GetState() != frontendv1.RenderState_RENDER_STATE_MERGED || cur.GetMergedAtMs() <= 0 {
		t.Fatalf("after a failed teardown state=%s merged_at_ms=%d, want MERGED with a positive instant",
			cur.GetState(), cur.GetMergedAtMs())
	}
}

func TestAnUnboundTeardownIsLoudlyNoted(t *testing.T) {
	// Arrange. No merge lease, so no session authority is bound.
	const ws = "/ws/alpha"
	m, cl, _ := openTest(t, fakeResolver{"s1": ws})

	// Act.
	if err := m.ApplyMergeTransition(ws, "merged", "cherry-pick landed"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}

	// Assert.
	if !cl.contains("merged teardown ws="+ws+" merged_at_ms=") || !cl.contains("decision=none") {
		t.Fatalf("no canonical log line for the unbound teardown; lines=%v", cl.lines)
	}
}

func TestBindingASecondTeardownIsRefused(t *testing.T) {
	// Arrange.
	const ws = "/ws/alpha"
	m, _, _, _ := openMergedTest(t, ws)

	// Act.
	err := m.bindMergedTeardown(&fakeSessionAuthority{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "already bound") {
		t.Fatalf("second bindMergedTeardown = %v, want an already-bound refusal", err)
	}
}

func TestBindingANilTeardownIsRefused(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "/ws/alpha"})

	// Act.
	err := m.bindMergedTeardown(nil)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "nil MergedTeardown") {
		t.Fatalf("bindMergedTeardown(nil) = %v, want a nil refusal", err)
	}
}

// firstStateFor drains pushes until it sees ws in the wanted state, failing the
// test if the channel closes first. It is a rendezvous on the push itself, so
// nothing here waits on a duration.
func firstStateFor(t *testing.T, states <-chan *frontendv1.WorkspaceState, ws string, want frontendv1.RenderState) *frontendv1.WorkspaceState {
	t.Helper()
	for msg := range states {
		if msg.GetWorkspace() == ws && msg.GetState() == want {
			return msg
		}
	}
	t.Fatalf("push channel closed before %s reached %s", ws, want)
	return nil
}
