package server

import (
	"fmt"
	"sort"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// countingCreationBridge counts the durable publication questions one snapshot
// composition asks. The bridge answers them by listing the whole job store, so
// the count IS the composition's complexity in the roster size.
type countingCreationBridge struct {
	*fakeWorkspaceCreation
	calls int
}

func (b *countingCreationBridge) SessionPublicationDecision(worktreePath, sessionID string) (SessionPublicationDecision, error) {
	b.calls++
	return b.fakeWorkspaceCreation.SessionPublicationDecision(worktreePath, sessionID)
}

// rosterSessions is a SessionMetaSource with a fixed roster.
type rosterSessions struct{ views []*frontendv1.SessionView }

func (f rosterSessions) SessionViews() []*frontendv1.SessionView { return f.views }

// rosterProgress is a ProgressSource over the same workspaces, so the same
// (workspace, session) question is asked by a second view family.
type rosterProgress struct{ views []*frontendv1.ProgressView }

func (f rosterProgress) Snapshot() []*frontendv1.ProgressView { return f.views }

// rosterQueues is a QueueSource over those workspaces — a third family.
type rosterQueues struct{ views []*frontendv1.QueueView }

func (f rosterQueues) QueueViews() []*frontendv1.QueueView { return f.views }

// rosterInits is a SessionInitSource — a fourth family over the workspaces.
type rosterInits struct{ views []*frontendv1.SessionInitView }

func (f rosterInits) SessionInits() []*frontendv1.SessionInitView { return f.views }

// rosterCatalogs is a TaskCatalogSource — its two halves are a fifth and sixth
// family asking the same questions again.
type rosterCatalogs struct {
	catalogs []*frontendv1.TaskCatalog
	bubbles  []*frontendv1.AsyncBubble
}

func (f rosterCatalogs) TaskCatalogs() []*frontendv1.TaskCatalog { return f.catalogs }
func (f rosterCatalogs) AsyncBubbles() []*frontendv1.AsyncBubble { return f.bubbles }

// rosterSources fills a provider with one view per family per workspace, which
// is what a real connect snapshot composes.
func rosterSources(provider *ssmSnapshotProvider, workspaces int) {
	sessions := make([]*frontendv1.SessionView, 0, workspaces)
	progress := make([]*frontendv1.ProgressView, 0, workspaces)
	queues := make([]*frontendv1.QueueView, 0, workspaces)
	inits := make([]*frontendv1.SessionInitView, 0, workspaces)
	catalogs := make([]*frontendv1.TaskCatalog, 0, workspaces)
	bubbles := make([]*frontendv1.AsyncBubble, 0, workspaces)
	for i := range workspaces {
		ws := fmt.Sprintf("/ws/%d", i)
		sessions = append(sessions, &frontendv1.SessionView{Workspace: ws, SessionId: "s"})
		progress = append(progress, &frontendv1.ProgressView{Workspace: ws})
		queues = append(queues, &frontendv1.QueueView{Workspace: ws})
		inits = append(inits, &frontendv1.SessionInitView{Workspace: ws})
		catalogs = append(catalogs, &frontendv1.TaskCatalog{Workspace: ws})
		bubbles = append(bubbles, &frontendv1.AsyncBubble{Id: "bubble:" + ws, Workspace: ws})
	}
	provider.sessions = rosterSessions{views: sessions}
	provider.progress = rosterProgress{views: progress}
	provider.queues = rosterQueues{views: queues}
	provider.inits = rosterInits{views: inits}
	provider.catalogs = rosterCatalogs{catalogs: catalogs, bubbles: bubbles}
}

func rosterProvider(t *testing.T, workspaces int) (*ssmSnapshotProvider, *countingCreationBridge) {
	t.Helper()
	bridge := &countingCreationBridge{fakeWorkspaceCreation: newFakeWorkspaceCreation()}
	provider := &ssmSnapshotProvider{workspaceCreation: bridge}
	rosterSources(provider, workspaces)
	return provider, bridge
}

// A composition asks the durable gate once per DISTINCT identity, not once per
// view. Every family repeats the same questions, and the bridge answers each by
// listing the whole job store under the store's mutex — which a concurrent boot
// sweep holds while it persists — so an un-memoized gate made the connect
// snapshot cost O(views x jobs) and serialized it against the sweep.
func TestSnapshotAsksThePublicationGateOncePerIdentity(t *testing.T) {
	// Arrange: six view families over the same eight workspaces.
	provider, bridge := rosterProvider(t, 8)

	// Act.
	provider.Snapshot()

	// Assert: 8 session identities plus 8 fenced (session-less) ones.
	if bridge.calls != 16 {
		t.Fatalf("publication gate calls = %d, want 16 (one per distinct identity, not one per view)", bridge.calls)
	}
}

// The gate count must grow LINEARLY with the roster: doubling the roster
// doubles the questions rather than squaring them.
func TestSnapshotPublicationGateCostGrowsLinearlyWithTheRoster(t *testing.T) {
	// Arrange.
	small, smallBridge := rosterProvider(t, 16)
	large, largeBridge := rosterProvider(t, 32)

	// Act.
	small.Snapshot()
	large.Snapshot()

	// Assert.
	if largeBridge.calls != 2*smallBridge.calls {
		t.Fatalf("gate calls %d for a doubled roster, want exactly %d", largeBridge.calls, 2*smallBridge.calls)
	}
}

// A HELD workspace stays held for every family in the same composition: the
// memo caches the verdict, so caching must not turn a hold into an allow.
func TestSnapshotMemoKeepsAHeldWorkspaceHeldAcrossFamilies(t *testing.T) {
	// Arrange.
	provider, bridge := rosterProvider(t, 2)
	bridge.decisions["/ws/1\x00s"] = SessionPublicationDecision{JobID: "j1", WorktreePath: "/ws/1", SessionID: "s"}
	bridge.decisions["/ws/1\x00"] = SessionPublicationDecision{JobID: "j1", WorktreePath: "/ws/1"}

	// Act.
	snap := provider.Snapshot()

	// Assert.
	for _, view := range snap.GetSessions() {
		if view.GetWorkspace() == "/ws/1" {
			t.Fatal("a held workspace's session view was published")
		}
	}
	for _, view := range snap.GetProgress() {
		if view.GetWorkspace() == "/ws/1" {
			t.Fatal("a held workspace's progress view was published")
		}
	}
}

// storeBackedBridge answers the publication question the way the real bridge
// does: by listing the whole durable job store — a copy and a sort of every job
// — for every question asked.
type storeBackedBridge struct {
	*fakeWorkspaceCreation
	jobs []string
}

func (b *storeBackedBridge) SessionPublicationDecision(worktreePath, sessionID string) (SessionPublicationDecision, error) {
	listed := make([]string, len(b.jobs))
	copy(listed, b.jobs)
	sort.Strings(listed)
	for _, job := range listed {
		if job == worktreePath {
			break
		}
	}
	return SessionPublicationDecision{WorktreePath: worktreePath, SessionID: sessionID, Materialized: true}, nil
}

// BenchmarkSnapshotComposition pins the composition cost at this user's live
// roster shape (178 workspaces, 289 durable jobs). Un-memoized it is
// O(views x jobs); memoized it is O(views + jobs).
func BenchmarkSnapshotComposition(b *testing.B) {
	const workspaces = 178
	jobs := make([]string, 0, 289)
	for i := range 289 {
		jobs = append(jobs, fmt.Sprintf("/ws/%d", i))
	}
	bridge := &storeBackedBridge{fakeWorkspaceCreation: newFakeWorkspaceCreation(), jobs: jobs}
	provider := &ssmSnapshotProvider{workspaceCreation: bridge}
	rosterSources(provider, workspaces)
	b.ResetTimer()
	for range b.N {
		provider.Snapshot()
	}
}
