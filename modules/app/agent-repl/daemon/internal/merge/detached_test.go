package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// theMergeWorkspace is the merge workspace the harness's fake forks for a
// detached request.
const theMergeWorkspace = ids.WorkspaceID("ws-merge")

// fakeDetached stands for the detached-merge service: its fork registers the
// harness's merge workspace in the fake registry, and every handback is kept.
type fakeDetached struct {
	h *harness

	mu        sync.Mutex
	forkErr   error
	forks     []string
	handbacks []handback
}

// handback is one HandBack the fake was handed.
type handback struct {
	requester ids.WorkspaceID
	mergeWS   ids.WorkspaceID
	outcome   DetachedOutcome
}

func (d *fakeDetached) Fork(_ context.Context, requester wsm.Workspace, request wsm.DetachedMergeID, branch string) (wsm.Workspace, error) {
	d.mu.Lock()
	d.forks = append(d.forks, fmt.Sprintf("%s:%s", requester.ID, branch))
	err := d.forkErr
	d.mu.Unlock()
	if err != nil {
		return wsm.Workspace{}, err
	}
	return d.h.registerMergeWorkspace(requester.ID, request, branch), nil
}

func (d *fakeDetached) HandBack(_ context.Context, requester ids.WorkspaceID, mergeWS wsm.Workspace, out DetachedOutcome) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.handbacks = append(d.handbacks, handback{requester: requester, mergeWS: mergeWS.ID, outcome: out})
	return nil
}

// sent answers every handback, in order.
func (d *fakeDetached) sent() []handback {
	d.mu.Lock()
	defer d.mu.Unlock()
	return append([]handback(nil), d.handbacks...)
}

// registerMergeWorkspace registers the merge workspace forked from requester
// for request, cut from branch, in the harness's one repository.
func (h *harness) registerMergeWorkspace(requester ids.WorkspaceID, request wsm.DetachedMergeID, branch string) wsm.Workspace {
	dir := h.t.TempDir()
	ws := wsm.Workspace{ID: theMergeWorkspace, Name: "feature-merge", Dir: dir, Branch: "feature-merge", Repo: theRepository,
		Parent: &requester, MergeOf: &wsm.MergeOrigin{Requester: requester, Request: request}}
	h.db.mu.Lock()
	h.db.workspaces[ws.ID] = ws
	h.db.jobs[ws.ID] = wsm.CreationJob{Workspace: ws.ID, BaseRef: branch, Layout: wsm.MergeLayout{
		SourceBranch: "feature-merge", SourceDir: dir, TargetDir: h.targetD, Origin: "create"}}
	h.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID}
	h.db.mu.Unlock()
	h.git.mu.Lock()
	h.git.branches[dir] = "feature-merge"
	h.git.commonDirs[dir] = string(h.repoKey())
	h.git.mu.Unlock()
	return ws
}

// HandOffDetachedMerge stands for the durable hand-off: the requester's
// detached request is replaced by the merge workspace's own, still requested.
func (f *fakeDB) HandOffDetachedMerge(_ context.Context, repo wsm.RepoKey, requester ids.WorkspaceID, request wsm.DetachedMergeID, mergeWS ids.WorkspaceID, branch string, at time.Time) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	for i, entry := range f.queues[repo] {
		if entry.Workspace != requester {
			continue
		}
		if entry.State != wsm.MergeRequested || entry.Source.Detach != request {
			return fmt.Errorf("the merge of %s is not the requested detached merge %s", requester, request)
		}
		f.queues[repo] = append(f.queues[repo][:i], f.queues[repo][i+1:]...)
		f.queues[repo] = append(f.queues[repo], wsm.MergeQueueEntry{
			Repo: repo, Workspace: mergeWS, State: wsm.MergeRequested,
			Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, Branch: branch}, EnqueuedAt: at,
		})
		f.renumber(repo)
		return nil
	}
	return fmt.Errorf("no queue entry for %s: %w", requester, wsm.ErrNotFound)
}

// requestDetached asks for the harness workspace's own branch to be merged
// detached, as BY asks.
func requestDetached(t *testing.T, h *harness, by Requester) error {
	t.Helper()
	return h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}, By: by, Detached: true})
}

// entryOf answers ws's queue entry, false when it has none.
func (h *harness) entryOf(ws ids.WorkspaceID) (wsm.MergeQueueEntry, bool) {
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	for _, entry := range entries {
		if entry.Workspace == ws {
			return entry, true
		}
	}
	return wsm.MergeQueueEntry{}, false
}

// headsOn answers every merge head published on ws's root feed, in order.
func (f *fakeFeed) headsOn(ws ids.WorkspaceID) []*frontendv1.FeedMerge {
	f.mu.Lock()
	defer f.mu.Unlock()
	var out []*frontendv1.FeedMerge
	for _, row := range f.rows {
		if row.WS == ws && row.Feed.Root {
			if merge := row.Row.GetActivity().GetMerge(); merge != nil {
				out = append(out, merge)
			}
		}
	}
	return out
}

// tabsOn answers every merge tab published on ws's feeds, in order.
func (f *fakeFeed) tabsOn(ws ids.WorkspaceID) []*frontendv1.FeedMergeTab {
	f.mu.Lock()
	defer f.mu.Unlock()
	var out []*frontendv1.FeedMergeTab
	for _, row := range f.rows {
		if row.WS == ws && row.Row.GetMergeTab() != nil {
			out = append(out, row.Row.GetMergeTab())
		}
	}
	return out
}

func TestADetachedRequestMintsItsOwnIdentity(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.mu.Lock()
	h.inFlight = "turn-asking"
	h.mu.Unlock()

	// Act: an agent's request, still waiting on the turn that asked.
	if err := requestDetached(t, h, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert
	entry, ok := h.entryOf(theWorkspace)
	if !ok || entry.Source.Detach == "" || entry.State != wsm.MergeRequested {
		t.Fatalf("entry = %+v, %v; want a requested detached request with its identity", entry, ok)
	}
}

func TestADetachedRequestThatIsNotAnOwnBranchMergeIsAnErrorWithNothingRecorded(t *testing.T) {
	tests := []struct {
		name   string
		source wsm.MergeSource
	}{
		{name: "kept open", source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, KeepOpen: true}},
		{name: "merged upstream", source: wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: tt.source, By: RequestedByUser, Detached: true})

			// Assert
			if err == nil {
				t.Fatal("Enqueue = nil, want the contradiction refused")
			}
			if _, ok := h.entryOf(theWorkspace); ok {
				t.Fatal("a contradictory detached request was recorded")
			}
			if _, ok := h.recordFor(dlog.LevelError, "daemon.merge.enqueue"); !ok {
				t.Fatal("the contradiction was not recorded at ERROR")
			}
		})
	}
}

func TestAUsersDetachedRequestDisplacesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert
	h.o.mu.Lock()
	displaces := h.o.displaces[theWorkspace] || h.o.displaces[theMergeWorkspace]
	h.o.mu.Unlock()
	if displaces {
		t.Fatal("a detached merge was marked to displace a turn")
	}
}

func TestADetachedRequestIsHandedToTheMergeWorkspaceForkedForIt(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act: the user's ask is put in line at once, which hands it off.
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert: the fork was of the requester's branch, the requester holds no
	// entry, and the merge workspace's own in-place merge is in line.
	if forks := h.detached.forks; len(forks) != 1 || forks[0] != string(theWorkspace)+":feature" {
		t.Fatalf("forks = %v, want one of the requester's branch", forks)
	}
	if _, ok := h.entryOf(theWorkspace); ok {
		t.Fatal("the requester still holds a queue entry after the hand-off")
	}
	entry, ok := h.entryOf(theMergeWorkspace)
	if !ok || entry.State != wsm.MergeQueued || entry.Source.Detach != "" || entry.Source.KeepOpen {
		t.Fatalf("merge workspace entry = %+v, %v; want its own in-place merge in line", entry, ok)
	}
}

func TestAForkThatFailsDropsTheRequestAndTellsTheRequester(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.detached.forkErr = errors.New("the worktree could not be made")

	// Act
	err := requestDetached(t, h, RequestedByUser)

	// Assert
	if err != nil {
		t.Fatalf("Enqueue = %v, want the request answered and the failure handed back", err)
	}
	if _, ok := h.entryOf(theWorkspace); ok {
		t.Fatal("a request whose fork failed still stands in the queue")
	}
	sent := h.detached.sent()
	if len(sent) != 1 || sent[0].requester != theWorkspace || sent[0].outcome.Kind != DetachedAbandoned ||
		!strings.Contains(sent[0].outcome.Summary, "the worktree could not be made") || sent[0].outcome.Branch != "feature" {
		t.Fatalf("handbacks = %+v, want one abandoned handback naming the fork's failure", sent)
	}
	if _, ok := h.recordFor(dlog.LevelError, "daemon.merge.detached"); !ok {
		t.Fatal("the failed fork was not recorded at ERROR")
	}
}

func TestADetachedMergesBubbleIsDrawnInTheRequestersFeedWithTheSwitch(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert: both feeds carry the head; only the requester's has the switch,
	// naming the merge workspace.
	own, mirrored := h.feed.headsOn(theMergeWorkspace), h.feed.headsOn(theWorkspace)
	if len(own) == 0 || len(mirrored) == 0 {
		t.Fatalf("heads: merge workspace %d, requester %d; want the bubble in both", len(own), len(mirrored))
	}
	if own[len(own)-1].GetHead().GetWorkspaceSwitch() != nil {
		t.Fatal("the merge workspace's own bubble carries a switch to itself")
	}
	target := mirrored[len(mirrored)-1].GetHead().GetWorkspaceSwitch().GetAvailable().GetTarget()
	if target.GetId() != string(theMergeWorkspace) || target.GetDir() == "" {
		t.Fatalf("requester's switch target = %+v, want the merge workspace's ref", target)
	}
}

func TestAnAgenticTabCarriesTheSwitchInTheRequestersPlacementOnly(t *testing.T) {
	tests := []struct {
		name    string
		kind    string
		tab     *frontendv1.FeedMergeTab
		agentic bool
	}{
		{name: "conflicts", kind: TabConflicts, agentic: true, tab: &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Conflicts{Conflicts: &frontendv1.FeedMergeTabConflicts{}}}},
		{name: "fixes", kind: TabFixes, agentic: true, tab: &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Fixes{Fixes: &frontendv1.FeedMergeTabFixes{}}}},
		{name: "pre-prompt", kind: TabPrePrompt, agentic: true, tab: &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_PrePrompt{PrePrompt: &frontendv1.FeedMergeTabPrePrompt{}}}},
		{name: "post-prompt", kind: TabPostPrompt, agentic: true, tab: &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_PostPrompt{PostPrompt: &frontendv1.FeedMergeTabPostPrompt{}}}},
		{name: "rebasing", kind: TabRebasing, agentic: false, tab: &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Rebasing{Rebasing: &frontendv1.FeedMergeTabRebasing{}}}},
		{name: "tests", kind: TabTests, agentic: false, tab: &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Tests{Tests: &frontendv1.FeedMergeTabTests{}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.registerMergeWorkspace(theWorkspace, "request-1", "feature")

			// Act
			h.o.publishTab(theMergeWorkspace, "lease-1", tt.kind, 1, tt.tab)

			// Assert: the merge workspace's tab is as composed, and the
			// requester's carries the switch exactly when the tab is agentic.
			own, mirrored := h.feed.tabsOn(theMergeWorkspace), h.feed.tabsOn(theWorkspace)
			if len(own) != 1 || len(mirrored) != 1 {
				t.Fatalf("tabs: merge workspace %d, requester %d; want one in each", len(own), len(mirrored))
			}
			if tabSwitchOf(own[0]) != nil {
				t.Fatal("the merge workspace's own tab carries a switch")
			}
			got := tabSwitchOf(mirrored[0])
			if (got != nil) != tt.agentic {
				t.Fatalf("requester's tab switch = %+v, want present = %v", got, tt.agentic)
			}
			if tt.agentic && got.GetAvailable().GetTarget().GetId() != string(theMergeWorkspace) {
				t.Fatalf("switch target = %+v, want the merge workspace", got.GetAvailable().GetTarget())
			}
		})
	}
}

// tabSwitchOf answers an agentic tab's switch, nil for any other.
func tabSwitchOf(tab *frontendv1.FeedMergeTab) *frontendv1.FeedMergeTabWorkspaceSwitch {
	switch arm := tab.GetKind().(type) {
	case *frontendv1.FeedMergeTab_PrePrompt:
		return arm.PrePrompt.GetWorkspaceSwitch()
	case *frontendv1.FeedMergeTab_Conflicts:
		return arm.Conflicts.GetWorkspaceSwitch()
	case *frontendv1.FeedMergeTab_Fixes:
		return arm.Fixes.GetWorkspaceSwitch()
	case *frontendv1.FeedMergeTab_PostPrompt:
		return arm.PostPrompt.GetWorkspaceSwitch()
	}
	return nil
}

func TestAnInPlaceMergesBubbleIsDrawnInItsOwnFeedAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.o.publishHead(theWorkspace, "lease-1", "feature → master", 1, nil)

	// Assert
	h.feed.mu.Lock()
	defer h.feed.mu.Unlock()
	if len(h.feed.rows) != 1 || h.feed.rows[0].WS != theWorkspace || h.feed.rows[0].Row.GetActivity().GetMerge().GetHead().GetWorkspaceSwitch() != nil {
		t.Fatalf("rows = %+v, want one head on the workspace's own feed with no switch", h.feed.rows)
	}
}

func TestHandBackTellsTheRequesterOfADetachedMerge(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "feature")

	// Act
	h.o.handBack(context.Background(), theMergeWorkspace, DetachedOutcome{Kind: DetachedFailed, Summary: "tests failed"})

	// Assert: told to the requester, with the branch the merge was asked for.
	sent := h.detached.sent()
	if len(sent) != 1 || sent[0].requester != theWorkspace || sent[0].mergeWS != theMergeWorkspace || sent[0].outcome.Branch != "feature" || sent[0].outcome.Kind != DetachedFailed {
		t.Fatalf("handbacks = %+v, want the failure told to the requester", sent)
	}
}

func TestHandBackSendsNothingForAnInPlaceMerge(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.o.handBack(context.Background(), theWorkspace, DetachedOutcome{Kind: DetachedLanded, Summary: "it landed"})

	// Assert
	if sent := h.detached.sent(); len(sent) != 0 {
		t.Fatalf("handbacks = %+v, want none for an in-place merge", sent)
	}
}

func TestHandBackOfAMergeWorkspaceWithNoCreationJobIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "feature")
	h.db.mu.Lock()
	delete(h.db.jobs, theMergeWorkspace)
	h.db.mu.Unlock()

	// Act
	h.o.handBack(context.Background(), theMergeWorkspace, DetachedOutcome{Kind: DetachedLanded, Summary: "it landed"})

	// Assert
	if sent := h.detached.sent(); len(sent) != 0 {
		t.Fatalf("handbacks = %+v, want none without the branch it was asked for", sent)
	}
	if _, ok := h.recordFor(dlog.LevelError, "daemon.merge.detached"); !ok {
		t.Fatal("the missing creation job was not recorded at ERROR")
	}
}

func TestALandedDetachedMergeIsHandedBackAsLanded(t *testing.T) {
	// Arrange: a detached request handed off and in line.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert
	sent := h.detached.sent()
	if len(sent) != 1 || sent[0].outcome.Kind != DetachedLanded || sent[0].requester != theWorkspace || sent[0].outcome.Branch != "feature" {
		t.Fatalf("handbacks = %+v, want the landing told to the requester", sent)
	}
}


func TestAnEvictedDetachedMergeIsHandedBackAsAbandoned(t *testing.T) {
	// Arrange: a detached request handed off and waiting in line.
	h := newHarness(t)
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act
	if err := h.o.Evict(context.Background(), theMergeWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert
	sent := h.detached.sent()
	if len(sent) != 1 || sent[0].outcome.Kind != DetachedAbandoned || sent[0].requester != theWorkspace {
		t.Fatalf("handbacks = %+v, want the abandonment told to the requester", sent)
	}
}

func TestEveryMergeTabIsPublishedThroughPublishTab(t *testing.T) {
	// Arrange: the package's own non-test sources.
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("Glob: %v", err)
	}

	// Act / Assert: tabRow is composed only inside publishTab, so no tab can
	// reach one placement and miss the other.
	for _, file := range files {
		if strings.HasSuffix(file, "_test.go") || file == "tabs.go" {
			continue
		}
		source, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("ReadFile %s: %v", file, err)
		}
		if strings.Contains(string(source), "tabRow(") {
			t.Errorf("%s composes a tab row itself, want every tab published through publishTab", file)
		}
	}
}
