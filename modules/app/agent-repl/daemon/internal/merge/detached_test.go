package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

// theMergeWorkspace is the merge workspace the harness's fake forks for a
// detached request.
const theMergeWorkspace = ids.WorkspaceID("ws-merge")

// fakeDetached stands for the detached-merge service: its fork registers the
// harness's merge workspace in the fake registry, and every handback is kept.
type fakeDetached struct {
	h *harness

	mu      sync.Mutex
	forkErr error
	forks   []string
	// origins records the origin each fork was handed, in order.
	origins   []wsm.MergeOrigin
	handbacks []handback
	// revealErr fails every reveal; reveals records each merge workspace a
	// reveal named.
	revealErr error
	reveals   []ids.WorkspaceID
	// claimsAtFork is how many name claims still stood, unreleased, when
	// each fork ran.
	claimsAtFork []int
}

// handback is one HandBack the fake was handed.
type handback struct {
	requester ids.WorkspaceID
	mergeWS   ids.WorkspaceID
	outcome   DetachedOutcome
}

func (d *fakeDetached) Fork(_ context.Context, requester wsm.Workspace, origin wsm.MergeOrigin, name, base string) (wsm.Workspace, error) {
	d.h.names.mu.Lock()
	standing := len(d.h.names.claims) - d.h.names.releases
	d.h.names.mu.Unlock()
	d.mu.Lock()
	d.claimsAtFork = append(d.claimsAtFork, standing)
	d.forks = append(d.forks, fmt.Sprintf("%s:%s:%s", requester.ID, name, base))
	d.origins = append(d.origins, origin)
	err := d.forkErr
	d.mu.Unlock()
	if err != nil {
		return wsm.Workspace{}, err
	}
	return d.h.registerMergeWorkspaceFor(origin, base), nil
}

func (d *fakeDetached) Reveal(_ context.Context, mergeWS ids.WorkspaceID) (bool, error) {
	d.mu.Lock()
	err := d.revealErr
	if err == nil {
		d.reveals = append(d.reveals, mergeWS)
	}
	d.mu.Unlock()
	if err != nil {
		return false, err
	}
	d.h.db.mu.Lock()
	defer d.h.db.mu.Unlock()
	ws := d.h.db.workspaces[mergeWS]
	if ws.MergeRevealed {
		return false, nil
	}
	ws.MergeRevealed = true
	d.h.db.workspaces[mergeWS] = ws
	return true, nil
}

// revealed answers every merge workspace a reveal named, in order.
func (d *fakeDetached) revealed() []ids.WorkspaceID {
	d.mu.Lock()
	defer d.mu.Unlock()
	return append([]ids.WorkspaceID(nil), d.reveals...)
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
	return h.registerMergeWorkspaceFor(wsm.MergeOrigin{Requester: requester, Request: request}, branch)
}

// registerMergeWorkspaceFor registers the merge workspace forked for origin,
// cut from branch, in the harness's one repository.
func (h *harness) registerMergeWorkspaceFor(origin wsm.MergeOrigin, branch string) wsm.Workspace {
	dir := h.t.TempDir()
	requester := origin.Requester
	// THE REAL FORK RECORDS THE COMMIT IT CUT the merge workspace at.
	if origin.ForkHead == "" {
		origin.ForkHead = "f0rkhead"
	}
	ws := wsm.Workspace{ID: theMergeWorkspace, Name: "feature-merge", Dir: dir, Branch: "feature-merge", Repo: theRepository,
		Parent: &requester, MergeOf: &origin}
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
func (f *fakeDB) HandOffDetachedMerge(_ context.Context, repo wsm.RepoKey, requester ids.WorkspaceID, request wsm.DetachedMergeID, mergeWS ids.WorkspaceID, branch, pick string, at time.Time) error {
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
			Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, Branch: branch, Pick: pick}, EnqueuedAt: at,
		})
		f.renumber(repo)
		return nil
	}
	return fmt.Errorf("no queue entry for %s: %w", requester, wsm.ErrNotFound)
}

// requestDetached asks for the harness workspace's own branch to be merged
// async, as BY asks, under the name "reconnect-fix".
func requestDetached(t *testing.T, h *harness, by Requester) error {
	t.Helper()
	return h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: asyncSource, By: by, Detached: &DetachedRun{Maker: theWorkspace, Name: "reconnect-fix"}})
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

func TestAnAsyncRequestMintsItsOwnIdentityAndCarriesItsName(t *testing.T) {
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
	if !ok || entry.Source.Detach == "" || entry.Source.DetachName != "reconnect-fix" || entry.State != wsm.MergeRequested {
		t.Fatalf("entry = %+v, %v; want a requested async request with its identity and its name", entry, ok)
	}
}

func TestAnAsyncRequestThatContradictsItselfIsAnErrorWithNothingRecorded(t *testing.T) {
	tests := []struct {
		name   string
		source wsm.MergeSource
		run    *DetachedRun
	}{
		{name: "kept open", source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, KeepOpen: true}, run: &DetachedRun{Maker: theWorkspace, Name: "n"}},
		{name: "merged upstream", source: wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}, run: &DetachedRun{Maker: theWorkspace, Name: "n"}},
		{name: "no name", source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}, run: &DetachedRun{Maker: theWorkspace, }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: tt.source, By: RequestedByUser, Detached: tt.run})

			// Assert
			if err == nil {
				t.Fatal("Enqueue = nil, want the contradiction refused")
			}
			if _, ok := h.entryOf(theWorkspace); ok {
				t.Fatal("a contradictory async request was recorded")
			}
			if _, ok := h.recordFor(dlog.LevelError, "daemon.merge.enqueue"); !ok {
				t.Fatal("the contradiction was not recorded at ERROR")
			}
		})
	}
}

func TestAnAsyncRequestWhoseNameIsTakenIsRefusedWithTheBranch(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.names.taken = map[string]bool{"reconnect-fix": true}

	// Act
	err := requestDetached(t, h, RequestedByAgent)

	// Assert
	refusal, ok := Refused(err)
	if !ok || refusal.Arm != ArmNameTaken || refusal.Fields["branch"] != "agent/reconnect-fix" {
		t.Fatalf("Enqueue = %v, want the name_taken refusal naming the branch", err)
	}
	if _, ok := h.entryOf(theWorkspace); ok {
		t.Fatal("a request whose name is taken was recorded")
	}
	if _, ok := h.recordFor(dlog.LevelWarn, "daemon.merge.enqueue"); !ok {
		t.Fatal("the refusal was not recorded")
	}
}

func TestAnAsyncRequestsNameCheckThatFailsIsAnErrorWithNothingRecorded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.names.err = errors.New("the registry could not be read")

	// Act
	err := requestDetached(t, h, RequestedByAgent)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the registry could not be read") {
		t.Fatalf("Enqueue = %v, want the check's failure", err)
	}
	if _, ok := h.entryOf(theWorkspace); ok {
		t.Fatal("a request whose name could not be checked was recorded")
	}
	if _, ok := h.recordFor(dlog.LevelError, "daemon.merge.enqueue"); !ok {
		t.Fatal("the failed check was not recorded at ERROR")
	}
}

func TestAnAsyncRequestLetsGoOfItsNameClaimOnceRecorded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.mu.Lock()
	h.inFlight = "turn-asking"
	h.mu.Unlock()

	// Act
	if err := requestDetached(t, h, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert: the record is the reservation from here, so the claim is gone.
	h.names.mu.Lock()
	defer h.names.mu.Unlock()
	if len(h.names.claims) != 1 || h.names.releases != 1 {
		t.Fatalf("claims = %v, releases = %d; want one claim, released", h.names.claims, h.names.releases)
	}
}

func TestAnAsyncRequestLetsGoOfItsNameBeforeItsMergeWorkspaceIsForked(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act: the user's ask forks at once, inside the request.
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert: the fork takes the reserved name, so no claim may stand.
	if got := h.detached.claimsAtFork; len(got) != 1 || got[0] != 0 {
		t.Fatalf("claims standing at the fork = %v, want none", got)
	}
}

func TestAUsersAsyncRequestDisplacesNothing(t *testing.T) {
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
		t.Fatal("an async merge was marked to displace a turn")
	}
}

func TestAnAsyncRequestIsHandedToTheMergeWorkspaceForkedUnderItsName(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act: the user's ask is put in line at once, which hands it off.
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert: the fork was named as asked and cut from the request's branch,
	// the requester holds no entry, and the merge workspace's own in-place
	// merge is in line.
	if forks := h.detached.forks; len(forks) != 1 || forks[0] != string(theWorkspace)+":reconnect-fix:"+asyncBranch {
		t.Fatalf("forks = %v, want one named reconnect-fix of the request's branch", forks)
	}
	if _, ok := h.entryOf(theWorkspace); ok {
		t.Fatal("the requester still holds a queue entry after the hand-off")
	}
	entry, ok := h.entryOf(theMergeWorkspace)
	if !ok || entry.State != wsm.MergeQueued || entry.Source.Detach != "" || entry.Source.KeepOpen {
		t.Fatalf("merge workspace entry = %+v, %v; want its own in-place merge in line", entry, ok)
	}
}

func TestAnAsyncRequestThatCompactsAfterRecordsTheAskOnItsEntry(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.mu.Lock()
	h.inFlight = "turn-asking"
	h.mu.Unlock()

	// Act: an agent's request, still waiting on the turn that asked.
	err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: asyncSource,
		By: RequestedByAgent, Detached: &DetachedRun{Maker: theWorkspace, Name: "reconnect-fix", CompactAfter: true}})

	// Assert
	if err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	entry, ok := h.entryOf(theWorkspace)
	if !ok || !entry.Source.CompactAfter || entry.Source.Detach == "" {
		t.Fatalf("entry = %+v, %v; want a detached request that compacts after it lands", entry, ok)
	}
}

func TestAnAsyncRequestsHandOffForksWithItsCompactAfterAsk(t *testing.T) {
	tests := []struct {
		name         string
		compactAfter bool
	}{
		{name: "asked", compactAfter: true},
		{name: "not asked", compactAfter: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act: the user's ask is put in line at once, which hands it off.
			err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: asyncSource,
				By: RequestedByUser, Detached: &DetachedRun{Maker: theWorkspace, Name: "reconnect-fix", CompactAfter: tt.compactAfter}})

			// Assert: the fork's origin names the requester, the request, and
			// the ask, which the merge workspace's handback reads.
			if err != nil {
				t.Fatalf("Enqueue: %v", err)
			}
			origins := h.detached.origins
			if len(origins) != 1 || origins[0].Requester != theWorkspace || origins[0].Request == "" || origins[0].CompactAfter != tt.compactAfter {
				t.Fatalf("origins = %+v, want one for the requester's request with compact_after %v", origins, tt.compactAfter)
			}
		})
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
		!strings.Contains(sent[0].outcome.Summary, "the worktree could not be made") || sent[0].outcome.Branch != "reconnect-fix" {
		t.Fatalf("handbacks = %+v, want one abandoned handback naming the fork's failure", sent)
	}
	if _, ok := h.recordFor(dlog.LevelError, "daemon.merge.detached"); !ok {
		t.Fatal("the failed fork was not recorded at ERROR")
	}
}

func TestAnAsyncMergesBubbleIsDrawnInTheRequestersFeedAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert
	if own := h.feed.headsOn(theMergeWorkspace); len(own) != 0 {
		t.Fatalf("the merge workspace's feed drew %d heads, want none", len(own))
	}
	if tabs := h.feed.tabsOn(theMergeWorkspace); len(tabs) != 0 {
		t.Fatalf("the merge workspace's feed drew %d tabs, want none", len(tabs))
	}
	if mirrored := h.feed.headsOn(theWorkspace); len(mirrored) == 0 {
		t.Fatal("the requester's feed drew no head")
	}
}

func TestAnAsyncMergesHeadIsNeverPinned(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "feature")

	// Act: a live head, the one a sync merge would pin.
	h.o.publishHead(theMergeWorkspace, "lease-1", "reconnect-fix → master", 1, headMerging, headRun)

	// Assert
	h.feed.mu.Lock()
	defer h.feed.mu.Unlock()
	if len(h.feed.rows) != 1 || h.feed.rows[0].WS != theWorkspace || h.feed.rows[0].Pinned {
		t.Fatalf("rows = %+v, want one placed head in the requester's feed", h.feed.rows)
	}
}

func TestASyncMergesLiveHeadIsPinnedInItsOwnFeed(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.o.publishHead(theWorkspace, "lease-1", "feature → master", 1, headMerging, headRun)

	// Assert
	h.feed.mu.Lock()
	defer h.feed.mu.Unlock()
	if len(h.feed.rows) != 1 || h.feed.rows[0].WS != theWorkspace || !h.feed.rows[0].Pinned ||
		h.feed.rows[0].Row.GetActivity().GetMerge().GetHead().GetMode().GetSync() == nil {
		t.Fatalf("rows = %+v, want one pinned sync head on the workspace's own feed", h.feed.rows)
	}
}

func TestAnAsyncHeadCarriesNoSwitchWhileItsWorkspaceIsHidden(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "feature")

	// Act
	h.o.publishHead(theMergeWorkspace, "lease-1", "reconnect-fix → master", 1, headMerging, headRun)

	// Assert
	heads := h.feed.headsOn(theWorkspace)
	if len(heads) != 1 || heads[0].GetHead().GetMode().GetAsync() == nil ||
		heads[0].GetHead().GetMode().GetAsync().GetWorkspaceSwitch() != nil {
		t.Fatalf("heads = %+v, want one async head with no switch to a hidden workspace", heads)
	}
}

func TestARevealRestatesTheAsyncHeadWithTheSwitch(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "feature")
	h.o.publishHead(theMergeWorkspace, "lease-1", "reconnect-fix → master", 1, headMerging, headRun)

	// Act
	if err := h.o.RevealMergeWorkspace(context.Background(), theMergeWorkspace, "the user selected it"); err != nil {
		t.Fatalf("RevealMergeWorkspace: %v", err)
	}

	// Assert
	heads := h.feed.headsOn(theWorkspace)
	target := heads[len(heads)-1].GetHead().GetMode().GetAsync().GetWorkspaceSwitch().GetAvailable().GetTarget()
	if len(heads) != 2 || target.GetId() != string(theMergeWorkspace) || target.GetDir() == "" {
		t.Fatalf("heads = %+v, want the head restated with the switch to the merge workspace", heads)
	}
}

func TestEveryHeadAnAsyncMergePublishesIsAsync(t *testing.T) {
	tests := []struct {
		name   string
		result any
	}{
		{"queued or running", headMerging},
		{"landed", &frontendv1.FeedMergeSuccess{}},
		{"failed", &frontendv1.FeedMergeError{Reason: &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{}}}},
		{"did not run", &frontendv1.FeedMergeError{Reason: &frontendv1.FeedMergeError_Abandoned{Abandoned: &frontendv1.FeedMergeAbandoned{}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.registerMergeWorkspace(theWorkspace, "request-1", "feature")

			// Act
			h.o.publishHead(theMergeWorkspace, "lease-1", "reconnect-fix → master", 1, tt.result, headRun)

			// Assert
			heads := h.feed.headsOn(theWorkspace)
			if len(heads) != 1 || heads[0].GetHead().GetMode().GetAsync() == nil {
				t.Fatalf("heads = %+v, want one async head", heads)
			}
		})
	}
}

func TestEveryHeadASyncMergePublishesIsSync(t *testing.T) {
	tests := []struct {
		name   string
		result any
	}{
		{"queued or running", headMerging},
		{"landed", &frontendv1.FeedMergeSuccess{}},
		{"failed", &frontendv1.FeedMergeError{Reason: &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			h.o.publishHead(theWorkspace, "lease-1", "feature → master", 1, tt.result, headRun)

			// Assert
			heads := h.feed.headsOn(theWorkspace)
			if len(heads) != 1 || heads[0].GetHead().GetMode().GetSync() == nil {
				t.Fatalf("heads = %+v, want one sync head", heads)
			}
		})
	}
}

func TestRevealMergeWorkspaceIsANoOpForAnOrdinaryWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	err := h.o.RevealMergeWorkspace(context.Background(), theWorkspace, "the user selected it")

	// Assert
	if err != nil || len(h.detached.revealed()) != 0 {
		t.Fatalf("RevealMergeWorkspace = %v, reveals = %v; want nothing done", err, h.detached.revealed())
	}
}

func TestARevealThatFailsIsAnErrorAndTheWorkspaceStaysHidden(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "feature")
	h.detached.revealErr = errors.New("the registry could not be written")

	// Act
	err := h.o.RevealMergeWorkspace(context.Background(), theMergeWorkspace, "the user selected it")

	// Assert
	if err != nil {
		t.Fatalf("RevealMergeWorkspace = %v, want the failure recorded, not raised", err)
	}
	if _, ok := h.recordFor(dlog.LevelError, "daemon.merge.reveal"); !ok {
		t.Fatal("the failed reveal was not recorded at ERROR")
	}
	if h.db.workspaces[theMergeWorkspace].MergeRevealed {
		t.Fatal("the merge workspace was revealed despite the failure")
	}
}

func TestATabOfAnAsyncMergeIsPublishedInTheRequestersFeedAsComposed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "feature")
	tab := &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_RebaseCommit{RebaseCommit: &frontendv1.FeedMergeTabRebaseCommit{
		Conflict: &frontendv1.FeedMergeRebaseCommitConflict{}}}}

	// Act
	h.o.publishTab(theMergeWorkspace, "lease-1", commitKey(1, 0), tab)

	// Assert
	if own := h.feed.tabsOn(theMergeWorkspace); len(own) != 0 {
		t.Fatalf("the merge workspace drew %d tabs, want none", len(own))
	}
	if mirrored := h.feed.tabsOn(theWorkspace); len(mirrored) != 1 || mirrored[0].GetRebaseCommit().GetConflict() == nil {
		t.Fatalf("requester tabs = %+v, want the conflicted commit's subtab", mirrored)
	}
}

func TestAMergeWorkspacesFactsNeverReachItsFooter(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "feature")

	// Act
	h.o.publish(h.repoKey(), theMergeWorkspace, MergeFacts{State: StateMerged})
	h.o.forget(theMergeWorkspace)

	// Assert
	if h.footer.toldTo(theMergeWorkspace) {
		t.Fatal("a merge workspace's footer was told its merge's facts")
	}
}

func TestASyncMergesFactsReachItsFooter(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.o.publish(h.repoKey(), theWorkspace, MergeFacts{State: StateMerged})

	// Assert
	if !h.footer.toldTo(theWorkspace) {
		t.Fatal("a sync merge's footer was not told its facts")
	}
}

func TestHandBackTellsTheRequesterUnderTheMergeWorkspacesName(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "feature")

	// Act
	h.o.handBack(context.Background(), theMergeWorkspace, DetachedOutcome{Kind: DetachedLanded, Summary: "it landed"})

	// Assert
	sent := h.detached.sent()
	if len(sent) != 1 || sent[0].requester != theWorkspace || sent[0].mergeWS != theMergeWorkspace || sent[0].outcome.Branch != "feature-merge" {
		t.Fatalf("handbacks = %+v, want the landing told under the merge workspace's name", sent)
	}
}

func TestALandedHandBackLeavesTheMergeWorkspaceHidden(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "feature")

	// Act
	h.o.handBack(context.Background(), theMergeWorkspace, DetachedOutcome{Kind: DetachedLanded, Summary: "it landed"})

	// Assert
	if revealed := h.detached.revealed(); len(revealed) != 0 {
		t.Fatalf("reveals = %v, want none for a landing", revealed)
	}
}

func TestAFailedOrAbandonedHandBackRevealsTheMergeWorkspace(t *testing.T) {
	tests := []struct {
		name string
		kind DetachedOutcomeKind
	}{
		{name: "failed", kind: DetachedFailed},
		{name: "abandoned", kind: DetachedAbandoned},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.registerMergeWorkspace(theWorkspace, "request-1", "feature")

			// Act
			h.o.handBack(context.Background(), theMergeWorkspace, DetachedOutcome{Kind: tt.kind, Summary: "it stopped"})

			// Assert
			if revealed := h.detached.revealed(); len(revealed) != 1 || revealed[0] != theMergeWorkspace {
				t.Fatalf("reveals = %v, want the merge workspace revealed", revealed)
			}
		})
	}
}

func TestHandBackSendsNothingForASyncMerge(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.o.handBack(context.Background(), theWorkspace, DetachedOutcome{Kind: DetachedLanded, Summary: "it landed"})

	// Assert
	if sent := h.detached.sent(); len(sent) != 0 {
		t.Fatalf("handbacks = %+v, want none for a sync merge", sent)
	}
}

func TestHandBackOfAMergeWorkspaceWhoseRecordWillNotReadIsAnError(t *testing.T) {
	// Arrange: the origin is read, then the record goes.
	h := newHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "feature")
	h.o.originOf(theMergeWorkspace)
	h.db.mu.Lock()
	delete(h.db.workspaces, theMergeWorkspace)
	h.db.mu.Unlock()

	// Act
	h.o.handBack(context.Background(), theMergeWorkspace, DetachedOutcome{Kind: DetachedLanded, Summary: "it landed"})

	// Assert
	if sent := h.detached.sent(); len(sent) != 0 {
		t.Fatalf("handbacks = %+v, want none without the merge workspace's record", sent)
	}
	if _, ok := h.recordFor(dlog.LevelError, "daemon.merge.detached"); !ok {
		t.Fatal("the unreadable record was not recorded at ERROR")
	}
}

func TestALandedAsyncMergeIsHandedBackAsLanded(t *testing.T) {
	// Arrange: an async request handed off and in line.
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
	if len(sent) != 1 || sent[0].outcome.Kind != DetachedLanded || sent[0].requester != theWorkspace {
		t.Fatalf("handbacks = %+v, want the landing told to the requester", sent)
	}
}

// landedAsync lands an async merge whose target the harness has set up, and
// answers the one report it handed back.
func landedAsync(t *testing.T, h *harness) handback {
	t.Helper()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}
	sent := h.detached.sent()
	if len(sent) != 1 || sent[0].outcome.Kind != DetachedLanded {
		t.Fatalf("handbacks = %+v, want one landing", sent)
	}
	return sent[0]
}

func TestALandingInTheDaemonsOwnCheckoutIsHandedBackAsOwedItsDeploy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ownCheckout()

	// Act
	sent := landedAsync(t, h)

	// Assert
	if sent.outcome.Deploy != LandingDeployOwed {
		t.Fatalf("deploy = %v, want the report owed its deploy", sent.outcome.Deploy)
	}
}

func TestALandingsReportIsHandedOverBeforeItsDeployIsTold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ownCheckout()

	// Act
	landedAsync(t, h)

	// Assert: the report exists before the deploy, so the deploy covers it.
	h.rollout.mu.Lock()
	defer h.rollout.mu.Unlock()
	if h.rollout.fired != 1 || h.rollout.handbacksAtFire != 1 {
		t.Fatalf("deploy fired %d time(s) with %d report(s) handed over, want once, after the report", h.rollout.fired, h.rollout.handbacksAtFire)
	}
}

func TestALandingInASiblingWorktreeIsHandedBackWithNothingToDeploy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.emacsRepo()

	// Act
	sent := landedAsync(t, h)

	// Assert
	if sent.outcome.Deploy != LandingNotDeployed {
		t.Fatalf("deploy = %v, want nothing to deploy", sent.outcome.Deploy)
	}
}

func TestALandingWhileTheRolloutsAreHeldIsHandedBackAsHeld(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ownCheckout()
	h.db.rolloutHold = wsm.RolloutHold{Held: true, Since: time.Unix(1, 0)}

	// Act
	sent := landedAsync(t, h)

	// Assert
	if sent.outcome.Deploy != LandingRolloutsHeld {
		t.Fatalf("deploy = %v, want the held rollouts named", sent.outcome.Deploy)
	}
}

func TestALandingWhoseHoldWillNotReadIsHandedBackAsUnread(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ownCheckout()
	h.db.rolloutHoldErr = errors.New("state database unreadable")

	// Act
	sent := landedAsync(t, h)

	// Assert
	if sent.outcome.Deploy != LandingHoldUnread {
		t.Fatalf("deploy = %v, want the unread hold named", sent.outcome.Deploy)
	}
}

func TestTheReportAndTheDeployReadTheHoldOnce(t *testing.T) {
	// Arrange: the hold reads as free for the first read only.
	h := newHarness(t)
	h.ownCheckout()
	h.db.rolloutHoldOnce = &wsm.RolloutHold{Held: true, Since: time.Unix(1, 0)}

	// Act
	sent := landedAsync(t, h)

	// Assert: the report says owed, and the deploy it was owed was told.
	h.rollout.mu.Lock()
	fired := h.rollout.fired
	h.rollout.mu.Unlock()
	if sent.outcome.Deploy != LandingDeployOwed || fired != 1 {
		t.Fatalf("deploy = %v and the deploy fired %d time(s), want one decision both act on", sent.outcome.Deploy, fired)
	}
}

func TestAnEvictedAsyncMergeIsNotHandedBack(t *testing.T) {
	// Arrange: an async request handed off and waiting in line.
	h := newHarness(t)
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act
	if err := h.o.Evict(context.Background(), theMergeWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert: an end somebody asked for is no failure to hand back, and
	// nothing needs the user, so the merge workspace stays hidden.
	if sent := h.detached.sent(); len(sent) != 0 {
		t.Fatalf("handbacks = %+v, want none for an eviction", sent)
	}
	if revealed := h.detached.revealed(); len(revealed) != 0 {
		t.Fatalf("reveals = %v, want none for an eviction", revealed)
	}
	var r dlog.Record
	for _, rec := range h.logs.Records() {
		if rec.Level == dlog.LevelInfo && rec.Message == "an async merge was taken off at an explicit request; its requester is not handed it back" {
			r = rec
		}
	}
	if r.Context["requester"] != string(theWorkspace) || r.Context["outcome"] != "abandoned" {
		t.Fatalf("no INFO record naming the requester the eviction was not handed back to: %+v", h.logs.Records())
	}
}

func TestAnAsyncMergeWhoseWorkspaceClosedIsHandedBackAsAbandoned(t *testing.T) {
	// Arrange: an async request handed off and waiting in line.
	h := newHarness(t)
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act: nobody asked for the merge to go; its workspace went under it.
	h.o.OnWorkspaceClosed(context.Background(), theMergeWorkspace)

	// Assert
	sent := h.detached.sent()
	if len(sent) != 1 || sent[0].outcome.Kind != DetachedAbandoned || sent[0].requester != theWorkspace || sent[0].outcome.Requested {
		t.Fatalf("handbacks = %+v, want the abandonment told to the requester", sent)
	}
}

func TestHandBackOfARequestedEndSendsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "feature")

	// Act
	h.o.handBack(context.Background(), theMergeWorkspace, DetachedOutcome{Kind: DetachedAbandoned, Summary: "evicted", Requested: true})

	// Assert
	if sent := h.detached.sent(); len(sent) != 0 {
		t.Fatalf("handbacks = %+v, want none", sent)
	}
}

func TestOnlyAnExplicitRemovalIsARequestedEnd(t *testing.T) {
	cases := []struct {
		cause AbandonCause
		want  bool
	}{
		{CauseUserDrop, true},
		{CauseUserDequeue, true},
		{CauseWorkspaceClosed, false},
		{CauseDaemonShutdown, false},
	}
	for _, tc := range cases {
		t.Run(string(tc.cause), func(t *testing.T) {
			// Act
			got := tc.cause.releases()

			// Assert
			if got != tc.want {
				t.Fatalf("%s.releases() = %v, want %v", tc.cause, got, tc.want)
			}
		})
	}
}

// TestEveryAbandonedHandbackSaysWhetherItWasRequested fails an abandoned
// handback that does not carry its cause's releases(), which would hand an
// explicit removal back to the requester as a failure.
func TestEveryAbandonedHandbackSaysWhetherItWasRequested(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("terminal.go")
	if err != nil {
		t.Fatalf("read: %v", err)
	}

	// Act
	abandoned := strings.Count(string(src), "DetachedOutcome{Kind: DetachedAbandoned")
	marked := strings.Count(string(src), "Requested: cause.releases()")

	// Assert
	if abandoned == 0 || abandoned != marked {
		t.Fatalf("terminal.go hands back %d abandonments and marks %d of them requested-or-not", abandoned, marked)
	}
}

func TestAnAskOpenedInAHiddenMergeWorkspaceRevealsIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "feature")

	// Act
	h.asks.OnPermission(theMergeWorkspace, nil, permissionEdge("perm-1", true))

	// Assert
	if revealed := h.detached.revealed(); len(revealed) != 1 || revealed[0] != theMergeWorkspace {
		t.Fatalf("reveals = %v, want the merge workspace revealed", revealed)
	}
}

func TestAnAskOpenedInAnOrdinaryWorkspaceRevealsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.asks.OnPermission(theWorkspace, nil, permissionEdge("perm-1", true))

	// Assert
	if revealed := h.detached.revealed(); len(revealed) != 0 {
		t.Fatalf("reveals = %v, want none", revealed)
	}
}

func TestEveryMergeTabIsPublishedThroughPublishTab(t *testing.T) {
	// Arrange / Act: the package's own production sources.
	files := sourcescan.Production(t)

	// Assert: tabRow is composed only inside publishTab, so no tab reaches a
	// feed other than its bubble's.
	for _, file := range files {
		if file.Name == "tabs.go" {
			continue
		}
		if strings.Contains(string(file.Source), "tabRow(") {
			t.Errorf("%s composes a tab row itself, want every tab published through publishTab", file.Name)
		}
	}
}

// asMainCheckout makes the requester the repository's main checkout: no
// creation job, its directory its own main worktree, its parent branch master.
func asMainCheckout(h *harness) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	delete(h.db.jobs, theWorkspace)
	requester := h.db.workspaces[theWorkspace]
	requester.ParentBranch = "master"
	h.db.workspaces[theWorkspace] = requester
}

// withParentBranch gives the created requester a recorded parent branch.
func withParentBranch(h *harness, parent string) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	requester := h.db.workspaces[theWorkspace]
	requester.ParentBranch = parent
	h.db.workspaces[theWorkspace] = requester
}

// detachedRunFor is the "reconnect-fix" request with ACTIONS, landing into the
// harness's other workspace as its maker when INTOMAKER, else made by the
// requester and landing on the primary branch.
func detachedRunFor(intoMaker bool, actions wsm.MergeActions) *DetachedRun {
	if intoMaker {
		return &DetachedRun{Maker: otherWorkspace, IntoMaker: true, Name: "reconnect-fix", Actions: actions}
	}
	return &DetachedRun{Maker: theWorkspace, Name: "reconnect-fix", Actions: actions}
}

func TestAnAsyncRequestsHandOffForksWithTheLandingTheQueueDecided(t *testing.T) {
	actions := wsm.MergeActions{Before: []string{"run the linters"}, After: []string{"post the summary"}}
	tests := []struct {
		name          string
		arrange       func(h *harness)
		wantTarget    func(h *harness) string
		wantParent    string
		actions       wsm.MergeActions
		wantOriginAct wsm.MergeActions
		intoMaker     bool
	}{
		{name: "a request onto the primary branch lands in the main worktree", arrange: func(h *harness) {},
			wantTarget: func(h *harness) string { return h.sourceD }, wantParent: "master", actions: actions, wantOriginAct: actions},
		{name: "a request into its maker lands in the maker's worktree on its branch", intoMaker: true, arrange: func(h *harness) {
			h.registerOther(otherWorkspace, "ws-two", "maker-branch")
		}, wantTarget: func(h *harness) string { return h.db.workspaces[otherWorkspace].Dir }, wantParent: "maker-branch", actions: actions, wantOriginAct: actions},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(h)

			// Act: the user's ask is put in line at once, which hands it off.
			err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: asyncSource,
				By: RequestedByUser, Detached: detachedRunFor(tt.intoMaker, tt.actions)})

			// Assert
			if err != nil {
				t.Fatalf("Enqueue: %v", err)
			}
			origins := h.detached.origins
			if len(origins) != 1 {
				t.Fatalf("origins = %+v, want one", origins)
			}
			got := origins[0]
			if got.TargetDir != tt.wantTarget(h) || got.ParentBranch != tt.wantParent || !reflect.DeepEqual(got.Actions, tt.wantOriginAct) {
				t.Fatalf("origin landing = (%q, %q, %+v), want (%q, %q, %+v)", got.TargetDir, got.ParentBranch, got.Actions,
					tt.wantTarget(h), tt.wantParent, tt.wantOriginAct)
			}
		})
	}
}

func TestAnAsyncRequestWithNoActionsForksWithNoneEvenWhenItsRequesterHasSome(t *testing.T) {
	// Arrange: the requester was created with merge actions of its own.
	h := newHarness(t)
	h.db.mu.Lock()
	job := h.db.jobs[theWorkspace]
	job.Actions = wsm.MergeActions{Before: []string{"the requester's own before"}, After: []string{"the requester's own after"}}
	h.db.jobs[theWorkspace] = job
	h.db.mu.Unlock()

	// Act
	err := requestDetached(t, h, RequestedByUser)

	// Assert
	if err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if origins := h.detached.origins; len(origins) != 1 || !reflect.DeepEqual(origins[0].Actions, wsm.MergeActions{}) {
		t.Fatalf("origins = %+v, want one carrying no merge action", origins)
	}
}

func TestAnAsyncRequestRecordsItsMergeActionsOnItsEntry(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.mu.Lock()
	h.inFlight = "turn-asking"
	h.mu.Unlock()
	actions := wsm.MergeActions{Before: []string{"run the linters"}}

	// Act: an agent's request, still waiting on the turn that asked.
	err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: asyncSource,
		By: RequestedByAgent, Detached: &DetachedRun{Maker: theWorkspace, Name: "reconnect-fix", Actions: actions}})

	// Assert
	if err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if entry, ok := h.entryOf(theWorkspace); !ok || !reflect.DeepEqual(entry.Source.Actions, actions) {
		t.Fatalf("entry = %+v, %v; want the request's actions recorded", entry, ok)
	}
}

func TestAHandOffWhoseLandingCannotBeDecidedDropsTheRequestAndTellsTheRequester(t *testing.T) {
	// Arrange: an agent's request recorded while the requester was created,
	// whose creation job is gone by its hand-off although it is no main
	// checkout.
	h := newHarness(t)
	h.mu.Lock()
	h.inFlight = "turn-asking"
	h.mu.Unlock()
	if err := requestDetached(t, h, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	entry, ok := h.entryOf(theWorkspace)
	if !ok {
		t.Fatal("the request was not recorded")
	}
	entry.Source.DetachMaker, entry.Source.DetachTargetMaker = "ws-gone", true

	// Act
	err := h.o.handOffDetached(context.Background(), theWorkspace, h.repoKey(), entry.Source)

	// Assert
	if err != nil {
		t.Fatalf("handOffDetached = %v, want the request answered and the failure handed back", err)
	}
	if len(h.detached.forks) != 0 {
		t.Fatalf("forks = %v, want none", h.detached.forks)
	}
	if _, ok := h.entryOf(theWorkspace); ok {
		t.Fatal("a request whose landing could not be decided still stands in the queue")
	}
	if sent := h.detached.sent(); len(sent) != 1 || sent[0].outcome.Kind != DetachedAbandoned {
		t.Fatalf("handbacks = %+v, want one abandoned handback", sent)
	}
}

