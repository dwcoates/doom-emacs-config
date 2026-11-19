package merge

import (
	"errors"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// prURL is the url ownedPR stores for pull request N.
func prURL(n uint32) string {
	return wsm.PullRequestKey{Owner: "o", Repo: "r", Number: n}.String()
}

// headTargetPR answers the pull request the last head drawn on WS's root feed
// opens from its target.
func headTargetPR(t *testing.T, h *harness, ws ids.WorkspaceID) string {
	t.Helper()
	heads := h.feed.headsOn(ws)
	if len(heads) == 0 {
		t.Fatalf("no head drawn on %s", ws)
	}
	return heads[len(heads)-1].GetHead().GetBranchLine().GetTarget().GetPullRequest().GetUrl()
}

func TestASyncHeadsTargetOpensTheWorkspacesPullRequest(t *testing.T) {
	tests := []struct {
		name  string
		links []wsm.WorkspaceLink
		want  string
	}{
		{name: "no links open nothing"},
		{name: "a reference opens nothing", links: []wsm.WorkspaceLink{{URL: prURL(1), Label: "a pull request", PullRequest: &wsm.PullRequestKey{Owner: "o", Repo: "r", Number: 1}}}},
		{name: "an owned link that is no pull request opens nothing", links: []wsm.WorkspaceLink{{URL: "https://example.com", Label: "a page", Owned: true}}},
		{name: "one open owned pull request", links: []wsm.WorkspaceLink{ownedPR(1, nil)}, want: prURL(1)},
		{name: "one merged owned pull request", links: []wsm.WorkspaceLink{ownedPR(1, &wsm.PullRequestEnd{Merged: true})}, want: prURL(1)},
		{name: "one closed unmerged opens nothing", links: []wsm.WorkspaceLink{ownedPR(1, &wsm.PullRequestEnd{})}},
		{name: "a closed one beside an open one opens the open one", links: []wsm.WorkspaceLink{ownedPR(1, &wsm.PullRequestEnd{}), ownedPR(2, nil)}, want: prURL(2)},
		{name: "several open ones open nothing", links: []wsm.WorkspaceLink{ownedPR(1, nil), ownedPR(2, nil)}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.db.mu.Lock()
			h.db.links[theWorkspace] = tt.links
			h.db.mu.Unlock()

			// Act
			h.o.publishHead(theWorkspace, "lease-1", branchLineOf("feature", "/repo"), 1, headMerging, headRun)

			// Assert
			if got := headTargetPR(t, h, theWorkspace); got != tt.want {
				t.Fatalf("the target opens %q, want %q", got, tt.want)
			}
		})
	}
}

func TestSeveralPullRequestsAreRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.links[theWorkspace] = []wsm.WorkspaceLink{ownedPR(1, nil), ownedPR(2, nil)}
	h.db.mu.Unlock()

	// Act
	h.o.publishHead(theWorkspace, "lease-1", branchLineOf("feature", "/repo"), 1, headMerging, headRun)

	// Assert
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opHeadPullRequest})
	if !ok || rec.Context["workspace"] != string(theWorkspace) {
		t.Fatalf("records = %+v, want one INFO record naming the workspace", h.logs.Records())
	}
}

func TestAnUnreadableLinkReadDrawsAPlainTargetAndRecordsError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.linksErr = errors.New("the store is gone")
	h.db.mu.Unlock()

	// Act
	h.o.publishHead(theWorkspace, "lease-1", branchLineOf("feature", "/repo"), 1, headMerging, headRun)

	// Assert
	if got := headTargetPR(t, h, theWorkspace); got != "" {
		t.Fatalf("the target opens %q, want none", got)
	}
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opHeadPullRequest})
	if !ok || rec.Context["owner"] != string(theWorkspace) || rec.Context["error"] != "the store is gone" {
		t.Fatalf("records = %+v, want one ERROR record naming the owner and the cause", h.logs.Records())
	}
}

func TestAnUnreadLineReadsNoLinks(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.links[theWorkspace] = []wsm.WorkspaceLink{ownedPR(1, nil)}
	h.db.mu.Unlock()

	// Act
	h.o.publishHead(theWorkspace, "lease-1", unreadLine(theWorkspace), 1, headMerging, headRun)

	// Assert
	if heads := h.feed.headsOn(theWorkspace); len(heads) != 1 || heads[0].GetHead().GetBranchLine().GetTarget() != nil {
		t.Fatalf("heads = %+v, want one head with no target", heads)
	}
}

func TestAnAsyncHeadsTargetOpensTheRequestersPullRequest(t *testing.T) {
	tests := []struct {
		name  string
		owner ids.WorkspaceID
	}{
		{name: "stored on the requester", owner: theWorkspace},
		{name: "stored on the merge workspace", owner: theMergeWorkspace},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.registerMergeWorkspace(theWorkspace, "request-1", "feature")
			h.db.mu.Lock()
			h.db.links[tt.owner] = []wsm.WorkspaceLink{ownedPR(1, nil)}
			h.db.mu.Unlock()

			// Act
			h.o.publishHead(theMergeWorkspace, "lease-1", branchLineOf("feature", "/repo"), 1, headMerging, headRun)

			// Assert
			if got := headTargetPR(t, h, theWorkspace); got != prURL(1) {
				t.Fatalf("the target opens %q, want %q", got, prURL(1))
			}
		})
	}
}

func TestAChangeOfLinksRedrawsTheHeadWithItsPullRequest(t *testing.T) {
	tests := []struct {
		name    string
		merging ids.WorkspaceID
		changed ids.WorkspaceID
		async   bool
		redrawn bool
	}{
		{name: "the sync merge's own workspace", merging: theWorkspace, changed: theWorkspace, redrawn: true},
		{name: "an async merge's requester", merging: theMergeWorkspace, changed: theWorkspace, async: true, redrawn: true},
		{name: "an async merge's merge workspace", merging: theMergeWorkspace, changed: theMergeWorkspace, async: true, redrawn: true},
		{name: "an unrelated workspace", merging: theWorkspace, changed: "elsewhere"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a head drawn before the finish hook stored the pull request.
			h := newHarness(t)
			if tt.async {
				h.registerMergeWorkspace(theWorkspace, "request-1", "feature")
			}
			h.o.publishHead(tt.merging, "lease-1", branchLineOf("feature", "/repo"), 1, headMerging, headRun)
			h.db.mu.Lock()
			h.db.links[tt.changed] = []wsm.WorkspaceLink{ownedPR(1, nil)}
			h.db.mu.Unlock()

			// Act
			h.o.WorkspaceLinksChanged(tt.changed)

			// Assert
			heads := h.feed.headsOn(theWorkspace)
			want, wantHeads := "", 1
			if tt.redrawn {
				want, wantHeads = prURL(1), 2
			}
			if len(heads) != wantHeads || headTargetPR(t, h, theWorkspace) != want {
				t.Fatalf("heads = %d opening %q, want %d opening %q", len(heads), headTargetPR(t, h, theWorkspace), wantHeads, want)
			}
		})
	}
}
