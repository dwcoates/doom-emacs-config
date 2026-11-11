package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/wsm"
)

func TestCloseSucceedsOnAQuietWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if !f.db.closedFlags["w1"] {
		t.Fatal("Close() did not record the workspace as closed")
	}
}

func TestCloseRecordsWhoAskedForIt(t *testing.T) {
	tests := []struct {
		name string
		by   wsm.CloseCause
	}{
		{name: "the user's close", by: wsm.CloseByUser},
		{name: "an agent's close", by: wsm.CloseByAgent},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())

			// Act.
			if err := f.verbs.Close(context.Background(), "w1", tt.by); err != nil {
				t.Fatalf("Close: %v", err)
			}

			// Assert.
			closures := f.db.closures["w1"]
			if len(closures) != 1 || closures[0].By != tt.by || closures[0].At.IsZero() {
				t.Fatalf("close records = %+v, want one by %s with its instant", closures, tt.by)
			}
			if closures[0].How != wsm.HowClosed {
				t.Fatalf("close record = %+v, want it told as a plain close", closures[0])
			}
		})
	}
}

func TestCloseRefusesATurnInFlight(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn

	// Act.
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert.
	asRefusal(t, err, "blocked")
}

func TestCloseRefusesLiveDetachedWork(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.running.LiveWork = sessionwatcher.LiveWorkSet{
		Agents: []*conversationv1.AgentId{{Value: "agent-1"}},
	}

	// Act.
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert.
	asRefusal(t, err, "blocked")
	if f.footer.closing["w1"].Reason != "live_work" {
		t.Fatalf("footer reason = %q, want live_work", f.footer.closing["w1"].Reason)
	}
}

func TestCloseRefusesHeldPrompts(t *testing.T) {
	// Arrange: undelivered user intent is never silently discarded.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.held["w1"] = []wsm.HeldPrompt{{Workspace: "w1", Turn: "t1"}}

	// Act.
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert.
	asRefusal(t, err, "blocked")
	if f.footer.closing["w1"].Reason != "held_prompts" {
		t.Fatalf("footer reason = %q, want held_prompts", f.footer.closing["w1"].Reason)
	}
}

func TestCloseIsAllowedWithAStandingColdGate(t *testing.T) {
	// Arrange: a standing cold gate holds no undelivered user intent, so it
	// deliberately does NOT block a close.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.footer.coldGates["w1"] = footer.ColdGate{Standing: true}

	// Act.
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert.
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
}

func TestCloseIsAllowedWithAFinishedMerge(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.merge.facts["w1"] = footer.MergeFacts{State: "merged"}

	// Act.
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert.
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
}

func TestCloseIsAllowedWithNoLiveSession(t *testing.T) {
	// Arrange: a parked session runs nothing, so it blocks nothing.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hasSession = false

	// Act.
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert.
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
}

func TestCloseManifestsTheRefusalInTheFooter(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn

	// Act.
	_ = f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert.
	blocked := f.footer.closing["w1"]
	if blocked == nil || blocked.Detail == "" {
		t.Fatalf("footer close refusal = %+v, want a drawn reason", blocked)
	}
}

func TestCloseRetiresTheFooterRefusalOnSuccess(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if f.footer.closing["w1"] != nil {
		t.Fatalf("footer close refusal = %+v, want it cleared", f.footer.closing["w1"])
	}
}

func TestCloseEvictsTheWorkspaceLogSink(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if len(f.log.evicted) != 1 || f.log.evicted[0] != ws.Dir {
		t.Fatalf("evicted sinks = %v, want the workspace's own", f.log.evicted)
	}
}

func TestCloseLeavesTheSessionAlone(t *testing.T) {
	// Arrange: Close is view-level; Kill is what ends a session.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.live["w1"] = true

	// Act.
	if err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if len(f.fleet.stopped) != 0 {
		t.Fatalf("stopped sessions = %+v, want none", f.fleet.stopped)
	}
}

func TestCloseSucceedsWhenTheSinkEvictionFails(t *testing.T) {
	// Arrange: a sink that will not release is a LEAK, not a reason to refuse a
	// close the record already says happened.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.log.evictErr = errors.New("the sink is already closed")

	// Act.
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert.
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
	if !f.db.closedFlags["w1"] {
		t.Fatal("Close() did not record the workspace as closed")
	}
}

func TestCloseRefusalIsNotLoggedAsAWarning(t *testing.T) {
	// Arrange: `blocked` is a LANDED CloseWorkspaceError arm, so the refusal is
	// an ordinary answer the client reads rather than a fault.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn

	// Act.
	_ = f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert.
	for _, record := range f.log.logger.Records() {
		if record.Operation == opClose && (record.Level == "warn" || record.Level == "error") {
			t.Fatalf("record = %+v, want the landed refusal recorded below warning", record)
		}
	}
}

func TestCloseRefusesAMergeWaitingInLine(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.merge.facts["w1"] = footer.MergeFacts{State: "queued", Step: footer.StepEnqueued, QueuePlace: 2, QueueWaiting: 2}

	// Act.
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert.
	asRefusal(t, err, "blocked")
	if f.footer.closing["w1"].Reason != "merge_queued" {
		t.Fatalf("footer reason = %q, want merge_queued", f.footer.closing["w1"].Reason)
	}
}

func TestCloseAnswersAParentWithOpenChildren(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.setClosedErr = errOpenChildren

	// Act.
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert.
	requireFamilyAnswer(t, f, err, errOpenChildren, opClose)
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a refused close", len(f.sidebar.registries))
	}
}

// ownedPullRequest is an owned link to pull request NUMBER, ended merged when
// MERGED and still followed and open otherwise.
func ownedPullRequest(number uint32, merged bool) wsm.WorkspaceLink {
	key := wsm.PullRequestKey{Owner: "o", Repo: "r", Number: number}
	sub := &wsm.PullRequestSubscription{PullRequest: key, Origin: wsm.PullRequestOriginOwnedLink}
	if merged {
		sub.Ended = &wsm.PullRequestEnd{Merged: true}
	}
	return wsm.WorkspaceLink{URL: key.String(), Owned: true, PullRequest: &key, Follow: sub}
}

func TestCloseWhenOwnedPullRequestsMerged(t *testing.T) {
	tests := []struct {
		name       string
		links      []wsm.WorkspaceLink
		turn       bool
		wantClosed bool
		wantMarked bool
	}{
		{name: "a quiet workspace whose owned pull requests all merged closes", links: []wsm.WorkspaceLink{ownedPullRequest(1, true), ownedPullRequest(2, true)}, wantClosed: true},
		{name: "a workspace with an owned pull request still open stays open", links: []wsm.WorkspaceLink{ownedPullRequest(1, true), ownedPullRequest(2, false)}},
		{name: "a workspace that owns no pull request stays open"},
		{name: "a workspace mid-turn closes when the turn ends", links: []wsm.WorkspaceLink{ownedPullRequest(1, true)}, turn: true, wantMarked: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.links = map[ids.WorkspaceID][]wsm.WorkspaceLink{"w1": tt.links}
			if tt.turn {
				turn := wsm.TurnID("t1")
				f.running.Turn = &turn
			}

			// Act
			err := f.verbs.CloseWhenOwnedPullRequestsMerged(context.Background(), "w1")

			// Assert
			if err != nil {
				t.Fatalf("CloseWhenOwnedPullRequestsMerged: %v", err)
			}
			if f.db.closedFlags["w1"] != tt.wantClosed {
				t.Fatalf("closed = %v, want %v", f.db.closedFlags["w1"], tt.wantClosed)
			}
			if tt.wantClosed {
				if closures := f.db.closures["w1"]; len(closures) != 1 || closures[0].By != wsm.CloseByMerge {
					t.Fatalf("closures = %+v, want one by the merge", closures)
				}
			}
			if marked := f.db.workspaces["w1"].CloseWhenTurnEnds != nil; marked != tt.wantMarked {
				t.Fatalf("marked to close at the turn's end = %v, want %v", marked, tt.wantMarked)
			}
		})
	}
}

func TestCloseWhenOwnedPullRequestsMergedSurfacesAnUnreadableLinkSet(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.linksReadErr = errFake

	// Act
	err := f.verbs.CloseWhenOwnedPullRequestsMerged(context.Background(), "w1")

	// Assert
	if !errors.Is(err, errFake) || f.db.closedFlags["w1"] {
		t.Fatalf("err %v, closed %v; want the read's failure and the workspace open", err, f.db.closedFlags["w1"])
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opClose, Message: "could not read the workspace's links; it is not closed for its merged pull requests"}); !ok {
		t.Fatalf("records = %+v, want the failure at error", f.log.logger.Records())
	}
}

func TestCloseRefusesAWorkspaceWhoseMergeStandsOnItsQueueWithNoFacts(t *testing.T) {
	tests := []struct {
		name  string
		close func(f *fixture) error
	}{
		{name: "a close", close: func(f *fixture) error { return f.verbs.Close(context.Background(), "w1", wsm.CloseByUser) }},
		{name: "a self-close", close: func(f *fixture) error {
			_, err := f.verbs.CloseSelf(context.Background(), "w1")
			return err
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a merge a pause suspended, which this daemon published
			// no facts for, still holds its queue's line.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.mergeQueues = map[wsm.RepoKey][]wsm.MergeQueueEntry{"/repo/.git": {{Workspace: "w1", State: wsm.MergeAdmitted}}}

			// Act.
			err := tt.close(f)

			// Assert.
			refusal := asRefusal(t, err, "blocked")
			if f.footer.closing["w1"].Reason != "merge_queued" || !strings.Contains(refusal.Reason, "on its queue") {
				t.Fatalf("footer reason = %q, refusal %q; want merge_queued naming the queue", f.footer.closing["w1"].Reason, refusal.Reason)
			}
			if f.db.closedFlags["w1"] {
				t.Fatal("the workspace closed with its merge standing on its queue")
			}
		})
	}
}

func TestCloseWhoseMergeQueuesCannotBeReadClosesNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.mergeQueuesErr = errors.New("database locked")

	// Act.
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "database locked") {
		t.Fatalf("Close = %v, want the read's failure", err)
	}
	if f.db.closedFlags["w1"] {
		t.Fatal("the workspace closed though its merge queues could not be read")
	}
	found := false
	for _, r := range f.log.logger.Records() {
		if r.Level == "error" && r.Operation == opClose && r.Message == "could not judge whether the workspace is quiet" {
			found = true
		}
	}
	if !found {
		t.Fatalf("no ERROR record of the failed judgment: %+v", f.log.logger.Records())
	}
}
