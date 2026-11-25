package workspace

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"
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

func TestCloseInterruptsATurnInFlightThenCloses(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)

	// Act.
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert: the turn is stopped ahead of the close, which is not refused.
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
	if len(f.shim.killedTurns) != 1 || !f.shim.killedTurns[0].Force {
		t.Fatalf("killed turns = %+v, want one confirmed kill", f.shim.killedTurns)
	}
	if !f.db.closedFlags["w1"] {
		t.Fatal("the workspace was not closed after its turn was interrupted")
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opClose, Message: "interrupted the running turn ahead of the verb"}); !ok {
		t.Fatal("no info record of the interrupt ahead of the close")
	}
}

func TestACloseWhoseInterruptFailsClosesNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)
	f.shim.killTurnErr = errors.New("shim unreachable")

	// Act.
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert.
	if err == nil {
		t.Fatal("Close succeeded although the turn could not be interrupted")
	}
	if f.db.closedFlags["w1"] {
		t.Fatal("the workspace was closed although its turn could not be interrupted")
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opClose}); !ok {
		t.Fatal("no error record of the failed interrupt")
	}
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
	if got, want := f.footer.closing["w1"].Detail, "1 held prompts have not been delivered; release or drop them first"; got != want {
		t.Fatalf("footer detail = %q, want %q", got, want)
	}
}

func TestCloseRefusesALiveDetachedShellWithNoTurnOpen(t *testing.T) {
	// Arrange: a detached shell outlives its turn; no turn is open.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.running.Turn = nil
	f.running.LiveWork = sessionwatcher.LiveWorkSet{Shells: []*conversationv1.DetachedWorkId{{Value: "work-close-1"}}}

	// Act.
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert.
	asRefusal(t, err, "blocked")
	closing := f.footer.closing["w1"]
	if closing.Reason != "live_work" {
		t.Fatalf("footer reason = %q, want live_work", closing.Reason)
	}
	if want := "0 detached agents and 1 detached shells are still live"; closing.Detail != want {
		t.Fatalf("footer detail = %q, want %q", closing.Detail, want)
	}
	if f.db.closedFlags["w1"] {
		t.Fatal("the workspace was closed although detached work was live")
	}
}

func TestCloseIsNotBlockedByHoldsTheHeldSectionDoesNotDraw(t *testing.T) {
	tests := []struct {
		name string
		hold wsm.HeldPrompt
	}{
		{name: "a workspace notice to the agent", hold: wsm.HeldPrompt{Workspace: "w1", Turn: "t1", Origin: "PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE"}},
		{name: "a retired prompt", hold: wsm.HeldPrompt{Workspace: "w1", Turn: "t1", Tombstone: &wsm.Tombstone{Kind: "dropped"}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the only hold is one the user can neither see nor drop.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.held["w1"] = []wsm.HeldPrompt{tt.hold}

			// Act.
			err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

			// Assert.
			if err != nil {
				t.Fatalf("Close = %v, want the close allowed", err)
			}
		})
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
	f.running.LiveWork = sessionwatcher.LiveWorkSet{
		Agents: []*conversationv1.AgentId{{Value: "agent-1"}},
	}

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
	f.running.LiveWork = sessionwatcher.LiveWorkSet{
		Agents: []*conversationv1.AgentId{{Value: "agent-1"}},
	}

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

// mergedNoticeFixture is a fixture whose merged-pull-requests brief is shipped.
func mergedNoticeFixture(t *testing.T) *fixture {
	t.Helper()
	f := newFixture(t)
	f.briefs[BriefOwnedPullRequestsMerged] = prompts.Prompt{
		Name: BriefOwnedPullRequestsMerged, Body: "merged: {{pull_requests}}", Placeholders: []string{"pull_requests"},
	}
	return f
}

func TestTellOwnedPullRequestsMerged(t *testing.T) {
	tests := []struct {
		name     string
		links    []wsm.WorkspaceLink
		turn     bool
		wantTold bool
	}{
		{name: "a workspace whose owned pull requests all merged is told so", links: []wsm.WorkspaceLink{ownedPullRequest(1, true), ownedPullRequest(2, true)}, wantTold: true},
		{name: "a workspace mid-turn is told so too, never closed", links: []wsm.WorkspaceLink{ownedPullRequest(1, true)}, turn: true, wantTold: true},
		{name: "a workspace with an owned pull request still open is told nothing", links: []wsm.WorkspaceLink{ownedPullRequest(1, true), ownedPullRequest(2, false)}},
		{name: "a workspace that owns no pull request is told nothing"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := mergedNoticeFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.links = map[ids.WorkspaceID][]wsm.WorkspaceLink{"w1": tt.links}
			if tt.turn {
				turn := wsm.TurnID("t1")
				f.running.Turn = &turn
			}

			// Act
			err := f.verbs.TellOwnedPullRequestsMerged(context.Background(), "w1")

			// Assert
			if err != nil {
				t.Fatalf("TellOwnedPullRequestsMerged: %v", err)
			}
			if f.db.closedFlags["w1"] || f.db.workspaces["w1"].CloseRequest != nil {
				t.Fatalf("closed %v, close requested %v; want the workspace left open with no close request", f.db.closedFlags["w1"], f.db.workspaces["w1"].CloseRequest != nil)
			}
			var told []string
			for _, sub := range f.queue.submissions {
				if sub.WS == "w1" && sub.Origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE {
					told = append(told, submittedText(sub.Said))
				}
			}
			if (len(told) == 1) != tt.wantTold || len(told) > 1 {
				t.Fatalf("notices = %q, want told %v once", told, tt.wantTold)
			}
			if tt.wantTold && !strings.Contains(told[0], tt.links[0].URL) {
				t.Fatalf("notice = %q, want it to name the merged pull requests", told[0])
			}
		})
	}
}

func TestTellOwnedPullRequestsMergedSurfacesAnUnreadableLinkSet(t *testing.T) {
	// Arrange
	f := mergedNoticeFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.linksReadErr = errFake

	// Act
	err := f.verbs.TellOwnedPullRequestsMerged(context.Background(), "w1")

	// Assert
	if !errors.Is(err, errFake) || len(f.queue.submissions) != 0 {
		t.Fatalf("err %v, submissions %+v; want the read's failure and nothing told", err, f.queue.submissions)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opClose, Message: "could not read the workspace's links; it is not told its pull requests merged"}); !ok {
		t.Fatalf("records = %+v, want the failure at error", f.log.logger.Records())
	}
}

func TestCloseRefusesAWorkspaceWhoseMergeStandsOnItsQueueWithNoFacts(t *testing.T) {
	tests := []struct {
		name  string
		close func(f *fixture) error
	}{
		{name: "a close", close: func(f *fixture) error { return f.verbs.Close(context.Background(), "w1", wsm.CloseByUser) }},
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

func TestCloseJudgesTheFamilyRuleOnlyForAnOpenWorkspacesGesture(t *testing.T) {
	tests := []struct {
		name      string
		by        wsm.CloseCause
		closed    bool
		wantAsked bool
	}{
		{name: "the user's close", by: wsm.CloseByUser, wantAsked: true},
		{name: "an agent's close", by: wsm.CloseByAgent, wantAsked: true},
		{name: "a merge's close", by: wsm.CloseByMerge},
		{name: "the daemon's close", by: wsm.CloseByDaemon},
		{name: "a user's close of a closed workspace", by: wsm.CloseByUser, closed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			ws := f.db.workspaces["w1"]
			ws.Closed = tt.closed
			f.db.workspaces["w1"] = ws

			// Act
			err := f.verbs.Close(context.Background(), "w1", tt.by)

			// Assert
			if err != nil {
				t.Fatalf("Close: %v", err)
			}
			if asked := slices.Contains(f.db.noOpenChildrenAsked, "w1"); asked != tt.wantAsked {
				t.Errorf("asked the family rule = %v, want %v", asked, tt.wantAsked)
			}
		})
	}
}

func TestCloseRefusesAParentWithOpenChildrenBeforeItsWrite(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.noOpenChildrenErr = errOpenChildren

	// Act
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert
	requireFamilyAnswer(t, f, err, errOpenChildren, opClose)
	if len(f.db.closures["w1"]) != 0 {
		t.Fatalf("closures = %+v, want none written", f.db.closures["w1"])
	}
}

func TestCloseFailsWhenItsOpenChildrenCannotBeRead(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.noOpenChildrenErr = errors.New("db gone")

	// Act
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert
	if err == nil || isCloseRefusal(err) || len(f.db.closures["w1"]) != 0 {
		t.Fatalf("Close = %v, closures %+v; want a hard failure with nothing written", err, f.db.closures["w1"])
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opClose,
		Message: "could not read whether the workspace has open children"}); !ok {
		t.Errorf("no error record: %+v", f.log.logger.Records())
	}
}

func TestIsCloseRefusal(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want bool
	}{
		{name: "the blocked arm", err: &Refusal{Rpc: "CloseWorkspace", Arm: "blocked"}, want: true},
		{name: "a family rule", err: errOpenChildren, want: true},
		{name: "a failure", err: errors.New("db gone")},
		{name: "no error"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := isCloseRefusal(tt.err)

			// Assert
			if got != tt.want {
				t.Errorf("isCloseRefusal(%v) = %v, want %v", tt.err, got, tt.want)
			}
		})
	}
}

// selfClosing is a fixture whose workspace w1 runs turn t1.
func selfClosing(t *testing.T) *fixture {
	t.Helper()
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn
	return f
}

func TestCloseSelfWithItsAskingTurnRunningStandsOnThatTurnAlone(t *testing.T) {
	// Arrange
	f := selfClosing(t)

	// Act
	later, waits, err := f.verbs.CloseSelf(context.Background(), "w1")

	// Assert
	if err != nil || !later || waits != nil {
		t.Fatalf("CloseSelf = %v, %+v, %v; want closes when the turn ends", later, waits, err)
	}
	request := f.db.workspaces["w1"].CloseRequest
	if f.db.closedFlags["w1"] || request == nil || request.By != wsm.CloseByAgent {
		t.Fatalf("closed %v, request %+v; want open with the agent's request standing", f.db.closedFlags["w1"], request)
	}
	if len(f.shim.killedTurns) != 0 {
		t.Fatalf("killed turns = %+v, want the asking turn left running", f.shim.killedTurns)
	}
	if got := f.standingCloses.reconsidered; len(got) != 1 || got[0] != "w1" {
		t.Fatalf("reconsidered = %v, want w1 owed a judgment once registered", got)
	}
}

func TestCloseSelfWithNoTurnRunningClosesNow(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	later, waits, err := f.verbs.CloseSelf(context.Background(), "w1")

	// Assert
	if err != nil || later || waits != nil {
		t.Fatalf("CloseSelf = %v, %+v, %v; want closed now", later, waits, err)
	}
	if closures := f.db.closures["w1"]; len(closures) != 1 || closures[0].By != wsm.CloseByAgent {
		t.Fatalf("closures = %+v, want one by the agent", closures)
	}
}

func TestCloseSelfStandsOnEverythingButItsOwnTurn(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		check   func(t *testing.T, waits *CloseWaits)
	}{
		{name: "a held prompt", arrange: func(f *fixture) { f.db.held["w1"] = []wsm.HeldPrompt{{Turn: "held-1"}} },
			check: func(t *testing.T, waits *CloseWaits) {
				if waits.Blocked == nil || waits.Blocked.TurnInFlight || waits.Blocked.HeldPrompts != 1 || waits.Family != nil {
					t.Fatalf("waits = %+v, want the held prompt alone", waits)
				}
			}},
		{name: "an open child", arrange: func(f *fixture) { f.db.noOpenChildrenErr = errOpenChildren },
			check: func(t *testing.T, waits *CloseWaits) {
				if waits.Blocked != nil || waits.Family != errOpenChildren {
					t.Fatalf("waits = %+v, want the open child alone", waits)
				}
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := selfClosing(t)
			tt.arrange(f)

			// Act
			later, waits, err := f.verbs.CloseSelf(context.Background(), "w1")

			// Assert
			if err != nil || later || waits == nil {
				t.Fatalf("CloseSelf = %v, %+v, %v; want it to stand", later, waits, err)
			}
			tt.check(t, waits)
			if f.db.workspaces["w1"].CloseRequest == nil || f.db.closedFlags["w1"] {
				t.Fatalf("request %+v, closed %v; want standing and open", f.db.workspaces["w1"].CloseRequest, f.db.closedFlags["w1"])
			}
			if blocked := f.footer.closing["w1"]; blocked == nil || !blocked.Standing {
				t.Fatalf("footer = %+v, want the standing close waiting", blocked)
			}
		})
	}
}

func TestCloseSelfFails(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(f *fixture)
		operation string
		message   string
	}{
		{name: "the request cannot be registered", arrange: func(f *fixture) { f.db.requestCloseErr = errors.New("db gone") },
			operation: opStandingClose, message: "could not register the standing close request"},
		{name: "the open children cannot be read", arrange: func(f *fixture) { f.db.noOpenChildrenErr = errors.New("db gone") },
			operation: opClose, message: "could not read whether the workspace has open children"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := selfClosing(t)
			tt.arrange(f)

			// Act
			_, _, err := f.verbs.CloseSelf(context.Background(), "w1")

			// Assert
			if _, refused := AsRefusal(err); err == nil || refused {
				t.Fatalf("CloseSelf = %v, want a hard failure", err)
			}
			if f.db.closedFlags["w1"] || f.db.workspaces["w1"].CloseRequest != nil || len(f.standingCloses.reconsidered) != 0 {
				t.Fatalf("closed %v, request %+v, reconsidered %v; want nothing changed", f.db.closedFlags["w1"], f.db.workspaces["w1"].CloseRequest, f.standingCloses.reconsidered)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: tt.operation, Message: tt.message}); !ok {
				t.Errorf("no error record %q: %+v", tt.message, f.log.logger.Records())
			}
		})
	}
}

func TestCloseQuietlyIsRefusedByATurnInFlight(t *testing.T) {
	// Arrange
	f := selfClosing(t)

	// Act
	err := f.verbs.(*verbs).closeQuietly(context.Background(), "w1", wsm.CloseByAgent)

	// Assert
	refusal := asRefusal(t, err, "blocked")
	if refusal.Fields["turn_in_flight"] != true || len(f.shim.killedTurns) != 0 || f.db.closedFlags["w1"] {
		t.Fatalf("fields %+v, killed %+v, closed %v; want refused for the turn, which runs on", refusal.Fields, f.shim.killedTurns, f.db.closedFlags["w1"])
	}
}

func TestASelfCloseStandsOnAMergeStandingOnItsQueueWithNoFacts(t *testing.T) {
	// Arrange: a merge a pause suspended, which this daemon published no
	// facts for, still holds its queue's line.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.mergeQueues = map[wsm.RepoKey][]wsm.MergeQueueEntry{"/repo/.git": {{Workspace: "w1", State: wsm.MergeAdmitted}}}

	// Act
	_, waits, err := f.verbs.CloseSelf(context.Background(), "w1")

	// Assert
	if err != nil || waits == nil || waits.Blocked == nil || waits.Blocked.Reason != "merge_queued" {
		t.Fatalf("CloseSelf = %+v, %v; want it to stand on the queued merge", waits, err)
	}
	if !strings.Contains(f.footer.closing["w1"].Detail, "on its queue") || f.db.closedFlags["w1"] {
		t.Fatalf("footer %+v, closed %v; want the standing close naming the queue, open", f.footer.closing["w1"], f.db.closedFlags["w1"])
	}
}
