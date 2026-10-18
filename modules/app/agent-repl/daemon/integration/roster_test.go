//go:build integration

package integration

import (
	"agentrepl/protohelpers/oneofarm"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

func TestRegisteringAWorkspaceDrawsOneRowWithStatusNone(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	roster := d.WatchRoster()
	harness.AwaitNext(t, d.Ctx(), roster, "the empty roster")
	repo := harness.NewRepo(t)

	// Act
	ws := harness.Register(t, d, repo.Dir)

	// Assert
	got := awaitRoster(t, d, roster, "the new workspace's row", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRepoRow(r, ws.GetId()) != nil
	})
	row := rosterRepoRow(got, ws.GetId())
	if row.GetNone() == nil {
		t.Fatalf("a registered workspace's status = %T, want RosterRowStatusNone before any session", row.GetStatus())
	}
	harness.ExpectNoPush(t, roster, harness.ProbeWindow, "registering a workspace produces exactly one roster push")
}

func TestRosterStatusFollowsTheSessionLifecycle(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	roster := f.d.WatchRoster()
	statusIs := func(pred func(*frontendv1.RosterRow) bool) func(*frontendv1.WorkspaceRoster) bool {
		return func(r *frontendv1.WorkspaceRoster) bool {
			row := rosterRow(r, f.ws.GetId())
			return row != nil && pred(row)
		}
	}
	awaitRoster(t, f.d, roster, "ready after readiness", statusIs(func(row *frontendv1.RosterRow) bool { return row.GetReady() != nil }))

	// Act / Assert: thinking during a turn.
	resp := f.submit("do the thing", "k-lifecycle", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	turn := resp.GetSuccess().GetTurn().GetTurn()
	if turn.GetValue() == "" {
		t.Fatalf("SubmitPrompt = %v, want a minted TurnId", resp)
	}
	awaitRoster(t, f.d, roster, "thinking during a turn", statusIs(func(row *frontendv1.RosterRow) bool { return row.GetThinking() != nil }))

	// Act / Assert: permission while a permission card is open.
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission("perm-1", "act-1")},
	}))
	awaitRoster(t, f.d, roster, "permission while a permission is open", statusIs(func(row *frontendv1.RosterRow) bool { return row.GetPermission() != nil }))

	// Act / Assert: done after the turn concludes.
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: answeredPermission("perm-1", "act-1")},
	}))
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	awaitRoster(t, f.d, roster, "done after the turn concludes", statusIs(func(row *frontendv1.RosterRow) bool { return row.GetDone() != nil }))

	// Act / Assert: idle_async, FULL, as soon as detached work runs, without
	// waiting for the user to view the unread result.
	pushDetachedShell(f.shim, "work-1", "sleep 1")
	awaitRoster(t, f.d, roster, "idle_async with detached work over the unread result",
		statusIs(func(row *frontendv1.RosterRow) bool { return row.GetIdleAsync() != nil && row.GetViewed() == nil }))
	f.d.AwaitWorkspaceLogOperation(f.ws.GetDir(), "daemon.sidebar.async_outranks_unread")

	// Act / Assert: viewing the idle_async row reads the unread result.
	// The row does not move, so no roster push comes: the roster's own
	// record of the read is what proves the report took.
	markViewed(t, f.d, f.ws)
	f.d.AwaitWorkspaceLogOperation(f.ws.GetDir(), "daemon.sidebar.result_read")
}

// tabBarIDs lists the roster's tab bar, first tab first.
func tabBarIDs(r *frontendv1.WorkspaceRoster) []string {
	var out []string
	for _, tab := range r.GetTabBar().GetTabs() {
		out = append(out, tab.GetWorkspace().GetId())
	}
	return out
}

// swapWorkspaces exchanges ws's and target's places and requires a success.
func swapWorkspaces(t *testing.T, d *harness.Daemon, ws, target *workspacev1.WorkspaceRef) {
	t.Helper()
	resp, err := d.Client().SwapWorkspaces(d.Ctx(), connect.NewRequest(
		&agentreplv1.SwapWorkspacesRequest{Workspace: ws, Target: target}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("SwapWorkspaces = (%v, %v), want success", resp, err)
	}
}

func TestSettingAPriorityMovesTheWorkspaceSecondToLast(t *testing.T) {
	t.Parallel()
	// Arrange: the bar is a, c, b.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	roster := d.WatchRoster()
	a := harness.Register(t, d, worktreeOf(t, repo, "a"))
	b := harness.Register(t, d, worktreeOf(t, repo, "b"))
	c := harness.Register(t, d, worktreeOf(t, repo, "c"))

	// Act: b takes a priority nobody else has, so it lands second-to-last
	// among the others.
	setPriority(t, d, b, &agentreplv1.WorkspacePriority{
		Level: &agentreplv1.WorkspacePriority_P05{P05: &agentreplv1.WorkspacePriorityP05{}},
	})

	// Assert.
	awaitRoster(t, d, roster, "the bar a, b, c with b's badge", func(r *frontendv1.WorkspaceRoster) bool {
		return sameOrder(tabBarIDs(r), []string{a.GetId(), b.GetId(), c.GetId()}) &&
			rosterRow(r, b.GetId()).GetPriority().GetLabel() != ""
	})
}

func TestReopeningAWorkspaceRestoresItsPlace(t *testing.T) {
	t.Parallel()
	// Arrange: the bar is a, c, b; c closes and d lands while it is away.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	roster := d.WatchRoster()
	a := harness.Register(t, d, worktreeOf(t, repo, "a"))
	b := harness.Register(t, d, worktreeOf(t, repo, "b"))
	cDir := worktreeOf(t, repo, "c")
	c := harness.Register(t, d, cDir)
	if _, err := d.Client().CloseWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: c})); err != nil {
		t.Fatalf("CloseWorkspace = error %v, want a success with nothing live", err)
	}
	dd := harness.Register(t, d, worktreeOf(t, repo, "d"))

	// Act: announcing the closed workspace re-opens it.
	harness.Register(t, d, cDir)

	// Assert: c is back where it was, before d.
	awaitRoster(t, d, roster, "the bar a, c, d, b", func(r *frontendv1.WorkspaceRoster) bool {
		return sameOrder(tabBarIDs(r), []string{a.GetId(), c.GetId(), dd.GetId(), b.GetId()})
	})
}

func TestSwapWorkspacesExchangesTheTwoPlaces(t *testing.T) {
	t.Parallel()
	// Arrange: the bar is a, c, b.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	roster := d.WatchRoster()
	a := harness.Register(t, d, worktreeOf(t, repo, "a"))
	b := harness.Register(t, d, worktreeOf(t, repo, "b"))
	c := harness.Register(t, d, worktreeOf(t, repo, "c"))

	// Act: a swaps with b, two tabs to its right.
	swapWorkspaces(t, d, a, b)

	// Assert: a and b exchange places and c, between them, stays put.
	awaitRoster(t, d, roster, "the bar b, c, a", func(r *frontendv1.WorkspaceRoster) bool {
		return sameOrder(tabBarIDs(r), []string{b.GetId(), c.GetId(), a.GetId()})
	})
}

func TestMoveWorkspaceTakesTheTargetsTabAndShiftsTheTabsBetween(t *testing.T) {
	t.Parallel()
	// Arrange: the bar is a, c, b.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	roster := d.WatchRoster()
	a := harness.Register(t, d, worktreeOf(t, repo, "a"))
	b := harness.Register(t, d, worktreeOf(t, repo, "b"))
	c := harness.Register(t, d, worktreeOf(t, repo, "c"))
	awaitRoster(t, d, roster, "the bar a, c, b", func(r *frontendv1.WorkspaceRoster) bool {
		return sameOrder(tabBarIDs(r), []string{a.GetId(), c.GetId(), b.GetId()})
	})

	// Act: a moves onto b's tab, two tabs to its right.
	resp, err := d.Client().MoveWorkspace(d.Ctx(), connect.NewRequest(
		&agentreplv1.MoveWorkspaceRequest{Workspace: a, Target: b}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("MoveWorkspace = (%v, %v), want success", resp, err)
	}

	// Assert: c and b shift left and a takes the last tab.
	awaitRoster(t, d, roster, "the bar c, b, a", func(r *frontendv1.WorkspaceRoster) bool {
		return sameOrder(tabBarIDs(r), []string{c.GetId(), b.GetId(), a.GetId()})
	})
}

func TestMoveWorkspaceIsRefusedAcrossRepositories(t *testing.T) {
	t.Parallel()
	// Arrange: one workspace in each of two repositories.
	d := newDaemon(t, harness.Opts{})
	one := harness.Register(t, d, worktreeOf(t, harness.NewRepo(t), "one"))
	two := harness.Register(t, d, worktreeOf(t, harness.NewRepo(t), "two"))

	// Act.
	resp, err := d.Client().MoveWorkspace(d.Ctx(), connect.NewRequest(
		&agentreplv1.MoveWorkspaceRequest{Workspace: two, Target: one}))

	// Assert: the tab bar keeps each repository's workspaces together.
	if err != nil || resp.Msg.GetError().GetOtherRepository() == nil {
		t.Fatalf("MoveWorkspace = (%v, %v), want other_repository", resp, err)
	}
}

// arrange sends one ArrangeSidebar change and requires its success.
func arrange(t *testing.T, d *harness.Daemon, req *agentreplv1.ArrangeSidebarRequest) *agentreplv1.ArrangeSidebarSuccess {
	t.Helper()
	resp, err := d.Client().ArrangeSidebar(d.Ctx(), connect.NewRequest(req))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("ArrangeSidebar = (%v, %v), want success", resp, err)
	}
	return resp.Msg.GetSuccess()
}

func TestTheFirstWorkspaceFiledInANewSubsectionBecomesItsRepositorysLastTab(t *testing.T) {
	t.Parallel()
	// Arrange: the owner's example, a, b, c in one repository.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	roster := d.WatchRoster()
	a := harness.Register(t, d, worktreeOf(t, repo, "a"))
	b := harness.Register(t, d, worktreeOf(t, repo, "b"))
	c := harness.Register(t, d, worktreeOf(t, repo, "c"))
	first := awaitRoster(t, d, roster, "three tabs", func(r *frontendv1.WorkspaceRoster) bool { return len(tabBarIDs(r)) == 3 })
	head := tabBarIDs(first)[0]
	section := first.GetRepository().GetSections()[0]
	created := arrange(t, d, &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_CreateSubsection{
		CreateSubsection: &agentreplv1.ArrangeCreateSubsection{Repository: section.GetKey().GetRepository(), Name: "later"}}}).GetCreated()

	// Act.
	var headRef *workspacev1.WorkspaceRef
	for _, ws := range []*workspacev1.WorkspaceRef{a, b, c} {
		if ws.GetId() == head {
			headRef = ws
		}
	}
	arrange(t, d, &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_PlaceWorkspace{
		PlaceWorkspace: &agentreplv1.ArrangePlaceWorkspace{
			Workspace: headRef,
			Into:      &agentreplv1.ArrangePlaceWorkspace_Subsection{Subsection: created},
			At:        &agentreplv1.ArrangePlaceWorkspace_End{End: &agentreplv1.ArrangeEnd{}},
		}}})

	// Assert.
	awaitRoster(t, d, roster, "the first tab now last", func(r *frontendv1.WorkspaceRoster) bool {
		ids := tabBarIDs(r)
		return len(ids) == 3 && ids[2] == head && len(r.GetRepository().GetSections()[0].GetSubsections()) == 1
	})
}

func TestMovingARepositoryMovesAllItsTabs(t *testing.T) {
	t.Parallel()
	// Arrange: one workspace in each of two repositories.
	d := newDaemon(t, harness.Opts{})
	roster := d.WatchRoster()
	harness.Register(t, d, worktreeOf(t, harness.NewRepo(t), "one"))
	harness.Register(t, d, worktreeOf(t, harness.NewRepo(t), "two"))
	r := awaitRoster(t, d, roster, "two sections", func(r *frontendv1.WorkspaceRoster) bool {
		return len(r.GetRepository().GetSections()) == 2 && len(tabBarIDs(r)) == 2
	})
	firstRepo := r.GetRepository().GetSections()[0].GetKey().GetRepository()
	before := tabBarIDs(r)

	// Act.
	arrange(t, d, &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_MoveRepository{
		MoveRepository: &agentreplv1.ArrangeMoveRepository{Repository: firstRepo, To: &agentreplv1.ArrangeMoveRepository_End{End: &agentreplv1.ArrangeEnd{}}}}})

	// Assert.
	awaitRoster(t, d, roster, "the tabs swapped with their repositories", func(r *frontendv1.WorkspaceRoster) bool {
		return sameOrder(tabBarIDs(r), []string{before[1], before[0]})
	})
}

func TestGetSidebarArrangementListsTheRepositoriesAndTheirWorkspaces(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{})
	ws := harness.Register(t, d, worktreeOf(t, harness.NewRepo(t), "one"))

	// Act.
	resp, err := d.Client().GetSidebarArrangement(d.Ctx(), connect.NewRequest(&agentreplv1.GetSidebarArrangementRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("GetSidebarArrangement: %v", err)
	}
	repos := resp.Msg.GetRepositories()
	if len(repos) != 1 || len(repos[0].GetWorkspaces()) != 1 || repos[0].GetWorkspaces()[0].GetWorkspace().GetId() != ws.GetId() {
		t.Fatalf("arrangement = %v, want the one workspace in its repository", resp.Msg)
	}
}

func TestClearingAPriorityRemovesTheBadge(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	roster := f.d.WatchRoster()
	setPriority(t, f.d, f.ws, &agentreplv1.WorkspacePriority{
		Level: &agentreplv1.WorkspacePriority_P1{P1: &agentreplv1.WorkspacePriorityP1{}},
	})
	awaitRoster(t, f.d, roster, "the badge", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetPriority() != nil
	})

	// Act
	setPriority(t, f.d, f.ws, nil)

	// Assert
	awaitRoster(t, f.d, roster, "the badge removed", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetPriority() == nil
	})
}

func TestAttentionMarkerIsSetOnNotificationAndClearedOnSelect(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	roster := f.d.WatchRoster()

	// Act: a permission request is a host notification.
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission("perm-attn", "act-attn")},
	}))

	// Assert
	awaitRoster(t, f.d, roster, "the attention marker set on a notification", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetAttention() != nil
	})
	f.selectWorkspace()
	awaitRoster(t, f.d, roster, "the attention marker cleared by SelectWorkspace", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetAttention() == nil
	})
}

// TestAttentionMarkerIsClearedWhenTheLastAskSettles pins the OTHER clear the
// marker has: the ask the notification was about is ANSWERED, so the
// notification is seen. A workspace nobody ever selects — the whole of a
// single-workspace session — would otherwise keep an amber dot for asks that
// resolved long ago (frontend.v1.RosterRow.attention).
func TestAttentionMarkerIsClearedWhenTheLastAskSettles(t *testing.T) {
	t.Parallel()
	// Arrange: an open ask has raised the marker.
	f := newOpened(t, harness.Opts{})
	roster := f.d.WatchRoster()
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission("perm-settle", "act-settle")},
	}))
	awaitRoster(t, f.d, roster, "the attention marker set by the ask", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetAttention() != nil
	})

	// Act: the ask is answered.
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: answeredPermission("perm-settle", "act-settle")},
	}))

	// Assert
	awaitRoster(t, f.d, roster, "the attention marker cleared by the answer", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetAttention() == nil
	})
}

func TestClosedWorkspaceDrawsClosedAndNukedLeavesTheRoster(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	closedDir := worktreeOf(t, repo, "closed")
	nukedDir := worktreeOf(t, repo, "nuked")
	closed := harness.Register(t, d, closedDir)
	nuked := harness.Register(t, d, nukedDir)
	roster := d.WatchRoster()

	// Act
	if _, err := d.Client().CloseWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: closed})); err != nil {
		t.Fatalf("CloseWorkspace = error %v, want a success with nothing live", err)
	}
	if _, err := d.Client().NukeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.NukeWorkspaceRequest{Workspace: nuked})); err != nil {
		t.Fatalf("NukeWorkspace = error %v, want a success", err)
	}

	// Assert
	got := awaitRoster(t, d, roster, "the closed row marked and the nuked row gone", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, closed.GetId())
		return row != nil && row.GetClosed().GetClosed() && rosterRow(r, nuked.GetId()) == nil
	})
	if row := rosterRow(got, closed.GetId()); !row.GetClosed().GetClosed() {
		t.Fatalf("the closed workspace's row.closed = false, want true")
	}
}

// TestBringUpDeathLeavesTheRosterRowStartFailed is the roster suite's own
// coverage of session_lifecycle_test.go's
// TestFakeShimExitingDuringBringUpEndsBringUpImmediately fixture: that test
// asserts the footer and SessionHealth arms; this one asserts the roster
// row's own status arm, RosterRowStatus.start_failed
// (frontend/v1/sidebar.proto's RosterRowStatusStartFailed), landed as arm 16.
func TestBringUpDeathLeavesTheRosterRowStartFailed(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a bring-up the test blocks or kills, the shim death the test drives.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.shimclient.exit", "daemon.shimclient.spawn",
		"daemon.workspace.bring_up", "daemon.workspace.open")
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{ExitOn: harness.ExitOnStartup, ExitCode: 7, Stderr: "boom: fake bring-up death"})
	roster := f.d.WatchRoster()

	// Act
	resp, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws}))
	if err != nil {
		t.Fatalf("OpenWorkspace onto a dying shim = transport error %v, want the spawn_failed arm", err)
	}
	if resp.Msg.GetError().GetSpawnFailed() == nil {
		t.Fatalf("OpenWorkspace onto a dying shim = %v, want OpenWorkspaceError.spawn_failed", resp.Msg)
	}

	// Assert
	got := awaitRoster(t, f.d, roster, "the roster row start_failed after a bring-up death", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetStartFailed() != nil
	})
	if row := rosterRow(got, f.ws.GetId()); row.GetStartFailed() == nil {
		t.Fatalf("the roster row's status = %T after a bring-up death, want start_failed", row.GetStatus())
	}
}

// TestUpdateTaskSetDoneTwiceAnswersNoChange covers UpdateTaskError.no_change:
// "the change asked for is what the task already holds".
func TestUpdateTaskSetDoneTwiceAnswersNoChange(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	task := createTask(t, f, "done twice")
	if _, err := f.d.Client().UpdateTask(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateTaskRequest{
		Task:   task,
		Change: &agentreplv1.UpdateTaskRequest_SetDone{SetDone: &agentreplv1.UpdateTaskSetDone{}},
	})); err != nil {
		t.Fatalf("UpdateTask{set_done} (first) = error %v, want a success", err)
	}

	// Act
	resp, err := f.d.Client().UpdateTask(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateTaskRequest{
		Task:   task,
		Change: &agentreplv1.UpdateTaskRequest_SetDone{SetDone: &agentreplv1.UpdateTaskSetDone{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("UpdateTask{set_done} (second) = transport error %v, want the in-band no_change arm", err)
	}
	if resp.Msg.GetError().GetNoChange() == nil {
		t.Fatalf("UpdateTask{set_done} on an already-done task = %v, want UpdateTaskError.no_change", resp.Msg)
	}
}

// ---------------------------------------------------------------------------
// Roster status arms: each of the seven arms below is asserted ON ITS OWN,
// never inside a disjunction with a sibling arm (audit-3 critique 23), driven
// from the real cause internal/resolve/sidebar/status.go's sessionArm and
// mergeArm read facts from.
// ---------------------------------------------------------------------------

// TestRosterRowIsClearingWhileAClearRuns covers RosterRowStatusClearing: a
// /clear submission is delivered as a real turn whose act is ActClear
// (prompt_test.go's TestClearAndCompactGoThroughTheQueueAsSessionActsAndProduceASeparationRow
// drives the same submission; this test asserts the roster's own arm for it).
func TestRosterRowIsClearingWhileAClearRuns(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "ready before the clear", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetReady() != nil
	})

	// Act
	resp := f.submit("/clear", "k-clearing", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	if resp.GetError() != nil {
		t.Fatalf("SubmitPrompt(/clear) = %v, want a success", resp)
	}
	f.shim.ExpectStartTurn()

	// Assert
	got := awaitRoster(t, f.d, roster, "clearing while the cut runs", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetClearing() != nil
	})
	if row := rosterRow(got, f.ws.GetId()); row.GetClearing() == nil {
		t.Fatalf("the roster row's status = %T while /clear runs, want clearing", row.GetStatus())
	}
}

// TestRosterRowIsCompactingWhileACompactRuns covers RosterRowStatusCompacting
// driven by a /compact submission (ActCompact), the sibling cause of the
// clearing test above.
func TestRosterRowIsCompactingWhileACompactRuns(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "ready before the compact", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetReady() != nil
	})

	// Act
	resp := f.submit("/compact", "k-compacting", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	if resp.GetError() != nil {
		t.Fatalf("SubmitPrompt(/compact) = %v, want a success", resp)
	}
	f.shim.ExpectStartTurn()

	// Assert
	got := awaitRoster(t, f.d, roster, "compacting while the cut runs", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetCompacting() != nil
	})
	if row := rosterRow(got, f.ws.GetId()); row.GetCompacting() == nil {
		t.Fatalf("the roster row's status = %T while /compact runs, want compacting", row.GetStatus())
	}
}

// TestRosterRowIsTurnDiedWhenTheQueryDiesUnderATurn covers a dead query as
// what the owner ruled it (2026-09-28, 2026-10-06): agent-repl's fault,
// drawn `turn_died` until the next turn, never `vendor_blocked` — nothing
// about the vendor or the account refuses the session, and the next prompt
// restarts the query. The watcher closes the
// turn the death cut as failed (internal/resolve/sidebar/status_test.go's
// TestAQueryDeathDoesNotBlockTheRow is the unit-level half).
func TestRosterRowIsTurnDiedWhenTheQueryDiesUnderATurn(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of the died query the test feeds.
	// A death under a turn also draws the turn's terminal row in the feed.
	f.d.ExpectWarnings("daemon.sessionwatcher.query_died", "daemon.feed.query_died")
	roster := f.d.WatchRoster()
	f.submit("go", "k-query-died", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	awaitRoster(t, f.d, roster, "the turn in flight before the query dies", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && (row.GetSubmitting() != nil || row.GetThinking() != nil)
	})

	// Act
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}},
	})

	// Assert
	got := awaitRoster(t, f.d, roster, "turn_died after the query died", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetTurnDied() != nil
	})
	if row := rosterRow(got, f.ws.GetId()); row.GetTurnDied() == nil {
		t.Fatalf("the roster row's status = %T after the query died, want turn_died", row.GetStatus())
	}
}

// bogusTask is a TaskRef naming no task the daemon ever minted.
func bogusTask() *agentreplv1.TaskRef { return &agentreplv1.TaskRef{Id: "no-such-task"} }

// TestUpdateTaskWithABogusTaskRefAnswersUnknownTask covers
// UpdateTaskError.unknown_task: "no task by that id".
func TestUpdateTaskWithABogusTaskRefAnswersUnknownTask(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})

	// Act
	resp, err := d.Client().UpdateTask(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateTaskRequest{
		Task:   bogusTask(),
		Change: &agentreplv1.UpdateTaskRequest_SetDone{SetDone: &agentreplv1.UpdateTaskSetDone{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("UpdateTask on a bogus TaskRef = transport error %v, want the in-band unknown_task arm", err)
	}
	if resp.Msg.GetError().GetUnknownTask() == nil {
		t.Fatalf("UpdateTask on a bogus TaskRef = %v, want UpdateTaskError.unknown_task", resp.Msg)
	}
}

// TestAssignWorkspaceTaskWithABogusTaskRefAnswersUnknownTask covers
// AssignWorkspaceTaskError.unknown_task: "no task by that id".
func TestAssignWorkspaceTaskWithABogusTaskRefAnswersUnknownTask(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})

	// Act
	resp, err := f.d.Client().AssignWorkspaceTask(f.d.Ctx(), connect.NewRequest(&agentreplv1.AssignWorkspaceTaskRequest{
		Workspace: f.ws,
		Task:      bogusTask(),
	}))

	// Assert
	if err != nil {
		t.Fatalf("AssignWorkspaceTask on a bogus TaskRef = transport error %v, want the in-band unknown_task arm", err)
	}
	if resp.Msg.GetError().GetUnknownTask() == nil {
		t.Fatalf("AssignWorkspaceTask on a bogus TaskRef = %v, want AssignWorkspaceTaskError.unknown_task", resp.Msg)
	}
}

// TestUpdateTaskWithABlankTitleAnswersBlankTitle covers UpdateTaskError.blank_title,
// UpdateTask's own landed arm for "the new title is blank once trimmed" — the
// same arm CreateTask carries, minted separately for UpdateTaskSetTitle.
func TestUpdateTaskWithABlankTitleAnswersBlankTitle(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	task := createTask(t, f, "has a title")

	// Act
	resp, err := f.d.Client().UpdateTask(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateTaskRequest{
		Task:   task,
		Change: &agentreplv1.UpdateTaskRequest_SetTitle{SetTitle: &agentreplv1.UpdateTaskSetTitle{Title: "   "}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("UpdateTask{set_title} with a blank title = transport error %v, want the in-band blank_title arm", err)
	}
	if resp.Msg.GetError().GetBlankTitle() == nil {
		t.Fatalf("UpdateTask{set_title} with a blank title = %v, want UpdateTaskError.blank_title", resp.Msg)
	}
}

// rosterStatusName answers which arm of RosterRow.status a row carries, by the
// proto's own field name, so a walk can be recorded and compared as data.
func rosterStatusName(row *frontendv1.RosterRow) string {
	if name, ok := oneofarm.Name(row, "status"); ok {
		return name
	}
	return "<unset>"
}

// TestRosterBringUpWalkIsMonotoneFromAColdSubmit pins the arm walk a workspace
// is published through when its FIRST prompt brings its session up: none ->
// init -> submitting -> thinking, each step no earlier than the one before,
// and never `ready`.
//
// Two defects sat in that walk, both found in a headless run's cold start. The
// rpc mints the turn and parks it under the revival hold, and the roster
// learned of the turn only when the hold was released -- so between the shim's
// SessionStarted and the release it read a live idle session with no turn and
// published `ready` for a workspace whose prompt the daemon had already
// accepted. And the session watcher, born on a connected link, applied the
// client's replayed bring-up `dialing` as a live transition and walked the
// row back to `init` right after `submitting`.
func TestRosterBringUpWalkIsMonotoneFromAColdSubmit(t *testing.T) {
	t.Parallel()
	// Arrange: registered, never opened, the roster watched from before the
	// prompt so every push of the walk is on the stream.
	f := newRegistered(t, harness.Opts{})
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "the registered row", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, f.ws.GetId()) != nil
	})

	// Act: the first prompt revives the workspace, which spawns the fake shim
	// and delivers the prompt once the session is up.
	resp := f.submit("bring the session up", "k-cold-walk", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	if resp.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
		t.Fatalf("SubmitPrompt on a cold workspace = %v, want a minted TurnId", resp)
	}
	shim := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")
	shim.ExpectStartSession()
	shim.ExpectStartTurn()

	// Assert: every arm the row was published through, up to thinking, in
	// the order it was published, with consecutive repeats collapsed.
	var walk []string
	for {
		r := harness.AwaitNext(t, f.d.Ctx(), roster, "the next roster push of the bring-up walk")
		row := rosterRow(r, f.ws.GetId())
		if row == nil {
			continue
		}
		name := rosterStatusName(row)
		if len(walk) == 0 || walk[len(walk)-1] != name {
			walk = append(walk, name)
		}
		if name == "thinking" {
			break
		}
	}
	rank := map[string]int{"none": 0, "init": 1, "submitting": 2, "thinking": 3}
	last := -1
	for _, arm := range walk {
		r, known := rank[arm]
		if !known {
			t.Fatalf("the bring-up walk published %q; the walk is none -> init -> submitting -> thinking, and the whole walk was %v", arm, walk)
		}
		if r < last {
			t.Fatalf("the bring-up walk stepped back to %q; the whole walk was %v", arm, walk)
		}
		last = r
	}
	for _, want := range []string{"init", "submitting"} {
		seen := false
		for _, arm := range walk {
			if arm == want {
				seen = true
			}
		}
		if !seen {
			t.Fatalf("the bring-up walk never published %q; the whole walk was %v", want, walk)
		}
	}
}
