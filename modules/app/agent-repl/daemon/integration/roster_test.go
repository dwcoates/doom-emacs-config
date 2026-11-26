//go:build integration

package integration

import (
	"agentrepl/protohelpers/oneofarm"
	"agentrepl/protohelpers/rosterwalk"
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
	d := viewInGroup(t, groupTabOrder)
	roster := d.WatchRoster()
	harness.AwaitNext(t, d.Ctx(), roster, "the empty roster")
	repo := harness.NewRepo(t)

	// Act
	ws := harness.Register(t, d, repo.Dir)

	// Assert
	got := awaitRoster(t, d, roster, "the new workspace's row", func(r *frontendv1.RosterPush) bool {
		return rosterRepoRow(r, ws.GetId()) != nil
	})
	row := rosterRepoRow(got, ws.GetId())
	if row.GetNone() == nil {
		t.Fatalf("a registered workspace's status = %T, want RosterRowStatusNone before any session", row.GetStatus())
	}
	harness.ExpectNoPush(t, roster, "registering a workspace produces exactly one roster push")
}

func TestRosterStatusFollowsTheSessionLifecycle(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	roster := f.d.WatchRoster()
	statusIs := func(pred func(*frontendv1.RosterRow) bool) func(*frontendv1.RosterPush) bool {
		return func(r *frontendv1.RosterPush) bool {
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
	// waiting for the user to interact with the turn end.
	pushDetachedShell(f.shim, "work-1", "sleep 1")
	awaitRoster(t, f.d, roster, "idle_async with detached work over the turn end",
		statusIs(func(row *frontendv1.RosterRow) bool { return row.GetIdleAsync() != nil && row.GetViewed() == nil }))
	f.d.AwaitWorkspaceLogOperation(f.ws.GetDir(), "daemon.sidebar.async_outranks_turn_end")

	// Act / Assert: the user scrolling the idle_async row's feed moments after
	// the turn ended stamps its interaction recency, and leaves it FULL: an
	// interaction inside the view exemption of a turn's end marks nothing
	// viewed (sidebar.ViewExemption).
	reportScrolled(t, f.d, f.ws)
	awaitRoster(t, f.d, roster, "idle_async, still unread, its recency stamped, after the user scrolled",
		statusIs(func(row *frontendv1.RosterRow) bool {
			return row.GetIdleAsync() != nil && row.GetViewed() == nil && row.GetLastInteracted() != nil
		}))
	f.d.AwaitWorkspaceLogOperation(f.ws.GetDir(), "daemon.sidebar.interaction_view_exempt")
}

// tabBarIDs lists the roster's tabs -- its open rows in the Open section's
// walk, folded ones included -- first tab first.
func tabBarIDs(r *frontendv1.RosterPush) []string {
	var out []string
	for _, open := range rosterwalk.OpenRows(r.GetWorkspaces()) {
		out = append(out, open.GetRow().GetWorkspace().GetWorkspace().GetId())
	}
	return out
}

func TestSettingAPriorityMovesTheWorkspaceSecondToLast(t *testing.T) {
	t.Parallel()
	// Arrange: the bar is a, c, b.
	d := viewInGroup(t, groupTabOrder)
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
	awaitRoster(t, d, roster, "the bar a, b, c with b's name wearing the p05 icon", func(r *frontendv1.RosterPush) bool {
		return sameOrder(tabBarIDs(r), []string{a.GetId(), b.GetId(), c.GetId()}) &&
			rosterRow(r, b.GetId()).GetName().GetPrefix().GetPriority().GetP05() != nil
	})
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

func TestASubsectionMovedBeforeAWorkspaceTakesItsTabsThere(t *testing.T) {
	t.Parallel()
	// Arrange: the owner's example, a, b, c, then [foo: d] at the bottom.
	d := viewInGroup(t, groupTabOrder)
	repo := harness.NewRepo(t)
	roster := d.WatchRoster()
	for _, name := range []string{"a", "b", "c", "d"} {
		harness.Register(t, d, worktreeOf(t, repo, name))
	}
	first := awaitRoster(t, d, roster, "four tabs", func(r *frontendv1.RosterPush) bool { return len(tabBarIDs(r)) == 4 })
	order := tabBarIDs(first)
	refs := map[string]*workspacev1.WorkspaceRef{}
	for _, open := range rosterwalk.OpenRows(first.GetWorkspaces()) {
		refs[open.GetRow().GetWorkspace().GetWorkspace().GetId()] = open.GetRow().GetWorkspace().GetWorkspace()
	}
	created := arrange(t, d, &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_CreateSubsection{
		CreateSubsection: &agentreplv1.ArrangeCreateSubsection{Repository: first.GetWorkspaces().GetRepository().GetSections()[0].GetKey().GetRepository(), Name: "foo"}}}).GetCreated()
	arrange(t, d, &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_PlaceWorkspace{
		PlaceWorkspace: &agentreplv1.ArrangePlaceWorkspace{
			Workspace: refs[order[3]],
			Into:      &agentreplv1.ArrangePlaceWorkspace_Subsection{Subsection: created},
			At:        &agentreplv1.ArrangePlaceWorkspace_End{End: &agentreplv1.ArrangeEnd{}},
		}}})

	// Act: the subsection, dragged onto the second workspace's line.
	arrange(t, d, &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_MoveSubsection{
		MoveSubsection: &agentreplv1.ArrangeMoveSubsection{Subsection: created,
			To: &agentreplv1.ArrangeMoveSubsection_BeforeWorkspace{BeforeWorkspace: refs[order[1]]}}}})

	// Assert: the sidebar draws it between the first two rows, and the tab
	// bar walks its workspace there.
	awaitRoster(t, d, roster, "the subsection between the first two rows", func(r *frontendv1.RosterPush) bool {
		entries := r.GetWorkspaces().GetRepository().GetSections()[0].GetEntries()
		return len(entries) == 4 && entries[1].GetSubsection().GetNamed().GetKey().GetSubsectionId() == created.GetId() &&
			sameOrder(tabBarIDs(r), []string{order[0], order[3], order[1], order[2]})
	})
}

func TestFoldingASubsectionFoldsItsWorkspacesTabs(t *testing.T) {
	t.Parallel()
	// Arrange: a and b in one repository, b filed in a subsection.
	d := viewInGroup(t, groupTabOrder)
	repo := harness.NewRepo(t)
	roster := d.WatchRoster()
	harness.Register(t, d, worktreeOf(t, repo, "a"))
	b := harness.Register(t, d, worktreeOf(t, repo, "b"))
	// b's session is started: a workspace with no session yet is visible under
	// a fold, and this test folds an idle one.
	harness.OpenAndStart(t, d, b)
	first := awaitRoster(t, d, roster, "two tabs", func(r *frontendv1.RosterPush) bool { return len(tabBarIDs(r)) == 2 })
	created := arrange(t, d, &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_CreateSubsection{
		CreateSubsection: &agentreplv1.ArrangeCreateSubsection{Repository: first.GetWorkspaces().GetRepository().GetSections()[0].GetKey().GetRepository(), Name: "later"}}}).GetCreated()
	arrange(t, d, &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_PlaceWorkspace{
		PlaceWorkspace: &agentreplv1.ArrangePlaceWorkspace{
			Workspace: b,
			Into:      &agentreplv1.ArrangePlaceWorkspace_Subsection{Subsection: created},
			At:        &agentreplv1.ArrangePlaceWorkspace_End{End: &agentreplv1.ArrangeEnd{}},
		}}})

	// Act.
	resp, err := d.Client().UpdateSidebarView(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateSidebarViewRequest{
		Change: &agentreplv1.UpdateSidebarViewRequest_FoldSection{FoldSection: &agentreplv1.SidebarViewFoldSection{
			Section: &agentreplv1.SidebarViewFoldSection_Subsection{Subsection: created},
			Fold:    &agentreplv1.SidebarViewFoldSection_Collapse{Collapse: &agentreplv1.SidebarViewCollapse{}},
		}}}))

	// Assert: the subsection is collapsed and b's tab folded, a numbered 1.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateSidebarView = (%v, %v), want success", resp, err)
	}
	awaitRoster(t, d, roster, "the subsection collapsed and b's tab folded", func(r *frontendv1.RosterPush) bool {
		subs := rosterwalk.Subsections(r.GetWorkspaces().GetRepository().GetSections()[0])
		if len(subs) != 1 || subs[0].GetCollapsed() == nil {
			return false
		}
		for _, open := range rosterwalk.OpenRows(r.GetWorkspaces()) {
			id := open.GetRow().GetWorkspace().GetWorkspace().GetId()
			if id == b.GetId() && open.GetTab().GetFolded() == nil {
				return false
			}
			if id != b.GetId() && open.GetTab().GetNumbered().GetNumber() != 1 {
				return false
			}
		}
		return true
	})
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
	f := newCanonicalRegistered(t)
	// The sweep covers every test; the declared records are evidence of a bring-up the test blocks or kills, the shim death the test drives.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.shimclient.exit", "daemon.shimclient.spawn",
		"daemon.workspace.bring_up", "daemon.workspace.restart")
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{ExitOn: harness.ExitOnStartup, ExitCode: 7, Stderr: "boom: fake bring-up death"})
	roster := f.d.WatchRoster()

	// Act
	resp, err := f.startRaw()
	if err != nil {
		t.Fatalf("a start onto a dying shim = transport error %v, want the relaunch_failed arm", err)
	}
	if resp.GetError().GetRelaunchFailed() == nil {
		t.Fatalf("a start onto a dying shim = %v, want RestartWorkspaceError.relaunch_failed", resp)
	}

	// Assert
	got := awaitRoster(t, f.d, roster, "the roster row start_failed after a bring-up death", func(r *frontendv1.RosterPush) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetStartFailed() != nil
	})
	if row := rosterRow(got, f.ws.GetId()); row.GetStartFailed() == nil {
		t.Fatalf("the roster row's status = %T after a bring-up death, want start_failed", row.GetStatus())
	}
}

// ---------------------------------------------------------------------------
// Roster status arms: each of the seven arms below is asserted ON ITS OWN,
// never inside a disjunction with a sibling arm (audit-3 critique 23), driven
// from the real cause internal/resolve/sidebar/status.go's sessionArm and
// mergeArm read facts from.
// ---------------------------------------------------------------------------

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
	f := newCanonicalRegistered(t)
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "the registered row", func(r *frontendv1.RosterPush) bool {
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

func TestTheFirstRosterStatesTheDeployedVersion(t *testing.T) {
	t.Parallel()
	// Arrange
	d := canonicalView(t)

	// Act
	roster := d.WatchRoster()
	first := harness.AwaitNext(t, d.Ctx(), roster, "the first roster")

	// Assert
	if got := first.GetWorkspaces().GetVersion().GetText(); got != harness.PinnedVersion {
		t.Fatalf("the first roster's version = %q, want the pinned checkout's %s", got, harness.PinnedVersion)
	}
}
