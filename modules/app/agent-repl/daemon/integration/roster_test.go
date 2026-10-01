//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
)

func TestRosterDeliversTheLatestViewToALateSubscriber(t *testing.T) {
	t.Parallel()
	// Arrange: register before anyone is watching.
	f := newRegistered(t, harness.Opts{})

	// Act
	roster := f.d.WatchRoster()
	first := harness.AwaitNext(t, f.d.Ctx(), roster, "the roster a late subscriber opens with")

	// Assert
	if rosterRow(first, f.ws.GetId()) == nil {
		t.Fatalf("the first push a late subscriber received has no row for %s; the subscription invariant delivers the latest view first", f.ws.GetId())
	}
}

func TestTwoRosterSubscribersReceiveIdenticalSequences(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	one := f.d.WatchRoster()
	two := f.d.WatchRosterOn(f.d.Dial())

	// Act
	f.selectWorkspace()

	// Assert
	selected := func(r *frontendv1.WorkspaceRoster) bool {
		return r.GetCurrent().GetWorkspace().GetId() == f.ws.GetId()
	}
	a := awaitRoster(t, f.d, one, "the selection on the first subscriber", selected)
	b := awaitRoster(t, f.d, two, "the selection on the second subscriber", selected)
	if !proto.Equal(a, b) {
		t.Fatalf("two subscribers saw different rosters:\nfirst:  %v\nsecond: %v", a, b)
	}
}

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

	// Act / Assert: the unread result holds the row on done while detached
	// work runs.
	// The row does not move, so no roster push comes: the roster's own
	// record of the decision is what proves the work was weighed and the
	// unread result kept the row on done.
	pushDetachedShell(f.shim, "work-1", "sleep 1")
	f.d.AwaitWorkspaceLogOperation(f.ws.GetDir(), "daemon.sidebar.unread_outranks_async")

	// Act / Assert: idle_async, FULL, once the user has viewed the result.
	markViewed(t, f.d, f.ws)
	awaitRoster(t, f.d, roster, "idle_async with detached work and the result read",
		statusIs(func(row *frontendv1.RosterRow) bool { return row.GetIdleAsync() != nil && row.GetViewed() == nil }))
}

func TestRosterOrdersRowsByPriority(t *testing.T) {
	t.Parallel()
	// Arrange: four workspaces in one repository, plus one left unprioritized.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	roster := d.WatchRoster()
	refs := map[string]*workspacev1.WorkspaceRef{}
	for _, name := range []string{"p05", "p1", "p2", "p3", "plain"} {
		wt := worktreeOf(t, repo, name)
		refs[name] = harness.Register(t, d, wt)
	}
	priorities := map[string]*agentreplv1.WorkspacePriority{
		"p05": {Level: &agentreplv1.WorkspacePriority_P05{P05: &agentreplv1.WorkspacePriorityP05{}}},
		"p1":  {Level: &agentreplv1.WorkspacePriority_P1{P1: &agentreplv1.WorkspacePriorityP1{}}},
		"p2":  {Level: &agentreplv1.WorkspacePriority_P2{P2: &agentreplv1.WorkspacePriorityP2{}}},
		"p3":  {Level: &agentreplv1.WorkspacePriority_P3{P3: &agentreplv1.WorkspacePriorityP3{}}},
	}

	// Act
	for name, p := range priorities {
		setPriority(t, d, refs[name], p)
	}

	// Assert. The predicate waits for the BADGES as well as the order: these
	// five names sort into the wanted order alphabetically too, so an order
	// check alone is satisfied by the roster from before any priority was set.
	want := []string{refs["p05"].GetId(), refs["p1"].GetId(), refs["p2"].GetId(), refs["p3"].GetId(), refs["plain"].GetId()}
	got := awaitRoster(t, d, roster, "the priority ordering P05 < P1 < P2 < P3 < unprioritized", func(r *frontendv1.WorkspaceRoster) bool {
		if !sameOrder(repoRowIDs(r), want) {
			return false
		}
		for name := range priorities {
			if rosterRow(r, refs[name].GetId()).GetPriority().GetLabel() == "" {
				return false
			}
		}
		return true
	})
	for name := range priorities {
		row := rosterRow(got, refs[name].GetId())
		if row.GetPriority().GetLabel() == "" {
			t.Fatalf("row %s carries no priority badge label, want the level's label", name)
		}
	}
	if row := rosterRow(got, refs["plain"].GetId()); row.GetPriority() != nil {
		t.Fatalf("the unprioritized row carries a badge %v, want none", row.GetPriority())
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

// TestRecentlyMergedListsAMergedWorkspaceWithItsMergeInstant covers the
// roster's durable half moving on a LANDED merge.
//
// It is built on the merge tests' own fixture (mergeCleanRepo), and
// deliberately so: a merge only lands for a workspace the daemon CREATED
// (its layout facts are what the merge reads its target and brief from), and
// a bare `RegisterWorkspace` on a hand-made worktree is refused for exactly
// that reason — which is what
// TestMergeWorkspaceOnAWorkspaceWithoutLayoutFactsIsRefused asserts.
//
// mergeCleanRepo's workspace is NOT such a bare registration: mergeCreateChild
// mints it through the real CreateWorkspace rpc, which writes the
// wsm.CreationJob carrying Layout.SourceBranch / SourceDir / TargetDir —
// internal/merge/orchestrator.go's layoutFor (queue.go's Enqueue calls it
// first) reads exactly that job, finds it, and returns it with no refusal.
// TestTheTestGatePassingSettlesTheTestsTab proves the same fixture's merge
// lands cleanly end to end, so no daemon.merge.enqueue WARN is ever produced
// on this path — that expectation was phantom and is dropped.
func TestRecentlyMergedListsAMergedWorkspaceWithItsMergeInstant(t *testing.T) {
	t.Parallel()
	// Arrange
	f, d, _, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	roster := d.WatchRoster()

	// Act
	harness.CommitWork(t, f.ws.GetDir())
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws, Source: harness.OwnBranch(false)})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// Assert
	got := awaitRoster(t, d, roster, "the merged workspace under recently_merged", func(r *frontendv1.WorkspaceRoster) bool {
		for _, row := range r.GetRecentlyMerged().GetRows().GetRows() {
			if row.GetWorkspace().GetWorkspace().GetId() == f.ws.GetId() {
				return true
			}
		}
		return false
	})
	var row *frontendv1.RosterRow
	for _, r := range got.GetRecentlyMerged().GetRows().GetRows() {
		if r.GetWorkspace().GetWorkspace().GetId() == f.ws.GetId() {
			row = r
		}
	}
	if row.GetWhen().GetMerged().GetAtMs() == 0 {
		t.Fatalf("the recently merged row's when = %v, want when.merged stamped", row.GetWhen())
	}
	// internal/resolve/sidebar/rows.go's recedes() greys a row on ANY of three
	// settled ends, one being rec.MergedAt != nil — a merged row's closed.closed
	// is true for that reason, not because the workspace's editor was closed.
	if !row.GetClosed().GetClosed() {
		t.Fatalf("the recently merged row's closed.closed = false, want true (recedes() on rec.MergedAt != nil)")
	}
}

func TestTaskViewGroupsAssignedWorkspaces(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	roster := f.d.WatchRoster()
	created, err := f.d.Client().CreateTask(f.d.Ctx(), connect.NewRequest(&agentreplv1.CreateTaskRequest{Title: "land the rebuild"}))
	if err != nil {
		t.Fatalf("CreateTask = error %v, want a task ref", err)
	}
	task := created.Msg.GetSuccess().GetTask()
	if task.GetId() == "" {
		t.Fatalf("CreateTask = %v, want a minted task id", created.Msg)
	}

	// Act
	if _, err := f.d.Client().AssignWorkspaceTask(f.d.Ctx(), connect.NewRequest(&agentreplv1.AssignWorkspaceTaskRequest{Workspace: f.ws, Task: task})); err != nil {
		t.Fatalf("AssignWorkspaceTask = error %v, want a success", err)
	}

	// Assert
	got := awaitRoster(t, f.d, roster, "the workspace grouped under its task", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterTaskRow(r, task.GetId(), f.ws.GetId()) != nil
	})
	section := taskSection(got, task.GetId())
	if section.GetHeader().GetLabel().GetText() != "land the rebuild" {
		t.Fatalf("task section label = %q, want the task's title", section.GetHeader().GetLabel().GetText())
	}
	if section.GetHeader().GetDone().GetDone() {
		t.Fatalf("a fresh task section's done check = true, want false")
	}
}

func TestMarkingATaskDoneChecksItsSection(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	task := createTask(t, f, "finish it")
	assignTask(t, f, task)
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "the task section", func(r *frontendv1.WorkspaceRoster) bool {
		return taskSection(r, task.GetId()) != nil
	})

	// Act
	if _, err := f.d.Client().UpdateTask(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateTaskRequest{
		Task:   task,
		Change: &agentreplv1.UpdateTaskRequest_SetDone{SetDone: &agentreplv1.UpdateTaskSetDone{}},
	})); err != nil {
		t.Fatalf("UpdateTask{set_done} = error %v, want a success", err)
	}

	// Assert
	awaitRoster(t, f.d, roster, "the task section's done check", func(r *frontendv1.WorkspaceRoster) bool {
		s := taskSection(r, task.GetId())
		return s != nil && s.GetHeader().GetDone().GetDone()
	})
}

func TestUnassigningReturnsTheRowToTheRepositoryGrouping(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	task := createTask(t, f, "temporary")
	assignTask(t, f, task)
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "the row under its task", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterTaskRow(r, task.GetId(), f.ws.GetId()) != nil
	})

	// Act
	if _, err := f.d.Client().AssignWorkspaceTask(f.d.Ctx(), connect.NewRequest(&agentreplv1.AssignWorkspaceTaskRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("AssignWorkspaceTask{unassign} = error %v, want a success", err)
	}

	// Assert
	awaitRoster(t, f.d, roster, "the row back under its repository", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterTaskRow(r, task.GetId(), f.ws.GetId()) == nil && rosterRepoRow(r, f.ws.GetId()) != nil
	})
}

// TestCreateTaskRefusesABlankTitle asserts the refusal IN BAND.
// `CreateTaskError.blank_title` is a LANDED arm, spelled for exactly this
// condition ("The title is blank once trimmed"), so the refusal is the
// response's `error` result and not a Connect error.
//
// endpoint_create_task.proto also carries the boilerplate line that a blank
// string is InvalidArgument — the same sentence appears in 35 endpoint files.
// The specific arm decides over the generic header: an arm minted for this
// condition is not an arm with no producer.
func TestCreateTaskRefusesABlankTitle(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	// blank_title is a LANDED CreateTaskError arm (see the comment above), so
	// it is answered in band at DEBUG through server.refuse — never the
	// daemon.refusal.unlanded_arm WARN that path was originally written
	// against.

	// Act
	resp, err := d.Client().CreateTask(d.Ctx(), connect.NewRequest(&agentreplv1.CreateTaskRequest{Title: "   "}))

	// Assert
	if err != nil {
		t.Fatalf("CreateTask with a blank title = transport error %v, want the in-band blank_title arm", err)
	}
	if resp.Msg.GetError().GetBlankTitle() == nil {
		t.Fatalf("CreateTask with a blank title = %v, want CreateTaskError.blank_title", resp.Msg)
	}
}

// TestKilledWorkspaceRowCarriesClosedTrue covers the OTHER of the three
// settled ends internal/resolve/sidebar/rows.go's recedes() greys a row for: a
// KillWorkspace-terminated session (session.Terminal.Kind == "killed"), not an
// editor-closed or merged workspace. TestKillWorkspaceForceKillsTheSessionAndReapsTheShim
// (session_lifecycle_test.go) already asserts the row's status arm is `dead`;
// this test is the roster suite's own coverage of the ORTHOGONAL closed.closed
// receding flag on that same row.
func TestKilledWorkspaceRowCarriesClosedTrue(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.kill")
	f.shim.ExpectStartSession()
	roster := f.d.WatchRoster()

	// Act
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}

	// Assert
	got := awaitRoster(t, f.d, roster, "the killed workspace's row receded", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetClosed().GetClosed()
	})
	if row := rosterRow(got, f.ws.GetId()); !row.GetClosed().GetClosed() {
		t.Fatalf("a killed workspace's row.closed.closed = false, want true")
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

// TestUpdateTaskSetTitleRelabelsTheTaskSectionHeader covers UpdateTaskSetTitle
// against RosterTaskSectionHeader.label (frontend/v1/sidebar.proto), the
// section header field the header's title text is drawn from.
func TestUpdateTaskSetTitleRelabelsTheTaskSectionHeader(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	task := createTask(t, f, "old title")
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "the task section under its old title", func(r *frontendv1.WorkspaceRoster) bool {
		return taskSection(r, task.GetId()).GetHeader().GetLabel().GetText() == "old title"
	})

	// Act
	if _, err := f.d.Client().UpdateTask(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateTaskRequest{
		Task:   task,
		Change: &agentreplv1.UpdateTaskRequest_SetTitle{SetTitle: &agentreplv1.UpdateTaskSetTitle{Title: "new title"}},
	})); err != nil {
		t.Fatalf("UpdateTask{set_title} = error %v, want a success", err)
	}

	// Assert
	got := awaitRoster(t, f.d, roster, "the task section relabeled", func(r *frontendv1.WorkspaceRoster) bool {
		return taskSection(r, task.GetId()).GetHeader().GetLabel().GetText() == "new title"
	})
	if label := taskSection(got, task.GetId()).GetHeader().GetLabel().GetText(); label != "new title" {
		t.Fatalf("task section label after set_title = %q, want %q", label, "new title")
	}
}

// TestUpdateTaskSetOpenUnchecksADoneTask covers UpdateTaskSetOpen reversing a
// prior UpdateTaskSetDone.
func TestUpdateTaskSetOpenUnchecksADoneTask(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	task := createTask(t, f, "flip me")
	roster := f.d.WatchRoster()
	if _, err := f.d.Client().UpdateTask(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateTaskRequest{
		Task:   task,
		Change: &agentreplv1.UpdateTaskRequest_SetDone{SetDone: &agentreplv1.UpdateTaskSetDone{}},
	})); err != nil {
		t.Fatalf("UpdateTask{set_done} = error %v, want a success", err)
	}
	awaitRoster(t, f.d, roster, "the task section done", func(r *frontendv1.WorkspaceRoster) bool {
		return taskSection(r, task.GetId()).GetHeader().GetDone().GetDone()
	})

	// Act
	if _, err := f.d.Client().UpdateTask(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateTaskRequest{
		Task:   task,
		Change: &agentreplv1.UpdateTaskRequest_SetOpen{SetOpen: &agentreplv1.UpdateTaskSetOpen{}},
	})); err != nil {
		t.Fatalf("UpdateTask{set_open} = error %v, want a success", err)
	}

	// Assert
	got := awaitRoster(t, f.d, roster, "the task section reopened", func(r *frontendv1.WorkspaceRoster) bool {
		return !taskSection(r, task.GetId()).GetHeader().GetDone().GetDone()
	})
	if done := taskSection(got, task.GetId()).GetHeader().GetDone().GetDone(); done {
		t.Fatalf("task section done after set_open = %v, want false", done)
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

// TestRosterRowIsSubmittingBeforeTheShimAcksTheTurn covers
// RosterRowStatusSubmitting: the turn is accepted and the shim has not yet
// produced any activity for it (internal/resolve/sidebar/status.go's
// sessionArm, `s.turn != nil && !s.sawActivity`).
func TestRosterRowIsSubmittingBeforeTheShimAcksTheTurn(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "ready before any turn", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetReady() != nil
	})

	// Act: StartTurn is visible in the roster before the fake shim has
	// produced any activity for it.
	resp := f.submit("do it", "k-submitting", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	if resp.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
		t.Fatalf("SubmitPrompt = %v, want a minted TurnId", resp)
	}
	f.shim.ExpectStartTurn()

	// Assert
	got := awaitRoster(t, f.d, roster, "submitting before the first activity", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetSubmitting() != nil
	})
	if row := rosterRow(got, f.ws.GetId()); row.GetSubmitting() == nil {
		t.Fatalf("the roster row's status = %T before any activity, want submitting", row.GetStatus())
	}
}

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

// TestRosterRowIsInterruptedAfterTheAgentAcknowledgesAUserStop covers
// RosterRowStatusInterrupted: the agent's own terminal frame acknowledges a
// user stop (support_test.go's interruptedFrame), which closes the turn as
// wsm.CloseKilled (internal/sessionwatcher/route.go's turnCloseOf) --
// footer_topbar_test.go's TestFooterInterruptedStatusIsRetiredByADaemonSideDwell
// drives the identical frame for the footer's own (transient) arm; the
// roster's arm persists, so no dwell-wait is needed here.
func TestRosterRowIsInterruptedAfterTheAgentAcknowledgesAUserStop(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	roster := f.d.WatchRoster()
	f.submit("do it", "k-interrupted", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	awaitRoster(t, f.d, roster, "submitting before the interrupt", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetSubmitting() != nil
	})

	// Act
	f.shim.PushAgentFrame(mainAgent, interruptedFrame(mainAgent))

	// Assert
	got := awaitRoster(t, f.d, roster, "interrupted after the agent's acknowledgement", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetInterrupted() != nil
	})
	if row := rosterRow(got, f.ws.GetId()); row.GetInterrupted() == nil {
		t.Fatalf("the roster row's status = %T after the agent's interrupted terminal, want interrupted", row.GetStatus())
	}
}

// TestRosterRowIsDegradedWhileASessionDiagnosticsWindowIsOpen covers
// RosterRowStatusDegraded: an open SessionDegradedWindow in the diagnostics
// push (footer_topbar_test.go's TestTopbarDegradedWindowIsDrawnOpenThenClosed
// drives the topbar's own reading of the identical push).
func TestRosterRowIsDegradedWhileASessionDiagnosticsWindowIsOpen(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "ready before the degraded window", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetReady() != nil
	})

	// Act
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{Diagnostics: &conversationv1.SessionDiagnostics{
			Health: &conversationv1.SessionDiagnostics_Healthy{Healthy: &conversationv1.SessionHealthy{}},
			DegradedWindows: []*conversationv1.SessionDegradedWindow{{
				Component: "converter",
				Reason:    "backlogged",
				BeganAtMs: 1_700_000_000_000,
				Extent:    &conversationv1.SessionDegradedWindow_Open{Open: &conversationv1.SessionDegradedOpen{}},
			}},
		}},
	})

	// Assert
	got := awaitRoster(t, f.d, roster, "degraded with the window open", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetDegraded() != nil
	})
	if row := rosterRow(got, f.ws.GetId()); row.GetDegraded() == nil {
		t.Fatalf("the roster row's status = %T with an open degraded window, want degraded", row.GetStatus())
	}
}

// TestRosterRowIsTurnFailedWhenTheQueryDiesUnderATurn covers a dead query as
// what the owner ruled it (2026-09-28): a FAILED TURN, drawn `turn_failed`,
// never `vendor_blocked` — nothing about the vendor or the account refuses the
// session, and the next prompt restarts the query. The watcher closes the
// turn the death cut as failed (internal/resolve/sidebar/status_test.go's
// TestAQueryDeathDoesNotBlockTheRow is the unit-level half).
func TestRosterRowIsTurnFailedWhenTheQueryDiesUnderATurn(t *testing.T) {
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
	got := awaitRoster(t, f.d, roster, "turn_failed after the query died", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetTurnFailed() != nil
	})
	if row := rosterRow(got, f.ws.GetId()); row.GetTurnFailed() == nil {
		t.Fatalf("the roster row's status = %T after the query died, want turn_failed", row.GetStatus())
	}
}

// TestRosterRowIsMergeFailedWhenTheMergeGitCommandFails covers
// RosterRowStatusMergeFailed: a genuine (non-conflict) error that gives up
// the run (internal/merge/phases.go's attempt, then abort() ->
// internal/merge/terminal.go's StateFailed) -- as opposed to
// merge_conflict/parked, which a scripted conflict or a test-gate escalation
// produce instead (merge_test.go's own tests). The scripted git failure is
// the harness's repo.ScriptFailure. (It was the landed-range read that
// failed here once; that read now follows a landing that has HAPPENED, so
// its failure reads merged, not failed -- internal/merge's
// TestALandingWhoseRangeWillNotReadStillConcludesAsMerged.)
func TestRosterRowIsMergeFailedWhenTheMergeGitCommandFails(t *testing.T) {
	t.Parallel()
	// Arrange: a clean self-repo merge (mergeCleanRepo, merge_test.go) whose
	// no-ff merge, made in the queue's own tree, git refuses outright.
	f, d, repo, _ := mergeCleanRepo(t)
	repo.ScriptFailure("", 1, "fatal: refusing to merge unrelated histories", "merge")
	roster := d.WatchRoster()
	d.ExpectWarnings("daemon.gitclient.merge_no_ff", "daemon.merge.merge_tab", "daemon.merge.abort")

	// Act
	harness.CommitWork(t, f.ws.GetDir())
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws, Source: harness.OwnBranch(false)})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// Assert
	got := awaitRoster(t, d, roster, "merge_failed after the merge git command fails", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetMergeFailed() != nil
	})
	if row := rosterRow(got, f.ws.GetId()); row.GetMergeFailed() == nil {
		t.Fatalf("the roster row's status = %T after a genuine (non-conflict) merge failure, want merge_failed", row.GetStatus())
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

// TestTasksAndWorkspaceAssignmentsSurviveADaemonRestart is the durability half
// of the task view: tasks and AssignWorkspaceTask rows are WSM-owned per
// SPEC.md's "Tasks are daemon-owned rows in WSM", so a restart on the same
// state root must reload them rather than starting the task view over empty.
func TestTasksAndWorkspaceAssignmentsSurviveADaemonRestart(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	task := createTask(t, f, "outlives the daemon")
	assignTask(t, f, task)
	if _, err := f.d.Client().UpdateTask(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateTaskRequest{
		Task:   task,
		Change: &agentreplv1.UpdateTaskRequest_SetDone{SetDone: &agentreplv1.UpdateTaskSetDone{}},
	})); err != nil {
		t.Fatalf("UpdateTask{set_done} = error %v, want a success", err)
	}
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "the workspace grouped under its done task, before the restart", func(r *frontendv1.WorkspaceRoster) bool {
		s := taskSection(r, task.GetId())
		return s != nil && s.GetHeader().GetDone().GetDone() && rosterTaskRow(r, task.GetId(), f.ws.GetId()) != nil
	})

	// Act: restart the daemon on the same state root, per prompt_test.go's
	// promptRestartDaemon idiom — it updates f.d in place.
	nd := promptRestartDaemon(t, f)

	// Assert
	after := nd.WatchRoster()
	got := harness.AwaitNext(t, nd.Ctx(), after, "the roster after the restart")
	section := taskSection(got, task.GetId())
	if section == nil {
		t.Fatalf("no task section for %s survived the restart", task.GetId())
	}
	if section.GetHeader().GetLabel().GetText() != "outlives the daemon" {
		t.Fatalf("task section label after restart = %q, want %q", section.GetHeader().GetLabel().GetText(), "outlives the daemon")
	}
	if !section.GetHeader().GetDone().GetDone() {
		t.Fatalf("task section done after restart = false, want true (the done mark must survive too)")
	}
	if rosterTaskRow(got, task.GetId(), f.ws.GetId()) == nil {
		t.Fatalf("the workspace's assignment to %s did not survive the restart", task.GetId())
	}
}

// rosterStatusName answers which arm of RosterRow.status a row carries, by the
// proto's own field name, so a walk can be recorded and compared as data.
func rosterStatusName(row *frontendv1.RosterRow) string {
	m := row.ProtoReflect()
	oneof := m.Descriptor().Oneofs().ByName("status")
	if field := m.WhichOneof(oneof); field != nil {
		return string(field.Name())
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
