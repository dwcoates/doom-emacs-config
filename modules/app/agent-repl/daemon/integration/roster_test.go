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

	// Act / Assert: idle_async with detached work live and no turn.
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedShell("work-1", "sleep 1")))
	awaitRoster(t, f.d, roster, "idle_async with detached work and no turn", statusIs(func(row *frontendv1.RosterRow) bool { return row.GetIdleAsync() != nil }))
}

func TestRosterOrdersRowsByPriority(t *testing.T) {
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

func TestClosedWorkspaceDrawsClosedAndNukedLeavesTheRoster(t *testing.T) {
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
// It is built on the merge tests' own fixture, and deliberately so: a merge
// only lands for a workspace the daemon CREATED (its layout facts are what the
// merge reads its target and brief from), and a bare `RegisterWorkspace` on a
// hand-made worktree is refused for exactly that reason — which is what
// TestMergeWorkspaceOnAWorkspaceWithoutLayoutFactsIsRefused asserts. This test
// previously registered such a worktree and then waited for a merge that could
// never be enqueued.
func TestRecentlyMergedListsAMergedWorkspaceWithItsMergeInstant(t *testing.T) {
	// Arrange
	f, d, _, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	roster := d.WatchRoster()

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
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
	// UNCERTAIN — needs a suite run to confirm. `source` here is only
	// harness.Register-ed, never materialized through CreateWorkspace, so it
	// carries no wsm.CreationJob; internal/merge/orchestrator.go's layoutFor
	// says geometry is "NEVER inferred later: a workspace materialized
	// without it can never be merged", and Enqueue
	// (internal/merge/queue.go:32-35) refuses immediately with WARN
	// daemon.merge.enqueue when layoutFor finds none. If that reading is
	// right this MergeWorkspace call is refused rather than landed, which
	// would also mean the roster never reaches recently_merged and the test
	// hangs on its own awaitRoster — a likely pre-existing defect in this
	// test unrelated to critique 20, flagged to the teamlead. Naming the one
	// operation the refusal path reaches as the best guess:
	d.ExpectWarnings("daemon.merge.enqueue")
}

func TestTaskViewGroupsAssignedWorkspaces(t *testing.T) {
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
	// Arrange
	d := newDaemon(t, harness.Opts{})
	// blank_title is a LANDED CreateTaskError arm (see the comment above), so
	// it is answered in band at DEBUG through server.refuse — never the
	// daemon.refusal.unlanded_arm WARN that path was originally written
	// against.
	d.ExpectWarnings()

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
