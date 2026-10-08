//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// updateSidebarView sends one UpdateSidebarView and answers its response,
// failing the test on a transport error.
func updateSidebarView(t *testing.T, f *fixture, req *agentreplv1.UpdateSidebarViewRequest) *agentreplv1.UpdateSidebarViewResponse {
	t.Helper()
	resp, err := f.d.Client().UpdateSidebarView(f.d.Ctx(), connect.NewRequest(req))
	if err != nil {
		t.Fatalf("UpdateSidebarView = error %v, want an in-band answer", err)
	}
	return resp.Msg
}

func foldSection(section *agentreplv1.SidebarViewFoldSection) *agentreplv1.UpdateSidebarViewRequest {
	return &agentreplv1.UpdateSidebarViewRequest{
		Change: &agentreplv1.UpdateSidebarViewRequest_FoldSection{FoldSection: section},
	}
}

func collapseMergedBand() *agentreplv1.UpdateSidebarViewRequest {
	return foldSection(&agentreplv1.SidebarViewFoldSection{
		Section: &agentreplv1.SidebarViewFoldSection_RecentlyMerged{RecentlyMerged: &agentreplv1.SidebarViewRecentlyMerged{}},
		Fold:    &agentreplv1.SidebarViewFoldSection_Collapse{Collapse: &agentreplv1.SidebarViewCollapse{}},
	})
}

func collapseTaskSection(task *agentreplv1.TaskRef) *agentreplv1.UpdateSidebarViewRequest {
	return foldSection(&agentreplv1.SidebarViewFoldSection{
		Section: &agentreplv1.SidebarViewFoldSection_Task{Task: task},
		Fold:    &agentreplv1.SidebarViewFoldSection_Collapse{Collapse: &agentreplv1.SidebarViewCollapse{}},
	})
}

func showTaskGrouping() *agentreplv1.UpdateSidebarViewRequest {
	return &agentreplv1.UpdateSidebarViewRequest{
		Change: &agentreplv1.UpdateSidebarViewRequest_ShowGrouping{ShowGrouping: &agentreplv1.SidebarViewShowGrouping{
			Grouping: &agentreplv1.SidebarViewShowGrouping_Task{Task: &agentreplv1.SidebarViewTaskGrouping{}},
		}},
	}
}

func mergedBandCollapsed(r *frontendv1.WorkspaceRoster) bool {
	return r.GetRecentlyMerged().GetCollapsed() != nil
}

// The band opens expanded until someone folds it (owner ruling, 2026-10-08).
func TestTheRecentlyMergedBandStartsExpanded(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})

	// Act
	first := harness.AwaitNext(t, f.d.Ctx(), f.d.WatchRoster(), "the opening roster")

	// Assert
	if first.GetRecentlyMerged().GetExpanded() == nil {
		t.Fatalf("the band's fold before anyone folded it = %v, want expanded", first.GetRecentlyMerged().GetFold())
	}
}

func TestFoldingTheRecentlyMergedBandReachesEveryRosterSubscriber(t *testing.T) {
	t.Parallel()
	// Arrange: two subscribers, as two workspaces' pages hold.
	f := newRegistered(t, harness.Opts{})
	one := f.d.WatchRoster()
	two := f.d.WatchRosterOn(f.d.Dial())

	// Act
	resp := updateSidebarView(t, f, collapseMergedBand())

	// Assert
	if resp.GetSuccess() == nil {
		t.Fatalf("UpdateSidebarView = %v, want success", resp)
	}
	awaitRoster(t, f.d, one, "the folded band on the first subscriber", mergedBandCollapsed)
	awaitRoster(t, f.d, two, "the folded band on the second subscriber", mergedBandCollapsed)
}

func TestFoldingATaskSectionReachesEveryRosterSubscriber(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	task := createTask(t, f, "fold me")
	one := f.d.WatchRoster()
	two := f.d.WatchRosterOn(f.d.Dial())
	collapsed := func(r *frontendv1.WorkspaceRoster) bool {
		return taskSection(r, task.GetId()).GetCollapsed() != nil
	}

	// Act
	resp := updateSidebarView(t, f, collapseTaskSection(task))

	// Assert
	if resp.GetSuccess() == nil {
		t.Fatalf("UpdateSidebarView = %v, want success", resp)
	}
	awaitRoster(t, f.d, one, "the folded task on the first subscriber", collapsed)
	awaitRoster(t, f.d, two, "the folded task on the second subscriber", collapsed)
}

func TestUpdateSidebarViewWithABogusTaskRefAnswersUnknownTask(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})

	// Act
	resp := updateSidebarView(t, f, collapseTaskSection(&agentreplv1.TaskRef{Id: "no-such-task"}))

	// Assert
	if resp.GetError().GetUnknownTask() == nil {
		t.Fatalf("UpdateSidebarView = %v, want unknown_task", resp)
	}
}

func TestTheSidebarViewSurvivesADaemonRestart(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	task := createTask(t, f, "stays folded")
	roster := f.d.WatchRoster()
	updateSidebarView(t, f, collapseTaskSection(task))
	updateSidebarView(t, f, collapseMergedBand())
	updateSidebarView(t, f, showTaskGrouping())
	awaitRoster(t, f.d, roster, "the whole view, before the restart", func(r *frontendv1.WorkspaceRoster) bool {
		return taskSection(r, task.GetId()).GetCollapsed() != nil && mergedBandCollapsed(r) && r.GetShownTask() != nil
	})

	// Act
	nd := promptRestartDaemon(t, f)

	// Assert
	got := harness.AwaitNext(t, nd.Ctx(), nd.WatchRoster(), "the roster after the restart")
	if taskSection(got, task.GetId()).GetCollapsed() == nil {
		t.Fatalf("task fold after restart = %v, want collapsed", taskSection(got, task.GetId()).GetFold())
	}
	if !mergedBandCollapsed(got) {
		t.Fatalf("band fold after restart = %v, want collapsed", got.GetRecentlyMerged().GetFold())
	}
	if got.GetShownTask() == nil {
		t.Fatalf("grouping after restart = %v, want the task grouping", got.GetShown())
	}
}

func TestTheRosterOpensOnTheRepositoryGrouping(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})

	// Act
	first := harness.AwaitNext(t, f.d.Ctx(), f.d.WatchRoster(), "the opening roster")

	// Assert
	if first.GetShownRepository() == nil {
		t.Fatalf("the grouping before anyone chose one = %v, want the repository grouping", first.GetShown())
	}
}

func TestShowingAGroupingReachesEveryRosterSubscriber(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	one := f.d.WatchRoster()
	two := f.d.WatchRosterOn(f.d.Dial())
	shownTask := func(r *frontendv1.WorkspaceRoster) bool { return r.GetShownTask() != nil }

	// Act
	resp := updateSidebarView(t, f, showTaskGrouping())

	// Assert
	if resp.GetSuccess() == nil {
		t.Fatalf("UpdateSidebarView = %v, want success", resp)
	}
	awaitRoster(t, f.d, one, "the task grouping on the first subscriber", shownTask)
	awaitRoster(t, f.d, two, "the task grouping on the second subscriber", shownTask)
}
