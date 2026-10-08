package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

func foldChange(section *agentreplv1.SidebarViewFoldSection) *agentreplv1.UpdateSidebarViewRequest {
	return &agentreplv1.UpdateSidebarViewRequest{
		Change: &agentreplv1.UpdateSidebarViewRequest_FoldSection{FoldSection: section},
	}
}

func collapseFold() *agentreplv1.SidebarViewFoldSection_Collapse {
	return &agentreplv1.SidebarViewFoldSection_Collapse{Collapse: &agentreplv1.SidebarViewCollapse{}}
}

func expandFold() *agentreplv1.SidebarViewFoldSection_Expand {
	return &agentreplv1.SidebarViewFoldSection_Expand{Expand: &agentreplv1.SidebarViewExpand{}}
}

func taskSection(id string) *agentreplv1.SidebarViewFoldSection_Task {
	return &agentreplv1.SidebarViewFoldSection_Task{Task: &agentreplv1.TaskRef{Id: id}}
}

func mergedSection() *agentreplv1.SidebarViewFoldSection_RecentlyMerged {
	return &agentreplv1.SidebarViewFoldSection_RecentlyMerged{RecentlyMerged: &agentreplv1.SidebarViewRecentlyMerged{}}
}

func repositorySection(id string) *agentreplv1.SidebarViewFoldSection_Repository {
	return &agentreplv1.SidebarViewFoldSection_Repository{Repository: &workspacev1.RepositoryRef{Id: id}}
}

func maturitySection(stage *workspacev1.MaturityStage) *agentreplv1.SidebarViewFoldSection_Maturity {
	return &agentreplv1.SidebarViewFoldSection_Maturity{Maturity: stage}
}

func backlogStage() *workspacev1.MaturityStage {
	return &workspacev1.MaturityStage{Stage: &workspacev1.MaturityStage_Backlog{Backlog: &workspacev1.MaturityStageBacklog{}}}
}

func inProgressStage() *workspacev1.MaturityStage {
	return &workspacev1.MaturityStage{Stage: &workspacev1.MaturityStage_InProgress{InProgress: &workspacev1.MaturityStageInProgress{}}}
}

func mergingStage() *workspacev1.MaturityStage {
	return &workspacev1.MaturityStage{Stage: &workspacev1.MaturityStage_Merging{Merging: &workspacev1.MaturityStageMerging{}}}
}

func repositoryGrouping() *agentreplv1.SidebarViewShowGrouping {
	return &agentreplv1.SidebarViewShowGrouping{
		Grouping: &agentreplv1.SidebarViewShowGrouping_Repository{Repository: &agentreplv1.SidebarViewRepositoryGrouping{}}}
}

func taskGrouping() *agentreplv1.SidebarViewShowGrouping {
	return &agentreplv1.SidebarViewShowGrouping{
		Grouping: &agentreplv1.SidebarViewShowGrouping_Task{Task: &agentreplv1.SidebarViewTaskGrouping{}}}
}

func maturityGrouping() *agentreplv1.SidebarViewShowGrouping {
	return &agentreplv1.SidebarViewShowGrouping{
		Grouping: &agentreplv1.SidebarViewShowGrouping_Maturity{Maturity: &agentreplv1.SidebarViewMaturityGrouping{}}}
}

func groupingChange(show *agentreplv1.SidebarViewShowGrouping) *agentreplv1.UpdateSidebarViewRequest {
	return &agentreplv1.UpdateSidebarViewRequest{
		Change: &agentreplv1.UpdateSidebarViewRequest_ShowGrouping{ShowGrouping: show},
	}
}

// sent answers the call's success, failing the test on anything else.
func sent(t *testing.T, h *harness, req *agentreplv1.UpdateSidebarViewRequest) {
	t.Helper()
	resp, err := h.Client.UpdateSidebarView(context.Background(), connect.NewRequest(req))
	if err != nil {
		t.Fatalf("UpdateSidebarView: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
}

func TestUpdateSidebarViewHandsASectionFoldToTheVerb(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.SidebarViewFoldSection
		want sectionFold
	}{
		{"collapse a task", &agentreplv1.SidebarViewFoldSection{Section: taskSection("task-1"), Fold: collapseFold()}, sectionFold{task: "task-1", folded: true}},
		{"expand a task", &agentreplv1.SidebarViewFoldSection{Section: taskSection("task-1"), Fold: expandFold()}, sectionFold{task: "task-1", folded: false}},
		{"expand the merged band", &agentreplv1.SidebarViewFoldSection{Section: mergedSection(), Fold: expandFold()}, sectionFold{merged: true, folded: false}},
		{"collapse the merged band", &agentreplv1.SidebarViewFoldSection{Section: mergedSection(), Fold: collapseFold()}, sectionFold{merged: true, folded: true}},
		{"collapse the backlog stage", &agentreplv1.SidebarViewFoldSection{Section: maturitySection(backlogStage()), Fold: collapseFold()}, sectionFold{stage: wsm.MaturityBacklog, folded: true}},
		{"collapse the in-progress stage", &agentreplv1.SidebarViewFoldSection{Section: maturitySection(inProgressStage()), Fold: collapseFold()}, sectionFold{stage: wsm.MaturityInProgress, folded: true}},
		{"expand the merging stage", &agentreplv1.SidebarViewFoldSection{Section: maturitySection(mergingStage()), Fold: expandFold()}, sectionFold{stage: wsm.MaturityMerging, folded: false}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			sent(t, h, foldChange(tt.req))

			// Assert
			if len(h.Verbs.sectionFolds) != 1 || h.Verbs.sectionFolds[0] != tt.want {
				t.Fatalf("section folds = %+v, want %+v", h.Verbs.sectionFolds, tt.want)
			}
		})
	}
}

func TestUpdateSidebarViewHandsTheResolvedRepositoryFoldToTheVerb(t *testing.T) {
	tests := []struct {
		name string
		fold *agentreplv1.SidebarViewFoldSection
		want repositoryFold
	}{
		{"collapse", &agentreplv1.SidebarViewFoldSection{Section: repositorySection("repo-1"), Fold: collapseFold()}, repositoryFold{repo: "repo-1", folded: true}},
		{"expand", &agentreplv1.SidebarViewFoldSection{Section: repositorySection("repo-1"), Fold: expandFold()}, repositoryFold{repo: "repo-1", folded: false}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.DB.repositories = append(h.DB.repositories, wsm.Repository{ID: "repo-1", Dir: "/repos/one"})

			// Act
			sent(t, h, foldChange(tt.fold))

			// Assert
			if len(h.Verbs.folds) != 1 || h.Verbs.folds[0] != tt.want {
				t.Fatalf("folds = %+v, want %+v", h.Verbs.folds, tt.want)
			}
		})
	}
}

func TestUpdateSidebarViewRefusesAnUnknownRepositoryBeforeTheVerb(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.UpdateSidebarView(context.Background(), connect.NewRequest(
		foldChange(&agentreplv1.SidebarViewFoldSection{Section: repositorySection("repo-nope"), Fold: collapseFold()})))

	// Assert
	if err != nil {
		t.Fatalf("UpdateSidebarView: %v", err)
	}
	if resp.Msg.GetError().GetUnknownRepository() == nil {
		t.Fatalf("result = %v, want unknown_repository", resp.Msg.GetResult())
	}
	if len(h.Verbs.folds) != 0 {
		t.Fatalf("folds = %+v, want none for an unknown repository", h.Verbs.folds)
	}
}

func TestUpdateSidebarViewHandsTheGroupingToTheVerb(t *testing.T) {
	tests := []struct {
		name string
		show *agentreplv1.SidebarViewShowGrouping
		want wsm.Grouping
	}{
		{"task", taskGrouping(), wsm.GroupingTask},
		{"repository", repositoryGrouping(), wsm.GroupingRepository},
		{"maturity", maturityGrouping(), wsm.GroupingMaturity},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			sent(t, h, groupingChange(tt.show))

			// Assert
			if len(h.Verbs.groupings) != 1 || h.Verbs.groupings[0] != tt.want {
				t.Fatalf("groupings = %+v, want [%s]", h.Verbs.groupings, tt.want)
			}
		})
	}
}

func TestUpdateSidebarViewMapsAVerbRefusalOntoItsArm(t *testing.T) {
	tests := []struct {
		name  string
		req   *agentreplv1.UpdateSidebarViewRequest
		arm   string
		check func(*agentreplv1.UpdateSidebarViewError) bool
	}{
		{"unknown task", foldChange(&agentreplv1.SidebarViewFoldSection{Section: taskSection("absent"), Fold: collapseFold()}),
			workspace.ArmUnknownTask, func(e *agentreplv1.UpdateSidebarViewError) bool { return e.GetUnknownTask() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Verbs.sectionFoldErr = &workspace.Refusal{Arm: tt.arm, Reason: "gone", NotFound: true}

			// Act
			resp, err := h.Client.UpdateSidebarView(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil {
				t.Fatalf("UpdateSidebarView: %v", err)
			}
			if !tt.check(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want %s", resp.Msg.GetResult(), tt.arm)
			}
		})
	}
}

func TestUpdateSidebarViewFailsAVerbError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Verbs.sectionFoldErr = errors.New("disk I/O error")

	// Act
	_, err := h.Client.UpdateSidebarView(context.Background(), connect.NewRequest(groupingChange(taskGrouping())))

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("UpdateSidebarView = %v, want an internal failure", err)
	}
}

func TestValidateUpdateSidebarViewRequestRefusesAnIncompleteRequest(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.UpdateSidebarViewRequest
	}{
		{"no change arm", &agentreplv1.UpdateSidebarViewRequest{}},
		{"a fold with no section arm", foldChange(&agentreplv1.SidebarViewFoldSection{Fold: expandFold()})},
		{"a task fold with no ref", foldChange(&agentreplv1.SidebarViewFoldSection{Section: &agentreplv1.SidebarViewFoldSection_Task{}, Fold: expandFold()})},
		{"a task fold with a blank id", foldChange(&agentreplv1.SidebarViewFoldSection{Section: taskSection(""), Fold: expandFold()})},
		{"a repository fold with no ref", foldChange(&agentreplv1.SidebarViewFoldSection{Section: &agentreplv1.SidebarViewFoldSection_Repository{}, Fold: expandFold()})},
		{"a fold with no fold arm", foldChange(&agentreplv1.SidebarViewFoldSection{Section: mergedSection()})},
		{"a maturity fold with no stage arm", foldChange(&agentreplv1.SidebarViewFoldSection{Section: maturitySection(&workspacev1.MaturityStage{}), Fold: expandFold()})},
		{"a grouping with no arm", &agentreplv1.UpdateSidebarViewRequest{
			Change: &agentreplv1.UpdateSidebarViewRequest_ShowGrouping{ShowGrouping: &agentreplv1.SidebarViewShowGrouping{}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := validateUpdateSidebarViewRequest(tt.req)

			// Assert
			if err == nil || err.Code() != connect.CodeInvalidArgument {
				t.Fatalf("validate = %v, want InvalidArgument", err)
			}
		})
	}
}

func TestUpdateSidebarViewRefusesAnInvalidRequestBeforeTheVerb(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.UpdateSidebarView(context.Background(), connect.NewRequest(
		foldChange(&agentreplv1.SidebarViewFoldSection{Section: taskSection(""), Fold: collapseFold()})))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("UpdateSidebarView = %v, want InvalidArgument", err)
	}
	if len(h.Verbs.sectionFolds) != 0 {
		t.Fatalf("section folds = %+v, want none for an invalid request", h.Verbs.sectionFolds)
	}
}
