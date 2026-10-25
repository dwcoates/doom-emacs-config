package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
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

func mergedSection() *agentreplv1.SidebarViewFoldSection_RecentlyMerged {
	return &agentreplv1.SidebarViewFoldSection_RecentlyMerged{RecentlyMerged: &agentreplv1.SidebarViewRecentlyMerged{}}
}

func hiddenSection() *agentreplv1.SidebarViewFoldSection_Hidden {
	return &agentreplv1.SidebarViewFoldSection_Hidden{Hidden: &agentreplv1.SidebarViewHidden{}}
}

func openSection() *agentreplv1.SidebarViewFoldSection_Open {
	return &agentreplv1.SidebarViewFoldSection_Open{Open: &agentreplv1.SidebarViewOpen{}}
}

func repositorySection(id string) *agentreplv1.SidebarViewFoldSection_Repository {
	return &agentreplv1.SidebarViewFoldSection_Repository{Repository: &workspacev1.RepositoryRef{Id: id}}
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
		{"expand the merged band", &agentreplv1.SidebarViewFoldSection{Section: mergedSection(), Fold: expandFold()}, sectionFold{merged: true, folded: false}},
		{"collapse the merged band", &agentreplv1.SidebarViewFoldSection{Section: mergedSection(), Fold: collapseFold()}, sectionFold{merged: true, folded: true}},
		{"collapse the hidden section", &agentreplv1.SidebarViewFoldSection{Section: hiddenSection(), Fold: collapseFold()}, sectionFold{hidden: true, folded: true}},
		{"expand the hidden section", &agentreplv1.SidebarViewFoldSection{Section: hiddenSection(), Fold: expandFold()}, sectionFold{hidden: true, folded: false}},
		{"collapse the open section", &agentreplv1.SidebarViewFoldSection{Section: openSection(), Fold: collapseFold()}, sectionFold{open: true, folded: true}},
		{"expand the open section", &agentreplv1.SidebarViewFoldSection{Section: openSection(), Fold: expandFold()}, sectionFold{open: true, folded: false}},
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

func showOpenChange(shows *agentreplv1.SidebarViewShowOpen) *agentreplv1.UpdateSidebarViewRequest {
	return &agentreplv1.UpdateSidebarViewRequest{
		Change: &agentreplv1.UpdateSidebarViewRequest_ShowOpen{ShowOpen: shows},
	}
}

func TestUpdateSidebarViewHandsWhatTheOpenSectionShowsToTheVerb(t *testing.T) {
	tests := []struct {
		name  string
		shows *agentreplv1.SidebarViewShowOpen
		want  bool
	}{
		{"merge queues", &agentreplv1.SidebarViewShowOpen{Shows: &agentreplv1.SidebarViewShowOpen_MergeQueues{MergeQueues: &agentreplv1.SidebarViewShowMergeQueues{}}}, true},
		{"workspaces", &agentreplv1.SidebarViewShowOpen{Shows: &agentreplv1.SidebarViewShowOpen_Workspaces{Workspaces: &agentreplv1.SidebarViewShowWorkspaces{}}}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			sent(t, h, showOpenChange(tt.shows))

			// Assert
			if len(h.Verbs.showOpen) != 1 || h.Verbs.showOpen[0] != tt.want {
				t.Fatalf("open section shows = %v, want [%v]", h.Verbs.showOpen, tt.want)
			}
			if len(h.Verbs.sectionFolds) != 0 {
				t.Fatalf("section folds = %+v, want none for a view change", h.Verbs.sectionFolds)
			}
		})
	}
}

func TestUpdateSidebarViewFailsAShowOpenVerbError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Verbs.sectionFoldErr = errors.New("disk I/O error")

	// Act
	_, err := h.Client.UpdateSidebarView(context.Background(), connect.NewRequest(showOpenChange(
		&agentreplv1.SidebarViewShowOpen{Shows: &agentreplv1.SidebarViewShowOpen_MergeQueues{MergeQueues: &agentreplv1.SidebarViewShowMergeQueues{}}})))

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("UpdateSidebarView = %v, want an internal failure", err)
	}
}

func TestUpdateSidebarViewRefusesAShowOpenWithNoArmBeforeTheVerb(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.UpdateSidebarView(context.Background(), connect.NewRequest(showOpenChange(&agentreplv1.SidebarViewShowOpen{})))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("UpdateSidebarView = %v, want InvalidArgument", err)
	}
	if len(h.Verbs.showOpen) != 0 {
		t.Fatalf("open section shows = %v, want none for an invalid request", h.Verbs.showOpen)
	}
}

func TestUpdateSidebarViewHandsTheResolvedRepositoryFoldToTheVerb(t *testing.T) {
	tests := []struct {
		name string
		fold *agentreplv1.SidebarViewFoldSection
		want repositoryFold
	}{
		{"collapse", &agentreplv1.SidebarViewFoldSection{Section: repositorySection("repo-1"), Fold: collapseFold()}, repositoryFold{repo: "repo-1", folded: true, requester: workspace.FoldRequester{RPC: "UpdateSidebarView"}}},
		{"expand", &agentreplv1.SidebarViewFoldSection{Section: repositorySection("repo-1"), Fold: expandFold()}, repositoryFold{repo: "repo-1", folded: false, requester: workspace.FoldRequester{RPC: "UpdateSidebarView"}}},
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

func TestARepositoryFoldNamesTheRequestIDItsClientSent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.DB.repositories = append(h.DB.repositories, wsm.Repository{ID: "repo-1", Dir: "/repos/one"})
	req := connect.NewRequest(foldChange(&agentreplv1.SidebarViewFoldSection{Section: repositorySection("repo-1"), Fold: collapseFold()}))
	req.Header().Set(requestIDHeader, "webhost-c1-7")

	// Act
	if _, err := h.Client.UpdateSidebarView(context.Background(), req); err != nil {
		t.Fatalf("UpdateSidebarView: %v", err)
	}

	// Assert
	want := workspace.FoldRequester{RPC: "UpdateSidebarView", RequestID: "webhost-c1-7"}
	if len(h.Verbs.folds) != 1 || h.Verbs.folds[0].requester != want {
		t.Fatalf("folds = %+v, want one asked by %+v", h.Verbs.folds, want)
	}
}

func TestASubsectionFoldNamesTheRequestIDItsClientSent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	req := connect.NewRequest(foldChange(&agentreplv1.SidebarViewFoldSection{Section: subsectionSection("sub-1"), Fold: collapseFold()}))
	req.Header().Set(requestIDHeader, "webhost-c1-8")

	// Act
	if _, err := h.Client.UpdateSidebarView(context.Background(), req); err != nil {
		t.Fatalf("UpdateSidebarView: %v", err)
	}

	// Assert
	if want := `FoldSubsection sub-1 true by UpdateSidebarView "webhost-c1-8"`; len(h.Verbs.arrangements) != 1 || h.Verbs.arrangements[0] != want {
		t.Fatalf("arrangements = %q, want %q", h.Verbs.arrangements, want)
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

func TestUpdateSidebarViewFailsAVerbError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Verbs.sectionFoldErr = errors.New("disk I/O error")

	// Act
	_, err := h.Client.UpdateSidebarView(context.Background(), connect.NewRequest(foldChange(&agentreplv1.SidebarViewFoldSection{Section: mergedSection(), Fold: collapseFold()})))

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
		{"a repository fold with no ref", foldChange(&agentreplv1.SidebarViewFoldSection{Section: &agentreplv1.SidebarViewFoldSection_Repository{}, Fold: expandFold()})},
		{"a fold with no fold arm", foldChange(&agentreplv1.SidebarViewFoldSection{Section: mergedSection()})},
		{"a show_open with no shows arm", showOpenChange(&agentreplv1.SidebarViewShowOpen{})},
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
		foldChange(&agentreplv1.SidebarViewFoldSection{Section: &agentreplv1.SidebarViewFoldSection_Repository{}, Fold: collapseFold()})))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("UpdateSidebarView = %v, want InvalidArgument", err)
	}
	if len(h.Verbs.sectionFolds) != 0 {
		t.Fatalf("section folds = %+v, want none for an invalid request", h.Verbs.sectionFolds)
	}
}

func subsectionSection(id string) *agentreplv1.SidebarViewFoldSection_Subsection {
	return &agentreplv1.SidebarViewFoldSection_Subsection{Subsection: &agentreplv1.SubsectionRef{Id: id}}
}

func TestUpdateSidebarViewHandsASubsectionFoldToTheVerb(t *testing.T) {
	tests := []struct {
		name string
		fold *agentreplv1.SidebarViewFoldSection
		want string
	}{
		{"collapse", &agentreplv1.SidebarViewFoldSection{Section: subsectionSection("sub-1"), Fold: collapseFold()}, `FoldSubsection sub-1 true by UpdateSidebarView ""`},
		{"expand", &agentreplv1.SidebarViewFoldSection{Section: subsectionSection("sub-1"), Fold: expandFold()}, `FoldSubsection sub-1 false by UpdateSidebarView ""`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			sent(t, h, foldChange(tt.fold))

			// Assert
			if len(h.Verbs.arrangements) != 1 || h.Verbs.arrangements[0] != tt.want {
				t.Fatalf("verb calls = %v, want [%s]", h.Verbs.arrangements, tt.want)
			}
		})
	}
}

func TestUpdateSidebarViewAnswersAnUnknownSubsection(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Verbs.arrangeErr = &workspace.Refusal{Rpc: "UpdateSidebarView", Arm: workspace.ArmUnknownSubsection, Reason: "gone", NotFound: true}

	// Act
	resp, err := h.Client.UpdateSidebarView(context.Background(), connect.NewRequest(
		foldChange(&agentreplv1.SidebarViewFoldSection{Section: subsectionSection("gone"), Fold: collapseFold()})))

	// Assert
	if err != nil {
		t.Fatalf("UpdateSidebarView: %v", err)
	}
	if resp.Msg.GetError().GetUnknownSubsection() == nil {
		t.Fatalf("result = %v, want unknown_subsection", resp.Msg.GetResult())
	}
}

func TestUpdateSidebarViewRefusesASubsectionFoldWithNoId(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.UpdateSidebarView(context.Background(), connect.NewRequest(
		foldChange(&agentreplv1.SidebarViewFoldSection{Section: subsectionSection(""), Fold: collapseFold()})))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("UpdateSidebarView = %v, want InvalidArgument", err)
	}
	if len(h.Verbs.arrangements) != 0 {
		t.Fatalf("verb calls = %v, want none for a malformed request", h.Verbs.arrangements)
	}
}

func TestUpdateSidebarViewAnswersACollapseThatHoldsTheCurrentWorkspace(t *testing.T) {
	refused := func() error {
		return &workspace.Refusal{Rpc: "UpdateSidebarView", Arm: workspace.ArmHoldsCurrent, Reason: "holds the current workspace"}
	}
	tests := []struct {
		name    string
		arrange func(h *harness)
		section *agentreplv1.SidebarViewFoldSection
	}{
		{
			name: "a repository",
			arrange: func(h *harness) {
				h.DB.repositories = append(h.DB.repositories, wsm.Repository{ID: "repo-1", Dir: "/repos/one"})
				h.Verbs.foldErr = refused()
			},
			section: &agentreplv1.SidebarViewFoldSection{Section: repositorySection("repo-1"), Fold: collapseFold()},
		},
		{
			name:    "a subsection",
			arrange: func(h *harness) { h.Verbs.arrangeErr = refused() },
			section: &agentreplv1.SidebarViewFoldSection{Section: subsectionSection("sub-1"), Fold: collapseFold()},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(h)

			// Act
			resp, err := h.Client.UpdateSidebarView(context.Background(), connect.NewRequest(foldChange(tt.section)))

			// Assert
			if err != nil {
				t.Fatalf("UpdateSidebarView: %v", err)
			}
			if resp.Msg.GetError().GetHoldsCurrent() == nil {
				t.Fatalf("result = %v, want holds_current", resp.Msg.GetResult())
			}
		})
	}
}
