package server

import (
	"context"
	"errors"
	"slices"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

func subRef(id string) *agentreplv1.SubsectionRef { return &agentreplv1.SubsectionRef{Id: id} }

func repoRef(id string) *workspacev1.RepositoryRef { return &workspacev1.RepositoryRef{Id: id} }

func endPlace() *agentreplv1.ArrangeEnd { return &agentreplv1.ArrangeEnd{} }

func createSubsection(repo, name string) *agentreplv1.ArrangeSidebarRequest {
	return &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_CreateSubsection{
		CreateSubsection: &agentreplv1.ArrangeCreateSubsection{Repository: repoRef(repo), Name: name}}}
}

func moveRepository(repo string, before *workspacev1.RepositoryRef) *agentreplv1.ArrangeSidebarRequest {
	move := &agentreplv1.ArrangeMoveRepository{Repository: repoRef(repo)}
	if before != nil {
		move.To = &agentreplv1.ArrangeMoveRepository_Before{Before: before}
	} else {
		move.To = &agentreplv1.ArrangeMoveRepository_End{End: endPlace()}
	}
	return &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_MoveRepository{MoveRepository: move}}
}

func placeWorkspace(ws *workspacev1.WorkspaceRef, into string, before *workspacev1.WorkspaceRef) *agentreplv1.ArrangeSidebarRequest {
	place := &agentreplv1.ArrangePlaceWorkspace{Workspace: ws}
	if into == "" {
		place.Into = &agentreplv1.ArrangePlaceWorkspace_NoSubsection{NoSubsection: &agentreplv1.ArrangeNoSubsection{}}
	} else {
		place.Into = &agentreplv1.ArrangePlaceWorkspace_Subsection{Subsection: subRef(into)}
	}
	if before != nil {
		place.At = &agentreplv1.ArrangePlaceWorkspace_Before{Before: before}
	} else {
		place.At = &agentreplv1.ArrangePlaceWorkspace_End{End: endPlace()}
	}
	return &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_PlaceWorkspace{PlaceWorkspace: place}}
}

// arrangeHarness is a harness holding two repositories and two workspaces.
func arrangeHarness(t *testing.T) (*harness, *workspacev1.WorkspaceRef) {
	t.Helper()
	h, other := swapHarness(t)
	h.DB.repositories = append(h.DB.repositories,
		wsm.Repository{ID: "repo-1", Dir: "/repos/one"}, wsm.Repository{ID: "repo-2", Dir: "/repos/two"})
	return h, other
}

func TestArrangeSidebarHandsEachChangeToItsVerb(t *testing.T) {
	_, other := arrangeHarness(t)
	tests := []struct {
		name string
		req  *agentreplv1.ArrangeSidebarRequest
		want string
	}{
		{name: "create a subsection", req: createSubsection("repo-1", "later"), want: "CreateSubsection repo-1 later"},
		{name: "rename a subsection", want: "RenameSubsection sub-1 sooner",
			req: &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_RenameSubsection{
				RenameSubsection: &agentreplv1.ArrangeRenameSubsection{Subsection: subRef("sub-1"), Name: "sooner"}}}},
		{name: "delete a subsection", want: "DeleteSubsection sub-1",
			req: &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_DeleteSubsection{
				DeleteSubsection: &agentreplv1.ArrangeDeleteSubsection{Subsection: subRef("sub-1")}}}},
		{name: "move a subsection before another", want: "MoveSubsection sub-1 sub-2",
			req: &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_MoveSubsection{
				MoveSubsection: &agentreplv1.ArrangeMoveSubsection{Subsection: subRef("sub-1"),
					To: &agentreplv1.ArrangeMoveSubsection_Before{Before: subRef("sub-2")}}}}},
		{name: "move a subsection to the end", want: "MoveSubsection sub-1 ",
			req: &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_MoveSubsection{
				MoveSubsection: &agentreplv1.ArrangeMoveSubsection{Subsection: subRef("sub-1"),
					To: &agentreplv1.ArrangeMoveSubsection_End{End: endPlace()}}}}},
		{name: "move a repository before another", req: moveRepository("repo-2", repoRef("repo-1")), want: "MoveRepository repo-2 repo-1"},
		{name: "move a repository to the end", req: moveRepository("repo-1", nil), want: "MoveRepository repo-1 "},
		{name: "place a workspace before another in a subsection", req: placeWorkspace(ref(), "sub-1", other),
			want: "PlaceWorkspace " + string(testWorkspaceID) + " sub-1 ws-2"},
		{name: "place a workspace last among the unsectioned", req: placeWorkspace(ref(), "", nil),
			want: "PlaceWorkspace " + string(testWorkspaceID) + "  "},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h, _ := arrangeHarness(t)

			// Act
			resp, err := h.Client.ArrangeSidebar(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil {
				t.Fatalf("ArrangeSidebar: %v", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("result = %v, want success", resp.Msg.GetResult())
			}
			if want := []string{tt.want}; !slices.Equal(h.Verbs.arrangements, want) {
				t.Fatalf("verb calls = %v, want %v", h.Verbs.arrangements, want)
			}
		})
	}
}

func TestArrangeSidebarRefusesAnUnknownRepositoryBeforeTheVerb(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.ArrangeSidebarRequest
	}{
		{name: "creating in it", req: createSubsection("repo-nope", "later")},
		{name: "moving it", req: moveRepository("repo-nope", nil)},
		{name: "moving before it", req: moveRepository("repo-1", repoRef("repo-nope"))},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h, _ := arrangeHarness(t)

			// Act
			resp, err := h.Client.ArrangeSidebar(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil {
				t.Fatalf("ArrangeSidebar: %v", err)
			}
			if resp.Msg.GetError().GetUnknownRepository() == nil {
				t.Fatalf("result = %v, want unknown_repository", resp.Msg.GetResult())
			}
			if len(h.Verbs.arrangements) != 0 {
				t.Fatalf("verb calls = %v, want none", h.Verbs.arrangements)
			}
		})
	}
}

func TestArrangeSidebarNamesAnUnknownWorkspaceBeforeTheVerb(t *testing.T) {
	// Arrange
	h, _ := arrangeHarness(t)

	// Act
	resp, err := h.Client.ArrangeSidebar(context.Background(), connect.NewRequest(
		placeWorkspace(ref(), "", &workspacev1.WorkspaceRef{Id: "ws-nope"})))

	// Assert
	if err != nil {
		t.Fatalf("ArrangeSidebar: %v", err)
	}
	if got := resp.Msg.GetError().GetUnknownWorkspace(); got.GetWorkspace().GetId() != "ws-nope" {
		t.Fatalf("result = %v, want unknown_workspace naming ws-nope", resp.Msg.GetResult())
	}
	if len(h.Verbs.arrangements) != 0 {
		t.Fatalf("verb calls = %v, want none", h.Verbs.arrangements)
	}
}

func TestArrangeSidebarRefusesARepositoryMovedBeforeItselfByAnotherSpelling(t *testing.T) {
	// Arrange
	h, _ := arrangeHarness(t)

	// Act
	_, err := h.Client.ArrangeSidebar(context.Background(), connect.NewRequest(
		moveRepository("repo-1", &workspacev1.RepositoryRef{Dir: "/repos/one"})))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("ArrangeSidebar = %v, want invalid argument", err)
	}
	if len(h.Verbs.arrangements) != 0 {
		t.Fatalf("verb calls = %v, want none", h.Verbs.arrangements)
	}
}

func TestArrangeSidebarMapsAVerbRefusalOntoItsArm(t *testing.T) {
	tests := []struct {
		arm    string
		fields map[string]any
		check  func(*agentreplv1.ArrangeSidebarError) bool
	}{
		{arm: workspace.ArmUnknownSubsection, fields: map[string]any{"subsection": subRef("sub-gone")},
			check: func(e *agentreplv1.ArrangeSidebarError) bool {
				return e.GetUnknownSubsection().GetSubsection().GetId() == "sub-gone"
			}},
		{arm: workspace.ArmBlankName, check: func(e *agentreplv1.ArrangeSidebarError) bool { return e.GetBlankName() != nil }},
		{arm: workspace.ArmOtherRepository, check: func(e *agentreplv1.ArrangeSidebarError) bool { return e.GetOtherRepository() != nil }},
		{arm: workspace.ArmAnchorElsewhere, check: func(e *agentreplv1.ArrangeSidebarError) bool { return e.GetAnchorElsewhere() != nil }},
		{arm: workspace.ArmTransferringAway, check: func(e *agentreplv1.ArrangeSidebarError) bool { return e.GetTransferringAway() != nil }},
		{arm: workspace.ArmNotYetAdopted, check: func(e *agentreplv1.ArrangeSidebarError) bool { return e.GetNotYetAdopted() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.arm, func(t *testing.T) {
			// Arrange
			h, _ := arrangeHarness(t)
			h.Verbs.arrangeErr = &workspace.Refusal{Arm: tt.arm, Reason: "refused", Fields: tt.fields}

			// Act
			resp, err := h.Client.ArrangeSidebar(context.Background(), connect.NewRequest(createSubsection("repo-1", "later")))

			// Assert
			if err != nil {
				t.Fatalf("ArrangeSidebar: %v", err)
			}
			if !tt.check(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want %s", resp.Msg.GetResult(), tt.arm)
			}
		})
	}
}

func TestArrangeSidebarFailsAVerbError(t *testing.T) {
	// Arrange
	h, _ := arrangeHarness(t)
	h.Verbs.arrangeErr = errors.New("disk I/O error")

	// Act
	_, err := h.Client.ArrangeSidebar(context.Background(), connect.NewRequest(createSubsection("repo-1", "later")))

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("ArrangeSidebar = %v, want an internal failure", err)
	}
}

func TestValidateArrangeSidebarRequestRefusesAnIncompleteRequest(t *testing.T) {
	selfBefore := placeWorkspace(ref(), "", ref())
	tests := []struct {
		name string
		req  *agentreplv1.ArrangeSidebarRequest
	}{
		{"no change arm", &agentreplv1.ArrangeSidebarRequest{}},
		{"a create with no repository", &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_CreateSubsection{
			CreateSubsection: &agentreplv1.ArrangeCreateSubsection{Name: "x"}}}},
		{"a rename with a blank id", &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_RenameSubsection{
			RenameSubsection: &agentreplv1.ArrangeRenameSubsection{Subsection: subRef(""), Name: "x"}}}},
		{"a delete with no ref", &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_DeleteSubsection{
			DeleteSubsection: &agentreplv1.ArrangeDeleteSubsection{}}}},
		{"a subsection move with no place", &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_MoveSubsection{
			MoveSubsection: &agentreplv1.ArrangeMoveSubsection{Subsection: subRef("sub-1")}}}},
		{"a subsection moved before itself", &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_MoveSubsection{
			MoveSubsection: &agentreplv1.ArrangeMoveSubsection{Subsection: subRef("sub-1"),
				To: &agentreplv1.ArrangeMoveSubsection_Before{Before: subRef("sub-1")}}}}},
		{"a repository move with no place", &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_MoveRepository{
			MoveRepository: &agentreplv1.ArrangeMoveRepository{Repository: repoRef("repo-1")}}}},
		{"a placement with no destination", &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_PlaceWorkspace{
			PlaceWorkspace: &agentreplv1.ArrangePlaceWorkspace{Workspace: ref(), At: &agentreplv1.ArrangePlaceWorkspace_End{End: endPlace()}}}}},
		{"a placement with no place", &agentreplv1.ArrangeSidebarRequest{Change: &agentreplv1.ArrangeSidebarRequest_PlaceWorkspace{
			PlaceWorkspace: &agentreplv1.ArrangePlaceWorkspace{Workspace: ref(),
				Into: &agentreplv1.ArrangePlaceWorkspace_NoSubsection{NoSubsection: &agentreplv1.ArrangeNoSubsection{}}}}}},
		{"a workspace placed before itself", selfBefore},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := validateArrangeSidebarRequest(tt.req)

			// Assert
			if err == nil || err.Code() != connect.CodeInvalidArgument {
				t.Fatalf("validate = %v, want invalid argument", err)
			}
		})
	}
}

func TestArrangeSidebarAnswersTheRevisionOfTheRosterThatCarriesTheChange(t *testing.T) {
	tests := []struct {
		name      string
		published []uint64
		want      uint64
	}{
		{name: "the latest roster's revision", published: []uint64{4, 7}, want: 7},
		{name: "zero before any roster", published: nil, want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h, _ := arrangeHarness(t)
			for _, revision := range tt.published {
				h.Sidebar.topic.Publish(&frontendv1.WorkspaceRoster{Revision: revision})
			}

			// Act
			resp, err := h.Client.ArrangeSidebar(context.Background(), connect.NewRequest(createSubsection("repo-1", "later")))

			// Assert
			if err != nil {
				t.Fatalf("ArrangeSidebar: %v", err)
			}
			if got := resp.Msg.GetSuccess().GetRosterRevision(); got != tt.want {
				t.Fatalf("roster revision = %d, want %d", got, tt.want)
			}
		})
	}
}
