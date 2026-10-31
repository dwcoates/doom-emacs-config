package server

import (
	"context"
	"errors"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// findHarness is a harness whose registry holds one repository with an open
// workspace (summarized) and a landed one, in place of the default workspace.
func findHarness(t *testing.T, opts ...option) *harness {
	t.Helper()
	h := newHarness(t, opts...)
	repo := wsm.Repository{ID: "repo-1", Dir: "/src/repo"}
	h.DB.repositories = []wsm.Repository{repo}
	created := time.UnixMilli(1_000)
	merged := time.UnixMilli(5_000)
	h.DB.workspaces = map[ids.WorkspaceID]wsm.Workspace{
		"ws-open":   {ID: "ws-open", Repo: repo.ID, Dir: "/src/open", Name: "open-one", Branch: "open-one", CreatedAt: created, LastActivityAt: ptrTo(time.UnixMilli(9_000))},
		"ws-landed": {ID: "ws-landed", Repo: repo.ID, Dir: "/src/landed", Name: "landed-one", Branch: "landed-one", CreatedAt: created, Closed: true, MergedAt: &merged},
	}
	h.DB.summaries = map[ids.WorkspaceID]wsm.WorkspaceSummary{"ws-open": {Title: "Fix the footer"}}
	return h
}

func TestFindWorkspacesAnswersEveryMatchWithItsSummary(t *testing.T) {
	// Arrange
	h := findHarness(t)

	// Act
	resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{}))

	// Assert
	if err != nil {
		t.Fatalf("FindWorkspaces: %v", err)
	}
	found := resp.Msg.GetSuccess().GetComplete().GetWorkspaces()
	if len(found) != 2 {
		t.Fatalf("found = %v, want both workspaces in a complete extent", resp.Msg)
	}
	open, landed := found[0], found[1]
	if open.GetName() != "open-one" || open.GetSummary() != "Fix the footer" || open.GetLastActiveAtMs() != 9_000 ||
		open.GetStanding().GetOpen() == nil || open.GetRepository().GetDir() != "/src/repo" || open.GetWorkspace().GetDir() != "/src/open" {
		t.Fatalf("first = %v, want the open workspace with its summary, most recently active first", open)
	}
	if landed.GetName() != "landed-one" || landed.Summary != nil || landed.GetStanding().GetLanded().GetLandedAtMs() != 5_000 || landed.GetCreatedAtMs() != 1_000 {
		t.Fatalf("second = %v, want the landed workspace with no summary", landed)
	}
}

func TestFindWorkspacesMapsTheRequestOntoTheQuery(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.FindWorkspacesRequest
		want []string
	}{
		{name: "by repository dir", req: &agentreplv1.FindWorkspacesRequest{Repository: &workspacev1.RepositoryRef{Dir: "/src/repo"}}, want: []string{"open-one", "landed-one"}},
		{name: "open", req: &agentreplv1.FindWorkspacesRequest{Standing: &agentreplv1.FindWorkspacesStanding{
			Standing: &agentreplv1.FindWorkspacesStanding_Open{Open: &agentreplv1.FindWorkspacesStandingOpen{}}}}, want: []string{"open-one"}},
		{name: "closed", req: &agentreplv1.FindWorkspacesRequest{Standing: &agentreplv1.FindWorkspacesStanding{
			Standing: &agentreplv1.FindWorkspacesStanding_Closed{Closed: &agentreplv1.FindWorkspacesStandingClosed{}}}}, want: []string{"landed-one"}},
		{name: "landed", req: &agentreplv1.FindWorkspacesRequest{Standing: &agentreplv1.FindWorkspacesStanding{
			Standing: &agentreplv1.FindWorkspacesStanding_Landed{Landed: &agentreplv1.FindWorkspacesStandingLanded{}}}}, want: []string{"landed-one"}},
		{name: "a window after the landing", req: &agentreplv1.FindWorkspacesRequest{Window: &agentreplv1.FindWorkspacesWindow{SinceMs: ptrTo(int64(6_000))}}, want: []string{"open-one"}},
		{name: "a window before the creation", req: &agentreplv1.FindWorkspacesRequest{Window: &agentreplv1.FindWorkspacesWindow{UntilMs: ptrTo(int64(500))}}, want: nil},
		{name: "text over the summary", req: &agentreplv1.FindWorkspacesRequest{Text: ptrTo("FOOTER")}, want: []string{"open-one"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := findHarness(t)

			// Act
			resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil {
				t.Fatalf("FindWorkspaces: %v", err)
			}
			var got []string
			for _, f := range resp.Msg.GetSuccess().GetComplete().GetWorkspaces() {
				got = append(got, f.GetName())
			}
			if len(got) != len(tt.want) || (len(got) > 0 && got[0] != tt.want[0]) || resp.Msg.GetSuccess().GetComplete() == nil {
				t.Fatalf("found %v (%v), want %v in a complete extent", got, resp.Msg, tt.want)
			}
		})
	}
}

func TestFindWorkspacesAnswersALimitedFindAsPartial(t *testing.T) {
	// Arrange
	h := findHarness(t)

	// Act
	resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{Limit: ptrTo(uint32(1))}))

	// Assert
	partial := resp.Msg.GetSuccess().GetPartial()
	if err != nil || len(partial.GetWorkspaces()) != 1 || partial.GetWorkspaces()[0].GetName() != "open-one" || partial.GetOmitted() != 1 {
		t.Fatalf("FindWorkspaces = %v, %v; want the open workspace and one omitted", resp, err)
	}
}

func TestFindWorkspacesAnswersAClosedWorkspaceWithItsCloseInstant(t *testing.T) {
	// Arrange
	h := findHarness(t)
	h.DB.workspaces["ws-closed"] = wsm.Workspace{ID: "ws-closed", Repo: "repo-1", Dir: "/src/closed", Name: "closed-one",
		CreatedAt: time.UnixMilli(1_000), Closed: true, Closure: &wsm.Closure{At: time.UnixMilli(3_000), By: wsm.CloseByAgent}}

	// Act
	resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{Text: ptrTo("closed-one")}))

	// Assert
	found := resp.Msg.GetSuccess().GetComplete().GetWorkspaces()
	if err != nil || len(found) != 1 || found[0].GetStanding().GetClosed().GetClosedAtMs() != 3_000 {
		t.Fatalf("FindWorkspaces = %v, %v; want the closed workspace closed at 3000", resp, err)
	}
}

func TestFindWorkspacesByCreatorNamesTheCreatorAndTheParent(t *testing.T) {
	// Arrange: the landed workspace was asked for by the open one, and is its child.
	h := findHarness(t)
	landed := h.DB.workspaces["ws-landed"]
	creator := ids.WorkspaceID("ws-open")
	landed.RequestedBy, landed.Parent = &creator, &creator
	h.DB.workspaces["ws-landed"] = landed

	// Act
	resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{
		CreatedBy: &workspacev1.WorkspaceRef{Id: "ws-open", Dir: "/src/open"}}))

	// Assert
	if err != nil {
		t.Fatalf("FindWorkspaces: %v", err)
	}
	found := resp.Msg.GetSuccess().GetComplete().GetWorkspaces()
	if len(found) != 1 || found[0].GetName() != "landed-one" ||
		found[0].GetCreatedBy().GetDir() != "/src/open" || found[0].GetParent().GetId() != "ws-open" {
		t.Fatalf("found = %v, want only the landed workspace, naming its creator and parent", found)
	}
}

func TestFindWorkspacesByAnUnknownCreatorIsAnAnswer(t *testing.T) {
	tests := []struct {
		name string
		ref  *workspacev1.WorkspaceRef
	}{
		{name: "an id the registry does not hold", ref: &workspacev1.WorkspaceRef{Id: "ws-gone"}},
		{name: "a dir that disagrees with the id's", ref: &workspacev1.WorkspaceRef{Id: "ws-open", Dir: "/src/elsewhere"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := findHarness(t)

			// Act
			resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{CreatedBy: tt.ref}))

			// Assert
			if err != nil || resp.Msg.GetError().GetUnknownCreator() == nil {
				t.Fatalf("FindWorkspaces = %v, %v; want unknown_creator", resp, err)
			}
		})
	}
}

func TestFindWorkspacesOfAnUnknownRepositoryIsAnAnswer(t *testing.T) {
	// Arrange
	h := findHarness(t)

	// Act
	resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{
		Repository: &workspacev1.RepositoryRef{Dir: "/src/nowhere"}}))

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownRepository() == nil {
		t.Fatalf("FindWorkspaces = %v, %v; want unknown_repository", resp, err)
	}
}

func TestFindWorkspacesRefusesAMalformedRequest(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.FindWorkspacesRequest
	}{
		{name: "an empty repository ref", req: &agentreplv1.FindWorkspacesRequest{Repository: &workspacev1.RepositoryRef{}}},
		{name: "a reversed window", req: &agentreplv1.FindWorkspacesRequest{Window: &agentreplv1.FindWorkspacesWindow{SinceMs: ptrTo(int64(2)), UntilMs: ptrTo(int64(1))}}},
		{name: "a standing with no arm", req: &agentreplv1.FindWorkspacesRequest{Standing: &agentreplv1.FindWorkspacesStanding{}}},
		{name: "a blank text", req: &agentreplv1.FindWorkspacesRequest{Text: ptrTo("  ")}},
		{name: "a zero limit", req: &agentreplv1.FindWorkspacesRequest{Limit: ptrTo(uint32(0))}},
		{name: "a creator ref naming no workspace", req: &agentreplv1.FindWorkspacesRequest{CreatedBy: &workspacev1.WorkspaceRef{}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := findHarness(t)

			// Act
			_, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if connectCode(t, err) != connect.CodeInvalidArgument {
				t.Fatalf("err = %v, want InvalidArgument", err)
			}
		})
	}
}

func TestFindWorkspacesFailsAndRecordsAFailedRead(t *testing.T) {
	tests := []struct {
		name  string
		edit  func(*fakeDB)
		cause string
	}{
		{name: "the repositories", edit: func(d *fakeDB) { d.listRepositoriesErr = errors.New("disk gone") }, cause: "list the repositories: disk gone"},
		{name: "the summaries", edit: func(d *fakeDB) { d.summariesErr = errors.New("disk gone") }, cause: "list the workspace summaries: disk gone"},
		{name: "a workspace of an unregistered repository", edit: func(d *fakeDB) {
			d.workspaces["ws-orphan"] = wsm.Workspace{ID: "ws-orphan", Repo: "repo-gone", Name: "orphan"}
		}, cause: "workspacefind: workspace ws-orphan names repository repo-gone, which the registry does not hold"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := findHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
			tt.edit(h.DB)

			// Act
			resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{}))

			// Assert
			if connectCode(t, err) != connect.CodeInternal || resp != nil {
				t.Fatalf("FindWorkspaces = %v, %v; want an internal failure and no answer", resp, err)
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "FindWorkspaces"})
			if !ok || rec.Context["cause"] != tt.cause {
				t.Fatalf("ERROR record = %v (%v), want cause %q", rec.Context, ok, tt.cause)
			}
		})
	}
}
