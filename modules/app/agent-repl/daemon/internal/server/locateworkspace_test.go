package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// locateHarness is findHarness's registry with a hidden merge workspace the
// roster draws no row of.
func locateHarness(t *testing.T, opts ...option) *harness {
	t.Helper()
	h := findHarness(t, opts...)
	h.DB.workspaces["ws-merge"] = wsm.Workspace{ID: "ws-merge", Repo: "repo-1", Dir: "/src/merge", Name: "merge-one",
		Backlogged: true, MergeOf: &wsm.MergeOrigin{Requester: "ws-open"}}
	return h
}

func TestLocateWorkspaceAnswersTheRegistrysWorkspaces(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.LocateWorkspaceRequest
		want []string
	}{
		{name: "a hidden merge workspace containing its worktree's subdirectory",
			req:  &agentreplv1.LocateWorkspaceRequest{By: &agentreplv1.LocateWorkspaceRequest_Containing{Containing: &agentreplv1.LocateWorkspaceContaining{Dir: "/src/merge/daemon"}}},
			want: []string{"ws-merge"}},
		{name: "a hidden merge workspace by name",
			req:  &agentreplv1.LocateWorkspaceRequest{By: &agentreplv1.LocateWorkspaceRequest_Named{Named: &agentreplv1.LocateWorkspaceNamed{Name: "merge-one"}}},
			want: []string{"ws-merge"}},
		{name: "a merged workspace, in no stream, at its directory",
			req:  &agentreplv1.LocateWorkspaceRequest{By: &agentreplv1.LocateWorkspaceRequest_At{At: &agentreplv1.LocateWorkspaceAt{Dir: "/src/landed"}}},
			want: []string{"ws-landed"}},
		{name: "a directory in no worktree",
			req: &agentreplv1.LocateWorkspaceRequest{By: &agentreplv1.LocateWorkspaceRequest_Containing{Containing: &agentreplv1.LocateWorkspaceContaining{Dir: "/elsewhere"}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := locateHarness(t)

			// Act
			resp, err := h.Client.LocateWorkspace(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("LocateWorkspace = %v, %v; want success", resp, err)
			}
			var got []string
			for _, ref := range resp.Msg.GetSuccess().GetWorkspaces() {
				got = append(got, ref.GetId())
			}
			if len(got) != len(tt.want) || (len(got) == 1 && got[0] != tt.want[0]) {
				t.Fatalf("located %v, want %v", got, tt.want)
			}
		})
	}
}

func TestLocateWorkspaceRefusesAnInvalidRequest(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.LocateWorkspaceRequest
	}{
		{name: "no arm", req: &agentreplv1.LocateWorkspaceRequest{}},
		{name: "a blank name", req: &agentreplv1.LocateWorkspaceRequest{By: &agentreplv1.LocateWorkspaceRequest_Named{Named: &agentreplv1.LocateWorkspaceNamed{Name: " "}}}},
		{name: "a relative directory", req: &agentreplv1.LocateWorkspaceRequest{By: &agentreplv1.LocateWorkspaceRequest_At{At: &agentreplv1.LocateWorkspaceAt{Dir: "src/merge"}}}},
		{name: "a relative containing directory", req: &agentreplv1.LocateWorkspaceRequest{By: &agentreplv1.LocateWorkspaceRequest_Containing{Containing: &agentreplv1.LocateWorkspaceContaining{}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := locateHarness(t)

			// Act
			_, err := h.Client.LocateWorkspace(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if connectCode(t, err) != connect.CodeInvalidArgument {
				t.Fatalf("err = %v, want InvalidArgument", err)
			}
		})
	}
}

func TestLocateWorkspaceFailsAndRecordsAFailedRead(t *testing.T) {
	tests := []struct {
		name  string
		edit  func(*harness)
		cause string
	}{
		{name: "the workspaces", edit: func(h *harness) { h.DB.listWorkspacesErr = errors.New("disk gone") }, cause: "list the workspaces: disk gone"},
		{name: "no roster published yet", edit: func(h *harness) { h.Sidebar.topic.ClearIf(func(*frontendv1.RosterPush) bool { return true }) },
			cause: "read the roster: the daemon has published no roster yet"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := locateHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
			tt.edit(h)

			// Act
			resp, err := h.Client.LocateWorkspace(context.Background(), connect.NewRequest(&agentreplv1.LocateWorkspaceRequest{
				By: &agentreplv1.LocateWorkspaceRequest_Named{Named: &agentreplv1.LocateWorkspaceNamed{Name: "merge-one"}}}))

			// Assert
			if connectCode(t, err) != connect.CodeInternal || resp != nil {
				t.Fatalf("LocateWorkspace = %v, %v; want an internal failure and no answer", resp, err)
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "LocateWorkspace"})
			if !ok || rec.Context["cause"] != tt.cause {
				t.Fatalf("ERROR record = %v (%v), want cause %q", rec.Context, ok, tt.cause)
			}
		})
	}
}
