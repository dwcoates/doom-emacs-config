package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

const ownedPullURL = "https://github.com/ChessCom/explanation-engine/pull/42"

var ownedPull = wsm.PullRequestKey{Owner: "ChessCom", Repo: "explanation-engine", Number: 42}

func findOwner(t *testing.T, h *harness, url string) (*connect.Response[agentreplv1.FindPullRequestOwnerResponse], error) {
	t.Helper()
	return h.Client.FindPullRequestOwner(context.Background(), connect.NewRequest(&agentreplv1.FindPullRequestOwnerRequest{Url: url}))
}

func TestFindPullRequestOwnerAnswersWhatTheUrlNames(t *testing.T) {
	tests := []struct {
		name   string
		url    string
		owners map[wsm.PullRequestKey]wsm.PullRequestOwner
		// want is the answer's arm, and for an owner its workspace and
		// standing.
		want       string
		wantID     string
		wantClosed bool
	}{
		{name: "an open owner", url: ownedPullURL,
			owners: map[wsm.PullRequestKey]wsm.PullRequestOwner{ownedPull: {Workspace: "ws-open"}},
			want:   "owned", wantID: "ws-open"},
		{name: "a closed owner", url: ownedPullURL,
			owners: map[wsm.PullRequestKey]wsm.PullRequestOwner{ownedPull: {Workspace: "ws-landed", Closed: true}},
			want:   "owned", wantID: "ws-landed", wantClosed: true},
		{name: "the pull request's files page names it too", url: ownedPullURL + "/files#diff",
			owners: map[wsm.PullRequestKey]wsm.PullRequestOwner{ownedPull: {Workspace: "ws-open"}},
			want:   "owned", wantID: "ws-open"},
		{name: "a pull request nobody owns", url: ownedPullURL, want: "unowned"},
		{name: "a url that is no pull request", url: "https://github.com/ChessCom/explanation-engine/issues/42", want: "not_pull_request"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := findHarness(t)
			h.DB.prOwners = tt.owners

			// Act
			resp, err := findOwner(t, h, tt.url)

			// Assert
			if err != nil {
				t.Fatalf("FindPullRequestOwner: %v", err)
			}
			success := resp.Msg.GetSuccess()
			var got string
			switch {
			case success.GetOwned() != nil:
				got = "owned"
			case success.GetUnowned() != nil:
				got = "unowned"
			case success.GetNotPullRequest() != nil:
				got = "not_pull_request"
			}
			if got != tt.want {
				t.Fatalf("answer = %v, want %s", success, tt.want)
			}
			if tt.want != "owned" {
				return
			}
			owned := success.GetOwned()
			ref := h.DB.workspaces[ids.WorkspaceID(owned.GetWorkspace().GetId())].Ref()
			if owned.GetWorkspace().GetId() != tt.wantID || owned.GetWorkspace().GetDir() != ref.GetDir() {
				t.Fatalf("owner = %v, want %s at %s", owned.GetWorkspace(), tt.wantID, ref.GetDir())
			}
			if (owned.GetClosed() != nil) != tt.wantClosed || (owned.GetOpen() != nil) == tt.wantClosed {
				t.Fatalf("standing = %v, want closed %v", owned, tt.wantClosed)
			}
		})
	}
}

func TestFindPullRequestOwnerRefusesABlankUrl(t *testing.T) {
	// Arrange
	h := findHarness(t)

	// Act
	_, err := findOwner(t, h, " ")

	// Assert
	if connectCode(t, err) != connect.CodeInvalidArgument {
		t.Fatalf("err = %v, want InvalidArgument", err)
	}
}

func TestFindPullRequestOwnerFailsAndRecordsAFailedRead(t *testing.T) {
	tests := []struct {
		name  string
		edit  func(*harness)
		cause string
	}{
		{name: "the owner", edit: func(h *harness) { h.DB.prOwnerErr = errors.New("disk gone") },
			cause: "read the pull request's owner: disk gone"},
		{name: "the owning workspace", edit: func(h *harness) {
			h.DB.prOwners = map[wsm.PullRequestKey]wsm.PullRequestOwner{ownedPull: {Workspace: "ws-open"}}
			h.DB.workspaceErr = errors.New("disk gone")
		}, cause: "read the owning workspace ws-open: disk gone"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := findHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
			tt.edit(h)

			// Act
			resp, err := findOwner(t, h, ownedPullURL)

			// Assert
			if connectCode(t, err) != connect.CodeInternal || resp != nil {
				t.Fatalf("FindPullRequestOwner = %v, %v; want an internal failure and no answer", resp, err)
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "FindPullRequestOwner"})
			if !ok || rec.Context["cause"] != tt.cause || rec.Context["pull_request"] != ownedPull.String() {
				t.Fatalf("ERROR record = %v (%v), want cause %q naming the pull request", rec.Context, ok, tt.cause)
			}
		})
	}
}
