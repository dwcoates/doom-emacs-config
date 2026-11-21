package server

import (
	"context"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/merge"
	"agent-repld/internal/wsm"
)

// landingAt is a landing of HEAD by PATH at second AT.
func landingAt(head string, path wsm.LandingPath, at int64) wsm.MergeLanding {
	return wsm.MergeLanding{Repo: "/repo/.git", Head: head, Path: path, Branch: "b-" + head, Workspace: "ws-" + wsm.WorkspaceID(head), WorkspaceName: "n-" + head, LandedAt: time.Unix(at, 0)}
}

// listLandings asks for repo-1's landings from SINCE to UNTIL, seconds; a
// zero UNTIL leaves the end unset.
func listLandings(t *testing.T, h *harness, since, until int64) *connect.Response[agentreplv1.ListMergeLandingsResponse] {
	t.Helper()
	req := &agentreplv1.ListMergeLandingsRequest{Repository: &workspacev1.RepositoryRef{Id: "repo-1"}, SinceMs: since * 1000}
	if until != 0 {
		ms := until * 1000
		req.UntilMs = &ms
	}
	resp, err := h.Client.ListMergeLandings(context.Background(), connect.NewRequest(req))
	if err != nil {
		t.Fatalf("ListMergeLandings: %v", err)
	}
	return resp
}

func TestListMergeLandingsSplitsTheQueuesFromEveryOther(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Merge.landings = merge.LandingsWindow{
		Repository:     wsm.Repository{ID: "repo-1", Dir: "/repo"},
		ThroughQueue:   []wsm.MergeLanding{landingAt("q1", wsm.LandingThroughQueue, 110), landingAt("q2", wsm.LandingThroughQueue, 120)},
		Otherwise:      []wsm.MergeLanding{landingAt("b1", wsm.LandingQueueBypass, 115), landingAt("m1", wsm.LandingDeployMediator, 130)},
		RecordingBegan: time.Unix(50, 0),
	}

	// Act
	resp := listLandings(t, h, 100, 200)

	// Assert
	got := resp.Msg.GetSuccess()
	queue, other := got.GetThroughQueue(), got.GetOtherwise()
	if queue.GetCount() != 2 || len(queue.GetLandings()) != 2 || queue.GetLandings()[1].GetLandedHead() != "q2" ||
		queue.GetLandings()[0].GetWorkspace().GetName() != "n-q1" || queue.GetLandings()[0].GetLandedAtMs() != 110_000 {
		t.Fatalf("through the queue = %v; want q1 and q2, counted, each naming its workspace", queue)
	}
	if other.GetCount() != 2 || other.GetLandings()[0].GetPath().GetQueueBypass() == nil || other.GetLandings()[1].GetPath().GetDeployMediator() == nil {
		t.Fatalf("otherwise = %v; want the bypass and the mediator landing, each with its path", other)
	}
	if got.GetCoverage().GetComplete() == nil || got.GetWindow().GetSinceMs() != 100_000 || got.GetWindow().GetUntilMs() != 200_000 {
		t.Fatalf("coverage %v, window %v; want a complete window of [100s, 200s]", got.GetCoverage(), got.GetWindow())
	}
	if ask := h.Merge.landingsAsked; len(ask) != 1 || ask[0].scope.ID != "repo-1" || !ask[0].since.Equal(time.Unix(100, 0)) || !ask[0].until.Equal(time.Unix(200, 0)) {
		t.Fatalf("asked %+v; want repo-1's [100s, 200s]", ask)
	}
}

func TestListMergeLandingsFlagsAWindowBeforeRecordingBegan(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Merge.landings = merge.LandingsWindow{Repository: wsm.Repository{ID: "repo-1"}, RecordingBegan: time.Unix(150, 0)}

	// Act
	resp := listLandings(t, h, 100, 200)

	// Assert
	if partial := resp.Msg.GetSuccess().GetCoverage().GetPartial(); partial == nil || partial.GetRecordedSinceMs() != 150_000 {
		t.Fatalf("coverage = %v; want partial, recorded since 150s", resp.Msg.GetSuccess().GetCoverage())
	}
}

func TestListMergeLandingsCountsAWindowStartingAsRecordingBeganComplete(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Merge.landings = merge.LandingsWindow{Repository: wsm.Repository{ID: "repo-1"}, RecordingBegan: time.Unix(100, 0)}

	// Act
	resp := listLandings(t, h, 100, 200)

	// Assert
	if resp.Msg.GetSuccess().GetCoverage().GetComplete() == nil {
		t.Fatalf("coverage = %v; want complete for a window starting the instant recording began", resp.Msg.GetSuccess().GetCoverage())
	}
}

func TestListMergeLandingsEndsAnUnsetWindowNow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	before := time.Now()

	// Act
	resp := listLandings(t, h, 100, 0)

	// Assert
	until := time.UnixMilli(resp.Msg.GetSuccess().GetWindow().GetUntilMs())
	if until.Before(before.Truncate(time.Millisecond)) || until.After(time.Now()) {
		t.Fatalf("window ends %v; want the read's instant, after %v", until, before)
	}
}

func TestListMergeLandingsAnswersAnUnknownRepository(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Merge.landingsErr = &merge.RefusalError{Arm: merge.ArmUnknownRepository, Reason: "no such repository"}

	// Act
	resp := listLandings(t, h, 100, 200)

	// Assert
	if resp.Msg.GetError().GetUnknownRepository() == nil {
		t.Fatalf("ListMergeLandings = %v; want unknown_repository", resp.Msg)
	}
}

func TestListMergeLandingsRefusesAMalformedWindow(t *testing.T) {
	tests := []struct {
		name         string
		since, until int64
	}{
		{name: "no start", since: 0, until: 200},
		{name: "an end before the start", since: 200, until: 100},
		{name: "an unset end with a start in the future", since: time.Now().Add(time.Hour).Unix()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			req := &agentreplv1.ListMergeLandingsRequest{Repository: &workspacev1.RepositoryRef{Id: "repo-1"}, SinceMs: tt.since * 1000}
			if tt.until != 0 {
				ms := tt.until * 1000
				req.UntilMs = &ms
			}

			// Act
			_, err := h.Client.ListMergeLandings(context.Background(), connect.NewRequest(req))

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Merge.landingsAsked) != 0 {
				t.Fatalf("ListMergeLandings = %v (asked %d); want InvalidArgument before the orchestrator is asked", err, len(h.Merge.landingsAsked))
			}
		})
	}
}

func TestRecordDirectLandingReportsTheCallerAndAnswersThePath(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Merge.directPath = wsm.LandingQueueBypass

	// Act
	resp, err := h.Client.RecordDirectLanding(context.Background(), connect.NewRequest(&agentreplv1.RecordDirectLandingRequest{
		Repository: &workspacev1.RepositoryRef{Dir: "/repo"}, Branch: "feature", LandedHead: "abc", Caller: ref()}))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetPath().GetQueueBypass() == nil {
		t.Fatalf("RecordDirectLanding = %v, %v; want the queue-bypass path", resp, err)
	}
	if r := h.Merge.directReports; len(r) != 1 || r[0].Caller != testWorkspaceID || r[0].Branch != "feature" || r[0].Head != "abc" || r[0].Repository.Dir != "/repo" {
		t.Fatalf("reports %+v; want the caller's landing of feature at abc", r)
	}
}

func TestRecordDirectLandingWithNoCallerNamesNone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Merge.directPath = wsm.LandingDirect

	// Act
	resp, err := h.Client.RecordDirectLanding(context.Background(), connect.NewRequest(&agentreplv1.RecordDirectLandingRequest{
		Repository: &workspacev1.RepositoryRef{Dir: "/repo"}, Branch: "feature", LandedHead: "abc"}))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetPath().GetDirect() == nil || len(h.Merge.directReports) != 1 || h.Merge.directReports[0].Caller != "" {
		t.Fatalf("RecordDirectLanding = %v, %v (reports %+v); want a direct landing naming no caller", resp, err, h.Merge.directReports)
	}
}

func TestRecordDirectLandingRefusesAnUnknownCaller(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.RecordDirectLanding(context.Background(), connect.NewRequest(&agentreplv1.RecordDirectLandingRequest{
		Repository: &workspacev1.RepositoryRef{Dir: "/repo"}, Branch: "feature", LandedHead: "abc", Caller: &workspacev1.WorkspaceRef{Id: "ws-nobody"}}))

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownCaller() == nil || len(h.Merge.directReports) != 0 {
		t.Fatalf("RecordDirectLanding = %v, %v; want unknown_caller with nothing reported", resp, err)
	}
}

func TestRecordDirectLandingRefusesAMalformedReport(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.RecordDirectLandingRequest
	}{
		{name: "no repository", req: &agentreplv1.RecordDirectLandingRequest{Branch: "feature", LandedHead: "abc"}},
		{name: "no branch", req: &agentreplv1.RecordDirectLandingRequest{Repository: &workspacev1.RepositoryRef{Dir: "/repo"}, LandedHead: "abc"}},
		{name: "no head", req: &agentreplv1.RecordDirectLandingRequest{Repository: &workspacev1.RepositoryRef{Dir: "/repo"}, Branch: "feature"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.RecordDirectLanding(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Merge.directReports) != 0 {
				t.Fatalf("RecordDirectLanding = %v; want InvalidArgument with nothing reported", err)
			}
		})
	}
}
