package server

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"sort"
	"strings"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// findRosterRow is a roster row of the workspace REF, drawn with STATUS.
func findRosterRow(ref *workspacev1.WorkspaceRef, status func(*frontendv1.RosterRow)) *frontendv1.RosterRow {
	row := &frontendv1.RosterRow{Workspace: &frontendv1.RosterRowWorkspace{Workspace: ref}}
	if status != nil {
		status(row)
	}
	return row
}

// findRoster is the push the find harness publishes: the open workspace
// drawn thinking in the open section. OPENSTATUS overrides the open row's
// status, nil drawing it with none. The landed workspace is in no stream: its
// row is the sidebar's merged rows' (landedRow).
func findRoster(openStatus func(*frontendv1.RosterRow)) *frontendv1.RosterPush {
	open := findRosterRow(&workspacev1.WorkspaceRef{Id: "ws-open", Dir: "/src/open"}, openStatus)
	return &frontendv1.RosterPush{
		Revision: 7,
		Workspaces: &frontendv1.WorkspaceRoster{Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{{
			Entries: rosterwalk.RowEntries([]*frontendv1.RosterOpenRow{{Row: open}})}}}},
		MergeQueue:     &frontendv1.RosterMergeQueueSection{},
		RecentlyMerged: &frontendv1.RosterMergedSection{},
		Closed:         &frontendv1.RosterClosedRows{},
	}
}

// landedRow is the landed workspace's row as a merged page draws it: done.
func landedRow() *frontendv1.RosterRow {
	return findRosterRow(&workspacev1.WorkspaceRef{Id: "ws-landed", Dir: "/src/landed"}, func(r *frontendv1.RosterRow) {
		r.Status = &frontendv1.RosterRow_Done{Done: &frontendv1.RosterRowStatusDone{}}
	})
}

// thinking draws a roster row with the thinking status.
func thinking(r *frontendv1.RosterRow) {
	r.Status = &frontendv1.RosterRow_Thinking{Thinking: &frontendv1.RosterRowStatusThinking{}}
}

// findHarness is a harness whose registry holds one repository with an open
// workspace (summarized) and a landed one, in place of the default workspace,
// and whose roster draws both (findRoster).
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
	h.DB.summaries = map[ids.WorkspaceID]wsm.WorkspaceSummary{"ws-open": {Long: "The footer stalls. Fix its detail.", Short: "Fix the footer"}}
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{
		"ws-open":   {Workspace: "ws-open", ConfigDir: "/home/.claude-work"},
		"ws-landed": {Workspace: "ws-landed", ConfigDir: "/home/.claude"},
	}
	h.DB.turnStarts = []wsm.TurnStart{
		{Workspace: "ws-landed", StartedAt: time.UnixMilli(2_000), Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT},
		{Workspace: "ws-open", StartedAt: time.UnixMilli(8_000), Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT},
		{Workspace: "ws-open", StartedAt: time.UnixMilli(9_000), Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT},
	}
	h.Sidebar.topic.Publish(findRoster(thinking))
	h.Sidebar.merged = []*frontendv1.RosterRow{landedRow()}
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
	if open.GetName() != "open-one" || open.GetShortSummary() != "Fix the footer" || open.GetLongSummary() != "The footer stalls. Fix its detail." || open.GetLastActiveAtMs() != 9_000 ||
		open.GetStanding().GetOpen() == nil || open.GetRepository().GetDir() != "/src/repo" || open.GetWorkspace().GetDir() != "/src/open" {
		t.Fatalf("first = %v, want the open workspace with its summary, most recently active first", open)
	}
	if landed.GetName() != "landed-one" || landed.ShortSummary != nil || landed.LongSummary != nil || landed.GetStanding().GetLanded().GetLandedAtMs() != 5_000 || landed.GetCreatedAtMs() != 1_000 {
		t.Fatalf("second = %v, want the landed workspace with no summary", landed)
	}
}

func TestFindWorkspacesAnswersEachWorkspacesConfigDirAndLastAgentTurn(t *testing.T) {
	// Arrange
	h := findHarness(t)
	h.DB.workspaces["ws-new"] = wsm.Workspace{ID: "ws-new", Repo: "repo-1", Dir: "/src/new", Name: "new-one", CreatedAt: time.UnixMilli(500)}

	// Act
	resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{}))

	// Assert
	if err != nil {
		t.Fatalf("FindWorkspaces: %v", err)
	}
	got := foundByName(resp)
	if open := got["open-one"]; open.GetConfigDir() != "/home/.claude-work" || open.GetLastTurnAtMs() != 8_000 {
		t.Fatalf("open = %v, want the work root and its last agent turn at 8000, not its compaction", open)
	}
	if fresh := got["new-one"]; fresh == nil || fresh.ConfigDir != nil || fresh.LastTurnAtMs != nil {
		t.Fatalf("new = %v, want no config dir and no last turn", fresh)
	}
}

// TestFindWorkspacesAnswersTheVendorTitleWhereNoSummaryStands pins the
// topbar's precedence: a workspace closed before its agent set a summary is
// found by the vendor's title its topbar drew, and a summary outranks it.
func TestFindWorkspacesAnswersTheVendorTitleWhereNoSummaryStands(t *testing.T) {
	// Arrange
	h := findHarness(t)
	h.DB.titles = map[ids.WorkspaceID]string{"ws-open": "Footer status work", "ws-landed": "Landed topbar work"}

	// Act
	resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{}))

	// Assert
	if err != nil {
		t.Fatalf("FindWorkspaces: %v", err)
	}
	got := foundByName(resp)
	if got["open-one"].GetShortSummary() != "Fix the footer" || got["landed-one"].GetShortSummary() != "Landed topbar work" || got["landed-one"].LongSummary != nil {
		t.Fatalf("summaries = %q, %q; want the agent's summary, then the vendor's title with no long summary", got["open-one"].GetShortSummary(), got["landed-one"].GetShortSummary())
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
		{name: "a turn window", req: &agentreplv1.FindWorkspacesRequest{Turned: &agentreplv1.FindWorkspacesTurnWindow{SinceMs: ptrTo(int64(1_000)), UntilMs: ptrTo(int64(3_000))}}, want: []string{"landed-one"}},
		{name: "a turn window holding only a compaction", req: &agentreplv1.FindWorkspacesRequest{Turned: &agentreplv1.FindWorkspacesTurnWindow{SinceMs: ptrTo(int64(8_500))}}, want: nil},
		{name: "a turn window until alone", req: &agentreplv1.FindWorkspacesRequest{Turned: &agentreplv1.FindWorkspacesTurnWindow{UntilMs: ptrTo(int64(8_000))}}, want: []string{"open-one", "landed-one"}},
		{name: "ended since after the landing keeps the open one", req: &agentreplv1.FindWorkspacesRequest{EndedSinceMs: ptrTo(int64(5_001))}, want: []string{"open-one"}},
		{name: "ended since at the landing keeps both", req: &agentreplv1.FindWorkspacesRequest{EndedSinceMs: ptrTo(int64(5_000))}, want: []string{"open-one", "landed-one"}},
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

func TestFindWorkspacesMarksAMergeWorkspace(t *testing.T) {
	tests := []struct {
		name      string
		requester ids.WorkspaceID
		// wantRequester is the requester's dir the marker names, "" for none.
		wantRequester string
	}{
		{name: "a requester the registry holds is named", requester: "ws-open", wantRequester: "/src/open"},
		{name: "a forgotten requester is answered as none", requester: "ws-forgotten", wantRequester: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a hidden merge workspace the roster draws no row of.
			h := findHarness(t)
			h.DB.workspaces["ws-merge"] = wsm.Workspace{ID: "ws-merge", Repo: "repo-1", Dir: "/src/merge", Name: "merge-one", Branch: "merge-one",
				CreatedAt: time.UnixMilli(1_000), Backlogged: true, MergeOf: &wsm.MergeOrigin{Requester: tt.requester}}

			// Act
			resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{Text: ptrTo("merge-one")}))

			// Assert
			if err != nil {
				t.Fatalf("FindWorkspaces: %v", err)
			}
			found := resp.Msg.GetSuccess().GetComplete().GetWorkspaces()
			if len(found) != 1 || found[0].GetWorkspace().GetId() != "ws-merge" || found[0].Merge == nil || found[0].Roster != nil {
				t.Fatalf("found = %v, want the merge workspace alone, marked, drawn on no roster", found)
			}
			if got := found[0].GetMerge().GetRequester().GetDir(); got != tt.wantRequester {
				t.Fatalf("merge requester dir = %q, want %q", got, tt.wantRequester)
			}
		})
	}
}

func TestFindWorkspacesLeavesAnOrdinaryWorkspaceUnmarked(t *testing.T) {
	// Arrange
	h := findHarness(t)

	// Act
	resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{}))

	// Assert
	if err != nil {
		t.Fatalf("FindWorkspaces: %v", err)
	}
	for _, f := range resp.Msg.GetSuccess().GetComplete().GetWorkspaces() {
		if f.Merge != nil {
			t.Fatalf("%s answered a merge marker, want none on an ordinary workspace", f.GetName())
		}
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

func TestFindWorkspacesByParentAnswersOnlyItsChildren(t *testing.T) {
	// Arrange: the landed workspace is the open one's child.
	h := findHarness(t)
	landed := h.DB.workspaces["ws-landed"]
	parent := ids.WorkspaceID("ws-open")
	landed.Parent = &parent
	h.DB.workspaces["ws-landed"] = landed

	// Act
	resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{
		Parent: &workspacev1.WorkspaceRef{Id: "ws-open", Dir: "/src/open"}}))

	// Assert
	found := resp.Msg.GetSuccess().GetComplete().GetWorkspaces()
	if err != nil || len(found) != 1 || found[0].GetName() != "landed-one" || found[0].GetStanding().GetLanded().GetLandedAtMs() != 5_000 {
		t.Fatalf("FindWorkspaces = %v, %v; want only the landed child, landed at 5000", resp, err)
	}
}

func TestFindWorkspacesByAnUnknownParentIsAnAnswer(t *testing.T) {
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
			resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{Parent: tt.ref}))

			// Assert
			if err != nil || resp.Msg.GetError().GetUnknownParent() == nil {
				t.Fatalf("FindWorkspaces = %v, %v; want unknown_parent", resp, err)
			}
		})
	}
}

func TestFindWorkspacesAnswersHowAClosedWorkspaceEnded(t *testing.T) {
	cases := []struct {
		name    string
		closure *wsm.Closure
		fold    ids.WorkspaceID
		want    func(*agentreplv1.FoundWorkspaceClosedHow) bool
	}{
		{name: "a close", closure: &wsm.Closure{At: time.UnixMilli(3_000), By: wsm.CloseByAgent, How: wsm.HowClosed},
			want: func(h *agentreplv1.FoundWorkspaceClosedHow) bool { return h.GetClosed() != nil }},
		{name: "a kill", closure: &wsm.Closure{At: time.UnixMilli(3_000), By: wsm.CloseByUser, How: wsm.HowKilled},
			want: func(h *agentreplv1.FoundWorkspaceClosedHow) bool { return h.GetKilled() != nil }},
		{name: "a close recorded before its kind", closure: &wsm.Closure{At: time.UnixMilli(3_000), By: wsm.CloseByUser, How: wsm.HowUnrecorded},
			want: func(h *agentreplv1.FoundWorkspaceClosedHow) bool { return h.GetUnrecorded() != nil }},
		{name: "a close with no record", want: func(h *agentreplv1.FoundWorkspaceClosedHow) bool { return h.GetUnrecorded() != nil }},
		{name: "a fold names the child it went into", closure: &wsm.Closure{At: time.UnixMilli(3_000), By: wsm.CloseByDaemon, How: wsm.HowClosed}, fold: "ws-open",
			want: func(h *agentreplv1.FoundWorkspaceClosedHow) bool { return h.GetFolded().GetInto().GetDir() == "/src/open" }},
		{name: "a fold into a forgotten child names none", closure: &wsm.Closure{At: time.UnixMilli(3_000), By: wsm.CloseByDaemon, How: wsm.HowClosed}, fold: "ws-gone",
			want: func(h *agentreplv1.FoundWorkspaceClosedHow) bool { return h.GetFolded() != nil && h.GetFolded().Into == nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := findHarness(t)
			h.DB.workspaces["ws-closed"] = wsm.Workspace{ID: "ws-closed", Repo: "repo-1", Dir: "/src/closed", Name: "closed-one",
				CreatedAt: time.UnixMilli(1_000), Closed: true, Closure: tc.closure}
			if tc.fold != "" {
				h.DB.folds = map[ids.WorkspaceID]ids.WorkspaceID{"ws-closed": tc.fold}
			}

			// Act
			resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{Text: ptrTo("closed-one")}))

			// Assert
			found := resp.Msg.GetSuccess().GetComplete().GetWorkspaces()
			if err != nil || len(found) != 1 || !tc.want(found[0].GetStanding().GetClosed().GetHow()) {
				t.Fatalf("FindWorkspaces = %v, %v; want %s", resp, err, tc.name)
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
		{name: "a caller ref naming no workspace", req: &agentreplv1.FindWorkspacesRequest{Caller: &workspacev1.WorkspaceRef{}}},
		{name: "a reversed turn window", req: &agentreplv1.FindWorkspacesRequest{Turned: &agentreplv1.FindWorkspacesTurnWindow{SinceMs: ptrTo(int64(2)), UntilMs: ptrTo(int64(1))}}},
		{name: "a parent ref naming no workspace", req: &agentreplv1.FindWorkspacesRequest{Parent: &workspacev1.WorkspaceRef{}}},
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
		{name: "the registry", edit: func(d *fakeDB) { d.readRegistryErr = errors.New("disk gone") }, cause: "read the registry: disk gone"},
		{name: "the summaries", edit: func(d *fakeDB) { d.summariesErr = errors.New("disk gone") }, cause: "list the workspace summaries: disk gone"},
		{name: "the vendor titles", edit: func(d *fakeDB) { d.titlesErr = errors.New("disk gone") }, cause: "list the vendor titles: disk gone"},
		{name: "the sessions", edit: func(d *fakeDB) { d.sessionErr = errors.New("disk gone") }, cause: "list the sessions: disk gone"},
		{name: "the turn starts", edit: func(d *fakeDB) { d.turnStartsErr = errors.New("disk gone") }, cause: "list the turn starts: disk gone"},
		{name: "the folded production children", edit: func(d *fakeDB) { d.foldsErr = errors.New("disk gone") }, cause: "list the folded production children: disk gone"},
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

func TestFindWorkspacesReadsTheRegistryAsOneSnapshot(t *testing.T) {
	// Arrange: the separate workspace and repository reads both fail, so
	// only the one snapshot can answer.
	h := findHarness(t)
	h.DB.listWorkspacesErr = errors.New("a separate read")
	h.DB.listRepositoriesErr = errors.New("a separate read")

	// Act
	resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{}))

	// Assert
	if err != nil {
		t.Fatalf("FindWorkspaces = %v, want it answered from the registry snapshot alone", err)
	}
	if got := len(resp.Msg.GetSuccess().GetComplete().GetWorkspaces()); got != 2 {
		t.Fatalf("found %d workspaces, want both", got)
	}
}

// closedUndrawn adds a closed workspace the roster draws no row of.
func closedUndrawn(h *harness) {
	h.DB.workspaces["ws-closed"] = wsm.Workspace{ID: "ws-closed", Repo: "repo-1", Dir: "/src/closed", Name: "closed-one",
		CreatedAt: time.UnixMilli(1_000), Closed: true}
}

// foundByName answers RESP's records by name.
func foundByName(resp *connect.Response[agentreplv1.FindWorkspacesResponse]) map[string]*agentreplv1.FoundWorkspace {
	out := map[string]*agentreplv1.FoundWorkspace{}
	for _, f := range resp.Msg.GetSuccess().GetComplete().GetWorkspaces() {
		out[f.GetName()] = f
	}
	return out
}

func TestFindWorkspacesAnswersHowTheRosterDrawsEachWorkspace(t *testing.T) {
	// Arrange
	h := findHarness(t)
	closedUndrawn(h)

	// Act
	resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{}))

	// Assert
	if err != nil {
		t.Fatalf("FindWorkspaces: %v", err)
	}
	found := foundByName(resp)
	if r := found["open-one"].GetRoster(); r.GetSection().GetOpen() == nil || r.GetStatus().GetArm() != "thinking" {
		t.Fatalf("open-one roster = %v, want the open section, thinking", r)
	}
	if r := found["landed-one"].GetRoster(); r.GetSection().GetMerged() == nil || r.GetStatus().GetArm() != "done" {
		t.Fatalf("landed-one roster = %v, want the merged section, done", r)
	}
	if f := found["closed-one"]; f == nil || f.Roster != nil {
		t.Fatalf("closed-one = %v, want it answered with no roster", f)
	}
}

func TestFindWorkspacesOnRosterAnswersOnlyTheDrawnWorkspaces(t *testing.T) {
	// Arrange
	h := findHarness(t)
	closedUndrawn(h)

	// Act
	resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{
		OnRoster: &agentreplv1.FindWorkspacesOnRoster{}}))

	// Assert
	if err != nil {
		t.Fatalf("FindWorkspaces: %v", err)
	}
	found := foundByName(resp)
	if len(found) != 2 || found["open-one"] == nil || found["landed-one"] == nil {
		t.Fatalf("found = %v, want only the two drawn workspaces", resp.Msg)
	}
}

func TestFindWorkspacesAnswersCallingOnlyForTheCaller(t *testing.T) {
	tests := []struct {
		name        string
		caller      *workspacev1.WorkspaceRef
		wantCalling map[string]bool
	}{
		{name: "the caller's own record", caller: &workspacev1.WorkspaceRef{Id: "ws-open", Dir: "/src/open"},
			wantCalling: map[string]bool{"open-one": true, "landed-one": false}},
		{name: "no caller", caller: nil, wantCalling: map[string]bool{"open-one": false, "landed-one": false}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := findHarness(t)

			// Act
			resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{Caller: tt.caller}))

			// Assert
			if err != nil {
				t.Fatalf("FindWorkspaces: %v", err)
			}
			found := foundByName(resp)
			for name, want := range tt.wantCalling {
				if got := found[name].GetCalling(); got != want {
					t.Fatalf("%s calling = %v, want %v", name, got, want)
				}
			}
		})
	}
}

func TestFindWorkspacesByAnUnknownCallerIsAnAnswer(t *testing.T) {
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
			resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{Caller: tt.ref}))

			// Assert
			if err != nil || resp.Msg.GetError().GetUnknownCaller() == nil {
				t.Fatalf("FindWorkspaces = %v, %v; want unknown_caller", resp, err)
			}
		})
	}
}

func TestFindWorkspacesFailsAndRecordsAnUnreadableRoster(t *testing.T) {
	tests := []struct {
		name    string
		publish func(*harness)
		cause   string
	}{
		{name: "no roster published yet", publish: func(h *harness) { h.Sidebar.topic.ClearIf(func(*frontendv1.RosterPush) bool { return true }) },
			cause: "read the roster: the daemon has published no roster yet"},
		{name: "a row with no status arm", publish: func(h *harness) { h.Sidebar.topic.Publish(findRoster(nil)) },
			cause: "workspacefind: the roster's open row of workspace ws-open sets no status arm"},
		{name: "a merged row with no status arm", publish: func(h *harness) {
			h.Sidebar.merged = []*frontendv1.RosterRow{findRosterRow(&workspacev1.WorkspaceRef{Id: "ws-landed", Dir: "/src/landed"}, nil)}
		}, cause: "workspacefind: the roster's merged row of workspace ws-landed sets no status arm"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := findHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
			tt.publish(h)

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

func TestRegisteredRefAnswersTheRecordARefNames(t *testing.T) {
	tests := []struct {
		name   string
		ref    *workspacev1.WorkspaceRef
		wantOK bool
	}{
		{name: "an id the registry holds, no dir", ref: &workspacev1.WorkspaceRef{Id: "ws-1"}, wantOK: true},
		{name: "an id and its own dir", ref: &workspacev1.WorkspaceRef{Id: "ws-1", Dir: "/src/one"}, wantOK: true},
		{name: "an id the registry does not hold", ref: &workspacev1.WorkspaceRef{Id: "ws-gone"}, wantOK: false},
		{name: "a dir that disagrees with the id's", ref: &workspacev1.WorkspaceRef{Id: "ws-1", Dir: "/src/other"}, wantOK: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			byID := map[wsm.WorkspaceID]wsm.Workspace{"ws-1": {ID: "ws-1", Dir: "/src/one"}}

			// Act
			got, ok := registeredRef(byID, tt.ref)

			// Assert
			if ok != tt.wantOK || (ok && got.ID != "ws-1") || (!ok && got.ID != "") {
				t.Fatalf("registeredRef = %+v, %v; want ok %v", got, ok, tt.wantOK)
			}
		})
	}
}

// loginHarness is findHarness with a second repository routed to another
// login, holding a child of ws-open and a workspace of its own, while
// repo-1 is routed to the work login.
func loginHarness(t *testing.T, opts ...option) *harness {
	t.Helper()
	h := findHarness(t, opts...)
	h.DB.repositories = append(h.DB.repositories, wsm.Repository{ID: "repo-2", Dir: "/src/personal"})
	parent := ids.WorkspaceID("ws-open")
	h.DB.workspaces["ws-child"] = wsm.Workspace{ID: "ws-child", Repo: "repo-2", Dir: "/src/child", Name: "child-one", Branch: "child-one",
		CreatedAt: time.UnixMilli(2_000), Parent: &parent}
	h.DB.workspaces["ws-personal"] = wsm.Workspace{ID: "ws-personal", Repo: "repo-2", Dir: "/src/mine", Name: "personal-one", Branch: "personal-one",
		CreatedAt: time.UnixMilli(3_000)}
	h.AccountRoots = map[string]string{"/src/repo": "/home/.claude-work"}
	return h
}

// loginOf is a login narrowing naming REF.
func loginOf(ref *workspacev1.WorkspaceRef) *agentreplv1.FindWorkspacesLogin {
	return &agentreplv1.FindWorkspacesLogin{Login: &agentreplv1.FindWorkspacesLogin_OfWorkspace{OfWorkspace: ref}}
}

func TestFindWorkspacesByLoginKeepsItsRepositoriesAndTheirChildren(t *testing.T) {
	tests := []struct {
		name string
		ref  *workspacev1.WorkspaceRef
		want []string
	}{
		{name: "a work workspace's login keeps work workspaces and their children", ref: &workspacev1.WorkspaceRef{Id: "ws-open"},
			want: []string{"child-one", "landed-one", "open-one"}},
		{name: "a child names the login of its own repository, not its parent's", ref: &workspacev1.WorkspaceRef{Id: "ws-child"},
			want: []string{"child-one", "personal-one"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := loginHarness(t)

			// Act
			resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{Login: loginOf(tt.ref)}))

			// Assert
			if err != nil {
				t.Fatalf("FindWorkspaces: %v", err)
			}
			var got []string
			for name := range foundByName(resp) {
				got = append(got, name)
			}
			sort.Strings(got)
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("found %v, want %v", got, tt.want)
			}
		})
	}
}

// loginRoot is a login narrowing naming the account root ROOT.
func loginRoot(root string) *agentreplv1.FindWorkspacesLogin {
	return &agentreplv1.FindWorkspacesLogin{Login: &agentreplv1.FindWorkspacesLogin_AccountRoot{AccountRoot: root}}
}

// TestFindWorkspacesByLoginRootMatchesTheLoginItsWorkspaceWouldName pins that
// naming a login by its root answers exactly what naming it by one of its
// workspaces does: one meaning of a login, two ways to name it.
func TestFindWorkspacesByLoginRootMatchesTheLoginItsWorkspaceWouldName(t *testing.T) {
	tests := []struct {
		name string
		root string
		ref  *workspacev1.WorkspaceRef
		want []string
	}{
		{name: "the work root", root: "/home/.claude-work", ref: &workspacev1.WorkspaceRef{Id: "ws-open"},
			want: []string{"child-one", "landed-one", "open-one"}},
		{name: "the default root", root: defaultAccountRoot, ref: &workspacev1.WorkspaceRef{Id: "ws-child"},
			want: []string{"child-one", "personal-one"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := loginHarness(t)
			names := func(resp *connect.Response[agentreplv1.FindWorkspacesResponse]) []string {
				var got []string
				for name := range foundByName(resp) {
					got = append(got, name)
				}
				sort.Strings(got)
				return got
			}

			// Act
			byRoot, rootErr := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{Login: loginRoot(tt.root)}))
			byWorkspace, wsErr := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{Login: loginOf(tt.ref)}))

			// Assert
			if rootErr != nil || wsErr != nil {
				t.Fatalf("FindWorkspaces: %v, %v", rootErr, wsErr)
			}
			if got := names(byRoot); !reflect.DeepEqual(got, tt.want) || !reflect.DeepEqual(got, names(byWorkspace)) {
				t.Fatalf("by root %v, by workspace %v; want both %v", got, names(byWorkspace), tt.want)
			}
		})
	}
}

func TestFindWorkspacesByAnUnknownLoginRootIsAnAnswer(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	h := loginHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })

	// Act
	resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{Login: loginRoot("/home/.claude-gone")}))

	// Assert
	unknown := resp.Msg.GetError().GetUnknownLoginRoot()
	if err != nil || unknown == nil || !reflect.DeepEqual(unknown.GetKnown(), h.KnownAccountRoots) {
		t.Fatalf("FindWorkspaces = %v, %v; want unknown_login_root naming the known roots %v", resp, err, h.KnownAccountRoots)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: "FindWorkspaces"})
	if !ok || rec.Context["arm"] != "unknown_login_root" || !strings.Contains(fmt.Sprint(rec.Context["reason"]), "/home/.claude-gone") {
		t.Fatalf("INFO record = %v (%v), want the unknown_login_root refusal naming the root", rec.Context, ok)
	}
}

func TestFindWorkspacesByTheLoginOfAnUnknownWorkspaceIsAnAnswer(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	h := loginHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })

	// Act
	resp, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{
		Login: loginOf(&workspacev1.WorkspaceRef{Id: "ws-gone"})}))

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownLoginWorkspace() == nil {
		t.Fatalf("FindWorkspaces = %v, %v; want unknown_login_workspace", resp, err)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: "FindWorkspaces"})
	if !ok || !strings.Contains(fmt.Sprint(rec.Context), "ws-gone") {
		t.Fatalf("INFO record = %v (%v), want the refusal naming the unknown workspace", rec.Context, ok)
	}
}

func TestFindWorkspacesRefusesAMalformedLogin(t *testing.T) {
	tests := []struct {
		name  string
		login *agentreplv1.FindWorkspacesLogin
	}{
		{name: "a login with no arm", login: &agentreplv1.FindWorkspacesLogin{}},
		{name: "a login naming an empty workspace ref", login: loginOf(&workspacev1.WorkspaceRef{})},
		{name: "a login naming a blank root", login: loginRoot(" ")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := loginHarness(t)

			// Act
			_, err := h.Client.FindWorkspaces(context.Background(), connect.NewRequest(&agentreplv1.FindWorkspacesRequest{Login: tt.login}))

			// Assert
			if connectCode(t, err) != connect.CodeInvalidArgument {
				t.Fatalf("err = %v, want InvalidArgument", err)
			}
		})
	}
}
