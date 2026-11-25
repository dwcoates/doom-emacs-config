package workspacefind

import (
	"maps"
	"reflect"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/wsm"
)

// at is a fixed instant DAYS days into the fixture's calendar.
func at(days int) time.Time {
	return time.Date(2026, 10, 1, 12, 0, 0, 0, time.UTC).AddDate(0, 0, days)
}

// ptr answers a pointer to V.
func ptr[T any](v T) *T { return &v }

var (
	doom  = wsm.Repository{ID: "repo-doom", Dir: "/src/doom"}
	other = wsm.Repository{ID: "repo-other", Dir: "/src/other"}
)

// record is a workspace of REPO created on day CREATED.
func record(id, name string, repo wsm.Repository, created int) wsm.Workspace {
	return wsm.Workspace{ID: wsm.WorkspaceID(id), Kind: wsm.WorkspaceKindRepository, Repo: repo.ID, Name: name, Branch: name, CreatedAt: at(created)}
}

// ids answers the found workspaces' ids, in order.
func ids(r Result) []string {
	var out []string
	for _, f := range r.Found {
		out = append(out, string(f.Workspace.ID))
	}
	return out
}

const (
	workRoot     = "/home/u/.claude-work"
	personalRoot = "/home/u/.claude"
)

// turn is a turn of WS of ORIGIN with TEXT, started on day DAY.
func turn(ws string, origin conversationv1.PromptOrigin, text string, day int) wsm.TurnStart {
	return wsm.TurnStart{Workspace: wsm.WorkspaceID(ws), StartedAt: at(day), Origin: origin, CommandText: text}
}

// fixture is three workspaces of two repositories: one open, one closed, one
// landed, with summaries on two. The open one runs under the work root and
// took an agent turn on day 9; the landed one runs under the personal root and
// took one on day 5; the closed one never came up, and its only turns are a
// restart's re-drive and a typed /clear.
func fixture() Records {
	open := record("open", "footer-fix", doom, 0)
	open.LastActivityAt = ptr(at(9))
	closed := record("closed", "topbar-title", doom, 2)
	closed.Closed = true
	closed.Closure = &wsm.Closure{At: at(4), By: wsm.CloseByUser}
	landed := record("landed", "store-prune", other, 5)
	landed.Closed = true
	landed.MergedAt = ptr(at(6))
	creator := wsm.WorkspaceID("open")
	landed.RequestedBy = &creator
	landed.Parent = &creator
	summaries := map[wsm.WorkspaceID]wsm.WorkspaceSummary{
		"open":   {Long: "The footer's clock drifts. Fix its tick.", Short: "Fix the footer's clock"},
		"landed": {Long: "Old rows pile up. Prune them on a schedule.", Short: "Prune the store's old rows"},
	}
	return Records{
		Workspaces:   []wsm.Workspace{open, closed, landed},
		Repositories: []wsm.Repository{doom, other},
		Summaries:    summaries,
		Sessions: map[wsm.WorkspaceID]wsm.Session{
			"open":   {Workspace: "open", ConfigDir: workRoot},
			"landed": {Workspace: "landed", ConfigDir: personalRoot},
			"closed": {Workspace: "closed", SelectedConfigDir: workRoot},
		},
		Turns: []wsm.TurnStart{
			turn("open", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, "", 1),
			turn("open", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, "", 9),
			turn("open", conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT, "/compact", 10),
			turn("landed", conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT, "", 5),
			turn("closed", conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART, "", 3),
			turn("closed", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, "/clear", 3),
		},
	}
}

func TestFindNarrowsByEveryCriterion(t *testing.T) {
	tests := []struct {
		name  string
		query Query
		want  []string
	}{
		{name: "nothing set finds every workspace, most recently active first", query: Query{}, want: []string{"open", "landed", "closed"}},
		{name: "a repository", query: Query{Repository: ptr(other.ID)}, want: []string{"landed"}},
		{name: "open", query: Query{Standing: OpenStanding}, want: []string{"open"}},
		{name: "closed includes landed", query: Query{Standing: ClosedStanding}, want: []string{"landed", "closed"}},
		{name: "landed", query: Query{Standing: LandedStanding}, want: []string{"landed"}},
		{name: "since drops a workspace last active before it", query: Query{Since: ptr(at(5))}, want: []string{"open", "landed"}},
		{name: "since is inclusive", query: Query{Since: ptr(at(4))}, want: []string{"open", "landed", "closed"}},
		{name: "until drops a workspace created after it", query: Query{Until: ptr(at(3))}, want: []string{"open", "closed"}},
		{name: "a window overlaps a workspace's life", query: Query{Since: ptr(at(3)), Until: ptr(at(3))}, want: []string{"open", "closed"}},
		{name: "text matches a name case-insensitively", query: Query{Text: ptr("TOPBAR")}, want: []string{"closed"}},
		{name: "text matches a short summary", query: Query{Text: ptr("clock")}, want: []string{"open"}},
		{name: "text matches a long summary", query: Query{Text: ptr("schedule")}, want: []string{"landed"}},
		{name: "text matching nothing finds nothing", query: Query{Text: ptr("nowhere")}, want: nil},
		{name: "criteria combine", query: Query{Repository: ptr(doom.ID), Standing: ClosedStanding}, want: []string{"closed"}},
		{name: "a creator keeps only what it asked for", query: Query{CreatedBy: ptr(wsm.WorkspaceID("open"))}, want: []string{"landed"}},
		{name: "a creator that asked for nothing finds nothing", query: Query{CreatedBy: ptr(wsm.WorkspaceID("closed"))}, want: nil},
		{name: "a parent keeps only its children", query: Query{Parent: ptr(wsm.WorkspaceID("open"))}, want: []string{"landed"}},
		{name: "a parent with no children finds nothing", query: Query{Parent: ptr(wsm.WorkspaceID("closed"))}, want: nil},
		{name: "ended since keeps the open and those ended at or after it", query: Query{EndedSince: ptr(at(6))}, want: []string{"open", "landed"}},
		{name: "ended since drops a workspace closed before it", query: Query{EndedSince: ptr(at(5))}, want: []string{"open", "landed"}},
		{name: "ended since keeps a workspace closed at it", query: Query{EndedSince: ptr(at(4))}, want: []string{"open", "landed", "closed"}},
		{name: "ended since judges a landed workspace by its landing", query: Query{EndedSince: ptr(at(7))}, want: []string{"open"}},
		{name: "a parent and ended since combine", query: Query{Parent: ptr(wsm.WorkspaceID("open")), EndedSince: ptr(at(7))}, want: nil},
		{name: "a turn window keeps a workspace with an agent turn inside it", query: Query{Turned: &TurnWindow{Since: ptr(at(4)), Until: ptr(at(6))}}, want: []string{"landed"}},
		{name: "a turn window is inclusive at both ends", query: Query{Turned: &TurnWindow{Since: ptr(at(5)), Until: ptr(at(9))}}, want: []string{"open", "landed"}},
		{name: "a turn window since alone is open-ended up to now", query: Query{Turned: &TurnWindow{Since: ptr(at(6))}}, want: []string{"open"}},
		{name: "a turn window until alone is open-ended into the past", query: Query{Turned: &TurnWindow{Until: ptr(at(4))}}, want: []string{"open"}},
		{name: "a turn window with neither end keeps every workspace that took an agent turn", query: Query{Turned: &TurnWindow{}}, want: []string{"open", "landed"}},
		{name: "a compaction inside a turn window is no agent turn", query: Query{Turned: &TurnWindow{Since: ptr(at(10))}}, want: nil},
		{name: "a restart re-drive and a typed clear are no agent turns", query: Query{Turned: &TurnWindow{Since: ptr(at(3)), Until: ptr(at(3))}}, want: nil},
		{name: "a login and a turn window combine", query: Query{LoginRepos: map[wsm.RepoID]bool{other.ID: true}, Turned: &TurnWindow{Since: ptr(at(6))}}, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			records := fixture()

			// Act
			got, err := Find(records, tt.query)

			// Assert
			if err != nil {
				t.Fatalf("Find: %v", err)
			}
			if !reflect.DeepEqual(ids(got), tt.want) || got.Omitted != 0 {
				t.Fatalf("Find = %v (omitted %d), want %v", ids(got), got.Omitted, tt.want)
			}
		})
	}
}

func TestFindEndedSinceDropsACloseWithNoRecordedInstant(t *testing.T) {
	// Arrange: a workspace a build before close records closed.
	records := fixture()
	records.Workspaces[1].Closure = nil

	// Act
	got, err := Find(records, Query{EndedSince: ptr(at(0))})

	// Assert
	if err != nil || !reflect.DeepEqual(ids(got), []string{"open", "landed"}) {
		t.Fatalf("Find = %v, %v; want the unrecorded close left out", ids(got), err)
	}
}

func TestFindLimitsAndCountsWhatItOmitted(t *testing.T) {
	tests := []struct {
		name        string
		limit       int
		want        []string
		wantOmitted int
	}{
		{name: "a limit below the matches", limit: 1, want: []string{"open"}, wantOmitted: 2},
		{name: "a limit equal to the matches", limit: 3, want: []string{"open", "landed", "closed"}, wantOmitted: 0},
		{name: "a limit above the matches", limit: 10, want: []string{"open", "landed", "closed"}, wantOmitted: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			records := fixture()

			// Act
			got, err := Find(records, Query{Limit: ptr(tt.limit)})

			// Assert
			if err != nil {
				t.Fatalf("Find: %v", err)
			}
			if !reflect.DeepEqual(ids(got), tt.want) || got.Omitted != tt.wantOmitted {
				t.Fatalf("Find = %v (omitted %d), want %v (omitted %d)", ids(got), got.Omitted, tt.want, tt.wantOmitted)
			}
		})
	}
}

func TestFindAnswersEachWorkspacesRepositoryAndSummary(t *testing.T) {
	// Arrange
	records := fixture()

	// Act
	got, err := Find(records, Query{})

	// Assert
	if err != nil {
		t.Fatalf("Find: %v", err)
	}
	byID := map[string]Found{}
	for _, f := range got.Found {
		byID[string(f.Workspace.ID)] = f
	}
	if f := byID["landed"]; f.Repository != other || f.Summary == nil || f.Summary.Short != "Prune the store's old rows" || !f.LastActiveAt.Equal(at(6)) {
		t.Fatalf("landed = %+v, want repository %v, its summary and last active on day 6", f, other)
	}
	if f := byID["closed"]; f.Summary != nil || !f.LastActiveAt.Equal(at(4)) {
		t.Fatalf("closed = %+v, want no summary and last active at its close", f)
	}
	if f := byID["open"]; f.ConfigDir == nil || *f.ConfigDir != workRoot || f.LastTurnAt == nil || !f.LastTurnAt.Equal(at(9)) {
		t.Fatalf("open = %+v, want the work root and its last agent turn on day 9", f)
	}
	if f := byID["closed"]; f.ConfigDir != nil || f.LastTurnAt != nil {
		t.Fatalf("closed = %+v, want no config dir and no agent turn", f)
	}
}

func TestFindLeavesOutThreadsAndAnswersMergeWorkspaces(t *testing.T) {
	// Arrange
	thread := record("thread", "chat", doom, 1)
	thread.Kind = wsm.WorkspaceKindThread
	merge := record("merge", "merge-footer", doom, 1)
	merge.MergeOf = &wsm.MergeOrigin{}
	agent := record("agent", "footer", doom, 1)

	// Act
	got, err := Find(Records{Workspaces: []wsm.Workspace{thread, merge, agent}, Repositories: []wsm.Repository{doom}}, Query{})

	// Assert
	if err != nil {
		t.Fatalf("Find: %v", err)
	}
	if !reflect.DeepEqual(ids(got), []string{"agent", "merge"}) {
		t.Fatalf("Find = %v, want the agent and the merge workspace, never the thread", ids(got))
	}
}

func TestFindFailsOnAWorkspaceOfAnUnregisteredRepository(t *testing.T) {
	// Arrange
	orphan := record("orphan", "lost", other, 1)

	// Act
	got, err := Find(Records{Workspaces: []wsm.Workspace{orphan}, Repositories: []wsm.Repository{doom}}, Query{})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "repo-other") {
		t.Fatalf("Find = %v, want an error naming the unregistered repository", err)
	}
	if got.Found != nil {
		t.Fatalf("a failed find answered %v", ids(got))
	}
}

func TestLastActiveAtIsTheLatestEdge(t *testing.T) {
	tests := []struct {
		name string
		edit func(*wsm.Workspace)
		want time.Time
	}{
		{name: "never active is its creation", edit: func(*wsm.Workspace) {}, want: at(1)},
		{name: "a turn start", edit: func(w *wsm.Workspace) { w.LastActivityAt = ptr(at(3)) }, want: at(3)},
		{name: "a turn end after the start", edit: func(w *wsm.Workspace) {
			w.LastActivityAt = ptr(at(3))
			w.LastTurnEndedAt = ptr(at(4))
		}, want: at(4)},
		{name: "a close", edit: func(w *wsm.Workspace) { w.Closure = &wsm.Closure{At: at(5)} }, want: at(5)},
		{name: "a landing", edit: func(w *wsm.Workspace) { w.MergedAt = ptr(at(6)) }, want: at(6)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ws := record("ws", "ws", doom, 1)
			tt.edit(&ws)

			// Act
			got := LastActiveAt(ws)

			// Assert
			if !got.Equal(tt.want) {
				t.Fatalf("LastActiveAt = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestFindOnRosterKeepsOnlyDrawnWorkspacesAndAnswersHowEachIsDrawn(t *testing.T) {
	tests := []struct {
		name       string
		query      Query
		want       []string
		wantRoster map[string]*Drawn
	}{
		{
			name:  "on roster keeps only the drawn workspaces",
			query: Query{OnRoster: true},
			want:  []string{"open", "landed"},
			wantRoster: map[string]*Drawn{
				"open":   {Section: OpenSection, Status: "thinking"},
				"landed": {Section: MergedSection, Status: "done"},
			},
		},
		{
			name:  "unset answers drawn and undrawn alike, the undrawn with no roster",
			query: Query{},
			want:  []string{"open", "landed", "closed"},
			wantRoster: map[string]*Drawn{
				"open":   {Section: OpenSection, Status: "thinking"},
				"landed": {Section: MergedSection, Status: "done"},
				"closed": nil,
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			records := fixture()
			records.Drawn = map[wsm.WorkspaceID]Drawn{
				"open":   {Section: OpenSection, Status: "thinking"},
				"landed": {Section: MergedSection, Status: "done"},
			}

			// Act
			got, err := Find(records, tt.query)

			// Assert
			if err != nil {
				t.Fatalf("Find: %v", err)
			}
			if !reflect.DeepEqual(ids(got), tt.want) {
				t.Fatalf("Find = %v, want %v", ids(got), tt.want)
			}
			for _, f := range got.Found {
				if want := tt.wantRoster[string(f.Workspace.ID)]; !reflect.DeepEqual(f.Roster, want) {
					t.Fatalf("%s roster = %+v, want %+v", f.Workspace.ID, f.Roster, want)
				}
			}
		})
	}
}

// loginFixture is a work repository (doom, routed to the login) and another
// (other): a work workspace, its child in the other repository, an
// unrelated workspace of the other repository, and a grandchild under the
// child.
func loginFixture() []wsm.Workspace {
	work := record("work", "intake-routing", doom, 0)
	child := record("child", "skill-script", other, 1)
	child.Parent = &work.ID
	unrelated := record("unrelated", "store-prune", other, 2)
	grandchild := record("grandchild", "deep-fix", other, 3)
	grandchild.Parent = &child.ID
	return []wsm.Workspace{work, child, unrelated, grandchild}
}

func TestFindNarrowsToALogin(t *testing.T) {
	tests := []struct {
		name  string
		repos map[wsm.RepoID]bool
		want  []string
	}{
		{name: "no login narrows nothing", repos: nil, want: []string{"grandchild", "unrelated", "child", "work"}},
		{name: "a login keeps its repository's workspaces and their children", repos: map[wsm.RepoID]bool{doom.ID: true}, want: []string{"child", "work"}},
		{name: "a login routed no repository keeps nothing", repos: map[wsm.RepoID]bool{}, want: nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			workspaces := loginFixture()

			// Act
			got, err := Find(Records{Workspaces: workspaces, Repositories: []wsm.Repository{doom, other}}, Query{LoginRepos: tc.repos})

			// Assert
			if err != nil {
				t.Fatalf("Find: %v", err)
			}
			if !reflect.DeepEqual(ids(got), tc.want) {
				t.Fatalf("found %v, want %v", ids(got), tc.want)
			}
		})
	}
}

// fixIt is a whole summary.
var fixIt = wsm.WorkspaceSummary{Long: "It is broken. Fix it.", Short: "Fix it"}

func TestSummaries(t *testing.T) {
	tests := []struct {
		name      string
		summaries map[wsm.WorkspaceID]wsm.WorkspaceSummary
		titles    map[wsm.WorkspaceID]string
		want      map[wsm.WorkspaceID]wsm.WorkspaceSummary
	}{
		{name: "a summary stands alone", summaries: map[wsm.WorkspaceID]wsm.WorkspaceSummary{"a": fixIt}, want: map[wsm.WorkspaceID]wsm.WorkspaceSummary{"a": fixIt}},
		{name: "a vendor title stands in for no summary as a short summary", titles: map[wsm.WorkspaceID]string{"a": "Its title"}, want: map[wsm.WorkspaceID]wsm.WorkspaceSummary{"a": {Short: "Its title"}}},
		{name: "a summary outranks the vendor title", summaries: map[wsm.WorkspaceID]wsm.WorkspaceSummary{"a": fixIt}, titles: map[wsm.WorkspaceID]string{"a": "Its title"}, want: map[wsm.WorkspaceID]wsm.WorkspaceSummary{"a": fixIt}},
		{name: "neither is no entry", want: map[wsm.WorkspaceID]wsm.WorkspaceSummary{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Summaries(tt.summaries, tt.titles)

			// Assert
			if !maps.Equal(got, tt.want) {
				t.Fatalf("Summaries = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestFindByLoginPanicsOnAParentTheListingDoesNotHold(t *testing.T) {
	// Arrange: a child whose parent is missing from the listing.
	orphan := record("orphan", "lost", other, 0)
	missing := wsm.WorkspaceID("gone")
	orphan.Parent = &missing
	defer func() {
		if recover() == nil {
			t.Fatalf("Find did not panic on a parent the listing does not hold")
		}
	}()

	// Act
	_, _ = Find(Records{Workspaces: []wsm.Workspace{orphan}, Repositories: []wsm.Repository{doom, other}}, Query{LoginRepos: map[wsm.RepoID]bool{doom.ID: true}})
}

func TestIsAgentTurn(t *testing.T) {
	tests := []struct {
		name string
		turn wsm.TurnStart
		want bool
	}{
		{name: "a user prompt", turn: turn("w", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, "", 0), want: true},
		{name: "a prompt another workspace sent", turn: turn("w", conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT, "", 0), want: true},
		{name: "a daemon brief", turn: turn("w", conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED, "", 0), want: true},
		{name: "a skill the user typed", turn: turn("w", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, "/agent-repl-install", 0), want: true},
		{name: "a typed compaction", turn: turn("w", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, "/compact keep the plan", 0), want: false},
		{name: "the daemon's compaction", turn: turn("w", conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT, "", 0), want: false},
		{name: "a restart's re-drive", turn: turn("w", conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART, "", 0), want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := IsAgentTurn(tt.turn)

			// Assert
			if got != tt.want {
				t.Fatalf("IsAgentTurn(%+v) = %v, want %v", tt.turn, got, tt.want)
			}
		})
	}
}
