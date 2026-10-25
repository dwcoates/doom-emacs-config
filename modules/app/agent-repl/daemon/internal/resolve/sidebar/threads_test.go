package sidebar_test

import (
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// THE SIDEBAR'S APPS: the rail title's switcher, the thread section, and the
// threads' absence from every repository section and the tab bar.

// zettelHomeID is the threads' repository, which the registry never lists.
const zettelHomeID = ids.RepoID("repo-zettel")

// thread is a chat thread in the zettel home, last asked a question at AT.
func thread(id, name string, at *time.Time) wsm.Workspace {
	return wsm.Workspace{
		ID: ids.WorkspaceID(id), Kind: wsm.WorkspaceKindThread, Repo: zettelHomeID,
		Dir: "/home/zettel/threads/" + name, Name: name, CreatedAt: epoch, LastAskedAt: at,
	}
}

// withThreads is a registry of one agent workspace and the given threads. It
// lists no zettel home, exactly as wsm.DB.ListRepositories never does.
func withThreads(threads ...wsm.Workspace) sidebar.Registry {
	return registry(append([]wsm.Workspace{placed(workspace("w-a", "a"), 1)}, threads...)...)
}

func TestThreadsAreDrawnOnlyInTheThreadSection(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)

	// Act
	r.SetRegistry(withThreads(thread("t-1", "why", at(time.Minute))))

	// Assert
	roster := latest(t, r)
	if got := rowNames(repoRows(t, roster)); !equal(got, []string{"a"}) {
		t.Fatalf("repository rows = %v, want only the agent workspace (and no zettel section)", got)
	}
	if got := tabIDsOf(roster); !equal(got, []string{"w-a:drawn"}) {
		t.Fatalf("tab bar = %v, want no thread tab", got)
	}
	rows := roster.GetThreads().GetRows()
	if len(rows) != 1 || rows[0].GetName().GetText() != "why" || rows[0].GetThread().GetId() != "t-1" {
		t.Fatalf("thread rows = %v, want the thread", rows)
	}
	if roster.GetThreads().GetNewThread().GetLabel() != sidebar.NewThreadLabel {
		t.Fatalf("new-thread button = %v", roster.GetThreads().GetNewThread())
	}
}

func TestTheThreadSectionPutsTheMostRecentlyAskedFirst(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	older, newer := thread("t-1", "older", at(time.Minute)), thread("t-2", "newer", at(2*time.Minute))

	// Act
	r.SetRegistry(withThreads(older, newer))

	// Assert
	var names []string
	for _, row := range latest(t, r).GetThreads().GetRows() {
		names = append(names, row.GetName().GetText())
	}
	if !equal(names, []string{"newer", "older"}) {
		t.Fatalf("thread rows = %v, want newer first", names)
	}
}

func TestViewingAThreadDoesNotMoveIt(t *testing.T) {
	// Arrange: the older-asked thread was viewed (and so worked in) last.
	r, _ := newResolver(t)
	viewed, asked := thread("t-1", "viewed", at(time.Minute)), thread("t-2", "asked", at(2*time.Minute))
	viewed.LastSelectedAt = at(3 * time.Minute)
	viewed.LastActivityAt = at(3 * time.Minute)

	// Act
	r.SetRegistry(withThreads(viewed, asked))

	// Assert
	rows := latest(t, r).GetThreads().GetRows()
	if rows[0].GetName().GetText() != "asked" {
		t.Fatalf("first row = %q, want the thread asked last", rows[0].GetName().GetText())
	}
}

func TestArchivedThreadsFollowEveryOpenThread(t *testing.T) {
	// Arrange: the archived thread is the one asked last.
	r, _ := newResolver(t)
	archived, open := thread("t-1", "archived", at(2*time.Minute)), thread("t-2", "open", at(time.Minute))
	archived.Closed = true

	// Act
	r.SetRegistry(withThreads(archived, open))

	// Assert
	rows := latest(t, r).GetThreads().GetRows()
	if len(rows) != 2 || rows[0].GetName().GetText() != "open" || rows[1].GetName().GetText() != "archived" {
		t.Fatalf("thread rows = %v, want the open thread first", rows)
	}
	if rows[0].GetStanding().GetOpen() == nil || rows[1].GetStanding().GetArchived() == nil {
		t.Fatalf("standings = %v, %v; want open then archived", rows[0].GetStanding(), rows[1].GetStanding())
	}
}

func TestAThreadRowCarriesItsSummaryAndCurrent(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.SetRegistrySelected(withThreads(thread("t-1", "why", nil)), "t-1")

	// Act
	r.SetSummary("t-1", "Rayleigh scattering")

	// Assert
	row := latest(t, r).GetThreads().GetRows()[0]
	if row.GetSummary().GetText() != "Rayleigh scattering" || row.GetCurrent() == nil {
		t.Fatalf("row = %v, want its summary and current", row)
	}
}

func TestTheAppSwitcherShowsTheSelectedWorkspacesApp(t *testing.T) {
	cases := []struct {
		name     string
		selected ids.WorkspaceID
		title    string
		chat     bool
	}{
		{name: "an agent workspace shows Workspaces", selected: "w-a", title: sidebar.WorkspacesAppLabel},
		{name: "a thread shows Chat", selected: "t-1", title: sidebar.ChatAppLabel, chat: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, _ := newResolver(t)

			// Act
			r.SetRegistrySelected(withThreads(thread("t-1", "why", nil)), tc.selected)

			// Assert
			switcher := latest(t, r).GetAppSwitcher()
			if switcher.GetTitle().GetText() != tc.title || (switcher.GetShown().GetChat() != nil) != tc.chat {
				t.Fatalf("switcher = %v, want %q shown", switcher, tc.title)
			}
			options := switcher.GetOptions()
			if len(options) != 2 || (options[1].GetShown() != nil) != tc.chat || (options[0].GetShown() != nil) == tc.chat {
				t.Fatalf("options = %v, want the shown app marked", options)
			}
		})
	}
}

func TestThreadAnswersALaidOutThreadWithItsRostersRevision(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.SetRegistry(withThreads(thread("t-1", "why", nil)))

	// Act
	revision, ok := r.Thread(ids.WorkspaceID("t-1"))

	// Assert
	if !ok || revision != latest(t, r).GetRevision() {
		t.Fatalf("Thread = %d, %v, want the roster's revision %d", revision, ok, latest(t, r).GetRevision())
	}
}

func TestThreadAnswersFalseForAWorkspaceThatIsARow(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.SetRegistry(withThreads(thread("t-1", "why", nil)))

	// Act
	_, ok := r.Thread(ids.WorkspaceID("w-a"))

	// Assert
	if ok {
		t.Fatal("Thread listed a repository workspace as a thread")
	}
}

func TestThreadAnswersFalseBeforeAnyRegistry(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)

	// Act
	_, ok := r.Thread(ids.WorkspaceID("t-1"))

	// Assert
	if ok {
		t.Fatal("Thread answered before any roster was resolved")
	}
}

func TestTheCurrentThreadsRowCarriesItsOutline(t *testing.T) {
	cases := []struct {
		name     string
		selected string
		want     bool
	}{
		{name: "the thread on screen carries it", selected: "t-1", want: true},
		{name: "a thread not on screen does not", selected: "w-a", want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, _ := newResolver(t)
			r.SetRegistrySelected(withThreads(thread("t-1", "why", nil)), ids.WorkspaceID(tc.selected))
			outline := &frontendv1.RosterThreadOutline{Rows: []*frontendv1.RosterThreadOutlineRow{{Label: &frontendv1.RosterThreadOutlineLabel{Text: "why"}}}}

			// Act
			r.SetThreadOutline("t-1", outline)

			// Assert
			row := latest(t, r).GetThreads().GetRows()[0]
			if got := row.Outline != nil; got != tc.want {
				t.Fatalf("outline carried = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestATakenBackOutlineLeavesTheRow(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.SetRegistrySelected(withThreads(thread("t-1", "why", nil)), "t-1")
	r.SetThreadOutline("t-1", &frontendv1.RosterThreadOutline{Rows: []*frontendv1.RosterThreadOutlineRow{{}}})

	// Act
	r.SetThreadOutline("t-1", nil)

	// Assert
	if row := latest(t, r).GetThreads().GetRows()[0]; row.Outline != nil {
		t.Fatalf("outline = %v, want none", row.Outline)
	}
}
