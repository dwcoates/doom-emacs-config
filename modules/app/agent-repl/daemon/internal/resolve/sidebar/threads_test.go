package sidebar_test

import (
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// THE SIDEBAR'S APPS: the rail title's switcher, the thread section, and the
// threads' absence from every repository section and the tab bar.

var zettelHome = wsm.Repository{ID: "repo-zettel", Dir: "/home/zettel/threads", Name: "zettel", Kind: wsm.RepositoryKindZettel}

// thread is a chat thread in the zettel home, last interacted with at AT.
func thread(id, name string, at *time.Time) wsm.Workspace {
	return wsm.Workspace{
		ID: ids.WorkspaceID(id), Kind: wsm.WorkspaceKindThread, Repo: zettelHome.ID,
		Dir: "/home/zettel/threads/" + name, Name: name, CreatedAt: epoch, LastSelectedAt: at,
	}
}

// withThreads is a registry of one agent workspace and the given threads.
func withThreads(threads ...wsm.Workspace) sidebar.Registry {
	reg := registry(append([]wsm.Workspace{placed(workspace("w-a", "a"), 1)}, threads...)...)
	reg.Repositories = append(reg.Repositories, zettelHome)
	return reg
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

func TestTheThreadSectionPutsTheMostRecentlyInteractedFirst(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	older, newer := thread("t-1", "older", at(time.Minute)), thread("t-2", "newer", nil)
	newer.LastActivityAt = at(2 * time.Minute)

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
