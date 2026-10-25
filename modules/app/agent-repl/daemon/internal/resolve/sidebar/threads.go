package sidebar

import (
	"sort"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/wsm"
)

// THE SIDEBAR'S APPS: agent-repl's workspaces and zettel's chat threads. The
// rail's title is the switcher between them, and which app is shown follows
// the selected workspace's kind: a thread on screen shows Chat, anything else
// shows Workspaces. Threads appear in no repository section, band or tab; they
// are drawn in the thread section alone.

// The switcher's words.
const (
	// WorkspacesAppLabel is the Workspaces app's title and option.
	WorkspacesAppLabel = "Workspaces"
	// ChatAppLabel is the Chat app's title and option.
	ChatAppLabel = "Chats"
	// NewThreadLabel is the thread section's new-thread button.
	NewThreadLabel = "New thread"
)

// workspacesApp and chatApp are the two apps' tokens.
func workspacesApp() *frontendv1.SidebarApp {
	return &frontendv1.SidebarApp{App: &frontendv1.SidebarApp_Workspaces{Workspaces: &frontendv1.SidebarAppWorkspaces{}}}
}

func chatApp() *frontendv1.SidebarApp {
	return &frontendv1.SidebarApp{App: &frontendv1.SidebarApp_Chat{Chat: &frontendv1.SidebarAppChat{}}}
}

// selectedIsThread reports whether the selected workspace is a chat thread.
func (r *resolver) selectedIsThread() bool {
	if r.state.selected == nil {
		return false
	}
	for _, ws := range r.state.reg.Workspaces {
		if ws.ID == *r.state.selected {
			return ws.IsThread()
		}
	}
	return false
}

// appSwitcher resolves the rail's title and its dropdown.
func (r *resolver) appSwitcher() *frontendv1.RosterAppSwitcher {
	chat := r.selectedIsThread()
	title, shown := WorkspacesAppLabel, workspacesApp()
	if chat {
		title, shown = ChatAppLabel, chatApp()
	}
	option := func(label string, app *frontendv1.SidebarApp, isShown bool) *frontendv1.RosterAppSwitcherOption {
		o := &frontendv1.RosterAppSwitcherOption{
			Label: &frontendv1.RosterAppSwitcherOptionLabel{Text: label},
			App:   app,
		}
		if isShown {
			o.Shown = &frontendv1.RosterAppSwitcherOptionShown{}
		}
		return o
	}
	return &frontendv1.RosterAppSwitcher{
		Title: &frontendv1.RosterAppSwitcherTitle{Text: title},
		Shown: shown,
		Options: []*frontendv1.RosterAppSwitcherOption{
			option(WorkspacesAppLabel, workspacesApp(), !chat),
			option(ChatAppLabel, chatApp(), chat),
		},
	}
}

// threadSection resolves the thread list: every open thread and then every
// archived one (a closed thread's workspace), each the one last asked a
// question first: viewing a thread never moves it (owner ruling, 2026-10-25).
func (r *resolver) threadSection(threads []wsm.Workspace) *frontendv1.RosterThreadSection {
	sorted := append([]wsm.Workspace(nil), threads...)
	sort.SliceStable(sorted, func(i, j int) bool {
		if sorted[i].Closed != sorted[j].Closed {
			return !sorted[i].Closed
		}
		a, b := sorted[i].AskedAt(), sorted[j].AskedAt()
		if !a.Equal(b) {
			return a.After(b)
		}
		return sorted[i].ID < sorted[j].ID
	})
	out := &frontendv1.RosterThreadSection{NewThread: &frontendv1.RosterThreadNewButton{Label: NewThreadLabel}}
	for _, ws := range sorted {
		row := &frontendv1.RosterThreadRow{
			Thread:   ws.Ref(),
			Name:     &frontendv1.RosterThreadName{Text: ws.Name},
			Standing: threadStanding(ws),
		}
		if s, ok := r.state.workspaces[ws.ID]; ok && s.summary != "" {
			row.Summary = &frontendv1.RosterThreadSummary{Text: s.summary}
		}
		if r.state.selected != nil && *r.state.selected == ws.ID {
			row.Current = &frontendv1.RosterThreadCurrent{}
			if s, ok := r.state.workspaces[ws.ID]; ok && s.outline != nil {
				row.Outline = s.outline
			}
		}
		out.Rows = append(out.Rows, row)
	}
	return out
}

// threadStanding is a thread's standing: archived when its workspace is
// closed, open otherwise.
func threadStanding(ws wsm.Workspace) *frontendv1.RosterThreadStanding {
	if ws.Closed {
		return &frontendv1.RosterThreadStanding{Standing: &frontendv1.RosterThreadStanding_Archived{Archived: &frontendv1.RosterThreadArchived{}}}
	}
	return &frontendv1.RosterThreadStanding{Standing: &frontendv1.RosterThreadStanding_Open{Open: &frontendv1.RosterThreadOpen{}}}
}
