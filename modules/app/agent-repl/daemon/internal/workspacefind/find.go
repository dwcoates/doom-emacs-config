// Package workspacefind answers agentrepl.v1.FindWorkspaces: which of the
// registry's agent workspaces, open or closed, match a caller's narrowing,
// most recently active first. It is a pure filter over records the caller has
// already read, so every narrowing rule is tested without a store.
package workspacefind

import (
	"fmt"
	"sort"
	"strings"
	"time"

	"agent-repld/internal/prompthandler"
	"agent-repld/internal/promptorigin"
	"agent-repld/internal/wsm"
)

// Standing narrows a find to one standing. AnyStanding narrows nothing.
type Standing int

const (
	// AnyStanding finds open and closed workspaces alike.
	AnyStanding Standing = iota
	// OpenStanding finds workspaces open now.
	OpenStanding
	// ClosedStanding finds workspaces closed now, landed or not.
	ClosedStanding
	// LandedStanding finds workspaces whose merge landed.
	LandedStanding
)

// Query is what to find. Every set criterion must hold; a nil one narrows
// nothing.
type Query struct {
	// Repository keeps only the workspaces of this registered repository.
	Repository *wsm.RepoID
	// Since keeps only workspaces last active at or after it.
	Since *time.Time
	// Until keeps only workspaces created at or before it.
	Until *time.Time
	// Standing keeps only workspaces in this standing.
	Standing Standing
	// Text keeps only workspaces whose name, branch, short summary or long
	// summary contains it, compared case-insensitively.
	Text *string
	// Limit answers at most this many, the most recently active first.
	Limit *int
	// CreatedBy keeps only the workspaces this workspace's agent asked for
	// (wsm.Workspace.RequestedBy).
	CreatedBy *wsm.WorkspaceID
	// OnRoster keeps only the workspaces the roster draws (Found.Roster set).
	OnRoster bool
	// LoginRepos keeps only the workspaces of these repositories (the ones
	// routed to one login) plus the direct children of those workspaces, in
	// any repository. nil narrows nothing; an empty set keeps nothing.
	LoginRepos map[wsm.RepoID]bool
	// Turned keeps only workspaces that had an agent turn inside it.
	Turned *TurnWindow
}

// TurnWindow is a window an agent turn's start must fall in, both ends
// inclusive; a nil end is open-ended.
type TurnWindow struct {
	// Since is the window's start.
	Since *time.Time
	// Until is the window's end.
	Until *time.Time
}

// contains reports whether AT falls inside W.
func (w TurnWindow) contains(at time.Time) bool {
	return (w.Since == nil || !at.Before(*w.Since)) && (w.Until == nil || !at.After(*w.Until))
}

// Records are what a find filters: the registry's rows and the facts kept
// beside them, read by the caller.
type Records struct {
	// Workspaces are every registered workspace.
	Workspaces []wsm.Workspace
	// Repositories are every registered repository.
	Repositories []wsm.Repository
	// Summaries are each workspace's words (Summaries).
	Summaries map[wsm.WorkspaceID]wsm.WorkspaceSummary
	// Drawn is how the roster draws each workspace (Placements).
	Drawn map[wsm.WorkspaceID]Drawn
	// Sessions are the session records by workspace, which say the config dir
	// each runs under.
	Sessions map[wsm.WorkspaceID]wsm.Session
	// Turns are every recorded turn's start.
	Turns []wsm.TurnStart
}

// Found is one workspace a find answered.
type Found struct {
	// Workspace is the workspace's registry record.
	Workspace wsm.Workspace
	// Repository is the repository the workspace belongs to.
	Repository wsm.Repository
	// Summary is the workspace's stored summary, else its vendor title as a
	// short summary alone (Summaries), nil when neither stands.
	Summary *wsm.WorkspaceSummary
	// LastActiveAt is LastActiveAt(Workspace), which the answer is ordered by.
	LastActiveAt time.Time
	// Roster is how the roster draws the workspace, nil when it draws no row
	// of it in its open, backlogged or recently merged section.
	Roster *Drawn
	// ConfigDir is the Claude config dir the workspace's session last came up
	// under, nil when it never came up.
	ConfigDir *string
	// LastTurnAt is when the workspace's last agent turn started, nil when it
	// never took one.
	LastTurnAt *time.Time
}

// Result is a find's answer.
type Result struct {
	// Found are the matches answered, the most recently active first.
	Found []Found
	// Omitted is how many further matches Query.Limit left out.
	Omitted int
}

// LastActiveAt is when WS was last active: the latest of its creation, its
// last turn's start or end, its close and its landing. A workspace that never
// took a turn was last active when it was created.
func LastActiveAt(ws wsm.Workspace) time.Time {
	latest := ws.CreatedAt
	for _, at := range []*time.Time{ws.LastActivityAt, ws.LastTurnEndedAt, closedAt(ws), ws.MergedAt} {
		if at != nil && at.After(latest) {
			latest = *at
		}
	}
	return latest
}

// closedAt is when WS was closed, nil while open or when its close predates
// the close record.
func closedAt(ws wsm.Workspace) *time.Time {
	if ws.Closure == nil {
		return nil
	}
	return &ws.Closure.At
}

// IsAgentWorkspace reports whether WS is a piece of work an agent looks for:
// any workspace but a zettel chat thread. The hidden workspace an async merge
// runs in IS one (its record is answered marked as a merge workspace): the
// agent that asked for the merge reads it, and the agent CLI resolves the
// merge workspace's own worktree to it.
func IsAgentWorkspace(ws wsm.Workspace) bool {
	return !ws.IsThread()
}

// Summaries answers each workspace's words for a find, by the topbar's own
// precedence: the summary its agent set (SUMMARIES), else the vendor's title
// for its conversation (TITLES) as a short summary alone, which the topbar
// draws while no summary stands. A workspace with neither is absent.
func Summaries(summaries map[wsm.WorkspaceID]wsm.WorkspaceSummary, titles map[wsm.WorkspaceID]string) map[wsm.WorkspaceID]wsm.WorkspaceSummary {
	out := make(map[wsm.WorkspaceID]wsm.WorkspaceSummary, len(summaries)+len(titles))
	for ws, title := range titles {
		out[ws] = wsm.WorkspaceSummary{Short: title}
	}
	for ws, summary := range summaries {
		out[ws] = summary
	}
	return out
}

// IsAgentTurn reports whether TURN set the agent working: its origin starts an
// agent turn (promptorigin.StartsAgentTurn), and its text is no session
// command the daemon acts on itself, such as a typed /compact or /clear
// (prompthandler.RecognizeText).
func IsAgentTurn(turn wsm.TurnStart) bool {
	return promptorigin.StartsAgentTurn(turn.Origin) && prompthandler.RecognizeText(turn.CommandText) == prompthandler.RecognizedNone
}

// agentTurns is one workspace's agent turns, as a find reads them.
type agentTurns struct {
	// last is the latest agent turn's start.
	last time.Time
	// inWindow reports whether any agent turn started inside the query's turn
	// window.
	inWindow bool
}

// agentTurnsOf indexes TURNS' agent turns by workspace, matching each against
// WINDOW when one is set.
func agentTurnsOf(turns []wsm.TurnStart, window *TurnWindow) map[wsm.WorkspaceID]agentTurns {
	out := make(map[wsm.WorkspaceID]agentTurns)
	for _, turn := range turns {
		if !IsAgentTurn(turn) {
			continue
		}
		got := out[turn.Workspace]
		if turn.StartedAt.After(got.last) {
			got.last = turn.StartedAt
		}
		got.inWindow = got.inWindow || (window != nil && window.contains(turn.StartedAt))
		out[turn.Workspace] = got
	}
	return out
}

// Find answers the agent workspaces among IN's that match Q, each with its
// repository, its summary, how the roster draws it, its config dir and its
// last agent turn. A workspace whose repository IN does not hold is an
// invariant violation (the registry's foreign key forbids it) and fails the
// find.
func Find(in Records, q Query) (Result, error) {
	repos := make(map[wsm.RepoID]wsm.Repository, len(in.Repositories))
	for _, repo := range in.Repositories {
		repos[repo.ID] = repo
	}
	byID := make(map[wsm.WorkspaceID]wsm.Workspace, len(in.Workspaces))
	for _, ws := range in.Workspaces {
		byID[ws.ID] = ws
	}
	turns := agentTurnsOf(in.Turns, q.Turned)
	var matched []Found
	for _, ws := range in.Workspaces {
		if !IsAgentWorkspace(ws) {
			continue
		}
		repo, ok := repos[ws.Repo]
		if !ok {
			return Result{}, fmt.Errorf("workspacefind: workspace %s names repository %s, which the registry does not hold", ws.ID, ws.Repo)
		}
		found := Found{Workspace: ws, Repository: repo, LastActiveAt: LastActiveAt(ws)}
		if summary, ok := in.Summaries[ws.ID]; ok {
			found.Summary = &summary
		}
		if d, ok := in.Drawn[ws.ID]; ok {
			found.Roster = &d
		}
		// A SESSION ROW CAN PRECEDE ITS FIRST BRING-UP (a root chosen before
		// the workspace ever came up), and records no root until then.
		if session, ok := in.Sessions[ws.ID]; ok && session.ConfigDir != "" {
			dir := session.ConfigDir
			found.ConfigDir = &dir
		}
		agent, turned := turns[ws.ID]
		if turned {
			last := agent.last
			found.LastTurnAt = &last
		}
		if matches(found, q, agent.inWindow) && ofLogin(ws, byID, q.LoginRepos) {
			matched = append(matched, found)
		}
	}
	sort.SliceStable(matched, func(i, j int) bool {
		a, b := matched[i], matched[j]
		if !a.LastActiveAt.Equal(b.LastActiveAt) {
			return a.LastActiveAt.After(b.LastActiveAt)
		}
		if !a.Workspace.CreatedAt.Equal(b.Workspace.CreatedAt) {
			return a.Workspace.CreatedAt.After(b.Workspace.CreatedAt)
		}
		return a.Workspace.ID < b.Workspace.ID
	})
	out := Result{Found: matched}
	if q.Limit != nil && len(matched) > *q.Limit {
		out.Found = matched[:*q.Limit]
		out.Omitted = len(matched) - *q.Limit
	}
	return out, nil
}

// matches reports whether F meets every criterion Q sets but its login,
// TURNED being whether one of its agent turns started inside Q's turn window.
func matches(f Found, q Query, turned bool) bool {
	ws := f.Workspace
	if q.Repository != nil && ws.Repo != *q.Repository {
		return false
	}
	if q.Since != nil && f.LastActiveAt.Before(*q.Since) {
		return false
	}
	if q.Until != nil && ws.CreatedAt.After(*q.Until) {
		return false
	}
	if !inStanding(ws, q.Standing) {
		return false
	}
	if q.Text != nil && !containsText(f, *q.Text) {
		return false
	}
	if q.CreatedBy != nil && (ws.RequestedBy == nil || *ws.RequestedBy != *q.CreatedBy) {
		return false
	}
	if q.OnRoster && f.Roster == nil {
		return false
	}
	if q.Turned != nil && !turned {
		return false
	}
	return true
}

// ofLogin reports whether WS belongs to the login whose repositories are
// REPOS: it is in one of them, or its parent (in BYID, the same listing) is.
// A nil REPOS narrows nothing. A parent the listing does not hold is an
// invariant violation (the registry's foreign key sets it NULL when the row
// is forgotten) and panics.
func ofLogin(ws wsm.Workspace, byID map[wsm.WorkspaceID]wsm.Workspace, repos map[wsm.RepoID]bool) bool {
	if repos == nil || repos[ws.Repo] {
		return true
	}
	if ws.Parent == nil {
		return false
	}
	parent, ok := byID[*ws.Parent]
	if !ok {
		panic(fmt.Sprintf("workspacefind: workspace %s names parent %s, which the registry listing does not hold", ws.ID, *ws.Parent))
	}
	return repos[parent.Repo]
}

// inStanding reports whether WS is in STANDING.
func inStanding(ws wsm.Workspace, standing Standing) bool {
	switch standing {
	case AnyStanding:
		return true
	case OpenStanding:
		return !ws.Closed
	case ClosedStanding:
		return ws.Closed
	case LandedStanding:
		return ws.MergedAt != nil
	default:
		panic(fmt.Sprintf("workspacefind: unknown standing %d", standing))
	}
}

// containsText reports whether F's name, branch, short summary or long
// summary contains TEXT, case-insensitively.
func containsText(f Found, text string) bool {
	needle := strings.ToLower(text)
	haystacks := []string{f.Workspace.Name, f.Workspace.Branch}
	if f.Summary != nil {
		haystacks = append(haystacks, f.Summary.Short, f.Summary.Long)
	}
	for _, h := range haystacks {
		if strings.Contains(strings.ToLower(h), needle) {
			return true
		}
	}
	return false
}
