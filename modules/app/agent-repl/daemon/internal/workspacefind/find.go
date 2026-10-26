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
	// Text keeps only workspaces whose name, branch or summary contains it,
	// compared case-insensitively.
	Text *string
	// Limit answers at most this many, the most recently active first.
	Limit *int
}

// Found is one workspace a find answered.
type Found struct {
	// Workspace is the workspace's registry record.
	Workspace wsm.Workspace
	// Repository is the repository the workspace belongs to.
	Repository wsm.Repository
	// Summary is the workspace's stored summary, nil when none stands.
	Summary *string
	// LastActiveAt is LastActiveAt(Workspace), which the answer is ordered by.
	LastActiveAt time.Time
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
// neither a zettel chat thread nor the hidden workspace an async merge runs in.
func IsAgentWorkspace(ws wsm.Workspace) bool {
	return !ws.IsThread() && ws.MergeOf == nil
}

// Find answers the agent workspaces among WORKSPACES that match Q, each with
// its repository from REPOSITORIES and its summary from SUMMARIES. A workspace
// whose repository REPOSITORIES does not hold is an invariant violation (the
// registry's foreign key forbids it) and fails the find.
func Find(workspaces []wsm.Workspace, repositories []wsm.Repository, summaries map[wsm.WorkspaceID]wsm.WorkspaceSummary, q Query) (Result, error) {
	repos := make(map[wsm.RepoID]wsm.Repository, len(repositories))
	for _, repo := range repositories {
		repos[repo.ID] = repo
	}
	var matched []Found
	for _, ws := range workspaces {
		if !IsAgentWorkspace(ws) {
			continue
		}
		repo, ok := repos[ws.Repo]
		if !ok {
			return Result{}, fmt.Errorf("workspacefind: workspace %s names repository %s, which the registry does not hold", ws.ID, ws.Repo)
		}
		found := Found{Workspace: ws, Repository: repo, LastActiveAt: LastActiveAt(ws)}
		if sum, ok := summaries[ws.ID]; ok {
			title := sum.Title
			found.Summary = &title
		}
		if matches(found, q) {
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

// matches reports whether F meets every criterion Q sets.
func matches(f Found, q Query) bool {
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
	return true
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

// containsText reports whether F's name, branch or summary contains TEXT,
// case-insensitively.
func containsText(f Found, text string) bool {
	needle := strings.ToLower(text)
	haystacks := []string{f.Workspace.Name, f.Workspace.Branch}
	if f.Summary != nil {
		haystacks = append(haystacks, *f.Summary)
	}
	for _, h := range haystacks {
		if strings.Contains(strings.ToLower(h), needle) {
			return true
		}
	}
	return false
}
