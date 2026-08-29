// Package sidebar is the roster resolver: both groupings, priority order, the
// attention marker, recently merged, the current workspace and each row's
// status arm.
//
// A merged, closed or KILLED workspace's row carries closed = true; a nuked
// workspace LEAVES the roster. The attention marker is set on a notification
// and cleared on SelectWorkspace. See ARCHITECTURE.md "resolvers".
package sidebar

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/notimpl"
	"claude-repld/internal/publish"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/vocab"
	"claude-repld/internal/wsm"
)

// Registry is the WSM-derived half of the roster: everything the durable
// records say. The resolver re-renders on any WSM change.
type Registry struct {
	// Workspaces are every registered workspace.
	Workspaces []wsm.Workspace
	// Repositories are every repository, for the repo grouping.
	Repositories []wsm.Repository
	// Tasks are every task, for the task grouping.
	Tasks []wsm.Task
	// Current is the selected workspace, nil when none is.
	Current *ids.WorkspaceID
}

// Resolver is the roster's whole surface. The roster is EDITOR-GLOBAL: one
// stream serves every webview alike.
type Resolver interface {
	sessionwatcher.SidebarSink

	// SetRegistry installs the durable half, called on any WSM change.
	SetRegistry(reg Registry)
	// SetMerge installs one workspace's merge facts, which drive its merge
	// status arm and its glyph.
	SetMerge(ws ids.WorkspaceID, facts footer.MergeFacts)
	// SetSelected records the user's selection, which also clears that
	// workspace's attention marker.
	SetSelected(ws ids.WorkspaceID)
	// Topic is the one editor-global roster publication.
	Topic() *publish.Topic[*frontendv1.WorkspaceRoster]
}

// New builds the roster resolver. colors supplies the roster_status and
// merge_glyphs tables, which the resolver asserts its arms against.
func New(colors vocab.RenderColors, log dlog.Surfaces) (Resolver, error) {
	return nil, notimpl.Err
}
