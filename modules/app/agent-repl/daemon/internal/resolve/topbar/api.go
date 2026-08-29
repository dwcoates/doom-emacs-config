// Package topbar is the topbar resolver: title and session line, the model
// selector, the permission-mode picker, the connectivity glyph, the warnings
// and the context chip.
//
// The model fact is LAST-WRITER-WINS in shim order. The connectivity glyph and
// its tone come from the render-colors vocabulary. See ARCHITECTURE.md
// "resolvers".
package topbar

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/notimpl"
	"claude-repld/internal/publish"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/vocab"
)

// Naming is the workspace's title and session line, from WSM.
type Naming struct {
	// Slug is the workspace's short name.
	Slug string
	// Title is the display title.
	Title string
	// Branch is the worktree's branch.
	Branch string
}

// Resolver is the topbar's whole surface.
type Resolver interface {
	sessionwatcher.TopbarSink

	// SetNaming installs the WSM-derived title and session line.
	SetNaming(ws ids.WorkspaceID, naming Naming)
	// SetModelCatalog installs the switchable model set the selector renders,
	// in display order.
	SetModelCatalog(ws ids.WorkspaceID, models []*conversationv1.ModelOption)
	// SetPermissionModePicker installs the picker: the mode in force plus
	// EXACTLY the switchable set the daemon will accept. SetPermissionMode
	// validates against what was served here.
	SetPermissionModePicker(ws ids.WorkspaceID, picker *frontendv1.TopbarPermissionModePicker)
	// SetAccount installs the account line read from the config root's
	// .claude.json.
	SetAccount(ws ids.WorkspaceID, email string)
	// Topic is the workspace's topbar publication.
	Topic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.TopbarView]
}

// New builds the topbar resolver. colors supplies the connectivity tone table,
// which the resolver asserts its emitted tones against.
func New(colors vocab.RenderColors, log dlog.Surfaces) (Resolver, error) {
	return nil, notimpl.Err
}
