// Package commandfile is the command-file ingress:
// $AGENT_REPL_STATE_DIR/output/workspace_commands_*.json.
//
// Every command maps onto THE SAME internal path as the equivalent rpc — there
// is no second implementation of a verb. A malformed file is an error that is
// logged and the file retired; it never partially applies. See ARCHITECTURE.md
// "commandfile".
package commandfile

import (
	"context"

	"claude-repld/internal/dlog"
	"claude-repld/internal/merge"
	"claude-repld/internal/notimpl"
	"claude-repld/internal/workspace"
)

// Ingress watches the output directory and applies what it finds.
type Ingress interface {
	// ApplyFile reads and applies one command file, then retires it. A file
	// that does not parse is retired with an error record and applies nothing.
	ApplyFile(ctx context.Context, path string) error
	// Run watches the ingress directory until ctx is cancelled, applying each
	// file as it appears.
	Run(ctx context.Context) error
}

// Deps are the ingress's collaborators. It reaches the verbs and the merge
// queue only through their own interfaces, which is what keeps the two paths
// one implementation.
type Deps struct {
	// Dir is the ingress directory.
	Dir string
	// Glob matches the command files within it.
	Glob string
	// Verbs is the same verb surface the rpcs call.
	Verbs workspace.Verbs
	// Merge is the same orchestrator MergeWorkspace calls.
	Merge merge.Orchestrator
	// Log is the ingress's logger.
	Log dlog.Surfaces
}

// New builds the ingress.
func New(deps Deps) (Ingress, error) {
	return nil, notimpl.Err
}
