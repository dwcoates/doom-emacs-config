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
	"fmt"
	"path/filepath"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/intakegate"
	"claude-repld/internal/merge"
	"claude-repld/internal/prompthandler"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
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
	// DB resolves an entry that names a workspace only by DIRECTORY, which is
	// what the older producers write. An entry naming an id goes through the
	// verbs' own ref resolution instead.
	DB wsm.DB
	// Merge is the same orchestrator MergeWorkspace calls.
	Merge merge.Orchestrator
	// Prompts is the same SubmitPrompt body the rpc calls, which is what makes
	// a command-file prompt indistinguishable from a typed one.
	Prompts prompthandler.Handler
	// Log is the ingress's logger.
	Log dlog.Surfaces
	// Home is the absolute home directory a leading `~` in an entry's
	// directory expands to (dirpath.Absolute).
	Home string
	// Serves reports whether THIS daemon takes the intake now:
	// rollout.Controller.ServesIntake. A sweep while it answers false takes
	// nothing. See internal/intakegate.
	Serves func() bool

	// Interval is how often the directory is polled. fsnotify is deliberately
	// not a dependency: the ingress is a directory of small files written by
	// shell scripts, and a poll cannot miss one the way a dropped watch can.
	// It doubles as the SETTLING WINDOW — a file younger than one interval is
	// only claimed once it parses as a complete document, so a half-written
	// file is never ingested. Zero means DefaultInterval.
	Interval time.Duration
	// ClaimedDir is where a claimed file is renamed to; empty means
	// "claimed" beneath Dir.
	ClaimedDir string
	// QuarantineDir is where a malformed file is renamed to; empty means
	// "quarantine" beneath Dir.
	QuarantineDir string
	// Now supplies the instant a file's age is judged against; nil means
	// time.Now.
	Now func() time.Time
}

// DefaultInterval is the ingress's poll cadence and settling window.
const DefaultInterval = 250 * time.Millisecond

// DefaultGlob matches the files the ingress claims. It is the same pattern
// every producer writes to, and the dot-prefixed temp names producers write
// through deliberately do not match it.
const DefaultGlob = "workspace_commands_*.json"

// New builds the ingress.
func New(deps Deps) (Ingress, error) {
	switch {
	case deps.Dir == "":
		return nil, fmt.Errorf("commandfile: an ingress directory is required")
	case deps.Verbs == nil:
		return nil, fmt.Errorf("commandfile: the verb surface is required")
	case deps.Merge == nil:
		return nil, fmt.Errorf("commandfile: the merge orchestrator is required")
	case deps.Prompts == nil:
		return nil, fmt.Errorf("commandfile: the prompt handler is required")
	case deps.Log == nil:
		return nil, fmt.Errorf("commandfile: log surfaces are required")
	case deps.Serves == nil:
		return nil, fmt.Errorf("commandfile: the serving answer is required")
	case !filepath.IsAbs(deps.Home):
		return nil, fmt.Errorf("commandfile: an absolute home directory is required, got %q", deps.Home)
	}
	if deps.Glob == "" {
		deps.Glob = DefaultGlob
	}
	if deps.Interval <= 0 {
		deps.Interval = DefaultInterval
	}
	if deps.ClaimedDir == "" {
		deps.ClaimedDir = filepath.Join(deps.Dir, "claimed")
	}
	if deps.QuarantineDir == "" {
		deps.QuarantineDir = filepath.Join(deps.Dir, "quarantine")
	}
	if deps.Now == nil {
		deps.Now = time.Now
	}
	return &ingress{
		deps: deps,
		gate: intakegate.New(deps.Serves, deps.Log.Global().With(dlog.Context{"dir": deps.Dir}), opGate),
	}, nil
}
