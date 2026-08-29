// Package account determines which vendor config dir a workspace runs under,
// and reads what that root says about the account.
//
// Routing is BY PATH and resolved AT CREATE TIME, with the no-inheritance
// asymmetry: a parent merely under $MULTI_REPO_ROOT has chosen nothing. See
// docs/overhaul/daemon.md decision 3, "ACCOUNT SELECTION (MULTI_REPO_ROOT)".
//
// INVARIANT — THE ACCOUNT IS DETERMINED, NEVER SELECTED. The path is the only
// input this package takes. There is no override parameter, no inheritance
// from a parent workspace, and no request field anywhere in the graph that
// could carry one, so a workspace whose account disagrees with its path is
// structurally unrepresentable.
package account

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
)

// Roots are the two config roots the daemon routes between.
type Roots struct {
	// Default is the config dir for a workspace outside the multi-repo root.
	Default string
	// MultiRepo is the config dir for a workspace under $MULTI_REPO_ROOT.
	MultiRepo string
	// MultiRepoRoot is the path prefix that selects MultiRepo.
	MultiRepoRoot string
}

// Account is what a config root says about who is signed in.
type Account struct {
	// ConfigDir is the root the facts came from.
	ConfigDir string
	// Email is the signed-in address from the root's .claude.json, empty when
	// the file names none.
	Email string
	// LoggedIn reports whether the root holds usable credentials.
	LoggedIn bool
}

// Transcript is one vendor transcript located on disk.
type Transcript struct {
	// Path is the `<root>/projects/<encoded cwd>/<vendor uuid>.jsonl` file.
	Path string
	// ConfigDir is the root the transcript was found under. An account switch
	// is exactly the case where this is not the routed root.
	ConfigDir string
	// SidecarDir is the `<vendor uuid>/` directory beside the transcript, empty
	// when the vendor wrote none.
	SidecarDir string
}

// NotFoundError is the answer when no root holds the asked-for transcript. It
// is the RESUME GUARD's input: a resume whose transcript is missing is refused
// with a typed arm BEFORE any process spawns, because a vanished file yields
// no death evidence and the redial ladder would otherwise loop forever on an
// unchangeable fact.
type NotFoundError struct {
	// VendorSessionID is the uuid that was looked for.
	VendorSessionID string
	// WorkspaceDir is the cwd whose project dir was probed.
	WorkspaceDir string
	// Probed are the paths that were stat'd, in probe order.
	Probed []string
}

func (e *NotFoundError) Error() string {
	return fmt.Sprintf("account: no transcript for vendor session %s under %v", e.VendorSessionID, e.Probed)
}

// Resolver answers the account questions.
type Resolver interface {
	// ConfigDirFor routes a workspace directory to its config root: under
	// MultiRepoRoot yields MultiRepo, anything else yields Default. A parent
	// merely under the root has chosen nothing, so nothing is inherited.
	//
	// The path handed in is the workspace's MAIN-REPO directory — for a plain
	// checkout that is the workspace dir itself, and for a worktree the caller
	// (internal/workspace, which owns the git client) resolves the common dir
	// first. This package stays a leaf and never shells out to git.
	ConfigDirFor(workspaceDir string) string
	// Read loads one config root's account facts from its .claude.json.
	Read(ctx context.Context, configDir string) (Account, error)
	// Roster is both roots' accounts, for the topbar's account display.
	Roster(ctx context.Context) ([]Account, error)
	// FindTranscript locates a vendor session's transcript, probing the routed
	// root first and the other root second, and answers WHICH root held it so
	// the caller can tell a same-account resume from an account switch. A miss
	// is a *NotFoundError.
	FindTranscript(ctx context.Context, workspaceDir, vendorSessionID string) (Transcript, error)
	// PortTranscript copies a parent's transcript (and its sidecar directory,
	// when the vendor wrote one) into the child's config root project dir,
	// which is what makes a forked workspace resumable. The daemon does this
	// BEFORE StartSession(resume). It refuses when the destination exists:
	// overwriting one conversation with another is never recovery.
	PortTranscript(ctx context.Context, transcriptPath, childConfigDir, childWorkspaceDir string) error
	// MoveTranscript MOVES a transcript (and its sidecar directory) into
	// toConfigDir's project dir for the same workspace. It is the
	// account-switch spelling: the daemon ports the vendor transcript between
	// the two roots itself, as a file move before the ordinary resume under the
	// new root, with no shim involvement. It refuses when the destination
	// exists.
	MoveTranscript(ctx context.Context, transcriptPath, toConfigDir, workspaceDir string) error
}

// New builds the resolver from the two roots. Both roots must be named: the
// account is determined, so an unnamed root would leave a workspace with no
// answer at all.
func New(roots Roots, log dlog.Logger) (Resolver, error) {
	return newResolver(roots, log)
}
