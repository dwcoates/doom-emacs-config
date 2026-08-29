// Package account determines which vendor config dir a workspace runs under,
// and reads what that root says about the account.
//
// Routing is BY PATH and resolved AT CREATE TIME, with the no-inheritance
// asymmetry: a parent merely under $MULTI_REPO_ROOT has chosen nothing. See
// docs/overhaul/daemon.md decision 3, "ACCOUNT SELECTION (MULTI_REPO_ROOT)".
package account

import (
	"context"

	"claude-repld/internal/notimpl"
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

// Resolver answers the account questions.
type Resolver interface {
	// ConfigDirFor routes a workspace directory to its config root: under
	// MultiRepoRoot yields MultiRepo, anything else yields Default. A parent
	// merely under the root has chosen nothing, so nothing is inherited.
	ConfigDirFor(workspaceDir string) string
	// Read loads one config root's account facts from its .claude.json.
	Read(ctx context.Context, configDir string) (Account, error)
	// Roster is both roots' accounts, for the topbar's account display.
	Roster(ctx context.Context) ([]Account, error)
	// FindTranscript locates a vendor session's transcript, probing the routed
	// root first. When the same vendor uuid exists under both roots it
	// disambiguates rather than picking one.
	FindTranscript(ctx context.Context, workspaceDir, vendorSessionID string) (string, error)
	// PortTranscript copies a parent's transcript into the child's config
	// root project dir, which is what makes a forked workspace resumable. The
	// daemon does this BEFORE StartSession(resume).
	PortTranscript(ctx context.Context, transcriptPath, childConfigDir, childWorkspaceDir string) error
}

// New builds the resolver from the two roots.
func New(roots Roots) (Resolver, error) {
	return nil, notimpl.Err
}
