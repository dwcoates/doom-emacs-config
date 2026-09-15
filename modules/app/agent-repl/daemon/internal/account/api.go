// Package account determines which vendor config dir a workspace runs under,
// and reads what that root says about the account.
//
// Routing is BY PATH and resolved AT CREATE TIME, with the no-inheritance
// asymmetry: a parent merely under $MULTI_REPO_ROOT has chosen nothing. See
// docs/overhaul/daemon.md decision 3, "ACCOUNT SELECTION (MULTI_REPO_ROOT)".
//
// INVARIANT — THE PATH IS THE ONLY INPUT THIS PACKAGE TAKES. ConfigDirFor has
// no override parameter and inherits nothing from a parent workspace, so a
// routing answer that disagrees with the path is unrepresentable here.
//
// THE READER'S CHOICE LIVES ONE LAYER UP, and it outranks the routing. Owner
// ruling 2026-09-13: the topbar's account cell offers every root this package
// knows, and picking one (SelectAccount) makes that workspace's session spend
// as that account. The choice is recorded on the session row and applied by
// internal/workspace (`accountRootFor`); it never reaches this package, which
// still answers exactly one question — where does this PATH route.
package account

import (
	"context"
	"errors"
	"fmt"
	"time"

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

// AdoptableTranscript is the newest transcript found under a workspace's ROUTED
// config root: the candidate a no-record bring-up adopts instead of starting a
// brand-new vendor session, so a conversation begun in the interactive vendor
// CLI is continued rather than abandoned.
type AdoptableTranscript struct {
	Transcript
	// VendorSessionID is the transcript's own id — its filename stem — and the
	// id an adoption resumes by.
	VendorSessionID string
	// ModTime is the transcript file's modification time, the input to the
	// caller's idle guard: a file written within the guard's window may still
	// be held open by a live external process and must NOT be adopted.
	ModTime time.Time
	// LastRecordAt is the timestamp of the transcript's last parseable record,
	// the key the newest transcript was selected by. Zero when no record carried
	// a parseable timestamp, in which case ModTime was the selection key.
	LastRecordAt time.Time
}

// ErrNoTranscripts is NewestTranscript's answer when the routed root holds no
// transcript for the workspace at all. It is not a failure — a workspace whose
// folder was never touched by the vendor CLI simply has nothing to adopt — so
// the caller distinguishes it from a real read error and comes up fresh either
// way.
var ErrNoTranscripts = errors.New("account: no transcript to adopt under the routed root")

// RemintedID answers the child's identity for one of the parent's, under a
// single fork's mapping. It is memoized by the port that produced it: the same
// old id always answers the same new one, and an id the port never saw is
// minted on first ask.
type RemintedID func(old string) string

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
	// NewestTranscript answers the most-recent transcript already on disk for a
	// workspace, so a bring-up that has NO session record of its own can adopt
	// and continue it rather than mint an empty new conversation. It probes ONLY
	// the ROUTED config root — never the other account's root, because adopting
	// a transcript filed under a different account would run the conversation as
	// the wrong account — and selects the newest by its LAST-RECORD timestamp
	// (the last parseable JSON line's `timestamp`), falling back to file mtime
	// only for a transcript whose records carry no parseable timestamp. It
	// answers the file's mtime alongside, which is the caller's idle guard input.
	// A routed root that holds no transcript is ErrNoTranscripts, which is an
	// answer and not a failure.
	NewestTranscript(ctx context.Context, workspaceDir string) (AdoptableTranscript, error)
	// PortTranscript copies a parent's transcript (and its sidecar directory,
	// when the vendor wrote one) into the child's config root project dir,
	// which is what makes a forked workspace resumable. The daemon does this
	// BEFORE StartSession(resume). It refuses when the destination exists:
	// overwriting one conversation with another is never recovery.
	// The copy is filed under childVendorSessionID, NOT the parent's id: a
	// vendor session id is single-occupancy (the shim takes
	// session-<id>.lock inside StartSession), so a fork of a live parent that
	// resumed the parent's own id could never come up. The conversation's
	// CONTENT is untouched, its original agent id included.
	//
	// It answers THE MAPPING it re-minted under, so the caller can carry the
	// rest of the conversation — the daemon's own prompt rows, which the
	// vendor transcript does not hold — under the SAME ids. Two mappings for
	// one fork would file the child's questions under identities its ported
	// transcript never mentions.
	PortTranscript(ctx context.Context, transcriptPath, childConfigDir, childWorkspaceDir, childVendorSessionID string) (RemintedID, error)
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
