package session

import (
	"os"
	"path/filepath"
	"strings"
)

// The account a workspace's CLI runs under is a FUNCTION OF ITS PATH, and this
// file is the daemon's single evaluation of that function.
//
// Emacs computes the same thing in agent-repl--compute-config-dir, and the two
// must agree byte for byte: a workspace whose CLI writes into
// ~/.claude-chesscom has no transcript under ~/.claude, so a disagreement is
// not a cosmetic drift but a resume that can never find its conversation.
//
// It used to be evaluated in exactly one place on the Go side — the
// list-transcripts subcommand — and workspace creation had no evaluation at
// all: it inherited the account from a live source session and, when there was
// no source session to inherit from, left the field empty and got the CLI
// default. That is fine for a repo whose account IS the default and silently
// wrong for every other repo. The first one-shot workspace ever created off a
// non-doom repo came up under ~/.claude while its parent ran under
// ~/.claude-chesscom, and nothing on either side said so.
const (
	// MultiRepoRootEnv names the directory whose subtree belongs to the
	// multi-repo account. It mirrors Emacs' agent-repl-multi-repo-root-env.
	MultiRepoRootEnv = "MULTI_REPO_ROOT"
	// MultiRepoConfigDirEnv overrides the multi-repo account's config root,
	// mirroring Emacs' agent-repl-multi-repo-config-dir.
	MultiRepoConfigDirEnv = "AGENT_REPL_MULTI_REPO_CONFIG_DIR"
	// DefaultMultiRepoConfigDir is that account's root when nothing overrides it.
	DefaultMultiRepoConfigDir = "~/.claude-chesscom"
)

// ExpandHome resolves a leading "~/" against the user's home directory and
// cleans the result. A path with no home prefix is returned cleaned.
func ExpandHome(path string) (string, error) {
	if path == "" {
		return "", nil
	}
	if path == "~" || strings.HasPrefix(path, "~/") {
		home, err := os.UserHomeDir()
		if err != nil {
			return "", err
		}
		path = filepath.Join(home, strings.TrimPrefix(strings.TrimPrefix(path, "~"), "/"))
	}
	return filepath.Clean(path), nil
}

// MultiRepoConfigDir returns the multi-repo account's config root, honoring the
// MultiRepoConfigDirEnv override.
func MultiRepoConfigDir() (string, error) {
	dir := os.Getenv(MultiRepoConfigDirEnv)
	if dir == "" {
		dir = DefaultMultiRepoConfigDir
	}
	return ExpandHome(dir)
}

// UnderDir reports whether path lies inside root (or is root itself), on
// cleaned paths so "/a/b" and "/a//b/." are one answer.
func UnderDir(root, path string) bool {
	if root == "" || path == "" {
		return false
	}
	root, path = filepath.Clean(root), filepath.Clean(path)
	if root == path {
		return true
	}
	return strings.HasPrefix(path, root+string(filepath.Separator))
}

// AccountConfigDirFor returns the config root a workspace at path must run
// under, in the SAME spelling CreateOpts.ConfigDir uses: the multi-repo
// account's absolute root for a path under $MULTI_REPO_ROOT, and "" — meaning
// the CLI's own default — for everything else.
//
// The empty string is deliberate and load-bearing: "" is how every existing
// record, wire field and registry row spells the default account, and minting
// an absolute ~/.claude here instead would make the same account compare
// unequal to itself across the daemon's records.
func AccountConfigDirFor(path string) (string, error) {
	if path == "" {
		return "", nil
	}
	resolved, err := ExpandHome(path)
	if err != nil {
		return "", err
	}
	root, err := ExpandHome(os.Getenv(MultiRepoRootEnv))
	if err != nil {
		return "", err
	}
	if !UnderDir(root, resolved) {
		return "", nil
	}
	return MultiRepoConfigDir()
}
