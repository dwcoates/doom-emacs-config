package workspace

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"

	"claude-repld/internal/dirpath"
)

// The naming rule's constants. The rule itself — lowercase, hyphenated, at
// most three words — is stated to the model by
// prompts/workspace-name-from-prompt.md and ENFORCED here on its answer: a
// name is validated, never repaired.
const (
	// UnnamedSlugPrefix leads the name a create with neither a supplied name
	// nor an initial prompt is given: the workspace's own minted id, prefixed
	// so the roster reads it as an unnamed workspace rather than a hash.
	UnnamedSlugPrefix = "workspace-"
	// SlugWordLimit is the "3 words max" the naming briefs state.
	SlugWordLimit = 3
	// SlugMaxLen bounds the slug's length in characters.
	SlugMaxLen = 40
	// WorktreeDirSuffix is appended to a repository's own directory name to
	// form the SIBLING worktrees directory new worktrees are created in. It is
	// the spelling `agent-repl-worktree-dir-suffix' has carried all along.
	WorktreeDirSuffix = "-worktrees"
	// PrefixEnv names the workspace-name prefix. A prefix makes names
	// "<prefix>/<slug>" (the "DWC/" convention), and the worktree directory is
	// still the bare slug.
	PrefixEnv = "AGENT_WORKSPACE_PREFIX"
	// LegacyPrefixEnv is the older spelling of PrefixEnv external launchers
	// still set. PrefixEnv beats it.
	LegacyPrefixEnv = "CLAUDE_WORKSPACE_PREFIX"
)

// Prefix reports the workspace-name prefix in bare form (no trailing slash),
// empty when neither environment variable names one.
func Prefix() string {
	if v := os.Getenv(PrefixEnv); v != "" {
		return v
	}
	return os.Getenv(LegacyPrefixEnv)
}

// slugPattern is the whole naming rule, as the daemon enforces it on the
// model's answer: at most SlugWordLimit hyphen-separated lowercase
// alphanumeric words, with no leading or trailing hyphen, no slash and no path
// component.
//
// THERE IS NO REPAIR PATH. An answer that does not match is refused and the
// call is made again; word truncation — the old `Slug` — is deleted, because
// a truncated name is a name nobody chose.
var slugPattern = regexp.MustCompile(`^[a-z0-9]+(-[a-z0-9]+){0,` + strconv.Itoa(SlugWordLimit-1) + `}$`)

// ValidateSlug reports whether a naming answer is a legal slug, naming what is
// wrong with it when it is not.
func ValidateSlug(slug string) error {
	if slug == "" {
		return fmt.Errorf("the answer is empty")
	}
	if len(slug) > SlugMaxLen {
		return fmt.Errorf("the answer is %d characters, over the %d-character bound", len(slug), SlugMaxLen)
	}
	if !slugPattern.MatchString(slug) {
		return fmt.Errorf("the answer is not at most %d lowercase hyphen-separated alphanumeric words", SlugWordLimit)
	}
	return nil
}

// Name composes the workspace name (which is also the branch name) from the
// prefix and the slug: "<prefix>/<slug>" when a prefix is set, the bare slug
// otherwise.
func Name(prefix, slug string) string {
	if prefix == "" {
		return slug
	}
	return strings.TrimSuffix(prefix, "/") + "/" + slug
}

// BareName strips a name's prefix, which is the component the worktree
// directory is named after ("DWC/foo" yields "foo").
func BareName(name string) string {
	return filepath.Base(name)
}

// WorktreeDir answers where a new worktree for branch goes, given the
// repository directory it is cut from. The rule, unchanged from the system this
// replaces:
//
//   - the directory name is the branch's BARE component;
//   - when repoDir is the MAIN worktree (its .git is a directory), the parent
//     is the SIBLING directory "<repo basename><WorktreeDirSuffix>" beside it —
//     `~/.config/doom` yields `~/.config/doom-worktrees/<slug>`;
//   - when repoDir is itself a worktree (its .git is a regular file), the
//     parent is repoDir's own parent, so worktrees stay siblings of each other
//     rather than nesting.
//
// A repository is identified throughout the daemon by its COMMON DIR
// ("<repo>/.git"), which is what a create is handed; the placement rule is
// about the WORKTREE, so a common dir is read back to the main worktree it
// belongs to first.
//
// A branch with no safe directory component is an error rather than a guess.
func WorktreeDir(repoDir, branch string) (string, error) {
	bare := BareName(branch)
	if bare == "" || bare == "." || bare == string(filepath.Separator) {
		return "", fmt.Errorf("branch %q has no safe worktree directory name", branch)
	}
	repoDir = MainWorktreeDir(repoDir)
	gitPath := filepath.Join(repoDir, ".git")
	info, err := os.Stat(gitPath)
	if err != nil {
		return "", fmt.Errorf("stat %s: %w", gitPath, err)
	}
	parent := filepath.Dir(repoDir)
	if !info.Mode().IsRegular() {
		parent = filepath.Join(parent, filepath.Base(repoDir)+WorktreeDirSuffix)
	}
	return filepath.Join(parent, bare), nil
}

// MainWorktreeDir answers the worktree a repository directory names: a common
// dir ("<repo>/.git", the spelling the registry keys repositories by) reads
// back to "<repo>"; anything else is already a worktree and is answered as it
// stands.
func MainWorktreeDir(repoDir string) string {
	if filepath.Base(repoDir) == ".git" {
		return filepath.Dir(repoDir)
	}
	return repoDir
}

// IsWorktree reports whether dir is a git worktree at all: it holds a .git
// entry, either the main worktree's directory or a linked worktree's file. A
// directory that is neither is refused at registration.
func IsWorktree(dir string) bool {
	_, err := os.Stat(filepath.Join(dir, ".git"))
	return err == nil
}

// normalizeDir is the one spelling of "the same directory": dirpath.Canonical's
// (absolute, cleaned, symlinks resolved, on-disk case). Registration is
// idempotent by this, and a WorkspaceRef's dir is compared against it.
func normalizeDir(dir string) (string, error) {
	if dir == "" {
		return "", fmt.Errorf("a workspace directory is required")
	}
	normalized, err := dirpath.Canonical(dir)
	if err != nil {
		return "", fmt.Errorf("resolve %q: %w", dir, err)
	}
	return normalized, nil
}
