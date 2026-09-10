package workspace

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// The naming rule's constants, adopted from the conventions
// prompts/workspace-generation-name-{prefixed,unprefixed}.md state — lowercase,
// hyphenated, at most three words — plus the branch-name length bound the old
// system used.
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

// Slug derives a workspace slug from free text by the naming rule: lowercase,
// hyphen-separated, at most SlugWordLimit words, at most SlugMaxLen
// characters. Text that yields nothing at all is an error — a workspace is
// never given a made-up name.
func Slug(text string) (string, error) {
	words := words(text)
	if len(words) == 0 {
		return "", fmt.Errorf("no slug can be derived from %q", text)
	}
	if len(words) > SlugWordLimit {
		words = words[:SlugWordLimit]
	}
	slug := strings.Join(words, "-")
	if len(slug) > SlugMaxLen {
		slug = slug[:SlugMaxLen]
		// Never end on the hyphen the truncation landed in the middle of: a
		// trailing hyphen is not a legal branch-name component tail.
		slug = strings.TrimRight(slug, "-")
	}
	if slug == "" {
		return "", fmt.Errorf("no slug can be derived from %q", text)
	}
	return slug, nil
}

// words splits free text into the slug's lowercase alphanumeric words. Runs of
// anything else separate; a digit or letter run is one word.
func words(text string) []string {
	var out []string
	var cur strings.Builder
	flush := func() {
		if cur.Len() > 0 {
			out = append(out, cur.String())
			cur.Reset()
		}
	}
	for _, r := range strings.ToLower(text) {
		switch {
		case r >= 'a' && r <= 'z', r >= '0' && r <= '9':
			cur.WriteRune(r)
		default:
			flush()
		}
	}
	flush()
	return out
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

// normalizeDir is the one spelling of "the same directory": absolute, cleaned,
// and with symlinks resolved when the path exists. Registration is idempotent
// by this, and a WorkspaceRef's dir is compared against it.
func normalizeDir(dir string) (string, error) {
	if dir == "" {
		return "", fmt.Errorf("a workspace directory is required")
	}
	abs, err := filepath.Abs(dir)
	if err != nil {
		return "", fmt.Errorf("resolve %q: %w", dir, err)
	}
	if resolved, err := filepath.EvalSymlinks(abs); err == nil {
		return filepath.Clean(resolved), nil
	}
	return filepath.Clean(abs), nil
}
