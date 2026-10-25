package workspace

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"agent-repld/internal/dirpath"
	"agent-repld/internal/dlog"
	"agent-repld/internal/refusalarm"
)

// The naming rule's constants. The rule itself — lowercase, hyphenated, at
// most four words — is stated to the model by
// prompts/workspace-name-from-prompt.md and checked here on its answer: a
// name is validated, never repaired.
const (
	// UnnamedSlugPrefix leads the name a create with neither a supplied name
	// nor an initial prompt is given: the workspace's own minted id, prefixed
	// so the roster reads it as an unnamed workspace rather than a hash.
	UnnamedSlugPrefix = "workspace-"
	// SlugWordLimit is the "4 words max" the naming briefs state. Raised from
	// three (owner ruling, 2026-10-11): Haiku answered four-word names often
	// enough that a create failed both attempts ("agent-repl-emacs-bindings").
	SlugWordLimit = 4
	// SlugMaxLen bounds the slug's length in characters.
	SlugMaxLen = 40
	// WorktreeDirSuffix is appended to a repository's own directory name to
	// form the SIBLING worktrees directory new worktrees are created in. It is
	// the spelling `agent-repl-worktree-dir-suffix' has carried all along.
	WorktreeDirSuffix = "-worktrees"
	// PrefixEnv names the workspace-name prefix. A prefix makes names
	// "<prefix>/<slug>" (the "ABC/" convention), and the worktree directory is
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

// slugShape is the naming rule's SHAPE, as the daemon enforces it on the
// model's answer: hyphen-separated lowercase alphanumeric words, with no
// leading or trailing hyphen, no slash and no path component. The word COUNT
// is checked apart from the shape, against SlugWordLimit, so a refusal can
// name the count an over-long answer had.
//
// THERE IS NO REPAIR PATH. An answer that does not match is refused and the
// call is made again; word truncation — the old `Slug` — is deleted, because
// a truncated name is a name nobody chose. A well-shaped answer that is only
// over the limits is settled for once the retry is spent, since it is a name
// the model chose.
var slugShape = regexp.MustCompile(`^[a-z0-9]+(-[a-z0-9]+)*$`)

// SlugWordCount counts the words in a naming answer: its runs of ASCII letters
// and digits. On a well-shaped slug that is its hyphen-separated words; on any
// other answer ("Fix the login bug") it is still the count a person would
// read, which is what the naming call's correction reports back.
func SlugWordCount(answer string) int {
	return len(strings.FieldsFunc(answer, func(r rune) bool {
		return !(r >= 'a' && r <= 'z' || r >= 'A' && r <= 'Z' || r >= '0' && r <= '9')
	}))
}

// ValidateSlugShape reports whether a naming answer has the slug's SHAPE —
// a usable branch and directory name — whatever its word count or length.
// An answer of this shape that breaks only the limits is still a name the
// naming call can settle for once its retry is spent (namecall.go).
func ValidateSlugShape(slug string) error {
	if slug == "" {
		return fmt.Errorf("the answer is empty")
	}
	if !slugShape.MatchString(slug) {
		return fmt.Errorf("the answer is not at most %d lowercase hyphen-separated alphanumeric words", SlugWordLimit)
	}
	return nil
}

// ValidateSlug reports whether a naming answer is a legal slug — its shape
// AND its limits — naming what is wrong with it when it is not.
func ValidateSlug(slug string) error {
	if err := ValidateSlugShape(slug); err != nil {
		return err
	}
	if words := SlugWordCount(slug); words > SlugWordLimit {
		return fmt.Errorf("the answer is %d words, over the %d-word limit", words, SlugWordLimit)
	}
	if len(slug) > SlugMaxLen {
		return fmt.Errorf("the answer is %d characters, over the %d-character bound", len(slug), SlugMaxLen)
	}
	return nil
}

// prefixShape is a supplied name's PREFIX component (a person's initials, a
// team): ASCII letters and digits, optionally joined by single `.`, `_` or
// `-`. Unlike a slug it may be uppercase, because a prefix is often initials.
var prefixShape = regexp.MustCompile(`^[A-Za-z0-9]+([._-][A-Za-z0-9]+)*$`)

// The rules a supplied name can break, spelled as the
// agentrepl.v1.CreateWorkspaceInvalidName.rule arms.
const (
	// NameRuleSlug is a supplied name whose last component is not a slug.
	NameRuleSlug = refusalarm.Nested("slug")
	// NameRulePrefix is a supplied name with a component before the last that
	// is not a prefix.
	NameRulePrefix = refusalarm.Nested("prefix")
)

// NameBreach is where a supplied name broke the naming rule: the
// `/`-separated component, as it appears in the name, and the rule it broke.
type NameBreach struct {
	Component string
	Rule      refusalarm.Nested
}

// ValidateSuppliedName checks a name the user SUPPLIED, which becomes the
// workspace's branch, worktree directory and tab label as it stands. Its last
// `/`-separated component must have the slug's shape (ValidateSlugShape) with
// no word or length limit, and every earlier component must be a prefix
// (prefixShape). It answers the first breach, or nil for a legal name. The
// name is validated, never repaired.
func ValidateSuppliedName(name string) *NameBreach {
	components := strings.Split(name, "/")
	last := len(components) - 1
	for i, component := range components[:last] {
		if !prefixShape.MatchString(component) {
			return &NameBreach{Component: components[i], Rule: NameRulePrefix}
		}
	}
	if ValidateSlugShape(components[last]) != nil {
		return &NameBreach{Component: components[last], Rule: NameRuleSlug}
	}
	return nil
}

// refuseSuppliedName refuses SUPPLIED as RPC's invalid_name, recorded on
// LOG, when it breaks the naming rule (ValidateSuppliedName), and answers nil
// for a legal name. It is the ONE spelling of that refusal: a create's
// supplied name and a rename are held to one rule, so they are refused alike.
func refuseSuppliedName(log dlog.Logger, rpc, supplied string) error {
	breach := ValidateSuppliedName(supplied)
	if breach == nil {
		return nil
	}
	return refuseWith(log, rpc, ArmInvalidName,
		fmt.Sprintf("the name %q breaks the %s rule at %q", supplied, breach.Rule, breach.Component), false,
		map[string]any{"name": supplied, "component": breach.Component, "rule": breach.Rule})
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
// directory is named after ("ABC/foo" yields "foo").
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
