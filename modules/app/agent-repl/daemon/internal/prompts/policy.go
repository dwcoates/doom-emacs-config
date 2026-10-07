package prompts

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// A REPOSITORY DEFINES ITS OWN ONE-SHOT AND MERGE POLICY, in files in its own
// tree. Owner ruling, 2026-09-12
// (docs/REALTEST-JUDGEMENT-CALLS.md, "one-shot policy is the repository's").
//
// The daemon's prompt corpus (the module's `prompts/`) is the policy of
// EXACTLY ONE repository — the one the daemon's own checkout lives in — and is
// never a fallback for any other. Every other repository states its policy in
// `.agent-repl/prompts/` at its main checkout root, in the same brief format
// the corpus uses, and a repository that states none does not inherit the
// corpus: the one-shot is refused instead.

// PolicyDirSegments are the path segments, relative to a repository's MAIN
// CHECKOUT ROOT, of the directory a repository states its policy in.
var PolicyDirSegments = []string{".agent-repl", "prompts"}

// PolicyDirRel is the policy directory as an author writes it in prose and in
// documentation.
const PolicyDirRel = ".agent-repl/prompts"

// The two policy sources a repository can have. They are recorded in the log
// so a one-shot's decoration can always be traced to the files it read.
const (
	// SourceCorpus is the daemon's own prompt corpus, which is the policy of
	// the repository the daemon's checkout lives in and of no other.
	SourceCorpus = "corpus"
	// SourceRepository is the repository's own `.agent-repl/prompts`.
	SourceRepository = "repository"
)

// The merge briefs a repository's policy may name. Both are OPTIONAL: a
// repository that states neither simply has no configured merge action, which
// is what every repository had before this policy existed.
const (
	// PolicyMergeBefore runs as the before-merge action of every merge of a
	// workspace in the repository.
	PolicyMergeBefore = "merge-before"
	// PolicyMergeAfter runs as the after-merge action of the same.
	PolicyMergeAfter = "merge-after"
)

// Source is where one repository's policy briefs are read from.
type Source struct {
	// Dir is the directory the briefs are loaded from.
	Dir string
	// Kind is SourceCorpus or SourceRepository.
	Kind string
	// RepositoryRoot is the repository's main checkout root, as resolved.
	RepositoryRoot string
}

// PolicyDir is a repository's own policy directory beneath its main checkout
// root.
func PolicyDir(repoRoot string) string {
	return filepath.Join(append([]string{repoRoot}, PolicyDirSegments...)...)
}

// SourceFor answers where repoRoot's policy briefs are read from.
//
// The daemon's corpus answers for the ONE repository the daemon's own checkout
// lives in — the module checkout root, which is either the repository root
// itself or a directory inside it — and corpusDir is that repository's policy
// directory. Every other repository's policy is its own `.agent-repl/prompts`,
// and the corpus is not consulted for it at all.
func SourceFor(repoRoot, checkoutRoot, corpusDir string) Source {
	if IsModuleRepository(repoRoot, checkoutRoot) {
		return Source{Dir: corpusDir, Kind: SourceCorpus, RepositoryRoot: repoRoot}
	}
	return Source{Dir: PolicyDir(repoRoot), Kind: SourceRepository, RepositoryRoot: repoRoot}
}

// IsModuleRepository reports whether repoRoot is the repository the daemon's
// own module checkout lives in: the checkout root itself, or an ancestor of
// it. A repository root is a MAIN WORKTREE and the module checkout is a
// directory inside it (`modules/app/agent-repl`), so the containment case is
// the ordinary one and the equality case covers a checkout deployed at a
// repository root.
func IsModuleRepository(repoRoot, checkoutRoot string) bool {
	if repoRoot == "" || checkoutRoot == "" {
		return false
	}
	repo := filepath.Clean(repoRoot)
	checkout := filepath.Clean(checkoutRoot)
	if repo == checkout {
		return true
	}
	return strings.HasPrefix(checkout, repo+string(filepath.Separator))
}

// Files is the policy directory's filesystem probe: which of a set of briefs a
// directory does not hold, and the text of one it does. It is an interface
// because the create and merge paths are unit-tested against a policy that
// never touches a disk; OnDisk is the production value.
type Files interface {
	// Missing answers the FILE NAMES (each with the .md suffix) of the named
	// briefs dir does not hold, in the order given. An empty answer means dir
	// holds every one of them.
	Missing(dir string, names []string) []string
	// Text loads one brief from dir and answers its body with no placeholders
	// spliced. A brief that declares placeholders is an error: a policy brief
	// is submitted verbatim and there is nothing to fill them from.
	Text(dir, name string) (string, error)
}

// OnDisk is the production Files: the real directory, read at EVERY use, so
// editing a repository's policy takes effect on the next one-shot or merge
// without a daemon bounce.
type OnDisk struct{}

// Missing implements Files.
func (OnDisk) Missing(dir string, names []string) []string {
	var missing []string
	for _, name := range names {
		if !Has(dir, name) {
			missing = append(missing, name+Suffix)
		}
	}
	return missing
}

// Text implements Files.
func (OnDisk) Text(dir, name string) (string, error) {
	prompt, err := Load(dir, name)
	if err != nil {
		return "", err
	}
	if len(prompt.Placeholders) > 0 {
		return "", fmt.Errorf("prompts: %s declares placeholders %v, and a policy brief is submitted verbatim",
			Path(dir, name), prompt.Placeholders)
	}
	return prompt.Body, nil
}

// Has reports whether dir holds the named brief as a regular file.
func Has(dir, name string) bool {
	info, err := os.Stat(Path(dir, name))
	return err == nil && info.Mode().IsRegular()
}
