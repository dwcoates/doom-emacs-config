package wsm

import "fmt"

// IsMainWorktree reports whether the workspace at workspaceDir is the main
// worktree of the repository at repoDir. A repository is keyed by its main
// worktree's directory, so the main worktree is exactly the workspace whose
// directory IS the repository's. Both are the normalized spellings the
// registry stores.
//
// It is the ONE spelling of that rule: the registry's mint and forget guards
// and the verbs that register and forget all ask it, so the rule cannot drift
// between them.
func IsMainWorktree(workspaceDir, repoDir string) bool {
	return workspaceDir == repoDir
}

// MainWorkspaceName is the name of a repository's main worktree workspace,
// "main (<repository-name>)", where REPOSITORY is the repository's display
// name (Repository.Name). It is the ONE spelling of that name: registration
// derives it, and the boot renames every main workspace registered before the
// rule to it, so the two cannot drift. It is deliberately not a name a user can
// supply (ValidateSuppliedName refuses its space and parentheses).
func MainWorkspaceName(repository string) string {
	return fmt.Sprintf("main (%s)", repository)
}
