package wsm

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
