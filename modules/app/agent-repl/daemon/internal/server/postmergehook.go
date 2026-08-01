package server

import (
	"fmt"

	"claude-repld/internal/workspace/merge"
	"claude-repld/internal/workspace/postmerge"
)

// This file binds merge.Coordinator's post-merge hook.
//
// BOTH OF ITS DEPENDENCIES ARE DERIVED FROM ALREADY-INJECTED ONES rather than
// injected separately, for the same reason mergeConflictResolver is (see
// agentshimwire.go). The hook prompts the PARENT workspace's session, and the
// parent's session must be on the SAME controller fleet that serves the user's
// prompts — a separately injected session source could be bound to a different
// fleet, so the phone-home would land in a session nobody is looking at. The
// postprocessing prompt comes from the SAME creation records the create
// commands write, so a second store binding could answer from a file no
// creation ever wrote to.
//
// Each derivation is a CHECKED assertion whose failure is a hard construction
// error naming exactly what is missing. It is never a silent downgrade to a
// disabled hook: a daemon whose merges quietly stop notifying parents is the
// state this whole package exists to end.

// buildPostMergeHook assembles the post-merge notifier from cfg's existing
// dependencies.
func buildPostMergeHook(cfg AgentShimConfig, logf func(string, ...any)) (merge.PostMergeHook, error) {
	parents, ok := cfg.Prompts.(postmerge.ParentSession)
	if !ok {
		return nil, fmt.Errorf("server: the PromptRouter (%T) cannot answer whether a workspace has a live session, so the post-merge parent handoff has no way to avoid spawning a session for a workspace nobody has open", cfg.Prompts)
	}
	postprocessing, ok := cfg.WorkspaceCreation.(postmerge.PostprocessingSource)
	if !ok {
		return nil, fmt.Errorf("server: the WorkspaceCreation bridge (%T) cannot resolve a workspace's postprocessing prompt, so a workspace created with one would merge and never run it", cfg.WorkspaceCreation)
	}
	probe, err := postmerge.NewGitWorktreeProbe(logf)
	if err != nil {
		return nil, fmt.Errorf("server: build post-merge worktree probe: %w", err)
	}
	notifier, err := postmerge.New(postmerge.Config{
		Logf:           logf,
		Parents:        parents,
		Worktrees:      probe,
		Postprocessing: postprocessing,
	})
	if err != nil {
		return nil, fmt.Errorf("server: build post-merge notifier: %w", err)
	}
	return notifier, nil
}
