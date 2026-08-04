// Package postmerge resolves the postprocessing action a workspace was created
// with. Delivery belongs to merge.Coordinator: it runs the prompt against the
// merged workspace's own session while the merge lease is still held.
package postmerge

import (
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/workspace/merge"
)

// PostprocessingSource resolves the postprocessing prompt a workspace was
// created with, keyed by that workspace's worktree path.
type PostprocessingSource interface {
	PostprocessingPrompt(worktreePath string) (string, error)
}

// Config constructs a Source. Every field is required.
type Config struct {
	Logf           dlog.Logf
	Postprocessing PostprocessingSource
}

// Source implements merge.AfterActionSource.
type Source struct {
	logf           dlog.Logf
	postprocessing PostprocessingSource
}

var _ merge.AfterActionSource = (*Source)(nil)

// New validates cfg and returns the after-action source.
func New(cfg Config) (*Source, error) {
	switch {
	case cfg.Logf == nil:
		return nil, fmt.Errorf("postmerge: Source Logf is required")
	case cfg.Postprocessing == nil:
		return nil, fmt.Errorf("postmerge: Source Postprocessing is required")
	}
	return &Source{logf: cfg.Logf, postprocessing: cfg.Postprocessing}, nil
}

// AfterAction reads the merged workspace's creation-time postprocessing prompt.
func (s *Source) AfterAction(req merge.Request) (string, error) {
	prompt, err := s.postprocessing.PostprocessingPrompt(req.SourceDir)
	if err != nil {
		s.logf("postmerge: after-action prompt lookup FAILED {child_ws=%s child_name=%s child_dir=%s}: %v — the phase is published without its text",
			req.Workspace, req.Name, req.SourceDir, err)
		return "", fmt.Errorf("postmerge: resolve after-action prompt for %q: %w", req.Name, err)
	}
	return prompt, nil
}
