package main

import (
	"context"
	"fmt"
	"path/filepath"
	"sort"

	"claude-repld/internal/registry"
	"claude-repld/internal/workspace/geometry"

	workspacecreate "claude-repld/internal/workspace/create"
)

// daemonGeometryRecorder turns a completed create job into the workspace's
// durable merge geometry.
//
// It lives in cmd (not in the create package) for the same reason DaemonWorktree
// does: path spelling and the choice of parent worktree are adapter business,
// and the create manager stays pure orchestration.
type daemonGeometryRecorder struct {
	Store *geometry.Store
	Logf  func(string, ...any)
}

var _ workspacecreate.WorkspaceGeometryRecorder = daemonGeometryRecorder{}

// RecordWorkspaceGeometry writes the three coordinates the daemon OBSERVED
// while creating this worktree.
//
//   - SourceDir is the worktree the daemon just added, which is also the
//     workspace key every other axis (the registry cwd, the SSM's rows, the
//     merge command's envelope) is filed under.
//   - SourceBranch is the branch the daemon planned and created it on.
//   - TargetDir is the worktree the workspace was cut FROM: the nominated source
//     workspace when the create named one, else the git root the worktree was
//     added to. Both are facts of the request that produced this worktree, which
//     is exactly the parent a merge is expected to land in.
//
// Every missing fact is an error. Nothing is defaulted: a workspace recorded
// with a guessed target is a cherry-pick into a repository nobody asked for, and
// the failure is far cheaper here (the create fails loudly) than at merge time.
func (r daemonGeometryRecorder) RecordWorkspaceGeometry(ctx context.Context, job workspacecreate.Job) error {
	if r.Store == nil || r.Logf == nil {
		return fmt.Errorf("workspace create: geometry recorder is not fully configured")
	}
	if job.WorktreePath == "" || job.Branch == "" {
		return fmt.Errorf("workspace create: job %s has no persisted worktree identity to record geometry from (path=%q branch=%q)", job.ID, job.WorktreePath, job.Branch)
	}
	target := job.Request.SourceDir
	if target == "" {
		target = job.Request.GitRoot
	}
	if target == "" {
		return fmt.Errorf("workspace create: job %s names neither a source workspace directory nor a git root, so it has no merge target", job.ID)
	}
	target, err := normalizeWorkspacePath(target)
	if err != nil {
		return fmt.Errorf("workspace create: job %s merge target %q: %w", job.ID, target, err)
	}
	rec := geometry.Record{
		Workspace:    filepath.Clean(job.WorktreePath),
		SourceBranch: job.Branch,
		SourceDir:    filepath.Clean(job.WorktreePath),
		TargetDir:    target,
		// The creation request's `before_ws_merge` is a merge coordinate like the
		// other three, and this is the one moment it is known as a fact. The merge
		// pipeline reads it back out of this record when the run starts.
		BeforeAction: job.Request.BeforeWSMerge,
		Origin:       geometry.OriginCreated,
	}
	if err := r.Store.Record(ctx, rec); err != nil {
		return fmt.Errorf("workspace create: job %s record merge geometry: %w", job.ID, err)
	}
	r.Logf("workspace-create: recorded merge geometry job=%s workspace=%s branch=%q target=%s", job.ID, rec.Workspace, rec.SourceBranch, rec.TargetDir)
	return nil
}

// registryGeometryLister is the boot backfill's candidate source: every live
// session's working directory IS a workspace the daemon still serves.
//
// Terminal records are excluded because they name a session that ended, not a
// workspace anyone can still merge, and a dead session's worktree is routinely
// gone — deriving geometry for it would produce nothing but noise.
type registryGeometryLister struct{ Reg *registry.Registry }

var _ geometry.WorkspaceLister = registryGeometryLister{}

func (l registryGeometryLister) GeometryBackfillCandidates() []string {
	if l.Reg == nil {
		return nil
	}
	seen := map[string]struct{}{}
	for _, rec := range l.Reg.All() {
		if rec.Terminal || rec.CWD == "" {
			continue
		}
		seen[geometry.Key(rec.CWD)] = struct{}{}
	}
	out := make([]string, 0, len(seen))
	for cwd := range seen {
		out = append(out, cwd)
	}
	sort.Strings(out)
	return out
}
