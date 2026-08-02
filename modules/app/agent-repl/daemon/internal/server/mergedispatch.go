package server

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ErrMergeWorkspaceUnrecorded marks a dispatched merge whose project_dir is not
// a workspace the daemon has a geometry record for.
//
// It is a REJECTION, not a structural failure: the command file that carried it
// is quarantined and the ingress moves on. The distinction matters because the
// two failures want opposite handling — a rejected file must never be re-claimed
// forever, while an unreachable queue must be retried rather than discarded.
var ErrMergeWorkspaceUnrecorded = errors.New("server: merge names no workspace the daemon recorded")

// MergeDispatch routes the daemon's OWN merge ingress — the "merge" verb in a
// workspace command file — onto the merge queue.
//
// It exists because that verb no longer round-trips through Emacs. The editor
// used to receive it as a host action, resolve the workspace name heuristically
// (literal name, then branch tail, then a project_dir lookup), and send a
// merge_workspace command back. Two resolvers of one map is how a merge reached
// a workspace nobody named, so there is now exactly one key and exactly one map:
//
//   - THE KEY is the entry's project_dir, an absolute path, matched EXACTLY
//     (filepath.Clean only) against the daemon's records. No name resolution, no
//     branch tail, no prefix match, no fallback of any kind.
//   - THE MAP is geometry.Store, the same record MergeWorkspace resolves
//     through, so a dispatched merge and a frontend merge cannot disagree about
//     where a workspace's commits land.
//
// Everything past resolution is the ordinary merge path: MergeWorkspace marks
// merge_enqueuing, resolves the geometry, and enqueues on the repository's
// queue. Reusing it is deliberate — a second enqueue path would be a second
// place for the merge phases to be recorded differently.
type MergeDispatch struct {
	geometry MergeGeometrySource
	merges   frontendMergeCommand
	logf     func(string, ...any)
}

// frontendMergeCommand is the narrow slice of the frontend command handler a
// dispatched merge needs. Declaring it (rather than taking *commandHandler)
// keeps MergeDispatch unit-testable without standing up the whole handler.
type frontendMergeCommand interface {
	MergeWorkspace(ctx context.Context, workspace, requestID string, cmd *frontendv1.MergeWorkspaceCmd) error
}

// NewMergeDispatch validates its dependencies.
//
// GEOMETRY MAY BE NIL, mirroring AgentShimConfig.MergeGeometry: a daemon wired
// without the map has an unsupported merge capability, not a broken one. Every
// dispatch is then refused loudly and structurally (never as a rejection, and
// never as a guessed target) — see DispatchMerge.
func NewMergeDispatch(geometry MergeGeometrySource, merges frontendMergeCommand, logf func(string, ...any)) (*MergeDispatch, error) {
	switch {
	case merges == nil:
		return nil, fmt.Errorf("server: merge dispatch needs a merge command surface")
	case logf == nil:
		return nil, fmt.Errorf("server: merge dispatch needs a logger")
	}
	return &MergeDispatch{geometry: geometry, merges: merges, logf: logf}, nil
}

// DispatchMerge resolves projectDir to a recorded workspace and merges it.
//
// NAME is the emitter's display name. It is carried for the merge tag and the
// logs and is NEVER resolved against anything: a name that disagrees with the
// project_dir loses, because the path is the identity every other axis of the
// daemon keys on.
//
// The geometry lookup here is not a duplicate of MergeWorkspace's own: this one
// exists to classify an unrecorded workspace as a REJECTION (see
// ErrMergeWorkspaceUnrecorded) rather than as the generic refusal a frontend
// command reports, so the ingress can quarantine the file instead of retrying
// it forever.
func (d *MergeDispatch) DispatchMerge(ctx context.Context, name, projectDir, requestID string) error {
	if projectDir == "" || !filepath.IsAbs(projectDir) {
		d.logf("server: merge dispatch REFUSED name=%q project_dir=%q request_id=%s reason=project-dir-must-be-absolute", name, projectDir, requestID)
		return fmt.Errorf("%w: project_dir %q is not an absolute path", ErrMergeWorkspaceUnrecorded, projectDir)
	}
	key := filepath.Clean(projectDir)
	if d.geometry == nil {
		d.logf("server: merge dispatch REFUSED name=%q project_dir=%s request_id=%s reason=no-geometry-source", name, key, requestID)
		return fmt.Errorf("server: merge dispatch for %s cannot be resolved: the daemon's merge-geometry record is not wired", key)
	}
	_, found, err := d.geometry.Lookup(ctx, key)
	if err != nil {
		d.logf("server: merge dispatch geometry lookup FAILED name=%q project_dir=%s request_id=%s: %v", name, key, requestID, err)
		return fmt.Errorf("server: merge dispatch for %s: read recorded geometry: %w", key, err)
	}
	if !found {
		d.logf("server: merge dispatch REJECTED name=%q project_dir=%s request_id=%s reason=no-recorded-workspace", name, key, requestID)
		return fmt.Errorf("%w: no workspace is recorded at project_dir %s (workspace name %q is diagnostic only and is never resolved)", ErrMergeWorkspaceUnrecorded, key, name)
	}
	d.logf("server: merge dispatch RESOLVED name=%q project_dir=%s request_id=%s", name, key, requestID)
	return d.merges.MergeWorkspace(ctx, key, requestID, &frontendv1.MergeWorkspaceCmd{WorkspaceName: name})
}
