package workspace

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// Forget removes a workspace from the REGISTRY and touches nothing else. It is
// the undo for a registration, and it is a third thing beside the two verbs it
// is easily confused with:
//
//   - Close is reversible and keeps the record: the workspace leaves the tab
//     bar and Open brings it back.
//   - Nuke destroys the WORKTREE and the branch and then forgets the record —
//     which git refuses outright for a repository's main working tree, so a
//     plain registered directory could never be undone at all.
//   - Forget deletes the record and leaves every file alone. The directory
//     survives, and re-registering it mints a fresh record.
//
// It exists because nothing else ever removed a registry record for a
// directory the user registered by hand. Registering mints a workspace record
// AND a repository record; the register-then-clean-up cycle therefore left both
// behind, naming a path that may no longer exist, and a row naming a deleted
// path breaks workspace and sink resolution elsewhere.
//
// A FORGET STANDS THE SESSION DOWN FIRST, the way Nuke does. Close is
// VIEW-LEVEL and deliberately leaves the session alone, so a closed workspace
// routinely still has a live shim — and this is the LAST verb that can address
// it, because the row it resolves through is about to be deleted. A forget that
// skipped this orphaned a running shim, and the orphan is not merely a leak:
// the shim's workspace lock is keyed by the DIRECTORY while its socket is keyed
// by the workspace ID, so the next registration of that same directory minted a
// fresh id, probed the directory's lock, found it HELD by the orphan, chose the
// adopt path, and dialed a socket for an id no process had ever bound. Every
// prompt to that workspace then spent the whole adoption bound and was dropped.
//
// A FORGET REQUIRES A CLOSED WORKSPACE. It cannot close one for the user: a
// close is the verb that owns the quiet requirement (no turn in flight, no live
// work, NO HELD PROMPTS, no queued merge), and a close-then-forget would either
// duplicate that gate or bypass it, and bypassing it silently discards
// undelivered user intent. Forgetting an OPEN workspace would also leave Emacs
// holding tabs and the fleet holding a live shim for an id that no longer
// resolves, with no verb left that can address either. So the user closes
// first, and the refusal says so.
//
// THE ORPHAN GUARD is two more refusals on top of that:
//
//   - The close verb's quiet check is re-run here. A workspace can be recorded
//     closed and still have held prompts parked against it — a hold outlives a
//     close when the close raced it — and the forget would destroy them with
//     the row.
//   - A workspace another workspace names as its PARENT is refused. The
//     schema's parent_id is ON DELETE SET NULL, so the forget would silently
//     flatten a fork's lineage and drop its children out of their roster
//     nesting with nothing recording why.
func (v *verbs) Forget(ctx context.Context, ws ids.WorkspaceID) error {
	record, log, err := v.owned(ctx, "ForgetWorkspace", ws)
	if err != nil {
		return err
	}

	if !record.Closed {
		return refuse(log, "ForgetWorkspace", ArmNotClosed,
			fmt.Sprintf("workspace %q is open; close it before forgetting it", ws), false)
	}

	blocked, err := v.closeBlocker(ctx, ws)
	if err != nil {
		log.Error(opForget, "could not judge whether the workspace is quiet", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("forget %q: %w", ws, err)
	}
	if blocked != nil {
		log.Info(opForget, "refused a forget that would discard live state", dlog.Context{
			"reason": blocked.Reason, "detail": blocked.Detail,
		})
		return &Refusal{
			Rpc:    "ForgetWorkspace",
			Arm:    "blocked",
			Reason: blocked.Detail,
			Fields: map[string]any{
				"turn_in_flight": blocked.TurnInFlight,
				"live_work":      blocked.LiveWork,
				"held_prompts":   blocked.HeldPrompts,
				"merge_queued":   blocked.MergeQueued,
				"summary":        blocked.Detail,
			},
		}
	}

	children, err := v.childrenOf(ctx, ws)
	if err != nil {
		log.Error(opForget, "could not read the workspaces for the child check", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("forget %q: %w", ws, err)
	}
	if len(children) > 0 {
		return refuseWith(log, "ForgetWorkspace", ArmHasChildren,
			fmt.Sprintf("%d workspaces were spawned from %q; forget them first", len(children), ws),
			false, map[string]any{"children": childIDs(children)})
	}

	// THE SESSION GOES BEFORE THE ROW. `kill` is the same stand-down Nuke
	// takes before it destroys, and for the same reason: once the row is gone
	// no verb can name this workspace's shim again.
	if v.deps.Sessions.Held(ws) {
		log.Info(opForget, "standing the workspace's live session down before the record goes", dlog.Context{
			"dir": record.Dir,
		})
		if err := v.kill(ctx, log, ws); err != nil {
			log.Error(opForget, "the workspace's session could not be stood down", dlog.Context{
				"cause": err.Error(),
			})
			return fmt.Errorf("forget %q: %w", ws, err)
		}
	} else {
		log.Debug(opForget, "no live session to stand down before the forget", nil)
	}

	// THE FORGET IS RECORDED BEFORE THE ROW GOES, and through the workspace's
	// OWN sink, for the same reason the close is: written afterwards, the one
	// record explaining why this workspace's log ends here would be the first
	// line the registry could no longer attribute to anything.
	log.Info(opForget, "forgetting the workspace's registry record", dlog.Context{
		"dir": record.Dir, "branch": record.Branch,
	})

	report, err := v.deps.DB.Forget(ctx, ws)
	if err != nil {
		log.Error(opForget, "could not forget the workspace record", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("forget %q: %w", ws, err)
	}
	log.Info(opForget, "forgot the workspace", dlog.Context{
		"dir": record.Dir, "repository_forgotten": report.RepositoryDir,
	})

	// The log sinks are released the way a close releases them. A failed
	// eviction is a LEAK rather than a correctness failure, so it warns instead
	// of failing a forget the registry already carried out.
	if err := v.deps.Log.Evict(record.Dir); err != nil {
		log.Warn(opForget, "could not evict the workspace log sinks", dlog.Context{
			"dir": record.Dir, "cause": err.Error(),
		})
	}

	v.republishRegistry(ctx, log, opForget)
	return nil
}

// childrenOf answers the workspaces that name ws as the parent they were
// spawned from. It reads the registry rather than the branch lineage, because
// the nesting is a recorded fact and not one derived from git.
func (v *verbs) childrenOf(ctx context.Context, ws ids.WorkspaceID) ([]ids.WorkspaceID, error) {
	workspaces, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		return nil, fmt.Errorf("list the workspaces: %w", err)
	}
	var out []ids.WorkspaceID
	for _, candidate := range workspaces {
		if candidate.Parent != nil && *candidate.Parent == ws {
			out = append(out, candidate.ID)
		}
	}
	return out, nil
}

// childIDs renders the child ids as the strings the refusal's arm carries.
func childIDs(children []ids.WorkspaceID) []string {
	out := make([]string, 0, len(children))
	for _, child := range children {
		out = append(out, string(child))
	}
	return out
}
