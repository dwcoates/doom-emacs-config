package workspace

import (
	"context"
	"fmt"
	"strings"

	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/refusalarm"
	"agent-repld/internal/wsm"
)

// rpcSetName is the rpc a rename's refusals are spelled under.
const rpcSetName = "SetWorkspaceName"

// SetName renames WS (agentrepl.v1.SetWorkspaceName).
//
// THE NAME ALONE CHANGES. The branch and the worktree directory keep the
// names the workspace was created with: the live session works in that
// directory and the vendor keeps its transcript under it, and merges and
// deploys name the branch.
//
// The requested name follows a supplied create name's rule and takes the
// same prefix (ValidateSuppliedName, suppliedBranch). It is refused when
// another workspace holds it as its name or branch, or a pending reservation
// or claim holds it (holdersLocked). The check and the write happen under the
// naming lock, so no create or async merge can hand the name out between them.
//
// Every surface that draws the name is republished (publishRename), and
// everything keyed by the workspace's id stands as it was.
func (v *verbs) SetName(ctx context.Context, ws ids.WorkspaceID, requested string) (string, error) {
	record, log, err := v.owned(ctx, rpcSetName, ws)
	if err != nil {
		return "", err
	}
	supplied := strings.TrimSpace(requested)
	if breach := ValidateSuppliedName(supplied); breach != nil {
		return "", refuseWith(log, rpcSetName, ArmInvalidName,
			fmt.Sprintf("the name %q breaks the %s rule at %q", supplied, breach.Rule, breach.Component), false,
			map[string]any{"name": supplied, "component": breach.Component, "rule": breach.Rule})
	}
	name := suppliedBranch(supplied)
	fields := dlog.Context{"name": name, "previous": record.Name, "branch": record.Branch}
	if name == record.Name {
		log.Info(opSetName, "the workspace already has the name; nothing changed", fields)
		return name, nil
	}
	if err := v.recordName(ctx, log, ws, name, fields); err != nil {
		return "", err
	}
	log.Info(opSetName, "renamed the workspace", fields)
	v.publishRename(ctx, log, ws, name)
	return name, nil
}

// recordName checks NAME is free to WS and records it, under the naming lock.
func (v *verbs) recordName(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, name string, fields dlog.Context) error {
	v.names.mu.Lock()
	defer v.names.mu.Unlock()
	held, err := v.holdersLocked(ctx, "")
	if err != nil {
		log.Error(opSetName, "could not read the names already taken; the workspace is not renamed", dlog.WithCause(fields, err))
		return fmt.Errorf("rename %q: %w", ws, err)
	}
	if holder, taken := held[name]; taken && holder.Workspace != ws {
		return v.nameTaken(ctx, log, name, holder)
	}
	if err := v.deps.DB.SetName(ctx, ws, name); err != nil {
		log.Error(opSetName, "could not record the workspace's new name", dlog.WithCause(fields, err))
		return fmt.Errorf("rename %q: %w", ws, err)
	}
	return nil
}

// nameTaken is the name_taken refusal of NAME, naming what holds it: the
// registered workspace, by its ref, or the nested `pending` arm.
func (v *verbs) nameTaken(ctx context.Context, log dlog.Logger, name string, holder nameHolder) error {
	if holder.Workspace == "" {
		return refuseWith(log, rpcSetName, ArmNameTaken,
			fmt.Sprintf("the name %q is reserved by a pending async merge or claimed by a create in flight", name), false,
			map[string]any{"name": name, "holder": refusalarm.Nested("pending")})
	}
	ref := &workspacev1.WorkspaceRef{Id: string(holder.Workspace)}
	if record, err := v.deps.DB.Workspace(ctx, holder.Workspace); err == nil {
		ref.Dir = record.Dir
	} else {
		// The refusal stands on the id alone; the directory is the ref's
		// courtesy, and the read that failed is recorded where it failed.
		log.Error(opSetName, "could not read the workspace holding the name; the refusal names it by id", dlog.Context{
			"name": name, "holder": string(holder.Workspace), "cause": err.Error()})
	}
	return refuseWith(log, rpcSetName, ArmNameTaken,
		fmt.Sprintf("the name %q is already workspace %s's name or branch", name, holder.Workspace), false,
		map[string]any{"name": name, "workspace": ref})
}

// publishRename republishes every surface that draws WS's name, once the new
// NAME is recorded:
//
//   - the roster, which renames the Emacs tab in place by its id;
//   - the topbar's workspace-name line;
//   - the host view, whose naming slug names the workspace's Emacs buffers;
//   - the footer's awaiting line of every workspace that awaits WS;
//   - the merge queues WS stands in.
//
// The rename is recorded before any of these runs, so a surface that cannot
// be republished is recorded at ERROR by its owner and the rest still are.
func (v *verbs) publishRename(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, name string) {
	v.republishRegistry(ctx, log, opSetName)
	v.deps.Topbar.SetName(ws, name)
	v.deps.Host.PublishHostWorkspace(ws)
	v.relabelAwaiters(ctx, log, ws, name)
	if err := v.deps.Merge.Relabel(ctx, ws); err != nil {
		log.Debug(opSetName, "the merge queue could not relabel the renamed workspace; its owner recorded why", dlog.Context{
			"name": name, "cause": err.Error()})
	}
}

// relabelAwaiters tells the footer of every workspace awaiting WS the
// awaited workspace's new NAME, as SetCorrespondence told it the old one.
func (v *verbs) relabelAwaiters(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, name string) {
	records, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error(opSetName, "could not list the workspaces awaiting the renamed one; their footers keep its old name", dlog.Context{
			"name": name, "cause": err.Error()})
		return
	}
	for _, record := range records {
		if awaitsRenamed(record, ws) {
			v.deps.Footer.SetAwaiting(record.ID, name)
		}
	}
}

// awaitsRenamed reports whether RECORD awaits WS.
func awaitsRenamed(record wsm.Workspace, ws ids.WorkspaceID) bool {
	return record.Awaiting != nil && *record.Awaiting == ws
}
