package workspace

import (
	"context"
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// BriefMergeRevert is the brief a reopened merged workspace is given when the
// user asked to revert its merge (agentrepl.v1.OpenWorkspaceRequest
// .revert_merge).
const BriefMergeRevert = "workspace-merge-revert"

// opRevertMerge is the operation every record of a revert-merge open carries.
const opRevertMerge = "daemon.workspace.revert_merge"

// ArmNotMerged is a revert asked of a workspace whose merge has not landed
// (OpenWorkspaceError.not_merged).
const ArmNotMerged = "not_merged"

// unrecordedParent is how the brief names a parent branch the registry did
// not record: the merge still landed somewhere, and the agent finds where.
const unrecordedParent = "the branch it was merged into (agent-repl did not record which; find it from the merge)"

// OpenToRevertMerge reopens a MERGED workspace exactly as Open does, then
// queues the merge-revert brief into its session, so its agent reverts the
// merge and the user follows the revert from the workspace.
//
// THE LANDED COMMIT IS READ BEFORE THE OPEN, because the open advances the
// branch to its parent's tip (advanceMerged) and the commit the branch held
// when it landed is then no longer its tip. The brief is composed before the
// open too, so a missing brief refuses with nothing opened.
//
// THE BRIEF WAITS OUT A COLD GATE (promptqueue.waitsOutColdGate): the open's
// bring-up is exactly where a large conversation parks at its gate, and the
// brief is held there rather than refused.
func (v *verbs) OpenToRevertMerge(ctx context.Context, ws ids.WorkspaceID, progress OpenProgress) error {
	const rpc = "OpenWorkspace"
	record, log, err := v.owned(ctx, rpc, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"branch": record.Branch, "parent_branch": record.ParentBranch})
	if record.MergedAt == nil {
		return refuse(log, rpc, ArmNotMerged,
			fmt.Sprintf("workspace %q has no landed merge to revert", record.Name), false)
	}
	landed, err := v.landedCommit(ctx, log, record)
	if err != nil {
		return err
	}
	parent := unrecordedParent
	if record.ParentBranch != "" {
		parent = "`" + record.ParentBranch + "`"
	}
	brief, err := v.composeBrief(ctx, log, opRevertMerge, BriefMergeRevert, map[string]string{
		"branch":        record.Branch,
		"parent_branch": parent,
		"landed_commit": landed,
		"merged_at":     record.MergedAt.Local().Format(time.DateTime),
	})
	if err != nil {
		return refuse(log, rpc, ArmBriefMissing, err.Error(), false)
	}
	if err := v.Open(ctx, ws, progress); err != nil {
		return err
	}
	submission := deferredBrief(ws, brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT)
	disposition, err := v.queueBrief(ctx, log, opRevertMerge, "the reopened workspace's", "the merge-revert brief", submission)
	if err != nil {
		return fmt.Errorf("revert the merge of %q: %w", ws, err)
	}
	log.Info(opRevertMerge, "reopened the merged workspace and queued its merge-revert brief", dlog.Context{
		"turn": string(submission.Turn), "landed_commit": landed,
		"delivered": disposition.Delivered, "parked": disposition.Parked()})
	return nil
}

// landedCommit reads the commit a merged workspace's branch holds, which is
// the commit its merge landed: nothing moves a merged branch until it is
// reopened. A branch that is gone leaves nothing to restore the worktree from
// and nothing to name in the brief, so it refuses as the open itself would.
func (v *verbs) landedCommit(ctx context.Context, log dlog.Logger, record wsm.Workspace) (string, error) {
	if record.Branch == "" {
		return "", refuseUnrestorable(log, "OpenWorkspace", record,
			fmt.Sprintf("workspace %q recorded no branch, so its landed merge cannot be found", record.Name))
	}
	repoDir, err := v.repoDirOf(ctx, record)
	if err != nil {
		log.Error(opRevertMerge, "could not resolve the repository of the merged workspace", dlog.Context{"cause": err.Error()})
		return "", fmt.Errorf("revert the merge of %q: %w", record.ID, err)
	}
	exists, err := v.deps.Git.BranchExists(ctx, repoDir, record.Branch)
	if err != nil {
		log.Error(opRevertMerge, "could not probe the merged workspace's branch", dlog.Context{"repository": repoDir, "cause": err.Error()})
		return "", fmt.Errorf("revert the merge of %q: probe branch %q: %w", record.ID, record.Branch, err)
	}
	if !exists {
		return "", refuseUnrestorable(log, "OpenWorkspace", record,
			fmt.Sprintf("the branch %s of workspace %q no longer exists, so its landed merge cannot be found", record.Branch, record.Name))
	}
	landed, err := v.deps.Git.ResolveRef(ctx, repoDir, "refs/heads/"+record.Branch)
	if err != nil {
		log.Error(opRevertMerge, "could not read the commit the merged workspace's branch landed", dlog.Context{"repository": repoDir, "cause": err.Error()})
		return "", fmt.Errorf("revert the merge of %q: resolve branch %q: %w", record.ID, record.Branch, err)
	}
	return landed, nil
}
