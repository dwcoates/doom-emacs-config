package merge

import (
	"errors"
	"fmt"

	"agent-repld/internal/ids"
)

// This file holds the merge orchestrator's PRE-STATE refusals.
//
// Enqueue refuses before it records anything: an unmergeable workspace leaves
// no enqueuing→failed trail, because there is no state to stamp yet. Each
// refusal is a typed value carrying the ARM NAME the contract owes it, and the
// server maps that name onto the arm of whichever rpc it is answering — the
// same refusal is `MergeWorkspaceError.already_queued` under MergeWorkspace and
// `UpdateMergeQueueError.no_such_queued_merge` under UpdateMergeQueue, so the
// per-rpc choice belongs to the handler and the CAUSE belongs here. Every arm
// below landed in landing 4; none of them owes an ERROR-ARMS.md row.

// The arm names, spelled exactly as the landed `<Rpc>Error` oneof arms spell
// them.
const (
	// ArmNoLayoutFacts refuses a workspace whose creation job recorded no merge
	// geometry. Geometry is recorded at creation and NEVER inferred later, so
	// its absence means this workspace can never be merged.
	ArmNoLayoutFacts = "no_layout_facts"
	// ArmSessionDeleted refuses a workspace whose session is deleted. A deleted
	// session refuses resurrection, so the configured prompts could never run.
	ArmSessionDeleted = "session_deleted"
	// ArmAlreadyQueued refuses a second enqueue of a merge that is already
	// waiting: a merge is queued once.
	ArmAlreadyQueued = "already_queued"
	// ArmAlreadyMerging refuses an enqueue for the workspace whose merge is
	// running right now.
	ArmAlreadyMerging = "already_merging"
	// ArmNoSuchQueuedMerge refuses an evict or a queue-offer answer for a workspace
	// with nothing on the queue (UpdateMergeQueueError.no_such_queued_merge).
	ArmNoSuchQueuedMerge = "no_such_queued_merge"
	// ArmNoOfferStanding refuses an answer to the merge-queue question when
	// none stands (AnswerMergeQueueOfferError.no_offer_standing).
	ArmNoOfferStanding = "no_offer_standing"
	// ArmAlreadyPaused refuses a pause of a queue that is already paused
	// (UpdateMergeQueueError.already_paused).
	ArmAlreadyPaused = "already_paused"
	// ArmUnknownRepository refuses a pause or a resume whose repository ref
	// matches no registered repository
	// (UpdateMergeQueueError.unknown_repository, landed at landing 6).
	ArmUnknownRepository = "unknown_repository"
	// ArmNotPaused refuses an unpause of a queue that is not paused
	// (UpdateMergeQueueError.not_paused).
	ArmNotPaused = "not_paused"
	// ArmDeploysAlreadyHeld refuses a hold of a repository whose deploy stage
	// is already held (UpdateDeployHoldError.already_held).
	ArmDeploysAlreadyHeld = "already_held"
	// ArmDeploysNotHeld refuses a release of a repository that is not held
	// (UpdateDeployHoldError.not_held).
	ArmDeploysNotHeld = "not_held"
	// ArmRacingAlreadyDisabled refuses a disable of racing that is already
	// disabled (UpdateMergeQueueError.racing_already_disabled).
	ArmRacingAlreadyDisabled = "racing_already_disabled"
	// ArmRacingNotDisabled refuses an enable of racing that is not disabled
	// (UpdateMergeQueueError.racing_not_disabled).
	ArmRacingNotDisabled = "racing_not_disabled"
	// ArmNotSubscribed refuses an unsubscribe naming a workspace and scope
	// with no merge queue subscription (UpdateMergeQueueError.not_subscribed).
	ArmNotSubscribed = "not_subscribed"
	// ArmSubscriberClosed refuses a subscription of a closed workspace
	// (UpdateMergeQueueError.subscriber_closed).
	ArmSubscriberClosed = "subscriber_closed"
	// ArmUnknownCloseAfter refuses an async request whose close-after
	// workspace is not open, is in another repository, or is the requester
	// itself (CreateWorkspaceError.async_merge_unknown_close_after).
	ArmUnknownCloseAfter = "async_merge_unknown_close_after"
	// ArmBranchOwned refuses an async request whose branch a registered
	// workspace owns: landing it is that workspace's own act
	// (CreateWorkspaceError.async_merge_branch_owned). Its `branch` and
	// `owner` fields name the branch and the workspace.
	ArmBranchOwned = "async_merge_branch_owned"
	// ArmUnknownMaker refuses an async request whose maker workspace is not
	// registered in the requester's repository
	// (CreateWorkspaceError.async_merge_unknown_maker).
	ArmUnknownMaker = "async_merge_unknown_maker"
	// ArmUnknownBranch refuses an async request whose branch does not exist
	// (CreateWorkspaceError.async_merge_unknown_branch).
	ArmUnknownBranch = "async_merge_unknown_branch"
	// ArmNameTaken refuses an async request whose merge workspace name is
	// already a workspace, a branch, an occupied worktree directory, or another
	// pending async merge's reservation (CreateWorkspaceError.async_merge_name_taken). Its `branch`
	// field names the branch the name resolves to.
	ArmNameTaken = "async_merge_name_taken"
	// ArmPlacementBehindNotInLine refuses a merge asked to go behind a merge
	// that does not stand in its queue.
	ArmPlacementBehindNotInLine = "placement_behind_not_in_line"
	// ArmNoBranchCheckedOut refuses a merge of a workspace's own branch whose
	// worktree has a detached HEAD (MergeWorkspaceError.no_branch_checked_out).
	// Its `dir` field names the worktree.
	ArmNoBranchCheckedOut = "no_branch_checked_out"
	// ArmOwnerLands refuses a workspace's own ask to land a child whose
	// context was not forked from its owner: only the owner, or the user,
	// lands it (MergeWorkspaceError.owner_lands). Its `owner` field names the
	// owner.
	ArmOwnerLands = "owner_lands"
	// ArmRequesterMayNotLand refuses an ask from a workspace that is neither
	// the workspace being landed nor its owner
	// (MergeWorkspaceError.requester_may_not_land).
	ArmRequesterMayNotLand = "requester_may_not_land"
	// ArmNotMergeTab refuses a merge-tab interrupt whose row is no live agentic
	// merge tab (InterruptError.not_merge_tab).
	ArmNotMergeTab = "not_merge_tab"
	// ArmUnknownMergeTestLog refuses a test log token that names no log this
	// daemon holds for the workspace (OpenInEditorError.unknown_merge_test_log).
	ArmUnknownMergeTestLog = "unknown_merge_test_log"
)

// RefusalError is one pre-state refusal: which arm the contract owes it, and
// the reason the server puts after the arm.
type RefusalError struct {
	// Arm is the intended <Rpc>Error arm name.
	Arm string
	// Workspace is the refused workspace.
	Workspace ids.WorkspaceID
	// Reason is the sentence explaining the refusal, kept as evidence.
	Reason string
	// Fields are the arm's own field values, keyed by their proto names: the
	// evidence an arm carries (name_taken's `branch`), set from what the
	// refusal stated. Nil for an arm that carries none.
	Fields map[string]any
}

// Error spells the refusal the way the transport answers it, so the arm travels
// with the message rather than being reattached by the handler.
func (e *RefusalError) Error() string {
	return fmt.Sprintf("merge: refused %s: %s (workspace %s)", e.Arm, e.Reason, e.Workspace)
}

// refuse builds a refusal for one workspace.
func refuse(arm string, ws ids.WorkspaceID, format string, args ...any) *RefusalError {
	return &RefusalError{Arm: arm, Workspace: ws, Reason: fmt.Sprintf(format, args...)}
}

// Refused answers the refusal behind an error, and false for any other error.
// It hands back the whole RefusalError rather than its arm alone: the server
// puts the REASON after the arm on the answered message, so an extractor that
// dropped it would leave every merge refusal with an empty sentence. The
// shape matches workspace.AsRefusal, the other component's extractor.
func Refused(err error) (*RefusalError, bool) {
	var r *RefusalError
	if errors.As(err, &r) {
		return r, true
	}
	return nil, false
}
