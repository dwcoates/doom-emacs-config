package workspace

import (
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
)

// opRefusal is the operation every unlanded-arm refusal is logged under, so the
// ledger in daemon/ERROR-ARMS.md can be reconciled against the log.
const opRefusal = "daemon.refusal.unlanded_arm"

// The arm names this package refuses under. They exist as constants because
// each one is a row in daemon/ERROR-ARMS.md and the two must not drift.
const (
	// ArmWorkspaceRefMismatch is an echoed WorkspaceRef whose dir disagrees
	// with the registry.
	ArmWorkspaceRefMismatch = "workspace_ref_mismatch"
	// ArmUnknownWorkspace is a ref naming an id the registry does not hold.
	ArmUnknownWorkspace = "unknown_workspace"
	// ArmTransferringAway is a workspace this daemon has handed to a successor.
	ArmTransferringAway = "transferring_away"
	// ArmNotYetAdopted is a workspace a joining daemon has not adopted yet.
	ArmNotYetAdopted = "not_yet_adopted"
	// ArmNotAWorktree is a registration whose directory is not a git worktree.
	ArmNotAWorktree = "not_a_worktree"
	// ArmUngatedWithoutConsent is a model/mode combination that disables the
	// permission gate with no consent recorded.
	ArmUngatedWithoutConsent = "ungated_without_consent"
	// ArmBaseRefUnresolved is a creation whose base ref does not resolve in
	// the repository it is cut from.
	ArmBaseRefUnresolved = "base_ref_unresolved"
	// ArmNoSlug is a creation whose name and initial prompt yield no slug.
	ArmNoSlug = "no_slug"
	// ArmFinishRequired is a one-shot creation with no finish action.
	ArmFinishRequired = "finish_required"
	// ArmFinishNotOneShot is a finish action on a standard creation.
	ArmFinishNotOneShot = "finish_not_one_shot"
	// ArmForkParentHasNoConversation is a fork whose parent never had one.
	ArmForkParentHasNoConversation = "fork_parent_has_no_conversation"
	// ArmSessionDeleted is a bring-up of a session whose record is terminal by
	// deletion: a deleted session refuses resurrection.
	ArmSessionDeleted = "session_deleted"
	// ArmTranscriptMissing is a resume whose vendor transcript file is gone —
	// refused BEFORE any process spawns.
	ArmTranscriptMissing = "transcript_missing"
	// ArmBlankTitle is a task verb with a blank title.
	ArmBlankTitle = "blank_title"
	// ArmNoStandingOffer is allow_standing on an ask that offered no standing
	// grant.
	ArmNoStandingOffer = "no_standing_offer"
	// ArmUnservedAnswer is an answer whose question text, chosen label or
	// permission id was never served.
	ArmUnservedAnswer = "unserved_answer"
	// ArmAskNotStanding is an answer addressed to a card that is not standing:
	// an ask the answer never names, or one no batch is open under. It is a
	// DISTINCT arm from ArmUnservedAnswer, which is a value the standing batch
	// never served (AnswerQuestionError spells the two apart).
	ArmAskNotStanding = "ask_not_standing"
	// ArmMultiPickOnSingleSelect is more than one choice on a single-select
	// question.
	ArmMultiPickOnSingleSelect = "multi_pick_on_single_select"
	// ArmNoColdGate is a cold-gate answer with no gate standing.
	ArmNoColdGate = "no_cold_gate"
	// ArmUnservedRemediation is a cold-gate remediation naming a model or
	// scope outside the served menu.
	ArmUnservedRemediation = "unserved_remediation"
	// ArmNoSession is a verb needing a live session on a workspace with none.
	ArmNoSession = "no_session"
	// ArmModeNotServed is a permission mode outside what the topbar's picker
	// served.
	ArmModeNotServed = "mode_not_served"
	// ArmNotInCatalog is a model outside what the topbar's selector served.
	ArmNotInCatalog = "not_in_catalog"
	// ArmPathEscapesWorkspace is an OpenInEditor path outside the workspace.
	ArmPathEscapesWorkspace = "path_escapes_workspace"
	// ArmBlankCommand is a RequestCommandSupport with no command named.
	ArmBlankCommand = "blank_command"
	// ArmBriefMissing is a composed brief the prompts directory does not hold.
	ArmBriefMissing = "brief_missing"
)

// Refusal is a state the daemon must refuse for which the contract has no
// typed error arm yet. Its message is EXACTLY the form ERROR-ARMS.md
// prescribes, so the transport can answer it verbatim.
//
// Rpc is filled in by whichever rpc's body raised it; a refusal raised by a
// shared helper leaves it empty and renders "<Rpc>", which the handler
// replaces with WithRpc before answering.
type Refusal struct {
	// Rpc is the rpc name the arm belongs to, empty when the raiser is shared.
	Rpc string
	// Arm is the intended arm's name.
	Arm string
	// Reason is the human-readable cause.
	Reason string
	// NotFound marks an unknown-id refusal, which answers CodeNotFound rather
	// than CodeFailedPrecondition.
	NotFound bool
}

// Error renders the exact message ERROR-ARMS.md prescribes.
func (r *Refusal) Error() string {
	rpc := r.Rpc
	if rpc == "" {
		rpc = "<Rpc>"
	}
	return fmt.Sprintf("intended arm: %sError.%s: %s", rpc, r.Arm, r.Reason)
}

// WithRpc names the rpc a shared refusal surfaced under. It returns a copy, so
// two handlers naming the same shared refusal cannot overwrite each other.
func (r *Refusal) WithRpc(rpc string) *Refusal {
	out := *r
	out.Rpc = rpc
	return &out
}

// AsRefusal reports whether err is one of these refusals, which is how the
// transport decides between an intended-arm answer and an ordinary failure.
func AsRefusal(err error) (*Refusal, bool) {
	var r *Refusal
	if errors.As(err, &r) {
		return r, true
	}
	return nil, false
}

// refuse records the intended arm at WARNING and returns it. Every refusal
// site in this package goes through here, so the log and the ledger agree.
func refuse(log dlog.Logger, rpc, arm, reason string, notFound bool) *Refusal {
	r := &Refusal{Rpc: rpc, Arm: arm, Reason: reason, NotFound: notFound}
	log.Warn(opRefusal, r.Error(), dlog.Context{
		"rpc":       rpc,
		"arm":       arm,
		"reason":    reason,
		"not_found": notFound,
	})
	return r
}
