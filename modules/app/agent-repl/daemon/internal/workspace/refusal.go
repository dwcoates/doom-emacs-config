package workspace

import (
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
)

// opRefusal is the operation a verb's typed refusal is logged under. A refusal
// raised here is an ORDINARY ANSWER — the contract carries an arm for it and
// the client reads that arm — so it is recorded at INFO. Only a genuinely
// unlanded arm warns, and it does so once, at the transport, through
// server.UnlandedArm under "daemon.refusal.unlanded_arm"; that operation stays
// usable for reconciling daemon/ERROR-ARMS.md precisely because this one does
// not borrow it.
const opRefusal = "daemon.refusal.typed"

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
	// ArmConversationOwned is a StartSession the shim refused because ANOTHER
	// shim holds this workspace's conversation: it took the workspace kernel
	// lock first, and two vendor processes on one conversation is what that
	// lock exists to prevent. OpenWorkspaceError has no arm for it.
	ArmConversationOwned = "conversation_owned"
	// ArmAlreadyStarted is a StartSession the shim refused because it ALREADY
	// serves a session: one shim serves exactly one. The daemon never sends a
	// second StartSession to a shim it adopted, so reaching this arm means the
	// daemon dialed a live shim it did not recognize as one — and that is a
	// state a client must be told about by NAME rather than through an untyped
	// internal. OpenWorkspaceError has no arm for it.
	ArmAlreadyStarted = "already_started"
	// ArmStartSessionUnspecified is a StartSessionFailure whose `cause` oneof
	// is unset — illegal on the wire, surfaced rather than guessed at.
	ArmStartSessionUnspecified = "start_session_unspecified"
	// ArmVendorStartFailed is a StartSession the shim refused because the
	// VENDOR failed to start inside an already-running shim: the shim process
	// itself is up and serving, so this is neither `spawn_failed` nor
	// `shim_start_failed`, both of which name the SHIM PROCESS failing.
	// LANDING 9 landed OpenWorkspaceError.vendor_start_failed{detail}, so the
	// shim's own account is relayed on that TYPED arm.
	ArmVendorStartFailed = "vendor_start_failed"
	// ArmUnknownSession is a StartSession(resume) the shim refused because it
	// has no transcript for the named conversation. The transcript-aware
	// source classifier keeps a never-turned session off this path, so the arm
	// is a genuinely VANISHED transcript, named rather than described.
	ArmUnknownSession = "unknown_session"
	// ArmTranscriptMissing is a resume whose vendor transcript file is gone —
	// refused BEFORE any process spawns.
	ArmTranscriptMissing = "transcript_missing"
	// ArmUnknownTask is a task ref naming no task this daemon minted.
	ArmUnknownTask = "unknown_task"
	// ArmNoChange is a task update asking for what the task already holds.
	ArmNoChange = "no_change"
	// ArmBlankTitle is a task verb with a blank title.
	ArmBlankTitle = "blank_title"
	// ArmNoStandingOffer is allow_standing on an ask that offered no standing
	// grant.
	ArmNoStandingOffer = "no_standing_offer"
	// ArmUnservedAnswer is an answer whose question text, chosen label or
	// permission id was never served.
	ArmUnservedAnswer = "unserved_answer"
	// ArmNotDetachedWork is an Interrupt(detached) whose addressed row names no
	// detached item. InterruptError carries this arm by name
	// (endpoint_interrupt.proto: "The FeedId names no detached item") and
	// carries no `unserved_answer`, which is an ANSWER verb's arm — a value the
	// served ask never offered — and says nothing about a row.
	ArmNotDetachedWork = "not_detached_work"
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
	// ArmSpawnFailed is a bring-up whose shim process would not come up.
	ArmSpawnFailed = "spawn_failed"
	// ArmBriefMissing is a composed brief the prompts directory does not hold.
	ArmBriefMissing = "brief_missing"
	// ArmOneShotPolicyMissing is a one-shot create in a repository that states
	// no one-shot policy of its own. It is DISTINCT from ArmBriefMissing,
	// which is a brief absent from the source that WAS chosen; this arm is the
	// repository having no policy source at all.
	ArmOneShotPolicyMissing = "one_shot_policy_missing"
	// ArmInvalidUrl is an OpenExternal link that is not an absolute url.
	ArmInvalidUrl = "invalid_url"
	// ArmNoBrowserConfigured is an OpenExternal on a daemon that resolved no
	// external browser launcher at all.
	ArmNoBrowserConfigured = "no_browser_configured"
	// ArmLaunchFailed is an OpenExternal whose launcher would not run. It
	// carries the launcher's own account of the failure as `detail`.
	ArmLaunchFailed = "launch_failed"
	// ArmGitFailed is a NukeWorkspace whose destruction of the worktree and
	// branch failed in git. It carries git's OWN account as `detail`.
	ArmGitFailed = "git_failed"
	// ArmNotClosed is a ForgetWorkspace on a workspace that is still open.
	// Forget is a registry act with no way to tear editor state down, and the
	// close verb owns the quiet requirement it would otherwise have to
	// duplicate or bypass.
	ArmNotClosed = "not_closed"
	// ArmHasChildren is a ForgetWorkspace on a workspace other workspaces were
	// spawned from. The schema nulls their parent_id rather than refusing, so
	// the forget would silently flatten a fork's lineage. It carries the
	// children's ids as `children`.
	ArmHasChildren = "has_children"
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
	// Fields are the intended arm message's OWN field values, keyed by proto
	// field name. An arm that carries evidence — CreateWorkspaceBaseRefUnresolved's
	// `ref`, for one — gets that evidence rather than only the sentence: the
	// transport copies this map onto the arm it sets, so a field the refusal
	// states is a field the client reads.
	Fields map[string]any
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
// two handlers naming the same shared refusal cannot overwrite each other. The
// arm's field values are copied too, so the rename never drops the evidence.
func (r *Refusal) WithRpc(rpc string) *Refusal {
	out := *r
	out.Rpc = rpc
	out.Fields = cloneFields(r.Fields)
	return &out
}

// cloneFields copies an arm's field values, so no two refusals share a map.
func cloneFields(in map[string]any) map[string]any {
	if in == nil {
		return nil
	}
	out := make(map[string]any, len(in))
	for k, v := range in {
		out[k] = v
	}
	return out
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

// refuse records the refusal at INFO and returns it. Every refusal site in this
// package goes through here, so every verb's refusal is recorded the same way.
func refuse(log dlog.Logger, rpc, arm, reason string, notFound bool) *Refusal {
	return refuseWith(log, rpc, arm, reason, notFound, nil)
}

// refuseWith is refuse for an arm that carries its OWN fields as evidence. A
// sentence is not a field: an arm spelling `ref` wants the ref, and a client
// reading the arm rather than the prose would otherwise get an empty string.
func refuseWith(log dlog.Logger, rpc, arm, reason string, notFound bool, fields map[string]any) *Refusal {
	r := &Refusal{Rpc: rpc, Arm: arm, Reason: reason, NotFound: notFound, Fields: cloneFields(fields)}
	ctx := dlog.Context{
		"rpc":       rpc,
		"arm":       arm,
		"reason":    reason,
		"not_found": notFound,
	}
	for name, value := range r.Fields {
		ctx["arm_"+name] = value
	}
	log.Info(opRefusal, r.Error(), ctx)
	return r
}
