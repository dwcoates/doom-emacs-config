package workspace

import (
	"context"
	"errors"
	"fmt"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/bounce"
	"agent-repld/internal/bringup"
	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
	"agent-repld/internal/replyquote"
	"agent-repld/internal/rollout"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// opCreateFailedReport is the operation a failed create's report to its
// repository's main-worktree workspace is recorded under.
const opCreateFailedReport = "daemon.workspace.create_failed_report"

// BriefCreateFailed is the request a failed create sends its repository's
// main-worktree workspace (prompts/workspace-create-failed.md).
const BriefCreateFailed = "workspace-create-failed"

// Create is the creation verb every way of making a workspace reaches: the
// CreateWorkspace rpc (synchronous and option-B), the command-file ingress, the
// support workspace and the merge queue's own workspace.
//
// A CREATE THAT FAILS IS ANALYZED, NOT ONLY ANSWERED. The caller still gets the
// failure exactly as before (Emacs draws it in the minibuffer); beside that,
// the repository's MAIN-WORKTREE workspace is opened when it is closed and
// asked to find out why (reportCreateFailure). The report hangs off the verb
// rather than any one caller, so no way of creating a workspace can fail
// without it. The report is made per ATTEMPT (attemptCreate): a create held
// for the network has not failed and is not reported, and a held create that
// later fails for any other cause is.
//
// A CREATE HOLDS A TICKET FOR ITS WHOLE RUN (creategate), so a stand-down
// waits for it rather than cutting it off part-built. A create the CreateWorkspace
// rpc admitted already holds one; one the daemon starts itself (a merge
// workspace, a support workspace) waits here while a stand-down has the gate
// closed, and ends with the daemon, before anything was named or built, if the
// stand-down goes ahead.
func (v *verbs) Create(ctx context.Context, spec CreateSpec) (wsm.Workspace, error) {
	return v.RunCreate(ctx, v.EnterCreate(spec))
}

// reportCreateFailure hands a failed create to its repository's main-worktree
// workspace, on its own goroutine: opening that workspace may start a session,
// and neither the create's caller (a synchronous rpc, the command-file
// ingress) nor a client's deadline may wait on or cancel it.
//
// Three failures are not reported. A MERGE WORKSPACE's failure is the merge
// queue's, which hands it back to the requester itself. A HANDOVER is not a
// failure at all (createHandedOver): the create is the successor daemon's to
// make. A refusal of the CALLER'S OWN INPUT (callerInputRefusal) is the
// caller's to correct, and the caller is answered with it: there is nothing
// for the main worktree to investigate.
func (v *verbs) reportCreateFailure(ctx context.Context, spec CreateSpec, cause error) {
	log := v.deps.Log.Global().With(dlog.Context{"repo_dir": spec.RepoDir, "create_failure": cause.Error()})
	if spec.MergeOf != nil {
		log.Debug(opCreateFailedReport, "a merge workspace's failed create is the merge queue's to hand back; it is not reported to the main worktree", nil)
		return
	}
	if createHandedOver(cause) {
		log.Debug(opCreateFailedReport, "the create was handed over or this daemon is exiting; it is not reported as a failure", nil)
		return
	}
	if arm, ok := callerInputRefusal(cause); ok {
		log.Debug(opCreateFailedReport, "the create was refused for the caller's own input; it is not reported to the main worktree", dlog.Context{"arm": arm})
		return
	}
	detached := context.WithoutCancel(ctx)
	v.createFailureReports.Add(1)
	inflight.Go(func() {
		defer v.createFailureReports.Done()
		v.deliverCreateFailure(detached, log, spec, cause)
	})
}

// createHandedOver answers whether a create's error is a HANDOVER rather than a
// failure: the workspace or the bring-up belongs to another daemon now, or
// this one is exiting. Every case is the rollout's ordering, which the caller
// is answered with and acts on; none is something to analyze.
func createHandedOver(err error) bool {
	var refusal *Refusal
	if errors.As(err, &refusal) && (refusal.Arm == ArmTransferringAway || refusal.Arm == ArmNotYetAdopted) {
		return true
	}
	return errors.Is(err, bringup.ErrNotServed) ||
		errors.Is(err, shimclient.ErrStandingDown) ||
		errors.Is(err, rollout.ErrNotYetAdopted) ||
		errors.Is(err, bounce.ErrMovedAway) ||
		errors.Is(err, context.Canceled)
}

// createRefusalArms classifies EVERY arm of agentrepl.v1.CreateWorkspaceError
// by whether the CALLER'S OWN INPUT causes it (true): a name, a workspace, a
// repository, a mode or a merge request the caller supplied and is answered
// about, to correct and ask again. Nothing there is a fault of the daemon or
// the repository, so none is reported to the main worktree. An arm marked
// false is a fault of the daemon, the repository or the machine, which the
// main worktree investigates, or a handover (createHandedOver), which is no
// failure at all.
//
// THE MAP IS EXHAUSTIVE (TestCreateRefusalArmsClassifyEveryArm checks it
// against the contract), so a new arm cannot reach the main worktree as a
// failed create merely because nobody remembered to exclude it. Arms only the
// async-merge path or the transport raise are classified too; they are named
// by their contract spelling where this package declares no constant.
var createRefusalArms = map[string]bool{
	// The caller's input.
	ArmUngatedWithoutConsent:                true,
	ArmNoSlug:                               true,
	ArmInsideTemporaryDirectory:             true,
	ArmUnknownForkSource:                    true,
	ArmForkSourceHasNoConversation:          true,
	ArmForkSourceHasNoFinishedTurn:          true,
	ArmNameReserved:                         true,
	ArmNameHeld:                             true,
	ArmInvalidName:                          true,
	ArmModelNotInCatalog:                    true,
	ArmModelCatalogUnknown:                  true,
	"unknown_context_workspace":             true,
	"parent_not_open":                       true,
	ArmUnknownOwner:                         true,
	ArmUnknownLandingTarget:                 true,
	ArmUnknownRequester:                     true,
	"async_merge_unknown_branch":            true,
	"async_merge_unknown_subagent":          true,
	"async_merge_unknown_maker":             true,
	"async_merge_context_not_maker":         true,
	"async_merge_name_taken":                true,
	"placement_behind_not_in_line":          true,
	"async_merge_unknown_close_after":       true,
	"op_id_with_async_merge":                true,
	"version_increment_required":            true,
	"version_increment_unexpected":          true,
	"async_merge_requester_queued":          true,
	"async_merge_requester_merging":         true,
	"async_merge_requester_session_deleted": true,
	"requester_bypassing_queue":             true,
	ArmPlacementOnChild:                     true,
	ArmPlacementOnMergeWorkspace:            true,
	ArmPlacementRequiresWorkspace:           true,
	ArmUnknownPlacementSubsection:           true,
	ArmBlankPlacementSubsectionName:         true,
	ArmGatherUnknownWorkspace:               true,
	ArmGatherNotGatherable:                  true,
	// A fault of the daemon, the repository or the machine.
	ArmBriefMissing:                     false,
	ArmUnknownRepository:                false,
	"worktree_creation_failed":          false,
	ArmSpawnFailed:                      false,
	ArmOneShotPolicyMissing:             false,
	ArmNamingFailed:                     false,
	ArmOriginFetchFailed:                false,
	"async_merge_no_deploy_config":      false,
	"async_merge_deploy_config_invalid": false,
	// A branch nothing the daemon serves owns is the repository's state: most
	// often a landed branch the landed-branch sweep kept, whose record says
	// why, which the main worktree investigates.
	ArmNameBranchExists:       false,
	ArmLifecycleConfigInvalid: false,
	ArmCreationHookFailed:     false,
	// A handover.
	ArmTransferringAway: false,
	ArmStandingDown:     false,
}

// callerInputRefusal answers the arm of a create's error when it is a refusal
// of the caller's own input (createRefusalArms). A family rule's refusal
// (wsm.ParentNotOpenError) is the create's `parent_not_open` arm.
func callerInputRefusal(err error) (string, bool) {
	var refusal *Refusal
	if errors.As(err, &refusal) && createRefusalArms[refusal.Arm] {
		return refusal.Arm, true
	}
	var parent *wsm.ParentNotOpenError
	if errors.As(err, &parent) && createRefusalArms["parent_not_open"] {
		return "parent_not_open", true
	}
	return "", false
}

// deliverCreateFailure opens the main-worktree workspace of the repository the
// failed create named and submits the analysis request to it. Every way it can
// fall short is recorded here, the report's one ownership point; the create's
// own failure was already recorded by the verb.
//
// A create whose directory is no repository, or whose repository registers no
// main-worktree workspace, has nowhere to be reported: that is recorded at
// INFO, because the create's own refusal already says what was wrong with the
// directory.
func (v *verbs) deliverCreateFailure(ctx context.Context, log dlog.Logger, spec CreateSpec, cause error) {
	main, err := v.deps.Git.MainWorktree(ctx, spec.RepoDir)
	if err != nil {
		log.Info(opCreateFailedReport, "the failed create names no git repository; it is reported nowhere but this log", dlog.Context{"lookup": err.Error()})
		return
	}
	target, err := v.deps.DB.WorkspaceByDir(ctx, main)
	if errors.Is(err, wsm.ErrNotFound) {
		log.Info(opCreateFailedReport, "the failed create's repository has no registered main-worktree workspace; it is reported nowhere but this log", dlog.Context{"main_worktree": main})
		return
	}
	if err != nil {
		log.Error(opCreateFailedReport, "could not read the main-worktree workspace to report a failed create to", dlog.Context{"main_worktree": main, "lookup": err.Error()})
		return
	}
	log = log.With(dlog.Context{"report_workspace": string(target.ID), "main_worktree": main})

	request, err := v.composeBrief(ctx, log, opCreateFailedReport, BriefCreateFailed, map[string]string{
		"repository":     main,
		"request":        describeCreateRequest(spec),
		"cause":          replyquote.Fenced(cause.Error()),
		"agent_repl_dir": v.deps.CheckoutRoot,
	})
	if err != nil {
		return
	}
	// OPEN IS IDEMPOTENT: a live main worktree is reconciled and left as it
	// is; a closed one is reopened, which is what draws its tab.
	if err := v.Open(ctx, target.ID, nil); err != nil {
		log.Error(opCreateFailedReport, "could not open the main-worktree workspace to report a failed create to", dlog.Context{"open": err.Error()})
		return
	}
	// DEFERRED: the report waits behind a turn the main worktree is running
	// and wakes it when idle; it never interrupts work.
	submission := deferredBrief(target.ID, request, conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATE_FAILED)
	if _, err := v.queueBrief(ctx, log, opCreateFailedReport, "the main-worktree workspace's", "the failed create's report", submission); err != nil {
		return
	}
	log.Info(opCreateFailedReport, "asked the repository's main-worktree workspace why a create failed", dlog.Context{"turn": string(submission.Turn)})
}

// describeCreateRequest words what a failed create asked for, one fact per
// line, so the agent analyzing it can tell the request apart from its cause.
// A fact the create did not state is left out rather than drawn empty.
func describeCreateRequest(spec CreateSpec) string {
	var lines []string
	add := func(label, value string) {
		if value != "" {
			lines = append(lines, fmt.Sprintf("- %s: `%s`", label, value))
		}
	}
	add("name", spec.Name)
	if spec.OneShot {
		lines = append(lines, "- one-shot: yes")
	}
	add("base ref", spec.BaseRef)
	if spec.Parent != nil {
		add("parent workspace id", string(*spec.Parent))
	}
	if spec.ForkFrom != nil {
		add("forked from workspace id", string(*spec.ForkFrom))
	}
	add("model", spec.Model)
	add("permission mode", spec.PermissionMode)
	if prompt := strings.TrimSpace(spec.InitialPrompt); prompt != "" {
		lines = append(lines, "- initial prompt:", "", replyquote.Fenced(prompt))
	} else {
		lines = append(lines, "- initial prompt: none")
	}
	return strings.Join(lines, "\n")
}
