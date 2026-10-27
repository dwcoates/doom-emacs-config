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
// without it.
//
// A CREATE HOLDS A TICKET FOR ITS WHOLE RUN (creategate), so a stand-down
// waits for it rather than cutting it off part-built. A create the CreateWorkspace
// rpc admitted already holds one; one the daemon starts itself (a merge
// workspace, a support workspace) waits here while a stand-down has the gate
// closed, and ends with the daemon, before anything was named or built, if the
// stand-down goes ahead.
func (v *verbs) Create(ctx context.Context, spec CreateSpec) (wsm.Workspace, error) {
	ctx, release, err := v.deps.Creates.Hold(ctx)
	if err != nil {
		v.deps.Log.Global().Info(opCreate, "the daemon stood down before the create could start; nothing was named or built", dlog.Context{
			"repo_dir": spec.RepoDir, "cause": err.Error(),
		})
		return wsm.Workspace{}, err
	}
	defer release()
	created, err := v.create(ctx, spec)
	if err != nil {
		v.reportCreateFailure(ctx, spec, err)
	}
	return created, err
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
	go func() {
		defer v.createFailureReports.Done()
		v.deliverCreateFailure(detached, log, spec, cause)
	}()
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

// callerInputArms are the create refusals the caller's own input causes:
// a supplied name that breaks the naming rule, a supplied name a pending
// merge or a create in flight already holds, and a promptless one-shot. Each
// is answered to the caller, who corrects the input; none is a fault of the
// daemon or the repository.
var callerInputArms = map[string]bool{
	ArmInvalidName:  true,
	ArmNameReserved: true,
	ArmNoSlug:       true,
}

// callerInputRefusal answers the arm of a create's error when it is a refusal
// of the caller's own input (callerInputArms).
func callerInputRefusal(err error) (string, bool) {
	var refusal *Refusal
	if errors.As(err, &refusal) && callerInputArms[refusal.Arm] {
		return refusal.Arm, true
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
