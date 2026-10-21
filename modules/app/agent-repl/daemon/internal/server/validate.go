package server

import (
	"agent-repld/internal/deployconfig"

	"fmt"
	"strings"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/prompthandler"
	"agent-repld/internal/promptorigin"
)

// THE VALIDATION INVARIANT (ARCHITECTURE.md "Cross-cutting conventions"): one
// base function per message, where that message's validation lives once; every
// message-typed field and every oneof arm gets its own dedicated function
// delegating to the child's base; an unset non-optional field and an unset
// oneof are ERRORS. A request that fails validation is refused AT ONCE with a
// Connect InvalidArgument naming the field — never partially served, never
// defaulted.
//
// PRESENCE, NEVER SENTINELS: "unset" is the only spelling of absent, and an
// `optional` field is the only field allowed to be absent.

// invalid is the one InvalidArgument spelling, so every field name reaches the
// caller the same way.
func invalid(field, why string) *connect.Error {
	return connect.NewError(connect.CodeInvalidArgument,
		fmt.Errorf("%s: %s", field, why))
}

// validateWorkspaceRef is workspace.v1.WorkspaceRef's base function. `id` is
// the sole identifier; `dir` is display, and is checked against the registry by
// the resolver rather than here.
func validateWorkspaceRef(field string, ref *workspacev1.WorkspaceRef) *connect.Error {
	if ref == nil {
		return invalid(field, "a workspace ref is required")
	}
	if ref.GetId() == "" {
		return invalid(field+".id", "a workspace id is required")
	}
	return nil
}

// validateRepositoryRef is workspace.v1.RepositoryRef's base function. A ref
// names an id, a dir, or both — but naming NEITHER identifies nothing.
func validateRepositoryRef(field string, ref *workspacev1.RepositoryRef) *connect.Error {
	if ref == nil {
		return invalid(field, "a repository ref is required")
	}
	if ref.GetId() == "" && ref.GetDir() == "" {
		return invalid(field, "a repository ref must name an id or a dir")
	}
	return nil
}

// validateSubsectionRef is SubsectionRef's base function.
func validateSubsectionRef(field string, ref *agentreplv1.SubsectionRef) *connect.Error {
	if ref == nil {
		return invalid(field, "a subsection ref is required")
	}
	if ref.GetId() == "" {
		return invalid(field+".id", "a subsection id is required")
	}
	return nil
}

// validateTaskRef is TaskRef's base function.
func validateTaskRef(field string, ref *agentreplv1.TaskRef) *connect.Error {
	if ref == nil {
		return invalid(field, "a task ref is required")
	}
	if ref.GetId() == "" {
		return invalid(field+".id", "a task id is required")
	}
	return nil
}

// validateFeedID is frontend.v1.FeedId's base function. The VALUE's decodability
// is a refusal (`feed_undecodable`), not a validation failure: a well-formed
// request may still address a feed this daemon cannot decode.
func validateFeedID(field string, id *frontendv1.FeedId) *connect.Error {
	if id == nil {
		return invalid(field, "a feed id is required")
	}
	if id.GetValue() == "" {
		return invalid(field+".value", "a feed id value is required")
	}
	return nil
}

// validateUserSaid is conversation.v1.UserSaid's base function.
func validateUserSaid(field string, said *conversationv1.UserSaid) *connect.Error {
	if said == nil {
		return invalid(field, "a prompt is required")
	}
	content := said.GetContent()
	if content == nil {
		return invalid(field+".content", "prompt content is required")
	}
	if len(content.GetBlocks()) == 0 {
		return invalid(field+".content.blocks", "a prompt carries at least one block")
	}
	for i, block := range content.GetBlocks() {
		if err := validateUserContentBlock(fmt.Sprintf("%s.content.blocks[%d]", field, i), block); err != nil {
			return err
		}
	}
	return nil
}

// validateUserContentBlock is UserContentBlock's base function: the oneof is
// required, because a block that says nothing is not a block.
func validateUserContentBlock(field string, block *conversationv1.UserContentBlock) *connect.Error {
	if block == nil {
		return invalid(field, "a content block is required")
	}
	if block.GetBlock() == nil {
		return invalid(field+".block", "a content block's arm is required")
	}
	return nil
}

// validateAgentModel is conversation.v1.AgentModel's base function.
func validateAgentModel(field string, model *conversationv1.AgentModel) *connect.Error {
	if model == nil {
		return invalid(field, "a model is required")
	}
	if model.GetName() == "" {
		return invalid(field+".name", "a model name is required")
	}
	return nil
}

// validateDrainReason is DrainReason's base function.
func validateDrainReason(field string, reason *agentreplv1.DrainReason) *connect.Error {
	if reason == nil {
		return invalid(field, "a drain reason is required")
	}
	if reason.GetKind() == nil {
		return invalid(field+".kind", "a drain reason's arm is required")
	}
	if operator := reason.GetOperator(); operator != nil {
		return validateDrainReasonOperator(field+".operator", operator)
	}
	return nil
}

// validateDrainReasonOperator is DrainReasonOperator's base function. The note
// is REQUIRED NON-BLANK by drain_reason.proto: an operator arm whose note says
// nothing is the operator arm saying nothing, which is what `maintenance`
// already spells.
func validateDrainReasonOperator(field string, operator *agentreplv1.DrainReasonOperator) *connect.Error {
	if strings.TrimSpace(operator.GetNote()) == "" {
		return invalid(field+".note", "an operator drain reason requires a non-blank note")
	}
	return nil
}

// validateSubmitPromptRequest is SubmitPromptRequest's base function.
func validateSubmitPromptRequest(req *agentreplv1.SubmitPromptRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if err := validateUserSaid("said", req.GetSaid()); err != nil {
		return err
	}
	if req.GetIdempotencyKey() == "" {
		return invalid("idempotency_key", "an idempotency key is required")
	}
	if req.GetOrigin() == conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
		return invalid("origin", "the prompt origin is required")
	}
	// THE SENDER IS NAMED EXACTLY WHEN AN AGENT SENT THE PROMPT.
	if err := promptorigin.CheckSender(req.GetOrigin(), req.SentBy != nil); err != nil {
		return invalid("sent_by", err.Error())
	}
	if req.SentBy != nil {
		if err := validateWorkspaceRef("sent_by", req.GetSentBy()); err != nil {
			return err
		}
	}
	if req.Feed != nil {
		if err := validateFeedID("feed", req.GetFeed()); err != nil {
			return err
		}
	}
	// AN ABSENT delivery is the ordinary one; a present one must name a
	// delivery the daemon honors. UNSPECIFIED is never sent, and a value this
	// build does not know is never read as the ordinary delivery.
	if _, err := prompthandler.DeliveryOf(req.Delivery); err != nil {
		return invalid("delivery", err.Error())
	}
	if req.SubmittedAtUnixMs != nil && req.GetSubmittedAtUnixMs() <= 0 {
		return invalid("submitted_at_unix_ms", "a submit instant must be positive Unix milliseconds")
	}
	return nil
}

// validateSelectFeedRowRequest is SelectFeedRowRequest's base function. A move
// is required, a step's direction is required, and a left-view report names
// its row: each is never sent unset, so a request missing one is
// InvalidArgument rather than a typed refusal, exactly as the proto states.
func validateSelectFeedRowRequest(req *agentreplv1.SelectFeedRowRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	switch move := req.GetMove().(type) {
	case *agentreplv1.SelectFeedRowRequest_TurnBoundary:
		return validateSelectFeedRowStep("move.turn_boundary", move.TurnBoundary)
	case *agentreplv1.SelectFeedRowRequest_Clear:
		return nil
	case *agentreplv1.SelectFeedRowRequest_LeftView:
		return validateFeedID("move.left_view.row", move.LeftView.GetRow())
	case *agentreplv1.SelectFeedRowRequest_Bubble:
		return validateFeedID("move.bubble.row", move.Bubble.GetRow())
	}
	return invalid("move", "a selection move is required")
}

// validateFoldMergeBubbleRequest requires the workspace, the row and the fold.
func validateFoldMergeBubbleRequest(req *agentreplv1.FoldMergeBubbleRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if err := validateFeedID("row", req.GetRow()); err != nil {
		return err
	}
	if req.GetFold() == nil {
		return invalid("fold", "a fold is required")
	}
	return nil
}

// validateSelectFeedRowStep requires a step's direction.
func validateSelectFeedRowStep(field string, step *agentreplv1.SelectFeedRowStep) *connect.Error {
	if step.GetDirection() == agentreplv1.SelectFeedRowDirection_SELECT_FEED_ROW_DIRECTION_UNSPECIFIED {
		return invalid(field+".direction", "a selection step's direction is required")
	}
	return nil
}

// validateAdjustFeedTextScaleRequest is AdjustFeedTextScaleRequest's base
// function. The scale is daemon-global, so unlike the per-workspace verbs there
// is no workspace ref to validate — only the direction, whose UNSPECIFIED value
// is never a legitimate nudge.
func validateAdjustFeedTextScaleRequest(req *agentreplv1.AdjustFeedTextScaleRequest) *connect.Error {
	if req.GetDirection() == agentreplv1.AdjustFeedTextScaleDirection_ADJUST_FEED_TEXT_SCALE_DIRECTION_UNSPECIFIED {
		return invalid("direction", "a feed-text-scale direction is required")
	}
	return nil
}

// validateRequestCommandSupportRequest is RequestCommandSupportRequest's base
// function.
func validateRequestCommandSupportRequest(req *agentreplv1.RequestCommandSupportRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetCommand() == "" {
		return invalid("command", "a command is required")
	}
	return nil
}

// validateBindWorkspaceSessionRequest is BindWorkspaceSessionRequest's base
// function. The vendor session id is an ECHO of a served value, so a blank one
// is an illegal shape rather than an unknown_transcript: a client may not
// invent one, and it certainly may not invent nothing.
func validateBindWorkspaceSessionRequest(req *agentreplv1.BindWorkspaceSessionRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetVendorSessionId() == "" {
		return invalid("vendor_session_id", "a vendor session id is required")
	}
	return nil
}

// validateOpenFeedRequest is OpenFeedRequest's base function.
func validateOpenFeedRequest(req *agentreplv1.OpenFeedRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.Feed != nil {
		return validateFeedID("feed", req.GetFeed())
	}
	return nil
}

// validateLoadFeedThroughRequest is LoadFeedThroughRequest's base function.
func validateLoadFeedThroughRequest(req *agentreplv1.LoadFeedThroughRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if err := validateFeedID("target", req.GetTarget()); err != nil {
		return err
	}
	if req.OldestHeld != nil {
		return validateFeedRowOrder("oldest_held", req.GetOldestHeld())
	}
	return nil
}

// validateWatchFeedRequest is WatchFeedRequest's base function.
func validateWatchFeedRequest(req *agentreplv1.WatchFeedRequest) *connect.Error {
	if req.GetWatch() == nil {
		return invalid("watch", "a watch token is required")
	}
	if req.GetWatch().GetValue() == "" {
		return invalid("watch.value", "a watch token value is required")
	}
	return nil
}

// validateGetFeedPageRequest is GetFeedPageRequest's base function.
func validateGetFeedPageRequest(req *agentreplv1.GetFeedPageRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.Feed != nil {
		if err := validateFeedID("feed", req.GetFeed()); err != nil {
			return err
		}
	}
	if req.GetPage() == nil {
		return invalid("page", "a page arm is required")
	}
	if older := req.GetOlderThan(); older != nil {
		return validateFeedRowOrder("older_than.oldest_held", older.GetOldestHeld())
	}
	return nil
}

// validateFeedRowOrder refuses an order a reader echoes back without its key.
func validateFeedRowOrder(field string, order *frontendv1.FeedRowOrder) *connect.Error {
	if order == nil {
		return invalid(field, "the oldest held row's order is required")
	}
	if order.GetKey() == "" {
		return invalid(field+".key", "the oldest held row's order key is required")
	}
	return nil
}

// validateInterruptRequest is InterruptRequest's base function.
func validateInterruptRequest(req *agentreplv1.InterruptRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetTarget() == nil {
		return invalid("target", "an interrupt target is required")
	}
	if detached := req.GetDetached(); detached != nil {
		return validateFeedID("detached", detached)
	}
	if tab := req.GetMergeTab(); tab != nil {
		return validateFeedID("merge_tab", tab)
	}
	return nil
}

// validateAnswerPermissionRequest is AnswerPermissionRequest's base function.
func validateAnswerPermissionRequest(req *agentreplv1.AnswerPermissionRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if err := validateFeedID("permission", req.GetPermission()); err != nil {
		return err
	}
	if req.GetAnswer() == nil {
		return invalid("answer", "a permission answer arm is required")
	}
	return nil
}

// validateAnswerQuestionRequest is AnswerQuestionRequest's base function.
func validateAnswerQuestionRequest(req *agentreplv1.AnswerQuestionRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if err := validateFeedID("question", req.GetQuestion()); err != nil {
		return err
	}
	if len(req.GetAnswers()) == 0 {
		return invalid("answers", "at least one answer is required")
	}
	for i, answer := range req.GetAnswers() {
		if err := validateAnswerQuestionAnswer(fmt.Sprintf("answers[%d]", i), answer); err != nil {
			return err
		}
	}
	return nil
}

// validateAnswerQuestionAnswer is AnswerQuestionAnswer's base function.
func validateAnswerQuestionAnswer(field string, answer *agentreplv1.AnswerQuestionAnswer) *connect.Error {
	if answer == nil {
		return invalid(field, "an answer is required")
	}
	if answer.GetQuestionText() == "" {
		return invalid(field+".question_text", "an answer names the question it answers")
	}
	return nil
}

// validateUpdateQuestionDraftRequest is UpdateQuestionDraftRequest's base
// function: every edit arm names its question, and a pick its option.
func validateUpdateQuestionDraftRequest(req *agentreplv1.UpdateQuestionDraftRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if err := validateFeedID("question", req.GetQuestion()); err != nil {
		return err
	}
	switch edit := req.GetEdit().(type) {
	case *agentreplv1.UpdateQuestionDraftRequest_Show:
		if edit.Show.GetQuestionText() == "" {
			return invalid("show.question_text", "a show names the question it shows")
		}
	case *agentreplv1.UpdateQuestionDraftRequest_Pick:
		if edit.Pick.GetQuestionText() == "" {
			return invalid("pick.question_text", "a pick names the question it answers")
		}
		if edit.Pick.GetLabel() == "" {
			return invalid("pick.label", "a pick names the option it clicks")
		}
	case *agentreplv1.UpdateQuestionDraftRequest_Confirm:
		if edit.Confirm.GetQuestionText() == "" {
			return invalid("confirm.question_text", "a confirm names the question it confirms")
		}
	default:
		return invalid("edit", "a draft edit arm is required")
	}
	return nil
}

// validateAnswerColdGateRequest is AnswerColdGateRequest's base function.
func validateAnswerColdGateRequest(req *agentreplv1.AnswerColdGateRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if err := validateFeedID("gate", req.GetGate()); err != nil {
		return err
	}
	if req.GetChoice() == nil {
		return invalid("choice", "a cold-gate choice arm is required")
	}
	if compact := req.GetCompact(); compact != nil {
		return validateAnswerColdGateCompact("compact", compact)
	}
	if compact := req.GetCompactAndResume(); compact != nil {
		return validateAnswerColdGateCompact("compact_and_resume", compact)
	}
	return nil
}

// validateAnswerColdGateCompact is the payload both compact arms share, at
// FIELD: a summarizer and a scope, neither defaulted.
func validateAnswerColdGateCompact(field string, compact *agentreplv1.AnswerColdGateCompact) *connect.Error {
	if err := validateAgentModel(field+".model", compact.GetModel()); err != nil {
		return err
	}
	if compact.GetScope() == conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED {
		return invalid(field+".scope", "a compaction scope is required")
	}
	return nil
}

// validateCreateWorkspaceRequest is CreateWorkspaceRequest's base function.
func validateCreateWorkspaceRequest(req *agentreplv1.CreateWorkspaceRequest) *connect.Error {
	if err := validateRepositoryRef("repository", req.GetRepository()); err != nil {
		return err
	}
	if req.GetForm() == nil {
		return invalid("form", "a creation form arm is required")
	}
	if standard := req.GetStandard(); standard != nil {
		if standard.InitialPrompt != nil {
			if err := validateUserSaid("standard.initial_prompt", standard.GetInitialPrompt()); err != nil {
				return err
			}
		}
		if err := validateWorkspaceTopology("standard.topology", standard.GetTopology()); err != nil {
			return err
		}
		if err := validateDeployHooks("standard.deploy_hooks", standard.GetDeployHooks()); err != nil {
			return err
		}
	}
	if oneShot := req.GetOneShot(); oneShot != nil {
		if err := validateUserSaid("one_shot.prompt", oneShot.GetPrompt()); err != nil {
			return err
		}
		if err := validateWorkspaceTopology("one_shot.topology", oneShot.GetTopology()); err != nil {
			return err
		}
		if about := oneShot.GetContext(); about != nil {
			if err := validateWorkspaceRef("one_shot.context.workspace", about.GetWorkspace()); err != nil {
				return err
			}
		}
	}
	if err := validateWorkspaceRequester("requester", req.GetRequester()); err != nil {
		return err
	}
	if err := validateWorkspaceContextOrigin("context_origin", req.GetContextOrigin()); err != nil {
		return err
	}
	if async := req.GetAsyncMerge(); async != nil {
		if err := validateCreateWorkspaceAsyncMerge("async_merge", async); err != nil {
			return err
		}
		if req.GetRequester().GetWorkspace() == nil {
			return invalid("requester", "an async merge's report and its queue place are its requesting workspace's, so the user cannot request one")
		}
		// The merge workspace forks its maker's session as it stands, under the
		// maker's model and priority, and is gated as its maker is.
		if req.Model != nil {
			return invalid("model", "an async merge's workspace runs its maker's model")
		}
		if req.Priority != nil {
			return invalid("priority", "an async merge's workspace is hidden and carries no priority")
		}
		if req.AllowUngated != nil {
			return invalid("allow_ungated", "an async merge's workspace is gated as its maker is")
		}
	}
	if req.Priority != nil {
		if err := validateWorkspacePriority("priority", req.GetPriority()); err != nil {
			return err
		}
	}
	return nil
}

// validateWorkspaceRequester is WorkspaceRequester's base function.
func validateWorkspaceRequester(field string, r *agentreplv1.WorkspaceRequester) *connect.Error {
	switch asker := r.GetAsker().(type) {
	case *agentreplv1.WorkspaceRequester_User:
		return nil
	case *agentreplv1.WorkspaceRequester_Workspace:
		return validateWorkspaceRef(field+".workspace.workspace", asker.Workspace.GetWorkspace())
	}
	return invalid(field, "a requester arm (user or workspace) is required")
}

// validateWorkspaceContextOrigin is WorkspaceContextOrigin's base function.
func validateWorkspaceContextOrigin(field string, c *agentreplv1.WorkspaceContextOrigin) *connect.Error {
	switch origin := c.GetOrigin().(type) {
	case *agentreplv1.WorkspaceContextOrigin_Fresh:
		return nil
	case *agentreplv1.WorkspaceContextOrigin_Fork:
		return validateWorkspaceRef(field+".fork.workspace", origin.Fork.GetWorkspace())
	}
	return invalid(field, "a context origin arm (fresh or fork) is required")
}

// validateWorkspaceTopology is WorkspaceTopology's base function.
func validateWorkspaceTopology(field string, t *agentreplv1.WorkspaceTopology) *connect.Error {
	switch shape := t.GetShape().(type) {
	case *agentreplv1.WorkspaceTopology_Primary:
		return nil
	case *agentreplv1.WorkspaceTopology_Child:
		if notice := shape.Child.OwnerNotice; notice != nil && strings.TrimSpace(notice.GetText()) == "" {
			return invalid(field+".child.owner_notice.text", "a notice to the owner says what is coming")
		}
		return validateWorkspaceRef(field+".child.owner", shape.Child.GetOwner())
	}
	return invalid(field, "a topology arm (primary or child) is required")
}

// validateMergeQueuePlacement is MergeQueuePlacement's base function.
func validateMergeQueuePlacement(field string, p *agentreplv1.MergeQueuePlacement) *connect.Error {
	if p.GetPlace() == nil {
		return invalid(field, "a placement arm (back or front) is required")
	}
	return nil
}

// validateMergeWorkspaceVersionIncrement is MergeWorkspaceVersionIncrement's
// base function; an unset increment is valid.
func validateMergeWorkspaceVersionIncrement(field string, v *agentreplv1.MergeWorkspaceVersionIncrement) *connect.Error {
	if v == nil {
		return nil
	}
	if v.GetIncrement() == nil {
		return invalid(field+".increment", "an increment arm (minor or patch) is required")
	}
	if err := versionIncrement(v).Validate(); err != nil {
		return invalid(field+".changelog_line", err.Error())
	}
	return nil
}

// validateCreateWorkspaceAsyncMerge is CreateWorkspaceAsyncMerge's base
// function.
func validateCreateWorkspaceAsyncMerge(field string, async *agentreplv1.CreateWorkspaceAsyncMerge) *connect.Error {
	if strings.TrimSpace(async.GetName()) == "" {
		return invalid(field+".name", "an async merge names its merge workspace")
	}
	if strings.TrimSpace(async.GetSource().GetBranch()) == "" {
		return invalid(field+".source.branch", "an async merge names the branch it lands")
	}
	switch maker := async.GetSource().GetMaker().GetMaker().(type) {
	case *agentreplv1.CreateWorkspaceAsyncMergeMaker_Subagent:
		if strings.TrimSpace(maker.Subagent.GetAgentId()) == "" {
			return invalid(field+".source.maker.subagent.agent_id", "a subagent maker names the subagent")
		}
	case *agentreplv1.CreateWorkspaceAsyncMergeMaker_Workspace:
		if err := validateWorkspaceRef(field+".source.maker.workspace.workspace", maker.Workspace.GetWorkspace()); err != nil {
			return err
		}
	default:
		return invalid(field+".source.maker", "a maker arm (subagent or workspace) is required")
	}
	if async.GetTarget().GetTarget() == nil {
		return invalid(field+".target", "a target arm (primary or maker) is required")
	}
	if async.Instruction != nil && strings.TrimSpace(async.GetInstruction()) == "" {
		return invalid(field+".instruction", "an instruction says how to prepare the branch")
	}
	if async.CloseAfter != nil {
		if err := validateWorkspaceRef(field+".close_after.workspace", async.GetCloseAfter().GetWorkspace()); err != nil {
			return err
		}
	}
	if err := validateMergeQueuePlacement(field+".placement", async.GetPlacement()); err != nil {
		return err
	}
	if err := validateDeployHooks(field+".deploy_hooks", async.GetDeployHooks()); err != nil {
		return err
	}
	return validateMergeWorkspaceVersionIncrement(field+".version_increment", async.GetVersionIncrement())
}

// validateDeployHooks holds a request's own deploy hooks to the rules a
// repository's config is held to (deployconfig.CheckHook). Unset is no hooks.
func validateDeployHooks(field string, hooks *agentreplv1.CreateWorkspaceDeployHooks) *connect.Error {
	for i, hook := range hooks.GetPreDeploy() {
		if err := deployconfig.CheckHook(fmt.Sprintf("%s.pre_deploy[%d]", field, i), hook); err != nil {
			return invalid(fmt.Sprintf("%s.pre_deploy[%d]", field, i), err.Error())
		}
	}
	for i, hook := range hooks.GetPostDeploy() {
		if err := deployconfig.CheckHook(fmt.Sprintf("%s.post_deploy[%d]", field, i), hook); err != nil {
			return invalid(fmt.Sprintf("%s.post_deploy[%d]", field, i), err.Error())
		}
	}
	return nil
}

// validateWorkspacePriority is WorkspacePriority's base function.
func validateWorkspacePriority(field string, p *agentreplv1.WorkspacePriority) *connect.Error {
	if p == nil {
		return invalid(field, "a priority is required")
	}
	if p.GetLevel() == nil {
		return invalid(field+".level", "a priority level arm is required")
	}
	return nil
}

// validateRegisterWorkspaceRequest is RegisterWorkspaceRequest's base function.
func validateRegisterWorkspaceRequest(req *agentreplv1.RegisterWorkspaceRequest) *connect.Error {
	if req.GetDir() == "" {
		return invalid("dir", "a workspace directory is required")
	}
	return nil
}

// validateRegisterRepositoryRequest is RegisterRepositoryRequest's base
// function. A blank path is a VALIDATION failure rather than an arm: the two
// arms are about what a real path turned out to be, and an empty string is not
// a path at all.
func validateRegisterRepositoryRequest(req *agentreplv1.RegisterRepositoryRequest) *connect.Error {
	if req.GetPath() == "" {
		return invalid("path", "a path inside the repository is required")
	}
	return nil
}

// validateCreateTaskRequest is CreateTaskRequest's base function. A blank title
// is a REFUSAL arm rather than a validation failure, so only the unset field is
// checked here.
func validateCreateTaskRequest(req *agentreplv1.CreateTaskRequest) *connect.Error {
	_ = req
	return nil
}

// validateUpdateTaskRequest is UpdateTaskRequest's base function.
func validateUpdateTaskRequest(req *agentreplv1.UpdateTaskRequest) *connect.Error {
	if err := validateTaskRef("task", req.GetTask()); err != nil {
		return err
	}
	if req.GetChange() == nil {
		return invalid("change", "a task change arm is required")
	}
	return nil
}

// validateAssignWorkspaceTaskRequest is AssignWorkspaceTaskRequest's base
// function. An unset `task` is the UNASSIGN spelling, which is legal.
func validateAssignWorkspaceTaskRequest(req *agentreplv1.AssignWorkspaceTaskRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.Task != nil {
		return validateTaskRef("task", req.GetTask())
	}
	return nil
}

// validateSetModelRequest is SetModelRequest's base function.
func validateSetModelRequest(req *agentreplv1.SetModelRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	return validateAgentModel("model", req.GetModel())
}

// validateSetEffortRequest is SetEffortRequest's base function. The level is
// an enum whose zero is unset, and a client never invents one.
func validateRetryVendorErrorRequest(req *agentreplv1.RetryVendorErrorRequest) *connect.Error {
	return validateWorkspaceRef("workspace", req.GetWorkspace())
}

func validateSetEffortRequest(req *agentreplv1.SetEffortRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetEffort() == conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
		return invalid("effort", "an effort level is required")
	}
	if _, known := conversationv1.AgentEffortLevel_name[int32(req.GetEffort())]; !known {
		return invalid("effort", "the effort level is not one this contract defines")
	}
	return nil
}

// validateSetPermissionModeRequest is SetPermissionModeRequest's base function.
func validateSetPermissionModeRequest(req *agentreplv1.SetPermissionModeRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetMode() == "" {
		return invalid("mode", "a permission mode is required")
	}
	return nil
}

// validateSelectAccountRequest is SelectAccountRequest's base function. The
// root is an ECHO TOKEN — an option's own config_dir — so a blank one names
// nothing the daemon could have served.
func validateSelectAccountRequest(req *agentreplv1.SelectAccountRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetConfigDir() == "" {
		return invalid("config_dir", "an account root is required")
	}
	return nil
}

// validateSetWorkspacePriorityRequest is SetWorkspacePriorityRequest's base
// function. An unset priority is the CLEAR spelling, which is legal.
func validateSetWorkspacePriorityRequest(req *agentreplv1.SetWorkspacePriorityRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.Priority != nil {
		return validateWorkspacePriority("priority", req.GetPriority())
	}
	return nil
}

// validateSetWorkspaceHiddenRequest is SetWorkspaceHiddenRequest's base
// function: a workspace and the place asked for are both required.
func validateSetWorkspaceHiddenRequest(req *agentreplv1.SetWorkspaceHiddenRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetVisibility() == nil {
		return invalid("visibility", "a visibility arm is required")
	}
	return nil
}

// validateSetWorkspaceParentRequest is SetWorkspaceParentRequest's base
// function: a workspace and the new target are both required, and a child_of
// target names its parent.
func validateSetWorkspaceParentRequest(req *agentreplv1.SetWorkspaceParentRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	switch target := req.GetParent().(type) {
	case *agentreplv1.SetWorkspaceParentRequest_ChildOf:
		return validateWorkspaceRef("child_of.parent", target.ChildOf.GetParent())
	case *agentreplv1.SetWorkspaceParentRequest_TopLevel:
		return nil
	default:
		return invalid("parent", "a parent arm is required")
	}
}

// validateSwapWorkspacesRequest is SwapWorkspacesRequest's base function: two
// refs naming two different workspaces.
func validateSwapWorkspacesRequest(req *agentreplv1.SwapWorkspacesRequest) *connect.Error {
	return validateWorkspacePair(req.GetWorkspace(), req.GetTarget(), "a workspace cannot swap with itself")
}

// validateMoveWorkspaceRequest is MoveWorkspaceRequest's base function: two
// refs naming two different workspaces.
func validateMoveWorkspaceRequest(req *agentreplv1.MoveWorkspaceRequest) *connect.Error {
	return validateWorkspacePair(req.GetWorkspace(), req.GetTarget(), "a workspace cannot move onto its own tab")
}

// validateWorkspacePair is the base of a request naming two different
// workspaces in its `workspace' and `target' fields; SAME is the refusal of
// one workspace named twice.
func validateWorkspacePair(workspace, target *workspacev1.WorkspaceRef, same string) *connect.Error {
	if err := validateWorkspaceRef("workspace", workspace); err != nil {
		return err
	}
	if err := validateWorkspaceRef("target", target); err != nil {
		return err
	}
	if workspace.GetId() == target.GetId() {
		return invalid("target", same)
	}
	return nil
}

// validateFoldRepositoryRequest is FoldRepositoryRequest's base function.
func validateFoldRepositoryRequest(req *agentreplv1.FoldRepositoryRequest) *connect.Error {
	if err := validateRepositoryRef("repository", req.GetRepository()); err != nil {
		return err
	}
	if req.GetFold() == nil {
		return invalid("fold", "a fold arm is required")
	}
	return nil
}

// validateUpdateSidebarViewRequest is UpdateSidebarViewRequest's base
// function: a change arm, and within it every arm and ref it names.
func validateUpdateSidebarViewRequest(req *agentreplv1.UpdateSidebarViewRequest) *connect.Error {
	switch change := req.GetChange().(type) {
	case nil:
		return invalid("change", "a change arm is required")
	case *agentreplv1.UpdateSidebarViewRequest_FoldSection:
		return validateSidebarViewFoldSection(change.FoldSection)
	}
	return nil
}

// validateSidebarViewFoldSection is SidebarViewFoldSection's base function: a
// section arm (a repository's carrying its ref) and a fold arm.
func validateSidebarViewFoldSection(fold *agentreplv1.SidebarViewFoldSection) *connect.Error {
	switch section := fold.GetSection().(type) {
	case nil:
		return invalid("fold_section.section", "a section arm is required")
	case *agentreplv1.SidebarViewFoldSection_Repository:
		if err := validateRepositoryRef("fold_section.repository", section.Repository); err != nil {
			return err
		}
	case *agentreplv1.SidebarViewFoldSection_Subsection:
		if err := validateSubsectionRef("fold_section.subsection", section.Subsection); err != nil {
			return err
		}
	}
	if fold.GetFold() == nil {
		return invalid("fold_section.fold", "a fold arm is required")
	}
	return nil
}

// validateArrangeSidebarRequest is ArrangeSidebarRequest's base function: a
// change arm, every ref and arm it names, and nothing placed relative to
// itself. A blank NAME is a refusal arm (blank_name), not a validation
// failure, as a blank task title is. Two repository refs naming one
// repository by different spellings are caught after resolution (the
// server's ArrangeSidebar).
func validateArrangeSidebarRequest(req *agentreplv1.ArrangeSidebarRequest) *connect.Error {
	switch change := req.GetChange().(type) {
	case nil:
		return invalid("change", "a change arm is required")
	case *agentreplv1.ArrangeSidebarRequest_CreateSubsection:
		return validateRepositoryRef("create_subsection.repository", change.CreateSubsection.GetRepository())
	case *agentreplv1.ArrangeSidebarRequest_RenameSubsection:
		return validateSubsectionRef("rename_subsection.subsection", change.RenameSubsection.GetSubsection())
	case *agentreplv1.ArrangeSidebarRequest_DeleteSubsection:
		return validateSubsectionRef("delete_subsection.subsection", change.DeleteSubsection.GetSubsection())
	case *agentreplv1.ArrangeSidebarRequest_MoveSubsection:
		return validateArrangeMoveSubsection(change.MoveSubsection)
	case *agentreplv1.ArrangeSidebarRequest_MoveRepository:
		return validateArrangeMoveRepository(change.MoveRepository)
	case *agentreplv1.ArrangeSidebarRequest_PlaceWorkspace:
		return validateArrangePlaceWorkspace(change.PlaceWorkspace)
	case *agentreplv1.ArrangeSidebarRequest_PlaceWorkspaces:
		return validateArrangePlaceWorkspaces(change.PlaceWorkspaces)
	}
	return nil
}

// validateArrangeMoveSubsection is ArrangeMoveSubsection's base function.
func validateArrangeMoveSubsection(move *agentreplv1.ArrangeMoveSubsection) *connect.Error {
	if err := validateSubsectionRef("move_subsection.subsection", move.GetSubsection()); err != nil {
		return err
	}
	switch to := move.GetTo().(type) {
	case nil:
		return invalid("move_subsection.to", "a place arm is required")
	case *agentreplv1.ArrangeMoveSubsection_Before:
		if err := validateSubsectionRef("move_subsection.before", to.Before); err != nil {
			return err
		}
		if to.Before.GetId() == move.GetSubsection().GetId() {
			return invalid("move_subsection.before", "a subsection cannot move before itself")
		}
	case *agentreplv1.ArrangeMoveSubsection_BeforeWorkspace:
		return validateWorkspaceRef("move_subsection.before_workspace", to.BeforeWorkspace)
	}
	return nil
}

// validateArrangeMoveRepository is ArrangeMoveRepository's base function.
func validateArrangeMoveRepository(move *agentreplv1.ArrangeMoveRepository) *connect.Error {
	if err := validateRepositoryRef("move_repository.repository", move.GetRepository()); err != nil {
		return err
	}
	switch to := move.GetTo().(type) {
	case nil:
		return invalid("move_repository.to", "a place arm is required")
	case *agentreplv1.ArrangeMoveRepository_Before:
		return validateRepositoryRef("move_repository.before", to.Before)
	}
	return nil
}

// validateArrangePlaceWorkspace is ArrangePlaceWorkspace's base function.
func validateArrangePlaceWorkspace(place *agentreplv1.ArrangePlaceWorkspace) *connect.Error {
	if err := validateWorkspaceRef("place_workspace.workspace", place.GetWorkspace()); err != nil {
		return err
	}
	switch into := place.GetInto().(type) {
	case nil:
		return invalid("place_workspace.into", "a destination arm is required")
	case *agentreplv1.ArrangePlaceWorkspace_Subsection:
		if err := validateSubsectionRef("place_workspace.subsection", into.Subsection); err != nil {
			return err
		}
	}
	switch at := place.GetAt().(type) {
	case nil:
		return invalid("place_workspace.at", "a place arm is required")
	case *agentreplv1.ArrangePlaceWorkspace_Before:
		if err := validateWorkspaceRef("place_workspace.before", at.Before); err != nil {
			return err
		}
		if at.Before.GetId() == place.GetWorkspace().GetId() {
			return invalid("place_workspace.before", "a workspace cannot be placed before itself")
		}
	case *agentreplv1.ArrangePlaceWorkspace_BeforeSubsection:
		return validateBeforeSubsection("place_workspace", at.BeforeSubsection, place.GetSubsection())
	}
	return nil
}

// validateBeforeSubsection refuses a placement before subsection REF that is
// malformed, or that names a destination subsection INTO: only a workspace
// in no subsection stands before a subsection.
func validateBeforeSubsection(field string, ref, into *agentreplv1.SubsectionRef) *connect.Error {
	if err := validateSubsectionRef(field+".before_subsection", ref); err != nil {
		return err
	}
	if into != nil {
		return invalid(field+".before_subsection", "only a workspace placed in no subsection stands before a subsection")
	}
	return nil
}

// validateArrangePlaceWorkspaces is ArrangePlaceWorkspaces' base function: at
// least one workspace, none twice, a destination and a place, and an anchor
// that is none of the placed workspaces.
func validateArrangePlaceWorkspaces(place *agentreplv1.ArrangePlaceWorkspaces) *connect.Error {
	if len(place.GetWorkspaces()) == 0 {
		return invalid("place_workspaces.workspaces", "at least one workspace is required")
	}
	seen := map[string]bool{}
	for i, ref := range place.GetWorkspaces() {
		if err := validateWorkspaceRef(fmt.Sprintf("place_workspaces.workspaces[%d]", i), ref); err != nil {
			return err
		}
		if seen[ref.GetId()] {
			return invalid(fmt.Sprintf("place_workspaces.workspaces[%d]", i), "a workspace is placed once")
		}
		seen[ref.GetId()] = true
	}
	switch into := place.GetInto().(type) {
	case nil:
		return invalid("place_workspaces.into", "a destination arm is required")
	case *agentreplv1.ArrangePlaceWorkspaces_Subsection:
		if err := validateSubsectionRef("place_workspaces.subsection", into.Subsection); err != nil {
			return err
		}
	}
	switch at := place.GetAt().(type) {
	case nil:
		return invalid("place_workspaces.at", "a place arm is required")
	case *agentreplv1.ArrangePlaceWorkspaces_Before:
		if err := validateWorkspaceRef("place_workspaces.before", at.Before); err != nil {
			return err
		}
		if seen[at.Before.GetId()] {
			return invalid("place_workspaces.before", "the workspaces cannot be placed before one of themselves")
		}
	case *agentreplv1.ArrangePlaceWorkspaces_BeforeSubsection:
		return validateBeforeSubsection("place_workspaces", at.BeforeSubsection, place.GetSubsection())
	}
	return nil
}

// validateUpdateHeldPromptRequest is UpdateHeldPromptRequest's base function.
func validateUpdateHeldPromptRequest(req *agentreplv1.UpdateHeldPromptRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetTurn() == nil {
		return invalid("turn", "a turn id is required")
	}
	if req.GetTurn().GetValue() == "" {
		return invalid("turn.value", "a turn id value is required")
	}
	if req.GetAction() == nil {
		return invalid("action", "a held-prompt action arm is required")
	}
	return nil
}

// validateEditHeldPromptRequest is EditHeldPromptRequest's base function. A
// commit carries the WHOLE new content, so a commit with no `said` is refused
// here rather than answered as an arm.
func validateEditHeldPromptRequest(req *agentreplv1.EditHeldPromptRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetTurn() == nil {
		return invalid("turn", "a turn id is required")
	}
	if req.GetTurn().GetValue() == "" {
		return invalid("turn.value", "a turn id value is required")
	}
	if req.GetAction() == nil {
		return invalid("action", "an edit step arm is required")
	}
	if commit := req.GetCommit(); commit != nil && commit.GetSaid() == nil {
		return invalid("commit.said", "a commit carries the prompt's new content")
	}
	return nil
}

// validateFoldHeldPromptRequest is FoldHeldPromptRequest's base function. A
// request naming one entry as both the folded prompt and the entry ahead is
// malformed rather than a state of the queue.
func validateFoldHeldPromptRequest(req *agentreplv1.FoldHeldPromptRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetTurn() == nil {
		return invalid("turn", "a turn id is required")
	}
	if req.GetTurn().GetValue() == "" {
		return invalid("turn.value", "a turn id value is required")
	}
	if req.GetAbove() == nil {
		return invalid("above", "the turn id of the entry ahead is required")
	}
	if req.GetAbove().GetValue() == "" {
		return invalid("above.value", "the turn id value of the entry ahead is required")
	}
	if req.GetAbove().GetValue() == req.GetTurn().GetValue() {
		return invalid("above", "the entry ahead must be another entry than the one folded")
	}
	return nil
}

// validateUpdateClassifierPromptRequest is UpdateClassifierPromptRequest's
// base function. The example's text may be empty (a prompt that was only an
// image still had a verdict); its route may not.
func validateUpdateClassifierPromptRequest(req *agentreplv1.UpdateClassifierPromptRequest) *connect.Error {
	if strings.TrimSpace(req.GetInstruction()) == "" {
		return invalid("instruction", "the change to make is required")
	}
	if req.GetExample() == nil {
		return invalid("example", "the classified prompt the change was asked from is required")
	}
	if req.GetExample().GetRoute() == agentreplv1.ClassifierRoute_CLASSIFIER_ROUTE_UNSPECIFIED {
		return invalid("example.route", "the classifier's verdict is required")
	}
	return nil
}

// validateAnswerMergeQueueOfferRequest is AnswerMergeQueueOfferRequest's base function.
func validateAnswerMergeQueueOfferRequest(req *agentreplv1.AnswerMergeQueueOfferRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetDecision() == nil {
		return invalid("decision", "a merge-queue answer arm is required")
	}
	return nil
}

// validateUpdateMergeQueueRequest is UpdateMergeQueueRequest's base function.
func validateUpdateMergeQueueRequest(req *agentreplv1.UpdateMergeQueueRequest) *connect.Error {
	if req.GetAction() == nil {
		return invalid("action", "a merge-queue action arm is required")
	}
	if pause := req.GetPause(); pause != nil && pause.Repository != nil {
		return validateRepositoryRef("pause.repository", pause.GetRepository())
	}
	if resume := req.GetResume(); resume != nil && resume.Repository != nil {
		return validateRepositoryRef("resume.repository", resume.GetRepository())
	}
	if evict := req.GetEvict(); evict != nil {
		return validateWorkspaceRef("evict.workspace", evict.GetWorkspace())
	}
	if up := req.GetMoveUp(); up != nil {
		return validateWorkspaceRef("move_up.workspace", up.GetWorkspace())
	}
	if down := req.GetMoveDown(); down != nil {
		return validateWorkspaceRef("move_down.workspace", down.GetWorkspace())
	}
	return nil
}

// validateGetMergeQueueStatusRequest is GetMergeQueueStatusRequest's base
// function.
func validateGetMergeQueueStatusRequest(req *agentreplv1.GetMergeQueueStatusRequest) *connect.Error {
	if req.Repository != nil {
		return validateRepositoryRef("repository", req.GetRepository())
	}
	return nil
}

// validateSubscribeMergeQueueRequest is SubscribeMergeQueueRequest's base
// function.
func validateSubscribeMergeQueueRequest(req *agentreplv1.SubscribeMergeQueueRequest) *connect.Error {
	if err := validateWorkspaceRef("subscriber", req.GetSubscriber()); err != nil {
		return err
	}
	if req.GetDetail().GetLevel() == nil {
		return invalid("detail", "a subscription's detail level arm is required")
	}
	if req.Repository != nil {
		return validateRepositoryRef("repository", req.GetRepository())
	}
	return nil
}

// validateUnsubscribeMergeQueueRequest is UnsubscribeMergeQueueRequest's base
// function.
func validateUnsubscribeMergeQueueRequest(req *agentreplv1.UnsubscribeMergeQueueRequest) *connect.Error {
	if err := validateWorkspaceRef("subscriber", req.GetSubscriber()); err != nil {
		return err
	}
	if req.Repository != nil {
		return validateRepositoryRef("repository", req.GetRepository())
	}
	return nil
}

// validateSubscribeWorkspaceRequest is SubscribeWorkspaceRequest's base
// function.
func validateSubscribeWorkspaceRequest(req *agentreplv1.SubscribeWorkspaceRequest) *connect.Error {
	if err := validateWorkspaceRef("subscriber", req.GetSubscriber()); err != nil {
		return err
	}
	if err := validateWorkspaceRef("followed", req.GetFollowed()); err != nil {
		return err
	}
	if len(req.GetEvents()) == 0 {
		return invalid("events", "a subscription names at least one event")
	}
	for _, e := range req.GetEvents() {
		if e == agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_UNSPECIFIED {
			return invalid("events", "an unspecified event is no event")
		}
		if _, declared := agentreplv1.WorkspaceNoticeEvent_name[int32(e)]; !declared {
			return invalid("events", fmt.Sprintf("event %d is not declared", e))
		}
	}
	return nil
}

// validateUnsubscribeWorkspaceRequest is UnsubscribeWorkspaceRequest's base
// function.
func validateUnsubscribeWorkspaceRequest(req *agentreplv1.UnsubscribeWorkspaceRequest) *connect.Error {
	if err := validateWorkspaceRef("subscriber", req.GetSubscriber()); err != nil {
		return err
	}
	return validateWorkspaceRef("followed", req.GetFollowed())
}

// validateCompactWorkspaceRequest is CompactWorkspaceRequest's base function.
func validateCompactWorkspaceRequest(req *agentreplv1.CompactWorkspaceRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if err := validateWorkspaceRequester("requester", req.GetRequester()); err != nil {
		return err
	}
	if req.Instructions != nil && strings.TrimSpace(req.GetInstructions().GetText()) == "" {
		return invalid("instructions.text", "instructions say what the summary keeps")
	}
	return nil
}

// validateEvaluateInEditorRequest is EvaluateInEditorRequest's base function.
func validateEvaluateInEditorRequest(req *agentreplv1.EvaluateInEditorRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if strings.TrimSpace(req.GetText()) == "" {
		return invalid("text", "an evaluation names the elisp to evaluate")
	}
	return nil
}

// validateAnswerEditorEvaluationRequest is AnswerEditorEvaluationRequest's
// base function.
func validateAnswerEditorEvaluationRequest(req *agentreplv1.AnswerEditorEvaluationRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetToken().GetValue() == "" {
		return invalid("token", "an answer echoes the evaluation's token")
	}
	if req.GetOutcome().GetOutcome() == nil {
		return invalid("outcome", "an outcome arm (value, read_error or eval_error) is required")
	}
	return nil
}

// validateSetWorkspaceCorrespondenceRequest is
// SetWorkspaceCorrespondenceRequest's base function.
func validateSetWorkspaceCorrespondenceRequest(req *agentreplv1.SetWorkspaceCorrespondenceRequest) *connect.Error {
	if err := validateWorkspaceRequester("requester", req.GetRequester()); err != nil {
		return err
	}
	switch mark := req.GetMark().(type) {
	case *agentreplv1.SetWorkspaceCorrespondenceRequest_Awaiting:
		return validateWorkspaceRef("awaiting.awaited", mark.Awaiting.GetAwaited())
	case *agentreplv1.SetWorkspaceCorrespondenceRequest_Clear:
		return nil
	}
	return invalid("mark", "a mark arm (awaiting or clear) is required")
}

// validateUpdateDeployHoldRequest is UpdateDeployHoldRequest's base function:
// a repository and an action arm.
func validateUpdateDeployHoldRequest(req *agentreplv1.UpdateDeployHoldRequest) *connect.Error {
	if req.GetRepository() == nil {
		return invalid("repository", "the repository whose deploy stage is held or released is required")
	}
	if req.GetAction() == nil {
		return invalid("action", "a deploy hold action arm (hold or release) is required")
	}
	return nil
}

// validateRollOutLandingRequest is RollOutLandingRequest's base function: a
// named deploy carries its ledger.
func validateRollOutLandingRequest(req *agentreplv1.RollOutLandingRequest) *connect.Error {
	if req.Deploy != nil && strings.TrimSpace(req.GetDeploy().GetLedger()) == "" {
		return invalid("deploy.ledger", "a named deploy carries its ledger")
	}
	return nil
}

// validateUpdateShutdownScheduleRequest is UpdateShutdownScheduleRequest's base
// function.
// validateUpdatePersistentWifiModeRequest refuses a request naming no action.
func validateUpdatePersistentWifiModeRequest(req *agentreplv1.UpdatePersistentWifiModeRequest) *connect.Error {
	if req.GetAction() == nil {
		return invalid("action", "a persistent-wifi action arm is required")
	}
	return nil
}

// validateDismissNewsDigestRequest refuses a dismiss naming no digest.
func validateDismissNewsDigestRequest(req *agentreplv1.DismissNewsDigestRequest) *connect.Error {
	if req.GetId().GetValue() == "" {
		return invalid("id.value", "the digest being dismissed is required")
	}
	return nil
}

func validateUpdateShutdownScheduleRequest(req *agentreplv1.UpdateShutdownScheduleRequest) *connect.Error {
	if req.GetAction() == nil {
		return invalid("action", "a shutdown-schedule action arm is required")
	}
	if schedule := req.GetSchedule(); schedule != nil {
		if schedule.GetAtMs() == 0 {
			return invalid("schedule.at_ms", "a schedule instant is required")
		}
		return validateDrainReason("schedule.reason", schedule.GetReason())
	}
	if now := req.GetNow(); now != nil {
		return validateDrainReason("now.reason", now.GetReason())
	}
	return nil
}

// validateClientLogRequest is ClientLogRequest's base function.
func validateClientLogRequest(req *agentreplv1.ClientLogRequest) *connect.Error {
	switch req.GetSource().(type) {
	case *agentreplv1.ClientLogRequest_Workspace:
		if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
			return err
		}
	case *agentreplv1.ClientLogRequest_Webhost:
	default:
		return invalid("source", "a source arm (workspace or webhost) is required")
	}
	return validateClientLogRecord("record", req.GetRecord())
}

// validateClientLogRecord is ClientLogRecord's base function.
func validateClientLogRecord(field string, record *agentreplv1.ClientLogRecord) *connect.Error {
	if record == nil {
		return invalid(field, "a log record is required")
	}
	if record.GetLevel() == nil {
		return invalid(field+".level", "a log level arm is required")
	}
	if record.GetOperation() == "" {
		return invalid(field+".operation", "an operation is required")
	}
	if record.GetMessage() == "" {
		return invalid(field+".message", "a message is required")
	}
	return nil
}

// validateOpenExternalRequest is OpenExternalRequest's base function.
func validateOpenExternalRequest(req *agentreplv1.OpenExternalRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetUrl() == "" {
		return invalid("url", "a url is required")
	}
	return nil
}

// validateOpenInEditorRequest is OpenInEditorRequest's base function.
func validateOpenInEditorRequest(req *agentreplv1.OpenInEditorRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	switch target := req.GetTarget().(type) {
	case *agentreplv1.OpenInEditorRequest_WorkspaceFile:
		if target.WorkspaceFile.GetPath() == "" {
			return invalid("workspace_file.path", "a path is required")
		}
	case *agentreplv1.OpenInEditorRequest_MergeTestLog:
		if target.MergeTestLog.GetValue() == "" {
			return invalid("merge_test_log.value", "a test log token is required")
		}
	case *agentreplv1.OpenInEditorRequest_FeedLink:
		if target.FeedLink.GetHref() == "" {
			return invalid("feed_link.href", "an href is required")
		}
		if target.FeedLink.GetOnUnresolved() == nil {
			return invalid("feed_link.on_unresolved", "an on_unresolved arm is required")
		}
	case *agentreplv1.OpenInEditorRequest_CommitLog:
		switch {
		case target.CommitLog.GetRepositoryDir() == "":
			return invalid("commit_log.repository_dir", "a repository dir is required")
		case target.CommitLog.GetBranch() == "":
			return invalid("commit_log.branch", "a branch is required")
		case target.CommitLog.GetCommit() == "":
			return invalid("commit_log.commit", "a commit is required")
		}
	default:
		return invalid("target", "a target arm is required")
	}
	return nil
}

// validateFinishWorkspaceRequest is FinishWorkspaceRequest's base function: the
// workspace's ref, its requester and its placement.
func validateFinishWorkspaceRequest(req *agentreplv1.FinishWorkspaceRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if err := validateWorkspaceRequester("requester", req.GetRequester()); err != nil {
		return err
	}
	if err := validateMergeQueuePlacement("placement", req.GetPlacement()); err != nil {
		return err
	}
	if err := validateMergeWorkspaceVersionIncrement("version_increment", req.GetVersionIncrement()); err != nil {
		return err
	}
	return nil
}

// validateSendLoginInputRequest is SendLoginInputRequest's base function.
func validateSendLoginInputRequest(req *agentreplv1.SendLoginInputRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetInput() == nil {
		return invalid("input", "a login input arm is required")
	}
	if keystrokes := req.GetKeystrokes(); keystrokes != nil && len(keystrokes.GetData()) == 0 {
		return invalid("keystrokes.data", "keystrokes carry at least one byte")
	}
	if resize := req.GetResize(); resize != nil {
		if resize.GetRows() <= 0 {
			return invalid("resize.rows", "a positive row count is required")
		}
		if resize.GetCols() <= 0 {
			return invalid("resize.cols", "a positive column count is required")
		}
	}
	return nil
}
