package server

import (
	"fmt"
	"strings"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/prompthandler"
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
	}
	if oneShot := req.GetOneShot(); oneShot != nil {
		if err := validateUserSaid("one_shot.prompt", oneShot.GetPrompt()); err != nil {
			return err
		}
		if about := oneShot.GetContext(); about != nil {
			if err := validateWorkspaceRef("one_shot.context.workspace", about.GetWorkspace()); err != nil {
				return err
			}
		}
	}
	if parent := req.GetParent(); parent != nil {
		if err := validateWorkspaceRef("parent.workspace", parent.GetWorkspace()); err != nil {
			return err
		}
		// A child always starts from its parent's tip.
		if standard := req.GetStandard(); standard != nil && standard.BaseRef != nil {
			return invalid("standard.base_ref", "a child is cut from its parent's tip, so it takes no base ref")
		}
	}
	if source := req.GetForkSource(); source != nil {
		if req.GetParent() != nil {
			return invalid("fork_source", "a fork into the primary branch has no parent; a child fork is parent.fork")
		}
		if err := validateWorkspaceRef("fork_source.workspace", source.GetWorkspace()); err != nil {
			return err
		}
	}
	if req.Priority != nil {
		if err := validateWorkspacePriority("priority", req.GetPriority()); err != nil {
			return err
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
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if err := validateWorkspaceRef("target", req.GetTarget()); err != nil {
		return err
	}
	if req.GetWorkspace().GetId() == req.GetTarget().GetId() {
		return invalid("target", "a workspace cannot swap with itself")
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
	}
	if fold.GetFold() == nil {
		return invalid("fold_section.fold", "a fold arm is required")
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
	if subscribe := req.GetSubscribe(); subscribe != nil {
		if err := validateWorkspaceRef("subscribe.subscriber", subscribe.GetSubscriber()); err != nil {
			return err
		}
		if subscribe.GetDetail().GetLevel() == nil {
			return invalid("subscribe.detail", "a subscription's detail level arm is required")
		}
		if subscribe.Repository != nil {
			return validateRepositoryRef("subscribe.repository", subscribe.GetRepository())
		}
	}
	if unsubscribe := req.GetUnsubscribe(); unsubscribe != nil {
		if err := validateWorkspaceRef("unsubscribe.subscriber", unsubscribe.GetSubscriber()); err != nil {
			return err
		}
		if unsubscribe.Repository != nil {
			return validateRepositoryRef("unsubscribe.repository", unsubscribe.GetRepository())
		}
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

// validateMergeWorkspaceRequest is MergeWorkspaceRequest's base function: the
// requester's ref, and exactly one mode arm, each carrying what it names.
func validateMergeWorkspaceRequest(req *agentreplv1.MergeWorkspaceRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if v := req.GetVersionIncrement(); v != nil {
		if v.GetIncrement() == nil {
			return invalid("version_increment.increment", "an increment arm (minor or patch) is required")
		}
		if err := versionIncrement(v).Validate(); err != nil {
			return invalid("version_increment.changelog_line", err.Error())
		}
	}
	switch mode := req.GetMode().(type) {
	case *agentreplv1.MergeWorkspaceRequest_OneShot:
		if mode.OneShot.GetLanding() == nil {
			return invalid("one_shot.landing", "a landing arm (own_branch or merged_upstream) is required")
		}
		return nil
	case *agentreplv1.MergeWorkspaceRequest_Async:
		async := mode.Async
		if strings.TrimSpace(async.GetName()) == "" {
			return invalid("async.name", "an async merge names its merge workspace")
		}
		if strings.TrimSpace(async.GetBranch()) == "" {
			return invalid("async.branch", "an async merge names the branch it lands")
		}
		if async.Instruction != nil && strings.TrimSpace(async.GetInstruction()) == "" {
			return invalid("async.instruction", "an instruction says how to prepare the branch")
		}
		if async.OneShot != nil {
			return validateWorkspaceRef("async.one_shot.workspace", async.GetOneShot().GetWorkspace())
		}
		return nil
	default:
		return invalid("mode", "a mode arm (one_shot or async) is required")
	}
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
