package shimclient

import (
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/promptorigin"
)

// The base-function convention, applied to the shim's request messages: one
// validate<Message> per message, every message-typed field and every oneof arm
// delegating to the child's own base function, and an unset non-optional field
// or an unset oneof an ERROR. Nothing here decides policy; a request that
// cannot be legal on the wire never reaches the shim.

// validateStartSessionRequest is StartSessionRequest's base function.
func validateStartSessionRequest(req *shimv1.StartSessionRequest) error {
	const m = "StartSessionRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	if req.GetWorkspaceName() == "" {
		return invalid(m, m+".workspace_name", "the session's system prompt names its workspace, and the name is empty")
	}
	switch source := req.GetSource().(type) {
	case *shimv1.StartSessionRequest_Fresh:
		return validateStartSessionFresh(m+".fresh", source.Fresh)
	case *shimv1.StartSessionRequest_Resume:
		return validateStartSessionResume(m+".resume", source.Resume)
	default:
		return invalid(m, m+".source", "oneof is unset")
	}
}

// validateStartSessionFresh is StartSessionFresh's base function.
func validateStartSessionFresh(field string, fresh *shimv1.StartSessionFresh) error {
	const m = "StartSessionFresh"
	if fresh == nil {
		return invalid(m, field, "arm is nil")
	}
	// LANDING 7: model is OPTIONAL — unset means the SDK's own default. A
	// model that IS named still has to be well formed, so presence selects
	// whether the base function runs, never a sentinel value.
	if fresh.Model != nil {
		if err := validateAgentModel(field+".model", fresh.GetModel()); err != nil {
			return err
		}
	}
	return validateAgentPermissionMode(field+".permission_mode", fresh.GetPermissionMode())
}

// validateStartSessionResume is StartSessionResume's base function.
func validateStartSessionResume(field string, resume *shimv1.StartSessionResume) error {
	const m = "StartSessionResume"
	if resume == nil {
		return invalid(m, field, "arm is nil")
	}
	if resume.GetVendorSessionId() == "" {
		return invalid(m, field+".vendor_session_id", "is empty")
	}
	if resume.ColdRemediation == nil {
		return nil
	}
	return validateSessionColdRemediation(field+".cold_remediation", resume.GetColdRemediation())
}

// validateSessionColdRemediation is SessionColdRemediation's base function.
func validateSessionColdRemediation(field string, rem *conversationv1.SessionColdRemediation) error {
	const m = "SessionColdRemediation"
	if rem == nil {
		return invalid(m, field, "is set but nil")
	}
	switch arm := rem.GetRemediation().(type) {
	case *conversationv1.SessionColdRemediation_Pay:
		if arm.Pay == nil {
			return invalid(m, field+".pay", "arm is nil")
		}
		return nil
	case *conversationv1.SessionColdRemediation_Clear:
		if arm.Clear == nil {
			return invalid(m, field+".clear", "arm is nil")
		}
		return nil
	case *conversationv1.SessionColdRemediation_Compact:
		return validateSessionColdCompact(field+".compact", arm.Compact)
	default:
		return invalid(m, field+".remediation", "oneof is unset")
	}
}

// validateSessionColdCompact is SessionColdCompact's base function.
func validateSessionColdCompact(field string, compact *conversationv1.SessionColdCompact) error {
	const m = "SessionColdCompact"
	if compact == nil {
		return invalid(m, field, "arm is nil")
	}
	if err := validateAgentModel(field+".model", compact.GetModel()); err != nil {
		return err
	}
	if compact.GetScope() == conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED {
		return invalid(m, field+".scope", "is unspecified")
	}
	return nil
}

// validateAgentModel is AgentModel's base function.
func validateAgentModel(field string, model *conversationv1.AgentModel) error {
	const m = "AgentModel"
	if model == nil {
		return invalid(m, field, "is unset")
	}
	if model.GetName() == "" {
		return invalid(m, field+".name", "is empty")
	}
	return nil
}

// validateAgentPermissionMode is AgentPermissionMode's base function. A mode IS
// a state, so an unset oneof names no mode at all.
func validateAgentPermissionMode(field string, mode *conversationv1.AgentPermissionMode) error {
	const m = "AgentPermissionMode"
	if mode == nil {
		return invalid(m, field, "is unset")
	}
	if mode.GetMode() == nil {
		return invalid(m, field+".mode", "oneof is unset")
	}
	return nil
}

// validateSetSessionModelRequest is SetSessionModelRequest's base function.
func validateSetSessionModelRequest(req *shimv1.SetSessionModelRequest) error {
	const m = "SetSessionModelRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	if err := validateAgentModel(m+".model", req.GetModel()); err != nil {
		return err
	}
	if req.ColdRemediation == nil {
		return nil
	}
	return validateSessionColdRemediation(m+".cold_remediation", req.GetColdRemediation())
}

// validateSetSessionEffortRequest is SetSessionEffortRequest's base function.
// The level is an enum whose zero is unset, so UNSPECIFIED is refused.
func validateSetSessionEffortRequest(req *shimv1.SetSessionEffortRequest) error {
	const m = "SetSessionEffortRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	if req.GetEffort() == conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
		return invalid(m, m+".effort", "an effort change names its level")
	}
	return nil
}

// validateSetSessionPermissionModeRequest is
// SetSessionPermissionModeRequest's base function.
func validateSetSessionPermissionModeRequest(req *shimv1.SetSessionPermissionModeRequest) error {
	const m = "SetSessionPermissionModeRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	return validateAgentPermissionMode(m+".permission_mode", req.GetPermissionMode())
}

// validateHibernateRequest is HibernateRequest's base function. The message is
// empty; the request itself must still exist.
func validateHibernateRequest(req *shimv1.HibernateRequest) error {
	const m = "HibernateRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	return nil
}

// validateSetKeepaliveRequest is SetKeepaliveRequest's base function: the
// cadence arm is the request, so an unset one asks for nothing.
func validateSetKeepaliveRequest(req *shimv1.SetKeepaliveRequest) error {
	const m = "SetKeepaliveRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	if req.GetCadence() == nil {
		return invalid(m, m+".cadence", "a keep-alive request names hold, run or a last round")
	}
	return nil
}

// validateSendKeepaliveRequest is SendKeepaliveRequest's base function: the
// request is empty by design, so only its absence is refused.
func validateSendKeepaliveRequest(req *shimv1.SendKeepaliveRequest) error {
	const m = "SendKeepaliveRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	return nil
}

// validateKillSessionRequest is KillSessionRequest's base function.
func validateKillSessionRequest(req *shimv1.KillSessionRequest) error {
	const m = "KillSessionRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	return nil
}

// validateStartTurnRequest is StartTurnRequest's base function.
func validateStartTurnRequest(req *shimv1.StartTurnRequest) error {
	const m = "StartTurnRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	if err := validateTurnID(m+".turn", req.GetTurn()); err != nil {
		return err
	}
	if err := validateUserSaid(m+".said", req.GetSaid()); err != nil {
		return err
	}
	if req.GetOrigin() == conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
		return invalid(m, m+".origin", "is unspecified")
	}
	if err := promptorigin.CheckSender(req.GetOrigin(), req.SentBy != nil); err != nil {
		return invalid(m, m+".sent_by", err.Error())
	}
	if req.SentBy != nil {
		if err := validateStartTurnSentBy(m+".sent_by", req.GetSentBy()); err != nil {
			return err
		}
	}
	return validateStartTurnOpening(m, req)
}

// validateStartTurnSentBy is StartTurnSentBy's base function. The name and the
// mode are each UNSET when the daemon holds none, never empty when set.
func validateStartTurnSentBy(field string, sentBy *shimv1.StartTurnSentBy) error {
	const m = "StartTurnSentBy"
	if sentBy.GetWorkspaceId() == "" {
		return invalid(m, field+".workspace_id", "is empty")
	}
	if sentBy.WorkspaceName != nil && sentBy.GetWorkspaceName() == "" {
		return invalid(m, field+".workspace_name", "is set but empty")
	}
	if sentBy.PermissionMode != nil {
		return validateAgentPermissionMode(field+".permission_mode", sentBy.GetPermissionMode())
	}
	return nil
}

// validateStartTurnOpening is StartTurnRequest.opening's dedicated function.
// UNSET IS REFUSED although the wire reads it as a repaint: the daemon never
// asks a turn's opening page to replay history (owner ruling, feed paging on
// demand), so a request without an arm is the daemon breaking its own rule.
func validateStartTurnOpening(m string, req *shimv1.StartTurnRequest) error {
	switch arm := req.GetOpening().(type) {
	case *shimv1.StartTurnRequest_KnownThrough:
		return validateHistoryPointer(m+".known_through", arm.KnownThrough)
	case *shimv1.StartTurnRequest_TailOnly:
		if arm.TailOnly == nil {
			return invalid(m, m+".tail_only", "arm is nil")
		}
		return nil
	default:
		return invalid(m, m+".opening", "oneof is unset: the daemon never asks for a repaint")
	}
}

// validateTurnID is TurnId's base function.
func validateTurnID(field string, turn *conversationv1.TurnId) error {
	const m = "TurnId"
	if turn == nil {
		return invalid(m, field, "is unset")
	}
	if turn.GetValue() == "" {
		return invalid(m, field+".value", "is empty")
	}
	return nil
}

// validateAgentID is AgentId's base function.
func validateAgentID(field string, agent *conversationv1.AgentId) error {
	const m = "AgentId"
	if agent == nil {
		return invalid(m, field, "is set but nil")
	}
	if agent.GetValue() == "" {
		return invalid(m, field+".value", "is empty")
	}
	return nil
}

// validateHistoryPointer is HistoryPointer's base function.
func validateHistoryPointer(field string, ptr *conversationv1.HistoryPointer) error {
	const m = "HistoryPointer"
	if ptr == nil {
		return invalid(m, field, "is set but nil")
	}
	if ptr.GetValue() == "" {
		return invalid(m, field+".value", "is empty")
	}
	return nil
}

// validateUserSaid is UserSaid's base function.
func validateUserSaid(field string, said *conversationv1.UserSaid) error {
	const m = "UserSaid"
	if said == nil {
		return invalid(m, field, "is unset")
	}
	content := said.GetContent()
	if content == nil {
		return invalid(m, field+".content", "is unset")
	}
	blocks := content.GetBlocks()
	if len(blocks) == 0 {
		return invalid(m, field+".content.blocks", "is empty")
	}
	for i, block := range blocks {
		if block == nil || block.GetBlock() == nil {
			return invalid(m, fmt.Sprintf("%s.content.blocks[%d]", field, i), "block has no arm set")
		}
	}
	return nil
}

// validateWatchAgentRequest is WatchAgentRequest's base function.
func validateWatchAgentRequest(req *shimv1.WatchAgentRequest) error {
	const m = "WatchAgentRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	if req.Target != nil {
		if err := validateAgentID(m+".target", req.GetTarget()); err != nil {
			return err
		}
	}
	return validateWatchAgentOpening(m, req)
}

// validateWatchAgentOpening is WatchAgentRequest.opening's dedicated function.
// UNSET IS REFUSED although the wire reads it as a repaint: opening a watch
// replays no history (owner ruling, feed paging on demand), so a request
// without an arm is the daemon breaking its own rule.
func validateWatchAgentOpening(m string, req *shimv1.WatchAgentRequest) error {
	switch arm := req.GetOpening().(type) {
	case *shimv1.WatchAgentRequest_KnownThrough:
		return validateHistoryPointer(m+".known_through", arm.KnownThrough)
	case *shimv1.WatchAgentRequest_TailOnly:
		if arm.TailOnly == nil {
			return invalid(m, m+".tail_only", "arm is nil")
		}
		return nil
	default:
		return invalid(m, m+".opening", "oneof is unset: the daemon never asks for a repaint")
	}
}

// validateUpdateAgentRequest is UpdateAgentRequest's base function.
func validateUpdateAgentRequest(req *shimv1.UpdateAgentRequest) error {
	const m = "UpdateAgentRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	if req.Target != nil {
		if err := validateAgentID(m+".target", req.GetTarget()); err != nil {
			return err
		}
	}
	return validateAgentInput(m+".input", req.GetInput())
}

// validateAgentInput is AgentInput's base function.
func validateAgentInput(field string, input *conversationv1.AgentInput) error {
	const m = "AgentInput"
	if input == nil {
		return invalid(m, field, "is unset")
	}
	switch arm := input.GetInput().(type) {
	case *conversationv1.AgentInput_Stop:
		if arm.Stop == nil {
			return invalid(m, field+".stop", "arm is nil")
		}
		return nil
	case *conversationv1.AgentInput_Answer:
		return validateAgentAnswer(field+".answer", arm.Answer)
	case *conversationv1.AgentInput_Prompt:
		return validateUserSaid(field+".prompt", arm.Prompt)
	default:
		return invalid(m, field+".input", "oneof is unset")
	}
}

// validateAgentAnswer is AgentAnswer's base function.
func validateAgentAnswer(field string, answer *conversationv1.AgentAnswer) error {
	const m = "AgentAnswer"
	if answer == nil {
		return invalid(m, field, "arm is nil")
	}
	switch arm := answer.GetAnswer().(type) {
	case *conversationv1.AgentAnswer_QuestionAnswer:
		return validateAgentQuestionAnswer(field+".question_answer", arm.QuestionAnswer)
	case *conversationv1.AgentAnswer_PermissionDecision:
		return validateAgentPermissionDecision(field+".permission_decision", arm.PermissionDecision)
	default:
		return invalid(m, field+".answer", "oneof is unset")
	}
}

// validateAgentQuestionAnswer is AgentQuestionAnswer's base function.
func validateAgentQuestionAnswer(field string, answer *conversationv1.AgentQuestionAnswer) error {
	const m = "AgentQuestionAnswer"
	if answer == nil {
		return invalid(m, field, "arm is nil")
	}
	if answer.GetAsk() == nil || answer.GetAsk().GetValue() == "" {
		return invalid(m, field+".ask", "is unset or empty")
	}
	if answer.GetAnswers() == nil {
		return invalid(m, field+".answers", "is unset")
	}
	return nil
}

// validateAgentPermissionDecision is AgentPermissionDecision's base function.
func validateAgentPermissionDecision(field string, decision *conversationv1.AgentPermissionDecision) error {
	const m = "AgentPermissionDecision"
	if decision == nil {
		return invalid(m, field, "arm is nil")
	}
	if decision.GetAsk() == nil || decision.GetAsk().GetValue() == "" {
		return invalid(m, field+".ask", "is unset or empty")
	}
	if decision.GetDecision() == nil {
		return invalid(m, field+".decision", "oneof is unset")
	}
	return nil
}

// validateKillTurnRequest is KillTurnRequest's base function.
func validateKillTurnRequest(req *shimv1.KillTurnRequest) error {
	const m = "KillTurnRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	return validateTurnID(m+".turn", req.GetTurn())
}

// validateRollBackSessionRequest is RollBackSessionRequest's base function:
// the first dropped turn, every dropped turn beginning with it, and the files
// choice are all required.
func validateRollBackSessionRequest(req *shimv1.RollBackSessionRequest) error {
	const m = "RollBackSessionRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	if err := validateTurnID(m+".to_before", req.GetToBefore()); err != nil {
		return err
	}
	dropped := req.GetDroppedTurns()
	if len(dropped) == 0 || dropped[0].GetValue() != req.GetToBefore().GetValue() {
		return invalid(m, m+".dropped_turns", "must begin with to_before")
	}
	for _, turn := range dropped {
		if err := validateTurnID(m+".dropped_turns", turn); err != nil {
			return err
		}
	}
	if req.GetFiles() == nil {
		return invalid(m, m+".files", "is unset")
	}
	return nil
}

// validateDetachedWorkID is DetachedWorkId's base function.
func validateDetachedWorkID(field string, work *conversationv1.DetachedWorkId) error {
	const m = "DetachedWorkId"
	if work == nil {
		return invalid(m, field, "is unset")
	}
	if work.GetValue() == "" {
		return invalid(m, field+".value", "is empty")
	}
	return nil
}

// validateStopBashRequest is StopBashRequest's base function.
func validateStopBashRequest(req *shimv1.StopBashRequest) error {
	const m = "StopBashRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	return validateDetachedWorkID(m+".work", req.GetWork())
}

// validateDetachForegroundRequest is DetachForegroundRequest's base function.
func validateDetachForegroundRequest(req *shimv1.DetachForegroundRequest) error {
	const m = "DetachForegroundRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	unit := req.GetUnit()
	if unit == nil {
		return invalid(m, m+".unit", "is unset")
	}
	if unit.GetValue() == "" {
		return invalid(m, m+".unit.value", "is empty")
	}
	return nil
}

// validateReadTranscriptsRequest is ReadTranscriptsRequest's base function. The
// message carries no fields (the shim resolves its own working directory), so
// the only illegal shape is a nil request.
func validateReadTranscriptsRequest(req *shimv1.ReadTranscriptsRequest) error {
	const m = "ReadTranscriptsRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	return nil
}

// validateResolvePeerDeliveryRequest is ResolvePeerDeliveryRequest's base
// function: one delivery id, never empty.
func validateResolvePeerDeliveryRequest(req *shimv1.ResolvePeerDeliveryRequest) error {
	const m = "ResolvePeerDeliveryRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	if req.GetDeliveryId().GetValue() == "" {
		return invalid(m, m+".delivery_id", "delivery id is unset or empty")
	}
	return nil
}

// validateResolveVendorTaskRequest is ResolveVendorTaskRequest's base function:
// a session's main agent and one task id, neither empty.
func validateResolveVendorTaskRequest(req *shimv1.ResolveVendorTaskRequest) error {
	const m = "ResolveVendorTaskRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	if err := validateAgentID(m+".session", req.GetSession()); err != nil {
		return err
	}
	if req.GetVendorTaskId() == "" {
		return invalid(m, m+".vendor_task_id", "vendor task id is empty")
	}
	return nil
}

// validateGatherTitleDigestRequest is GatherTitleDigestRequest's base function.
// The message carries no fields (the shim resolves its own transcript), so the
// only illegal shape is a nil request.
func validateGatherTitleDigestRequest(req *shimv1.GatherTitleDigestRequest) error {
	const m = "GatherTitleDigestRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	return nil
}

// validateGetForkPointRequest is GetForkPointRequest's base function.
func validateGetForkPointRequest(req *shimv1.GetForkPointRequest) error {
	const m = "GetForkPointRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	return nil
}

// validateForkSessionRequest is ForkSessionRequest's base function: a parent,
// where it lives, and a point with exactly one extent.
func validateForkSessionRequest(req *shimv1.ForkSessionRequest) error {
	const m = "ForkSessionRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	if req.GetParentVendorSessionId() == "" {
		return invalid(m, m+".parent_vendor_session_id", "is empty")
	}
	if req.GetParentWorkspaceDir() == "" {
		return invalid(m, m+".parent_workspace_dir", "is empty")
	}
	point := req.GetPoint()
	if point == nil {
		return invalid(m, m+".point", "is unset")
	}
	switch extent := point.GetExtent().(type) {
	case *shimv1.ForkPoint_Whole:
		return nil
	case *shimv1.ForkPoint_ThroughRecord:
		if extent.ThroughRecord.GetRecord().GetValue() == "" {
			return invalid(m, m+".point.through_record.record", "names no record")
		}
		return nil
	case *shimv1.ForkPoint_BeforeRecord:
		if extent.BeforeRecord.GetRecord().GetValue() == "" {
			return invalid(m, m+".point.before_record.record", "names no record")
		}
		return nil
	default:
		return invalid(m, m+".point.extent", "has no arm set")
	}
}

// validateReadHistoryRequest is ReadHistoryRequest's base function.
func validateReadHistoryRequest(req *shimv1.ReadHistoryRequest) error {
	const m = "ReadHistoryRequest"
	if req == nil {
		return invalid(m, m, "request is nil")
	}
	if req.Target != nil {
		if err := validateAgentID(m+".target", req.GetTarget()); err != nil {
			return err
		}
	}
	switch position := req.GetPosition().(type) {
	case *shimv1.ReadHistoryRequest_First:
		if position.First == nil {
			return invalid(m, m+".first", "arm is nil")
		}
		return nil
	case *shimv1.ReadHistoryRequest_After:
		return validateHistoryPointer(m+".after", position.After)
	case *shimv1.ReadHistoryRequest_Through:
		// A BOUNDED READ, a fork's inherited past from its fork point: the
		// bound is a positive instant, or the book as it stood before anything
		// was said, which is a malformed request rather than an empty page.
		if position.Through.GetAtMs() <= 0 {
			return invalid(m, m+".through.at_ms", "is not a positive instant")
		}
		return nil
	default:
		return invalid(m, m+".position", "oneof is unset")
	}
}
