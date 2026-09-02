package workspace

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/sessionwatcher"
)

// THE QUEUE'S DELIVERY SURFACE.
//
// The prompt queue delivers through a narrow Sender rather than through a shim
// client, so it is testable against a fake instead of a process. The narrowing
// lives here, beside the fleet, because the fleet is what holds the client and
// because every shim refusal is already given its typed arm in this package —
// a second unwrapping elsewhere would answer the same failures differently.

// turnPageSize is the budget the opening page of an accepted turn is asked
// for. It matches the session watcher's opening catch-up budget: the page
// StartTurn returns is fed through the same history path a watch's own opening
// page takes, and two budgets for one path would page differently by accident.
const turnPageSize = 200

// Sender answers the workspace's queue delivery surface, false when no session
// is up. It is the promptqueue.ClientFunc the queue is wired with.
func (f *Fleet) Sender(ws ids.WorkspaceID) (promptqueue.Sender, bool) {
	client, ok := f.Client(ws)
	if !ok {
		return nil, false
	}
	return &sender{client: client}, true
}

// Watcher answers the workspace's session watcher, false when no session is
// up. It is the promptqueue.WatcherFunc: an accepted turn is handed over to
// the watcher that will see it end.
func (f *Fleet) Watcher(ws ids.WorkspaceID) (promptqueue.Watcher, bool) {
	f.mu.RLock()
	session, ok := f.sessions[ws]
	f.mu.RUnlock()
	if !ok || session.watcher == nil {
		return nil, false
	}
	return session.watcher, true
}

// sender narrows a shim client to the queue's delivery surface.
type sender struct{ client shimclientSender }

// shimclientSender is the slice of the shim client the delivery surface uses.
// It is named so the adapter can be exercised against a fake client without
// the whole Client interface behind it.
type shimclientSender interface {
	StartTurn(ctx context.Context, req *shimv1.StartTurnRequest) (*shimv1.StartTurnResponse, error)
	UpdateAgent(ctx context.Context, req *shimv1.UpdateAgentRequest) (*shimv1.UpdateAgentResponse, error)
	KillTurn(ctx context.Context, req *shimv1.KillTurnRequest) (*shimv1.KillTurnResponse, error)
	SetSessionModel(ctx context.Context, req *shimv1.SetSessionModelRequest) (*shimv1.SetSessionModelResponse, error)
	SetSessionPermissionMode(ctx context.Context, req *shimv1.SetSessionPermissionModeRequest) (*shimv1.SetSessionPermissionModeResponse, error)
}

// StartTurn opens the turn and hands back the shim's success WHOLE: the queue
// needs the prompt's agent for SetMainAgent and the opening page for the
// watcher's turn handover, and neither is recoverable from an error.
func (s *sender) StartTurn(ctx context.Context, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin) (*shimv1.StartTurnSuccess, error) {
	response, err := s.client.StartTurn(ctx, &shimv1.StartTurnRequest{
		Turn:     &conversationv1.TurnId{Value: string(turn)},
		Said:     said,
		Origin:   origin,
		PageSize: turnPageSize,
	})
	if err != nil {
		return nil, err
	}
	if failure := response.GetFailure(); failure != nil {
		return nil, &ShimRefusal{Verb: "StartTurn", Arm: startTurnArm(failure), Detail: failure.GetDetail()}
	}
	success := response.GetSuccess()
	if success == nil {
		return nil, fmt.Errorf("shim StartTurn answered neither success nor failure for turn %q", turn)
	}
	return success, nil
}

// PromptAgent delivers a bubble-composer prompt to one agent, through the same
// UpdateAgent path every other agent-addressed input takes.
func (s *sender) PromptAgent(ctx context.Context, agent *conversationv1.AgentId, said *conversationv1.UserSaid) error {
	response, err := s.client.UpdateAgent(ctx, &shimv1.UpdateAgentRequest{
		Target: agent,
		Input: &conversationv1.AgentInput{
			Input: &conversationv1.AgentInput_Prompt{Prompt: said},
		},
	})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return &ShimRefusal{Verb: "UpdateAgent", Arm: updateAgentArm(failure), Detail: failure.GetDetail()}
	}
	return nil
}

// KillTurn interrupts the open turn for an interjection.
func (s *sender) KillTurn(ctx context.Context, turn ids.TurnID, force bool) error {
	response, err := s.client.KillTurn(ctx, &shimv1.KillTurnRequest{
		Turn:  &conversationv1.TurnId{Value: string(turn)},
		Force: force,
	})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return &ShimRefusal{Verb: "KillTurn", Arm: killTurnArm(failure), Detail: failure.GetDetail()}
	}
	return nil
}

// SetModel switches the session's model. The cold threshold is stated as ZERO
// and no remediation is named, which is the FIRST ATTEMPT the contract
// describes: the switch learns what it would cost before anything is paid, and
// the cold answer comes back as a refusal the caller reacts to.
func (s *sender) SetModel(ctx context.Context, model string) error {
	response, err := s.client.SetSessionModel(ctx, &shimv1.SetSessionModelRequest{
		Model: &conversationv1.AgentModel{Name: model},
	})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return &ShimRefusal{Verb: "SetSessionModel", Arm: setModelArm(failure), Detail: failure.GetDetail()}
	}
	return nil
}

// SetPermissionMode switches the session's permission mode.
func (s *sender) SetPermissionMode(ctx context.Context, mode string) error {
	response, err := s.client.SetSessionPermissionMode(ctx, &shimv1.SetSessionPermissionModeRequest{
		PermissionMode: permissionMode(mode),
	})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return &ShimRefusal{Verb: "SetSessionPermissionMode", Arm: setPermissionModeArm(failure), Detail: failure.GetDetail()}
	}
	return nil
}

// startTurnArm names a StartTurn refusal's arm.
func startTurnArm(failure *shimv1.StartTurnFailure) string {
	switch {
	case failure.GetTurnAlreadyOpen() != nil:
		return "turn_already_open"
	case failure.GetNoSession() != nil:
		return ArmShimNoSession
	case failure.GetVendorRefused() != nil:
		return "vendor_refused"
	case failure.GetQueryDead() != nil:
		return "query_dead"
	default:
		return ArmShimUnspecified
	}
}

// setModelArm names a SetSessionModel refusal's arm.
func setModelArm(failure *shimv1.SetSessionModelFailure) string {
	switch {
	case failure.GetCold() != nil:
		return "cold"
	case failure.GetModelNotInCatalog() != nil:
		return "model_not_in_catalog"
	case failure.GetNoSession() != nil:
		return ArmShimNoSession
	case failure.GetVendorRefused() != nil:
		return "vendor_refused"
	default:
		return ArmShimUnspecified
	}
}

// setPermissionModeArm names a SetSessionPermissionMode refusal's arm.
func setPermissionModeArm(failure *shimv1.SetSessionPermissionModeFailure) string {
	switch {
	case failure.GetNoSession() != nil:
		return ArmShimNoSession
	case failure.GetVendorRefused() != nil:
		return "vendor_refused"
	default:
		return ArmShimUnspecified
	}
}

// The compile-time assertions that the fleet answers the queue's two source
// functions and that the watcher answers the queue's watcher slice.
var (
	_ promptqueue.ClientFunc  = (*Fleet)(nil).Sender
	_ promptqueue.WatcherFunc = (*Fleet)(nil).Watcher
	_ promptqueue.Sender      = (*sender)(nil)
	_ promptqueue.Watcher     = (sessionwatcher.Watcher)(nil)
)
