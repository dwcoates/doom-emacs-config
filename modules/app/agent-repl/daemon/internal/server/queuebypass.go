package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/workspace"
)

// UpdateQueueBypassMode takes or releases a workspace's queue bypass mode.
func (s *server) UpdateQueueBypassMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateQueueBypassModeRequest],
) (*connect.Response[agentreplv1.UpdateQueueBypassModeResponse], error) {
	const rpc = "UpdateQueueBypassMode"
	if err := validateUpdateQueueBypassModeRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateQueueBypassModeResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	on, err := s.deps.Verbs.UpdateQueueBypassMode(ctx, subject.Record.ID, queueBypassChange(req.Msg), workspace.QueueBypassContextDefault)
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	// The switch swapped the conversation behind the workspace; the host view
	// is recomposed from what the swap left behind, as a bind's is.
	s.PublishHostWorkspace(ctx, subject.Record.ID)
	resp.Result = &agentreplv1.UpdateQueueBypassModeResponse_Success{
		Success: &agentreplv1.UpdateQueueBypassModeSuccess{On: on},
	}
	return connect.NewResponse(resp), nil
}

// queueBypassChange is the change a validated request's action arm asks.
func queueBypassChange(req *agentreplv1.UpdateQueueBypassModeRequest) workspace.QueueBypassChange {
	switch req.GetAction().(type) {
	case *agentreplv1.UpdateQueueBypassModeRequest_On:
		return workspace.QueueBypassOn
	case *agentreplv1.UpdateQueueBypassModeRequest_Off:
		return workspace.QueueBypassOff
	case *agentreplv1.UpdateQueueBypassModeRequest_Toggle:
		return workspace.QueueBypassToggle
	}
	// validateUpdateQueueBypassModeRequest refused every other arm, so one
	// reaching here is a contract this build does not know.
	panic(fmt.Sprintf("server: undeclared UpdateQueueBypassModeRequest.action arm %T", req.GetAction()))
}

// validateUpdateQueueBypassModeRequest is UpdateQueueBypassModeRequest's
// validation: a workspace and one action arm.
func validateUpdateQueueBypassModeRequest(req *agentreplv1.UpdateQueueBypassModeRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	switch req.GetAction().(type) {
	case *agentreplv1.UpdateQueueBypassModeRequest_On,
		*agentreplv1.UpdateQueueBypassModeRequest_Off,
		*agentreplv1.UpdateQueueBypassModeRequest_Toggle:
		return nil
	case nil:
		return invalid("action", "a queue-bypass action arm is required")
	}
	return invalid("action", "the queue-bypass action is not one this contract defines")
}

func (s *requestLoggingServer) UpdateQueueBypassMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateQueueBypassModeRequest],
) (*connect.Response[agentreplv1.UpdateQueueBypassModeResponse], error) {
	return serveUnary(s, ctx, "UpdateQueueBypassMode", req, s.server.UpdateQueueBypassMode)
}

// ArmRequesterBypassingQueue refuses a one-shot, an async merge or a one-shot
// mark asked by a workspace that holds its repository's queue bypass mode.
const ArmRequesterBypassingQueue = "requester_bypassing_queue"

// bypassReminder is what a refused dispatch reminds its workspace to do.
const bypassReminder = "this workspace holds its repository's queue bypass mode, which hard-stops the merge queue, " +
	"and a one-shot workspace or an async merge lands through that queue. Do the work in this workspace, " +
	"or create a child that is no one-shot and land its branch on the main branch yourself with land-direct.sh"

// refuseWhileBypassing answers the refusal for WS dispatching WHAT while WS
// holds its repository's queue bypass mode (owner ruling, 2026-11-10: a
// workspace in the mode lands everything directly, so nothing it dispatches
// may land through the stopped queue), and false when WS holds no such mode.
// A failed read of the standing is the rpc's failure.
func (s *server) refuseWhileBypassing(ctx context.Context, log dlog.Logger, rpc string, resp proto.Message, ws ids.WorkspaceID, what string) (*connect.Error, bool, error) {
	standing, err := s.deps.DB.QueueBypass(ctx, ws)
	if err != nil {
		log.Error(rpc, "could not read whether the requester holds the queue bypass mode; nothing was dispatched", dlog.Context{"workspace": string(ws), "cause": err.Error()})
		return nil, false, fmt.Errorf("%s: read the queue bypass standing of %s: %w", rpc, ws, err)
	}
	if !standing.Holds {
		return nil, false, nil
	}
	log.Info(rpc, "refused a dispatch from a workspace holding the queue bypass mode", dlog.Context{"workspace": string(ws), "dispatch": what})
	return s.refuse(log, rpc, resp, s.fill(refusal{Arm: ArmRequesterBypassingQueue,
		Reason: what + " refused: " + bypassReminder, Fields: map[string]any{"detail": what + " refused: " + bypassReminder}})), true, nil
}
