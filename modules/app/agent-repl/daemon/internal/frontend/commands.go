package frontend

import (
	"context"
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// CommandHandler is the injected dispatch surface for inbound FrontendCommands.
// It is defined narrowly HERE (the frontend package owns its own dependency
// contract); the stitch phase binds a concrete implementation backed by the
// shim client, the SSM, and the merge port. Each method carries the originating
// workspace and the command's request_id for correlation and logging.
//
// Every method returns an error; a non-nil error becomes a CommandAck with
// ok=false and the error text (a loud Nack-style failure), never a silent drop.
type CommandHandler interface {
	// SubmitPrompt forwards a prompt to the workspace's session.
	SubmitPrompt(ctx context.Context, workspace, requestID string, cmd *frontendv1.SubmitPromptCmd) error
	// Interrupt interrupts the in-flight turn (hard = SDK interrupt).
	Interrupt(ctx context.Context, workspace, requestID string, cmd *frontendv1.InterruptCmd) error
	// AnswerPermission answers a pending canUseTool permission request.
	AnswerPermission(ctx context.Context, workspace, requestID string, cmd *frontendv1.PermissionAnswerCmd) error
	// MergeWorkspace runs (or resumes, on conflict_resolved_continue) a merge.
	MergeWorkspace(ctx context.Context, workspace, requestID string, cmd *frontendv1.MergeWorkspaceCmd) error
	// CloseWorkspace closes/discards a workspace.
	CloseWorkspace(ctx context.Context, workspace, requestID string, cmd *frontendv1.CloseWorkspaceCmd) error
	// OpenWorkspace (re)opens a workspace.
	OpenWorkspace(ctx context.Context, workspace, requestID string, cmd *frontendv1.OpenWorkspaceCmd) error
	// Resync arranges a conversation replay from the given exclusive seq. The
	// server independently re-sends a StateSnapshot to the requesting client;
	// this hook covers the conversation-delta replay the snapshot omits.
	Resync(ctx context.Context, workspace, requestID string, cmd *frontendv1.ResyncCmd) error
	// CreateSession brings up a session for the command's cwd (the UDS
	// replacement for POST /sessions). The daemon delivers the resulting
	// session identity via a pushed SessionView; the ack carries only ok/error.
	CreateSession(ctx context.Context, workspace, requestID string, cmd *frontendv1.CreateSessionCmd) error
	// DeleteSession marks a session terminal and stops its shim (the UDS
	// replacement for DELETE /sessions/{id}).
	DeleteSession(ctx context.Context, workspace, requestID string, cmd *frontendv1.DeleteSessionCmd) error
	// Shutdown begins the daemon's graceful teardown (the UDS replacement for
	// POST /shutdown), the same path SIGTERM triggers.
	Shutdown(ctx context.Context, workspace, requestID string, cmd *frontendv1.ShutdownCmd) error
}

// Dispatch routes a FrontendCommand to the handler and returns the CommandAck to
// send back to the requesting client. An unknown/empty command oneof produces a
// loud failing ack (never silently ignored). A handler error becomes a failing
// ack carrying the error text.
func Dispatch(ctx context.Context, h CommandHandler, cmd *frontendv1.FrontendCommand) *frontendv1.CommandAck {
	if cmd == nil {
		return failAck("", "frontend: nil command")
	}
	reqID := cmd.GetRequestId()
	ws := cmd.GetWorkspace()

	var err error
	switch c := cmd.GetCommand().(type) {
	case *frontendv1.FrontendCommand_SubmitPrompt:
		err = h.SubmitPrompt(ctx, ws, reqID, c.SubmitPrompt)
	case *frontendv1.FrontendCommand_Interrupt:
		err = h.Interrupt(ctx, ws, reqID, c.Interrupt)
	case *frontendv1.FrontendCommand_PermissionAnswer:
		err = h.AnswerPermission(ctx, ws, reqID, c.PermissionAnswer)
	case *frontendv1.FrontendCommand_MergeWorkspace:
		err = h.MergeWorkspace(ctx, ws, reqID, c.MergeWorkspace)
	case *frontendv1.FrontendCommand_CloseWorkspace:
		err = h.CloseWorkspace(ctx, ws, reqID, c.CloseWorkspace)
	case *frontendv1.FrontendCommand_OpenWorkspace:
		err = h.OpenWorkspace(ctx, ws, reqID, c.OpenWorkspace)
	case *frontendv1.FrontendCommand_Resync:
		err = h.Resync(ctx, ws, reqID, c.Resync)
	case *frontendv1.FrontendCommand_CreateSession:
		err = h.CreateSession(ctx, ws, reqID, c.CreateSession)
	case *frontendv1.FrontendCommand_DeleteSession:
		err = h.DeleteSession(ctx, ws, reqID, c.DeleteSession)
	case *frontendv1.FrontendCommand_Shutdown:
		err = h.Shutdown(ctx, ws, reqID, c.Shutdown)
	default:
		// Unknown/empty command oneof: fail loudly, never silently.
		return failAck(reqID, fmt.Sprintf("frontend: unknown command (workspace=%q): the command oneof was empty or unrecognized", ws))
	}
	if err != nil {
		return failAck(reqID, err.Error())
	}
	return &frontendv1.CommandAck{RequestId: reqID, Ok: true}
}

func failAck(requestID, msg string) *frontendv1.CommandAck {
	return &frontendv1.CommandAck{RequestId: requestID, Ok: false, Error: msg}
}
