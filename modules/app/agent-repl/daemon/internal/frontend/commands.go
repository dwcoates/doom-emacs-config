package frontend

import (
	"context"
	"errors"
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
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
	// WorkspaceMaterialized records the Emacs host's durable materialization
	// acknowledgement and may release the workspace's queued initial prompt.
	WorkspaceMaterialized(ctx context.Context, workspace, requestID string, cmd *frontendv1.WorkspaceMaterializedCmd) error
	// HostActionCompleted records the Emacs host's completion of one durable
	// UI-only inbox action.
	HostActionCompleted(ctx context.Context, workspace, requestID string, cmd *frontendv1.HostActionCompletedCmd) error
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
	// Resync arranges a conversation replay from the given seq, INCLUSIVE (a
	// re-push replaces by uuid, so re-sending the client's last-seen item costs
	// nothing and re-sending one short of it would lose a bubble). The actual
	// start is raised to the newest clear or compaction when there is one — see
	// sessiondrv.Manager.Resync. The server independently re-sends a
	// StateSnapshot to the requesting client; this hook covers the
	// conversation-delta replay the snapshot omits.
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
	// ClientLog mirrors a frontend-side diagnostic line into the daemon's own
	// log. It is EVIDENCE, not a control signal: the handler records it and
	// changes no daemon state.
	ClientLog(ctx context.Context, workspace, requestID string, cmd *frontendv1.ClientLogCmd) error
	// ForceQueueEntry delivers a held prompt now, running the same interject
	// sequence an INTERJECT verdict does (E4).
	ForceQueueEntry(ctx context.Context, workspace, requestID string, cmd *frontendv1.QueueForceCmd) error
	// AcceptQueueEntry confirms a held prompt's classification. View state
	// only: it changes nothing about when the prompt is delivered.
	AcceptQueueEntry(ctx context.Context, workspace, requestID string, cmd *frontendv1.QueueAcceptCmd) error
	// CancelQueueEntry drops a held prompt; it is never delivered.
	CancelQueueEntry(ctx context.Context, workspace, requestID string, cmd *frontendv1.QueueCancelCmd) error
	// PaintAck records a frontend's attestation that it PAINTED the
	// workspace's conversation through the carried seq.
	//
	// The daemon tracks STATE; a frontend decides when that state is
	// RENDERABLE. Nothing on this side of the wire can distinguish a webview
	// that drew the history from one that received it and drew nothing, so
	// the workspace stays on the compromised-route state until a frontend
	// says otherwise. An absent ack is a meaningful, correct outcome.
	PaintAck(ctx context.Context, workspace, requestID string, cmd *frontendv1.PaintAckCmd) error
	// DaemonHealth proves the daemon-global ready boundary.  The returned view
	// is delivered to the requesting connection before its command ack.
	DaemonHealth(ctx context.Context, workspace, requestID string, cmd *frontendv1.DaemonHealthCmd) (*frontendv1.DaemonHealthView, error)
	// SessionHealth proves the named session's live daemon-to-shim path.  A
	// false view is a completed assertion, not a transport error, and is
	// delivered to the requesting connection before its command ack.
	SessionHealth(ctx context.Context, workspace, requestID string, cmd *frontendv1.SessionHealthCmd) (*frontendv1.SessionHealthView, error)
}

// Dispatch routes a FrontendCommand to the handler and returns the CommandAck to
// send back to the requesting client. An unknown/empty command oneof produces a
// loud failing ack (never silently ignored). A handler error becomes a failing
// ack carrying the error text.
//
// This is THE classification point for command failures: every handler error
// in the tree funnels through the one `err != nil` below, which makes it both
// the only place a classifier is needed and the natural one. logf carries the
// classifier's loud unclassified-fallthrough line.
func Dispatch(ctx context.Context, logf dlog.Logf, h CommandHandler, cmd *frontendv1.FrontendCommand) *frontendv1.CommandAck {
	ack, _ := DispatchWithResponse(ctx, logf, h, cmd)
	return ack
}

// DispatchWithResponse is Dispatch plus the command-specific correlated frame
// health commands require.  Only health has a result frame today; all other
// commands return nil so the existing CommandAck contract stays unchanged.
func DispatchWithResponse(ctx context.Context, logf dlog.Logf, h CommandHandler, cmd *frontendv1.FrontendCommand) (*frontendv1.CommandAck, *frontendv1.FrontendFrame) {
	if cmd == nil {
		return failAck(logf, "", fmt.Errorf("frontend: nil command")), nil
	}
	reqID := cmd.GetRequestId()
	ws := cmd.GetWorkspace()

	var err error
	var response *frontendv1.FrontendFrame
	switch c := cmd.GetCommand().(type) {
	case *frontendv1.FrontendCommand_CreateWorkspace:
		// Workspace creation has exactly ONE ingestion point: a
		// workspace_commands_<uuid>.json file in the daemon's inbox. A second
		// wire path would let a caller create a workspace the durable inbox
		// never recorded, so the arm is rejected here rather than routed.
		err = fmt.Errorf("frontend: createWorkspace is not accepted over the wire; write a workspace_commands_<uuid>.json command file")
	case *frontendv1.FrontendCommand_WorkspaceMaterialized:
		err = h.WorkspaceMaterialized(ctx, ws, reqID, c.WorkspaceMaterialized)
	case *frontendv1.FrontendCommand_HostActionCompleted:
		err = h.HostActionCompleted(ctx, ws, reqID, c.HostActionCompleted)
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
	case *frontendv1.FrontendCommand_ClientLog:
		err = h.ClientLog(ctx, ws, reqID, c.ClientLog)
	case *frontendv1.FrontendCommand_QueueForce:
		err = h.ForceQueueEntry(ctx, ws, reqID, c.QueueForce)
	case *frontendv1.FrontendCommand_QueueAccept:
		err = h.AcceptQueueEntry(ctx, ws, reqID, c.QueueAccept)
	case *frontendv1.FrontendCommand_QueueCancel:
		err = h.CancelQueueEntry(ctx, ws, reqID, c.QueueCancel)
	case *frontendv1.FrontendCommand_PaintAck:
		err = h.PaintAck(ctx, ws, reqID, c.PaintAck)
	case *frontendv1.FrontendCommand_DaemonHealth:
		var view *frontendv1.DaemonHealthView
		view, err = h.DaemonHealth(ctx, ws, reqID, c.DaemonHealth)
		if view != nil {
			response = DaemonHealthFrame(view)
		}
	case *frontendv1.FrontendCommand_SessionHealth:
		var view *frontendv1.SessionHealthView
		view, err = h.SessionHealth(ctx, ws, reqID, c.SessionHealth)
		if view != nil {
			response = SessionHealthFrame(view)
		}
	default:
		// Unknown/empty command oneof: fail loudly, never silently.
		return failAck(logf, reqID, fmt.Errorf("frontend: unknown command (workspace=%q): the command oneof was empty or unrecognized", ws)), nil
	}
	if err != nil {
		// THE ONE NON-FAILURE REFUSAL. An interrupt confirm challenge is a
		// command the daemon UNDERSTOOD and deliberately did not perform, so it
		// takes the challenge arm and leaves `failure` and `error` unset — see
		// InterruptConfirmRequired. Everything else still funnels into the one
		// classifier below, unchanged.
		var challenge *InterruptConfirmRequired
		if errors.As(err, &challenge) {
			if logf != nil {
				logf("frontend: interrupt confirm required request_id=%s workspace=%s live_tasks=%d — refusing until the client resends with confirm_agents",
					reqID, ws, challenge.LiveTasks)
			}
			return &frontendv1.CommandAck{
				RequestId:                reqID,
				Ok:                       false,
				InterruptConfirmRequired: &frontendv1.InterruptConfirmRequired{LiveTasks: challenge.LiveTasks},
			}, nil
		}
		return failAck(logf, reqID, err), nil
	}
	return &frontendv1.CommandAck{RequestId: reqID, Ok: true}, response
}

// InterruptConfirmRequired is the interrupt gate's CHALLENGE, carried on the
// handler's error channel because that is the only channel a CommandHandler
// method has — but it is NOT a failure.
//
// The distinction is the whole point of the proto's separate arm: the command
// was understood and deliberately not performed, because no turn was live and
// stopping live subagents deserves an explicit yes. So the ack it becomes
// carries ok=false with `failure` and `error` UNSET, and a client answers it by
// resending InterruptCmd{confirm_agents: true} rather than by rendering a
// failure card.
//
// It is a distinct TYPE rather than a sentinel value so the payload — what the
// stop would actually interrupt — travels with it and the client can ask a
// concrete question instead of a bare are-you-sure.
type InterruptConfirmRequired struct {
	// LiveTasks is how many subagent tasks are working right now.
	LiveTasks int64
}

func (e *InterruptConfirmRequired) Error() string {
	return fmt.Sprintf("interrupt confirmation required: %d subagent task(s) are working", e.LiveTasks)
}

// failAck builds a refusal ack carrying BOTH the classified failure and the
// legacy error text. The text stays until both frontends read the classified
// field; dropping it now would blank the one surface (Emacs's echo) that
// renders a refusal at all today.
func failAck(logf dlog.Logf, requestID string, err error) *frontendv1.CommandAck {
	return &frontendv1.CommandAck{
		RequestId: requestID,
		Ok:        false,
		Error:     err.Error(),
		Failure:   errclass.Command(logf, err),
	}
}
