package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/workspace"
)

// ListWorkspaceTranscripts answers every vendor conversation filed under a
// workspace's own directory, so a person can choose which one it runs.
//
// PULL, NOT PUSH: the answer is a list read once while choosing, and a
// transcript set changes only when a conversation is started or cleared.
func (s *server) ListWorkspaceTranscripts(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListWorkspaceTranscriptsRequest],
) (*connect.Response[agentreplv1.ListWorkspaceTranscriptsResponse], error) {
	const rpc = "ListWorkspaceTranscripts"
	resp := &agentreplv1.ListWorkspaceTranscriptsResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	transcripts, err := s.deps.Verbs.ListTranscripts(ctx, subject.Record.ID)
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.ListWorkspaceTranscriptsResponse_Success{
		Success: &agentreplv1.ListWorkspaceTranscriptsSuccess{Transcripts: transcripts},
	}
	return connect.NewResponse(resp), nil
}

// BindWorkspaceSession points a workspace at a different vendor conversation
// in its own directory.
func (s *server) BindWorkspaceSession(
	ctx context.Context,
	req *connect.Request[agentreplv1.BindWorkspaceSessionRequest],
) (*connect.Response[agentreplv1.BindWorkspaceSessionResponse], error) {
	const rpc = "BindWorkspaceSession"
	resp := &agentreplv1.BindWorkspaceSessionResponse{}
	if cerr := validateBindWorkspaceSessionRequest(req.Msg); cerr != nil {
		return answer(resp, cerr)
	}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	// A client that minted an op_id also gets the bind's STAGES, pushed onto
	// WatchDaemon keyed on it while this rpc is still in flight. A client that
	// minted none gets no pushes at all, exactly as the contract states. The
	// terminal outcome stays on this answer either way.
	var progress workspace.BindProgress
	if opID := req.Msg.GetOpId(); opID != "" {
		progress = bindProgressReporter{server: s, opID: opID}
	}
	if err := s.deps.Verbs.BindSession(ctx, subject.Record.ID, req.Msg.GetVendorSessionId(), progress); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	// The bind swapped the session behind the workspace; the host view is
	// recomposed from what the swap left behind, as an open's is.
	s.PublishHostWorkspace(ctx, subject.Record.ID)
	resp.Result = &agentreplv1.BindWorkspaceSessionResponse_Success{
		Success: &agentreplv1.BindWorkspaceSessionSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// bindProgressReporter relays a bind's stage transitions onto the
// mutation-progress channel, keyed on the op_id the request supplied. It
// satisfies workspace.BindProgress, keeping the verb itself proto-free.
//
// IT REPORTS THROUGH THE OPEN ARM, because a bind IS a session bring-up: it
// ends the current session and starts one on the chosen conversation through
// the ordinary resume, which is the very wait the open stage's
// `starting_session` arm names. `workspace_mutation_progress.proto` carries no bind arm of
// its own; inventing one here would be a contract decision this layer does not
// get to make.
//
// ONLY THE STAGE THAT HAS AN HONEST SPELLING IS PUSHED. A bind's fresh read,
// its stop and its record write are fast and share no stage with an open, and
// relaying them under a name that means something else — `CLEARING_CLOSED` for
// a session being stopped — would tell the user a thing that is not happening.
// They are recorded instead, so the wait is still accounted for in the log.
type bindProgressReporter struct {
	server *server
	opID   string
}

func (r bindProgressReporter) Stage(stage workspace.BindStage) {
	switch stage {
	case workspace.BindStageStartingSession:
		r.server.MutationProgress(&agentreplv1.WorkspaceMutationProgress{
			OpId: r.opID,
			Event: &agentreplv1.WorkspaceMutationProgress_Open{
				Open: &agentreplv1.WorkspaceOpenProgress{
					EnteredStage: &agentreplv1.WorkspaceOpenStage{
						Stage: &agentreplv1.WorkspaceOpenStage_StartingSession{
							StartingSession: &agentreplv1.WorkspaceOpenStageStartingSession{},
						},
					},
				},
			},
		})
	case workspace.BindStageReadingTranscripts, workspace.BindStageStoppingSession, workspace.BindStageRecordingBinding:
		r.server.log.Debug("daemon.server.bind_workspace_session", "a bind stage with no wire spelling was not relayed",
			dlog.Context{"op_id": r.opID, "stage": int(stage)})
	default:
		// An unmapped stage is a bug in this switch, not a client condition —
		// but a bind's progress relay must never take the bind down, so it is
		// surfaced loudly and the bind proceeds unaffected.
		r.server.log.Error("daemon.server.bind_workspace_session", "an unmapped bind stage was reported; it was not relayed",
			dlog.Context{"op_id": r.opID, "stage": int(stage)})
	}
}
