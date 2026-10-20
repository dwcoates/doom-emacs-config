package server

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
)

// A PROMPT'S ECHO IS TIMED BY THE PAGE THAT DRAWS IT (prompt_echo in
// proto/vocab/latencies.json). The editor stamps the instant the person
// submitted (SubmitPromptRequest.submitted_at_unix_ms); the daemon hands it
// back to the workspace's page once the submission drew its prompt row, and the
// page measures from that instant to its own draw of the row. The daemon
// pairs nothing and measures nothing here: it only carries the start instant
// through the channel the interaction already travels.

// promptEchoOf answers the echo a submission's page is handed, and whether
// it is handed one: only a send the editor stamped, accepted, whose prompt row
// was drawn at submit. A delivered prompt and one folded into the running turn
// are drawn by the acceptance mirror before Submit answers; a held prompt draws
// when it is delivered later, and a command mints no prompt row.
func promptEchoOf(req *agentreplv1.SubmitPromptRequest, outcome prompthandler.Outcome) (*agentreplv1.WebWorkspacePromptEcho, bool) {
	if req.SubmittedAtUnixMs == nil ||
		outcome.Recognition != prompthandler.RecognizedNone ||
		outcome.Turn == "" ||
		outcome.Disposition.RefusedArm != "" ||
		outcome.Disposition.Parked() {
		return nil, false
	}
	return &agentreplv1.WebWorkspacePromptEcho{
		Turn:              &conversationv1.TurnId{Value: string(outcome.Turn)},
		SubmittedAtUnixMs: req.GetSubmittedAtUnixMs(),
	}, true
}

// pushPromptEcho hands ws's page ECHO. With no page listening it is gone: no
// page saw the echo happen.
func (s *server) pushPromptEcho(log dlog.Logger, ws ids.WorkspaceID, echo *agentreplv1.WebWorkspacePromptEcho) {
	events := s.webEventStream(ws)
	listening := events.Subscribers()
	events.Emit(&agentreplv1.WatchWebWorkspaceResponse{
		Push: &agentreplv1.WatchWebWorkspaceResponse_PromptEcho{PromptEcho: echo},
	})
	log.Debug("daemon.server.prompt_echo", "handed the page a prompt's submit instant to time its echo",
		dlog.Context{"turn": echo.GetTurn().GetValue(), "submitted_at_unix_ms": echo.GetSubmittedAtUnixMs(), "pages": listening})
}
