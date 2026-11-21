package server

import (
	"context"
	"fmt"
	"strings"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/holdfold"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/sessioncommand"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// CompactWorkspace submits the vendor's compaction to a workspace's prompt
// queue, deferred behind its running turn: the one act on a conversation an
// agent may perform besides prompting it.
func (s *server) CompactWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.CompactWorkspaceRequest],
) (*connect.Response[agentreplv1.CompactWorkspaceResponse], error) {
	const rpc = "CompactWorkspace"
	if err := validateCompactWorkspaceRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.CompactWorkspaceResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	outcome, refused, err := s.submitCompaction(ctx, subject, req.Msg.GetInstructions())
	if err != nil {
		return nil, fail(subject.Log, rpc, err)
	}
	if refused != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, refused, nil))
	}
	asker, by := askerOf(req.Msg.GetRequester())
	subject.Log.Info("daemon.server.compact_workspace", "submitted a compaction to the workspace's prompt queue", dlog.Context{
		"requested_by": asker.String(), "requester": string(by), "instructions": req.Msg.Instructions != nil, "turn": string(outcome.Turn)})
	resp.Result = &agentreplv1.CompactWorkspaceResponse_Success{Success: &agentreplv1.CompactWorkspaceSuccess{}}
	return connect.NewResponse(resp), nil
}

// submitCompaction submits the compaction of the resolved workspace to its
// prompt queue, deferred behind its running turn. A closed workspace or the
// queue's refusal answers REFUSED, which the caller renders on its own rpc's
// error arms; any other failure answers err.
func (s *server) submitCompaction(
	ctx context.Context,
	subject resolved,
	instructions *agentreplv1.CompactWorkspaceInstructions,
) (outcome prompthandler.Outcome, refused error, err error) {
	if subject.Record.Closed {
		return prompthandler.Outcome{}, &workspace.Refusal{Arm: "closed", Reason: "a closed workspace runs no session to compact"}, nil
	}
	key, err := prompthandler.MintKey("workspace-compact")
	if err != nil {
		return prompthandler.Outcome{}, nil, err
	}
	outcome, err = s.deps.Prompts.Submit(ctx, subject.Record.ID, feed.SaidText(CompactionPrompt(instructions)), key,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT, nil, wsm.DeliveryDeferred, nil)
	if err != nil {
		return prompthandler.Outcome{}, err, nil
	}
	return outcome, nil, nil
}

// CompactionPrompt is the compaction a CompactWorkspace request submits: the
// vendor's command, followed by the summary's instructions when it names any.
func CompactionPrompt(instructions *agentreplv1.CompactWorkspaceInstructions) string {
	compact := sessioncommand.Literal(conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)
	if instructions == nil {
		return compact
	}
	return compact + " " + strings.TrimSpace(instructions.GetText())
}

// agentSlashCommand answers the slash command an agent-sent prompt begins
// with, false for any other prompt. An agent never sends a slash command as a
// prompt: the acts it may perform on a conversation are the ones the service
// names (CompactWorkspace), so clearing stays the user's.
func agentSlashCommand(origin conversationv1.PromptOrigin, said *conversationv1.UserSaid) (string, bool) {
	if origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT {
		return "", false
	}
	parsed := sessioncommand.Parse(holdfold.SaidText(said))
	return parsed.Name, parsed.Slash
}

// agentSlashRefusal is SubmitPrompt's slash_command_from_agent refusal for
// COMMAND.
func (s *server) agentSlashRefusal(command string) refusal {
	return submitRefusal(s.fill(refusal{
		Arm:    "slash_command_from_agent",
		Reason: fmt.Sprintf("an agent-sent prompt began with the slash command %s; an agent compacts through CompactWorkspace and never clears", command),
		Fields: map[string]any{"command": command},
	}))
}
