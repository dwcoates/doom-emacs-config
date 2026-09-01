package server

import (
	"context"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/prompthandler"
	"claude-repld/internal/resolve/topbar"
)

// The panel source the prompt handler takes.
//
// A recognized panel command is answered by the DAEMON, from facts the prompt
// handler does not own. Exactly ONE of the six panels has a producer today: the
// topbar resolver assembles the context tree, and `/context` draws it. The
// other five — status, todos, agents, mcp, help — have NO producer in the
// landed daemon (the `/agents` and `/help` views are ruled UNPRODUCED, and the
// status, todos and mcp views have no resolver at all).
//
// This builder lives in `server` rather than in `boot` because the panel source
// is the surface's own view seam; it is a FUNCTION rather than a method because
// prompthandler.New runs BEFORE the surface exists.

// Panels builds the prompt handler's panel source from the topbar resolver. A
// command whose panel has no producer is answered LOUDLY: the handler surfaces
// the failure rather than drawing an empty card.
func Panels(resolver topbar.Resolver, log dlog.Logger) prompthandler.PanelFunc {
	return func(_ context.Context, ws ids.WorkspaceID, command conversationv1.SessionCommand) (*agentreplv1.SubmitPromptCommandPanel, error) {
		if command != conversationv1.SessionCommand_SESSION_COMMAND_CONTEXT {
			log.Error("daemon.server.panels", "a recognized panel command has no producer",
				dlog.Context{"workspace": string(ws), "command": command.String()})
			return nil, fmt.Errorf("server: no producer assembles the panel for %s", command)
		}
		panel, ok := resolver.ContextPanel(ws)
		if !ok {
			log.Warn("daemon.server.panels", "no context panel stands for this workspace",
				dlog.Context{"workspace": string(ws)})
			return nil, fmt.Errorf("server: workspace %q has no context panel to draw", ws)
		}
		log.Debug("daemon.server.panels", "answered the context panel",
			dlog.Context{"workspace": string(ws)})
		return &agentreplv1.SubmitPromptCommandPanel{
			Panel: &agentreplv1.SubmitPromptCommandPanel_Context{Context: panel},
		}, nil
	}
}
