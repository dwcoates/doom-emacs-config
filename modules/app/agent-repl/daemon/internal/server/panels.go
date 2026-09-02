package server

import (
	"context"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/prompthandler"
	"claude-repld/internal/resolve/topbar"
)

// The panel source the prompt handler takes.
//
// A recognized panel command is answered by the DAEMON, from facts the prompt
// handler does not own. TWO of the six panels have a producer: the topbar
// resolver assembles the context tree, which `/context` draws, and it also
// holds the session facts `/status` splices. `/todos` and `/mcp` have no
// resolver at all this wave, and the `/agents` and `/help` views are ruled
// UNPRODUCED — they answer as `command_refused` before recognition ever
// reaches a panel.
//
// This builder lives in `server` rather than in `boot` because the panel source
// is the surface's own view seam; it is a FUNCTION rather than a method because
// prompthandler.New runs BEFORE the surface exists.

// VersionFunc reads the daemon's build stamp, which is the one row the /status
// panel states about the daemon itself.
type VersionFunc func() (string, error)

// Panels builds the prompt handler's panel source from the topbar resolver and
// the daemon's build stamp. A command whose panel has no producer is answered
// LOUDLY: the handler surfaces the failure rather than drawing an empty card.
func Panels(resolver topbar.Resolver, version VersionFunc, log dlog.Logger) prompthandler.PanelFunc {
	return func(_ context.Context, ws ids.WorkspaceID, command conversationv1.SessionCommand) (*agentreplv1.SubmitPromptCommandPanel, error) {
		switch command {
		case conversationv1.SessionCommand_SESSION_COMMAND_CONTEXT:
			return contextPanel(resolver, ws, log)
		case conversationv1.SessionCommand_SESSION_COMMAND_STATUS:
			return statusPanel(resolver, version, ws, log)
		default:
			log.Error("daemon.server.panels", "a recognized panel command has no producer",
				dlog.Context{"workspace": string(ws), "command": command.String()})
			return nil, fmt.Errorf("server: no producer assembles the panel for %s", command)
		}
	}
}

// contextPanel draws the context-fill tree the topbar resolver assembles.
func contextPanel(resolver topbar.Resolver, ws ids.WorkspaceID, log dlog.Logger) (*agentreplv1.SubmitPromptCommandPanel, error) {
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

// statusPanel draws the version row plus the spliced session facts.
//
// /status DEGRADES BY DESIGN (project lead): the vendor handshake is deferred,
// so there is no cwd, auth, plugin or memory fact to state and the panel is
// these rows and no others. A thin panel is the settled consequence, not a bug.
// A row whose fact the session has not stated is OMITTED — StatusPanelRow.value
// is never empty — and a build stamp that cannot be read fails the panel
// LOUDLY rather than drawing a version the daemon does not know. The Version
// row obeys the same omission rule as the rest: a checkout the deploy chain
// never stamped has NO version to state, and an empty-valued row would state
// one anyway.
func statusPanel(resolver topbar.Resolver, version VersionFunc, ws ids.WorkspaceID, log dlog.Logger) (*agentreplv1.SubmitPromptCommandPanel, error) {
	stamp, err := version()
	if err != nil {
		log.Error("daemon.server.panels", "the daemon's build stamp could not be read for the status panel",
			dlog.Context{"workspace": string(ws), "cause": err.Error()})
		return nil, fmt.Errorf("server: read the daemon's version for the status panel: %w", err)
	}
	facts, ok := resolver.StatusFacts(ws)
	if !ok {
		log.Warn("daemon.server.panels", "no session facts stand for this workspace",
			dlog.Context{"workspace": string(ws)})
		return nil, fmt.Errorf("server: workspace %q has no session facts to draw a status panel from", ws)
	}
	view := &frontendv1.StatusPanelView{}
	for _, row := range []struct{ label, value string }{
		{"Version", stamp},
		{"Account", facts.Account},
		{"Model", facts.Model},
		{"Permission mode", facts.PermissionMode},
	} {
		if row.value == "" {
			continue
		}
		view.Rows = append(view.Rows, &frontendv1.StatusPanelRow{Label: row.label, Value: row.value})
	}
	log.Debug("daemon.server.panels", "answered the status panel",
		dlog.Context{"workspace": string(ws), "rows": len(view.Rows)})
	return &agentreplv1.SubmitPromptCommandPanel{
		Panel: &agentreplv1.SubmitPromptCommandPanel_Status{Status: view},
	}, nil
}
