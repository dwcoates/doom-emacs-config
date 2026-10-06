package topbar

import (
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
)

// SetAgentReplSession states agent-repl's session on every strip, present and
// future: the connectivity indicator's dropdown draws it (owner ruling,
// 2026-10-06). It is a daemon-scoped fact like the persistent-wifi standing:
// the resolver holds the one session and every workspace's view carries it.
// internal/agentreplsession calls it when a session begins and once per
// sampling round in which traffic was counted.
func (r *resolver) SetAgentReplSession(session *frontendv1.TopbarAgentReplSession) {
	r.eachStrip("daemon.topbar.agent_repl_session", "the topbar took agent-repl's session on every strip",
		dlog.Context{
			"started_at":     time.UnixMilli(session.GetStartedAtMs()).UTC().Format(time.RFC3339Nano),
			"bytes_received": session.GetBytesReceived(),
			"bytes_sent":     session.GetBytesSent(),
		},
		func() { r.agentReplSession = session },
		func(*wsState) {})
}
