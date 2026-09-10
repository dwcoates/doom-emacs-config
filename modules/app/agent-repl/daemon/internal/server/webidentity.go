package server

import (
	"context"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// THE WEB LINK'S SESSION IDENTITY (landing 15). The webapp is the one runtime
// whose records are written by somebody else: a browser has no durable sink,
// so every diagnostic it raises travels through ClientLog and lands in the
// daemon's webapp.log. Without an identity on that record it could be joined
// to a workspace and no further — never to the session the page was actually
// showing, which is what a fault in the page has to be read against.
//
// It is STATE, so it rides its own topic and is composed from exactly the
// facts the host view's identity is composed from: the fleet's live session
// identity when this daemon operates the session, the durable record's when it
// does not, and the record's vendor conversation. Both may be empty — a
// workspace with no session, a session with no conversation yet — and an empty
// identity is a legitimate answer the page reads as "attributed to the
// workspace alone", never a sentinel.

// publishWebSessionIdentity composes one workspace's web identity and
// publishes it onto the web STATE topic. The topic's own proto.Equal dedupe
// drops a re-render that changed nothing, so it is called from every edge that
// republishes the host view rather than from a second set of edges that would
// drift from it.
func (s *server) publishWebSessionIdentity(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) {
	const op = "daemon.server.publish_web_session_identity"
	identity, ok := s.composeWebSessionIdentity(ctx, log, ws)
	if !ok {
		return
	}
	s.webStateTopic(ws).Publish(identity)
	log.Debug(op, "published the web link's session identity", dlog.Context{
		dlog.KeyAgentReplSessionID: identity.GetAgentReplSessionId(),
	})
}

// composeWebSessionIdentity builds the identity, reporting false when the
// session record could not be read. A record that cannot be read is a gap in
// the daemon's own state and is raised as one; it is never published as an
// empty identity, which the page would read as the honest "no session yet".
func (s *server) composeWebSessionIdentity(
	ctx context.Context,
	log dlog.Logger,
	ws ids.WorkspaceID,
) (*agentreplv1.WebWorkspaceSessionIdentity, bool) {
	const op = "daemon.server.compose_web_session_identity"
	session, hasSession, err := s.deps.DB.Session(ctx, ws)
	if err != nil {
		if endedOnCancel(err) {
			log.Info(op, "the web identity was not composed; the stream's context was cancelled",
				dlog.Context{"stream": "WatchWebWorkspace", "cause": err.Error()})
			return nil, false
		}
		log.Error(op, "could not read the session record", dlog.Context{"cause": err.Error()})
		return nil, false
	}
	identity := &agentreplv1.WebWorkspaceSessionIdentity{}
	if hasSession {
		identity.ClaudeSessionId = session.VendorSessionID
		identity.AgentReplSessionId = session.HostSessionID
	}
	// THE OPERATING DAEMON'S ANSWER WINS. The fleet mints the identity, so
	// while this daemon operates the session its answer is newer than the row
	// the mint has not been written back to yet.
	if facts, live := s.deps.SessionFacts.HostSessionFacts(ws); live && facts.SessionID != "" {
		identity.AgentReplSessionId = facts.SessionID
	}
	return identity, true
}
