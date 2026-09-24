package server

import (
	"context"
	"errors"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"connectrpc.com/connect"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// receiveWebIdentity reads the web stream up to its next `session_identity`
// push.
func receiveWebIdentity(
	t *testing.T,
	stream *connect.ServerStreamForClient[agentreplv1.WatchWebWorkspaceResponse],
) *agentreplv1.WebWorkspaceSessionIdentity {
	t.Helper()
	for stream.Receive() {
		if identity := stream.Msg().GetSessionIdentity(); identity != nil {
			return identity
		}
	}
	t.Fatalf("the web stream ended before an identity arrived: %v", stream.Err())
	return nil
}

// openWebStream opens the workspace's web link stream.
func openWebStream(
	t *testing.T,
	h *harness,
	ctx context.Context,
) *connect.ServerStreamForClient[agentreplv1.WatchWebWorkspaceResponse] {
	t.Helper()
	stream, err := h.Client.WatchWebWorkspace(ctx,
		connect.NewRequest(&agentreplv1.WatchWebWorkspaceRequest{Workspace: ref(), WebappBuild: "webapp-test"}))
	if err != nil {
		t.Fatalf("open the web stream: %v", err)
	}
	return stream
}

// TestTheWebStreamOpensWithTheOperatedSessionsIdentity pins the opening push:
// the page binds its log context from it, so a fresh stream that carried none
// would leave every record of that page's life unattributed.
func TestTheWebStreamOpensWithTheOperatedSessionsIdentity(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{
		testWorkspaceID: {HostSessionID: "sess-row", VendorSessionID: "claude-1"},
	}
	h.Facts.facts[testWorkspaceID] = HostFacts{SessionID: "sess-live", Generation: "1"}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	identity := receiveWebIdentity(t, openWebStream(t, h, ctx))

	// Assert.
	if identity.GetAgentReplSessionId() != "sess-live" {
		t.Fatalf("agent_repl_session_id = %q, want the operating fleet's",
			identity.GetAgentReplSessionId())
	}
}

// TestTheWebIdentityCarriesTheVendorConversation pins that the page also
// learns the Claude conversation, which is the identity a vendor-side fault is
// read against.
func TestTheWebIdentityCarriesTheVendorConversation(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{
		testWorkspaceID: {HostSessionID: "sess-row", VendorSessionID: "claude-1"},
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	identity := receiveWebIdentity(t, openWebStream(t, h, ctx))

	// Assert.
	if identity.GetClaudeSessionId() != "claude-1" {
		t.Fatalf("claude_session_id = %q, want the record's vendor conversation",
			identity.GetClaudeSessionId())
	}
}

// TestTheWebIdentityFallsBackToTheDurableRecord pins the identity a workspace
// this daemon operates no session for still carries: the row is the durable
// truth, and a page attached to it correlates on the same key.
func TestTheWebIdentityFallsBackToTheDurableRecord(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{
		testWorkspaceID: {HostSessionID: "sess-row"},
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	identity := receiveWebIdentity(t, openWebStream(t, h, ctx))

	// Assert.
	if identity.GetAgentReplSessionId() != "sess-row" {
		t.Fatalf("agent_repl_session_id = %q, want the durable record's",
			identity.GetAgentReplSessionId())
	}
}

// TestTheWebIdentityIsEmptyBeforeAnySessionExists pins that a workspace with
// no session yet is stated as an EMPTY identity rather than withheld: the page
// reads absence as "attributed to the workspace alone" and must not stall
// waiting for a push that would never come.
func TestTheWebIdentityIsEmptyBeforeAnySessionExists(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	identity := receiveWebIdentity(t, openWebStream(t, h, ctx))

	// Assert.
	if identity.GetAgentReplSessionId() != "" {
		t.Fatalf("agent_repl_session_id = %q, want empty", identity.GetAgentReplSessionId())
	}
}

// TestTheWebIdentityIsRepublishedWhenTheSessionRotates pins the rebinding
// edge: a restart mints a new identity, and a page that kept the boot-time one
// would file the new session's records under the retired session.
func TestTheWebIdentityIsRepublishedWhenTheSessionRotates(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{
		testWorkspaceID: {HostSessionID: "sess-1"},
	}
	h.Facts.facts[testWorkspaceID] = HostFacts{SessionID: "sess-1", Generation: "1"}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openWebStream(t, h, ctx)
	if got := receiveWebIdentity(t, stream).GetAgentReplSessionId(); got != "sess-1" {
		t.Fatalf("opening agent_repl_session_id = %q, want sess-1", got)
	}

	// Act.
	h.Facts.facts[testWorkspaceID] = HostFacts{SessionID: "sess-2", Generation: "1"}
	h.Server.Relay().PublishHostWorkspace(testWorkspaceID)

	// Assert.
	if got := receiveWebIdentity(t, stream).GetAgentReplSessionId(); got != "sess-2" {
		t.Fatalf("republished agent_repl_session_id = %q, want sess-2", got)
	}
}

// TestTheWebIdentityIsWithheldWhenTheSessionRecordCannotBeRead pins that an
// unreadable record is a gap raised as one, never published as the empty
// identity a page would read as the honest "no session yet".
func TestTheWebIdentityIsWithheldWhenTheSessionRecordCannotBeRead(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessionErr = errors.New("the database is gone")
	log := &recordingLogger{}

	// Act.
	identity, ok := h.Server.(*server).composeWebSessionIdentity(
		context.Background(), log, testWorkspaceID)

	// Assert.
	if ok || identity != nil {
		t.Fatalf("composed %v, ok = %v; want the identity withheld", identity, ok)
	}
}
