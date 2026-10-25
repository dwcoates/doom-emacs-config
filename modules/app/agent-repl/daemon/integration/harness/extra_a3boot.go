package harness

import (
	"agent-repld/internal/h2cclient"
	"testing"

	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
)

// DialAt builds a Connect client against an arbitrary address, for a daemon
// the ordinary Client() cannot reach: a joining successor defers its own
// client wiring (Daemon.dial) until it owns every workspace, but it still
// binds and serves its own listener the moment it starts, and a test that
// wants to prove THAT needs to dial it directly by the address it reported.
func DialAt(t testing.TB, addr string) agentreplv1connect.AgentReplClient {
	t.Helper()
	httpClient := h2cclient.Client()
	return agentreplv1connect.NewAgentReplClient(httpClient, "http://"+addr)
}
