package harness

import (
	"context"
	"crypto/tls"
	"net"
	"net/http"
	"testing"

	"agentrepl/proto/agentrepl/v1/agentreplv1connect"

	"golang.org/x/net/http2"
)

// DialAt builds a Connect client against an arbitrary address, for a daemon
// the ordinary Client() cannot reach: a joining successor defers its own
// client wiring (Daemon.dial) until it owns every workspace, but it still
// binds and serves its own listener the moment it starts, and a test that
// wants to prove THAT needs to dial it directly by the address it reported.
func DialAt(t *testing.T, addr string) agentreplv1connect.AgentReplClient {
	t.Helper()
	httpClient := &http.Client{
		Transport: &http2.Transport{
			AllowHTTP: true,
			DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
				var dialer net.Dialer
				return dialer.DialContext(ctx, network, addr)
			},
		},
	}
	return agentreplv1connect.NewAgentReplClient(httpClient, "http://"+addr)
}
