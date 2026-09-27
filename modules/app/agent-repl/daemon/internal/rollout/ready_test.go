package rollout

import (
	"context"
	"net"
	"net/http"
	"testing"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
)

// healthOnly answers DaemonHealth and nothing else.
type healthOnly struct {
	agentreplv1connect.UnimplementedAgentReplHandler
	answer error
}

func (h healthOnly) DaemonHealth(context.Context, *connect.Request[agentreplv1.DaemonHealthRequest]) (*connect.Response[agentreplv1.DaemonHealthResponse], error) {
	if h.answer != nil {
		return nil, h.answer
	}
	return connect.NewResponse(&agentreplv1.DaemonHealthResponse{}), nil
}

// serveHealth serves svc over h2c on a loopback listener and answers its
// address; the server is closed at the end of the test.
func serveHealth(t *testing.T, svc agentreplv1connect.AgentReplHandler) string {
	t.Helper()
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	mux := http.NewServeMux()
	mux.Handle(agentreplv1connect.NewAgentReplHandler(svc))
	server := &http.Server{Handler: h2c.NewHandler(mux, &http2.Server{})}
	served := make(chan error, 1)
	go func() { served <- server.Serve(listener) }()
	t.Cleanup(func() {
		if err := server.Close(); err != nil {
			t.Errorf("close the health server: %v", err)
		}
		<-served
	})
	return listener.Addr().String()
}

func TestDaemonHealthProbe(t *testing.T) {
	tests := []struct {
		name    string
		address func(t *testing.T) string
		wantErr bool
	}{
		{name: "a daemon that answers is serving", address: func(t *testing.T) string {
			return serveHealth(t, healthOnly{})
		}},
		{name: "a daemon whose health rpc fails is not proven serving", address: func(t *testing.T) string {
			return serveHealth(t, healthOnly{answer: connect.NewError(connect.CodeInternal, context.Canceled)})
		}, wantErr: true},
		{name: "an address nobody listens on is not serving", address: func(t *testing.T) string {
			listener, err := net.Listen("tcp", "127.0.0.1:0")
			if err != nil {
				t.Fatalf("listen: %v", err)
			}
			address := listener.Addr().String()
			if err := listener.Close(); err != nil {
				t.Fatalf("close: %v", err)
			}
			return address
		}, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			address := tt.address(t)

			// Act
			err := DaemonHealthProbe(context.Background(), address)

			// Assert
			if (err != nil) != tt.wantErr {
				t.Fatalf("DaemonHealthProbe = %v, want error %v", err, tt.wantErr)
			}
		})
	}
}
