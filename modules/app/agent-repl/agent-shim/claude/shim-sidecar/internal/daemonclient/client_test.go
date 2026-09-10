package daemonclient

import (
	"context"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
)

type clientLogServer struct {
	response *agentreplv1.ClientLogResponse
	request  *agentreplv1.ClientLogRequest
}

func serveClientLog(t *testing.T, response *agentreplv1.ClientLogResponse) (*Client, *clientLogServer) {
	t.Helper()
	stateDir := t.TempDir()
	recorder := &clientLogServer{response: response}
	mux := http.NewServeMux()
	mux.Handle(agentreplv1connect.AgentReplClientLogProcedure,
		connect.NewUnaryHandler(agentreplv1connect.AgentReplClientLogProcedure,
			func(_ context.Context, req *connect.Request[agentreplv1.ClientLogRequest]) (*connect.Response[agentreplv1.ClientLogResponse], error) {
				recorder.request = proto.Clone(req.Msg).(*agentreplv1.ClientLogRequest)
				return connect.NewResponse(recorder.response), nil
			}))
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen for fake daemon: %v", err)
	}
	server := &http.Server{Handler: mux}
	go func() { _ = server.Serve(listener) }()
	t.Cleanup(func() {
		_ = server.Shutdown(context.Background())
	})
	if err := os.WriteFile(filepath.Join(stateDir, "daemon.addr"), []byte(listener.Addr().String()+"\n"), 0o600); err != nil {
		t.Fatalf("write daemon.addr: %v", err)
	}
	return New(stateDir), recorder
}

func TestForwardSendsACompleteSidecarClientLogRequest(t *testing.T) {
	// Arrange.
	client, server := serveClientLog(t, &agentreplv1.ClientLogResponse{
		Result: &agentreplv1.ClientLogResponse_Success{Success: &agentreplv1.ClientLogSuccess{}},
	})
	record := logging.ForwardRecord{
		Timestamp: "2026-09-10T12:34:56.789000-04:00", PID: 4242,
		Level: "warn", Verbose: true, Operation: "sidecar.tail.read",
		Message: "the transcript could not be decoded", WorkspaceDir: "/work/repo",
		WorkspaceID: "deadbeef", ClaudeSessionID: "claude-1",
		Context: map[string]any{
			"path": "/work/repo/session.jsonl", "pid": 4242.0,
			"write_ids": []string{"write-1", "write-2"},
		},
	}

	// Act.
	_, err := client.Forward(record)

	// Assert.
	if err != nil {
		t.Fatalf("Forward returned %v", err)
	}
	got := server.request
	if got.GetWorkspace().GetId() != "deadbeef" || got.GetWorkspace().GetDir() != "/work/repo" {
		t.Fatalf("workspace ref = %v, want the sidecar's complete ref", got.GetWorkspace())
	}
	if got.GetRecord().GetSidecar() == nil || got.GetRecord().GetWarn() == nil {
		t.Fatalf("record arms = runtime %T level %T, want sidecar/warn", got.GetRecord().GetRuntime(), got.GetRecord().GetLevel())
	}
	if got.GetRecord().GetTimestamp() != record.Timestamp || !got.GetRecord().GetVerbose() {
		t.Fatalf("record clock/class = %q/%t, want %q/true", got.GetRecord().GetTimestamp(), got.GetRecord().GetVerbose(), record.Timestamp)
	}
	if got.GetRecord().GetContext().AsMap()["path"] != record.Context["path"] {
		t.Fatalf("record context = %v, want path preserved", got.GetRecord().GetContext().AsMap())
	}
	writeIDs := got.GetRecord().GetContext().GetFields()["write_ids"].GetListValue().GetValues()
	if len(writeIDs) != 2 || writeIDs[0].GetStringValue() != "write-1" || writeIDs[1].GetStringValue() != "write-2" {
		t.Fatalf("record context.write_ids = %v, want the complete typed string list", writeIDs)
	}
}

func TestForwardRejectsANonLoopbackDaemonAddress(t *testing.T) {
	// Arrange.
	stateDir := t.TempDir()
	if err := os.WriteFile(filepath.Join(stateDir, "daemon.addr"), []byte("192.0.2.10:8123\n"), 0o600); err != nil {
		t.Fatalf("write daemon.addr: %v", err)
	}

	// Act.
	address, err := New(stateDir).Forward(logging.ForwardRecord{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "not a loopback IP") {
		t.Fatalf("Forward error = %v, want a loopback refusal", err)
	}
	if address != "192.0.2.10:8123" {
		t.Fatalf("failure address = %q, want the refused daemon address", address)
	}
}

func TestForwardRefusesAResponseWithNoResultArm(t *testing.T) {
	// Arrange.
	client, _ := serveClientLog(t, &agentreplv1.ClientLogResponse{})
	record := logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.read", Message: "read",
		WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef",
	}

	// Act.
	_, err := client.Forward(record)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "neither success nor error") {
		t.Fatalf("Forward error = %v, want an unset-result refusal", err)
	}
}
