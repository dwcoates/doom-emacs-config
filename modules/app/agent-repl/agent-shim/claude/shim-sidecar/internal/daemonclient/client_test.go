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
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
)

type clientLogServer struct {
	response       *agentreplv1.ClientLogResponse
	workspace      *workspacev1.WorkspaceRef
	request        *agentreplv1.ClientLogRequest
	rosterRequests int
}

func serveClientLog(t *testing.T, response *agentreplv1.ClientLogResponse) (*Client, *clientLogServer) {
	t.Helper()
	stateDir := t.TempDir()
	workspaceDir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("normalize fake daemon workspace: %v", err)
	}
	recorder := &clientLogServer{
		response: response,
		workspace: &workspacev1.WorkspaceRef{
			Id: "daemon-workspace-id", Dir: workspaceDir,
		},
	}
	mux := http.NewServeMux()
	mux.Handle(agentreplv1connect.AgentReplWatchWorkspaceRosterProcedure,
		connect.NewServerStreamHandler(agentreplv1connect.AgentReplWatchWorkspaceRosterProcedure,
			func(_ context.Context, _ *connect.Request[agentreplv1.WatchWorkspaceRosterRequest], stream *connect.ServerStream[agentreplv1.WatchWorkspaceRosterResponse]) error {
				recorder.rosterRequests++
				return stream.Send(rosterResponse(recorder.workspace))
			}))
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
		Message: "the transcript could not be decoded", WorkspaceDir: server.workspace.GetDir(),
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
	if got.GetWorkspace().GetId() != "daemon-workspace-id" || got.GetWorkspace().GetDir() != record.WorkspaceDir {
		t.Fatalf("workspace ref = %v, want the daemon-minted complete ref", got.GetWorkspace())
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

func TestForwardCachesTheRosterRefForTheSameDaemonAndWorkspace(t *testing.T) {
	// Arrange.
	client, server := serveClientLog(t, &agentreplv1.ClientLogResponse{
		Result: &agentreplv1.ClientLogResponse_Success{Success: &agentreplv1.ClientLogSuccess{}},
	})
	record := logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.read", Message: "read",
		WorkspaceDir: server.workspace.GetDir(), WorkspaceID: "deadbeef",
	}

	// Act.
	if _, err := client.Forward(record); err != nil {
		t.Fatalf("first Forward returned %v", err)
	}
	if _, err := client.Forward(record); err != nil {
		t.Fatalf("second Forward returned %v", err)
	}

	// Assert.
	if server.rosterRequests != 1 {
		t.Fatalf("WatchWorkspaceRoster requests = %d, want one cached lookup", server.rosterRequests)
	}
}

func TestNormalizeWorkspaceDirResolvesTheDeepestExistingAncestor(t *testing.T) {
	// Arrange.
	base := t.TempDir()
	requested := filepath.Join(base, "deleted", "workspace")
	resolvedBase, err := filepath.EvalSymlinks(base)
	if err != nil {
		t.Fatalf("resolve fixture base: %v", err)
	}

	// Act.
	got, err := normalizeWorkspaceDir(requested)

	// Assert.
	if err != nil {
		t.Fatalf("normalizeWorkspaceDir returned %v", err)
	}
	want := filepath.Join(resolvedBase, "deleted", "workspace")
	if got != want {
		t.Fatalf("normalizeWorkspaceDir = %q, want %q", got, want)
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
	client, server := serveClientLog(t, &agentreplv1.ClientLogResponse{})
	record := logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.read", Message: "read",
		WorkspaceDir: server.workspace.GetDir(), WorkspaceID: "deadbeef",
	}

	// Act.
	_, err := client.Forward(record)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "neither success nor error") {
		t.Fatalf("Forward error = %v, want an unset-result refusal", err)
	}
}

func TestForwardInvalidatesTheRosterRefAfterClientLogRefusal(t *testing.T) {
	// Arrange.
	client, server := serveClientLog(t, &agentreplv1.ClientLogResponse{
		Result: &agentreplv1.ClientLogResponse_Error{Error: &agentreplv1.ClientLogError{}},
	})
	record := logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.read", Message: "read",
		WorkspaceDir: server.workspace.GetDir(), WorkspaceID: "deadbeef",
	}

	// Act.
	if _, err := client.Forward(record); err == nil {
		t.Fatal("first Forward succeeded, want the fake daemon's refusal")
	}
	server.response = &agentreplv1.ClientLogResponse{
		Result: &agentreplv1.ClientLogResponse_Success{Success: &agentreplv1.ClientLogSuccess{}},
	}
	if _, err := client.Forward(record); err != nil {
		t.Fatalf("second Forward returned %v", err)
	}

	// Assert.
	if server.rosterRequests != 2 {
		t.Fatalf("WatchWorkspaceRoster requests = %d, want the refused ref resolved again", server.rosterRequests)
	}
}

func rosterResponse(ref *workspacev1.WorkspaceRef) *agentreplv1.WatchWorkspaceRosterResponse {
	return &agentreplv1.WatchWorkspaceRosterResponse{Roster: &frontendv1.WorkspaceRoster{
		Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{{
			Rows: &frontendv1.RosterRows{Rows: []*frontendv1.RosterRow{{
				Workspace: &frontendv1.RosterRowWorkspace{Workspace: copyWorkspaceRef(ref)},
			}}},
		}}},
	}}
}
