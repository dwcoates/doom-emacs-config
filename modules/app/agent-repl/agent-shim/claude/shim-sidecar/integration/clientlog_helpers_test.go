package integration

import (
	"context"
	"encoding/json"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"sync"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	workspacev1 "agentrepl/proto/workspace/v1"
	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
)

// fakeClientLog is the sidecar suite's private daemon boundary. Every real
// sidecar process receives its address through that process's private
// daemon.addr, so no test can contact the owner's deployed daemon.
type fakeClientLog struct {
	t        *testing.T
	server   *http.Server
	listener net.Listener

	mu       sync.Mutex
	requests []*agentreplv1.ClientLogRequest
	records  []logRecord
	received chan struct{}
}

var clientLogsByGlobalPath sync.Map

func startFakeClientLog(t *testing.T, stateDir, globalLogPath string) *fakeClientLog {
	t.Helper()
	if err := os.MkdirAll(stateDir, 0o700); err != nil {
		t.Fatalf("create fake daemon state dir: %v", err)
	}
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen for fake ClientLog: %v", err)
	}
	fake := &fakeClientLog{t: t, listener: listener, received: make(chan struct{}, 4096)}
	mux := http.NewServeMux()
	mux.Handle(agentreplv1connect.AgentReplRegisterWorkspaceProcedure,
		connect.NewUnaryHandler(agentreplv1connect.AgentReplRegisterWorkspaceProcedure, fake.registerWorkspace))
	mux.Handle(agentreplv1connect.AgentReplClientLogProcedure,
		connect.NewUnaryHandler(agentreplv1connect.AgentReplClientLogProcedure, fake.handle))
	fake.server = &http.Server{Handler: mux}
	if err := os.WriteFile(filepath.Join(stateDir, "daemon.addr"), []byte(listener.Addr().String()+"\n"), 0o600); err != nil {
		listener.Close()
		t.Fatalf("publish fake daemon.addr: %v", err)
	}
	clientLogsByGlobalPath.Store(globalLogPath, fake)
	go func() { _ = fake.server.Serve(listener) }()
	t.Cleanup(func() {
		clientLogsByGlobalPath.Delete(globalLogPath)
		ctx, cancel := context.WithTimeout(context.Background(), waitBudget)
		defer cancel()
		_ = fake.server.Shutdown(ctx)
	})
	return fake
}

func (f *fakeClientLog) registerWorkspace(
	_ context.Context,
	req *connect.Request[agentreplv1.RegisterWorkspaceRequest],
) (*connect.Response[agentreplv1.RegisterWorkspaceResponse], error) {
	return connect.NewResponse(&agentreplv1.RegisterWorkspaceResponse{
		Result: &agentreplv1.RegisterWorkspaceResponse_Success{Success: &agentreplv1.RegisterWorkspaceSuccess{
			Workspace: &workspacev1.WorkspaceRef{Id: "daemon-workspace-id", Dir: req.Msg.GetDir()},
		}},
	}), nil
}

func (f *fakeClientLog) handle(
	_ context.Context,
	req *connect.Request[agentreplv1.ClientLogRequest],
) (*connect.Response[agentreplv1.ClientLogResponse], error) {
	recorded := proto.Clone(req.Msg).(*agentreplv1.ClientLogRequest)
	log := forwardedLogRecord(recorded)
	f.mu.Lock()
	f.requests = append(f.requests, recorded)
	f.records = append(f.records, log)
	f.mu.Unlock()
	select {
	case f.received <- struct{}{}:
	default:
	}
	return connect.NewResponse(&agentreplv1.ClientLogResponse{
		Result: &agentreplv1.ClientLogResponse_Success{Success: &agentreplv1.ClientLogSuccess{}},
	}), nil
}

func forwardedLogRecord(req *agentreplv1.ClientLogRequest) logRecord {
	record := req.GetRecord()
	ctx := record.GetContext().AsMap()
	pid, _ := ctx["pid"].(float64)
	claudeSessionID, _ := ctx["claude_session_id"].(string)
	delete(ctx, "pid")
	delete(ctx, "claude_session_id")
	level := "error"
	switch {
	case record.GetDebug() != nil:
		level = "debug"
	case record.GetInfo() != nil:
		level = "info"
	case record.GetWarn() != nil:
		level = "warn"
	}
	verbosity := "normal"
	if record.GetVerbose() {
		verbosity = "verbose"
	}
	return logRecord{
		Timestamp: record.GetTimestamp(), Runtime: "sidecar", PID: int(pid),
		Level: level, Verbosity: verbosity, Operation: record.GetOperation(),
		Message: record.GetMessage(), WorkspaceDir: req.GetWorkspace().GetDir(),
		WorkspaceID: req.GetWorkspace().GetId(), ClaudeSessionID: claudeSessionID,
		Context: ctx,
	}
}

func (f *fakeClientLog) recordsSnapshot() []logRecord {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]logRecord(nil), f.records...)
}

func (f *fakeClientLog) requestsSnapshot() []*agentreplv1.ClientLogRequest {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]*agentreplv1.ClientLogRequest, len(f.requests))
	for i, req := range f.requests {
		out[i] = proto.Clone(req).(*agentreplv1.ClientLogRequest)
	}
	return out
}

func (f *fakeClientLog) awaitRequest(ctx context.Context, match func(*agentreplv1.ClientLogRequest) bool) *agentreplv1.ClientLogRequest {
	f.t.Helper()
	for {
		for _, req := range f.requestsSnapshot() {
			if match(req) {
				return req
			}
		}
		select {
		case <-f.received:
		case <-ctx.Done():
			encoded, _ := json.Marshal(f.requestsSnapshot())
			f.t.Fatalf("no matching ClientLog request before the deadline; requests=%s", encoded)
		}
	}
}
