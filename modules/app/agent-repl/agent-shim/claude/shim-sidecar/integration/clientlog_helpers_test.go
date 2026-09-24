package integration

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io/fs"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
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
	roots    []string
}

var clientLogsByGlobalPath sync.Map

func startFakeClientLog(t *testing.T, stateDir, globalLogPath string, roots []string) *fakeClientLog {
	t.Helper()
	if err := os.MkdirAll(stateDir, 0o700); err != nil {
		t.Fatalf("create fake daemon state dir: %v", err)
	}
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen for fake ClientLog: %v", err)
	}
	fake := &fakeClientLog{
		t: t, listener: listener, received: make(chan struct{}, 4096),
		roots: append([]string(nil), roots...),
	}
	mux := http.NewServeMux()
	mux.Handle(agentreplv1connect.AgentReplWatchWorkspaceRosterProcedure,
		connect.NewServerStreamHandler(agentreplv1connect.AgentReplWatchWorkspaceRosterProcedure, fake.watchWorkspaceRoster))
	mux.Handle(agentreplv1connect.AgentReplClientLogProcedure,
		connect.NewUnaryHandler(agentreplv1connect.AgentReplClientLogProcedure, fake.handle))
	fake.server = &http.Server{Handler: mux}
	if err := os.WriteFile(filepath.Join(stateDir, "daemon.addr"), []byte(listener.Addr().String()+"\n"), 0o600); err != nil {
		closeErr := listener.Close()
		t.Fatalf("publish fake daemon.addr: %v (closing the listener afterward: %v)", err, closeErr)
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

func (f *fakeClientLog) watchWorkspaceRoster(
	ctx context.Context,
	_ *connect.Request[agentreplv1.WatchWorkspaceRosterRequest],
	stream *connect.ServerStream[agentreplv1.WatchWorkspaceRosterResponse],
) error {
	ticker := time.NewTicker(10 * time.Millisecond)
	defer ticker.Stop()
	for {
		refs, err := f.workspaceRefs()
		if err != nil {
			return err
		}
		if len(refs) > 0 {
			if err := stream.Send(fakeRoster(refs)); err != nil {
				return err
			}
		}
		select {
		case <-ctx.Done():
			return nil
		case <-ticker.C:
		}
	}
}

func (f *fakeClientLog) workspaceRefs() ([]*workspacev1.WorkspaceRef, error) {
	byDir := map[string]*workspacev1.WorkspaceRef{}
	for _, root := range f.roots {
		err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, walkErr error) error {
			if walkErr != nil {
				return walkErr
			}
			if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".jsonl") {
				return nil
			}
			raw, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			for _, line := range bytes.Split(raw, []byte{'\n'}) {
				var record struct {
					CWD string `json:"cwd"`
				}
				if json.Unmarshal(line, &record) != nil || record.CWD == "" {
					continue
				}
				dir, err := normalizeFakeWorkspaceDir(record.CWD)
				if err != nil {
					return err
				}
				id, err := sharedlogging.WorkspaceID(dir)
				if err != nil {
					return err
				}
				byDir[dir] = &workspacev1.WorkspaceRef{Id: "daemon-" + id, Dir: dir}
				break
			}
			return nil
		})
		if err != nil {
			return nil, fmt.Errorf("scan fake daemon roster beneath %q: %w", root, err)
		}
	}
	dirs := make([]string, 0, len(byDir))
	for dir := range byDir {
		dirs = append(dirs, dir)
	}
	sort.Strings(dirs)
	refs := make([]*workspacev1.WorkspaceRef, 0, len(dirs))
	for _, dir := range dirs {
		refs = append(refs, byDir[dir])
	}
	return refs, nil
}

func normalizeFakeWorkspaceDir(dir string) (string, error) {
	abs, err := filepath.Abs(dir)
	if err != nil {
		return "", err
	}
	abs = filepath.Clean(abs)
	rest := ""
	head := abs
	for {
		if resolved, err := filepath.EvalSymlinks(head); err == nil {
			return filepath.Clean(filepath.Join(resolved, rest)), nil
		}
		parent := filepath.Dir(head)
		if parent == head {
			return "", fmt.Errorf("no existing ancestor for %q", dir)
		}
		rest = filepath.Join(filepath.Base(head), rest)
		head = parent
	}
}

func fakeRoster(refs []*workspacev1.WorkspaceRef) *agentreplv1.WatchWorkspaceRosterResponse {
	rows := make([]*frontendv1.RosterRow, 0, len(refs))
	for _, ref := range refs {
		rows = append(rows, &frontendv1.RosterRow{
			Workspace: &frontendv1.RosterRowWorkspace{Workspace: ref},
		})
	}
	return &agentreplv1.WatchWorkspaceRosterResponse{Roster: &frontendv1.WorkspaceRoster{
		Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{{
			Rows: &frontendv1.RosterRows{Rows: rows},
		}}},
	}}
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
