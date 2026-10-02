package shimclient

import (
	"context"
	"errors"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"
	"agentrepl/proto/shim/v1/shimv1connect"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
)

// fakeShim is an in-process shim.v1 server on a unix socket: the test double
// every client test dials. It scripts each verb's answer and lets a test push
// session frames, drop the session stream, and read back what it received.
type fakeShim struct {
	mu       sync.Mutex
	sessions []*fakeSession
	received map[string]int

	// opened is signaled once per WatchSession open.
	opened chan struct{}

	// answers a test scripts.
	startSessionResp    *shimv1.StartSessionResponse
	startTurnResp       *shimv1.StartTurnResponse
	killTurnResp        *shimv1.KillTurnResponse
	rollBackSessionResp *shimv1.RollBackSessionResponse
	rollBackSessionErr  error
	watchAgentRefusal   error
	watchAgentFrames    []*shimv1.WatchAgentResponse
	watchBashRefusal    error
	watchSessionRefuse  error

	// shutdown stops the server; a test calls it through stop.
	shutdown func()
	// conns are the accepted connections. h2c HIJACKS them, so http.Server's
	// own Close leaves them open — the fake has to close them itself for a
	// stop to look like a shim that is gone.
	conns map[net.Conn]struct{}
}

// stop shuts the fake down and removes its socket, so a client dialing it
// afterwards meets the same evidence a dead shim leaves behind.
func (f *fakeShim) stop() {
	f.mu.Lock()
	stop := f.shutdown
	f.mu.Unlock()
	if stop != nil {
		stop()
	}
}

// fakeSession is one open WatchSession stream.
type fakeSession struct {
	updates chan *conversationv1.SessionUpdate
	drop    chan struct{}
}

// startFakeShim serves a fake shim on a unix socket inside dir and returns it
// with the socket's path. The server is shut down when the test ends.
func startFakeShim(t *testing.T, dir string) (*fakeShim, string) {
	t.Helper()

	f := &fakeShim{
		received: map[string]int{},
		opened:   make(chan struct{}, 64),
	}
	udsPath := filepath.Join(dir, "shim.sock")
	f.serve(t, udsPath)
	return f, udsPath
}

// serve binds the socket and serves h2c until the test ends.
func (f *fakeShim) serve(t *testing.T, udsPath string) {
	t.Helper()

	_ = os.Remove(udsPath)
	ln, err := net.Listen("unix", udsPath)
	if err != nil {
		t.Fatalf("listen %q: %v", udsPath, err)
	}
	mux := http.NewServeMux()
	mux.Handle(shimv1connect.NewShimHandler(f))
	f.mu.Lock()
	f.conns = map[net.Conn]struct{}{}
	f.mu.Unlock()

	srv := &http.Server{
		Handler: h2c.NewHandler(mux, &http2.Server{}),
		ConnState: func(conn net.Conn, state http.ConnState) {
			f.mu.Lock()
			defer f.mu.Unlock()
			switch state {
			case http.StateClosed:
				delete(f.conns, conn)
			default:
				f.conns[conn] = struct{}{}
			}
		},
	}

	done := make(chan struct{})
	go func() {
		defer close(done)
		_ = srv.Serve(ln)
	}()
	var once sync.Once
	shutdown := func() {
		once.Do(func() {
			_ = srv.Close()
			f.mu.Lock()
			conns := make([]net.Conn, 0, len(f.conns))
			for conn := range f.conns {
				conns = append(conns, conn)
			}
			f.conns = map[net.Conn]struct{}{}
			f.mu.Unlock()
			for _, conn := range conns {
				_ = conn.Close()
			}
			<-done
			_ = os.Remove(udsPath)
		})
	}
	f.mu.Lock()
	f.shutdown = shutdown
	f.mu.Unlock()
	t.Cleanup(shutdown)
}

// count is how many times a verb was called.
func (f *fakeShim) count(verb string) int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.received[verb]
}

// record notes one call.
func (f *fakeShim) record(verb string) {
	f.mu.Lock()
	f.received[verb]++
	f.mu.Unlock()
}

// push delivers one session update to every open session stream.
func (f *fakeShim) push(update *conversationv1.SessionUpdate) {
	f.mu.Lock()
	sessions := append([]*fakeSession(nil), f.sessions...)
	f.mu.Unlock()
	for _, s := range sessions {
		s.updates <- update
	}
}

// dropSessions ends every open session stream from the producer's side.
func (f *fakeShim) dropSessions() {
	f.mu.Lock()
	sessions := f.sessions
	f.sessions = nil
	f.mu.Unlock()
	for _, s := range sessions {
		close(s.drop)
	}
}

// healthy is the readiness frame: a diagnostics push whose arm says healthy.
func healthyUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{
			Diagnostics: &conversationv1.SessionDiagnostics{
				Health: &conversationv1.SessionDiagnostics_Healthy{
					Healthy: &conversationv1.SessionHealthy{},
				},
			},
		},
	}
}

// unhealthyUpdate is a diagnostics push whose arm says unhealthy — an answer,
// never readiness.
func unhealthyUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{
			Diagnostics: &conversationv1.SessionDiagnostics{
				Health: &conversationv1.SessionDiagnostics_Unhealthy{
					Unhealthy: &conversationv1.SessionUnhealthy{
						Faults: []*conversationv1.SessionFault{{Component: "store", Detail: "unreachable"}},
					},
				},
			},
		},
	}
}

// compactingUpdate is a session frame that is NOT diagnostics, used to prove
// readiness waits for the health answer specifically.
func compactingUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Compacting{Compacting: &conversationv1.SessionCompacting{}},
	}
}

// ---- shimv1connect.ShimHandler ----

func (f *fakeShim) StartSession(_ context.Context, req *connect.Request[shimv1.StartSessionRequest]) (*connect.Response[shimv1.StartSessionResponse], error) {
	f.record("StartSession")
	if f.startSessionResp != nil {
		return connect.NewResponse(f.startSessionResp), nil
	}
	return connect.NewResponse(&shimv1.StartSessionResponse{}), nil
}

func (f *fakeShim) WatchSession(ctx context.Context, _ *connect.Request[shimv1.WatchSessionRequest], stream *connect.ServerStream[shimv1.WatchSessionResponse]) error {
	f.record("WatchSession")
	if f.watchSessionRefuse != nil {
		return f.watchSessionRefuse
	}
	s := &fakeSession{updates: make(chan *conversationv1.SessionUpdate), drop: make(chan struct{})}
	f.mu.Lock()
	f.sessions = append(f.sessions, s)
	f.mu.Unlock()

	select {
	case f.opened <- struct{}{}:
	default:
	}

	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-s.drop:
			return nil
		case update := <-s.updates:
			frame := &shimv1.WatchSessionResponse{}
			if update != nil {
				frame.Frame = &shimv1.WatchSessionResponse_Update{Update: update}
			}
			if err := stream.Send(frame); err != nil {
				return err
			}
		}
	}
}

func (f *fakeShim) SetSessionModel(_ context.Context, _ *connect.Request[shimv1.SetSessionModelRequest]) (*connect.Response[shimv1.SetSessionModelResponse], error) {
	f.record("SetSessionModel")
	return connect.NewResponse(&shimv1.SetSessionModelResponse{}), nil
}

func (f *fakeShim) SetSessionEffort(_ context.Context, _ *connect.Request[shimv1.SetSessionEffortRequest]) (*connect.Response[shimv1.SetSessionEffortResponse], error) {
	f.record("SetSessionEffort")
	return connect.NewResponse(&shimv1.SetSessionEffortResponse{}), nil
}

func (f *fakeShim) SetSessionPermissionMode(_ context.Context, _ *connect.Request[shimv1.SetSessionPermissionModeRequest]) (*connect.Response[shimv1.SetSessionPermissionModeResponse], error) {
	f.record("SetSessionPermissionMode")
	return connect.NewResponse(&shimv1.SetSessionPermissionModeResponse{}), nil
}

func (f *fakeShim) Hibernate(_ context.Context, _ *connect.Request[shimv1.HibernateRequest]) (*connect.Response[shimv1.HibernateResponse], error) {
	f.record("Hibernate")
	return connect.NewResponse(&shimv1.HibernateResponse{}), nil
}

func (f *fakeShim) KillSession(_ context.Context, _ *connect.Request[shimv1.KillSessionRequest]) (*connect.Response[shimv1.KillSessionResponse], error) {
	f.record("KillSession")
	return connect.NewResponse(&shimv1.KillSessionResponse{}), nil
}

func (f *fakeShim) StartTurn(_ context.Context, _ *connect.Request[shimv1.StartTurnRequest]) (*connect.Response[shimv1.StartTurnResponse], error) {
	f.record("StartTurn")
	if f.startTurnResp != nil {
		return connect.NewResponse(f.startTurnResp), nil
	}
	return connect.NewResponse(&shimv1.StartTurnResponse{}), nil
}

func (f *fakeShim) WatchAgent(_ context.Context, _ *connect.Request[shimv1.WatchAgentRequest], stream *connect.ServerStream[shimv1.WatchAgentResponse]) error {
	f.record("WatchAgent")
	if f.watchAgentRefusal != nil {
		return f.watchAgentRefusal
	}
	for _, frame := range f.watchAgentFrames {
		if err := stream.Send(frame); err != nil {
			return err
		}
	}
	return nil
}

func (f *fakeShim) UpdateAgent(_ context.Context, _ *connect.Request[shimv1.UpdateAgentRequest]) (*connect.Response[shimv1.UpdateAgentResponse], error) {
	f.record("UpdateAgent")
	return connect.NewResponse(&shimv1.UpdateAgentResponse{}), nil
}

func (f *fakeShim) KillTurn(_ context.Context, _ *connect.Request[shimv1.KillTurnRequest]) (*connect.Response[shimv1.KillTurnResponse], error) {
	f.record("KillTurn")
	if f.killTurnResp != nil {
		return connect.NewResponse(f.killTurnResp), nil
	}
	return connect.NewResponse(&shimv1.KillTurnResponse{}), nil
}

func (f *fakeShim) RollBackSession(_ context.Context, _ *connect.Request[shimv1.RollBackSessionRequest]) (*connect.Response[shimv1.RollBackSessionResponse], error) {
	f.record("RollBackSession")
	if f.rollBackSessionErr != nil {
		return nil, f.rollBackSessionErr
	}
	if f.rollBackSessionResp != nil {
		return connect.NewResponse(f.rollBackSessionResp), nil
	}
	return connect.NewResponse(&shimv1.RollBackSessionResponse{}), nil
}

func (f *fakeShim) WatchBash(_ context.Context, _ *connect.Request[shimv1.WatchBashRequest], stream *connect.ServerStream[shimv1.WatchBashResponse]) error {
	f.record("WatchBash")
	if f.watchBashRefusal != nil {
		return f.watchBashRefusal
	}
	return stream.Send(&shimv1.WatchBashResponse{Bash: &conversationv1.AgentBash{}})
}

func (f *fakeShim) StopBash(_ context.Context, _ *connect.Request[shimv1.StopBashRequest]) (*connect.Response[shimv1.StopBashResponse], error) {
	f.record("StopBash")
	return connect.NewResponse(&shimv1.StopBashResponse{}), nil
}

func (f *fakeShim) GetWorkflow(_ context.Context, _ *connect.Request[shimv1.GetWorkflowRequest]) (*connect.Response[shimv1.GetWorkflowResponse], error) {
	return nil, connect.NewError(connect.CodeUnimplemented, errors.New("workflow is kicked"))
}

func (f *fakeShim) WatchWorkflow(_ context.Context, _ *connect.Request[shimv1.WatchWorkflowRequest], _ *connect.ServerStream[shimv1.WatchWorkflowResponse]) error {
	return connect.NewError(connect.CodeUnimplemented, errors.New("workflow is kicked"))
}

func (f *fakeShim) StopWorkflow(_ context.Context, _ *connect.Request[shimv1.StopWorkflowRequest]) (*connect.Response[shimv1.StopWorkflowResponse], error) {
	return nil, connect.NewError(connect.CodeUnimplemented, errors.New("workflow is kicked"))
}

func (f *fakeShim) DetachForeground(_ context.Context, _ *connect.Request[shimv1.DetachForegroundRequest]) (*connect.Response[shimv1.DetachForegroundResponse], error) {
	f.record("DetachForeground")
	return connect.NewResponse(&shimv1.DetachForegroundResponse{}), nil
}

func (f *fakeShim) ReadHistory(_ context.Context, _ *connect.Request[shimv1.ReadHistoryRequest]) (*connect.Response[shimv1.ReadHistoryResponse], error) {
	f.record("ReadHistory")
	return connect.NewResponse(&shimv1.ReadHistoryResponse{}), nil
}

func (f *fakeShim) ReadTranscripts(_ context.Context, _ *connect.Request[shimv1.ReadTranscriptsRequest]) (*connect.Response[shimv1.ReadTranscriptsResponse], error) {
	f.record("ReadTranscripts")
	return connect.NewResponse(&shimv1.ReadTranscriptsResponse{}), nil
}

func (f *fakeShim) GatherTitleDigest(_ context.Context, _ *connect.Request[shimv1.GatherTitleDigestRequest]) (*connect.Response[shimv1.GatherTitleDigestResponse], error) {
	f.record("GatherTitleDigest")
	return connect.NewResponse(&shimv1.GatherTitleDigestResponse{}), nil
}
