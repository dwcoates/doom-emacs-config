// Package server serves testservice.v1.Tests over Connect on a unix socket.
//
// IT OWNS THE ONE MAPPING between the protocol (testservice.v1) and the run
// registry's own Go types (package runs): every request is validated and
// translated here, and every registry answer or refusal is translated back.
//
// LOGGING OWNERSHIP. The registry logs every start, stop, end and storage
// failure where it happens. This layer logs each refused request (at info,
// naming the refusal) and each watch's end (at debug); it never re-logs what
// the registry recorded.
package server

import (
	"context"
	"errors"
	"net"
	"net/http"
	"strings"
	"time"

	"agentrepl/logging/recordlog"
	testservicev1 "agentrepl/proto/testservice/v1"
	"agentrepl/proto/testservice/v1/testservicev1connect"
	"agentrepl/testservice/internal/runs"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
)

// EnvSocket overrides the default socket path, so a test harness can point the
// daemon and the service at a private socket without editing a command line.
const EnvSocket = "AGENT_REPL_TESTS_SOCKET"

// Server is the Connect handler set plus the HTTP server that serves it.
type Server struct {
	runs *runs.Registry
	log  *recordlog.Logger
	http *http.Server
}

var _ testservicev1connect.TestsHandler = (*Server)(nil)

// New builds a Server over an open registry.
func New(reg *runs.Registry, log *recordlog.Logger) *Server {
	if reg == nil || log == nil {
		panic("tests server: nil registry or logger")
	}
	s := &Server{runs: reg, log: log}
	mux := http.NewServeMux()
	path, handler := testservicev1connect.NewTestsHandler(s)
	mux.Handle(path, handler)
	s.http = &http.Server{Handler: h2c.NewHandler(mux, &http2.Server{}), ReadHeaderTimeout: 10 * time.Second}
	return s
}

// Serve serves on ln until Shutdown; it returns http.ErrServerClosed then.
func (s *Server) Serve(ln net.Listener) error { return s.http.Serve(ln) }

// Shutdown drains in-flight requests within ctx. A watch in flight ends with
// its request: the run goes on, and its watcher watches again.
func (s *Server) Shutdown(ctx context.Context) error { return s.http.Shutdown(ctx) }

// StartRun starts one run and answers its id.
func (s *Server) StartRun(_ context.Context, req *connect.Request[testservicev1.StartRunRequest]) (*connect.Response[testservicev1.StartRunResponse], error) {
	const op = "tests.rpc.start_run"
	m := req.Msg
	fail := func(f *testservicev1.StartRunFailure) *connect.Response[testservicev1.StartRunResponse] {
		return connect.NewResponse(&testservicev1.StartRunResponse{Result: &testservicev1.StartRunResponse_Failure{Failure: f}})
	}
	invalid := func(field string) *connect.Response[testservicev1.StartRunResponse] {
		detail := "the start names no " + field
		s.log.Info(op, "refused a start that names no "+field, recordlog.Context{"field": field})
		return fail(&testservicev1.StartRunFailure{Detail: detail, Kind: &testservicev1.StartRunFailure_InvalidRequest{InvalidRequest: &testservicev1.StartRunInvalidRequest{Field: field}}})
	}
	switch {
	case strings.TrimSpace(m.GetDir()) == "":
		return invalid("dir"), nil
	case len(m.GetArgv()) == 0 || strings.TrimSpace(m.GetArgv()[0]) == "":
		return invalid("argv"), nil
	case strings.TrimSpace(m.GetLabel()) == "":
		return invalid("label"), nil
	}
	id, err := s.runs.Start(runs.Spec{Dir: m.GetDir(), Argv: m.GetArgv(), Env: m.GetEnv(), Label: m.GetLabel()})
	var unstartable *runs.UnstartableError
	switch {
	case errors.As(err, &unstartable):
		return fail(&testservicev1.StartRunFailure{Detail: err.Error(), Kind: &testservicev1.StartRunFailure_Unstartable{Unstartable: &testservicev1.StartRunUnstartable{}}}), nil
	case err != nil:
		return fail(&testservicev1.StartRunFailure{Detail: err.Error(), Kind: &testservicev1.StartRunFailure_StorageFailure{StorageFailure: &testservicev1.StartRunStorageFailure{}}}), nil
	}
	return connect.NewResponse(&testservicev1.StartRunResponse{Result: &testservicev1.StartRunResponse_Success{
		Success: &testservicev1.StartRunSuccess{Run: &testservicev1.RunId{Value: id}}}}), nil
}

// WatchRun streams a run's lines from a line on, then its end.
func (s *Server) WatchRun(ctx context.Context, req *connect.Request[testservicev1.WatchRunRequest], stream *connect.ServerStream[testservicev1.WatchRunResponse]) error {
	const op = "tests.rpc.watch_run"
	id := req.Msg.GetRun().GetValue()
	fail := func(f *testservicev1.WatchRunFailure) error {
		return stream.Send(&testservicev1.WatchRunResponse{Event: &testservicev1.WatchRunResponse_Failure{Failure: f}})
	}
	if strings.TrimSpace(id) == "" {
		s.log.Info(op, "refused a watch that names no run", nil)
		return fail(&testservicev1.WatchRunFailure{Detail: "the watch names no run", Kind: &testservicev1.WatchRunFailure_InvalidRequest{InvalidRequest: &testservicev1.WatchRunInvalidRequest{Field: "run"}}})
	}
	fields := recordlog.Context{"run": id, "from_line": req.Msg.GetFromLine()}
	err := s.runs.Watch(ctx, id, req.Msg.GetFromLine(), func(e runs.Event) error {
		if e.End != nil {
			return stream.Send(&testservicev1.WatchRunResponse{Event: &testservicev1.WatchRunResponse_Ended{Ended: endOf(*e.End)}})
		}
		return stream.Send(&testservicev1.WatchRunResponse{Event: &testservicev1.WatchRunResponse_Line{Line: &testservicev1.RunLine{Number: e.Number, Text: e.Text}}})
	})
	var storage *runs.StorageError
	switch {
	case err == nil:
		s.log.Debug(op, "a watch was told its run's end", fields)
		return nil
	case errors.Is(err, runs.ErrUnknownRun):
		s.log.Info(op, "refused a watch of a run the service holds no record of", fields)
		return fail(&testservicev1.WatchRunFailure{Detail: err.Error(), Kind: &testservicev1.WatchRunFailure_UnknownRun{UnknownRun: &testservicev1.WatchRunUnknownRun{}}})
	case errors.As(err, &storage):
		s.log.Error(op, "the run's record could not be read for its watch", withCause(fields, err))
		return fail(&testservicev1.WatchRunFailure{Detail: err.Error(), Kind: &testservicev1.WatchRunFailure_StorageFailure{StorageFailure: &testservicev1.WatchRunStorageFailure{}}})
	case ctx.Err() != nil:
		// THE WATCHER LEFT: a daemon's deploy, or its run cut. The run goes on.
		s.log.Debug(op, "a watch ended with its caller; the run goes on", fields)
		return ctx.Err()
	default:
		s.log.Info(op, "a watch could not be sent to its watcher; the run goes on", withCause(fields, err))
		return err
	}
}

// StopRun kills one run's process group.
func (s *Server) StopRun(_ context.Context, req *connect.Request[testservicev1.StopRunRequest]) (*connect.Response[testservicev1.StopRunResponse], error) {
	const op = "tests.rpc.stop_run"
	id := req.Msg.GetRun().GetValue()
	fail := func(f *testservicev1.StopRunFailure) *connect.Response[testservicev1.StopRunResponse] {
		return connect.NewResponse(&testservicev1.StopRunResponse{Result: &testservicev1.StopRunResponse_Failure{Failure: f}})
	}
	if strings.TrimSpace(id) == "" {
		s.log.Info(op, "refused a stop that names no run", nil)
		return fail(&testservicev1.StopRunFailure{Detail: "the stop names no run", Kind: &testservicev1.StopRunFailure_InvalidRequest{InvalidRequest: &testservicev1.StopRunInvalidRequest{Field: "run"}}}), nil
	}
	err := s.runs.Stop(id)
	switch {
	case errors.Is(err, runs.ErrUnknownRun):
		s.log.Info(op, "refused a stop of a run the service holds no record of", recordlog.Context{"run": id})
		return fail(&testservicev1.StopRunFailure{Detail: err.Error(), Kind: &testservicev1.StopRunFailure_UnknownRun{UnknownRun: &testservicev1.StopRunUnknownRun{}}}), nil
	case err != nil:
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	return connect.NewResponse(&testservicev1.StopRunResponse{Result: &testservicev1.StopRunResponse_Success{Success: &testservicev1.StopRunSuccess{}}}), nil
}

// endOf is a registry end as the protocol says it.
func endOf(e runs.End) *testservicev1.RunEnd {
	out := &testservicev1.RunEnd{CpuMs: e.CPU.Milliseconds(), WallMs: e.Wall.Milliseconds()}
	if e.Lost {
		out.How = &testservicev1.RunEnd_Lost{Lost: &testservicev1.RunLost{Detail: e.LostDetail}}
		return out
	}
	out.How = &testservicev1.RunEnd_Exited{Exited: &testservicev1.RunExited{ExitCode: int32(e.ExitCode), Stopped: e.Stopped}}
	return out
}

func withCause(fields recordlog.Context, err error) recordlog.Context {
	out := make(recordlog.Context, len(fields)+1)
	for k, v := range fields {
		out[k] = v
	}
	out["cause"] = err.Error()
	return out
}
