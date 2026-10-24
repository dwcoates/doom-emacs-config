// Package server serves zettel.v1.Zettel over Connect on a unix socket.
//
// IT OWNS THE ONE MAPPING between the protocol (zettel.v1) and the graph's own
// Go types (package graph): every request is validated and translated here,
// and every graph result or refusal is translated back here, so neither side
// re-derives the other's shape.
//
// LOGGING OWNERSHIP. The graph logs every storage failure where it happens.
// This layer logs each refused request (at info, naming the refusal) and each
// served request (at debug); it never re-logs a storage failure.
package server

import (
	"context"
	"errors"
	"net"
	"net/http"
	"strings"
	"time"

	zettelv1 "agentrepl/proto/zettel/v1"
	"agentrepl/proto/zettel/v1/zettelv1connect"
	"agentrepl/zettel/internal/graph"
	"agentrepl/zettel/internal/zlog"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
)

// EnvSocket overrides the default socket path, so a test harness can point the
// daemon and the service at a private socket without editing a command line.
const EnvSocket = "AGENT_REPL_ZETTEL_SOCKET"

// Server is the Connect handler set plus the HTTP server that serves it.
type Server struct {
	graph *graph.DB
	log   *zlog.Logger
	http  *http.Server
}

var _ zettelv1connect.ZettelHandler = (*Server)(nil)

// New builds a Server over an open graph.
func New(g *graph.DB, log *zlog.Logger) *Server {
	if g == nil || log == nil {
		panic("zettel server: nil graph or logger")
	}
	s := &Server{graph: g, log: log}
	mux := http.NewServeMux()
	path, handler := zettelv1connect.NewZettelHandler(s)
	mux.Handle(path, handler)
	s.http = &http.Server{Handler: h2c.NewHandler(mux, &http2.Server{}), ReadHeaderTimeout: 10 * time.Second}
	return s
}

// Serve serves on ln until Shutdown; it returns http.ErrServerClosed then.
func (s *Server) Serve(ln net.Listener) error { return s.http.Serve(ln) }

// Shutdown drains in-flight requests within ctx.
func (s *Server) Shutdown(ctx context.Context) error { return s.http.Shutdown(ctx) }

// ---- Exchanges ----

// BeginExchange records a question's origin before its answer exists.
func (s *Server) BeginExchange(ctx context.Context, req *connect.Request[zettelv1.BeginExchangeRequest]) (*connect.Response[zettelv1.BeginExchangeResponse], error) {
	const op = "zettel.rpc.begin_exchange"
	m := req.Msg
	fail := func(detail string, kind any) *connect.Response[zettelv1.BeginExchangeResponse] {
		f := &zettelv1.BeginExchangeFailure{Detail: detail}
		switch k := kind.(type) {
		case *zettelv1.BeginExchangeInvalidRequest:
			f.Kind = &zettelv1.BeginExchangeFailure_InvalidRequest{InvalidRequest: k}
		case *zettelv1.BeginExchangeUnknownParent:
			f.Kind = &zettelv1.BeginExchangeFailure_UnknownParent{UnknownParent: k}
		case *zettelv1.BeginExchangeThreadAlreadyStarted:
			f.Kind = &zettelv1.BeginExchangeFailure_ThreadAlreadyStarted{ThreadAlreadyStarted: k}
		case *zettelv1.BeginExchangeTurnAlreadyBegun:
			f.Kind = &zettelv1.BeginExchangeFailure_TurnAlreadyBegun{TurnAlreadyBegun: k}
		default:
			f.Kind = &zettelv1.BeginExchangeFailure_StorageFailure{StorageFailure: &zettelv1.BeginExchangeStorageFailure{}}
		}
		return connect.NewResponse(&zettelv1.BeginExchangeResponse{Result: &zettelv1.BeginExchangeResponse_Failure{Failure: f}})
	}
	invalid := func(field string) (*connect.Response[zettelv1.BeginExchangeResponse], error) {
		s.refused(op, "invalid_request", field)
		return fail("missing or empty field "+field, &zettelv1.BeginExchangeInvalidRequest{Field: field}), nil
	}
	thread, ok := threadOf(m.GetThread())
	if !ok {
		return invalid("thread")
	}
	if blank(m.GetTurn().GetTurnId()) {
		return invalid("turn")
	}
	if blank(m.GetQuestion().GetText()) {
		return invalid("question")
	}
	parent, field, ok := ParentFromProto(m.GetParent())
	if !ok {
		return invalid(field)
	}
	id, err := s.graph.BeginExchange(ctx, thread, m.GetTurn().GetTurnId(), m.GetQuestion().GetText(), parent)
	var already *graph.TurnAlreadyBegunError
	switch {
	case err == nil:
		s.log.Debug(op, "served", zlog.Context{"thread": string(thread), "exchange": string(id)})
		return connect.NewResponse(&zettelv1.BeginExchangeResponse{Result: &zettelv1.BeginExchangeResponse_Success{
			Success: &zettelv1.BeginExchangeSuccess{Exchange: &zettelv1.ExchangeId{Id: string(id)}},
		}}), nil
	case errors.Is(err, graph.ErrUnknownParent):
		s.refused(op, "unknown_parent", string(thread))
		return fail(err.Error(), &zettelv1.BeginExchangeUnknownParent{}), nil
	case errors.Is(err, graph.ErrThreadAlreadyStarted):
		s.refused(op, "thread_already_started", string(thread))
		return fail(err.Error(), &zettelv1.BeginExchangeThreadAlreadyStarted{}), nil
	case errors.As(err, &already):
		s.refused(op, "turn_already_begun", string(thread))
		return fail(err.Error(), &zettelv1.BeginExchangeTurnAlreadyBegun{Existing: &zettelv1.ExchangeId{Id: string(already.Existing)}}), nil
	default:
		return fail(err.Error(), nil), nil
	}
}

// CompleteExchange turns an exchange into a node.
func (s *Server) CompleteExchange(ctx context.Context, req *connect.Request[zettelv1.CompleteExchangeRequest]) (*connect.Response[zettelv1.CompleteExchangeResponse], error) {
	const op = "zettel.rpc.complete_exchange"
	m := req.Msg
	fail := func(detail string, kind any) *connect.Response[zettelv1.CompleteExchangeResponse] {
		f := &zettelv1.CompleteExchangeFailure{Detail: detail}
		switch k := kind.(type) {
		case *zettelv1.CompleteExchangeInvalidRequest:
			f.Kind = &zettelv1.CompleteExchangeFailure_InvalidRequest{InvalidRequest: k}
		case *zettelv1.CompleteExchangeUnknownExchange:
			f.Kind = &zettelv1.CompleteExchangeFailure_UnknownExchange{UnknownExchange: k}
		default:
			f.Kind = &zettelv1.CompleteExchangeFailure_StorageFailure{StorageFailure: &zettelv1.CompleteExchangeStorageFailure{}}
		}
		return connect.NewResponse(&zettelv1.CompleteExchangeResponse{Result: &zettelv1.CompleteExchangeResponse_Failure{Failure: f}})
	}
	if blank(m.GetExchange().GetId()) {
		s.refused(op, "invalid_request", "exchange")
		return fail("missing or empty field exchange", &zettelv1.CompleteExchangeInvalidRequest{Field: "exchange"}), nil
	}
	if blank(m.GetAnswerMarkdown()) {
		s.refused(op, "invalid_request", "answer_markdown")
		return fail("missing or empty field answer_markdown", &zettelv1.CompleteExchangeInvalidRequest{Field: "answer_markdown"}), nil
	}
	node, err := s.graph.CompleteExchange(ctx, graph.ExchangeID(m.GetExchange().GetId()), m.GetAnswerMarkdown())
	switch {
	case err == nil:
		s.log.Debug(op, "served", zlog.Context{"node": string(node.ID)})
		return connect.NewResponse(&zettelv1.CompleteExchangeResponse{Result: &zettelv1.CompleteExchangeResponse_Success{
			Success: &zettelv1.CompleteExchangeSuccess{Node: NodeToProto(node)},
		}}), nil
	case errors.Is(err, graph.ErrUnknownExchange):
		s.refused(op, "unknown_exchange", m.GetExchange().GetId())
		return fail(err.Error(), &zettelv1.CompleteExchangeUnknownExchange{}), nil
	default:
		return fail(err.Error(), nil), nil
	}
}

// AbandonExchange removes an exchange that will never be answered.
func (s *Server) AbandonExchange(ctx context.Context, req *connect.Request[zettelv1.AbandonExchangeRequest]) (*connect.Response[zettelv1.AbandonExchangeResponse], error) {
	const op = "zettel.rpc.abandon_exchange"
	m := req.Msg
	fail := func(detail string, kind any) *connect.Response[zettelv1.AbandonExchangeResponse] {
		f := &zettelv1.AbandonExchangeFailure{Detail: detail}
		switch k := kind.(type) {
		case *zettelv1.AbandonExchangeInvalidRequest:
			f.Kind = &zettelv1.AbandonExchangeFailure_InvalidRequest{InvalidRequest: k}
		case *zettelv1.AbandonExchangeUnknownExchange:
			f.Kind = &zettelv1.AbandonExchangeFailure_UnknownExchange{UnknownExchange: k}
		default:
			f.Kind = &zettelv1.AbandonExchangeFailure_StorageFailure{StorageFailure: &zettelv1.AbandonExchangeStorageFailure{}}
		}
		return connect.NewResponse(&zettelv1.AbandonExchangeResponse{Result: &zettelv1.AbandonExchangeResponse_Failure{Failure: f}})
	}
	if blank(m.GetExchange().GetId()) {
		s.refused(op, "invalid_request", "exchange")
		return fail("missing or empty field exchange", &zettelv1.AbandonExchangeInvalidRequest{Field: "exchange"}), nil
	}
	err := s.graph.AbandonExchange(ctx, graph.ExchangeID(m.GetExchange().GetId()))
	switch {
	case err == nil:
		s.log.Debug(op, "served", zlog.Context{"exchange": m.GetExchange().GetId()})
		return connect.NewResponse(&zettelv1.AbandonExchangeResponse{Result: &zettelv1.AbandonExchangeResponse_Success{Success: &zettelv1.AbandonExchangeSuccess{}}}), nil
	case errors.Is(err, graph.ErrUnknownExchange):
		s.refused(op, "unknown_exchange", m.GetExchange().GetId())
		return fail(err.Error(), &zettelv1.AbandonExchangeUnknownExchange{}), nil
	default:
		return fail(err.Error(), nil), nil
	}
}

// ListPendingExchanges serves every exchange in flight.
func (s *Server) ListPendingExchanges(ctx context.Context, _ *connect.Request[zettelv1.ListPendingExchangesRequest]) (*connect.Response[zettelv1.ListPendingExchangesResponse], error) {
	pending, err := s.graph.AllPending(ctx)
	if err != nil {
		return connect.NewResponse(&zettelv1.ListPendingExchangesResponse{Result: &zettelv1.ListPendingExchangesResponse_Failure{
			Failure: &zettelv1.ListPendingExchangesFailure{Detail: err.Error(), Kind: &zettelv1.ListPendingExchangesFailure_StorageFailure{StorageFailure: &zettelv1.ListPendingExchangesStorageFailure{}}},
		}}), nil
	}
	out := make([]*zettelv1.PendingExchange, 0, len(pending))
	for _, p := range pending {
		out = append(out, PendingToProto(p))
	}
	return connect.NewResponse(&zettelv1.ListPendingExchangesResponse{Result: &zettelv1.ListPendingExchangesResponse_Success{
		Success: &zettelv1.ListPendingExchangesSuccess{Pending: out},
	}}), nil
}

// ---- Graph reads ----

// GetThread serves one thread whole.
func (s *Server) GetThread(ctx context.Context, req *connect.Request[zettelv1.GetThreadRequest]) (*connect.Response[zettelv1.GetThreadResponse], error) {
	const op = "zettel.rpc.get_thread"
	thread, ok := threadOf(req.Msg.GetThread())
	if !ok {
		s.refused(op, "invalid_request", "thread")
		return connect.NewResponse(&zettelv1.GetThreadResponse{Result: &zettelv1.GetThreadResponse_Failure{Failure: &zettelv1.GetThreadFailure{
			Detail: "missing or empty field thread", Kind: &zettelv1.GetThreadFailure_InvalidRequest{InvalidRequest: &zettelv1.GetThreadInvalidRequest{Field: "thread"}},
		}}}), nil
	}
	nodes, pending, err := s.graph.Thread(ctx, thread)
	if err != nil {
		return connect.NewResponse(&zettelv1.GetThreadResponse{Result: &zettelv1.GetThreadResponse_Failure{Failure: &zettelv1.GetThreadFailure{
			Detail: err.Error(), Kind: &zettelv1.GetThreadFailure_StorageFailure{StorageFailure: &zettelv1.GetThreadStorageFailure{}},
		}}}), nil
	}
	success := &zettelv1.GetThreadSuccess{}
	for _, n := range nodes {
		success.Nodes = append(success.Nodes, NodeToProto(n))
	}
	for _, p := range pending {
		success.Pending = append(success.Pending, PendingToProto(p))
	}
	return connect.NewResponse(&zettelv1.GetThreadResponse{Result: &zettelv1.GetThreadResponse_Success{Success: success}}), nil
}

// ---- Sightings ----

// RecordSightings counts one sighting per named node, all or nothing.
func (s *Server) RecordSightings(ctx context.Context, req *connect.Request[zettelv1.RecordSightingsRequest]) (*connect.Response[zettelv1.RecordSightingsResponse], error) {
	const op = "zettel.rpc.record_sightings"
	fail := func(detail string, kind any) *connect.Response[zettelv1.RecordSightingsResponse] {
		f := &zettelv1.RecordSightingsFailure{Detail: detail}
		switch k := kind.(type) {
		case *zettelv1.RecordSightingsInvalidRequest:
			f.Kind = &zettelv1.RecordSightingsFailure_InvalidRequest{InvalidRequest: k}
		case *zettelv1.RecordSightingsUnknownNode:
			f.Kind = &zettelv1.RecordSightingsFailure_UnknownNode{UnknownNode: k}
		default:
			f.Kind = &zettelv1.RecordSightingsFailure_StorageFailure{StorageFailure: &zettelv1.RecordSightingsStorageFailure{}}
		}
		return connect.NewResponse(&zettelv1.RecordSightingsResponse{Result: &zettelv1.RecordSightingsResponse_Failure{Failure: f}})
	}
	nodes := make([]graph.NodeID, 0, len(req.Msg.GetNodes()))
	for _, n := range req.Msg.GetNodes() {
		if blank(n.GetId()) {
			s.refused(op, "invalid_request", "nodes")
			return fail("a node id is empty", &zettelv1.RecordSightingsInvalidRequest{Field: "nodes"}), nil
		}
		nodes = append(nodes, graph.NodeID(n.GetId()))
	}
	if len(nodes) == 0 {
		s.refused(op, "invalid_request", "nodes")
		return fail("no nodes named", &zettelv1.RecordSightingsInvalidRequest{Field: "nodes"}), nil
	}
	err := s.graph.RecordSightings(ctx, nodes)
	var unknown *graph.UnknownNodeError
	switch {
	case err == nil:
		s.log.Debug(op, "served", zlog.Context{"nodes": len(nodes)})
		return connect.NewResponse(&zettelv1.RecordSightingsResponse{Result: &zettelv1.RecordSightingsResponse_Success{Success: &zettelv1.RecordSightingsSuccess{}}}), nil
	case errors.As(err, &unknown):
		s.refused(op, "unknown_node", string(unknown.Node))
		return fail(err.Error(), &zettelv1.RecordSightingsUnknownNode{Node: &zettelv1.NodeId{Id: string(unknown.Node)}}), nil
	default:
		return fail(err.Error(), nil), nil
	}
}

// refused logs one refused request.
func (s *Server) refused(op, refusal, subject string) {
	s.log.Info(op, "refused a request", zlog.Context{"refusal": refusal, "subject": subject})
}

func blank(v string) bool { return strings.TrimSpace(v) == "" }

func threadOf(t *zettelv1.ThreadId) (graph.ThreadID, bool) {
	if blank(t.GetId()) {
		return "", false
	}
	return graph.ThreadID(t.GetId()), true
}
