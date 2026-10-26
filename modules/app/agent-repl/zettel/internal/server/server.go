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

// RecordExchange records an answered question as a node, idempotent by turn.
func (s *Server) RecordExchange(ctx context.Context, req *connect.Request[zettelv1.RecordExchangeRequest]) (*connect.Response[zettelv1.RecordExchangeResponse], error) {
	const op = "zettel.rpc.record_exchange"
	m := req.Msg
	fail := func(detail string, kind any) *connect.Response[zettelv1.RecordExchangeResponse] {
		f := &zettelv1.RecordExchangeFailure{Detail: detail}
		switch k := kind.(type) {
		case *zettelv1.RecordExchangeInvalidRequest:
			f.Kind = &zettelv1.RecordExchangeFailure_InvalidRequest{InvalidRequest: k}
		case *zettelv1.RecordExchangeUnknownParent:
			f.Kind = &zettelv1.RecordExchangeFailure_UnknownParent{UnknownParent: k}
		case *zettelv1.RecordExchangeThreadAlreadyStarted:
			f.Kind = &zettelv1.RecordExchangeFailure_ThreadAlreadyStarted{ThreadAlreadyStarted: k}
		default:
			f.Kind = &zettelv1.RecordExchangeFailure_StorageFailure{StorageFailure: &zettelv1.RecordExchangeStorageFailure{}}
		}
		return connect.NewResponse(&zettelv1.RecordExchangeResponse{Result: &zettelv1.RecordExchangeResponse_Failure{Failure: f}})
	}
	invalid := func(field string) (*connect.Response[zettelv1.RecordExchangeResponse], error) {
		s.refused(op, "invalid_request", field)
		return fail("missing or empty field "+field, &zettelv1.RecordExchangeInvalidRequest{Field: field}), nil
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
	if a := m.GetQuestion().Abbreviation; a != nil && blank(*a) {
		return invalid("question.abbreviation")
	}
	if blank(m.GetAnswerMarkdown()) {
		return invalid("answer_markdown")
	}
	parent, field, ok := ParentFromProto(m.GetParent())
	if !ok {
		return invalid(field)
	}
	node, created, err := s.graph.RecordExchange(ctx, thread, m.GetTurn().GetTurnId(), m.GetQuestion().GetText(), m.GetQuestion().Abbreviation, parent, m.GetAnswerMarkdown())
	switch {
	case err == nil:
		s.log.Debug(op, "served", zlog.Context{"thread": string(thread), "node": string(node.ID), "created": created})
		return connect.NewResponse(&zettelv1.RecordExchangeResponse{Result: &zettelv1.RecordExchangeResponse_Success{
			Success: &zettelv1.RecordExchangeSuccess{Node: NodeToProto(node)},
		}}), nil
	case errors.Is(err, graph.ErrUnknownParent):
		s.refused(op, "unknown_parent", string(thread))
		return fail(err.Error(), &zettelv1.RecordExchangeUnknownParent{}), nil
	case errors.Is(err, graph.ErrThreadAlreadyStarted):
		s.refused(op, "thread_already_started", string(thread))
		return fail(err.Error(), &zettelv1.RecordExchangeThreadAlreadyStarted{}), nil
	default:
		return fail(err.Error(), nil), nil
	}
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
	nodes, err := s.graph.Thread(ctx, thread)
	if err != nil {
		return connect.NewResponse(&zettelv1.GetThreadResponse{Result: &zettelv1.GetThreadResponse_Failure{Failure: &zettelv1.GetThreadFailure{
			Detail: err.Error(), Kind: &zettelv1.GetThreadFailure_StorageFailure{StorageFailure: &zettelv1.GetThreadStorageFailure{}},
		}}}), nil
	}
	success := &zettelv1.GetThreadSuccess{}
	for _, n := range nodes {
		success.Nodes = append(success.Nodes, NodeToProto(n))
	}
	return connect.NewResponse(&zettelv1.GetThreadResponse{Result: &zettelv1.GetThreadResponse_Success{Success: success}}), nil
}

// SearchThreads serves the threads whose nodes say the query's words.
func (s *Server) SearchThreads(ctx context.Context, req *connect.Request[zettelv1.SearchThreadsRequest]) (*connect.Response[zettelv1.SearchThreadsResponse], error) {
	const op = "zettel.rpc.search_threads"
	fail := func(f *zettelv1.SearchThreadsFailure) (*connect.Response[zettelv1.SearchThreadsResponse], error) {
		return connect.NewResponse(&zettelv1.SearchThreadsResponse{Result: &zettelv1.SearchThreadsResponse_Failure{Failure: f}}), nil
	}
	matches, err := s.graph.Search(ctx, req.Msg.GetQuery())
	switch {
	case errors.Is(err, graph.ErrBlankQuery):
		s.refused(op, "invalid_request", "query")
		return fail(&zettelv1.SearchThreadsFailure{
			Detail: err.Error(), Kind: &zettelv1.SearchThreadsFailure_InvalidRequest{InvalidRequest: &zettelv1.SearchThreadsInvalidRequest{Field: "query"}},
		})
	case err != nil:
		return fail(&zettelv1.SearchThreadsFailure{
			Detail: err.Error(), Kind: &zettelv1.SearchThreadsFailure_StorageFailure{StorageFailure: &zettelv1.SearchThreadsStorageFailure{}},
		})
	}
	success := &zettelv1.SearchThreadsSuccess{}
	for _, m := range matches {
		success.Matches = append(success.Matches, MatchToProto(m))
	}
	s.log.Debug(op, "served", zlog.Context{"matches": len(matches)})
	return connect.NewResponse(&zettelv1.SearchThreadsResponse{Result: &zettelv1.SearchThreadsResponse_Success{Success: success}}), nil
}

// ---- Deletion ----

// DeleteThread destroys one thread's graph, all of it or none.
func (s *Server) DeleteThread(ctx context.Context, req *connect.Request[zettelv1.DeleteThreadRequest]) (*connect.Response[zettelv1.DeleteThreadResponse], error) {
	const op = "zettel.rpc.delete_thread"
	fail := func(f *zettelv1.DeleteThreadFailure) (*connect.Response[zettelv1.DeleteThreadResponse], error) {
		return connect.NewResponse(&zettelv1.DeleteThreadResponse{Result: &zettelv1.DeleteThreadResponse_Failure{Failure: f}}), nil
	}
	thread, ok := threadOf(req.Msg.GetThread())
	if !ok {
		s.refused(op, "invalid_request", "thread")
		return fail(&zettelv1.DeleteThreadFailure{
			Detail: "missing or empty field thread", Kind: &zettelv1.DeleteThreadFailure_InvalidRequest{InvalidRequest: &zettelv1.DeleteThreadInvalidRequest{Field: "thread"}},
		})
	}
	deleted, err := s.graph.DeleteThread(ctx, thread)
	if err != nil {
		return fail(&zettelv1.DeleteThreadFailure{
			Detail: err.Error(), Kind: &zettelv1.DeleteThreadFailure_StorageFailure{StorageFailure: &zettelv1.DeleteThreadStorageFailure{}},
		})
	}
	return connect.NewResponse(&zettelv1.DeleteThreadResponse{Result: &zettelv1.DeleteThreadResponse_Success{
		Success: &zettelv1.DeleteThreadSuccess{NodesDeleted: uint32(deleted)},
	}}), nil
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

// ---- Abbreviations ----

// SetQuestionAbbreviation sets one node's question abbreviation.
func (s *Server) SetQuestionAbbreviation(ctx context.Context, req *connect.Request[zettelv1.SetQuestionAbbreviationRequest]) (*connect.Response[zettelv1.SetQuestionAbbreviationResponse], error) {
	const op = "zettel.rpc.set_question_abbreviation"
	fail := func(detail string, kind any) *connect.Response[zettelv1.SetQuestionAbbreviationResponse] {
		f := &zettelv1.SetQuestionAbbreviationFailure{Detail: detail}
		switch k := kind.(type) {
		case *zettelv1.SetQuestionAbbreviationInvalidRequest:
			f.Kind = &zettelv1.SetQuestionAbbreviationFailure_InvalidRequest{InvalidRequest: k}
		case *zettelv1.SetQuestionAbbreviationUnknownNode:
			f.Kind = &zettelv1.SetQuestionAbbreviationFailure_UnknownNode{UnknownNode: k}
		default:
			f.Kind = &zettelv1.SetQuestionAbbreviationFailure_StorageFailure{StorageFailure: &zettelv1.SetQuestionAbbreviationStorageFailure{}}
		}
		return connect.NewResponse(&zettelv1.SetQuestionAbbreviationResponse{Result: &zettelv1.SetQuestionAbbreviationResponse_Failure{Failure: f}})
	}
	invalid := func(field string) (*connect.Response[zettelv1.SetQuestionAbbreviationResponse], error) {
		s.refused(op, "invalid_request", field)
		return fail("missing or empty field "+field, &zettelv1.SetQuestionAbbreviationInvalidRequest{Field: field}), nil
	}
	m := req.Msg
	if blank(m.GetNode().GetId()) {
		return invalid("node")
	}
	if blank(m.GetAbbreviation()) {
		return invalid("abbreviation")
	}
	node, err := s.graph.SetQuestionAbbreviation(ctx, graph.NodeID(m.GetNode().GetId()), m.GetAbbreviation())
	var unknown *graph.UnknownNodeError
	switch {
	case err == nil:
		s.log.Debug(op, "served", zlog.Context{"node": string(node.ID)})
		return connect.NewResponse(&zettelv1.SetQuestionAbbreviationResponse{Result: &zettelv1.SetQuestionAbbreviationResponse_Success{
			Success: &zettelv1.SetQuestionAbbreviationSuccess{Node: NodeToProto(node)},
		}}), nil
	case errors.As(err, &unknown):
		s.refused(op, "unknown_node", string(unknown.Node))
		return fail(err.Error(), &zettelv1.SetQuestionAbbreviationUnknownNode{Node: &zettelv1.NodeId{Id: string(unknown.Node)}}), nil
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
