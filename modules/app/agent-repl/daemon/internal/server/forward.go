package server

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"syscall"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/workspace"
	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// ForwardedHeader marks a call one daemon forwarded to the daemon serving its
// workspace. A daemon never forwards a call that carries it again.
const ForwardedHeader = "Agent-Repl-Forwarded"

// RelayedPageHeader names the page a relayed subscription pushes to
// (relayWatch): the daemon holding the page's stream sets it on the dedicated
// stream it opens at the daemon serving the workspace, which holds the page
// as connected to that workspace for as long as the stream stands.
const RelayedPageHeader = "Agent-Repl-Relayed-Page"

// AcceptedHeader is set on the response headers of a standing stream the
// daemon ACCEPTED (acceptWriter.accept). A refused stream answers headers
// too, with its refusal in the trailer, so a daemon relaying another's
// stream reads the acceptance here rather than from the headers' arrival.
const AcceptedHeader = "Agent-Repl-Accepted"

// FORWARDING REPLACES THE REDIRECT (docs/investigations/
// 2026-11-18-stable-sockets-design.md, D3). Every client reaches the one
// shared address, which only the daemon holding the boot claim accepts on; a
// call for a workspace another live daemon serves is re-issued there, on its
// private address, and its answer relayed. The target is the one the typed
// redirects named: the successor a transferred workspace went to, or the
// incumbent that still serves one this daemon has not adopted.

// errTargetGone is what a forward or a relay answers when the daemon it
// dialed no longer accepts connections (targetGone). The caller then answers
// the call here, exactly as it does for a daemon that no longer runs.
var errTargetGone = errors.New("server: the daemon the call was sent to no longer accepts connections")

// targetGone reports whether ERR is the kernel refusing a connection to
// another daemon's private address.
//
// A REFUSED CONNECTION IS THE DAEMON'S END, READ WHERE IT CANNOT BE STALE. A
// daemon's private listener never reopens, so a refusal says for good that
// the daemon accepts nothing more. Its life lock says less: an exiting daemon
// closes its listener first and holds the lock until its process is gone, so
// for that span the lock reads "runs" of a daemon no call can reach
// (2026-11-19: a page's five subscriptions were forwarded to an incumbent 36
// ms after it logged its exit, were refused, and the page filed
// daemonUnreachable). No request byte left this daemon on a refused dial, so
// answering the call here repeats nothing.
func targetGone(err error) bool {
	return errors.Is(err, syscall.ECONNREFUSED)
}

// forwardTarget answers the private address of the daemon that serves the
// call's workspace, when that is not this one. A call already forwarded is
// served here whatever its standing: forwarding it again could only loop.
func (s *server) forwardTarget(ctx context.Context, rpc string, header http.Header, msg proto.Message) (string, bool, error) {
	if header.Get(ForwardedHeader) != "" {
		return "", false, nil
	}
	// A CALL THAT NAMES A DAEMON GOES TO THAT DAEMON, before any workspace
	// rule: a page call belongs to the daemon holding the page's stream, and
	// a feed tail to the daemon holding its pin, whichever daemon serves the
	// workspace the call's arm happens to name.
	if instance, named := requestInstance(msg); named {
		return s.instanceTarget(rpc, instance)
	}
	id := requestWorkspaceID(msg)
	if id == "" {
		if target, ok := adoptTarget(rpc, s.deps.SuccessorAddress()); ok {
			return target, true, nil
		}
		return "", false, nil
	}
	return s.servingTarget(ctx, rpc, ids.WorkspaceID(id))
}

// requestInstance answers the daemon instance a call names, and whether the
// call is one that names a daemon at all: a page call names the daemon that
// issued its attachment, a feed tail the daemon that minted its token. An
// empty instance on such a call is malformed, which its handler refuses.
func requestInstance(msg proto.Message) (string, bool) {
	switch m := msg.(type) {
	case *agentreplv1.SubscribePageRequest:
		return m.GetAttachment().GetInstance(), true
	case *agentreplv1.UnsubscribePageRequest:
		return m.GetAttachment().GetInstance(), true
	case *agentreplv1.WatchFeedRequest:
		return m.GetWatch().GetInstance(), true
	}
	return "", false
}

// instanceTarget answers the private address of daemon INSTANCE when it is
// another live daemon. This daemon's own instance, and one that no longer
// runs, are served here: the handler answers what a call for a daemon that is
// gone is owed (a page call `page_gone`, a feed tail `unknown_token`).
func (s *server) instanceTarget(rpc, instance string) (string, bool, error) {
	if instance == "" || instance == string(s.deps.Instance) {
		return "", false, nil
	}
	address, alive, err := s.deps.InstanceAddress(ids.InstanceID(instance))
	if err != nil {
		s.log.Error("daemon.server.forward", "could not resolve the daemon a call names to forward it", dlog.Context{"rpc": rpc, "instance": instance, "cause": err.Error()})
		return "", false, err
	}
	if !alive {
		s.log.Info("daemon.server.forward", "the daemon a call names no longer runs; the call is answered here", dlog.Context{"rpc": rpc, "instance": instance})
		return "", false, nil
	}
	return address, true, nil
}

// servingTarget answers the private address of the daemon that serves WS,
// when that is not this one.
//
// AN ADOPT CALL GOES WHERE ITS WORKSPACE'S RENDEZVOUS IS. For a workspace
// this daemon serves or has handed on, that is its successor while a handover
// has one (adoptTarget). For a workspace this daemon has NOT adopted it is
// decided by the serving row like any other call: an older daemon that still
// holds the workspace takes it, never this daemon's own successor, which
// serves nothing of a workspace two generations back.
func (s *server) servingTarget(ctx context.Context, rpc string, ws ids.WorkspaceID) (string, bool, error) {
	id := string(ws)
	standing, err := s.deps.Ownership.Standing(ctx, ws)
	if err != nil {
		s.log.Error("daemon.server.forward", "could not resolve the call's serving standing to forward it", dlog.Context{"rpc": rpc, "workspace_id": id, "cause": err.Error()})
		return "", false, err
	}
	if standing != workspace.StandingNotYetAdopted {
		if target, ok := adoptTarget(rpc, s.deps.SuccessorAddress()); ok {
			return target, true, nil
		}
	}
	switch standing {
	case workspace.StandingTransferringAway:
		if target := s.deps.SuccessorAddress(); target != "" {
			return target, true, nil
		}
	case workspace.StandingNotYetAdopted:
		// THE SERVING ROW NAMES WHO SERVES IT. An older daemon still holding
		// it gets the call; a row released for the adoption under way here
		// means this daemon is the one adopting it, so the call -- an adopt
		// call above all, which IS the adoption -- is served here.
		target, ok, err := s.deps.ServingElsewhere(ctx, ids.WorkspaceID(id))
		if err != nil {
			s.log.Error("daemon.server.forward", "could not read the workspace's serving row to forward the call", dlog.Context{"rpc": rpc, "workspace_id": id, "cause": err.Error()})
			return "", false, err
		}
		if ok {
			return target, true, nil
		}
	}
	return "", false, nil
}

// adoptRPCs are the calls that ARE an adoption: a client joining the
// rendezvous a handover arms on its successor.
var adoptRPCs = map[string]bool{"AdoptWebWorkspace": true, "AdoptHostWorkspace": true}

// adoptTarget answers SUCCESSOR for an adopt call while a handover has one.
// THE RENDEZVOUS LIVES ON THE SUCCESSOR: a client that reloads onto the
// shared address mid-handover reaches the incumbent, which still accepts on
// it, before the workspace stands transferred there, and only the successor
// can adopt it. Any other call, and an adopt call with no handover in flight,
// is decided by its workspace's standing.
func adoptTarget(rpc, successor string) (string, bool) {
	if adoptRPCs[rpc] && successor != "" {
		return successor, true
	}
	return "", false
}

// forwardRequest copies REQ for TARGET: its message and headers, marked as
// forwarded.
func forwardRequest[Req any](req *connect.Request[Req]) *connect.Request[Req] {
	out := connect.NewRequest(req.Msg)
	for name, values := range req.Header() {
		for _, v := range values {
			out.Header().Add(name, v)
		}
	}
	out.Header().Set(ForwardedHeader, "1")
	return out
}

// forwardUnary re-issues the unary call REQ to the daemon at TARGET and
// answers its response.
func forwardUnary[Req, Resp any](ctx context.Context, s *server, rpc, target string, req *connect.Request[Req]) (*connect.Response[Resp], error) {
	client := connect.NewClient[Req, Resp](s.deps.Forward, "http://"+target+req.Spec().Procedure)
	resp, err := client.CallUnary(ctx, forwardRequest(req))
	if targetGone(err) {
		s.log.Info("daemon.server.forward", "the daemon a call was forwarded to no longer accepts connections; the call is answered here", dlog.Context{"rpc": rpc, "target": target, "cause": err.Error()})
		return nil, errTargetGone
	}
	if err != nil {
		s.log.Info("daemon.server.forward", "the daemon a call was forwarded to answered it with an error", dlog.Context{"rpc": rpc, "target": target, "cause": err.Error()})
		return nil, err
	}
	return resp, nil
}

// forwardStream re-issues the server-streaming call REQ to the daemon at
// TARGET and relays every message it sends onto STREAM until it ends.
//
// A RELAYED STREAM ENDS WITH THIS DAEMON'S SERVING LIFE (Close), as every
// standing stream it serves does: the client re-opens it on the shared
// address, which the daemon that accepts next answers. Kept open, it held
// this daemon's exit until the client gave up.
func forwardStream[Req, Resp any](ctx context.Context, s *server, rpc, target string, req *connect.Request[Req], stream *connect.ServerStream[Resp]) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	stop := context.AfterFunc(s.life, cancel)
	defer stop()
	return pipeStream[Req, Resp](ctx, s, rpc, target, req.Spec().Procedure, forwardRequest(req), stream, func() { s.acceptStream(ctx, rpc) })
}

// pipeStream opens the server-streaming rpc at PROCEDURE on the daemon at
// TARGET with REQ and writes every message it sends into SINK, answering how
// the target's stream ended: nil for a clean end, errTargetGone for a target
// that no longer accepts connections, its error otherwise. It is
// the one place a stream is carried from another daemon, for a forwarded
// stream (forwardStream) and a relayed page subscription (relayWatch) alike.
//
// THE TARGET'S ACCEPTANCE IS STATED ONCE, BEFORE ANY MESSAGE. ACCEPTED is
// called when the target's response headers state it (AcceptedHeader), so a
// watch the target accepted with nothing to push yet is accepted here too, or
// the client would read "accepted but quiet" as "not accepted" (accept.go). A
// target that sends a message without stating acceptance has a stream that
// stands all the same, so the message is the acceptance.
func pipeStream[Req, Resp any](ctx context.Context, s *server, rpc, target, procedure string, req *connect.Request[Req], sink streamSink[Resp], accepted func()) error {
	client := connect.NewClient[Req, Resp](s.deps.Forward, "http://"+target+procedure)
	gone := func(err error) error {
		s.log.Info("daemon.server.forward", "the daemon a stream was forwarded to no longer accepts connections; the stream is served here", dlog.Context{"rpc": rpc, "target": target, "cause": err.Error()})
		return errTargetGone
	}
	in, err := client.CallServerStream(ctx, req)
	if targetGone(err) {
		return gone(err)
	}
	if err != nil {
		s.log.Info("daemon.server.forward", "the daemon a stream was forwarded to refused it", dlog.Context{"rpc": rpc, "target": target, "cause": err.Error()})
		return err
	}
	defer in.Close()
	stated := in.ResponseHeader().Get(AcceptedHeader) != ""
	if stated {
		accepted()
	}
	for in.Receive() {
		if !stated {
			stated = true
			accepted()
		}
		if err := sink.Send(in.Msg()); err != nil {
			return fmt.Errorf("relay %s from %s: %w", rpc, target, err)
		}
	}
	// A DIAL IS REFUSED AT THE FIRST RECEIVE, before the target stated
	// anything: nothing of the stream exists there or here.
	if err := in.Err(); !stated && targetGone(err) {
		return gone(err)
	}
	return in.Err()
}
