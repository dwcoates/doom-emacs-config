package server

import (
	"context"
	"fmt"
	"net/http"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/workspace"
)

// ForwardedHeader marks a call one daemon forwarded to the daemon serving its
// workspace. A daemon never forwards a call that carries it again.
const ForwardedHeader = "Agent-Repl-Forwarded"

// FORWARDING REPLACES THE REDIRECT (docs/investigations/
// 2026-11-18-stable-sockets-design.md, D3). Every client reaches the one
// shared address, which only the daemon holding the boot claim accepts on; a
// call for a workspace another live daemon serves is re-issued there, on its
// private address, and its answer relayed. The target is the one the typed
// redirects named: the successor a transferred workspace went to, or the
// incumbent that still serves one this daemon has not adopted.

// forwardTarget answers the private address of the daemon that serves the
// call's workspace, when that is not this one. A call already forwarded is
// served here whatever its standing: forwarding it again could only loop.
func (s *server) forwardTarget(ctx context.Context, rpc string, header http.Header, msg proto.Message) (string, bool, error) {
	if header.Get(ForwardedHeader) != "" {
		return "", false, nil
	}
	id := requestWorkspaceID(msg)
	if id == "" {
		return "", false, nil
	}
	standing, err := s.deps.Ownership.Standing(ctx, ids.WorkspaceID(id))
	if err != nil {
		s.log.Error("daemon.server.forward", "could not resolve the call's serving standing to forward it", dlog.Context{"rpc": rpc, "workspace_id": id, "cause": err.Error()})
		return "", false, err
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
	client := connect.NewClient[Req, Resp](s.deps.Forward, "http://"+target+req.Spec().Procedure)
	in, err := client.CallServerStream(ctx, forwardRequest(req))
	if err != nil {
		s.log.Info("daemon.server.forward", "the daemon a stream was forwarded to refused it", dlog.Context{"rpc": rpc, "target": target, "cause": err.Error()})
		return err
	}
	defer in.Close()
	for in.Receive() {
		if err := stream.Send(in.Msg()); err != nil {
			return fmt.Errorf("relay %s from %s: %w", rpc, target, err)
		}
	}
	return in.Err()
}

// clientAddress is the address a client is told to reach the daemon at
// PRIVATE on: the shared address when the daemons share one, PRIVATE
// otherwise. A forward always goes to PRIVATE (forwardTarget).
func (s *server) clientAddress(private string) string {
	if private == "" {
		return ""
	}
	if shared := s.deps.SharedAddress(); shared != "" {
		return shared
	}
	return private
}
