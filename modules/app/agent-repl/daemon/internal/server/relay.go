package server

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"

	"agent-repld/internal/dlog"
)

// errSubscriptionMoved is what a relayed subscription's body returns when the
// daemon serving it ended the view WITHOUT an error: it stood down, or
// stopped serving the workspace. The page is told `moved` and subscribes
// again (endedFrame).
var errSubscriptionMoved = errors.New("server: the daemon serving the relayed subscription ended it")

// relayTarget answers the private address of the daemon WATCH's view is served
// from, when that is not this one: the daemon that minted a feed tail's token,
// or the daemon that serves the watch's workspace. A watch of the daemon
// itself, and a view this daemon serves, answer false.
func (s *server) relayTarget(ctx context.Context, watch pageWatch) (string, bool, error) {
	switch {
	case watch.Relay == nil:
		return "", false, nil
	case watch.Minter != "":
		return s.instanceTarget(watch.Name, watch.Minter)
	case watch.Workspace != "":
		return s.servingTarget(ctx, watch.Name, watch.Workspace)
	}
	return "", false, nil
}

// relayWatch serves one page subscription from the daemon at TARGET: it opens
// the dedicated rpc RPC there with REQ and writes every push into SINK, the
// page's own stream.
//
// THE PAGE'S STREAM AND THE WORKSPACE'S DAEMON ARE INDEPENDENT. At a deploy's
// handover the page's stream is in whichever daemon accepted it, and the
// workspace is served by whichever daemon holds its serving row; the relay is
// what lets a page attached anywhere draw a workspace served anywhere.
//
// THE TARGET'S ACCEPTANCE IS THE SUBSCRIPTION'S. The target states it in its
// response headers (AcceptedHeader), so SubscribePage answers when the
// target's subscription exists, exactly as it does for a body run here. A
// target that refuses the stream answers its refusal as the error, which is
// SubscribePage's own answer.
//
// THE TARGET HOLDS THE PAGE AS CONNECTED (RelayedPageHeader), so a workspace
// open running there sees the page connect and draw.
//
// A CLEAN END IS `moved`, never `source_ended`: the target ended the view
// without an error, so nothing failed and the page subscribes again.
func relayWatch[Req, Resp any](ctx context.Context, s *server, rpc, procedure, target, page string, req *Req, sink streamSink[Resp]) error {
	relayCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	client := connect.NewClient[Req, Resp](s.deps.Forward, "http://"+target+procedure)
	out := connect.NewRequest(req)
	out.Header().Set(ForwardedHeader, "1")
	out.Header().Set(RelayedPageHeader, page)
	fields := dlog.Context{"rpc": rpc, "target": target, "page": page}
	in, err := client.CallServerStream(relayCtx, out)
	if err != nil {
		s.log.Info("daemon.server.relay", "the daemon a page subscription is relayed from could not be asked", dlog.WithCause(fields, err))
		return err
	}
	defer in.Close()
	accepted := in.ResponseHeader().Get(AcceptedHeader) != ""
	if accepted {
		s.acceptStream(ctx, rpc)
	}
	for in.Receive() {
		if !accepted {
			// A push is itself the proof of a stream that stands.
			accepted = true
			s.acceptStream(ctx, rpc)
		}
		if err := sink.Send(in.Msg()); err != nil {
			return fmt.Errorf("relay %s from %s: %w", rpc, target, err)
		}
	}
	if err := in.Err(); err != nil {
		if ctx.Err() == nil {
			s.log.Info("daemon.server.relay", "the daemon a page subscription is relayed from ended it with an error", dlog.WithCause(fields, err))
		}
		return err
	}
	if ctx.Err() != nil {
		return ctx.Err()
	}
	s.log.Info("daemon.server.relay", "the daemon a page subscription is relayed from ended it cleanly; the page subscribes again", fields)
	return errSubscriptionMoved
}
