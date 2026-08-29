package main

import (
	"context"
	"fmt"
	"net"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
)

type connContextKey struct{}

// withConn stashes the accepted TCP connection on every request's context so
// /_fake/end {abort: true} can drop it WITHOUT writing an end frame — the
// "producer ended without a terminal frame" transport failure the elisp
// transport must detect (elisp.md, "Gotchas").
func withConn(ctx context.Context, c net.Conn) context.Context {
	return context.WithValue(ctx, connContextKey{}, c)
}

func connFrom(ctx context.Context) net.Conn {
	c, _ := ctx.Value(connContextKey{}).(net.Conn)
	return c
}

// serveStream is the body of every stream Emacs holds.  A subscription
// replays any stored snapshot, then stands until the client cancels or the
// control plane ends it — standing streams never conclude on their own.
func serveStream[Res any](ctx context.Context, s *fakeServer, name, workspaceID string, stream *connect.ServerStream[Res]) error {
	sub := s.addSubscriber(name, workspaceID, connFrom(ctx))
	defer s.removeSubscriber(sub)

	for {
		select {
		case <-ctx.Done():
			// The client cancelled: the graceful close on this contract.
			logInfo("fakedaemon.stream.client-cancelled", "client cancelled a stream",
				map[string]any{"id": sub.id, "stream": name, "workspace_id": workspaceID})
			return ctx.Err()
		case msg := <-sub.msgs:
			typed, ok := any(msg).(*Res)
			if !ok {
				logError("fakedaemon.stream.push-type-mismatch", "pushed message does not match the stream",
					map[string]any{"id": sub.id, "stream": name,
						"pushed": string(proto.MessageName(msg))})
				return connect.NewError(connect.CodeInternal,
					fmt.Errorf("fakedaemon: %s push has the wrong message type", name))
			}
			if err := stream.Send(typed); err != nil {
				logWarn("fakedaemon.stream.send-failed", "send to a stream subscriber failed",
					map[string]any{"id": sub.id, "stream": name, "error": err.Error()})
				return err
			}
			logDebug("fakedaemon.stream.sent", "delivered a push to a subscriber",
				map[string]any{"id": sub.id, "stream": name, "workspace_id": workspaceID})
		case req := <-sub.end:
			if req.abort {
				if sub.conn == nil {
					logError("fakedaemon.stream.abort-no-conn", "abort requested but no connection was captured",
						map[string]any{"id": sub.id, "stream": name})
					return connect.NewError(connect.CodeInternal,
						fmt.Errorf("fakedaemon: no connection captured for stream %d", sub.id))
				}
				logInfo("fakedaemon.stream.aborted", "dropping the TCP connection without an end frame",
					map[string]any{"id": sub.id, "stream": name})
				_ = sub.conn.Close()
				return errAborted
			}
			if req.err != nil {
				logInfo("fakedaemon.stream.end-with-error", "ending a stream with a Connect error",
					map[string]any{"id": sub.id, "stream": name, "code": req.err.Code().String()})
				return req.err
			}
			logInfo("fakedaemon.stream.end-clean", "ending a stream with a clean end frame",
				map[string]any{"id": sub.id, "stream": name})
			return nil
		}
	}
}

// connContext is the http.Server hook that captures each accepted connection.
func connContext(ctx context.Context, c net.Conn) context.Context {
	return withConn(ctx, c)
}
