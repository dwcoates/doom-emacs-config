package main

import (
	"agent-repld/internal/h2cclient"
	"context"
	"fmt"
	"net/http"

	"connectrpc.com/connect"

	"agentrepl/proto/agentrepl/v1/agentreplv1connect"

	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/stateroot"
)

// The verbs that CALL the serving daemon (deploy, merge-queue, call) share these two
// steps: find the address the daemon advertises in its state root, and dial it.

// newDaemonClient is a client for the daemon at a loopback address: h2c on
// the daemon's one origin.
func newDaemonClient(address string, options ...connect.ClientOption) agentreplv1connect.AgentReplClient {
	return agentreplv1connect.NewAgentReplClient(h2cClient(), "http://"+address, options...)
}

// h2cClient is the transport every verb dials the daemon with: HTTP/2 in
// cleartext over the loopback.
func h2cClient() *http.Client {
	return h2cclient.Client()
}

// daemonStream is a server stream a verb reads from the daemon, opened on a
// context of its own. A verb that stops reading early must END the stream
// before closing it: connect's Close drains the response body (up to 4MiB)
// before it closes, and a standing stream (the roster, a footer) never ends on
// its own, so a Close on a live stream blocks until the daemon has pushed
// 4MiB. Close here cancels the stream's context first, so the drain ends at
// once whether the stream was read to its end or left early.
type daemonStream[Res any] struct {
	*connect.ServerStreamForClient[Res]
	cancel context.CancelFunc
}

// Close ends the stream's context and then closes the stream.
func (s daemonStream[Res]) Close() error {
	s.cancel()
	return s.ServerStreamForClient.Close()
}

// openDaemonStream opens a server stream with OPEN, one of the daemon
// client's server-streaming methods, on a context of the stream's own. Every
// server stream a verb reads is opened here, never by calling the method
// directly, so no reader can close a live stream without ending it first.
func openDaemonStream[Req, Res any](
	ctx context.Context,
	open func(context.Context, *connect.Request[Req]) (*connect.ServerStreamForClient[Res], error),
	req *Req,
) (daemonStream[Res], error) {
	ctx, cancel := context.WithCancel(ctx)
	stream, err := open(ctx, connect.NewRequest(req))
	if err != nil {
		cancel()
		return daemonStream[Res]{}, err
	}
	return daemonStream[Res]{ServerStreamForClient: stream, cancel: cancel}, nil
}

// servingAddress answers the address the state root's daemon.addr advertises.
// No file, or a file naming no address, is NO DAEMON SERVING, and says so.
func servingAddress(layout stateroot.Layout) (string, error) {
	advert, err := daemonaddr.ReadAdvertisement(layout.DaemonAddr())
	if err != nil {
		return "", fmt.Errorf("no daemon is serving: read %s: %w", layout.DaemonAddr(), err)
	}
	if advert.Address == "" {
		return "", fmt.Errorf("no daemon is serving: %s names no address", layout.DaemonAddr())
	}
	return advert.Address, nil
}
