package main

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"os"
	"sync"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	"agentrepl/proto/agentrepl/v1/agentreplv1connect"

	"agent-repld/internal/h2cclient"

	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/stateroot"
)

// The verbs that CALL the serving daemon (deploy, merge-queue, call) share these two
// steps: find the address the daemon advertises in its state root, and dial it.

// newDaemonClient is a client for the daemon at a loopback address: h2c on
// the daemon's one origin.
func newDaemonClient(address string, options ...connect.ClientOption) agentreplv1connect.AgentReplClient {
	return newRedirectingClient(h2cClient().Transport, address, os.Stderr, options...)
}

// newRedirectingClient is a client for the daemon at ADDRESS over BASE that
// FOLLOWS A HANDOVER (2026-11-10): a unary call a daemon answers with its
// `transferring_away` refusal, naming the successor it handed the workspace
// to, is sent once more, unchanged, to that successor, and every later call
// of the client goes there too. During a rolling deploy the advertised daemon
// is the incumbent until every workspace has moved, while a workspace it has
// already handed over is the successor's; without this, every verb a mid-turn
// workspace ran was refused for the whole handover, so it could not store its
// links, tell its parent or finish. A refusal is a call that did nothing, so
// sending it again is safe; a successor that is itself handing over is
// answered as refused, never chased further. NOTE tells the caller of the
// resend.
func newRedirectingClient(base http.RoundTripper, address string, note io.Writer, options ...connect.ClientOption) agentreplv1connect.AgentReplClient {
	redirect := &successorRedirect{base: base, note: note}
	options = append(options, connect.WithInterceptors(redirect.interceptor()))
	return agentreplv1connect.NewAgentReplClient(&http.Client{Transport: redirect}, "http://"+address, options...)
}

// successorRedirect sends a client's calls to the successor a handover named,
// once one has.
type successorRedirect struct {
	base http.RoundTripper
	note io.Writer

	mu   sync.Mutex
	host string
}

// RoundTrip sends REQ to the successor when a handover named one, and where
// it was addressed otherwise.
func (r *successorRedirect) RoundTrip(req *http.Request) (*http.Response, error) {
	r.mu.Lock()
	host := r.host
	r.mu.Unlock()
	if host != "" {
		req = req.Clone(req.Context())
		req.URL.Host, req.Host = host, host
	}
	return r.base.RoundTrip(req)
}

// interceptor resends a unary call refused as transferring away to the
// successor the refusal names, once per client.
func (r *successorRedirect) interceptor() connect.UnaryInterceptorFunc {
	return func(next connect.UnaryFunc) connect.UnaryFunc {
		return func(ctx context.Context, req connect.AnyRequest) (connect.AnyResponse, error) {
			resp, err := next(ctx, req)
			if err != nil || resp == nil {
				return resp, err
			}
			msg, ok := resp.Any().(proto.Message)
			if !ok {
				return resp, err
			}
			successor := transferredTo(msg.ProtoReflect(), 0)
			if successor == "" {
				return resp, err
			}
			r.mu.Lock()
			followed := r.host != ""
			if !followed {
				r.host = successor
			}
			r.mu.Unlock()
			if followed {
				return resp, err
			}
			fmt.Fprintf(r.note, "agent-repld: the daemon is handing over; sending the call to its successor at %s\n", successor)
			return next(ctx, req)
		}
	}
}

// maxRefusalDepth bounds the search for a refusal in an answer: an answer's
// error arm sits a level or two below its root.
const maxRefusalDepth = 4

// transferredTo answers the successor address of the `transferring_away`
// refusal MSG carries, or "" when it carries none: every answer of the
// contract names that refusal the same way, a `transferring_away` message
// whose `address` is the successor.
func transferredTo(msg protoreflect.Message, depth int) string {
	if depth > maxRefusalDepth {
		return ""
	}
	var found string
	msg.Range(func(fd protoreflect.FieldDescriptor, v protoreflect.Value) bool {
		if fd.Kind() != protoreflect.MessageKind || fd.IsList() || fd.IsMap() {
			return true
		}
		sub := v.Message()
		if fd.Name() == "transferring_away" {
			if address := sub.Descriptor().Fields().ByName("address"); address != nil && address.Kind() == protoreflect.StringKind {
				found = sub.Get(address).String()
			}
			return found == ""
		}
		found = transferredTo(sub, depth+1)
		return found == ""
	})
	return found
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
