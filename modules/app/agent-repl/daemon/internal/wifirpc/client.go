package wifirpc

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	persistentwifiv1 "agentrepl/proto/persistentwifi/v1"
	"agentrepl/proto/persistentwifi/v1/persistentwifiv1connect"

	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/h2cclient"
	"agent-repld/internal/publish"
)

// Operations the client's records carry.
const (
	opClientUpdate = "daemon.wifirpc.client.update"
	opClientWatch  = "daemon.wifirpc.client.watch"
)

// ErrUnavailable is a call that never reached the service or never came
// back: the service is down, restarting, or its socket is absent.
var ErrUnavailable = errors.New("wifirpc: the persistent-wifi service is unavailable")

// Deps are the client's collaborators.
type Deps struct {
	// RPC is the service's Connect client. REQUIRED.
	RPC persistentwifiv1connect.PersistentWifiClient
	// CloseIdle closes the idle connections RPC holds, so the next call dials
	// fresh: a connection that ended is never reused, since the service behind
	// it may be the one that is going away (Dial answers it). REQUIRED.
	CloseIdle func()
	// Log is the daemon's canonical logger. REQUIRED.
	Log dlog.Logger
	// OnStanding is handed every standing the service publishes, and every
	// service-unreachable standing the client publishes in its place, after
	// the client's own topic holds it: the topbar takes the standing through
	// it and the views follow its quiet state. REQUIRED.
	OnStanding func(*agentreplv1.PersistentWifiState)
	// Clock times the first-standing bound. REQUIRED.
	Clock clock.Clock
	// FirstStandingBound is how long a connection may go without serving its
	// first standing before that is recorded at ERROR. Zero is
	// DefaultFirstStandingBound.
	FirstStandingBound time.Duration
}

// DefaultFirstStandingBound bounds the time from a new connection to the
// service's unix socket to the first standing it serves. A connection made
// while the service starts queues on the held socket until the service has
// read the machine's standing, so the expected time is that read plus the
// connection: the live daemon's own record of 205 standing reads (the
// `took_ms` of `daemon.persistentwifi.probe`, 2026-11-25 to 2026-11-26) had
// a median of 40ms, a 99th percentile of 159ms and a worst of 412ms, and a
// connection to a serving Server took at worst 1.31ms over 300 fresh
// connections on 2026-11-26 under the test runner's CPU governor. Five times
// 413.3ms is 2.07s, rounded up to 2.1s. It bounds only the record: a
// connection that passes it keeps waiting on the held socket, never reopening.
const DefaultFirstStandingBound = 2100 * time.Millisecond

// Client is the daemon's view of the persistent-wifi service: the mode change
// passes through to it, and the standing it publishes is followed.
//
// THE DAEMON NEVER HOLDS A STANDING THE SERVICE DID NOT SERVE ON THE
// CONNECTION IT HOLDS. The client publishes a service-unreachable standing
// (agentrepl.v1.PersistentWifiState.service_unreachable) from its start (Start)
// until the first connection serves, and the moment a connection that served
// ends, before reopening, so the chip and the views never show a standing
// from before a restart or a crash. Nothing holds the machine quiet while the
// service is unreachable, so the unreachable standing carries no quiet state
// and the daemon's views lift.
//
// THE FOLLOW IS CONNECTION-SCOPED. Each connection begins with the current
// standing, so a service that restarted is read again from its first message.
// The socket is owned by a socket holder, so a connect made while the service
// restarts queues until the next one accepts, and there is no start-up ordering
// and no retry timer. The daemon dials only after its boot's standalone step
// has ensured that holder (cmd/agent-repld gates the loop), so the one dial
// that finds no socket is a holder that could not be installed. A connection that ends after serving at least one
// standing is reopened at once. One that never served any ends the follow with
// an error instead, since reopening it at once would only spin. A connection
// the service accepts but does not answer within the first-standing bound is
// recorded at ERROR once and kept, still unreachable, waiting on the socket.
type Client struct {
	rpc        persistentwifiv1connect.PersistentWifiClient
	closeIdle  func()
	log        dlog.Logger
	onStanding func(*agentreplv1.PersistentWifiState)
	clock      clock.Clock
	bound      time.Duration
	topic      publish.Topic[*agentreplv1.PersistentWifiState]
}

// New answers a client over deps.
func New(d Deps) (*Client, error) {
	switch {
	case d.RPC == nil:
		return nil, fmt.Errorf("wifirpc: a service client is required")
	case d.CloseIdle == nil:
		return nil, fmt.Errorf("wifirpc: a way to close idle connections is required")
	case d.Log == nil:
		return nil, fmt.Errorf("wifirpc: a logger is required")
	case d.OnStanding == nil:
		return nil, fmt.Errorf("wifirpc: an OnStanding function is required")
	case d.Clock == nil:
		return nil, fmt.Errorf("wifirpc: a clock is required")
	}
	bound := d.FirstStandingBound
	if bound == 0 {
		bound = DefaultFirstStandingBound
	}
	return &Client{rpc: d.RPC, closeIdle: d.CloseIdle, log: d.Log, onStanding: d.OnStanding, clock: d.Clock, bound: bound}, nil
}

// Dial answers a service client over the unix socket at socketPath, and the
// function that closes its idle connections. Nothing is dialed until the first
// call.
func Dial(socketPath string) (persistentwifiv1connect.PersistentWifiClient, func()) {
	transport := h2cclient.UnixTransport(socketPath, nil)
	return persistentwifiv1connect.NewPersistentWifiClient(&http.Client{Transport: transport}, "http://persistent-wifi.uds"), transport.CloseIdleConnections
}

// Topic is the standing the service last published, or the unreachable
// standing that replaced it, replayed to every subscriber (the Emacs
// WatchDaemon streams).
func (c *Client) Topic() *publish.Topic[*agentreplv1.PersistentWifiState] { return &c.topic }

// Start publishes the service-unreachable standing, so the boot serves a
// standing without waiting on the service: the first connection that serves
// replaces it.
func (c *Client) Start() {
	c.unreachable("the persistent-wifi service has not answered since this daemon started")
}

// Update passes one validated mode change through to the service and answers
// its response whole. A call that cannot reach the service is recorded and
// answered as ErrUnavailable wrapped in an Unavailable Connect error.
func (c *Client) Update(ctx context.Context, req *agentreplv1.UpdatePersistentWifiModeRequest) (*agentreplv1.UpdatePersistentWifiModeResponse, error) {
	resp, err := c.rpc.UpdateMode(ctx, connect.NewRequest(&persistentwifiv1.UpdateModeRequest{Request: req}))
	if err != nil {
		c.log.Error(opClientUpdate, "the persistent-wifi service could not change the mode", dlog.Context{"cause": err.Error(), "code": connect.CodeOf(err).String()})
		return nil, connect.NewError(connect.CodeUnavailable, fmt.Errorf("%w: %v", ErrUnavailable, err))
	}
	return resp.Msg.GetResponse(), nil
}

// Run follows the service's standing until ctx ends. Each connection ends
// when its stream does; see Client for what ends the follow instead.
func (c *Client) Run(ctx context.Context) error {
	for {
		served, answered, err := c.follow(ctx)
		if ctx.Err() != nil {
			return nil
		}
		// The next connection is a fresh one, never the one that ended.
		c.closeIdle()
		if !served && !answered {
			c.log.Error(opClientWatch, "the persistent-wifi service's standing watch ended before it served a standing; it is not reopened", dlog.Context{"cause": errString(err)})
			return fmt.Errorf("%w: %v", ErrUnavailable, errString(err))
		}
		c.unreachable("the persistent-wifi service's standing watch ended: " + errString(err))
		if !served {
			// A refusal from a service that is shutting down: the next
			// connection is made on a fresh dial, which queues on the held
			// socket for the service that replaces it.
			c.log.Debug(opClientWatch, "the persistent-wifi service refused the standing watch; it is reopened on a fresh connection", dlog.Context{"cause": errString(err)})
			continue
		}
		// A served watch ending is the service restarting under it (a deploy
		// replacing its binary ends every stream it serves) and the reopen is
		// the designed handover, so the record is INFO; the faults of the
		// follow (a connection that never serves) are the ERROR records above
		// and in watchFirst.
		c.log.Info(opClientWatch, "the persistent-wifi service's standing watch ended; its standing is unreachable until the watch is reopened and served", dlog.Context{"cause": errString(err)})
	}
}

// follow runs one connection: it hands every standing to take and answers
// whether it served any, whether the service answered at all (it refused the
// watch with Unavailable, as a draining service does), and the error that
// ended it.
func (c *Client) follow(ctx context.Context) (served, answered bool, err error) {
	ctx, cancel := context.WithCancel(ctx)
	first, watched := make(chan struct{}), make(chan struct{})
	// inflight: joined: the follow waits for it before it returns.
	go func() { defer close(watched); c.watchFirst(ctx, first) }()
	// The bound's watcher ends with the connection, so what it records is
	// written before the follow returns.
	defer func() { cancel(); <-watched }()
	stream, err := c.rpc.WatchStanding(ctx, connect.NewRequest(&persistentwifiv1.WatchStandingRequest{}))
	if err != nil {
		return false, false, err
	}
	defer func() { _ = stream.Close() }()
	for stream.Receive() {
		if !served {
			served = true
			close(first)
		}
		c.take(stream.Msg().GetStanding())
	}
	err = stream.Err()
	// A refusal from the service itself (headers came back) is an answer; a
	// dial that failed has none.
	return served, len(stream.ResponseHeader()) > 0, err
}

// watchFirst records, once, that a connection served no standing within the
// bound. The connection is kept: the service may be answering late, and its
// socket is held for it.
func (c *Client) watchFirst(ctx context.Context, first <-chan struct{}) {
	select {
	case <-first:
	case <-ctx.Done():
	case <-c.clock.After(c.bound):
		select {
		case <-first:
		case <-ctx.Done():
		default:
			c.log.Error(opClientWatch, "the persistent-wifi service served no standing within {bound}; the daemon keeps waiting on its socket, and its standing stays unreachable",
				dlog.Context{"bound": c.bound.String()})
		}
	}
}

// unreachable publishes the service-unreachable standing, with detail.
func (c *Client) unreachable(detail string) {
	c.take(&agentreplv1.PersistentWifiState{
		ServiceUnreachable: &agentreplv1.PersistentWifiServiceUnreachable{Detail: detail},
	})
}

// take publishes one standing and hands it to the receiver.
func (c *Client) take(standing *agentreplv1.PersistentWifiState) {
	c.topic.Publish(standing)
	c.onStanding(standing)
}

func errString(err error) string {
	if err == nil {
		return "the stream ended cleanly"
	}
	return err.Error()
}
