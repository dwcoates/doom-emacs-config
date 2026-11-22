// Package watchtest is an in-memory shim client for tests of the daemon's
// session watcher (shimclient.Client), with the streams it serves and the
// gates that hold an open.
//
// IT IS A PACKAGE, NOT A _test FILE, because two kinds of test drive the real
// watcher through it: the watcher's own unit tests, and the scenario goldens'
// in-process projection (internal/scenariogolden), which runs the real
// watcher and the real feed resolver with no daemon, shim process,
// repository or log file.
package watchtest

import (
	"context"
	"io"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/proflabel"
	"agent-repld/internal/shimclient"
)

// waitDeadline bounds every wait the fake makes on its test's behalf.
const waitDeadline = 5 * time.Second

// Stream is one shim stream driven by the test. Its frame channel is
// UNBUFFERED, which is what makes the tests deterministic without a sleep: a
// send returns only once the watcher has received the frame, and the watcher
// receives the next frame only after it has finished routing the previous one.
type Stream[T any] struct {
	Frames chan T
	Errs   chan error
	Closed chan struct{}
	Once   sync.Once
	// Drained is closed when Recv has answered io.EOF, which is the exact
	// moment the watcher's reader for this stream has finished routing
	// everything it was given: the loop only returns to Recv after routing and
	// its off-lock flush are done. It is how a test bounds a frame that reaps
	// its own stream -- see routeReaping.
	Drained     chan struct{}
	DrainedOnce sync.Once
	// BlockClose, when non-nil, holds Close until it is closed. It stands for
	// the real transport's Close, which drains the response body and does not
	// return until the SERVER ends the stream.
	BlockClose chan struct{}
}

func NewStream[T any]() *Stream[T] {
	return &Stream[T]{
		Frames:  make(chan T),
		Errs:    make(chan error, 1),
		Closed:  make(chan struct{}),
		Drained: make(chan struct{}),
	}
}

// Recv implements shimclient.Stream.
func (s *Stream[T]) Recv() (T, error) {
	var zero T
	select {
	case frame := <-s.Frames:
		return frame, nil
	case err := <-s.Errs:
		return zero, err
	case <-s.Closed:
		s.DrainedOnce.Do(func() { close(s.Drained) })
		return zero, io.EOF
	}
}

// Close implements shimclient.Stream.
func (s *Stream[T]) Close() {
	s.Once.Do(func() {
		if s.BlockClose != nil {
			<-s.BlockClose
		}
		close(s.Closed)
	})
}

// Send hands one frame to the watcher and returns once it has been received.
func (s *Stream[T]) Send(t *testing.T, frame T) {
	t.Helper()
	select {
	case s.Frames <- frame:
	case <-time.After(waitDeadline):
		t.Fatal("the watcher never received the frame")
	}
}

// Fail ends the stream with a transport error.
func (s *Stream[T]) Fail(err error) { s.Errs <- err }

// AwaitDrained waits until the watcher's reader for this stream has answered
// io.EOF, which it can only do once it has finished routing every frame it was
// handed.
func (s *Stream[T]) AwaitDrained(t *testing.T) {
	t.Helper()
	select {
	case <-s.Drained:
	case <-time.After(waitDeadline):
		t.Fatal("the watcher never finished the reaped stream")
	}
}

// IsClosed reports whether the watcher closed this stream.
func (s *Stream[T]) IsClosed() bool {
	select {
	case <-s.Closed:
		return true
	default:
		return false
	}
}

// ---- the fake shim client ----

type AgentOpen struct {
	Req    *shimv1.WatchAgentRequest
	Stream *Stream[*shimv1.WatchAgentResponse]
}

type BashOpen struct {
	Work   *conversationv1.DetachedWorkId
	Stream *Stream[*conversationv1.AgentBash]
}

// Client is a shimclient.Client whose watches are channel-backed streams
// the test drives. Only the three watch verbs and Connectivity are exercised;
// every other verb belongs to callers this package is not.
type Client struct {
	SessionOpens chan *Stream[*shimv1.WatchSessionResponse]
	// SessionLabel is the profiler label (internal/proflabel) the last
	// WatchSession's context carried, and SessionLabeled whether it carried one.
	SessionLabel   ids.WorkspaceID
	SessionLabeled bool
	AgentOpens     chan AgentOpen
	BashOpens      chan BashOpen
	Links          chan shimclient.LinkState
	// Conns is what Connections answers; an adopted client starts
	// connected once.
	Conns atomic.Uint64
	Exits chan shimclient.ExitInfo
	// RefusedOpens carries the procedure of every watch open the fake
	// answered with an error, the moment it answers. With the watcher's
	// opens made off its lock, it is how a test knows a refused open was
	// MADE before it joins the open's completion (awaitRefusedOpen).
	RefusedOpens chan string

	// StashedAgentOpens are opens nextAgentOpenFor read past while looking
	// for another target. Opens decided together are MADE concurrently, so
	// the order they reach agentOpens in means nothing; a test that wants one
	// by target takes it by target, and the rest stay queued in order. Read
	// and written by the test goroutine alone.
	StashedAgentOpens []AgentOpen
	// Settle joins every watch open the watcher has in flight. The harness
	// sets it, and the no-open assertions call it first, so they judge every
	// open the watcher decided rather than only the ones already made.
	Settle func()

	// SessionGate, agentGate and bashGate each hold the NEXT open of their
	// verb until released: a shim that never serves a stream's first frame.
	// Each is taken by the one open it holds (see openGate.hold).
	SessionGate *Gate
	AgentGate   *Gate
	BashGate    *Gate

	Mu           sync.Mutex
	AgentErr     error
	BashErr      error
	SessionCount int
	// ReapedExit is the decoded exit Reaped answers, nil when nothing was ReapedExit.
	ReapedExit *shimclient.ExitInfo
	// Exiting, when set, is the inferred exit Died answers for a process that
	// reads Exiting before its reap is in: the EOF-before-reap order, staged.
	Exiting *shimclient.ExitInfo
	// StandDownLatched is the shim's stand-down latch, which the real client sets
	// the moment a KillSession is asked of it.
	StandDownLatched bool
	// AskedOnly marks the latch raised by a KillSession still awaiting its
	// answer, which a refusal can undo: StandDownOrdered reads it false.
	AskedOnly bool
}

// standDown latches the fake as having been asked to end its session, which is
// what every route to a deliberate teardown does to the real client.
func (c *Client) StandDown() bool {
	c.Mu.Lock()
	defer c.Mu.Unlock()
	c.StandDownLatched = true
	return true
}

// StandingDown answers the stand-down latch.
func (c *Client) StandingDown() bool {
	c.Mu.Lock()
	defer c.Mu.Unlock()
	return c.StandDownLatched
}

// StandDownOrdered answers the fake's settled stand-down: its latch, unless
// a KillSession is only awaiting its answer (askedOnly).
func (c *Client) StandDownOrdered() bool {
	c.Mu.Lock()
	defer c.Mu.Unlock()
	return c.StandDownLatched && !c.AskedOnly
}

// SetReaped arranges the decoded exit a dead link's fault carries.
func (c *Client) SetReaped(info shimclient.ExitInfo) {
	c.Mu.Lock()
	defer c.Mu.Unlock()
	c.ReapedExit = &info
}

// Gate holds one watch open inside the fake's verb, the way a shim that
// never serves the stream's first frame holds shimclient.openStream.
type Gate struct {
	// Blocked is signalled the moment the open is being held.
	Blocked chan struct{}
	// Release lets the held open complete, successfully.
	Release chan struct{}
	// HonorCtx lets the open's context end the hold, as the real transport
	// does; without it only release does, which is how a test makes an open
	// COMPLETE after the watcher stopped wanting it.
	HonorCtx bool
}

func NewGate(honorCtx bool) *Gate {
	return &Gate{Blocked: make(chan struct{}, 1), Release: make(chan struct{}), HonorCtx: honorCtx}
}

// Hold blocks one open until it is released, or until ctx ends when the gate
// honors it. It answers the context's error for an open ended that way.
func (g *Gate) Hold(ctx context.Context) error {
	if g == nil {
		return nil
	}
	g.Blocked <- struct{}{}
	if !g.HonorCtx {
		<-g.Release
		return nil
	}
	select {
	case <-g.Release:
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

// AwaitBlocked waits until the gate is holding its open.
func (g *Gate) AwaitBlocked(t *testing.T) {
	t.Helper()
	select {
	case <-g.Blocked:
	case <-time.After(waitDeadline):
		t.Fatal("the gated open was never made")
	}
}

// TakeGate hands the caller the gate *slot holds, clearing it, so a gate holds
// exactly one open.
func (c *Client) TakeGate(slot **Gate) *Gate {
	c.Mu.Lock()
	defer c.Mu.Unlock()
	g := *slot
	*slot = nil
	return g
}

// GateIn arranges a GateIn for the next open of one verb and returns it.
func (c *Client) GateIn(slot **Gate, honorCtx bool) *Gate {
	g := NewGate(honorCtx)
	c.Mu.Lock()
	defer c.Mu.Unlock()
	*slot = g
	return g
}

func NewClient() *Client {
	c := &Client{
		SessionOpens: make(chan *Stream[*shimv1.WatchSessionResponse], 8),
		AgentOpens:   make(chan AgentOpen, 32),
		BashOpens:    make(chan BashOpen, 32),
		Links:        make(chan shimclient.LinkState, 8),
		Exits:        make(chan shimclient.ExitInfo),
		RefusedOpens: make(chan string, 64),
	}
	c.Conns.Store(1)
	return c
}

func (c *Client) WatchSession(ctx context.Context) (shimclient.Stream[*shimv1.WatchSessionResponse], error) {
	if err := c.TakeGate(&c.SessionGate).Hold(ctx); err != nil {
		return nil, err
	}
	stream := NewStream[*shimv1.WatchSessionResponse]()
	c.Mu.Lock()
	c.SessionCount++
	c.SessionLabel, c.SessionLabeled = proflabel.Of(ctx)
	c.Mu.Unlock()
	c.SessionOpens <- stream
	return stream, nil
}

func (c *Client) WatchAgent(ctx context.Context, req *shimv1.WatchAgentRequest) (shimclient.Stream[*shimv1.WatchAgentResponse], error) {
	if err := c.TakeGate(&c.AgentGate).Hold(ctx); err != nil {
		return nil, err
	}
	c.Mu.Lock()
	err := c.AgentErr
	c.Mu.Unlock()
	if err != nil {
		c.RefusedOpens <- "WatchAgent"
		return nil, err
	}
	stream := NewStream[*shimv1.WatchAgentResponse]()
	c.AgentOpens <- AgentOpen{Req: req, Stream: stream}
	return stream, nil
}

// SetAgentErr arranges the refusal WatchAgent answers with. The shim really
// does refuse a book its store has not registered yet.
func (c *Client) SetAgentErr(err error) {
	c.Mu.Lock()
	defer c.Mu.Unlock()
	c.AgentErr = err
}

// SetBashErr arranges the refusal WatchBash answers with. The shim really does
// refuse a handle its store holds no rows for yet.
func (c *Client) SetBashErr(err error) {
	c.Mu.Lock()
	defer c.Mu.Unlock()
	c.BashErr = err
}

func (c *Client) WatchBash(ctx context.Context, work *conversationv1.DetachedWorkId) (shimclient.Stream[*conversationv1.AgentBash], error) {
	if err := c.TakeGate(&c.BashGate).Hold(ctx); err != nil {
		return nil, err
	}
	c.Mu.Lock()
	err := c.BashErr
	c.Mu.Unlock()
	if err != nil {
		c.RefusedOpens <- "WatchBash"
		return nil, err
	}
	stream := NewStream[*conversationv1.AgentBash]()
	c.BashOpens <- BashOpen{Work: work, Stream: stream}
	return stream, nil
}

func (c *Client) Connectivity() <-chan shimclient.LinkState { return c.Links }

func (c *Client) Connections() uint64 { return c.Conns.Load() }

// LinkBack is the client re-establishing its link: the connection count
// advances before LinkConnected is published, as the real client's connected()
// does. A replayed bring-up transition is sent on links directly instead,
// because it announces no new connection.
func (c *Client) LinkBack() {
	c.Conns.Add(1)
	c.Links <- shimclient.LinkConnected
}

// NextAgentOpen returns the next WatchAgent the watcher opened, a stashed one
// first.
func (c *Client) NextAgentOpen(t *testing.T) AgentOpen {
	t.Helper()
	if len(c.StashedAgentOpens) > 0 {
		open := c.StashedAgentOpens[0]
		c.StashedAgentOpens = c.StashedAgentOpens[1:]
		return open
	}
	select {
	case open := <-c.AgentOpens:
		return open
	case <-time.After(waitDeadline):
		t.Fatal("no WatchAgent was opened")
		return AgentOpen{}
	}
}

// NextAgentOpenFor returns the next WatchAgent opened for target (empty for
// the main agent's unset target), stashing every other open it reads past for
// nextAgentOpen.
func (c *Client) NextAgentOpenFor(t *testing.T, target string) AgentOpen {
	t.Helper()
	for i, open := range c.StashedAgentOpens {
		if open.Req.GetTarget().GetValue() == target {
			c.StashedAgentOpens = append(c.StashedAgentOpens[:i:i], c.StashedAgentOpens[i+1:]...)
			return open
		}
	}
	deadline := time.After(waitDeadline)
	for {
		select {
		case open := <-c.AgentOpens:
			if open.Req.GetTarget().GetValue() == target {
				return open
			}
			c.StashedAgentOpens = append(c.StashedAgentOpens, open)
		case <-deadline:
			t.Fatalf("no WatchAgent was opened for %q", target)
			return AgentOpen{}
		}
	}
}

// AwaitRefusedOpen waits until the fake has answered one open of procedure
// with its arranged error, then joins every open in flight, so the watcher
// has ruled on the refusal by the time it returns.
func (c *Client) AwaitRefusedOpen(t *testing.T, procedure string) {
	t.Helper()
	select {
	case got := <-c.RefusedOpens:
		if got != procedure {
			t.Fatalf("refused open = %s, want %s", got, procedure)
		}
	case <-time.After(waitDeadline):
		t.Fatalf("no %s open was refused", procedure)
	}
	c.SettleOpens()
}

// SettleOpens joins every open the watcher has in flight.
func (c *Client) SettleOpens() {
	if c.Settle != nil {
		c.Settle()
	}
}

// NextBashOpen returns the next WatchBash the watcher opened.
func (c *Client) NextBashOpen(t *testing.T) BashOpen {
	t.Helper()
	select {
	case open := <-c.BashOpens:
		return open
	case <-time.After(waitDeadline):
		t.Fatal("no WatchBash was opened")
		return BashOpen{}
	}
}

// NextSessionOpen returns the next WatchSession the watcher opened.
func (c *Client) NextSessionOpen(t *testing.T) *Stream[*shimv1.WatchSessionResponse] {
	t.Helper()
	select {
	case stream := <-c.SessionOpens:
		return stream
	case <-time.After(waitDeadline):
		t.Fatal("no WatchSession was opened")
		return nil
	}
}

// NoAgentOpen asserts no further WatchAgent was opened. It reads what is
// already queued rather than waiting: every open the watcher makes is queued
// before the call that provoked it has been observed to finish.
func (c *Client) NoAgentOpen(t *testing.T) {
	t.Helper()
	c.SettleOpens()
	if len(c.StashedAgentOpens) > 0 {
		t.Fatalf("an unexpected WatchAgent was opened for %q", c.StashedAgentOpens[0].Req.GetTarget().GetValue())
	}
	select {
	case open := <-c.AgentOpens:
		t.Fatalf("an unexpected WatchAgent was opened for %q", open.Req.GetTarget().GetValue())
	default:
	}
}

// NoSessionOpen asserts no further WatchSession was opened.
func (c *Client) NoSessionOpen(t *testing.T) {
	t.Helper()
	c.SettleOpens()
	select {
	case <-c.SessionOpens:
		t.Fatal("an unexpected WatchSession was opened")
	default:
	}
}

// NoBashOpen asserts no further WatchBash was opened.
func (c *Client) NoBashOpen(t *testing.T) {
	t.Helper()
	c.SettleOpens()
	select {
	case open := <-c.BashOpens:
		t.Fatalf("an unexpected WatchBash was opened for %q", open.Work.GetValue())
	default:
	}
}

// The verbs the sessionwatcher never calls. A watcher that grew a write would
// fail here rather than reaching a real shim.
func (c *Client) StartSession(context.Context, *shimv1.StartSessionRequest) (*shimv1.StartSessionResponse, error) {
	panic("sessionwatcher must not call StartSession")
}

func (c *Client) SummarizeConversation(context.Context, *shimv1.SummarizeConversationRequest) (*shimv1.SummarizeConversationResponse, error) {
	panic("sessionwatcher must not call SummarizeConversation")
}

func (c *Client) SetSessionModel(context.Context, *shimv1.SetSessionModelRequest) (*shimv1.SetSessionModelResponse, error) {
	panic("sessionwatcher must not call SetSessionModel")
}

func (c *Client) SetSessionEffort(context.Context, *shimv1.SetSessionEffortRequest) (*shimv1.SetSessionEffortResponse, error) {
	panic("sessionwatcher must not call SetSessionEffort")
}

func (c *Client) SetSessionRemoteControl(context.Context, *shimv1.SetSessionRemoteControlRequest) (*shimv1.SetSessionRemoteControlResponse, error) {
	panic("sessionwatcher must not call SetSessionRemoteControl")
}

func (c *Client) SetSessionPermissionMode(context.Context, *shimv1.SetSessionPermissionModeRequest) (*shimv1.SetSessionPermissionModeResponse, error) {
	panic("sessionwatcher must not call SetSessionPermissionMode")
}

func (c *Client) RetryVendorErrorNow(context.Context, *shimv1.RetryVendorErrorNowRequest) (*shimv1.RetryVendorErrorNowResponse, error) {
	panic("sessionwatcher must not call RetryVendorErrorNow")
}

func (c *Client) NetworkChanged(context.Context, *shimv1.NetworkChangedRequest) (*shimv1.NetworkChangedResponse, error) {
	panic("sessionwatcher must not call NetworkChanged")
}

func (c *Client) ReviseResponse(context.Context, *shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
	panic("sessionwatcher must not call ReviseResponse")
}

func (c *Client) SendKeepalive(context.Context, *shimv1.SendKeepaliveRequest) (*shimv1.SendKeepaliveResponse, error) {
	panic("sessionwatcher must not call SendKeepalive")
}

func (c *Client) HoldParkedBridge(context.Context, *shimv1.HoldParkedBridgeRequest) (shimclient.Stream[*shimv1.HoldParkedBridgeResponse], error) {
	panic("sessionwatcher must not call HoldParkedBridge")
}

func (c *Client) Hibernate(context.Context, *shimv1.HibernateRequest) (*shimv1.HibernateResponse, error) {
	panic("sessionwatcher must not call Hibernate")
}

func (c *Client) KillSession(context.Context, *shimv1.KillSessionRequest) (*shimv1.KillSessionResponse, error) {
	panic("sessionwatcher must not call KillSession")
}

func (c *Client) StartTurn(context.Context, *shimv1.StartTurnRequest) (*shimv1.StartTurnResponse, error) {
	panic("sessionwatcher must not call StartTurn")
}

func (c *Client) UpdateAgent(context.Context, *shimv1.UpdateAgentRequest) (*shimv1.UpdateAgentResponse, error) {
	panic("sessionwatcher must not call UpdateAgent")
}

func (c *Client) KillTurn(context.Context, *shimv1.KillTurnRequest) (*shimv1.KillTurnResponse, error) {
	panic("sessionwatcher must not call KillTurn")
}

func (c *Client) RollBackSession(context.Context, *shimv1.RollBackSessionRequest) (*shimv1.RollBackSessionResponse, error) {
	panic("sessionwatcher must not call RollBackSession")
}

func (c *Client) StopBash(context.Context, *shimv1.StopBashRequest) (*shimv1.StopBashResponse, error) {
	panic("sessionwatcher must not call StopBash")
}

func (c *Client) DetachForeground(context.Context, *shimv1.DetachForegroundRequest) (*shimv1.DetachForegroundResponse, error) {
	panic("sessionwatcher must not call DetachForeground")
}

func (c *Client) ReadTranscripts(context.Context, *shimv1.ReadTranscriptsRequest) (*shimv1.ReadTranscriptsResponse, error) {
	panic("sessionwatcher must not call ReadTranscripts")
}

func (c *Client) ResolvePeerDelivery(context.Context, *shimv1.ResolvePeerDeliveryRequest) (*shimv1.ResolvePeerDeliveryResponse, error) {
	panic("sessionwatcher must not call ResolvePeerDelivery")
}

func (c *Client) ResolveVendorTask(context.Context, *shimv1.ResolveVendorTaskRequest) (*shimv1.ResolveVendorTaskResponse, error) {
	panic("sessionwatcher must not call ResolveVendorTask")
}

func (c *Client) GetForkPoint(context.Context, *shimv1.GetForkPointRequest) (*shimv1.GetForkPointResponse, error) {
	panic("sessionwatcher must not call GetForkPoint")
}

func (c *Client) ForkSession(context.Context, *shimv1.ForkSessionRequest) (*shimv1.ForkSessionResponse, error) {
	panic("sessionwatcher must not call ForkSession")
}

func (c *Client) Occupy(string) (func(), error) { panic("sessionwatcher must not take the lease") }
func (c *Client) Kill(context.Context, shimclient.KillAttribution) error {
	panic("sessionwatcher must never kill: attach ends nothing")
}
func (c *Client) Detach()                            { panic("sessionwatcher must not detach the client") }
func (c *Client) Exited() <-chan shimclient.ExitInfo { return c.Exits }
func (c *Client) PID() int                           { return 4242 }

// Died answers the reaped exit, or the staged exiting one when no reap is in.
func (c *Client) Died() (shimclient.ExitInfo, bool) {
	if info, ok := c.Reaped(); ok {
		return info, true
	}
	c.Mu.Lock()
	defer c.Mu.Unlock()
	if c.Exiting == nil {
		return shimclient.ExitInfo{}, false
	}
	return *c.Exiting, true
}

// Reaped answers the decoded exit the fake was told to hold.
func (c *Client) Reaped() (shimclient.ExitInfo, bool) {
	c.Mu.Lock()
	defer c.Mu.Unlock()
	if c.ReapedExit == nil {
		return shimclient.ExitInfo{}, false
	}
	return *c.ReapedExit, true
}
