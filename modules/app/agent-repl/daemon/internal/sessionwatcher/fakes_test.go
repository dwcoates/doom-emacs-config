package sessionwatcher

import (
	"context"
	"errors"
	"io"
	"strconv"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/lockwatch"
	"claude-repld/internal/shimclient"
)

// waitDeadline is how long a test waits for a frame to reach a sink before it
// declares the watcher wedged. It is a FAILURE DEADLINE, never a
// synchronization device: every wait below returns the moment its event
// arrives.
const waitDeadline = 5 * time.Second

// ---- channel-backed streams ----

// fakeStream is one shim stream driven by the test. Its frame channel is
// UNBUFFERED, which is what makes the tests deterministic without a sleep: a
// send returns only once the watcher has received the frame, and the watcher
// receives the next frame only after it has finished routing the previous one.
type fakeStream[T any] struct {
	frames chan T
	errs   chan error
	closed chan struct{}
	once   sync.Once
	// drained is closed when Recv has answered io.EOF, which is the exact
	// moment the watcher's reader for this stream has finished routing
	// everything it was given: the loop only returns to Recv after routing and
	// its off-lock flush are done. It is how a test bounds a frame that reaps
	// its own stream -- see routeReaping.
	drained     chan struct{}
	drainedOnce sync.Once
	// blockClose, when non-nil, holds Close until it is closed. It stands for
	// the real transport's Close, which drains the response body and does not
	// return until the SERVER ends the stream.
	blockClose chan struct{}
}

func newFakeStream[T any]() *fakeStream[T] {
	return &fakeStream[T]{
		frames:  make(chan T),
		errs:    make(chan error, 1),
		closed:  make(chan struct{}),
		drained: make(chan struct{}),
	}
}

// Recv implements shimclient.Stream.
func (s *fakeStream[T]) Recv() (T, error) {
	var zero T
	select {
	case frame := <-s.frames:
		return frame, nil
	case err := <-s.errs:
		return zero, err
	case <-s.closed:
		s.drainedOnce.Do(func() { close(s.drained) })
		return zero, io.EOF
	}
}

// Close implements shimclient.Stream.
func (s *fakeStream[T]) Close() {
	s.once.Do(func() {
		if s.blockClose != nil {
			<-s.blockClose
		}
		close(s.closed)
	})
}

// send hands one frame to the watcher and returns once it has been received.
func (s *fakeStream[T]) send(t *testing.T, frame T) {
	t.Helper()
	select {
	case s.frames <- frame:
	case <-time.After(waitDeadline):
		t.Fatal("the watcher never received the frame")
	}
}

// fail ends the stream with a transport error.
func (s *fakeStream[T]) fail(err error) { s.errs <- err }

// awaitDrained waits until the watcher's reader for this stream has answered
// io.EOF, which it can only do once it has finished routing every frame it was
// handed.
func (s *fakeStream[T]) awaitDrained(t *testing.T) {
	t.Helper()
	select {
	case <-s.drained:
	case <-time.After(waitDeadline):
		t.Fatal("the watcher never finished the reaped stream")
	}
}

// isClosed reports whether the watcher closed this stream.
func (s *fakeStream[T]) isClosed() bool {
	select {
	case <-s.closed:
		return true
	default:
		return false
	}
}

// ---- the fake shim client ----

type agentOpen struct {
	req    *shimv1.WatchAgentRequest
	stream *fakeStream[*shimv1.WatchAgentResponse]
}

type bashOpen struct {
	work   *conversationv1.DetachedWorkId
	stream *fakeStream[*conversationv1.AgentBash]
}

// fakeClient is a shimclient.Client whose watches are channel-backed streams
// the test drives. Only the three watch verbs and Connectivity are exercised;
// every other verb belongs to callers this package is not.
type fakeClient struct {
	sessionOpens chan *fakeStream[*shimv1.WatchSessionResponse]
	agentOpens   chan agentOpen
	bashOpens    chan bashOpen
	links        chan shimclient.LinkState
	// connections is what Connections answers; an adopted client starts
	// connected once.
	connections atomic.Uint64
	exits       chan shimclient.ExitInfo
	// refusedOpens carries the procedure of every watch open the fake
	// answered with an error, the moment it answers. With the watcher's
	// opens made off its lock, it is how a test knows a refused open was
	// MADE before it joins the open's completion (awaitRefusedOpen).
	refusedOpens chan string

	// stashedAgentOpens are opens nextAgentOpenFor read past while looking
	// for another target. Opens decided together are MADE concurrently, so
	// the order they reach agentOpens in means nothing; a test that wants one
	// by target takes it by target, and the rest stay queued in order. Read
	// and written by the test goroutine alone.
	stashedAgentOpens []agentOpen
	// settle joins every watch open the watcher has in flight. The harness
	// sets it, and the no-open assertions call it first, so they judge every
	// open the watcher decided rather than only the ones already made.
	settle func()

	// sessionGate, agentGate and bashGate each hold the NEXT open of their
	// verb until released: a shim that never serves a stream's first frame.
	// Each is taken by the one open it holds (see openGate.hold).
	sessionGate *openGate
	agentGate   *openGate
	bashGate    *openGate

	mu           sync.Mutex
	agentErr     error
	bashErr      error
	sessionCount int
	// reaped is the decoded exit Reaped answers, nil when nothing was reaped.
	reaped *shimclient.ExitInfo
	// standingDown is the shim's stand-down latch, which the real client sets
	// the moment a KillSession is asked of it.
	standingDown bool
}

// standDown latches the fake as having been asked to end its session, which is
// what every route to a deliberate teardown does to the real client.
func (c *fakeClient) StandDown() bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.standingDown = true
	return true
}

// StandingDown answers the stand-down latch.
func (c *fakeClient) StandingDown() bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.standingDown
}

// setReaped arranges the decoded exit a dead link's fault carries.
func (c *fakeClient) setReaped(info shimclient.ExitInfo) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.reaped = &info
}

// openGate holds one watch open inside the fake's verb, the way a shim that
// never serves the stream's first frame holds shimclient.openStream.
type openGate struct {
	// blocked is signalled the moment the open is being held.
	blocked chan struct{}
	// release lets the held open complete, successfully.
	release chan struct{}
	// honorCtx lets the open's context end the hold, as the real transport
	// does; without it only release does, which is how a test makes an open
	// COMPLETE after the watcher stopped wanting it.
	honorCtx bool
}

func newOpenGate(honorCtx bool) *openGate {
	return &openGate{blocked: make(chan struct{}, 1), release: make(chan struct{}), honorCtx: honorCtx}
}

// hold blocks one open until it is released, or until ctx ends when the gate
// honors it. It answers the context's error for an open ended that way.
func (g *openGate) hold(ctx context.Context) error {
	if g == nil {
		return nil
	}
	g.blocked <- struct{}{}
	if !g.honorCtx {
		<-g.release
		return nil
	}
	select {
	case <-g.release:
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

// awaitBlocked waits until the gate is holding its open.
func (g *openGate) awaitBlocked(t *testing.T) {
	t.Helper()
	select {
	case <-g.blocked:
	case <-time.After(waitDeadline):
		t.Fatal("the gated open was never made")
	}
}

// takeGate hands the caller the gate *slot holds, clearing it, so a gate holds
// exactly one open.
func (c *fakeClient) takeGate(slot **openGate) *openGate {
	c.mu.Lock()
	defer c.mu.Unlock()
	g := *slot
	*slot = nil
	return g
}

// gate arranges a gate for the next open of one verb and returns it.
func (c *fakeClient) gate(slot **openGate, honorCtx bool) *openGate {
	g := newOpenGate(honorCtx)
	c.mu.Lock()
	defer c.mu.Unlock()
	*slot = g
	return g
}

func newFakeClient() *fakeClient {
	c := &fakeClient{
		sessionOpens: make(chan *fakeStream[*shimv1.WatchSessionResponse], 8),
		agentOpens:   make(chan agentOpen, 32),
		bashOpens:    make(chan bashOpen, 32),
		links:        make(chan shimclient.LinkState, 8),
		exits:        make(chan shimclient.ExitInfo),
		refusedOpens: make(chan string, 64),
	}
	c.connections.Store(1)
	return c
}

func (c *fakeClient) WatchSession(ctx context.Context) (shimclient.Stream[*shimv1.WatchSessionResponse], error) {
	if err := c.takeGate(&c.sessionGate).hold(ctx); err != nil {
		return nil, err
	}
	stream := newFakeStream[*shimv1.WatchSessionResponse]()
	c.mu.Lock()
	c.sessionCount++
	c.mu.Unlock()
	c.sessionOpens <- stream
	return stream, nil
}

func (c *fakeClient) WatchAgent(ctx context.Context, req *shimv1.WatchAgentRequest) (shimclient.Stream[*shimv1.WatchAgentResponse], error) {
	if err := c.takeGate(&c.agentGate).hold(ctx); err != nil {
		return nil, err
	}
	c.mu.Lock()
	err := c.agentErr
	c.mu.Unlock()
	if err != nil {
		c.refusedOpens <- "WatchAgent"
		return nil, err
	}
	stream := newFakeStream[*shimv1.WatchAgentResponse]()
	c.agentOpens <- agentOpen{req: req, stream: stream}
	return stream, nil
}

// setAgentErr arranges the refusal WatchAgent answers with. The shim really
// does refuse a book its store has not registered yet.
func (c *fakeClient) setAgentErr(err error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.agentErr = err
}

// setBashErr arranges the refusal WatchBash answers with. The shim really does
// refuse a handle its store holds no rows for yet.
func (c *fakeClient) setBashErr(err error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.bashErr = err
}

func (c *fakeClient) WatchBash(ctx context.Context, work *conversationv1.DetachedWorkId) (shimclient.Stream[*conversationv1.AgentBash], error) {
	if err := c.takeGate(&c.bashGate).hold(ctx); err != nil {
		return nil, err
	}
	c.mu.Lock()
	err := c.bashErr
	c.mu.Unlock()
	if err != nil {
		c.refusedOpens <- "WatchBash"
		return nil, err
	}
	stream := newFakeStream[*conversationv1.AgentBash]()
	c.bashOpens <- bashOpen{work: work, stream: stream}
	return stream, nil
}

func (c *fakeClient) Connectivity() <-chan shimclient.LinkState { return c.links }

func (c *fakeClient) Connections() uint64 { return c.connections.Load() }

// linkBack is the client re-establishing its link: the connection count
// advances before LinkConnected is published, as the real client's connected()
// does. A replayed bring-up transition is sent on links directly instead,
// because it announces no new connection.
func (c *fakeClient) linkBack() {
	c.connections.Add(1)
	c.links <- shimclient.LinkConnected
}

// nextAgentOpen returns the next WatchAgent the watcher opened, a stashed one
// first.
func (c *fakeClient) nextAgentOpen(t *testing.T) agentOpen {
	t.Helper()
	if len(c.stashedAgentOpens) > 0 {
		open := c.stashedAgentOpens[0]
		c.stashedAgentOpens = c.stashedAgentOpens[1:]
		return open
	}
	select {
	case open := <-c.agentOpens:
		return open
	case <-time.After(waitDeadline):
		t.Fatal("no WatchAgent was opened")
		return agentOpen{}
	}
}

// nextAgentOpenFor returns the next WatchAgent opened for target (empty for
// the main agent's unset target), stashing every other open it reads past for
// nextAgentOpen.
func (c *fakeClient) nextAgentOpenFor(t *testing.T, target string) agentOpen {
	t.Helper()
	for i, open := range c.stashedAgentOpens {
		if open.req.GetTarget().GetValue() == target {
			c.stashedAgentOpens = append(c.stashedAgentOpens[:i:i], c.stashedAgentOpens[i+1:]...)
			return open
		}
	}
	deadline := time.After(waitDeadline)
	for {
		select {
		case open := <-c.agentOpens:
			if open.req.GetTarget().GetValue() == target {
				return open
			}
			c.stashedAgentOpens = append(c.stashedAgentOpens, open)
		case <-deadline:
			t.Fatalf("no WatchAgent was opened for %q", target)
			return agentOpen{}
		}
	}
}

// awaitRefusedOpen waits until the fake has answered one open of procedure
// with its arranged error, then joins every open in flight, so the watcher
// has ruled on the refusal by the time it returns.
func (c *fakeClient) awaitRefusedOpen(t *testing.T, procedure string) {
	t.Helper()
	select {
	case got := <-c.refusedOpens:
		if got != procedure {
			t.Fatalf("refused open = %s, want %s", got, procedure)
		}
	case <-time.After(waitDeadline):
		t.Fatalf("no %s open was refused", procedure)
	}
	c.settleOpens()
}

// settleOpens joins every open the watcher has in flight.
func (c *fakeClient) settleOpens() {
	if c.settle != nil {
		c.settle()
	}
}

// nextBashOpen returns the next WatchBash the watcher opened.
func (c *fakeClient) nextBashOpen(t *testing.T) bashOpen {
	t.Helper()
	select {
	case open := <-c.bashOpens:
		return open
	case <-time.After(waitDeadline):
		t.Fatal("no WatchBash was opened")
		return bashOpen{}
	}
}

// nextSessionOpen returns the next WatchSession the watcher opened.
func (c *fakeClient) nextSessionOpen(t *testing.T) *fakeStream[*shimv1.WatchSessionResponse] {
	t.Helper()
	select {
	case stream := <-c.sessionOpens:
		return stream
	case <-time.After(waitDeadline):
		t.Fatal("no WatchSession was opened")
		return nil
	}
}

// noAgentOpen asserts no further WatchAgent was opened. It reads what is
// already queued rather than waiting: every open the watcher makes is queued
// before the call that provoked it has been observed to finish.
func (c *fakeClient) noAgentOpen(t *testing.T) {
	t.Helper()
	c.settleOpens()
	if len(c.stashedAgentOpens) > 0 {
		t.Fatalf("an unexpected WatchAgent was opened for %q", c.stashedAgentOpens[0].req.GetTarget().GetValue())
	}
	select {
	case open := <-c.agentOpens:
		t.Fatalf("an unexpected WatchAgent was opened for %q", open.req.GetTarget().GetValue())
	default:
	}
}

// noSessionOpen asserts no further WatchSession was opened.
func (c *fakeClient) noSessionOpen(t *testing.T) {
	t.Helper()
	c.settleOpens()
	select {
	case <-c.sessionOpens:
		t.Fatal("an unexpected WatchSession was opened")
	default:
	}
}

// noBashOpen asserts no further WatchBash was opened.
func (c *fakeClient) noBashOpen(t *testing.T) {
	t.Helper()
	c.settleOpens()
	select {
	case open := <-c.bashOpens:
		t.Fatalf("an unexpected WatchBash was opened for %q", open.work.GetValue())
	default:
	}
}

// The verbs the sessionwatcher never calls. A watcher that grew a write would
// fail here rather than reaching a real shim.
func (c *fakeClient) StartSession(context.Context, *shimv1.StartSessionRequest) (*shimv1.StartSessionResponse, error) {
	panic("sessionwatcher must not call StartSession")
}

func (c *fakeClient) SetSessionModel(context.Context, *shimv1.SetSessionModelRequest) (*shimv1.SetSessionModelResponse, error) {
	panic("sessionwatcher must not call SetSessionModel")
}

func (c *fakeClient) SetSessionPermissionMode(context.Context, *shimv1.SetSessionPermissionModeRequest) (*shimv1.SetSessionPermissionModeResponse, error) {
	panic("sessionwatcher must not call SetSessionPermissionMode")
}

func (c *fakeClient) Hibernate(context.Context, *shimv1.HibernateRequest) (*shimv1.HibernateResponse, error) {
	panic("sessionwatcher must not call Hibernate")
}

func (c *fakeClient) KillSession(context.Context, *shimv1.KillSessionRequest) (*shimv1.KillSessionResponse, error) {
	panic("sessionwatcher must not call KillSession")
}

func (c *fakeClient) StartTurn(context.Context, *shimv1.StartTurnRequest) (*shimv1.StartTurnResponse, error) {
	panic("sessionwatcher must not call StartTurn")
}

func (c *fakeClient) UpdateAgent(context.Context, *shimv1.UpdateAgentRequest) (*shimv1.UpdateAgentResponse, error) {
	panic("sessionwatcher must not call UpdateAgent")
}

func (c *fakeClient) KillTurn(context.Context, *shimv1.KillTurnRequest) (*shimv1.KillTurnResponse, error) {
	panic("sessionwatcher must not call KillTurn")
}

func (c *fakeClient) StopBash(context.Context, *shimv1.StopBashRequest) (*shimv1.StopBashResponse, error) {
	panic("sessionwatcher must not call StopBash")
}

func (c *fakeClient) DetachForeground(context.Context, *shimv1.DetachForegroundRequest) (*shimv1.DetachForegroundResponse, error) {
	panic("sessionwatcher must not call DetachForeground")
}

func (c *fakeClient) ReadHistory(context.Context, *shimv1.ReadHistoryRequest) (*shimv1.ReadHistoryResponse, error) {
	panic("sessionwatcher must not call ReadHistory")
}

func (c *fakeClient) ReadTranscripts(context.Context, *shimv1.ReadTranscriptsRequest) (*shimv1.ReadTranscriptsResponse, error) {
	panic("sessionwatcher must not call ReadTranscripts")
}

func (c *fakeClient) GatherTitleDigest(context.Context, *shimv1.GatherTitleDigestRequest) (*shimv1.GatherTitleDigestResponse, error) {
	panic("sessionwatcher must not call GatherTitleDigest")
}

func (c *fakeClient) Occupy(string) (func(), error) { panic("sessionwatcher must not take the lease") }
func (c *fakeClient) Kill(context.Context, shimclient.KillAttribution) error {
	panic("sessionwatcher must never kill: attach ends nothing")
}
func (c *fakeClient) Detach()                            { panic("sessionwatcher must not detach the client") }
func (c *fakeClient) Exited() <-chan shimclient.ExitInfo { return c.exits }
func (c *fakeClient) PID() int                           { return 4242 }

// Reaped answers the decoded exit the fake was told to hold.
func (c *fakeClient) Reaped() (shimclient.ExitInfo, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.reaped == nil {
		return shimclient.ExitInfo{}, false
	}
	return *c.reaped, true
}

// ---- recording sinks ----

// event is one sink call, as the recorder saw it.
type event struct {
	sink   string
	method string
	agent  string
	detail string
	turn   *ids.TurnID
	close  TurnClose
	// turns is the lifecycle sink's batch of turns that ended unobserved.
	turns []ids.TurnID
	live  *LiveWorkSet
	note  *HostNotification
	link  LinkState
	// attached is the lifecycle sink's bare shim-attachment edge.
	attached *bool
	// linkFault is the lost-link evidence the lifecycle sink was handed.
	linkFault *LinkFault
	// refusal is the refused-open evidence the lifecycle sink was handed.
	refusal *WatchOpenRefusal
	// boundary is the boundary arm of a page the feed was handed: "floor",
	// "more" or "" for none.
	boundary string
}

// name is the "sink.Method" spelling the assertions compare on.
func (e event) name() string { return e.sink + "." + e.method }

// recorder is the ordered record of every sink call, on a channel so a test
// waits for routing to finish rather than sleeping through it.
type recorder struct {
	ch chan event
	// frees carries every OnFree edge on a channel of its own. The edge is
	// told on a goroutine, so its arrival relative to the ordered sink calls
	// is not fixed, and folding it into ch would make every sequence
	// assertion depend on scheduling.
	frees chan ids.WorkspaceID
	// departures carries every OnDeparted edge, on a channel of its own for
	// the same reason.
	departures chan departedEdge

	// mu guards mains.
	mu sync.Mutex
	// mains is every main-agent naming the views were given, as
	// "<sink>:<agent>", in order.
	mains []string
}

func newRecorder() *recorder {
	return &recorder{
		ch:         make(chan event, 512),
		frees:      make(chan ids.WorkspaceID, 64),
		departures: make(chan departedEdge, 64),
	}
}

func (r *recorder) emit(e event) { r.ch <- e }

// nameMain records one main-agent naming a view was given.
func (r *recorder) nameMain(sink, agent string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.mains = append(r.mains, sink+":"+agent)
}

// mainNamings answers every main-agent naming the views were given so far.
func (r *recorder) mainNamings() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]string(nil), r.mains...)
}

// until reads events until the named one arrives and returns everything
// BEFORE it. Sending a sentinel frame down the same stream after the frame
// under test is what bounds a routing assertion: the stream's frames are
// consumed one at a time, so the sentinel cannot be routed until the frame
// before it has been.
func (r *recorder) until(t *testing.T, name string) []event {
	t.Helper()
	return r.untilEvent(t, name, func(e event) bool { return e.name() == name })
}

// untilEvent is until for the first event the predicate accepts, which is how
// a sentinel is told from a call of the same name the frame under test made.
func (r *recorder) untilEvent(t *testing.T, name string, is func(event) bool) []event {
	t.Helper()
	var seen []event
	for {
		select {
		case e := <-r.ch:
			if is(e) {
				return seen
			}
			seen = append(seen, e)
		case <-time.After(waitDeadline):
			t.Fatalf("the sentinel %s never arrived; saw %v", name, names(seen))
			return nil
		}
	}
}

// drain returns every event already recorded, without waiting for another. It
// is for a NEGATIVE assertion whose subject has already happened: the
// watcher's start queues every sink call it made before Start returned.
func (r *recorder) drain() []event {
	var seen []event
	for {
		select {
		case e := <-r.ch:
			seen = append(seen, e)
		default:
			return seen
		}
	}
}

// names renders a run of events for an assertion message.
func names(events []event) []string {
	out := make([]string, 0, len(events))
	for _, e := range events {
		out = append(out, e.name())
	}
	return out
}

// find returns the first event with the given name, and whether there was one.
func find(events []event, name string) (event, bool) {
	for _, e := range events {
		if e.name() == name {
			return e, true
		}
	}
	return event{}, false
}

type feedSink struct{ rec *recorder }

func (s *feedSink) OnTurnOpened(_ ids.WorkspaceID, turn ids.TurnID) {
	s.rec.emit(event{sink: "feed", method: "OnTurnOpened", detail: string(turn)})
}

func (s *feedSink) OnPrompt(_ ids.WorkspaceID, agent *conversationv1.AgentId, prompt *conversationv1.AgentPrompt, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnPrompt", agent: agent.GetValue(), detail: prompt.GetId().GetValue()})
}

func (s *feedSink) OnPeerMessage(_ ids.WorkspaceID, peer *conversationv1.PeerMessage, _ *conversationv1.TurnId, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnPeerMessage", agent: peer.GetAgent().GetValue(), detail: peer.GetId()})
}

func (s *feedSink) OnPromptRetired(_ ids.WorkspaceID, prompt *conversationv1.AgentPrompt, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnPromptRetired", agent: prompt.GetAgent().GetValue(), detail: prompt.GetId().GetValue()})
}

func (s *feedSink) OnPeerMessageRetired(_ ids.WorkspaceID, peer *conversationv1.PeerMessage, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnPeerMessageRetired", agent: peer.GetAgent().GetValue(), detail: peer.GetId()})
}

func (s *feedSink) OnApiErrorRetired(_ ids.WorkspaceID, agent *conversationv1.AgentId, failed *conversationv1.ApiRequestFailed, _ *conversationv1.TurnId, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnApiErrorRetired", agent: agent.GetValue(), detail: failed.GetMessage()})
}

func (s *feedSink) OnActivity(_ ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity, _ *conversationv1.TurnId, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnActivity", agent: agent.GetValue(), detail: act.GetActivityId().GetValue()})
}

func (s *feedSink) OnQuestion(_ ids.WorkspaceID, agent *conversationv1.AgentId, q *conversationv1.AgentQuestion, _ *conversationv1.TurnId, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnQuestion", agent: agent.GetValue(), detail: q.GetId().GetValue()})
}

func (s *feedSink) OnPermission(_ ids.WorkspaceID, agent *conversationv1.AgentId, p *conversationv1.AgentPermission, _ *conversationv1.TurnId, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnPermission", agent: agent.GetValue(), detail: p.GetId().GetValue()})
}

func (s *feedSink) OnContextCut(_ ids.WorkspaceID, agent *conversationv1.AgentId, _ *conversationv1.ContextCut, _ *conversationv1.HistoryPointer, _ *conversationv1.TurnId, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnContextCut", agent: agent.GetValue()})
}

func (s *feedSink) OnApiError(_ ids.WorkspaceID, agent *conversationv1.AgentId, failed *conversationv1.ApiRequestFailed, _ *conversationv1.TurnId, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnApiError", agent: agent.GetValue(), detail: failed.GetMessage()})
}

func (s *feedSink) OnAgentTerminal(_ ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, _ *conversationv1.AgentSuccess, _ *conversationv1.AgentFailure, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnAgentTerminal", agent: agent.GetValue(), turn: turn})
}

// OnMainAgent is recorded BESIDE the event stream rather than in it: the
// naming precedes the routing it serves, and threading it through every exact
// routing sequence would restate one fact in every assertion.
func (s *feedSink) OnMainAgent(_ ids.WorkspaceID, agent *conversationv1.AgentId) {
	s.rec.nameMain("feed", agent.GetValue())
}

func (s *feedSink) OnDetachedWork(_ ids.WorkspaceID, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork, _ *conversationv1.TurnId, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnDetachedWork", agent: agent.GetValue(), detail: work.GetWork().GetValue()})
}

func (s *feedSink) OnBash(_ ids.WorkspaceID, work *conversationv1.DetachedWorkId, _ *conversationv1.AgentBash, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnBash", detail: work.GetValue()})
}

func (s *feedSink) OnLiveWorkChanged(_ ids.WorkspaceID, live LiveWorkSet) {
	held := live
	s.rec.emit(event{sink: "feed", method: "OnLiveWorkChanged", live: &held})
}

func (s *feedSink) OnSessionUpdate(_ ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	s.rec.emit(event{sink: "feed", method: "OnSessionUpdate", detail: sessionArm(update)})
}

func (s *feedSink) OnHistoryPage(_ ids.WorkspaceID, agent *conversationv1.AgentId, page *conversationv1.HistoryPage, _ OutputAddress) {
	boundary := ""
	switch page.GetBoundary().(type) {
	case *conversationv1.HistoryPage_Floor:
		boundary = "floor"
	case *conversationv1.HistoryPage_More:
		boundary = "more"
	}
	s.rec.emit(event{sink: "feed", method: "OnHistoryPage", agent: agent.GetValue(), detail: itoa(len(page.GetEntries())), boundary: boundary})
}

type footerSink struct{ rec *recorder }

func (s *footerSink) OnTurnOpened(_ ids.WorkspaceID, turn ids.TurnID) {
	s.rec.emit(event{sink: "footer", method: "OnTurnOpened", detail: string(turn)})
}

func (s *footerSink) OnActivity(_ ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity) {
	s.rec.emit(event{sink: "footer", method: "OnActivity", agent: agent.GetValue(), detail: act.GetActivityId().GetValue()})
}

func (s *footerSink) OnQuestion(_ ids.WorkspaceID, agent *conversationv1.AgentId, _ *conversationv1.AgentQuestion) {
	s.rec.emit(event{sink: "footer", method: "OnQuestion", agent: agent.GetValue()})
}

func (s *footerSink) OnPermission(_ ids.WorkspaceID, agent *conversationv1.AgentId, _ *conversationv1.AgentPermission) {
	s.rec.emit(event{sink: "footer", method: "OnPermission", agent: agent.GetValue()})
}

func (s *footerSink) OnContextCut(_ ids.WorkspaceID, agent *conversationv1.AgentId, _ *conversationv1.ContextCut) {
	s.rec.emit(event{sink: "footer", method: "OnContextCut", agent: agent.GetValue()})
}

func (s *footerSink) OnHistoryPage(_ ids.WorkspaceID, agent *conversationv1.AgentId, page *conversationv1.HistoryPage) {
	s.rec.emit(event{sink: "footer", method: "OnHistoryPage", agent: agent.GetValue(), detail: strconv.Itoa(len(page.GetEntries()))})
}

func (s *footerSink) OnApiError(_ ids.WorkspaceID, agent *conversationv1.AgentId, _ *conversationv1.ApiRequestFailed) {
	s.rec.emit(event{sink: "footer", method: "OnApiError", agent: agent.GetValue()})
}

func (s *footerSink) OnAgentTerminal(_ ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, _ *conversationv1.AgentSuccess, _ *conversationv1.AgentFailure) {
	s.rec.emit(event{sink: "footer", method: "OnAgentTerminal", agent: agent.GetValue(), turn: turn})
}

func (s *footerSink) OnDetachedWork(_ ids.WorkspaceID, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork) {
	s.rec.emit(event{sink: "footer", method: "OnDetachedWork", agent: agent.GetValue(), detail: work.GetWork().GetValue()})
}

func (s *footerSink) OnBash(_ ids.WorkspaceID, work *conversationv1.DetachedWorkId, _ *conversationv1.AgentBash) {
	s.rec.emit(event{sink: "footer", method: "OnBash", detail: work.GetValue()})
}

func (s *footerSink) OnSubagent(_ ids.WorkspaceID, work *conversationv1.DetachedWorkId, _ *conversationv1.AgentSubagent) {
	s.rec.emit(event{sink: "footer", method: "OnSubagent", detail: work.GetValue()})
}

func (s *footerSink) OnContextBudgetWarning(_ ids.WorkspaceID, agent *conversationv1.AgentId, w *conversationv1.ContextBudgetWarning) {
	s.rec.emit(event{sink: "footer", method: "OnContextBudgetWarning", agent: agent.GetValue(), detail: w.GetText()})
}

func (s *footerSink) OnSessionUpdate(_ ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	s.rec.emit(event{sink: "footer", method: "OnSessionUpdate", detail: sessionArm(update)})
}

func (s *footerSink) OnSessionStarted(_ ids.WorkspaceID, _ *conversationv1.SessionStarted) {
	s.rec.emit(event{sink: "footer", method: "OnSessionStarted"})
}

func (s *footerSink) OnLiveWorkChanged(_ ids.WorkspaceID, live LiveWorkSet) {
	held := live
	s.rec.emit(event{sink: "footer", method: "OnLiveWorkChanged", live: &held})
}

func (s *footerSink) OnLink(_ ids.WorkspaceID, link LinkState) {
	s.rec.emit(event{sink: "footer", method: "OnLink", link: link})
}

type topbarSink struct{ rec *recorder }

func (s *topbarSink) OnSessionStarted(_ ids.WorkspaceID, _ *conversationv1.SessionStarted) {
	s.rec.emit(event{sink: "topbar", method: "OnSessionStarted"})
}

func (s *topbarSink) OnSessionUpdate(_ ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	s.rec.emit(event{sink: "topbar", method: "OnSessionUpdate", detail: sessionArm(update)})
}

func (s *topbarSink) OnContextCut(_ ids.WorkspaceID, agent *conversationv1.AgentId, _ *conversationv1.ContextCut) {
	s.rec.emit(event{sink: "topbar", method: "OnContextCut", agent: agent.GetValue()})
}

func (s *topbarSink) OnActivity(_ ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity) {
	s.rec.emit(event{sink: "topbar", method: "OnActivity", agent: agent.GetValue(), detail: act.GetActivityId().GetValue()})
}

func (s *topbarSink) OnLink(_ ids.WorkspaceID, link LinkState) {
	s.rec.emit(event{sink: "topbar", method: "OnLink", link: link})
}

type sidebarSink struct{ rec *recorder }

func (s *sidebarSink) OnSessionStarted(_ ids.WorkspaceID, _ *conversationv1.SessionStarted) {
	s.rec.emit(event{sink: "sidebar", method: "OnSessionStarted"})
}

func (s *sidebarSink) OnAgentTerminal(_ ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, _ *conversationv1.AgentSuccess, _ *conversationv1.AgentFailure) {
	s.rec.emit(event{sink: "sidebar", method: "OnAgentTerminal", agent: agent.GetValue(), turn: turn})
}

func (s *sidebarSink) OnActivity(_ ids.WorkspaceID, agent *conversationv1.AgentId, _ *conversationv1.AgentActivity) {
	s.rec.emit(event{sink: "sidebar", method: "OnActivity", agent: agent.GetValue()})
}

func (s *sidebarSink) OnDetachedWork(_ ids.WorkspaceID, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork) {
	s.rec.emit(event{sink: "sidebar", method: "OnDetachedWork", agent: agent.GetValue(), detail: work.GetWork().GetValue()})
}

func (s *sidebarSink) OnPermission(_ ids.WorkspaceID, agent *conversationv1.AgentId, _ *conversationv1.AgentPermission) {
	s.rec.emit(event{sink: "sidebar", method: "OnPermission", agent: agent.GetValue()})
}

func (s *sidebarSink) OnSessionUpdate(_ ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	s.rec.emit(event{sink: "sidebar", method: "OnSessionUpdate", detail: sessionArm(update)})
}

func (s *sidebarSink) OnLink(_ ids.WorkspaceID, link LinkState) {
	s.rec.emit(event{sink: "sidebar", method: "OnLink", link: link})
}

func (s *sidebarSink) OnLiveWorkChanged(_ ids.WorkspaceID, live LiveWorkSet) {
	held := live
	s.rec.emit(event{sink: "sidebar", method: "OnLiveWorkChanged", live: &held})
}

type lifecycleSink struct {
	rec *recorder
	// onTurnEnded, when set, runs inside OnTurnEnded: what the queue would do
	// on the same call, such as ask the watcher which turn is running.
	onTurnEnded func()
}

func (s *lifecycleSink) OnTurnEnded(_ ids.WorkspaceID, turn ids.TurnID, how TurnClose) {
	held := turn
	s.rec.emit(event{sink: "lifecycle", method: "OnTurnEnded", turn: &held, close: how})
	if s.onTurnEnded != nil {
		s.onTurnEnded()
	}
}

func (s *lifecycleSink) OnTurnAdopted(_ ids.WorkspaceID, turn ids.TurnID) {
	held := turn
	s.rec.emit(event{sink: "lifecycle", method: "OnTurnAdopted", turn: &held})
}

func (s *lifecycleSink) OnTurnsEndedUnobserved(_ ids.WorkspaceID, turns []ids.TurnID) {
	s.rec.emit(event{sink: "lifecycle", method: "OnTurnsEndedUnobserved", turns: append([]ids.TurnID(nil), turns...)})
}

func (s *lifecycleSink) OnLiveWorkChanged(_ ids.WorkspaceID, live LiveWorkSet) {
	held := live
	s.rec.emit(event{sink: "lifecycle", method: "OnLiveWorkChanged", live: &held})
}

func (s *lifecycleSink) OnFree(ws ids.WorkspaceID) { s.rec.frees <- ws }

// departedEdge is one OnDeparted call.
type departedEdge struct {
	ws        ids.WorkspaceID
	departed  Watcher
	departure Departure
}

func (s *lifecycleSink) OnDeparted(ws ids.WorkspaceID, departed Watcher, departure Departure) {
	s.rec.departures <- departedEdge{ws: ws, departed: departed, departure: departure}
}

func (s *lifecycleSink) OnLinkChanged(_ ids.WorkspaceID, attached bool) {
	s.rec.emit(event{sink: "lifecycle", method: "OnLinkChanged", attached: &attached})
}

func (s *lifecycleSink) OnLinkFault(_ ids.WorkspaceID, fault LinkFault) {
	held := fault
	s.rec.emit(event{sink: "lifecycle", method: "OnLinkFault", linkFault: &held})
}

func (s *lifecycleSink) OnWatchOpenRefused(_ ids.WorkspaceID, refusal WatchOpenRefusal) {
	held := refusal
	s.rec.emit(event{sink: "lifecycle", method: "OnWatchOpenRefused", refusal: &held})
}

func (s *lifecycleSink) OnSessionDiagnostics(_ ids.WorkspaceID, _ *conversationv1.SessionDiagnostics) {
	s.rec.emit(event{sink: "lifecycle", method: "OnSessionDiagnostics"})
}

func (s *lifecycleSink) OnNotification(_ ids.WorkspaceID, note HostNotification) {
	held := note
	s.rec.emit(event{sink: "lifecycle", method: "OnNotification", note: &held})
}

func (s *lifecycleSink) OnAsksSettled(_ ids.WorkspaceID) {
	s.rec.emit(event{sink: "lifecycle", method: "OnAsksSettled"})
}

// itoa keeps the recorder free of a strconv import at every call site.
func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	var digits []byte
	for n > 0 {
		digits = append([]byte{byte('0' + n%10)}, digits...)
		n /= 10
	}
	return string(digits)
}

// ---- the harness ----

// harness is one started watcher plus everything a test drives it with.
type harness struct {
	t      *testing.T
	client *fakeClient
	rec    *recorder
	log    *dlog.TestLogger
	w      *watcher

	session *fakeStream[*shimv1.WatchSessionResponse]
	main    *fakeStream[*shimv1.WatchAgentResponse]
	mainReq *shimv1.WatchAgentRequest

	// lifecycle is the lifecycle sink the watcher was built with.
	lifecycle *lifecycleSink

	// sentinels counts the sentinels sent, so each one is a row of its own:
	// a cut is routed once per pointer, and a second sentinel at the first
	// one's pointer would be dropped as a replay.
	sentinels int
}

// newHarness starts a watcher on the given opening session and drains
// everything the start itself emitted, so a test asserts only on what it
// sends.
func newHarness(t *testing.T, session Session) *harness {
	t.Helper()
	h := startHarness(t, session, nil)
	h.session = h.client.nextSessionOpen(t)
	open := h.client.nextAgentOpenFor(t, "")
	h.main, h.mainReq = open.stream, open.req
	return h
}

// newHarnessAttachingPurely starts a watcher opened with NO session facts: the
// adopting daemon's case. NO main agent watch is opened at start — there is no
// main agent until a session announces itself — so the caller takes only the
// session stream, and the agent open is taken after the re-announcement.
func newHarnessAttachingPurely(t *testing.T) *harness {
	t.Helper()
	h := startHarness(t, Session{}, nil)
	h.session = h.client.nextSessionOpen(t)
	return h
}

// newHarnessRefusingAgents starts a watcher whose every WatchAgent open the
// shim refuses, which is the fresh-bring-up race: the store has not registered
// the main agent's book yet. The MAIN watch is therefore never opened, so the
// caller takes only the session stream.
func newHarnessRefusingAgents(t *testing.T, session Session, refusal error) *harness {
	t.Helper()
	h := startHarness(t, session, func(c *fakeClient) { c.setAgentErr(refusal) })
	h.session = h.client.nextSessionOpen(t)
	// The start's main open is made off the watcher's lock: it is joined
	// here, so every test begins with the refusal already ruled on.
	h.client.awaitRefusedOpen(t, "WatchAgent")
	return h
}

// refusedOpenError is a watch open the SHIM refused, shaped exactly as the
// shim client shapes one: a StreamOpenError wrapping the Connect refusal.
func refusedOpenError(procedure string, code connect.Code, message string) error {
	return &shimclient.StreamOpenError{
		Procedure: procedure,
		Err:       connect.NewError(code, errors.New(message)),
	}
}

// startHarness starts a watcher, applying prep to the fake client first, and
// consumes nothing the start emitted.
func startHarness(t *testing.T, session Session, prep func(*fakeClient)) *harness {
	t.Helper()
	return startHarnessWatched(t, session, prep, nil)
}

// startHarnessWatched is startHarness with the watcher's mutex registered with
// stalls.
func startHarnessWatched(t *testing.T, session Session, prep func(*fakeClient), stalls lockwatch.Registry) *harness {
	t.Helper()
	h := &harness{t: t, client: newFakeClient(), rec: newRecorder(), log: dlog.NewTestLogger()}
	h.lifecycle = &lifecycleSink{rec: h.rec}
	if prep != nil {
		prep(h.client)
	}
	// A test that states no opening is a workspace's first opening: the
	// watcher every test before the replay rule was written against.
	if session.Opening.validate() != nil {
		session.Opening = WorkspaceOpened()
	}

	started, err := Start(context.Background(), ids.WorkspaceID("ws-1"), h.client, session, Sinks{
		Feed:      &feedSink{rec: h.rec},
		Footer:    &footerSink{rec: h.rec},
		Topbar:    &topbarSink{rec: h.rec},
		Sidebar:   &sidebarSink{rec: h.rec},
		Lifecycle: h.lifecycle,
		Stalls:    stalls,
	}, h.log)
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	h.w = started.(*watcher)
	h.client.settle = h.w.opens.Wait
	t.Cleanup(func() { _ = h.w.Close() })
	return h
}

// quiet drains every event emitted so far by routing a sentinel down the main
// agent's stream and reading up to it.
func (h *harness) quiet() {
	h.t.Helper()
	h.sentinel(h.main)
}

// sentinel pushes a context cut, whose routing is fixed and short (the feed,
// then the footer, then the topbar), and reads the recorder up to it.
// Everything returned is what the frame under test provoked.
func (h *harness) sentinel(stream *fakeStream[*shimv1.WatchAgentResponse]) []event {
	h.t.Helper()
	h.sentinels++
	stream.send(h.t, entryFrameAt(frameUpdate("sentinel", &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: &conversationv1.ContextCut{}},
	}), "ptr-sentinel-"+itoa(h.sentinels)))
	// The cut reaches the feed, the footer AND the topbar in that order, so
	// the TOPBAR's call is the sentinel: reading to any earlier one would
	// leave the later cut calls behind to pollute the next assertion. The two
	// that precede it are the sentinel's own, not the frame's, so they are
	// stripped from the tail. EVERY MATCH IS ON THE SENTINEL'S OWN AGENT, so a
	// frame under test that is itself a cut is never taken for the sentinel.
	seen := h.rec.untilEvent(h.t, "the sentinel's topbar.OnContextCut", func(e event) bool {
		return e.name() == "topbar.OnContextCut" && e.agent == "sentinel"
	})
	for _, name := range []string{"footer.OnContextCut", "feed.OnContextCut"} {
		if n := len(seen); n > 0 && seen[n-1].name() == name && seen[n-1].agent == "sentinel" {
			seen = seen[:n-1]
		}
	}
	return seen
}

// route sends one frame down a stream and returns exactly the sink calls it
// provoked.
func (h *harness) route(stream *fakeStream[*shimv1.WatchAgentResponse], frame *shimv1.WatchAgentResponse) []event {
	h.t.Helper()
	stream.send(h.t, frame)
	return h.sentinel(stream)
}

// hasRecord reports whether the watcher logged a record with this operation at
// this level.
func (h *harness) hasRecord(level, operation string) bool {
	for _, r := range h.log.Records() {
		if r.Level == level && r.Operation == operation {
			return true
		}
	}
	return false
}

// ---- frame builders ----

// agentID is the AgentId spelling every builder takes.
func agentID(value string) *conversationv1.AgentId {
	return &conversationv1.AgentId{Value: value}
}

// workID is the DetachedWorkId spelling every builder takes.
func workID(value string) *conversationv1.DetachedWorkId {
	return &conversationv1.DetachedWorkId{Value: value}
}

// frameUpdate wraps an AgentUpdate as the agent's frame.
func frameUpdate(agent string, update *conversationv1.AgentUpdate) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: agentID(agent),
		Result:  &conversationv1.AgentFrame_Update{Update: update},
	}
}

// frameSuccess is an agent's stream ending on terms the consumer asked for.
// The oneof wrapper types are unexported by the generated package, so every
// builder here takes the whole message rather than one arm.
func frameSuccess(agent string, success *conversationv1.AgentSuccess) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: agentID(agent),
		Result:  &conversationv1.AgentFrame_Success{Success: success},
	}
}

// completed, interrupted and backgrounded are the three success arms.
func completed() *conversationv1.AgentSuccess {
	return &conversationv1.AgentSuccess{Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}}}
}

func interrupted() *conversationv1.AgentSuccess {
	return &conversationv1.AgentSuccess{Outcome: &conversationv1.AgentSuccess_Interrupted{Interrupted: &conversationv1.AgentInterrupted{}}}
}

func backgrounded() *conversationv1.AgentSuccess {
	return &conversationv1.AgentSuccess{Outcome: &conversationv1.AgentSuccess_Backgrounded{Backgrounded: &conversationv1.AgentBackgrounded{}}}
}

// frameDetached is a detached-work announcement riding an agent's stream.
func frameDetached(agent string, work *conversationv1.AgentDetachedWork) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: agentID(agent),
		Result:  &conversationv1.AgentFrame_DetachedWork{DetachedWork: work},
	}
}

// entryFrame wraps an AgentFrame as one live history entry.
func entryFrame(frame *conversationv1.AgentFrame) *shimv1.WatchAgentResponse {
	return &shimv1.WatchAgentResponse{Frame: &shimv1.WatchAgentResponse_Entry{
		Entry: &conversationv1.HistoryEntryAt{
			At: &conversationv1.HistoryPointer{Value: "ptr-" + frame.GetAgentId().GetValue()},
			Entry: &conversationv1.HistoryEntry{
				Entry: &conversationv1.HistoryEntry_AgentFrame{AgentFrame: frame},
			},
		},
	}}
}

// entryFrameAt wraps an AgentFrame as one live history entry at a stated
// pointer, for the catch-up assertions.
func entryFrameAt(frame *conversationv1.AgentFrame, pointer string) *shimv1.WatchAgentResponse {
	resp := entryFrame(frame)
	resp.GetEntry().At = &conversationv1.HistoryPointer{Value: pointer}
	return resp
}

// entryPrompt wraps a prompt as one live history entry.
func entryPrompt(turn, agent string) *shimv1.WatchAgentResponse {
	return &shimv1.WatchAgentResponse{Frame: &shimv1.WatchAgentResponse_Entry{
		Entry: &conversationv1.HistoryEntryAt{
			At: &conversationv1.HistoryPointer{Value: "ptr-" + turn},
			Entry: &conversationv1.HistoryEntry{
				Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
					Id:    &conversationv1.TurnId{Value: turn},
					Agent: agentID(agent),
				}},
			},
		},
	}}
}

// entryPeer wraps a peer message as one live history entry.
func entryPeer(id, agent, sender string) *shimv1.WatchAgentResponse {
	return &shimv1.WatchAgentResponse{Frame: &shimv1.WatchAgentResponse_Entry{
		Entry: &conversationv1.HistoryEntryAt{
			At: &conversationv1.HistoryPointer{Value: "ptr-" + id},
			Entry: &conversationv1.HistoryEntry{
				Entry: &conversationv1.HistoryEntry_PeerMessage{PeerMessage: &conversationv1.PeerMessage{
					Agent:  agentID(agent),
					Sender: sender,
					Id:     id,
				}},
			},
		},
	}}
}

// retiredFrame carries an entry the store retired, as the shim last served it.
func retiredFrame(at *conversationv1.HistoryEntryAt) *shimv1.WatchAgentResponse {
	return &shimv1.WatchAgentResponse{Frame: &shimv1.WatchAgentResponse_Retired{Retired: at}}
}

// peerEntryAt is one entry carrying a peer message.
func peerEntryAt(pointer, id, agent string) *conversationv1.HistoryEntryAt {
	return &conversationv1.HistoryEntryAt{
		At: &conversationv1.HistoryPointer{Value: pointer},
		Entry: &conversationv1.HistoryEntry{
			Entry: &conversationv1.HistoryEntry_PeerMessage{PeerMessage: &conversationv1.PeerMessage{
				Agent: agentID(agent),
				Id:    id,
			}},
		},
	}
}

// pageFrame is a watch's opening catch-up page.
func pageFrame(entries ...*conversationv1.HistoryEntryAt) *shimv1.WatchAgentResponse {
	return &shimv1.WatchAgentResponse{Frame: &shimv1.WatchAgentResponse_Page{
		Page: &conversationv1.HistoryPage{Entries: entries},
	}}
}

// frameEntryAt is one page entry carrying an agent frame, for opening-page
// assertions.
func frameEntryAt(pointer string, frame *conversationv1.AgentFrame) *conversationv1.HistoryEntryAt {
	return &conversationv1.HistoryEntryAt{
		At: &conversationv1.HistoryPointer{Value: pointer},
		Entry: &conversationv1.HistoryEntry{
			Entry: &conversationv1.HistoryEntry_AgentFrame{AgentFrame: frame},
		},
	}
}

// promptEntry is one page entry carrying a prompt.
func promptEntry(pointer, turn, agent string) *conversationv1.HistoryEntryAt {
	return &conversationv1.HistoryEntryAt{
		At: &conversationv1.HistoryPointer{Value: pointer},
		Entry: &conversationv1.HistoryEntry{
			Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
				Id:    &conversationv1.TurnId{Value: turn},
				Agent: agentID(agent),
			}},
		},
	}
}

// activityUpdate wraps one unit of synchronous progress as an AgentUpdate.
func activityUpdate(act *conversationv1.AgentActivity) *conversationv1.AgentUpdate {
	return &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{Activity: act}}
}

// readActivity is an ordinary modeled tool call.
func readActivity(activityID string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: activityID},
		Item:       &conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{}},
	}
}

// unmodeledActivity is a call the schema does not model, which is the one
// activity the topbar sees.
func unmodeledActivity(activityID, tool string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: activityID},
		Item: &conversationv1.AgentActivity_Unmodeled{Unmodeled: &conversationv1.AgentUnmodeled{
			Result: &conversationv1.AgentUnmodeled_Start{Start: &conversationv1.AgentUnmodeledStart{ToolName: tool}},
		}},
	}
}

// handbackActivity is a subagent handing its final report back.
func handbackActivity(activityID string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: activityID},
		Item: &conversationv1.AgentActivity_SubagentHandback{SubagentHandback: &conversationv1.AgentSubagentHandback{
			Result: &conversationv1.AgentSubagentHandback_Start{Start: &conversationv1.AgentSubagentHandbackStart{
				Report: &conversationv1.AgentSubagentHandbackReport{Text: "the report"},
			}},
		}},
	}
}

// mcpActivity is an MCP server's tool call announcing itself.
func mcpActivity(activityID, tool string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: activityID},
		Item: &conversationv1.AgentActivity_McpToolCall{McpToolCall: &conversationv1.AgentMcpToolCall{
			Result: &conversationv1.AgentMcpToolCall_Start{Start: &conversationv1.AgentMcpToolCallStart{
				Tool: &conversationv1.AgentMcpTool{Name: tool},
			}},
		}},
	}
}

// subagentActivity is the spawn that names the agent a detached subagent watch
// is addressed by.
func subagentActivity(activityID, created string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: activityID},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
				CreatedAgentId: agentID(created),
			}},
		}},
	}
}

// detachedSubagentHarness arranges the whole life of a DETACHED run up to the
// point it settles: the spawn is streamed as the turn's own progress, its watch
// is opened, and the announcement then promotes that watch to detached work
// under the handle "w-1". The run is live and addressed by its handle when this
// returns, which is the only state its terminal is interesting from.
func detachedSubagentHarness(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(subagentActivity("spawn-1", "sub-1")))))
	h.client.nextAgentOpen(t)
	h.route(h.main, entryFrame(frameDetached("main-1", detachedWork("w-1", "spawn-1", subagentKind("sub-1")))))
	if live := h.w.LiveWork(); len(live.Agents) != 1 {
		t.Fatalf("live work = %v, want the detached subagent live before its terminal", live.Agents)
	}
	h.quiet()
	return h
}

// settledSubagentActivity is the SPAWN UNIT's terminal arm — the frame a
// detached run actually settles on, since its own stream never carries an agent
// terminal. `failed` picks the failure arm over the success one.
func settledSubagentActivity(activityID string, failed bool) *conversationv1.AgentActivity {
	sub := &conversationv1.AgentSubagent{
		Result: &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{}},
	}
	if failed {
		sub.Result = &conversationv1.AgentSubagent_Failure{Failure: &conversationv1.AgentSubagentFailure{}}
	}
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: activityID},
		Item:       &conversationv1.AgentActivity_Subagent{Subagent: sub},
	}
}

// runningSubagentActivity is the spawn unit's UPDATE arm: the run reported
// progress and has not settled.
func runningSubagentActivity(activityID string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: activityID},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Update{Update: &conversationv1.AgentSubagentUpdate{}},
		}},
	}
}

// bashActivity is an in-turn shell call, the unit a detached shell detaches
// from.
// sendMessageActivity is a SendMessage call's start: the unit a resumed
// subagent detaches from.
func sendMessageActivity(activityID string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: activityID},
		Item: &conversationv1.AgentActivity_SendMessage{SendMessage: &conversationv1.AgentSendMessage{
			Result: &conversationv1.AgentSendMessage_Start{Start: &conversationv1.AgentSendMessageStart{AddressedTo: "a5583"}},
		}},
	}
}

// withKind restates an announcement's kind, for the announcements a producer
// must never send.
func withKind(work *conversationv1.AgentDetachedWork, kind *conversationv1.DetachedWorkKind) *conversationv1.AgentDetachedWork {
	work.Kind = kind
	return work
}

// lastFooterLiveWork answers the last live-work set the footer was handed
// among `events`.
func lastFooterLiveWork(events []event) (LiveWorkSet, bool) {
	var last *LiveWorkSet
	for _, e := range events {
		if e.name() == "footer.OnLiveWorkChanged" && e.live != nil {
			last = e.live
		}
	}
	if last == nil {
		return LiveWorkSet{}, false
	}
	return *last, true
}

func bashActivity(activityID string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: activityID},
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{
			Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{}},
		}},
	}
}

// monitorActivity is a background watcher, live or ended.
func monitorActivity(activityID string, ended bool) *conversationv1.AgentActivity {
	monitor := &conversationv1.AgentMonitor{Result: &conversationv1.AgentMonitor_Start{Start: &conversationv1.AgentMonitorStart{}}}
	if ended {
		monitor = &conversationv1.AgentMonitor{Result: &conversationv1.AgentMonitor_Ended{Ended: &conversationv1.AgentMonitorEnded{}}}
	}
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: activityID},
		Item:       &conversationv1.AgentActivity_Monitor{Monitor: monitor},
	}
}

// monitorFailedActivity is a background watcher that could not be armed.
func monitorFailedActivity(activityID string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: activityID},
		Item: &conversationv1.AgentActivity_Monitor{Monitor: &conversationv1.AgentMonitor{
			Result: &conversationv1.AgentMonitor_Failure{Failure: &conversationv1.AgentMonitorFailure{}},
		}},
	}
}

// createdWork is a detached-work announcement whose origin describes the work,
// stating the SAME kind as its description, as a producer does.
func createdWork(work string, created *conversationv1.DetachableWork) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Work:   workID(work),
		Kind:   kindDescribing(created),
		Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{WorkCreated: created}},
	}
}

// kindDescribing is the kind a producer states beside a `created` description.
func kindDescribing(created *conversationv1.DetachableWork) *conversationv1.DetachedWorkKind {
	switch arm := created.GetWork().(type) {
	case *conversationv1.DetachableWork_Subagent:
		return subagentKind(arm.Subagent.GetStart().GetCreatedAgentId().GetValue())
	case *conversationv1.DetachableWork_Bash:
		return bashKind()
	case *conversationv1.DetachableWork_Monitor:
		return monitorKind()
	case *conversationv1.DetachableWork_Workflow:
		return workflowKind()
	default:
		return nil
	}
}

// detachedWork is an announcement whose origin names only the in-turn unit the
// work used to be, and whose kind the producer states beside it.
func detachedWork(work, from string, kind *conversationv1.DetachedWorkKind) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Work: workID(work),
		Kind: kind,
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: &conversationv1.AgentActivityId{Value: from},
			Cause:          &conversationv1.DetachedWorkDetached_Requested{Requested: &conversationv1.DetachedCauseRequested{}},
		}},
	}
}

// subagentKind, bashKind, monitorKind and workflowKind are the four kinds an
// announcement can state; a subagent's names the agent that is running.
func subagentKind(agent string) *conversationv1.DetachedWorkKind {
	return &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Subagent{
		Subagent: &conversationv1.DetachedWorkKindSubagent{AgentId: agentID(agent)},
	}}
}

func bashKind() *conversationv1.DetachedWorkKind {
	return &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Bash{Bash: &conversationv1.DetachedWorkKindBash{}}}
}

func monitorKind() *conversationv1.DetachedWorkKind {
	return &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Monitor{Monitor: &conversationv1.DetachedWorkKindMonitor{}}}
}

func workflowKind() *conversationv1.DetachedWorkKind {
	return &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Workflow{Workflow: &conversationv1.DetachedWorkKindWorkflow{}}}
}

// subagentWork, bashWork, monitorWork and workflowWork are the four kinds a
// `created` announcement can name.
func subagentWork(created string) *conversationv1.DetachableWork {
	return &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Subagent{
		Subagent: &conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Start{
			Start: &conversationv1.AgentSubagentStart{CreatedAgentId: agentID(created)},
		}},
	}}
}

func bashWork() *conversationv1.DetachableWork {
	return &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Bash{
		Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{}}},
	}}
}

func monitorWork() *conversationv1.DetachableWork {
	return &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Monitor{
		Monitor: &conversationv1.AgentMonitor{Result: &conversationv1.AgentMonitor_Start{Start: &conversationv1.AgentMonitorStart{}}},
	}}
}

func workflowWork() *conversationv1.DetachableWork {
	return &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Workflow{
		Workflow: &conversationv1.AgentWorkflowStart{},
	}}
}

// sessionStarted is the opening level a watcher is started on.
func sessionStarted(turn string, live ...*conversationv1.AgentDetachedWork) *conversationv1.SessionStarted {
	started := &conversationv1.SessionStarted{VendorSessionId: "vendor-1", LiveWork: live}
	if turn != "" {
		started.TurnInFlight = &conversationv1.TurnId{Value: turn}
	}
	return started
}

// newTestLogger is the logger every test starts a watcher with.
func newTestLogger() *dlog.TestLogger { return dlog.NewTestLogger() }

// contains is strings.Contains, kept local so the assertions read as
// assertions.
func contains(haystack, needle string) bool {
	return len(needle) == 0 || len(haystack) >= len(needle) && indexOf(haystack, needle) >= 0
}

func indexOf(haystack, needle string) int {
	for i := 0; i+len(needle) <= len(haystack); i++ {
		if haystack[i:i+len(needle)] == needle {
			return i
		}
	}
	return -1
}

// ---- assertions ----

// assertNames asserts a frame's routing was EXACTLY this run of sink calls, in
// order. Exactness is the point: a route to one sink too many is a duplicated
// row, and one too few is a fact nobody draws.
func assertNames(t *testing.T, got []event, want []string) {
	t.Helper()
	have := names(got)
	if len(have) != len(want) {
		t.Fatalf("routed to %v, want %v", have, want)
	}
	for i := range want {
		if have[i] != want[i] {
			t.Fatalf("routed to %v, want %v", have, want)
		}
	}
}

// requireEvent returns the named event, failing when the routing did not
// produce one.
func requireEvent(t *testing.T, got []event, name string) event {
	t.Helper()
	e, ok := find(got, name)
	if !ok {
		t.Fatalf("no %s in %v", name, names(got))
	}
	return e
}

// ---- more frame builders ----

// questionUpdate is an agent blocked on the user's choice.
func questionUpdate(id, header, text string) *conversationv1.AgentUpdate {
	return &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Question{
		Question: &conversationv1.AgentQuestion{
			Id: &conversationv1.AgentQuestionId{Value: id},
			Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{
				Batch: &conversationv1.AgentQuestionBatch{Questions: []*conversationv1.AgentQuestionAsked{{
					Question: &conversationv1.AgentQuestionText{Text: text},
					Header:   header,
				}}},
				StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1700000000000},
			}},
		},
	}}
}

// permissionUpdate is an agent blocked on the user's consent for one call.
func permissionUpdate(id, gatedCall, title, displayName string) *conversationv1.AgentUpdate {
	return &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Permission{
		Permission: &conversationv1.AgentPermission{
			Id:        &conversationv1.AgentPermissionId{Value: id},
			GatedCall: &conversationv1.AgentActivityId{Value: gatedCall},
			Result: &conversationv1.AgentPermission_Start{Start: &conversationv1.AgentPermissionStart{
				Prompt:    &conversationv1.AgentPermissionPrompt{Title: title, DisplayName: displayName},
				StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1700000000000},
			}},
		},
	}}
}

// permissionSettledUpdate is a permission ask DECIDED, which is what retires
// the attention marker the open ask raised. `decision` is the success arm.
func permissionSettledUpdate(id string, decision *conversationv1.AgentPermissionSuccess) *conversationv1.AgentUpdate {
	return &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Permission{
		Permission: &conversationv1.AgentPermission{
			Id:     &conversationv1.AgentPermissionId{Value: id},
			Result: &conversationv1.AgentPermission_Success{Success: decision},
		},
	}}
}

// allowedOnce is the user's own answer on an open ask.
func allowedOnce() *conversationv1.AgentPermissionSuccess {
	return &conversationv1.AgentPermissionSuccess{
		Decision: &conversationv1.AgentPermissionSuccess_Allowed{
			Allowed: &conversationv1.AgentPermissionAllowed{
				Scope: &conversationv1.AgentPermissionAllowed_Once{
					Once: &conversationv1.AgentPermissionAllowedOnce{},
				},
			},
		},
	}
}

// deniedByPolicy is a refusal nobody was asked for.
func deniedByPolicy() *conversationv1.AgentPermissionSuccess {
	return &conversationv1.AgentPermissionSuccess{
		Decision: &conversationv1.AgentPermissionSuccess_Denied{
			Denied: &conversationv1.AgentPermissionDenied{
				By: &conversationv1.AgentPermissionDenied_Policy{
					Policy: &conversationv1.AgentPermissionDeniedByPolicy{},
				},
			},
		},
	}
}

// questionSettledUpdate is a question ask that concluded, however it did.
func questionSettledUpdate(id string) *conversationv1.AgentUpdate {
	return &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Question{
		Question: &conversationv1.AgentQuestion{
			Id: &conversationv1.AgentQuestionId{Value: id},
			Result: &conversationv1.AgentQuestion_Success{Success: &conversationv1.AgentQuestionSuccess{
				Batch: &conversationv1.AgentQuestionBatch{},
				Outcome: &conversationv1.AgentQuestionSuccess_Answered{
					Answered: &conversationv1.AgentQuestionAnswers{},
				},
			}},
		},
	}}
}

// apiErrorUpdate is a vendor request that failed MID-TURN, which is evidence
// and never a terminal.
func apiErrorUpdate(message string) *conversationv1.AgentUpdate {
	return &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_ApiError{
		ApiError: &conversationv1.ApiRequestFailed{
			Message: message,
			Kind:    &conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{}},
		},
	}}
}

// contextInjectedActivity is a FILE-PLANE-ONLY fact: it arrives through an
// agent watch's replay or follow and never on the live session stream.
func contextInjectedActivity(activityID string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: activityID},
		Item: &conversationv1.AgentActivity_ContextInjected{
			ContextInjected: &conversationv1.AgentContextInjected{},
		},
	}
}

// responseActivity is prose, which is not a tool call.
func responseActivity(activityID string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: activityID},
		Item:       &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{}},
	}
}

// lostFailure is a DetachedLost terminal: work the daemon lost track of. It is
// an ORDINARY terminal, which is exactly what this builder exists to prove.
func lostFailure() *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_Lost{Lost: &conversationv1.DetachedLost{
			How: &conversationv1.DetachedLost_WentSilent{WentSilent: &conversationv1.DetachedLostWentSilent{}},
		}},
	}
}

// ---- session update builders, one per routed arm ----

func diagnosticsUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_Diagnostics{
		Diagnostics: &conversationv1.SessionDiagnostics{
			Health: &conversationv1.SessionDiagnostics_Healthy{Healthy: &conversationv1.SessionHealthy{}},
		},
	}}
}

func contextUsageUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_ContextUsage{
		ContextUsage: &conversationv1.SessionContextUsage{TotalTokens: 1000, MaxTokens: 200000},
	}}
}

func identityRotatedUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_IdentityRotated{
		IdentityRotated: &conversationv1.SessionIdentityRotated{
			PreviousVendorSessionId: "vendor-1", VendorSessionId: "vendor-2",
		},
	}}
}

func fastModeUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_FastMode{
		FastMode: &conversationv1.SessionFastMode{
			State: &conversationv1.SessionFastMode_On{On: &conversationv1.SessionFastModeOn{}},
		},
	}}
}

func titleUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_Title{
		Title: &conversationv1.SessionTitle{Text: "Add SPC j keybinding support"},
	}}
}

func mcpServerUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_McpServer{
		McpServer: &conversationv1.SessionMcpServer{
			Name:   "things",
			Health: &conversationv1.SessionMcpServer_Connected{Connected: &conversationv1.SessionMcpServerConnected{}},
		},
	}}
}

func modelChangedUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_ModelChanged{
		ModelChanged: &conversationv1.SessionModelChanged{},
	}}
}

func permissionModeChangedUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_PermissionModeChanged{
		PermissionModeChanged: &conversationv1.SessionPermissionModeChanged{},
	}}
}

func accountUsageUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_AccountUsage{
		AccountUsage: &conversationv1.SessionAccountUsage{ObservedAtMs: 1700000000000},
	}}
}

func rateLimitStatusUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_RateLimitStatus{
		RateLimitStatus: &conversationv1.SessionRateLimitStatus{
			Status: &conversationv1.SessionRateLimitStatus_Allowed{Allowed: &conversationv1.SessionRateLimitAllowed{}},
		},
	}}
}

// budgetWarningFrame is the vendor's context-budget warning on the AGENT
// plane, which is where the arm lives.
func budgetWarningFrame() *conversationv1.AgentUpdate {
	return &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_ContextBudgetWarning{
		ContextBudgetWarning: &conversationv1.ContextBudgetWarning{Text: "the context is filling"},
	}}
}

func compactingUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_Compacting{
		Compacting: &conversationv1.SessionCompacting{},
	}}
}

func queryDiedUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_QueryDied{
		QueryDied: &conversationv1.SessionQueryDied{
			Cause: &conversationv1.SessionQueryDied_UnexpectedEof{UnexpectedEof: &conversationv1.SessionQueryUnexpectedEof{}},
		},
	}}
}

// routeNow applies one frame through the watcher's own routing SYNCHRONOUSLY
// and returns exactly the sink calls it made.
//
// WHY NOT A SENTINEL: the sentinel bound works only WITHIN one stream, because
// one stream's frames are consumed one at a time. Two streams are two
// goroutines with no order between them, so a frame on the session or a bash
// stream cannot be bounded by a sentinel on the agent stream. A synchronous
// call is the exact bound, and the stream plumbing that would otherwise be
// skipped has tests of its own.
func (h *harness) routeNow(apply func(w *watcher)) []event {
	h.t.Helper()
	h.w.mu.Lock()
	apply(h.w)
	h.w.mu.Unlock()
	// The turn ends recorded under mu reach the lifecycle sink only once it is
	// released, exactly as every stream goroutine does it.
	h.w.flushTurnEnds()
	return h.drainNow()
}

// drainNow reads everything already recorded and returns it.
func (h *harness) drainNow() []event {
	var seen []event
	for {
		select {
		case e := <-h.rec.ch:
			seen = append(seen, e)
		default:
			return seen
		}
	}
}

// shellWatchFor returns the watcher's own entry for one detached shell.
func (h *harness) shellWatchFor(work string) *shellWatch {
	h.t.Helper()
	h.w.mu.Lock()
	defer h.w.mu.Unlock()
	entry, ok := h.w.shells[work]
	if !ok {
		h.t.Fatalf("no shell watch for %q", work)
	}
	return entry
}

// shimResponse is the agent stream's frame type, spelled once so the
// serialization test's helper signature stays readable.
type shimResponse = shimv1.WatchAgentResponse

// quietAll drains everything the START emitted and returns it, so a test can
// assert on the opening facts themselves.
func (h *harness) quietAll() []event {
	h.t.Helper()
	return h.sentinel(h.main)
}

// addressNow reads the output address currently in force.
func (h *harness) addressNow() OutputAddress {
	h.w.mu.Lock()
	defer h.w.mu.Unlock()
	return h.w.addr
}

// collect reads exactly n events.
func (h *harness) collect(t *testing.T, n int) []event {
	t.Helper()
	seen := make([]event, 0, n)
	for len(seen) < n {
		select {
		case e := <-h.rec.ch:
			seen = append(seen, e)
		case <-time.After(waitDeadline):
			t.Fatalf("only %d of %d events arrived", len(seen), n)
		}
	}
	return seen
}

// awaitRecord waits for the watcher to log a record, which is how a test
// observes a branch that touches no sink.
func (h *harness) awaitRecord(t *testing.T, level, operation string) {
	t.Helper()
	deadline := time.After(waitDeadline)
	for {
		if h.hasRecord(level, operation) {
			return
		}
		select {
		case <-deadline:
			t.Fatalf("the record %s/%s was never logged", level, operation)
		case <-time.After(time.Millisecond):
		}
	}
}

// feedFor addresses one subagent bubble's sub-feed.
func feedFor(agent string) feedid.Feed {
	return feedid.Feed{Agent: agentID(agent)}
}

// assertLiveWork asserts the live set is exactly this one.
func assertLiveWork(t *testing.T, got, want LiveWorkSet) {
	t.Helper()
	assertIDs(t, "agents", agentValues(got.Agents), agentValues(want.Agents))
	assertIDs(t, "shells", workValues(got.Shells), workValues(want.Shells))
	assertIDs(t, "monitors", workValues(got.Monitors), workValues(want.Monitors))
}

func assertIDs(t *testing.T, what string, got, want []string) {
	t.Helper()
	if len(got) != len(want) {
		t.Fatalf("live %s = %v, want %v", what, got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("live %s = %v, want %v", what, got, want)
		}
	}
}

func agentValues(ids []*conversationv1.AgentId) []string {
	out := make([]string, 0, len(ids))
	for _, id := range ids {
		out = append(out, id.GetValue())
	}
	return out
}

func workValues(ids []*conversationv1.DetachedWorkId) []string {
	out := make([]string, 0, len(ids))
	for _, id := range ids {
		out = append(out, id.GetValue())
	}
	return out
}

// assertPerAgentOrder asserts the feed saw one agent's activities in the order
// they were sent, and that each frame's feed call is immediately followed by
// its footer call — nothing from another stream landed between the two, which
// is what serialization buys the resolvers.
func assertPerAgentOrder(t *testing.T, got []event, agent string, frames int) {
	t.Helper()
	seen := 0
	for i, e := range got {
		if e.name() != "feed.OnActivity" || e.agent != agent {
			continue
		}
		want := agent + "-" + itoa(seen)
		if e.detail != want {
			t.Fatalf("%s activity %d = %q, want %q", agent, seen, e.detail, want)
		}
		if i+1 >= len(got) || got[i+1].name() != "footer.OnActivity" || got[i+1].detail != want {
			t.Fatalf("%s activity %q was interrupted between the feed and the footer", agent, want)
		}
		if i+2 >= len(got) || got[i+2].name() != "topbar.OnActivity" || got[i+2].detail != want {
			t.Fatalf("%s activity %q was interrupted between the footer and the topbar", agent, want)
		}
		seen++
	}
	if seen != frames {
		t.Fatalf("the feed saw %d of %s's %d activities", seen, agent, frames)
	}
}

// routeReaping sends a frame that REAPS its own stream and returns exactly the
// sink calls it provoked.
//
// A SENTINEL CANNOT BE USED HERE, and using one is a real flake rather than a
// theoretical one. The reap closes the very stream the sentinel would ride
// (reapAgentLocked hands the stream to `go stream.Close()`), so the sentinel
// send races that goroutine: on a quiet box the reader is back in Recv first
// and takes the frame, and under load the close wins, Recv answers io.EOF, the
// reader exits, and the send blocks until the failure deadline.
//
// The bound is the stream's OWN END instead. The reader returns to Recv only
// after routing and its off-lock flush have finished, so an io.EOF answered
// there means every sink call this frame provoked is already recorded -- which
// makes the drain below both complete AND exhaustive, so an unexpected extra
// call still fails the assertion.
func (h *harness) routeReaping(stream *fakeStream[*shimResponse], frame *shimResponse) []event {
	h.t.Helper()
	stream.send(h.t, frame)
	stream.awaitDrained(h.t)
	return h.drainNow()
}

// awaitLinkFault reads events until the lifecycle sink's lost-link evidence
// arrives, and answers it.
func (h *harness) awaitLinkFault(t *testing.T) LinkFault {
	t.Helper()
	deadline := time.After(waitDeadline)
	var seen []event
	for {
		select {
		case e := <-h.rec.ch:
			if e.name() == "lifecycle.OnLinkFault" {
				return *e.linkFault
			}
			seen = append(seen, e)
		case <-deadline:
			t.Fatalf("no link fault arrived; saw %v", names(seen))
			return LinkFault{}
		}
	}
}

// awaitRefusal waits for the refused-open evidence the lifecycle sink is
// handed.
func (h *harness) awaitRefusal(t *testing.T) WatchOpenRefusal {
	t.Helper()
	deadline := time.After(waitDeadline)
	var seen []event
	for {
		select {
		case e := <-h.rec.ch:
			if e.name() == "lifecycle.OnWatchOpenRefused" {
				return *e.refusal
			}
			seen = append(seen, e)
		case <-deadline:
			t.Fatalf("no refused-open fault arrived; saw %v", names(seen))
			return WatchOpenRefusal{}
		}
	}
}

// sendSessionUpdate pushes one SessionUpdate as the session stream's `update`
// frame, which is the shape the shim client hands the watcher.
func (h *harness) sendSessionUpdate(t *testing.T, u *conversationv1.SessionUpdate) {
	t.Helper()
	h.session.send(t, &shimv1.WatchSessionResponse{
		Frame: &shimv1.WatchSessionResponse_Update{Update: u},
	})
}

// sendSessionStarted pushes the shim's once-per-watch re-announcement.
func (h *harness) sendSessionStarted(t *testing.T, started *conversationv1.SessionStarted) {
	t.Helper()
	h.session.send(t, &shimv1.WatchSessionResponse{
		Frame: &shimv1.WatchSessionResponse_SessionStarted{SessionStarted: started},
	})
}

// recordContext answers the context of the FIRST record at one level for one
// operation, which is how a test asserts what a record NAMES rather than only
// that it happened.
func (h *harness) recordContext(t *testing.T, level, operation string) dlog.Context {
	t.Helper()
	for _, r := range h.log.Records() {
		if r.Level == level && r.Operation == operation {
			return dlog.Context(r.Context)
		}
	}
	t.Fatalf("no %s/%s record was logged", level, operation)
	return nil
}

// relink severs the session's standing stream and brings the link back, which
// re-opens the whole fleet. The main watch's re-open is taken as the harness's
// main stream, and its request is returned so a test can read what it asked
// for. Nothing emitted is drained: the caller quiets when it wants to.
func (h *harness) relink(t *testing.T) *shimv1.WatchAgentRequest {
	t.Helper()
	h.session.fail(errors.New("connection reset"))
	h.rec.until(t, "sidebar.OnLink")
	h.client.linkBack()
	h.session = h.client.nextSessionOpen(t)
	open := h.client.nextAgentOpenFor(t, "")
	h.main, h.mainReq = open.stream, open.req
	return open.req
}

// withTitle installs a recording title sink, under the watcher's own lock
// because the stream goroutines read the sink set under it.
func (h *harness) withTitle() *recordingTitleSink {
	ts := &recordingTitleSink{}
	h.w.mu.Lock()
	h.w.sinks.Title = ts
	h.w.mu.Unlock()
	return ts
}

// cutUpdate wraps a context cut as an agent-plane update.
func cutUpdate(cut *conversationv1.ContextCut) *conversationv1.AgentUpdate {
	return &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: cut}}
}

// compactedCut is a completed compaction.
func compactedCut() *conversationv1.ContextCut {
	return &conversationv1.ContextCut{Cut: &conversationv1.ContextCut_Compacted{Compacted: &conversationv1.ContextCompacted{}}}
}

// clearedCut is a /clear.
func clearedCut() *conversationv1.ContextCut {
	return &conversationv1.ContextCut{Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}}}
}

// failedCompactionCut is a compaction that cut nothing.
func failedCompactionCut() *conversationv1.ContextCut {
	return &conversationv1.ContextCut{Cut: &conversationv1.ContextCut_CompactionFailed{
		CompactionFailed: &conversationv1.ContextCompactionFailed{Error: "prompt too long"},
	}}
}

// cutEntryAt is one page entry carrying the main agent's context cut.
func cutEntryAt(pointer string, cut *conversationv1.ContextCut) *conversationv1.HistoryEntryAt {
	return frameEntryAt(pointer, frameUpdate("main-1", cutUpdate(cut)))
}

// liveCutAt is the main agent's context cut served as a live entry.
func liveCutAt(pointer string, cut *conversationv1.ContextCut) *shimv1.WatchAgentResponse {
	return entryFrameAt(frameUpdate("main-1", cutUpdate(cut)), pointer)
}

// awaitDeparture waits for the lifecycle sink's departure edge.
func (h *harness) awaitDeparture(t *testing.T) departedEdge {
	t.Helper()
	select {
	case d := <-h.rec.departures:
		return d
	case <-time.After(waitDeadline):
		t.Fatalf("no departure was told to the lifecycle sink")
		return departedEdge{}
	}
}

// noMoreDepartures fails if a departure is already standing. Every caller asks
// after a synchronous Close, which tells its departure before it returns, so
// the check waits on nothing.
func (h *harness) noMoreDepartures(t *testing.T) {
	t.Helper()
	select {
	case d := <-h.rec.departures:
		t.Fatalf("an unexpected departure was told: %+v", d.departure)
	default:
	}
}
