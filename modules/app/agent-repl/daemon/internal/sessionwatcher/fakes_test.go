package sessionwatcher

import (
	"context"
	"io"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
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
}

func newFakeStream[T any]() *fakeStream[T] {
	return &fakeStream[T]{
		frames: make(chan T),
		errs:   make(chan error, 1),
		closed: make(chan struct{}),
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
		return zero, io.EOF
	}
}

// Close implements shimclient.Stream.
func (s *fakeStream[T]) Close() { s.once.Do(func() { close(s.closed) }) }

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
	sessionOpens chan *fakeStream[*conversationv1.SessionUpdate]
	agentOpens   chan agentOpen
	bashOpens    chan bashOpen
	links        chan shimclient.LinkState
	exits        chan shimclient.ExitInfo

	mu           sync.Mutex
	agentErr     error
	sessionCount int
}

func newFakeClient() *fakeClient {
	return &fakeClient{
		sessionOpens: make(chan *fakeStream[*conversationv1.SessionUpdate], 8),
		agentOpens:   make(chan agentOpen, 32),
		bashOpens:    make(chan bashOpen, 32),
		links:        make(chan shimclient.LinkState, 8),
		exits:        make(chan shimclient.ExitInfo),
	}
}

func (c *fakeClient) WatchSession(context.Context) (shimclient.Stream[*conversationv1.SessionUpdate], error) {
	stream := newFakeStream[*conversationv1.SessionUpdate]()
	c.mu.Lock()
	c.sessionCount++
	c.mu.Unlock()
	c.sessionOpens <- stream
	return stream, nil
}

func (c *fakeClient) WatchAgent(_ context.Context, req *shimv1.WatchAgentRequest) (shimclient.Stream[*shimv1.WatchAgentResponse], error) {
	c.mu.Lock()
	err := c.agentErr
	c.mu.Unlock()
	if err != nil {
		return nil, err
	}
	stream := newFakeStream[*shimv1.WatchAgentResponse]()
	c.agentOpens <- agentOpen{req: req, stream: stream}
	return stream, nil
}

func (c *fakeClient) WatchBash(_ context.Context, work *conversationv1.DetachedWorkId) (shimclient.Stream[*conversationv1.AgentBash], error) {
	stream := newFakeStream[*conversationv1.AgentBash]()
	c.bashOpens <- bashOpen{work: work, stream: stream}
	return stream, nil
}

func (c *fakeClient) Connectivity() <-chan shimclient.LinkState { return c.links }

// nextAgentOpen returns the next WatchAgent the watcher opened.
func (c *fakeClient) nextAgentOpen(t *testing.T) agentOpen {
	t.Helper()
	select {
	case open := <-c.agentOpens:
		return open
	case <-time.After(waitDeadline):
		t.Fatal("no WatchAgent was opened")
		return agentOpen{}
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
func (c *fakeClient) nextSessionOpen(t *testing.T) *fakeStream[*conversationv1.SessionUpdate] {
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
	select {
	case open := <-c.agentOpens:
		t.Fatalf("an unexpected WatchAgent was opened for %q", open.req.GetTarget().GetValue())
	default:
	}
}

// noBashOpen asserts no further WatchBash was opened.
func (c *fakeClient) noBashOpen(t *testing.T) {
	t.Helper()
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

func (c *fakeClient) Occupy(string) (func(), error) { panic("sessionwatcher must not take the lease") }
func (c *fakeClient) Kill(shimclient.KillAttribution) error {
	panic("sessionwatcher must never kill: attach ends nothing")
}
func (c *fakeClient) Detach()                            { panic("sessionwatcher must not detach the client") }
func (c *fakeClient) Exited() <-chan shimclient.ExitInfo { return c.exits }
func (c *fakeClient) PID() int                           { return 4242 }

// ---- recording sinks ----

// event is one sink call, as the recorder saw it.
type event struct {
	sink   string
	method string
	agent  string
	detail string
	turn   *ids.TurnID
	close  TurnClose
	live   *LiveWorkSet
	note   *HostNotification
	link   LinkState
}

// name is the "sink.Method" spelling the assertions compare on.
func (e event) name() string { return e.sink + "." + e.method }

// recorder is the ordered record of every sink call, on a channel so a test
// waits for routing to finish rather than sleeping through it.
type recorder struct{ ch chan event }

func newRecorder() *recorder { return &recorder{ch: make(chan event, 512)} }

func (r *recorder) emit(e event) { r.ch <- e }

// until reads events until the named one arrives and returns everything
// BEFORE it. Sending a sentinel frame down the same stream after the frame
// under test is what bounds a routing assertion: the stream's frames are
// consumed one at a time, so the sentinel cannot be routed until the frame
// before it has been.
func (r *recorder) until(t *testing.T, name string) []event {
	t.Helper()
	var seen []event
	for {
		select {
		case e := <-r.ch:
			if e.name() == name {
				return seen
			}
			seen = append(seen, e)
		case <-time.After(waitDeadline):
			t.Fatalf("the sentinel %s never arrived; saw %v", name, names(seen))
			return nil
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

// count reports how many events carry the given name.
func count(events []event, name string) int {
	n := 0
	for _, e := range events {
		if e.name() == name {
			n++
		}
	}
	return n
}

type feedSink struct{ rec *recorder }

func (s *feedSink) OnPrompt(_ ids.WorkspaceID, agent *conversationv1.AgentId, prompt *conversationv1.AgentPrompt, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnPrompt", agent: agent.GetValue(), detail: prompt.GetId().GetValue()})
}

func (s *feedSink) OnActivity(_ ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnActivity", agent: agent.GetValue(), detail: act.GetActivityId().GetValue()})
}

func (s *feedSink) OnQuestion(_ ids.WorkspaceID, agent *conversationv1.AgentId, q *conversationv1.AgentQuestion, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnQuestion", agent: agent.GetValue(), detail: q.GetId().GetValue()})
}

func (s *feedSink) OnPermission(_ ids.WorkspaceID, agent *conversationv1.AgentId, p *conversationv1.AgentPermission, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnPermission", agent: agent.GetValue(), detail: p.GetId().GetValue()})
}

func (s *feedSink) OnContextCut(_ ids.WorkspaceID, agent *conversationv1.AgentId, _ *conversationv1.ContextCut, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnContextCut", agent: agent.GetValue()})
}

func (s *feedSink) OnApiError(_ ids.WorkspaceID, agent *conversationv1.AgentId, failed *conversationv1.ApiRequestFailed, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnApiError", agent: agent.GetValue(), detail: failed.GetMessage()})
}

func (s *feedSink) OnAgentTerminal(_ ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, _ *conversationv1.AgentSuccess, _ *conversationv1.AgentFailure, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnAgentTerminal", agent: agent.GetValue(), turn: turn})
}

func (s *feedSink) OnDetachedWork(_ ids.WorkspaceID, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnDetachedWork", agent: agent.GetValue(), detail: work.GetWork().GetValue()})
}

func (s *feedSink) OnBash(_ ids.WorkspaceID, work *conversationv1.DetachedWorkId, _ *conversationv1.AgentBash, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnBash", detail: work.GetValue()})
}

func (s *feedSink) OnSessionUpdate(_ ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	s.rec.emit(event{sink: "feed", method: "OnSessionUpdate", detail: sessionArm(update)})
}

func (s *feedSink) OnHistoryPage(_ ids.WorkspaceID, agent *conversationv1.AgentId, page *conversationv1.HistoryPage, _ OutputAddress) {
	s.rec.emit(event{sink: "feed", method: "OnHistoryPage", agent: agent.GetValue(), detail: itoa(len(page.GetEntries()))})
}

type footerSink struct{ rec *recorder }

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

func (s *footerSink) OnSessionUpdate(_ ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	s.rec.emit(event{sink: "footer", method: "OnSessionUpdate", detail: sessionArm(update)})
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

type lifecycleSink struct{ rec *recorder }

func (s *lifecycleSink) OnTurnEnded(_ ids.WorkspaceID, turn ids.TurnID, how TurnClose) {
	held := turn
	s.rec.emit(event{sink: "lifecycle", method: "OnTurnEnded", turn: &held, close: how})
}

func (s *lifecycleSink) OnLiveWorkChanged(_ ids.WorkspaceID, live LiveWorkSet) {
	held := live
	s.rec.emit(event{sink: "lifecycle", method: "OnLiveWorkChanged", live: &held})
}

func (s *lifecycleSink) OnNotification(_ ids.WorkspaceID, note HostNotification) {
	held := note
	s.rec.emit(event{sink: "lifecycle", method: "OnNotification", note: &held})
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

	session *fakeStream[*conversationv1.SessionUpdate]
	main    *fakeStream[*shimv1.WatchAgentResponse]
	mainReq *shimv1.WatchAgentRequest
}

// newHarness starts a watcher on the given opening session and drains
// everything the start itself emitted, so a test asserts only on what it
// sends.
func newHarness(t *testing.T, session Session) *harness {
	t.Helper()
	h := &harness{t: t, client: newFakeClient(), rec: newRecorder(), log: dlog.NewTestLogger()}

	started, err := Start(context.Background(), ids.WorkspaceID("ws-1"), h.client, session, Sinks{
		Feed:      &feedSink{rec: h.rec},
		Footer:    &footerSink{rec: h.rec},
		Topbar:    &topbarSink{rec: h.rec},
		Sidebar:   &sidebarSink{rec: h.rec},
		Lifecycle: &lifecycleSink{rec: h.rec},
	}, h.log)
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	h.w = started.(*watcher)
	t.Cleanup(func() { _ = h.w.Close() })

	h.session = h.client.nextSessionOpen(t)
	open := h.client.nextAgentOpen(t)
	h.main, h.mainReq = open.stream, open.req
	return h
}

// quiet drains every event emitted so far by routing a sentinel down the main
// agent's stream and reading up to it.
func (h *harness) quiet() {
	h.t.Helper()
	h.sentinel(h.main)
}

// sentinel pushes a context cut, whose routing is fixed and short (the feed
// then the footer), and reads the recorder up to it. Everything returned is
// what the frame under test provoked.
func (h *harness) sentinel(stream *fakeStream[*shimv1.WatchAgentResponse]) []event {
	h.t.Helper()
	stream.send(h.t, entryFrame(frameUpdate("sentinel", &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: &conversationv1.ContextCut{}},
	})))
	// The cut reaches the feed AND the footer, and the footer is second, so
	// the FOOTER's call is the sentinel: reading only to the feed's would
	// leave the footer's behind to pollute the next assertion.
	seen := h.rec.until(h.t, "footer.OnContextCut")
	if len(seen) > 0 && seen[len(seen)-1].name() == "feed.OnContextCut" {
		seen = seen[:len(seen)-1]
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

// warnings returns every WARN and ERROR record the watcher logged.
func (h *harness) warnings() []dlog.Record {
	var out []dlog.Record
	for _, r := range h.log.Records() {
		if r.Level == "warn" || r.Level == "error" {
			out = append(out, r)
		}
	}
	return out
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

// frameFailure is an agent's stream ending because something broke.
func frameFailure(agent string) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: agentID(agent),
		Result: &conversationv1.AgentFrame_Failure{Failure: &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_ExecutionError{ExecutionError: &conversationv1.AgentExecutionError{}},
		}},
	}
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

// pageFrame is a watch's opening catch-up page.
func pageFrame(entries ...*conversationv1.HistoryEntryAt) *shimv1.WatchAgentResponse {
	return &shimv1.WatchAgentResponse{Frame: &shimv1.WatchAgentResponse_Page{
		Page: &conversationv1.HistoryPage{Entries: entries},
	}}
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

// bashActivity is an in-turn shell call, the unit a detached shell detaches
// from.
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

// createdWork is a detached-work announcement whose origin STATES the kind.
func createdWork(work string, created *conversationv1.DetachableWork) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Work:   workID(work),
		Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{WorkCreated: created}},
	}
}

// detachedWork is an announcement whose origin names only the in-turn unit the
// work used to be.
func detachedWork(work, from string) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Work: workID(work),
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: &conversationv1.AgentActivityId{Value: from},
			Cause:          &conversationv1.DetachedWorkDetached_Requested{Requested: &conversationv1.DetachedCauseRequested{}},
		}},
	}
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

func budgetWarningUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_ContextBudgetWarning{
		ContextBudgetWarning: &conversationv1.SessionContextBudgetWarning{Text: "the context is filling"},
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
