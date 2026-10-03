package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"sync"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"
)

// Stream names accepted by the control plane.  These are Emacs's three
// streams (elisp.md, "The HOST section" and "The daemon handover"); every
// other agentrepl.v1 stream belongs to the webapp and the fake refuses it.
const (
	streamHost   = "host"
	streamDaemon = "daemon"
	streamRoster = "roster"
)

// errAborted is returned by a stream handler that was told to drop its TCP
// connection without an end frame (a producer-side end without a terminal
// frame — the transport failure the elisp transport must detect).
var errAborted = errors.New("fakedaemon: stream aborted without an end frame")

type recordedCall struct {
	Method string          `json:"method"`
	Body   json.RawMessage `json:"body"`
	// Raw is the request EXACTLY as the client wrote it.  Body is a
	// re-marshal of the decoded message and so drops explicit zero values;
	// Raw is what an assertion about explicit `false' encoding must read.
	Raw string `json:"raw,omitempty"`
	// Headers are the request headers EXACTLY as the client sent them,
	// canonically named.  The Connect protocol fixes them (fanout §3:
	// `Content-Type' plus `Connect-Protocol-Version: 1', and a distinct
	// streaming content type), and nothing else in the recording can see
	// them.
	Headers map[string]string `json:"headers,omitempty"`
}

type endRequest struct {
	err   *connect.Error
	abort bool
}

type subscriber struct {
	id          int64
	stream      string
	workspaceID string
	msgs        chan proto.Message
	end         chan endRequest
	conn        net.Conn
}

// subscriberInfo is the /_fake/subscribers view of one open stream.
type subscriberInfo struct {
	ID          int64  `json:"id"`
	Stream      string `json:"stream"`
	WorkspaceID string `json:"workspace_id,omitempty"`
}

type snapshotKey struct {
	stream      string
	workspaceID string
}

type fakeServer struct {
	mu          sync.Mutex
	subChanged  *sync.Cond
	callChanged *sync.Cond
	calls       []recordedCall
	scripts     map[string]json.RawMessage
	subscribers map[int64]*subscriber
	snapshots   map[snapshotKey][]proto.Message
	// gates holds one channel per method whose answer is being withheld; see
	// gate.go.
	gates     map[string]chan struct{}
	nextSubID int64
}

func newFakeServer() *fakeServer {
	s := &fakeServer{
		scripts:     map[string]json.RawMessage{},
		subscribers: map[int64]*subscriber{},
		snapshots:   map[snapshotKey][]proto.Message{},
		gates:       map[string]chan struct{}{},
	}
	s.subChanged = sync.NewCond(&s.mu)
	s.callChanged = sync.NewCond(&s.mu)
	return s
}

// awaitSubscribersBound is how long awaitSubscribers waits. A subscription
// to this in-process fake lands in milliseconds (the slowest seen across the
// suite is well under 100ms), so this is a generous multiple of the healthy
// maximum: a wait that reaches it is a subscription that never came.
const awaitSubscribersBound = 2 * time.Second

// awaitSubscribers blocks until at least N subscribers of STREAM (and, for
// the host stream, of WORKSPACEID) are registered, or WITHIN has passed, in
// which case it answers an error naming what it waited for and what it saw.
// Real synchronization on the registry's own condition variable, so a caller
// never guesses how long a subscription takes to land; the bound only turns a
// subscription that never lands into a loud failure instead of a hang.
func (s *fakeServer) awaitSubscribers(stream, workspaceID string, n int, within time.Duration) error {
	deadline := time.Now().Add(within)
	expired := false
	timer := time.AfterFunc(within, func() {
		s.mu.Lock()
		expired = true
		s.mu.Unlock()
		s.subChanged.Broadcast()
	})
	defer timer.Stop()
	s.mu.Lock()
	defer s.mu.Unlock()
	for {
		have := s.countSubscribersLocked(stream, workspaceID)
		if have >= n {
			return nil
		}
		if expired || !time.Now().Before(deadline) {
			return fmt.Errorf("fakedaemon: waited %s for %d subscriber(s) of stream %q (workspace %q); %d registered",
				within, n, stream, workspaceID, have)
		}
		s.subChanged.Wait()
	}
}

func (s *fakeServer) countSubscribersLocked(stream, workspaceID string) int {
	count := 0
	for _, sub := range s.subscribers {
		if sub.stream != stream {
			continue
		}
		if stream == streamHost && sub.workspaceID != workspaceID {
			continue
		}
		count++
	}
	return count
}

// ---- recording ----

func (s *fakeServer) record(ctx context.Context, method string, msg proto.Message) {
	body, err := protojson.Marshal(msg)
	if err != nil {
		// Cannot happen for a message the codec already decoded; log loudly
		// rather than silently dropping the call from the recording.
		logError("fakedaemon.record.marshal-failed", "could not re-marshal a decoded request",
			map[string]any{"method": method, "error": err.Error()})
		body = []byte("null")
	}
	s.mu.Lock()
	s.calls = append(s.calls, recordedCall{
		Method:  method,
		Body:    body,
		Raw:     rawBodyFrom(ctx),
		Headers: headersFrom(ctx),
	})
	n := len(s.calls)
	s.callChanged.Broadcast()
	s.mu.Unlock()
	logDebug("fakedaemon.rpc.recorded", "recorded a unary request",
		map[string]any{"method": method, "index": n - 1, "body": string(body)})
}

// awaitRecordedCalls blocks until N calls of METHOD are on the record.  Real
// synchronization on the recorder's own condition variable, so a caller never
// guesses how long an in-flight call takes to land.
func (s *fakeServer) awaitRecordedCalls(method string, n int) {
	s.mu.Lock()
	defer s.mu.Unlock()
	for s.countCallsLocked(method) < n {
		s.callChanged.Wait()
	}
}

func (s *fakeServer) countCallsLocked(method string) int {
	count := 0
	for _, call := range s.calls {
		if call.Method == method {
			count++
		}
	}
	return count
}

func (s *fakeServer) recordedCalls() []recordedCall {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]recordedCall, len(s.calls))
	copy(out, s.calls)
	return out
}

// ---- scripting ----

func (s *fakeServer) script(method string, body json.RawMessage) {
	s.mu.Lock()
	s.scripts[method] = body
	s.mu.Unlock()
	logInfo("fakedaemon.script.set", "scripted a unary response",
		map[string]any{"method": method, "response": string(body)})
}

func (s *fakeServer) scriptedBody(method string) (json.RawMessage, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	body, ok := s.scripts[method]
	return body, ok
}

// ---- subscribers ----

func (s *fakeServer) addSubscriber(stream, workspaceID string, conn net.Conn) *subscriber {
	s.mu.Lock()
	s.nextSubID++
	sub := &subscriber{
		id:          s.nextSubID,
		stream:      stream,
		workspaceID: workspaceID,
		// Buffered so a control-plane push never blocks on a subscriber the
		// test has not yet drained; the stream goroutine drains in order.
		msgs: make(chan proto.Message, 64),
		end:  make(chan endRequest, 1),
		conn: conn,
	}
	s.subscribers[sub.id] = sub
	s.subChanged.Broadcast()
	snaps := append([]proto.Message(nil), s.snapshots[snapshotKey{stream, workspaceID}]...)
	s.mu.Unlock()
	logInfo("fakedaemon.stream.subscribed", "stream subscriber registered",
		map[string]any{"id": sub.id, "stream": stream, "workspace_id": workspaceID, "snapshots": len(snaps)})
	for _, snap := range snaps {
		sub.msgs <- snap
	}
	return sub
}

func (s *fakeServer) removeSubscriber(sub *subscriber) {
	s.mu.Lock()
	delete(s.subscribers, sub.id)
	s.subChanged.Broadcast()
	s.mu.Unlock()
	logInfo("fakedaemon.stream.unsubscribed", "stream subscriber removed",
		map[string]any{"id": sub.id, "stream": sub.stream, "workspace_id": sub.workspaceID})
}

func (s *fakeServer) subscriberInfos() []subscriberInfo {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]subscriberInfo, 0, len(s.subscribers))
	for _, sub := range s.subscribers {
		out = append(out, subscriberInfo{ID: sub.id, Stream: sub.stream, WorkspaceID: sub.workspaceID})
	}
	return out
}

func (s *fakeServer) matchingSubscribers(stream, workspaceID string) []*subscriber {
	s.mu.Lock()
	defer s.mu.Unlock()
	var out []*subscriber
	for _, sub := range s.subscribers {
		if sub.stream != stream {
			continue
		}
		if stream == streamHost && sub.workspaceID != workspaceID {
			continue
		}
		out = append(out, sub)
	}
	return out
}

// push delivers MSG to every open subscriber of STREAM (and, for the host
// stream, of WORKSPACEID).  Returns how many subscribers it reached.
func (s *fakeServer) push(stream, workspaceID string, msg proto.Message, snapshot bool) int {
	if snapshot {
		key := snapshotKey{stream, workspaceID}
		s.mu.Lock()
		s.snapshots[key] = append(s.snapshots[key], msg)
		s.mu.Unlock()
		logInfo("fakedaemon.stream.snapshot-stored", "stored a replay snapshot",
			map[string]any{"stream": stream, "workspace_id": workspaceID})
	}
	subs := s.matchingSubscribers(stream, workspaceID)
	for _, sub := range subs {
		sub.msgs <- msg
	}
	logInfo("fakedaemon.stream.pushed", "pushed to matching subscribers",
		map[string]any{"stream": stream, "workspace_id": workspaceID, "subscribers": len(subs)})
	return len(subs)
}

// endStreams ends every matching subscriber, either with an end frame
// (carrying ERR when non-nil) or, with ABORT, by dropping the TCP
// connection so no end frame is written at all.
func (s *fakeServer) endStreams(stream, workspaceID string, req endRequest) int {
	subs := s.matchingSubscribers(stream, workspaceID)
	for _, sub := range subs {
		sub.end <- req
	}
	logInfo("fakedaemon.stream.ended", "ended matching subscribers",
		map[string]any{"stream": stream, "workspace_id": workspaceID,
			"subscribers": len(subs), "abort": req.abort, "with_error": req.err != nil})
	return len(subs)
}

// abortAllStreams drops every standing stream's TCP connection, with no end
// frame, and reports how many it dropped.
//
// THIS IS THE EXIT PATH, and a standing stream is why it needs one.  A
// subscription never concludes on its own, so `http.Server.Shutdown' — which
// waits for in-flight requests to return — waits for streams that by
// construction never will, burns its whole grace period, times out, and only
// then does `http.Server.Close' drop the connections anyway.  The observable
// end is identical either way (an abrupt drop, no end frame); the difference
// is purely the seconds spent reaching it, once per daemon stop, in a suite
// that stops a daemon in most of its scenarios.
//
// Aborting first is the same act Close would have performed, done when it is
// known to be needed rather than after waiting to find out.
func (s *fakeServer) abortAllStreams() int {
	s.mu.Lock()
	subs := make([]*subscriber, 0, len(s.subscribers))
	for _, sub := range s.subscribers {
		subs = append(subs, sub)
	}
	s.mu.Unlock()

	for _, sub := range subs {
		// `end' is buffered, so a subscriber whose goroutine is already on
		// its way out cannot wedge the exit here.
		select {
		case sub.end <- endRequest{abort: true}:
		default:
			logWarn("fakedaemon.exit.stream-abort-dropped",
				"a standing stream already had an end pending at exit",
				map[string]any{"id": sub.id, "stream": sub.stream})
		}
	}
	logInfo("fakedaemon.exit.streams-aborted", "dropped every standing stream before shutdown",
		map[string]any{"subscribers": len(subs)})
	return len(subs)
}

// ---- unary plumbing ----

// handleUnary is the one body every unary method delegates to: record the
// request, then answer from the scripted table or from the default synthesis.
func handleUnary[Req any, Res any](ctx context.Context, s *fakeServer, method string, msg *Req) (*connect.Response[Res], error) {
	reqMsg, ok := any(msg).(proto.Message)
	if !ok {
		return nil, connect.NewError(connect.CodeInternal,
			fmt.Errorf("fakedaemon: request type for %s is not a proto message", method))
	}
	s.record(ctx, method, reqMsg)

	if err := validateRequest(reqMsg); err != nil {
		logError("fakedaemon.rpc.invalid-request", "refused a request that breaches the validation invariant",
			map[string]any{"method": method, "error": err.Error()})
		return nil, connect.NewError(connect.CodeInvalidArgument, err)
	}

	// The gate, if armed, holds the ANSWER — after recording and validation,
	// so a scenario sees the call land and can assert on the client's state
	// while it is still in flight.
	if gate := s.gateFor(method); gate != nil {
		logInfo("fakedaemon.gate.holding", "holding a gated answer",
			map[string]any{"method": method})
		select {
		case <-gate:
			logInfo("fakedaemon.gate.released", "a gated answer was released",
				map[string]any{"method": method})
		case <-ctx.Done():
			return nil, connect.NewError(connect.CodeCanceled, ctx.Err())
		}
	}

	out := new(Res)
	outMsg, ok := any(out).(proto.Message)
	if !ok {
		return nil, connect.NewError(connect.CodeInternal,
			fmt.Errorf("fakedaemon: response type for %s is not a proto message", method))
	}

	if body, scripted := s.scriptedBody(method); scripted {
		// The scripted body was already validated against this exact type at
		// /_fake/script time; a failure here is a fake-daemon defect.
		if err := protojson.Unmarshal(body, outMsg); err != nil {
			logError("fakedaemon.rpc.scripted-unmarshal-failed", "scripted response no longer parses",
				map[string]any{"method": method, "error": err.Error()})
			return nil, connect.NewError(connect.CodeInternal, err)
		}
		logDebug("fakedaemon.rpc.answered-scripted", "answered from the scripted table",
			map[string]any{"method": method})
		return connect.NewResponse(out), nil
	}

	if err := applyDefault(method, reqMsg, outMsg); err != nil {
		logError("fakedaemon.rpc.default-failed", "could not synthesize a default response",
			map[string]any{"method": method, "error": err.Error()})
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	logDebug("fakedaemon.rpc.answered-default", "answered from the default synthesis",
		map[string]any{"method": method})
	return connect.NewResponse(out), nil
}

// ---- reset ----

// resetCounts reports what a /_fake/reset threw away, so a caller sees that
// the fake really was carrying state rather than guessing.
type resetCounts struct {
	Calls     int `json:"calls"`
	Scripts   int `json:"scripts"`
	Snapshots int `json:"snapshots"`
	Gates     int `json:"gates"`
	Ended     int `json:"ended"`
}

// reset returns the fake to the state newFakeServer left it in, WITHOUT
// restarting the process: the recording, the scripted table, the stored
// snapshots and every armed gate are dropped, and every standing stream is
// ended with a clean end frame.
//
// THIS IS WHAT MAKES ONE FAKE SERVE A WHOLE SUITE.  A per-test process spawn
// costs seconds of boot; a per-test reset costs one request.  The clearing is
// total on purpose — a reset that left one table behind would leak exactly the
// state a fresh process used to guarantee away.
//
// An armed gate is CLOSED rather than merely forgotten, so a call still held
// by the previous test's gate is released instead of hanging until its
// client's deadline.
func (s *fakeServer) reset() resetCounts {
	s.mu.Lock()
	counts := resetCounts{
		Calls:     len(s.calls),
		Scripts:   len(s.scripts),
		Snapshots: len(s.snapshots),
		Gates:     len(s.gates),
	}
	s.calls = nil
	s.scripts = map[string]json.RawMessage{}
	s.snapshots = map[snapshotKey][]proto.Message{}
	for method, gate := range s.gates {
		delete(s.gates, method)
		close(gate)
	}
	subs := make([]*subscriber, 0, len(s.subscribers))
	for _, sub := range s.subscribers {
		subs = append(subs, sub)
	}
	s.callChanged.Broadcast()
	s.mu.Unlock()

	for _, sub := range subs {
		sub.end <- endRequest{}
	}
	counts.Ended = len(subs)
	logInfo("fakedaemon.reset", "reset the fake to its start-of-process state",
		map[string]any{"calls": counts.Calls, "scripts": counts.Scripts,
			"snapshots": counts.Snapshots, "gates": counts.Gates, "ended": counts.Ended})
	return counts
}
