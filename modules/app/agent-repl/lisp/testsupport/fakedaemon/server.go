package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"sync"

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
	calls       []recordedCall
	scripts     map[string]json.RawMessage
	subscribers map[int64]*subscriber
	snapshots   map[snapshotKey][]proto.Message
	nextSubID   int64
}

func newFakeServer() *fakeServer {
	s := &fakeServer{
		scripts:     map[string]json.RawMessage{},
		subscribers: map[int64]*subscriber{},
		snapshots:   map[snapshotKey][]proto.Message{},
	}
	s.subChanged = sync.NewCond(&s.mu)
	return s
}

// awaitSubscribers blocks until at least N subscribers of STREAM (and, for
// the host stream, of WORKSPACEID) are registered.  Real synchronization on
// the registry's own condition variable, so a caller never has to guess how
// long a subscription takes to land.
func (s *fakeServer) awaitSubscribers(stream, workspaceID string, n int) {
	s.mu.Lock()
	defer s.mu.Unlock()
	for s.countSubscribersLocked(stream, workspaceID) < n {
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

func (s *fakeServer) record(method string, msg proto.Message) {
	body, err := protojson.Marshal(msg)
	if err != nil {
		// Cannot happen for a message the codec already decoded; log loudly
		// rather than silently dropping the call from the recording.
		logError("fakedaemon.record.marshal-failed", "could not re-marshal a decoded request",
			map[string]any{"method": method, "error": err.Error()})
		body = []byte("null")
	}
	s.mu.Lock()
	s.calls = append(s.calls, recordedCall{Method: method, Body: body})
	n := len(s.calls)
	s.mu.Unlock()
	logDebug("fakedaemon.rpc.recorded", "recorded a unary request",
		map[string]any{"method": method, "index": n - 1, "body": string(body)})
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

// ---- unary plumbing ----

// handleUnary is the one body every unary method delegates to: record the
// request, then answer from the scripted table or from the default synthesis.
func handleUnary[Req any, Res any](ctx context.Context, s *fakeServer, method string, msg *Req) (*connect.Response[Res], error) {
	reqMsg, ok := any(msg).(proto.Message)
	if !ok {
		return nil, connect.NewError(connect.CodeInternal,
			fmt.Errorf("fakedaemon: request type for %s is not a proto message", method))
	}
	s.record(method, reqMsg)

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
