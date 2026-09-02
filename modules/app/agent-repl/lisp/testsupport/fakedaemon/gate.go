package main

import (
	"context"

	"connectrpc.com/connect"
)

// THE ANSWER GATE.  Some contract sentences are about ORDER, not content:
// "call AdoptHostWorkspace ... THEN cancel the old stream and re-subscribe"
// is only pinned by observing the client WHILE the adopt is still in flight.
// A gate holds one method's answer open — the request is recorded and
// validated as usual, and the answer is withheld until the control plane
// releases it — so a scenario can assert the client's intermediate state.
//
// For a UNARY method the answer is the response message.  For a STREAM
// method the answer is its ACCEPTANCE: the response header block, which is
// what the client reads as "this subscription stands" (fanout §3
// STANDING-STREAM ACCEPTANCE).  A gated stream is therefore a stream that has
// been DIALLED but not accepted — the only way to observe a client that must
// wait for acceptance before acting, and something no unary gate can stage.

// streamMethods is the closed set of stream rpcs a gate may hold the
// acceptance of: the three streams Emacs itself opens.  Every other
// agentrepl.v1 stream belongs to the webapp and this fake refuses it outright,
// so gating one would stage nothing.
var streamMethods = map[string]struct{}{
	"WatchHostWorkspace":   {},
	"WatchDaemon":          {},
	"WatchWorkspaceRoster": {},
}

// armGate makes the next (and every subsequent) call of METHOD block before
// answering.  Arming an already-armed method is a no-op, so a scenario may
// arm defensively.
func (s *fakeServer) armGate(method string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if _, ok := s.gates[method]; !ok {
		s.gates[method] = make(chan struct{})
	}
}

// releaseGate lets every held call of METHOD answer and disarms the gate.
// Reports whether a gate was armed, so releasing one that never existed is a
// loud control-plane 400 rather than a silent success.
func (s *fakeServer) releaseGate(method string) bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	gate, ok := s.gates[method]
	if !ok {
		return false
	}
	delete(s.gates, method)
	close(gate)
	return true
}

// gateFor returns METHOD's armed gate, or nil.
func (s *fakeServer) gateFor(method string) chan struct{} {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.gates[method]
}

// awaitAcceptanceGate holds a stream's ACCEPTANCE while METHOD is gated.  It
// runs after the request was recorded and validated and BEFORE the
// subscription is registered, so a gated stream lists no subscriber and
// flushes no headers: the dial stands, unaccepted, exactly as a client that
// must wait for acceptance sees it.  A client that gives up first cancels the
// context and the hold ends with it.
func awaitAcceptanceGate(ctx context.Context, s *fakeServer, method string) error {
	gate := s.gateFor(method)
	if gate == nil {
		return nil
	}
	logInfo("fakedaemon.gate.holding-acceptance", "withholding a stream's acceptance",
		map[string]any{"method": method})
	select {
	case <-gate:
		logInfo("fakedaemon.gate.acceptance-released", "a withheld stream acceptance was released",
			map[string]any{"method": method})
		return nil
	case <-ctx.Done():
		logInfo("fakedaemon.gate.acceptance-cancelled", "the client cancelled a stream while its acceptance was withheld",
			map[string]any{"method": method})
		return connect.NewError(connect.CodeCanceled, ctx.Err())
	}
}
