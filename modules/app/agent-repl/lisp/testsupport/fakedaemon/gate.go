package main

// THE ANSWER GATE.  Some contract sentences are about ORDER, not content:
// "call AdoptHostWorkspace ... THEN cancel the old stream and re-subscribe"
// is only pinned by observing the client WHILE the adopt is still in flight.
// A gate holds one unary method's answer open — the request is recorded and
// validated as usual, and the response is withheld until the control plane
// releases it — so a scenario can assert the client's intermediate state.

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
