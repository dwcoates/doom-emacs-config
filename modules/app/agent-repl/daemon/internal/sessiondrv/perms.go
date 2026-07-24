package sessiondrv

import (
	"fmt"
	"sync"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"google.golang.org/protobuf/types/known/structpb"
)

// permRegistry is the rendezvous between the shimclient PermissionHandler
// (which blocks a canUseTool round-trip on the demux side) and the frontend
// command surface (which delivers the human's answer via a FrontendCommand
// PermissionAnswerCmd, §5.4). A canUseTool arrives as a PermissionRequest; the
// handler parks a waiter keyed by request_id and blocks; AnswerPermission
// resolves that waiter with the human's decision, unblocking the handler which
// then returns the PermissionResponse for the client to send back to the shim.
//
// There is NO auto-answer and NO timeout-default: an unanswered permission
// blocks honestly (the shim's canUseTool stays pending) until the human
// answers or the connection tears down (Fail). Papering a pending permission
// over with an implicit allow/deny would be exactly the silent fallback the
// no-fallbacks rule forbids.
type permRegistry struct {
	mu      sync.Mutex
	waiters map[string]chan *corev1.PermissionResponse
	logf    func(string, ...any)
}

func newPermRegistry(logf func(string, ...any)) *permRegistry {
	if logf == nil {
		logf = func(string, ...any) {}
	}
	return &permRegistry{waiters: make(map[string]chan *corev1.PermissionResponse), logf: logf}
}

// await parks a waiter for requestID and returns the channel the answer will
// arrive on, plus a release func that removes the waiter (always call release
// once done, so a torn-down or answered request never leaks). A duplicate
// request_id replaces the prior waiter (loud-logged): the shim reusing a live
// request_id is an anomaly, not something to silently multiplex.
func (p *permRegistry) await(requestID string) (<-chan *corev1.PermissionResponse, func()) {
	ch := make(chan *corev1.PermissionResponse, 1)
	p.mu.Lock()
	if _, dup := p.waiters[requestID]; dup {
		p.logf("sessiondrv: permission request_id=%s already pending; replacing waiter", requestID)
	}
	p.waiters[requestID] = ch
	p.mu.Unlock()
	release := func() {
		p.mu.Lock()
		if cur, ok := p.waiters[requestID]; ok && cur == ch {
			delete(p.waiters, requestID)
		}
		p.mu.Unlock()
	}
	return ch, release
}

// answer delivers a decision to the parked waiter for requestID, returning an
// error when no such request is pending (a stale or double answer — surfaced,
// never swallowed). allow selects ALLOW vs DENY; denyMessage and updatedInput
// ride along per the canUseTool contract.
func (p *permRegistry) answer(requestID string, allow bool, denyMessage string, updatedInput *structpb.Struct) error {
	p.mu.Lock()
	ch, ok := p.waiters[requestID]
	if ok {
		delete(p.waiters, requestID)
	}
	p.mu.Unlock()
	if !ok {
		return fmt.Errorf("sessiondrv: no pending permission for request_id=%s (stale or duplicate answer)", requestID)
	}
	ch <- buildPermissionResponse(requestID, allow, denyMessage, updatedInput)
	return nil
}

// fail resolves every outstanding waiter with a nil response (connection
// teardown). A nil response tells the shimclient handler to send nothing (the
// shim stays blocked and will re-ask on reattach) rather than fabricate a
// decision. Loud-logged per waiter.
func (p *permRegistry) fail(reason string) {
	p.mu.Lock()
	pending := p.waiters
	p.waiters = make(map[string]chan *corev1.PermissionResponse)
	p.mu.Unlock()
	for id, ch := range pending {
		p.logf("sessiondrv: permission request_id=%s abandoned: %s (no response sent; shim re-asks on reattach)", id, reason)
		ch <- nil
	}
}

// buildPermissionResponse maps a frontend answer to the core PermissionResponse.
func buildPermissionResponse(requestID string, allow bool, denyMessage string, updatedInput *structpb.Struct) *corev1.PermissionResponse {
	decision := corev1.PermissionDecision_PERMISSION_DECISION_DENY
	if allow {
		decision = corev1.PermissionDecision_PERMISSION_DECISION_ALLOW
	}
	return &corev1.PermissionResponse{
		RequestId:    requestID,
		Decision:     decision,
		UpdatedInput: updatedInput,
		DenyMessage:  denyMessage,
	}
}
