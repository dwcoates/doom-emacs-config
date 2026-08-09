package sessioncontroller

import (
	"fmt"
	"sort"
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
//
// THE RENDEZVOUS IS RE-ARMABLE, because the shim re-sends every unanswered
// PermissionRequest on every reattach (control.ts resendPending). A registry
// that only ever saw each request_id once cannot serve that: a re-send may
// find no waiter at all (this daemon restarted, or the previous one abandoned
// the request), may find one already parked (two reconnects in quick
// succession), or may name a request this daemon ALREADY ANSWERED (the answer
// was recorded but its PermissionResponse never reached the shim). await and
// recall between them make all three idempotent.
//
// A request_id is the shim's per-canUseTool uuid, so identity here is exact
// and a re-send is provably the same question rather than a similar one.

// answeredMemoryLimit bounds the recalled-answer ring. A re-send can only name
// a request the shim still believes open, and a session holds a handful of
// those at most, so this is generous by orders of magnitude while keeping the
// memory O(1) for a daemon that runs for weeks.
const answeredMemoryLimit = 64

// permWaiter is the parked side of one request_id. It holds ONE channel per
// parked handler rather than one channel outright: a re-sent request arrives on
// a fresh shim connection with its own handler goroutine, and both goroutines
// must be released by the single answer. Replacing the waiter instead would
// wedge the displaced handler on a channel nobody writes to.
type permWaiter struct {
	chans     []chan *corev1.PermissionResponse
	workspace string
}

type permRegistry struct {
	mu      sync.Mutex
	waiters map[string]*permWaiter
	// answered remembers recent decisions so a re-sent request whose answer
	// never reached the shim is served the RECORDED answer instead of asking
	// the human the same question a second time.
	//
	// IN-MEMORY, NOT DURABLE, DELIBERATELY. The window this closes is a
	// response frame lost to a connection drop while this daemon lived; the
	// memory therefore needs exactly this daemon's lifetime. Persisting it
	// would replay an answer across a daemon restart into a frontend that has
	// rebuilt its conversation from scratch, which is the one case where
	// re-asking is the honest outcome — the human's answer is genuinely gone
	// with the process that held it. Re-asking is merely annoying; replaying a
	// remembered answer is correct only while the identity it was recorded
	// against is still this process's, which is exactly what an in-memory ring
	// guarantees.
	answered      map[string]*corev1.PermissionResponse
	answeredOrder []string
	logf          func(string, ...any)
}

func newPermRegistry(logf func(string, ...any)) *permRegistry {
	if logf == nil {
		logf = func(string, ...any) {}
	}
	return &permRegistry{
		waiters:  make(map[string]*permWaiter),
		answered: make(map[string]*corev1.PermissionResponse),
		logf:     logf,
	}
}

// await parks a waiter for requestID (tagged with its workspace, so the
// daemon's GET /sessions can list a workspace's pending permission ids) and
// returns the channel this caller's answer will arrive on, plus a release func
// that removes it (always call release once done, so a torn-down or answered
// request never leaks). The registry entry survives until its LAST parked
// caller releases.
//
// A duplicate request_id JOINS the existing rendezvous instead of replacing it:
// it is the shim restating an ask this daemon already holds, and the one answer
// resolves every parked caller.
func (p *permRegistry) await(requestID, workspace string) (<-chan *corev1.PermissionResponse, func()) {
	ch := make(chan *corev1.PermissionResponse, 1)
	p.mu.Lock()
	w, dup := p.waiters[requestID]
	if dup {
		p.logf("session-controller: permission request_id=%s already pending; joining the existing rendezvous (re-sent ask, %d parked)", requestID, len(w.chans)+1)
	} else {
		w = &permWaiter{workspace: workspace}
		p.waiters[requestID] = w
	}
	w.chans = append(w.chans, ch)
	p.mu.Unlock()
	release := func() {
		p.mu.Lock()
		if cur, ok := p.waiters[requestID]; ok && cur == w {
			for i, held := range w.chans {
				if held == ch {
					w.chans = append(w.chans[:i], w.chans[i+1:]...)
					break
				}
			}
			if len(w.chans) == 0 {
				delete(p.waiters, requestID)
			}
		}
		p.mu.Unlock()
	}
	return ch, release
}

// recall returns the recorded decision for requestID when this daemon already
// answered it, so a re-sent request is served that answer rather than re-asked.
func (p *permRegistry) recall(requestID string) (*corev1.PermissionResponse, bool) {
	p.mu.Lock()
	defer p.mu.Unlock()
	resp, ok := p.answered[requestID]
	return resp, ok
}

// rememberAnswered records a decision in the bounded ring. Caller holds p.mu.
func (p *permRegistry) rememberAnswered(requestID string, resp *corev1.PermissionResponse) {
	if _, dup := p.answered[requestID]; !dup {
		p.answeredOrder = append(p.answeredOrder, requestID)
	}
	p.answered[requestID] = resp
	for len(p.answeredOrder) > answeredMemoryLimit {
		evicted := p.answeredOrder[0]
		p.answeredOrder = p.answeredOrder[1:]
		delete(p.answered, evicted)
	}
}

// idsForWorkspace returns the request ids of every pending permission bound to
// workspace, sorted for stable output (the GET /sessions pending_permissions
// field, SUPERSEDED S7).
func (p *permRegistry) idsForWorkspace(workspace string) []string {
	p.mu.Lock()
	defer p.mu.Unlock()
	var ids []string
	for id, w := range p.waiters {
		if w.workspace == workspace {
			ids = append(ids, id)
		}
	}
	sort.Strings(ids)
	return ids
}

// answer delivers a decision to every caller parked on requestID, returning an
// error when no such request is pending (a stale or double answer — surfaced,
// never swallowed). allow selects ALLOW vs DENY; denyMessage and updatedInput
// ride along per the canUseTool contract. The decision is also recorded, so a
// re-send that crosses the answer is served it rather than re-asked.
func (p *permRegistry) answer(requestID string, allow bool, denyMessage string, updatedInput *structpb.Struct) error {
	resp := buildPermissionResponse(requestID, allow, denyMessage, updatedInput)
	p.mu.Lock()
	w, ok := p.waiters[requestID]
	if ok {
		delete(p.waiters, requestID)
		p.rememberAnswered(requestID, resp)
	}
	p.mu.Unlock()
	if !ok {
		return fmt.Errorf("session-controller: no pending permission for request_id=%s (stale or duplicate answer)", requestID)
	}
	for _, ch := range w.chans {
		ch <- resp
	}
	return nil
}

// fail resolves every outstanding waiter with a nil response (connection
// teardown). A nil response tells the shimclient handler to send nothing (the
// shim stays blocked and will re-ask on reattach) rather than fabricate a
// decision. Loud-logged per waiter.
//
// Recorded answers are NOT cleared: the shim's re-send on reattach is exactly
// the case they exist for, and an abandoned request that turns out to have been
// answered already must be served that answer rather than re-asked.
func (p *permRegistry) fail(reason string) {
	p.mu.Lock()
	pending := p.waiters
	p.waiters = make(map[string]*permWaiter)
	p.mu.Unlock()
	for id, w := range pending {
		p.logf("session-controller: permission request_id=%s abandoned: %s (no response sent; shim re-asks on reattach)", id, reason)
		for _, ch := range w.chans {
			ch <- nil
		}
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
