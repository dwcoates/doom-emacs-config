package shimclient

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"errors"
	"fmt"
	"sync"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// ErrReplayNotConnected reports that a replay was asked for on a session with
// no live shim connection.
//
// The shim IS the session's transport, so there is no second route to its
// history — and there deliberately must not be one. A daemon that dialled the
// store directly when the shim was down would be serving through a side door
// while the session's own transport is broken, which papers over an outage
// that eager-ensure already surfaces loudly (metaprompt no-fallbacks rule).
var ErrReplayNotConnected = errors.New("shimclient: no live shim connection to replay history from")

// ErrReplayLinkLost reports that the shim connection went away UNDER an
// in-flight replay, so the range was abandoned rather than bounded.
//
// It is deliberately NOT the same answer as a truncation. A truncation is the
// shim's own verdict — a cap tripped, an idle window elapsed — and means "this
// is all you are getting". A lost link means the question was never finished
// being asked, and the commonest cause is a DELIBERATE re-handshake: the shim
// bounces the daemon link when the vendor rotates its session uuid. Reporting
// that as a truncation is what turned a rotation into a failure card with zero
// events behind it, so the two are distinct errors and the caller decides.
var ErrReplayLinkLost = errors.New("shimclient: the shim connection was lost under an in-flight replay")

// ReplayResult reports how a bounded replay ended, as the shim described it.
// Truncated means a bound tripped before to_seq was reached, so the caller
// received only PART of the range and must say so rather than presenting the
// result as complete.
type ReplayResult struct {
	Delivered uint64
	Truncated bool
	Reason    string
}

// replayWaiter is one in-flight replay: the caller's event sink, the channel
// ReplayDone resolves, and the channel a LOST LINK resolves.
//
// The two completions are separate channels rather than one, because they are
// different facts and the caller acts differently on each: `done` carries the
// shim's own verdict about a bounded range, while `lost` says the connection
// carrying the question disappeared. Folding the second into a synthetic
// truncated ReplayDone — which is what this used to do — made a deliberate
// re-handshake indistinguishable from the shim hitting its cap.
type replayWaiter struct {
	onEvent func(*corev1.Event)
	done    chan *corev1.ReplayDone
	lost    chan string
}

// replayRegistry tracks in-flight replays by request id.
type replayRegistry struct {
	mu sync.Mutex
	m  map[string]*replayWaiter
}

func newReplayRegistry() *replayRegistry {
	return &replayRegistry{m: make(map[string]*replayWaiter)}
}

func (r *replayRegistry) add(id string, w *replayWaiter) {
	r.mu.Lock()
	r.m[id] = w
	r.mu.Unlock()
}

func (r *replayRegistry) remove(id string) {
	r.mu.Lock()
	delete(r.m, id)
	r.mu.Unlock()
}

func (r *replayRegistry) get(id string) (*replayWaiter, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	w, ok := r.m[id]
	return w, ok
}

// failAll resolves every in-flight replay as LINK LOST with reason. Called on
// connection teardown: a replay whose shim went away is not going to finish,
// and leaving the caller blocked would be worse than telling it so.
//
// It resolves the `lost` channel rather than fabricating a truncated
// ReplayDone. The shim never said this range was truncated; the connection it
// was being asked over ended. Saying so exactly is what lets the daemon retry a
// replay the shim's own deliberate re-handshake interrupted instead of
// reporting a bound nobody tripped.
func (r *replayRegistry) failAll(reason string) {
	r.mu.Lock()
	waiters := make([]*replayWaiter, 0, len(r.m))
	for id, w := range r.m {
		waiters = append(waiters, w)
		delete(r.m, id)
	}
	r.mu.Unlock()
	for _, w := range waiters {
		select {
		case w.lost <- reason:
		default:
		}
	}
}

// Replay asks the shim for a bounded slice of this session's persisted history
// and streams it to onEvent, returning when the shim's ReplayDone lands.
//
// fromSeq is EXCLUSIVE; toSeq is the first seq the caller's own live window
// already covers (0 = no upper bound). maxEvents caps one replay (0 = the
// shim's default).
//
// THE STRUCTURAL GUARANTEE. Replayed events arrive as `ReplayEvent`, a
// different wire type from the live `Event` stream, so the read loop's type
// switch physically cannot route them into dispatchEvent — the sink that feeds
// the SSM, the task catalog, and the progress resolver. Those planes consumed
// this history once already; applying it again is what makes historical tasks
// masquerade as live activity. Before this the separation was a daemon-side
// convention (one function that remembered not to call the others); now it is
// the frame type, and no caller can get it wrong.
//
// The standing subscription is likewise untouched: this sends a ReplayRequest,
// never a Subscribe, and last_seen_seq is never advanced from a replayed event.
func (c *Client) Replay(ctx context.Context, fromSeq, toSeq uint64, maxEvents uint32, onEvent func(*corev1.Event)) (ReplayResult, error) {
	if onEvent == nil {
		return ReplayResult{}, fmt.Errorf("shimclient: Replay needs an onEvent sink")
	}
	c.mu.Lock()
	ac := c.active
	c.mu.Unlock()
	if ac == nil {
		return ReplayResult{}, fmt.Errorf("%w (session %s)", ErrReplayNotConnected, c.cfg.SessionID)
	}

	requestID := newReplayID()
	w := &replayWaiter{onEvent: onEvent, done: make(chan *corev1.ReplayDone, 1), lost: make(chan string, 1)}
	c.replays.add(requestID, w)
	defer c.replays.remove(requestID)

	if err := ac.writeMsg(&corev1.ReplayRequest{
		RequestId: requestID,
		FromSeq:   fromSeq,
		ToSeq:     toSeq,
		MaxEvents: maxEvents,
	}); err != nil {
		return ReplayResult{}, fmt.Errorf("shimclient: sending ReplayRequest (session %s): %w", c.cfg.SessionID, err)
	}
	c.logf("replay requested request_id=%s from_seq=%d to_seq=%d max_events=%d", requestID, fromSeq, toSeq, maxEvents)

	select {
	case done := <-w.done:
		res := ReplayResult{
			Delivered: done.GetDelivered(),
			Truncated: done.GetTruncated(),
			Reason:    done.GetReason(),
		}
		c.logf("replay complete request_id=%s delivered=%d truncated=%v reason=%q",
			requestID, res.Delivered, res.Truncated, res.Reason)
		return res, nil
	case reason := <-w.lost:
		// The connection died under the replay. Truncated is still reported so a
		// caller that renders whatever arrived knows the range is incomplete, but
		// the ERROR says which kind of incomplete this is.
		c.logf("replay link lost request_id=%s: %s", requestID, reason)
		return ReplayResult{Truncated: true, Reason: reason},
			fmt.Errorf("%w: request_id=%s (session %s): %s", ErrReplayLinkLost, requestID, c.cfg.SessionID, reason)
	case <-ctx.Done():
		// Preserve the caller's cancellation CAUSE. The session driver uses a
		// progress-rearmed idle context whose cause carries delivered counts and
		// seq range; flattening it to context.Canceled would discard the exact
		// evidence needed to distinguish zero progress from a partial stream.
		cause := context.Cause(ctx)
		if cause == nil {
			panic("shimclient: replay context closed without a cancellation cause")
		}
		return ReplayResult{Truncated: true, Reason: cause.Error()},
			fmt.Errorf("shimclient: replay request_id=%s (session %s): %w", requestID, c.cfg.SessionID, cause)
	}
}

// dispatchReplayEvent routes one replayed event to the replay that asked for
// it. It NEVER touches last_seen_seq and NEVER reaches the state/frame sinks:
// this is historical content for conversation translation only, which is why
// it arrives as its own message type.
//
// An event for an unknown request id is a late frame from a replay the caller
// already gave up on. It is loud-logged and dropped — never redirected into
// the live path, which is precisely the mistake the separate type prevents.
func (c *Client) dispatchReplayEvent(re *corev1.ReplayEvent) {
	w, ok := c.replays.get(re.GetRequestId())
	if !ok {
		c.logf("replay event for unknown request_id=%s seq=%d; dropped (the replay already ended)",
			re.GetRequestId(), re.GetEvent().GetSeq())
		return
	}
	if ev := re.GetEvent(); ev != nil {
		w.onEvent(ev)
	}
}

// dispatchReplayDone resolves the replay its request id names.
func (c *Client) dispatchReplayDone(done *corev1.ReplayDone) {
	w, ok := c.replays.get(done.GetRequestId())
	if !ok {
		c.logf("replay completion for unknown request_id=%s; dropped (the replay already ended)", done.GetRequestId())
		return
	}
	select {
	case w.done <- done:
	default:
		c.logf("replay completion for request_id=%s arrived twice; ignoring the duplicate", done.GetRequestId())
	}
}

// newReplayID mints a process-unique replay correlation id.
func newReplayID() string {
	var b [6]byte
	_, _ = rand.Read(b[:])
	return "replay-" + hex.EncodeToString(b[:])
}
