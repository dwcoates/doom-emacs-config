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

// ReplayResult reports how a bounded replay ended, as the shim described it.
// Truncated means a bound tripped before to_seq was reached, so the caller
// received only PART of the range and must say so rather than presenting the
// result as complete.
type ReplayResult struct {
	Delivered uint64
	Truncated bool
	Reason    string
}

// replayWaiter is one in-flight replay: the caller's event sink plus the
// channel ReplayDone resolves.
type replayWaiter struct {
	onEvent func(*corev1.Event)
	done    chan *corev1.ReplayDone
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

// failAll resolves every in-flight replay as truncated with reason. Called on
// connection teardown: a replay whose shim went away is not going to finish,
// and leaving the caller blocked would be worse than telling it so.
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
		case w.done <- &corev1.ReplayDone{Truncated: true, Reason: reason}:
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
	w := &replayWaiter{onEvent: onEvent, done: make(chan *corev1.ReplayDone, 1)}
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
	case <-ctx.Done():
		// The caller's deadline. Say so rather than reporting a short answer as
		// complete; late ReplayEvents for this id are dropped loudly by
		// dispatchReplayEvent once the registry entry is gone.
		return ReplayResult{Truncated: true, Reason: ctx.Err().Error()},
			fmt.Errorf("shimclient: replay request_id=%s (session %s): %w", requestID, c.cfg.SessionID, ctx.Err())
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
