package sessioncontroller

import (
	"context"
	"fmt"
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/shimclient"
)

// repullIdleTimeout bounds how long one below-floor re-pull may make no
// progress. Every replayed event rearms it, so a large healthy stream may run
// for arbitrarily longer than this while a wedged replay still fails finitely.
const repullIdleTimeout = 60 * time.Second

// repullProgressLogEvery bounds progress instrumentation on large histories.
const repullProgressLogEvery = 512

// repullMaxEvents caps how many events ONE re-pull may deliver. Generously
// above the largest observed backfill burst (~1,009 events) and the retained
// ring (4,096), while making an unbounded replay impossible. Carried on the
// ReplayRequest so the bound is the REQUESTER's stated policy rather than
// something the shim invents on the daemon's behalf.
const repullMaxEvents = 20000

// ErrRepullInFlight reports that the workspace already has a re-pull running
// that does NOT cover the newly requested range. Its value lives in
// internal/errclass beside its classification; this is the historic name.
var ErrRepullInFlight = errclass.ErrRepullInFlight

// ErrRepullTruncated reports that a re-pull hit one of its bounds before
// reaching the retained floor, so the frontend received only part of the
// history it asked for. Surfaced, never presented as a complete answer.
var ErrRepullTruncated = errclass.ErrRepullTruncated

// repullState is one workspace's in-flight re-pull, guarded by m.mu.
type repullState struct {
	fromSeq uint64
	stopAt  uint64
	// epoch is sessionController.rotEpoch as it stood when this re-pull started — the
	// generation of the seq space its bounds are expressed in. Comparing bounds
	// across generations is comparing numbers from different spaces, so the
	// coalescing rule refuses rather than pretends.
	epoch uint64
}

type repullTimer interface {
	Stop() bool
	Reset(time.Duration) bool
}

type repullSnapshot struct {
	delivered uint64
	firstSeq  uint64
	lastSeq   uint64
	elapsed   time.Duration
}

// repullNoProgressError is the activity deadline's owned failure. Counts and
// seq range make a timeout distinguish "the store never yielded its first row"
// from "streaming stopped partway through a large replay".
type repullNoProgressError struct {
	timeout   time.Duration
	delivered uint64
	firstSeq  uint64
	lastSeq   uint64
}

func (e *repullNoProgressError) Error() string {
	return fmt.Sprintf("session-controller: history re-pull made no progress for %s after delivered=%d first_seq=%d last_seq=%d",
		e.timeout, e.delivered, e.firstSeq, e.lastSeq)
}

// repullActivity owns one progress-rearmed cancellation context. expire
// re-checks the elapsed idle duration before cancelling, which makes a timer
// callback racing with Observe harmless: recent progress rearms the remaining
// interval instead of losing to a stale callback.
type repullActivity struct {
	mu           sync.Mutex
	ctx          context.Context
	cancel       context.CancelCauseFunc
	timer        repullTimer
	timeout      time.Duration
	now          func() time.Time
	started      time.Time
	lastActivity time.Time
	delivered    uint64
	firstSeq     uint64
	lastSeq      uint64
	stopped      bool
}

func newRepullActivity(parent context.Context, timeout time.Duration) *repullActivity {
	return newRepullActivityWithClock(parent, timeout, time.Now, func(d time.Duration, f func()) repullTimer {
		return time.AfterFunc(d, f)
	})
}

func newRepullActivityWithClock(
	parent context.Context,
	timeout time.Duration,
	now func() time.Time,
	newTimer func(time.Duration, func()) repullTimer,
) *repullActivity {
	if timeout <= 0 || now == nil || newTimer == nil {
		panic("session-controller: repull activity requires a positive timeout, clock, and timer factory")
	}
	ctx, cancel := context.WithCancelCause(parent)
	started := now()
	a := &repullActivity{
		ctx:          ctx,
		cancel:       cancel,
		timeout:      timeout,
		now:          now,
		started:      started,
		lastActivity: started,
	}
	a.timer = newTimer(timeout, a.expire)
	if a.timer == nil {
		panic("session-controller: repull activity timer factory returned nil")
	}
	return a
}

func (a *repullActivity) observe(seq uint64) repullSnapshot {
	a.mu.Lock()
	defer a.mu.Unlock()
	if a.stopped {
		return a.snapshotLocked()
	}
	a.delivered++
	if a.delivered == 1 {
		a.firstSeq = seq
	}
	a.lastSeq = seq
	a.lastActivity = a.now()
	a.timer.Reset(a.timeout)
	return a.snapshotLocked()
}

func (a *repullActivity) expire() {
	a.mu.Lock()
	if a.stopped {
		a.mu.Unlock()
		return
	}
	idleFor := a.now().Sub(a.lastActivity)
	if idleFor < a.timeout {
		a.timer.Reset(a.timeout - idleFor)
		a.mu.Unlock()
		return
	}
	a.stopped = true
	cause := &repullNoProgressError{
		timeout:   a.timeout,
		delivered: a.delivered,
		firstSeq:  a.firstSeq,
		lastSeq:   a.lastSeq,
	}
	a.mu.Unlock()
	a.cancel(cause)
}

func (a *repullActivity) stop() repullSnapshot {
	a.mu.Lock()
	if !a.stopped {
		a.stopped = true
		a.timer.Stop()
	}
	snapshot := a.snapshotLocked()
	a.mu.Unlock()
	a.cancel(context.Canceled)
	return snapshot
}

func (a *repullActivity) snapshotLocked() repullSnapshot {
	return repullSnapshot{
		delivered: a.delivered,
		firstSeq:  a.firstSeq,
		lastSeq:   a.lastSeq,
		elapsed:   a.now().Sub(a.started),
	}
}

// ---------------------------------------------------------------------------
// Why a below-floor re-pull exists, and why it goes THROUGH THE SHIM
// ---------------------------------------------------------------------------
//
// The daemon deliberately subscribes each shim from its HIGH-WATER mark. That
// is a settled tradeoff with a scar to show for it: a resumed conversation that
// re-subscribed from 0 replayed thousands of frames down the same connection
// the control Acks ride, and the first prompt after a restart blew its 10s ack
// timeout while the daemon re-read the whole history (see
// RegistrySeqStore.LastSeq). Nothing here reopens that.
//
// What this reopens is narrower: a FRONTEND — not the daemon — may ask for
// history the daemon no longer holds. The retained ring is 4,096 events and is
// empty outright after a restart, so `resync(fromSeq)` below the ring's oldest
// retained seq would otherwise answer with silence, which is precisely the
// blank-feed bug.
//
// # The store stays behind the agent-shim facade
//
// An earlier version of this dialled the shim-store DIRECTLY (a deleted
// internal/storesub package). It worked, and it was wrong twice over:
//
//   - It LEAKED FACADE INTERNALS into the daemon. The store's socket location,
//     its connection lifecycle, and the rule that its seq space is keyed by the
//     vendor uuid all became daemon knowledge. The agent-shim exists so the
//     daemon consumes exactly one totally-ordered stream per session and knows
//     nothing about how it is produced; a second, private route to the same
//     data dissolves that boundary.
//   - Its one apparent ADVANTAGE — history still served while the shim is down
//     — is a FALLBACK under the metaprompt's no-fallbacks rule. The shim IS the
//     session's transport. Serving history through a side door while it is down
//     masks an outage that eager-ensure already surfaces loudly, and trades a
//     visible failure for a quietly half-working display.
//
// So the range is asked for over the shim connection (core.proto
// ReplayRequest) and the shim serves it from a throwaway store subscription of
// its own. Four standing constraints hold:
//
//  1. FRONTEND-INITIATED. Nothing in the daemon starts one. It exists only as
//     the tail of a ResyncCmd a frontend sent.
//  2. BOUNDED. to_seq is the ring floor (the first seq the live window already
//     covers); max_events caps one replay; the shim adds an idle window; this
//     side adds a progress-rearmed idle deadline. A tripped bound comes back
//     as a TRUNCATED ReplayDone and becomes ErrRepullTruncated here.
//  3. SIDE CHANNEL. The shim opens a throwaway store subscription and leaves
//     its standing one alone, so the daemon's own consumption position never
//     moves and no SeqStore write happens.
//  4. CONVERSATION ONLY — and now STRUCTURALLY so. Replayed events arrive as
//     `ReplayEvent`, a different wire type from live `Event`s, so shimclient's
//     read loop cannot route them into the SSM, the task catalog, or the
//     progress resolver even by mistake. Those planes consumed this history
//     once already; applying it again is what makes historical tasks
//     masquerade as live activity. This used to be a daemon-side convention at
//     one choke point; it is now the frame type.
//
// A daemon-standing version of any of this would be the replay storm the
// high-water subscribe exists to prevent. This is not that, and must not become
// it.

// startRepull runs a below-floor history re-pull for d over the session's shim,
// delivering replayed events to CONVERSATION TRANSLATION ONLY (constraint 4).
//
// Concurrency: at most one re-pull per workspace. A second request whose range
// is already COVERED by the in-flight one is coalesced onto it — the pull's
// output is broadcast to every subscriber of the workspace, so the second
// caller is genuinely served by the first pull rather than being told "yes"
// while nothing happens. A request reaching FURTHER BACK than the in-flight one
// is not covered, so it is refused loudly (ErrRepullInFlight) instead of being
// silently under-served.
//
// Runs synchronously: it is the tail of a ResyncCmd, and the CommandAck should
// report what actually happened rather than acknowledging an intent.
func (m *Manager) startRepull(d *sessionController, fromSeq, stopAt uint64) error {
	m.mu.Lock()
	epoch := d.rotEpoch
	if cur := d.repull; cur != nil {
		// A re-pull from a RETIRED seq space covers nothing in this one, however
		// the numbers happen to compare. Coalescing across a rotation would hand
		// this caller an in-flight pull of a conversation the vendor has retired
		// and call it served.
		sameSpace := cur.epoch == epoch
		covered := sameSpace && cur.fromSeq <= fromSeq
		m.mu.Unlock()
		if covered {
			m.logf("session-controller: coalescing re-pull ws=%q from_seq=%d onto the in-flight one (from_seq=%d stop_at=%d)",
				d.workspace, fromSeq, cur.fromSeq, cur.stopAt)
			return nil
		}
		if !sameSpace {
			return fmt.Errorf("%w: ws=%q the in-flight re-pull (from_seq=%d stop_at=%d) counts in a RETIRED vendor seq space (epoch %d, now %d), so it cannot cover the requested from_seq=%d",
				ErrRepullInFlight, d.workspace, cur.fromSeq, cur.stopAt, cur.epoch, epoch, fromSeq)
		}
		return fmt.Errorf("%w: ws=%q in-flight from_seq=%d does not cover the requested from_seq=%d",
			ErrRepullInFlight, d.workspace, cur.fromSeq, fromSeq)
	}
	d.repull = &repullState{fromSeq: fromSeq, stopAt: stopAt, epoch: epoch}
	m.mu.Unlock()
	defer func() {
		m.mu.Lock()
		d.repull = nil
		m.mu.Unlock()
	}()

	activity := newRepullActivity(m.rootCtx, repullIdleTimeout)
	defer activity.stop()
	m.logf("session-controller: re-pulling history ws=%q session=%s from_seq=%d stop_at=%d max_events=%d idle_timeout_ms=%d (frontend-initiated, via the shim, conversation only)",
		d.workspace, d.sessionID, fromSeq, stopAt, repullMaxEvents, repullIdleTimeout.Milliseconds())
	res, err := d.client.Replay(activity.ctx, fromSeq, stopAt, repullMaxEvents, func(ev *corev1.Event) {
		progress := activity.observe(ev.GetSeq())
		if progress.delivered == 1 || progress.delivered%repullProgressLogEvery == 0 {
			m.logf("session-controller: re-pull progress ws=%q session=%s from_seq=%d stop_at=%d delivered=%d first_seq=%d last_seq=%d elapsed_ms=%d idle_timeout_ms=%d",
				d.workspace, d.sessionID, fromSeq, stopAt, progress.delivered, progress.firstSeq, progress.lastSeq,
				progress.elapsed.Milliseconds(), repullIdleTimeout.Milliseconds())
		}
		d.consumer.repullConversation(ev)
	})
	progress := activity.stop()
	if err != nil {
		m.logf("session-controller: history re-pull FAILED ws=%q session=%s from_seq=%d stop_at=%d delivered=%d first_seq=%d last_seq=%d elapsed_ms=%d idle_timeout_ms=%d cause=%q",
			d.workspace, d.sessionID, fromSeq, stopAt, progress.delivered, progress.firstSeq, progress.lastSeq,
			progress.elapsed.Milliseconds(), repullIdleTimeout.Milliseconds(), err)
		return fmt.Errorf("session-controller: history re-pull for ws %q (from_seq=%d stop_at=%d) failed: %w",
			d.workspace, fromSeq, stopAt, err)
	}
	if res.Truncated {
		// A partial answer is reported as one. The frontend rendered whatever
		// did arrive; the ack tells it the rest is still missing.
		m.logf("session-controller: history re-pull TRUNCATED ws=%q session=%s from_seq=%d stop_at=%d delivered=%d observed=%d first_seq=%d last_seq=%d elapsed_ms=%d idle_timeout_ms=%d reason=%q",
			d.workspace, d.sessionID, fromSeq, stopAt, res.Delivered, progress.delivered, progress.firstSeq, progress.lastSeq,
			progress.elapsed.Milliseconds(), repullIdleTimeout.Milliseconds(), res.Reason)
		return fmt.Errorf("%w: ws=%q from_seq=%d stop_at=%d delivered %d event(s): %s",
			ErrRepullTruncated, d.workspace, fromSeq, stopAt, res.Delivered, res.Reason)
	}
	m.logf("session-controller: re-pull complete ws=%q session=%s delivered=%d observed=%d first_seq=%d last_seq=%d from_seq=%d stop_at=%d elapsed_ms=%d idle_timeout_ms=%d",
		d.workspace, d.sessionID, res.Delivered, progress.delivered, progress.firstSeq, progress.lastSeq,
		fromSeq, stopAt, progress.elapsed.Milliseconds(), repullIdleTimeout.Milliseconds())
	return nil
}

// repullConversation is the ONLY sink a replayed historical event reaches.
//
// It deliberately does NOT call retain, ssm.Apply, applyProgress,
// observeBackfill, or PushTaskCatalog. Every one of those planes already
// consumed this event when it was live; re-applying it would double-count
// tasks, re-open closed turns, and re-drive the footer from history. The event
// is translated to a ConversationDelta and pushed, exactly as consumer.resync
// does for the retained ring, and that is all.
//
// The guarantee no longer rests on this function alone: a replayed event
// arrives as a `ReplayEvent`, so shimclient's read loop has nowhere else to put
// it (see replay.go). This is where the content is rendered, not where the
// separation is enforced.
//
// The ring is not extended either: the ring is the LIVE window, and back-filling
// it with replayed history would make the floor drift under the next request.
// Every replayed event is offered to the curator, which decides what carries
// conversation content — including the first-class clear and compaction, which
// are not vendor payloads and were skipped outright while this filtered on one.
// A clear or a compaction re-pulled from history does NOT re-raise the replay
// floor: the floor is durable, a re-pull is bounded ABOVE by it, and letting
// history move it would be the replayed-history-as-live-state class of bug
// repull exists to avoid.
func (c *consumer) repullConversation(ev *corev1.Event) {
	c.pushConversation(ev, false)
}

// compile-time proof that the real client satisfies the session controller's replay need.
var _ interface {
	Replay(context.Context, uint64, uint64, uint32, func(*corev1.Event)) (shimclient.ReplayResult, error)
} = (*shimclient.Client)(nil)
