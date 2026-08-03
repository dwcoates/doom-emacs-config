package sessioncontroller

import (
	"context"
	"fmt"
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/dlog"
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

// repullWaitGrace bounds how long a WAITING request keeps waiting after the
// in-flight re-pull's own activity deadline has already fired.
//
// The wait itself is NOT bounded by a fixed wall-clock budget, and deliberately
// so: a healthy re-pull of a large history legitimately runs far longer than
// any single idle window, and cutting a waiter off while the pull it waits on
// is actively streaming would recreate the under-serve this whole path exists
// to remove. What bounds the wait is the in-flight pull's OWN discipline — its
// repullActivity cancels on one idle window without progress, which is finite
// by construction — plus this grace for the pull to unwind and release the
// workspace once that deadline has tripped. A pull that ignores its own
// cancellation past the grace is a wedge, and the waiter says so loudly rather
// than waiting on it forever.
const repullWaitGrace = repullIdleTimeout

// repullState is one workspace's in-flight re-pull, guarded by m.mu.
type repullState struct {
	fromSeq uint64
	stopAt  uint64
	// epoch is sessionController.rotEpoch as it stood when this re-pull started — the
	// generation of the seq space its bounds are expressed in. Comparing bounds
	// across generations is comparing numbers from different spaces, so the
	// coalescing rule refuses rather than pretends.
	epoch uint64
	// done is closed once this re-pull has finished AND released the workspace,
	// so a serialized request can start the moment the slot is free. Closed
	// exactly once, by releaseRepull.
	done chan struct{}
	// activity is this re-pull's progress-rearmed deadline. A waiter watches its
	// context so that "the pull I am queued behind is still making progress" and
	// "the pull I am queued behind has given up" are distinguishable without
	// polling anything.
	activity *repullActivity
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
// Why a below-floor re-pull exists, and which route serves it
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
// # The gap is split at the durable high-water mark
//
// The range a re-pull covers is history that has, by construction, already
// been INGESTED: it lies below the ring floor, and the ring floor is at most
// one past the durable last_seen_seq. So the store — the same store the shim
// itself reads — normally holds every row of it, and the daemon already knows
// how far the store's seq space reaches for this session (SeqStore.LastSeq, the
// mark settleBackfillFromStore settles the backfill from).
//
// The gap is therefore SPLIT at that mark:
//
//   - (from_seq, min(stop_at, high_water+1)) is served from DURABLE history
//     directly (storehistory.Reader), with no shim in the loop.
//   - [high_water+1, stop_at), if it is non-empty, is history the store cannot
//     be shown to hold, so it is asked for over the SHIM connection
//     (core.proto ReplayRequest) exactly as the whole range used to be. When
//     no shim can serve it, the failure is the loud one it always was, and it
//     names the remainder that went unserved.
//
// WHY THE STORE HALF IS NOT A FALLBACK. An earlier version of this dialled the
// store while the shim was DOWN, as a second route to the range the shim was
// supposed to serve, and that was correctly deleted: it masked an outage and
// traded a visible failure for a quietly half-working display. This is not
// that. The split is not "try the shim, then try the store"; it is a
// PARTITION, decided before either route is used, by a fact the daemon holds
// about the store's own coverage. Nothing about a shim's health enters the
// decision, no failure of one route is answered by the other, and the range
// the store cannot be shown to cover is never quietly excused — it is still
// the shim's to serve or to fail on.
//
// WHY THE FACADE SURVIVES IT. The store's socket location, its connection
// lifecycle, and the vendor-uuid keying rule stay in storehistory (see that
// package's header), reached through the DurableHistorySource interface, so
// none of it becomes session-controller knowledge. With no source wired the
// whole range goes to the shim, and the log says so.
//
// Four standing constraints hold on either route:
//
//  1. FRONTEND-INITIATED. Nothing in the daemon starts one. It exists only as
//     the tail of a ResyncCmd a frontend sent.
//  2. BOUNDED. to_seq is the ring floor (the first seq the live window already
//     covers); max_events caps one replay; each route adds an idle window;
//     this side adds a progress-rearmed idle deadline. A tripped bound comes
//     back TRUNCATED and becomes ErrRepullTruncated here, from either route.
//  3. SIDE CHANNEL. The shim opens a throwaway store subscription and leaves
//     its standing one alone; the durable read opens its own throwaway
//     subscription. Either way the daemon's own consumption position never
//     moves and no SeqStore write happens.
//  4. CONVERSATION ONLY. Replayed events reach repullConversation and nothing
//     else. Over the shim this is STRUCTURAL — they arrive as `ReplayEvent`, a
//     different wire type from live `Event`s, so shimclient's read loop cannot
//     route them into the SSM, the task catalog, or the progress resolver even
//     by mistake. The durable read has no read loop to be routed by: it hands
//     each row to the callback given here, and that callback is
//     repullConversation. Those planes consumed this history once already;
//     applying it again is what makes historical tasks masquerade as live
//     activity.
//
// A daemon-standing version of any of this would be the replay storm the
// high-water subscribe exists to prevent. This is not that, and must not become
// it.

// startRepull runs a below-floor history re-pull for d, delivering replayed
// events to CONVERSATION TRANSLATION ONLY (constraint 4).
//
// The gap is split at the durable high-water mark: the covered prefix is read
// from the store with no shim in the loop, and only the remainder above it is
// asked of the shim (see the section above).
//
// Concurrency: at most one re-pull per workspace AT A TIME, and every request
// is served.
//
//   - A request whose range is already COVERED by the in-flight one is coalesced
//     onto it — the pull's output is broadcast to every subscriber of the
//     workspace, so the second caller is genuinely served by the first pull
//     rather than being told "yes" while nothing happens.
//   - A request reaching FURTHER BACK than the in-flight one is not covered, so
//     it WAITS for that pull to finish and then runs itself, over its own full
//     requested range.
//
// WHY THE WAIT REPLACED A REFUSAL. This used to answer an uncovered concurrent
// request with ErrRepullInFlight, on the implicit contract that the caller would
// retry. No caller ever implemented that retry, so the refusal was a silent
// under-serve with extra steps, and it was observed costing a freshly-loaded
// client its entire history: an incremental resync (from_seq=4905) was in flight
// when a fresh page asked from the floor, the fresh page was nacked, never
// re-asked, and its feed stayed permanently missing seqs 4586-4904.
//
// WHY THE FULL RANGE, NOT THE UNCOVERED REMAINDER. The suffix the in-flight pull
// already delivered is re-delivered by this one. That is the SAME double
// delivery the coalescing rule above has always produced (a caller asking from
// 5 while a pull from 0 runs receives 0..5 too), and the frontend keys every
// conversation item on its record uuid, which is stable across a replay, so a
// re-delivery replaces the item rather than duplicating it. Serving the whole
// requested range keeps this path a single fact — "the range you asked for was
// pulled" — instead of a fact that depends on what another client happened to be
// doing at the time.
//
// Runs synchronously: it is the tail of a ResyncCmd, and the CommandAck should
// report what actually happened rather than acknowledging an intent.
func (m *Manager) startRepull(d *sessionController, fromSeq, stopAt uint64) error {
	state, err := m.claimRepull(d, fromSeq, stopAt)
	if err != nil {
		return err
	}
	if state == nil {
		return nil // coalesced onto an in-flight pull that covers this range
	}
	activity := state.activity
	defer m.releaseRepull(d, state)
	defer activity.stop()

	storeFrom, storeStop, shimFrom, shimStop := m.splitRepullRange(d, fromSeq, stopAt)
	if storeStop > storeFrom {
		if err := m.repullFromStore(d, activity, storeFrom, storeStop, fromSeq, stopAt); err != nil {
			return err
		}
	}
	if shimStop <= shimFrom {
		return nil
	}
	return m.repullFromShim(d, activity, shimFrom, shimStop)
}

// claimRepull acquires the workspace's single re-pull slot for (fromSeq,
// stopAt), waiting out any in-flight pull that does not cover the request.
//
// It returns exactly one of three answers, and the caller distinguishes them by
// shape: a non-nil state means the slot is HELD and the pull must run (and must
// be released); (nil, nil) means the request is COALESCED onto an in-flight pull
// that covers it and nothing further is owed; a non-nil error means the request
// could not be served and the ack must say so.
//
// The epoch read on ENTRY is the generation the caller's bounds were computed
// in (Resync derived them from the ring/high-water immediately before calling).
// It is re-checked after every wait, because a rotation retires the seq space
// those bounds count in — serving them afterwards is exactly the ceiling-from-a-
// retired-space defect purgeRetainedOnRotation exists to close.
func (m *Manager) claimRepull(d *sessionController, fromSeq, stopAt uint64) (*repullState, error) {
	m.mu.Lock()
	reqEpoch := d.rotEpoch
	waited := false
	for {
		cur := d.repull
		if cur == nil {
			state := &repullState{
				fromSeq:  fromSeq,
				stopAt:   stopAt,
				epoch:    reqEpoch,
				done:     make(chan struct{}),
				activity: newRepullActivity(m.rootCtx, repullIdleTimeout),
			}
			d.repull = state
			m.mu.Unlock()
			if waited {
				m.logf("session-controller: the in-flight re-pull ws=%q finished; now serving the request that waited for it from_seq=%d stop_at=%d epoch=%d",
					d.workspace, fromSeq, stopAt, reqEpoch)
			}
			return state, nil
		}
		// A re-pull from a RETIRED seq space covers nothing in this one, however
		// the numbers happen to compare. Coalescing across a rotation would hand
		// this caller an in-flight pull of a conversation the vendor has retired
		// and call it served, so it is treated as uncovered and waited out.
		sameSpace := cur.epoch == reqEpoch
		covered := sameSpace && cur.fromSeq <= fromSeq
		curFrom, curStop, curEpoch := cur.fromSeq, cur.stopAt, cur.epoch
		done, activity := cur.done, cur.activity
		m.mu.Unlock()
		if covered {
			m.logf("session-controller: coalescing re-pull ws=%q from_seq=%d onto the in-flight one (from_seq=%d stop_at=%d)",
				d.workspace, fromSeq, curFrom, curStop)
			return nil, nil
		}
		m.logf("session-controller: the in-flight re-pull ws=%q (from_seq=%d stop_at=%d epoch=%d) does NOT cover the requested from_seq=%d (epoch %d, same_seq_space=%v) — WAITING for the in-flight re-pull, then serving from_seq=%d itself rather than refusing the request",
			d.workspace, curFrom, curStop, curEpoch, fromSeq, reqEpoch, sameSpace, fromSeq)
		if err := m.awaitRepull(d, curFrom, curStop, fromSeq, done, activity); err != nil {
			return nil, err
		}
		waited = true
		m.mu.Lock()
		if d.rotEpoch != reqEpoch {
			nowEpoch := d.rotEpoch
			m.mu.Unlock()
			m.logf("session-controller: re-pull request ws=%q from_seq=%d stop_at=%d was computed in a seq space the vendor RETIRED while it waited (epoch %d -> %d); it is refused rather than served against the new space, and the rotation itself re-drives the frontend's resync",
				d.workspace, fromSeq, stopAt, reqEpoch, nowEpoch)
			return nil, fmt.Errorf("%w: ws=%q the request (from_seq=%d stop_at=%d) waited for an in-flight re-pull and the vendor session ROTATED meanwhile (epoch %d, now %d), so its bounds count in a RETIRED seq space and must not be served",
				ErrRepullInFlight, d.workspace, fromSeq, stopAt, reqEpoch, nowEpoch)
		}
	}
}

// awaitRepull blocks until the in-flight re-pull releases the workspace, or
// fails loudly if waiting any longer would be waiting on a wedge.
//
// There is no polling and no sleep: the three things that can end the wait are
// all channels — the in-flight pull's release, the daemon's own shutdown, and
// the in-flight pull's progress-rearmed deadline. Only once that deadline has
// tripped does a fixed grace apply, because only then is the pull known to owe
// an unwind rather than to be doing work (see repullWaitGrace).
func (m *Manager) awaitRepull(d *sessionController, curFrom, curStop, fromSeq uint64, done <-chan struct{}, activity *repullActivity) error {
	select {
	case <-done:
		return nil
	case <-m.rootCtx.Done():
		return m.repullWaitAbandoned(d, curFrom, curStop, fromSeq)
	case <-activity.ctx.Done():
	}
	grace := m.repullGrace()
	m.logf("session-controller: the in-flight re-pull ws=%q (from_seq=%d stop_at=%d) hit its OWN deadline (cause=%v) while a request from_seq=%d waited behind it; allowing %s for it to unwind and release the workspace",
		d.workspace, curFrom, curStop, context.Cause(activity.ctx), fromSeq, grace)
	timer := time.NewTimer(grace)
	defer timer.Stop()
	select {
	case <-done:
		return nil
	case <-m.rootCtx.Done():
		return m.repullWaitAbandoned(d, curFrom, curStop, fromSeq)
	case <-timer.C:
		m.logf("session-controller: re-pull WAIT TIMED OUT ws=%q: the in-flight re-pull (from_seq=%d stop_at=%d) did not release the workspace within %s of its own deadline, so the request from_seq=%d is NOT served",
			d.workspace, curFrom, curStop, grace, fromSeq)
		return fmt.Errorf("%w: ws=%q waited for the in-flight re-pull (from_seq=%d stop_at=%d), whose own deadline had already tripped, and it did not release the workspace within %s; the requested from_seq=%d went unserved",
			ErrRepullInFlight, d.workspace, curFrom, curStop, grace, fromSeq)
	}
}

// repullWaitAbandoned is the shutdown outcome of a wait, reported as the failure
// it is: the daemon is going away, so nobody is going to serve this range.
func (m *Manager) repullWaitAbandoned(d *sessionController, curFrom, curStop, fromSeq uint64) error {
	m.logf("session-controller: re-pull wait ABANDONED ws=%q: the daemon is shutting down while a request from_seq=%d waited for the in-flight re-pull (from_seq=%d stop_at=%d)",
		d.workspace, fromSeq, curFrom, curStop)
	return fmt.Errorf("%w: ws=%q the daemon shut down while the request from_seq=%d waited for the in-flight re-pull (from_seq=%d stop_at=%d)",
		ErrRepullInFlight, d.workspace, fromSeq, curFrom, curStop)
}

// repullGrace is repullWaitGrace unless a test shortened it, mirroring
// interruptDrain: the timeout branch is reachable without a minute-long wait.
func (m *Manager) repullGrace() time.Duration {
	if m.repullWaitGraceOverride > 0 {
		return m.repullWaitGraceOverride
	}
	return repullWaitGrace
}

// releaseRepull frees the workspace's re-pull slot and wakes everything queued
// behind it. Closing done is what makes the wait a handoff rather than a poll.
func (m *Manager) releaseRepull(d *sessionController, state *repullState) {
	m.mu.Lock()
	holder := d.repull
	if holder == state {
		d.repull = nil
	}
	m.mu.Unlock()
	if holder != state {
		// Unreachable by the single-owner discipline above; reported rather than
		// silently corrected, because clearing another pull's slot would strand it.
		m.logf("session-controller: INVARIANT VIOLATION ws=%q: the finishing re-pull (from_seq=%d stop_at=%d) does not hold the workspace's re-pull slot; the slot is left alone",
			d.workspace, state.fromSeq, state.stopAt)
	}
	close(state.done)
}

// splitRepullRange partitions the requested exclusive range (fromSeq, stopAt)
// into the prefix the DURABLE store is known to cover and the remainder that is
// the shim's to serve.
//
// The mark is SeqStore.LastSeq — the highest store seq this daemon has durably
// observed for the session, and the same evidence settleBackfillFromStore
// treats as proof the store holds the history. Everything at or below it is the
// store's; everything above it is not shown to be, so it stays the shim's.
//
// Both returned ranges are exclusive at both ends, and either may be empty
// (stop <= from), which is how "no store portion" and "no shim remainder" are
// expressed. With no durable source wired the whole range is the shim's, which
// is exactly the behavior that predates the split.
func (m *Manager) splitRepullRange(d *sessionController, fromSeq, stopAt uint64) (storeFrom, storeStop, shimFrom, shimStop uint64) {
	logf := dlog.Tag(dlog.Logf(m.logf), "ws", d.workspace, "session", d.sessionID,
		"from_seq", fromSeq, "stop_at", stopAt)
	if m.cfg.DurableHistory == nil {
		logf("session-controller: re-pull gap ws=%q session=%s from_seq=%d stop_at=%d has NO durable history source wired, so the whole range is asked of the SHIM (nothing is being read from the store)",
			d.workspace, d.sessionID, fromSeq, stopAt)
		return fromSeq, fromSeq, fromSeq, stopAt
	}
	highWater := m.cfg.SeqStore.LastSeq(d.sessionID)
	storeFrom, storeStop = fromSeq, stopAt
	if highWater+1 < storeStop {
		storeStop = highWater + 1
	}
	// (from, stop) is exclusive at both ends, so it holds a seq only when
	// stop > from+1. Anything narrower is an EMPTY segment, reported as such
	// rather than dispatched as a replay that could never deliver a row.
	if storeStop <= storeFrom+1 {
		logf("session-controller: re-pull gap ws=%q session=%s from_seq=%d stop_at=%d is entirely ABOVE the store high-water %d, so the whole range is asked of the SHIM",
			d.workspace, d.sessionID, fromSeq, stopAt, highWater)
		return fromSeq, fromSeq, fromSeq, stopAt
	}
	shimFrom, shimStop = storeStop-1, stopAt
	if shimStop <= shimFrom+1 {
		logf("session-controller: re-pull gap ws=%q session=%s from_seq=%d stop_at=%d is fully covered by the store high-water %d, so it is served from DURABLE history with no shim in the loop",
			d.workspace, d.sessionID, fromSeq, stopAt, highWater)
		return storeFrom, storeStop, shimFrom, shimFrom
	}
	logf("session-controller: re-pull gap ws=%q session=%s from_seq=%d stop_at=%d SPLIT at the store high-water %d: store serves (%d,%d), the shim serves the remainder (%d,%d)",
		d.workspace, d.sessionID, fromSeq, stopAt, highWater, storeFrom, storeStop, shimFrom, shimStop)
	return storeFrom, storeStop, shimFrom, shimStop
}

// repullFromStore serves the store-covered prefix of one gap straight from
// DURABLE history, delivering to conversation translation only.
//
// A store read that FAILS fails the whole re-pull. It is never answered by
// re-asking the shim for the same range: that would be the fallback this path
// is explicitly not, and it would hide a broken store behind a route that
// happens to be up. The log names the store as the source that failed.
func (m *Manager) repullFromStore(d *sessionController, activity *repullActivity, fromSeq, stopAt, reqFrom, reqStop uint64) error {
	logf := dlog.Tag(dlog.Logf(m.logf), "ws", d.workspace, "session", d.sessionID, "source", "shim-store")
	logf("session-controller: re-pulling history ws=%q session=%s from_seq=%d stop_at=%d max_events=%d idle_timeout_ms=%d (frontend-initiated, from DURABLE history, conversation only; requested gap was (%d,%d))",
		d.workspace, d.sessionID, fromSeq, stopAt, repullMaxEvents, repullIdleTimeout.Milliseconds(), reqFrom, reqStop)
	res, err := m.cfg.DurableHistory.ReplayHistory(activity.ctx, d.workspace, d.sessionID, fromSeq, stopAt, repullMaxEvents,
		func(ev *corev1.Event) {
			progress := activity.observe(ev.GetSeq())
			if progress.delivered == 1 || progress.delivered%repullProgressLogEvery == 0 {
				logf("session-controller: re-pull progress ws=%q session=%s source=store from_seq=%d stop_at=%d delivered=%d first_seq=%d last_seq=%d elapsed_ms=%d idle_timeout_ms=%d",
					d.workspace, d.sessionID, fromSeq, stopAt, progress.delivered, progress.firstSeq, progress.lastSeq,
					progress.elapsed.Milliseconds(), repullIdleTimeout.Milliseconds())
			}
			d.consumer.repullConversation(ev)
		})
	if err != nil {
		logf("session-controller: history re-pull FAILED ws=%q session=%s source=store from_seq=%d stop_at=%d delivered=%d: %v — the store is the source that failed, and the shim is NOT asked for this range instead",
			d.workspace, d.sessionID, fromSeq, stopAt, res.Delivered, err)
		return fmt.Errorf("session-controller: history re-pull for ws %q (from_seq=%d stop_at=%d) from durable history failed: %w",
			d.workspace, fromSeq, stopAt, err)
	}
	if res.Truncated {
		logf("session-controller: history re-pull TRUNCATED ws=%q session=%s source=store from_seq=%d stop_at=%d delivered=%d first_seq=%d last_seq=%d reason=%q",
			d.workspace, d.sessionID, fromSeq, stopAt, res.Delivered, res.FirstSeq, res.LastSeq, res.Reason)
		return fmt.Errorf("%w: ws=%q from_seq=%d stop_at=%d delivered %d event(s) from durable history: %s",
			ErrRepullTruncated, d.workspace, fromSeq, stopAt, res.Delivered, res.Reason)
	}
	logf("session-controller: re-pull segment COMPLETE ws=%q session=%s source=store from_seq=%d stop_at=%d delivered=%d first_seq=%d last_seq=%d",
		d.workspace, d.sessionID, fromSeq, stopAt, res.Delivered, res.FirstSeq, res.LastSeq)
	return nil
}

// repullFromShim serves the remainder of one gap — the part ABOVE the store's
// high-water mark, or the whole gap when no durable source is wired — over the
// session's shim connection.
func (m *Manager) repullFromShim(d *sessionController, activity *repullActivity, fromSeq, stopAt uint64) error {
	m.logf("session-controller: re-pulling history ws=%q session=%s from_seq=%d stop_at=%d max_events=%d idle_timeout_ms=%d (frontend-initiated, via the shim, conversation only)",
		d.workspace, d.sessionID, fromSeq, stopAt, repullMaxEvents, repullIdleTimeout.Milliseconds())
	res, err := d.client.Replay(activity.ctx, fromSeq, stopAt, repullMaxEvents, func(ev *corev1.Event) {
		progress := activity.observe(ev.GetSeq())
		if progress.delivered == 1 || progress.delivered%repullProgressLogEvery == 0 {
			m.logf("session-controller: re-pull progress ws=%q session=%s source=shim from_seq=%d stop_at=%d delivered=%d first_seq=%d last_seq=%d elapsed_ms=%d idle_timeout_ms=%d",
				d.workspace, d.sessionID, fromSeq, stopAt, progress.delivered, progress.firstSeq, progress.lastSeq,
				progress.elapsed.Milliseconds(), repullIdleTimeout.Milliseconds())
		}
		d.consumer.repullConversation(ev)
	})
	progress := activity.stop()
	if err != nil {
		m.logf("session-controller: history re-pull FAILED ws=%q session=%s source=shim from_seq=%d stop_at=%d delivered=%d first_seq=%d last_seq=%d elapsed_ms=%d idle_timeout_ms=%d cause=%q — this is the range the store was not shown to cover, and it is left unserved",
			d.workspace, d.sessionID, fromSeq, stopAt, progress.delivered, progress.firstSeq, progress.lastSeq,
			progress.elapsed.Milliseconds(), repullIdleTimeout.Milliseconds(), err)
		return fmt.Errorf("session-controller: history re-pull for ws %q (from_seq=%d stop_at=%d) failed: %w",
			d.workspace, fromSeq, stopAt, err)
	}
	if res.Truncated {
		// A partial answer is reported as one. The frontend rendered whatever
		// did arrive; the ack tells it the rest is still missing.
		m.logf("session-controller: history re-pull TRUNCATED ws=%q session=%s source=shim from_seq=%d stop_at=%d delivered=%d observed=%d first_seq=%d last_seq=%d elapsed_ms=%d idle_timeout_ms=%d reason=%q",
			d.workspace, d.sessionID, fromSeq, stopAt, res.Delivered, progress.delivered, progress.firstSeq, progress.lastSeq,
			progress.elapsed.Milliseconds(), repullIdleTimeout.Milliseconds(), res.Reason)
		return fmt.Errorf("%w: ws=%q from_seq=%d stop_at=%d delivered %d event(s): %s",
			ErrRepullTruncated, d.workspace, fromSeq, stopAt, res.Delivered, res.Reason)
	}
	// observed counts EVERY event this re-pull delivered, including a store-served
	// prefix, because the activity deadline spans the whole re-pull.
	m.logf("session-controller: re-pull complete ws=%q session=%s source=shim delivered=%d observed=%d first_seq=%d last_seq=%d from_seq=%d stop_at=%d elapsed_ms=%d idle_timeout_ms=%d",
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
