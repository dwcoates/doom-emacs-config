// link.go is the sidecar's store-connection state machine.
//
// THE RULE: the sidecar reads a watched file only while a store connection is
// established, and the FIRST act of every established connection — boot and
// reconnect alike, with no distinction between them — is recovering the cursors
// and authoritative open-task set that store holds. Boot is not a case; boot is
// simply the first time the link is not up yet.
//
// WHAT THIS REPLACES. Cursor recovery used to run exactly once, at process
// boot, and its failure path was a silent cold start: if the store's socket was
// not listening yet, the sidecar logged "cursor recovery failed (starting
// cold)" and then re-read every watched file from offset 0. That is a fallback
// masking a down dependency, and it caused a real incident — a boot race
// re-ingested whole conversations and drove an SSM task-count clamp storm. The
// honest behavior while the store is unreachable is producing NOTHING, loudly.
//
// WHY THIS IS STRUCTURAL RATHER THAN A RETRY AROUND A SPECIAL CASE. There is no
// boot path left to get wrong. A tailer's read position can only ever come from
// a cursor the store handed us, because:
//
//   - `cursors` is nil unless a connection recovered it, and only `establish`
//     ever sets it;
//   - `rescan` is the only thing that builds a tailer, and it asserts (hard,
//     per the metaprompt's invariant rule) that `cursors` was recovered;
//   - after construction a tailer advances only through Commit, which the poll
//     loop calls only on a durable store ack;
//   - every ingestion step runs through `whenUp`, so nothing polls, sweeps, or
//     heartbeats while the link is down.
//
// Boot ordering is therefore irrelevant by construction: a store that starts
// late simply means the link is not up yet, which is the same state a store
// that dies mid-run puts the sidecar into, handled by the same code.
//
// "Cold" survives only as its truthful case — a CONNECTED store that genuinely
// holds no cursor for a file, which is the newly-discovered-transcript backfill
// (see backfill_test.go) and reads from offset 0 exactly as it should.
package main

import (
	"fmt"
	"math/rand"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/handler"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// linkState is the sidecar's store-connection state. There are exactly two, and
// every file read happens in the second one.
type linkState int

const (
	// linkDown: no established store connection. The sidecar reads nothing and
	// its only activity is dialing.
	linkDown linkState = iota
	// linkUp: a connection is established AND its cursors have been recovered.
	// Those two facts are never separable — `establish` sets this state only
	// after both hold.
	linkUp
)

// Redial backoff: an immediate first attempt, then 100ms doubling to a ceiling
// the ladder holds FOREVER. There is no attempt budget and no terminal state —
// a link that is down is a link that is being redialed, for as long as it stays
// down.
const (
	dialBackoffMin = 100 * time.Millisecond
	dialBackoffMax = 30 * time.Second
)

// dialJitterFraction spreads each armed delay over ±20% of its backoff, so a
// fleet of sidecars that lost the same store does not rediscover it in lockstep
// bursts.
const dialJitterFraction = 0.2

// dialTick is how often Run asks the state machine whether a redial is due.
//
// THE LADDER IS A DEADLINE THE LOOP COMPARES AGAINST, NOT AN EVENT SOMEONE MUST
// REMEMBER TO SCHEDULE. It used to be a single re-armable time.Timer, and that
// made the whole steady-state ladder depend on one Stop/drain/Reset dance
// executing correctly on every transition: a single lost re-arm silenced
// redialing entirely, with no state left in the process saying a redial was
// owed. That is exactly what a production store bounce produced — one failed
// dial, then 15 minutes of a down link that healed only when unrelated work
// happened to dial again. A deadline cannot be lost: the ticker that checks it
// runs unconditionally for the life of the process, so the worst a mishandled
// transition can cost is one tick of latency rather than the ladder itself.
const dialTick = 50 * time.Millisecond

// degradedComponent names this state machine in DegradedState reports.
const degradedComponent = "shim-claude-sidecar-store-link"

// whenUp runs one unit of ingestion work, and only while the store link is up.
// EVERY periodic action in the sidecar goes through here, so "reads while the
// store is down" is one decision in one place rather than a condition each
// caller has to remember.
func (s *sidecar) whenUp(work func()) {
	if s.link == linkUp {
		work()
	}
}

// requireLinkUp asserts the invariant every ingestion step depends on: no file
// is read, and no tailer is built, without an established connection whose
// cursors are already in hand. A violation is a bug in this state machine, so
// it fails hard instead of quietly cold-starting.
func (s *sidecar) requireLinkUp(what string) {
	if s.link != linkUp || s.cursors == nil {
		panic(fmt.Sprintf("sidecar: %s with the store link down — a tailer's position may only come from a cursor recovered on the live connection", what))
	}
}

// dialDue runs the LADDER: it dials whenever the link is down and the armed
// deadline has passed, and does nothing otherwise. Run calls it on every tick,
// so redialing is driven by the link's state rather than by anything
// remembering to schedule it.
func (s *sidecar) dialDue() {
	if s.link == linkUp {
		return
	}
	if s.now().Before(s.nextDialAt) {
		return
	}
	s.dial()
}

// dial attempts to bring the link up, backing off between attempts. It is the
// only thing a down sidecar does, and a no-op once the link is up.
//
// It is SINGLE-FLIGHT: a demand-driven dial and the ladder's own dial are the
// same call, and the second one to arrive while the first is still inside
// establish returns immediately rather than opening a second socket and racing
// two cursor recoveries onto one link.
func (s *sidecar) dial() {
	if s.link == linkUp {
		return
	}
	if s.dialing {
		return
	}
	s.dialing = true
	defer func() { s.dialing = false }()
	if err := s.establish(); err != nil {
		s.dialFailures++
		s.backoff = nextBackoff(s.backoff)
		// "Reading no files" is the whole file plane stopped: for the length
		// of this backoff nothing on disk reaches the store, so the record
		// belongs at the severity an ingestion outage carries.
		delay := s.jitter(s.backoff)
		s.log.With(logging.Context{Operation: "dial", Level: "warn"}).Log("dial attempt %d failed, retrying in %s while reading no files: %v", s.dialFailures, delay, err)
		s.armDial(delay)
		return
	}
}

// establish runs the first act of a store connection: dial, recover cursors and
// authoritative open tasks, and only then start reading. Any failure leaves the
// link down with nothing read, which is the whole point — a half-established
// link that could write but had no recovery state is exactly the cold start this
// design removes.
func (s *sidecar) establish() error {
	if err := s.store.Connect(); err != nil {
		return err
	}
	recovery, err := s.store.Recover("")
	if err != nil {
		// Drop the socket: a producer connection with no recovered cursors must
		// never survive, or a later write would ride a link that skipped
		// recovery.
		s.store.Close()
		return fmt.Errorf("recovering startup state: %w", err)
	}
	if err := s.tracker.Restore(recovery.OpenTasks); err != nil {
		s.store.Close()
		return fmt.Errorf("restoring authoritative open tasks: %w", err)
	}
	s.cursors = indexCursorsByPath(recovery.Cursors)
	// Seed the spool-owner index from the SAME authoritative snapshot. A spool
	// carries no session of its own, and the launch line that names its owner
	// may sit far behind this connection's resumed cursor — so without this
	// seed a restart could leave a live task's spool unowned, and therefore
	// unread, until its transcript happened to be re-read. The persisted
	// TaskStarted events are exactly that mapping, already fetched.
	s.resetOwners()
	seeded := s.seedOwners(recovery.OpenTasks)
	s.link = linkUp
	s.backoff = 0
	s.log.With(logging.Context{Operation: "recover-startup-state"}).Log(
		"store link up, recovered %d cursor(s), %d authoritative open task(s), seeded %d spool owner(s)",
		len(s.cursors), len(recovery.OpenTasks), seeded)

	// Reading may begin now, and not one statement earlier.
	s.rescan()
	// The machine boot sweep is about MACHINE boot, not about this connection,
	// so it runs once per process — but it emits LOST events, so it can only run
	// once there is a store to emit them to. It used to run at process boot,
	// where its writes were dropped whenever the store had not started yet.
	if !s.bootSwept {
		s.bootSweep()
		s.bootSwept = true
	}
	s.reportOutageClosed()
	return nil
}

// linkLost tears the link down and schedules an immediate redial. It is
// idempotent, because a single poll pass can surface the same dead connection
// through several failed writes.
func (s *sidecar) linkLost(operation string) {
	if s.link == linkDown {
		return
	}
	s.link = linkDown
	s.cursors = nil
	s.store.Close()
	s.downSince = s.now()
	s.dialFailures = 0
	s.backoff = 0
	// The caller owns the causal error with the session or request context it
	// alone knows. This record owns only the link-state transition so the same
	// error is not copied into both narratives.
	// The record that OPENS the degradation window: every tail stops here and
	// the file plane produces nothing until the link returns.
	s.log.With(logging.Context{Operation: "link-lost", Level: "warn"}).Log("store link lost after operation=%s; reading no files until it returns", operation)
	s.armDial(0)
}

// noteStoreErr folds a transport failure into the state machine. A store
// REJECTION arrives on a healthy connection and is NOT a link loss, so the
// connection's own liveness decides rather than the presence of an error.
func (s *sidecar) noteStoreErr(what string, err error) {
	if err == nil || s.store.Connected() {
		return
	}
	s.linkLost(what)
}

// reportOutageClosed surfaces the window the sidecar just spent unable to
// ingest, as the closing report of a DegradedState window (`recovered` is
// documented as exactly that).
//
// Only the CLOSING report is sendable, and that is honest rather than a
// shortcut: the store is the sidecar's only channel, so while the link is down
// there is by definition nobody to tell. The outage is loud in the log
// throughout, and becomes an event the moment there is a store to carry it.
//
// A link that came up on its first attempt spent no time down and reports
// nothing.
func (s *sidecar) reportOutageClosed() {
	if s.dialFailures == 0 {
		return
	}
	downMs := s.now().Sub(s.downSince).Milliseconds()
	reason := fmt.Sprintf("store unreachable for %dms across %d failed dial attempt(s); no files were read during the outage",
		downMs, s.dialFailures)
	s.log.With(logging.Context{Operation: "link-recovered"}).Log("store link recovered: %s", reason)
	s.emit(degradedWindowEvents(s.watchedSessions(), reason))
	s.dialFailures = 0
}

// watchedSessions lists the distinct session ids the sidecar currently watches,
// which are the channels a degraded report can actually reach: the store fans
// an event out to that session's subscribers and nobody else.
func (s *sidecar) watchedSessions() []string {
	seen := map[string]bool{}
	var out []string
	for _, w := range s.watchers {
		id := w.sessionID
		if id == "" || seen[id] {
			continue
		}
		seen[id] = true
		out = append(out, id)
	}
	return out
}

// degradedWindowEvents reports the outage as BOTH HALVES OF ONE WINDOW, opening
// then closing, per session.
//
// The consumer's runtime-fault plumbing is a window: a DegradedState with
// recovered=false OPENS a fault for the component, and one with recovered=true
// CLOSES it. Sending only the closing half — which is all the sidecar could
// ever send live, since the store it reports through is the very thing that was
// down — asked the daemon to close a window that was never opened, and it
// rejected the close (`branch=not-open`) instead of recording anything. The
// outage was then invisible to workspace health even though the failure card
// showed up.
//
// Both halves are sent on the recovered connection, in order, which is the
// honest shape of a window that is already over by the time it can be told.
func degradedWindowEvents(sessions []string, reason string) []*corev1.Event {
	out := make([]*corev1.Event, 0, 2*len(sessions))
	for _, id := range sessions {
		out = append(out, degradedEvents([]string{id}, reason, false)...)
		out = append(out, degradedEvents([]string{id}, reason, true)...)
	}
	return out
}

// degradedEvents builds one DegradedState per session, opening the window
// (recovered=false) or closing it (recovered=true). They are
// SYNTHETIC/EPHEMERAL, matching how the shim reports its own degraded windows:
// an operational notice about the pipe belongs in the live stream, never in the
// durable conversation history the pipe carries.
func degradedEvents(sessions []string, reason string, recovered bool) []*corev1.Event {
	now := time.Now().UnixMilli()
	out := make([]*corev1.Event, 0, len(sessions))
	for _, id := range sessions {
		out = append(out, &corev1.Event{
			SessionId:    id,
			Plane:        corev1.Plane_PLANE_SYNTHETIC,
			Class:        corev1.EventClass_EVENT_CLASS_EPHEMERAL,
			ProducedAtMs: now,
			Payload: &corev1.Event_DegradedState{DegradedState: &corev1.DegradedState{
				Component: degradedComponent,
				Reason:    reason,
				Recovered: recovered,
			}},
		})
	}
	return out
}

// armDial records that the next dial attempt is due after d. It only moves a
// deadline, so it has no way to fail silently and nothing has to be undone if
// the link state changes before the deadline arrives — dialDue re-reads the
// link on every tick.
func (s *sidecar) armDial(d time.Duration) {
	s.nextDialAt = s.now().Add(d)
}

// jitterBackoff spreads d over ±dialJitterFraction of itself. A zero delay
// (the immediate redial a fresh link loss arms) stays immediate.
func jitterBackoff(d time.Duration) time.Duration {
	if d <= 0 {
		return 0
	}
	spread := float64(d) * dialJitterFraction
	return time.Duration(float64(d) - spread + 2*spread*rand.Float64())
}

// nextBackoff doubles d from dialBackoffMin up to the dialBackoffMax ceiling.
func nextBackoff(d time.Duration) time.Duration {
	if d == 0 {
		return dialBackoffMin
	}
	d *= 2
	if d > dialBackoffMax {
		return dialBackoffMax
	}
	return d
}

// storeWrite is the sidecar's ONLY path to the store. Routing every write
// through here is what keeps a dead connection from going unnoticed and leaving
// the reader running against a corpse.
func (s *sidecar) storeWrite(what string, batch *corev1.EventBatch) error {
	_, err := s.store.Write(handler.Producer, batch)
	s.noteStoreErr(what, err)
	return err
}
