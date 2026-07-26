// link.go is the sidecar's store-connection state machine.
//
// THE RULE: the sidecar reads a watched file only while a store connection is
// established, and the FIRST act of every established connection — boot and
// reconnect alike, with no distinction between them — is recovering the cursors
// that store holds. Boot is not a case; boot is simply the first time the link
// is not up yet.
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
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/handler"
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

// Redial backoff, matching the discipline the shim's own links use (an
// immediate first attempt, then 100ms doubling to a 5s ceiling).
const (
	dialBackoffMin = 100 * time.Millisecond
	dialBackoffMax = 5 * time.Second
)

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

// dial attempts to bring the link up, backing off between attempts. It is the
// only thing a down sidecar does, and a no-op once the link is up.
func (s *sidecar) dial() {
	if s.link == linkUp {
		return
	}
	if err := s.establish(); err != nil {
		s.dialFailures++
		s.backoff = nextBackoff(s.backoff)
		s.log("store link DOWN: dial attempt %d failed, retrying in %s (reading no files meanwhile): %v",
			s.dialFailures, s.backoff, err)
		s.armDial(s.backoff)
		return
	}
}

// establish runs the first act of a store connection: dial, recover cursors,
// and only then start reading. Any failure leaves the link down with nothing
// read, which is the whole point — a half-established link that could write but
// had no cursors is exactly the cold start this design removes.
func (s *sidecar) establish() error {
	if err := s.store.Connect(); err != nil {
		return err
	}
	cs, err := s.store.RecoverCursors("")
	if err != nil {
		// Drop the socket: a producer connection with no recovered cursors must
		// never survive, or a later write would ride a link that skipped
		// recovery.
		s.store.Close()
		return fmt.Errorf("recovering cursors: %w", err)
	}
	s.cursors = indexCursorsByPath(cs)
	s.link = linkUp
	s.backoff = 0
	s.log("store link UP: recovered %d cursor(s)", len(s.cursors))

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
func (s *sidecar) linkLost(reason string) {
	if s.link == linkDown {
		return
	}
	s.link = linkDown
	s.cursors = nil
	s.store.Close()
	s.downSince = time.Now()
	s.dialFailures = 0
	s.backoff = 0
	s.log("store link LOST (%s); reading no files until it returns", reason)
	s.armDial(0)
}

// noteStoreErr folds a transport failure into the state machine. A store
// REJECTION arrives on a healthy connection and is NOT a link loss, so the
// connection's own liveness decides rather than the presence of an error.
func (s *sidecar) noteStoreErr(what string, err error) {
	if err == nil || s.store.Connected() {
		return
	}
	s.linkLost(fmt.Sprintf("%s: %v", what, err))
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
	downMs := time.Since(s.downSince).Milliseconds()
	reason := fmt.Sprintf("store unreachable for %dms across %d failed dial attempt(s); no files were read during the outage",
		downMs, s.dialFailures)
	s.log("store link recovered: %s", reason)
	s.emit(degradedEvents(s.watchedSessions(), reason))
	s.dialFailures = 0
}

// watchedSessions lists the distinct session ids the sidecar currently watches,
// which are the channels a degraded report can actually reach: the store fans
// an event out to that session's subscribers and nobody else.
func (s *sidecar) watchedSessions() []string {
	seen := map[string]bool{}
	var out []string
	for _, w := range s.watchers {
		id := w.target.SessionID
		if id == "" || seen[id] {
			continue
		}
		seen[id] = true
		out = append(out, id)
	}
	return out
}

// degradedEvents builds one closing DegradedState per session. They are
// SYNTHETIC/EPHEMERAL, matching how the shim reports its own degraded windows:
// an operational notice about the pipe belongs in the live stream, never in the
// durable conversation history the pipe carries.
func degradedEvents(sessions []string, reason string) []*corev1.Event {
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
				Recovered: true,
			}},
		})
	}
	return out
}

// armDial schedules the next dial attempt after d. Every caller runs on Run's
// single goroutine, so the stop/drain dance needs no further synchronization.
func (s *sidecar) armDial(d time.Duration) {
	if !s.dialT.Stop() {
		select {
		case <-s.dialT.C:
		default:
		}
	}
	s.dialT.Reset(d)
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
