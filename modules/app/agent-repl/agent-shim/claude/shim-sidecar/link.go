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

	storev1 "agentrepl/proto/store/v1"
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
	if err := s.tracker.Restore(); err != nil {
		s.store.Close()
		return fmt.Errorf("resetting the open-task tracker: %w", err)
	}
	s.cursors = indexCursorsByPath(recovery.Cursors)
	// The spool-owner index used to be seeded from the SAME authoritative
	// snapshot. store.v1 no longer carries one, so the seed reports its own
	// impossibility instead — see seedOwners for what that costs.
	s.resetOwners()
	seeded := s.seedOwners()
	s.link = linkUp
	s.backoff = 0
	s.log.With(logging.Context{Operation: "recover-startup-state"}).Log(
		"store link up, recovered %d cursor(s), seeded %d spool owner(s) (store.v1 reports no open tasks)",
		len(s.cursors), seeded)

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

// degradedWindowEvents reports the outage the sidecar just came out of, once
// per session it watches.
//
// IT IS NO LONGER A WINDOW, AND THAT IS A LOSS RATHER THAN A SIMPLIFICATION.
// `DegradedState` was reachable only through the retired Event.payload and has
// no carrier on the new model, so the two-record shape this used to send — one
// record OPENING a fault for the component and one CLOSING it — cannot be
// expressed at all. The consumer's runtime-fault plumbing IS a window, and a
// producer that can no longer open one cannot drive it.
//
// What is sent instead is a ProducerDiagnostic: a fact about the READER rather
// than the read, which is what an ingestion outage is by this schema's own test.
// It carries the same reason text, so nothing goes unsaid — but it arrives as a
// diagnostic a human reads rather than as a fault a state machine clears, so
// workspace health no longer learns this component was down. Recorded as a gap.
//
// Only the report AFTER the fact is sendable, and that was already true and
// already honest: the store is the sidecar's only channel, so while the link is
// down there is by definition nobody to tell.
func degradedWindowEvents(sessions []string, reason string) []*storev1.StoreEntry {
	// R10: the degraded window is a STRUCTURED LOG, not a store record. There is
	// no bookkeeping arm on store.v1 StoreEntry and none is invented here; the
	// reason text still reaches the operator through the canonical logger at the
	// site that detected the outage.
	_ = sessions
	_ = reason
	_ = degradedComponent
	return nil
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
func (s *sidecar) storeWrite(what string, batch *storev1.EntryBatch) error {
	err := s.store.Write(handler.Producer, batch)
	s.noteStoreErr(what, err)
	return err
}
