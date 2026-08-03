package server

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"sort"
	"strings"
	"sync"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/statedb"
)

// ShutdownScheduler is the daemon-global SCHEDULED-SHUTDOWN DRAIN LEASE.
//
// WHY A LEASE AND NOT A TIMER. The thing a deploy actually wants is "bounce the
// daemon as soon as it is safe to", and the only honest definition of safe is
// that no agent turn and no background task is in flight anywhere. A timeout
// would answer that question with a guess, and the guess is wrong in the one
// case that matters: a turn that is taking a long time is precisely a turn
// whose work is expensive to throw away. So THERE IS NO DRAIN TIMEOUT AT ALL. A
// hung turn holds the drain forever, by design, and the holds list broadcast to
// every client is what makes that visible instead of mysterious.
//
// WHAT THE LEASE DOES WHILE IT STANDS:
//
//   - No NEW turn may start anywhere. Every submitted prompt is PARKED on its
//     session's queue with the schedule stamped on it (sessioncontroller's
//     shutdownlease.go), unclassified, and durably recorded so the bounce it is
//     waiting for cannot lose it.
//   - The holds list — every workspace with a turn in flight or live background
//     tasks — is recomputed and rebroadcast on every change.
//   - The moment the holds list is EMPTY the ordinary graceful shutdown runs,
//     with the stop_shims decision the schedule fixed when it was taken.
//
// WHAT THE LEASE NEVER DOES: replace itself. Scheduling over a live schedule is
// a loud nack, and cancelling with an id that is not the live one is a loud nack
// too, so two deploy flows can neither silently merge their intents nor kill
// each other's bounce.
//
// The lease is DURABLE. A daemon that crashes mid-drain reboots, reads the row
// back (Restore), and rebroadcasts the same schedule — because the clients that
// were told a bounce was coming are still connected and still waiting for it.
type ShutdownScheduler struct {
	store     ShutdownScheduleStore
	holds     DrainHoldSource
	evidence  DrainEvidenceSource
	tasks     sessioncontroller.LiveTaskCounter
	broadcast func(*frontendv1.ShutdownScheduleView)
	shutdown  func(stopShims bool)
	logf      dlog.Logf
	now       func() int64
	newID     func() string

	mu sync.Mutex
	// cur is the live lease, or nil when idle.
	cur *shutdownSchedule
	// lastHolds is the holds list as it was last BROADCAST, so an activity
	// notification that changed nothing does not put a redundant frame on every
	// client's queue.
	lastHolds []sessioncontroller.DrainHold
	// unresolved is the pessimistic set a RESTORED lease seeds itself with: the
	// sessions whose real hold state this daemon has not observed yet, keyed by
	// session id.
	//
	// IT IS A THIRD STATE, and that is the whole point. Before it existed a
	// session contributed either a hold or nothing, and "nothing" was produced
	// both by a session that genuinely holds nothing and by a session nobody has
	// asked — which at boot is EVERY session. An unresolved entry counts as a
	// hold, so the two can no longer be confused into a bounce.
	//
	// It empties only by AFFIRMATIVE RESOLUTION (resolveUnresolved), never by
	// elapsed time: there is no timer here for the same reason there is no drain
	// timeout anywhere in this engine.
	unresolved map[string]RegisteredSession
}

// shutdownSchedule is one held lease.
type shutdownSchedule struct {
	id            string
	scheduledAtMs int64
	cause         string
	stopShims     bool
	// executing latches the one transition to the shutdown itself, so two
	// simultaneous last-hold-cleared observations cannot both run it.
	executing bool
}

// ShutdownScheduleStore is the durable half of the lease. Satisfied by
// *statedb.ShutdownSchedules. Required: a scheduler with no store could promise
// a bounce that a crash erases without trace, and every client would keep
// waiting for a drain nothing is driving.
type ShutdownScheduleStore interface {
	PutSchedule(rec statedb.ShutdownSchedule) error
	Schedule() (statedb.ShutdownSchedule, bool, error)
	ClearSchedule() (bool, error)
}

// DrainHoldSource reports every workspace currently holding the drain open, and
// is the fleet the lease binds itself to. Satisfied by
// *sessioncontroller.Manager, which already owns the observed turn boundary the
// prompt queue acts on — the drain reads the same fact rather than a second
// derivation of it.
type DrainHoldSource interface {
	DrainHolds(tasks sessioncontroller.LiveTaskCounter) []sessioncontroller.DrainHold
	// BindShutdownLease binds this engine to the fleet. Called by the engine's
	// own constructor.
	BindShutdownLease(l sessioncontroller.ShutdownLease) error
	// AcquireShutdownHolds parks the prompts already queued when the lease is
	// taken, and reports how many.
	AcquireShutdownHolds(scheduleID string) int
	// ReleaseShutdownHolds sheds the hold from every prompt one schedule parked,
	// returning them to ordinary delivery flow.
	ReleaseShutdownHolds(scheduleID string)
	// WiredSessions names every session the fleet currently holds a controller
	// for. It is what lets "absent from DrainHolds" be read as "holds nothing"
	// rather than "has not wired yet".
	WiredSessions() []string
}

// ShutdownSchedulerConfig collects the engine's dependencies. Every field is
// required; a nil one is a construction error rather than a nil-deref at
// schedule time, and — for the shutdown func especially — rather than a lease
// that drains perfectly and then never bounces anything.
type ShutdownSchedulerConfig struct {
	Store ShutdownScheduleStore
	Holds DrainHoldSource
	// Evidence is the durable evidence a RESTORED lease seeds its unresolved set
	// from. Required: without it Restore would judge quiescence against a fleet
	// that has not been wired yet and bounce the daemon over live turns.
	Evidence  DrainEvidenceSource
	LiveTasks sessioncontroller.LiveTaskCounter
	Broadcast func(*frontendv1.ShutdownScheduleView)
	Shutdown  func(stopShims bool)
	Logf      dlog.Logf
	// Now and NewScheduleID are injected by tests. Zero values take the real
	// wall clock and a crypto/rand id.
	Now           func() int64
	NewScheduleID func() string
}

// NewShutdownScheduler validates the engine's dependencies, BINDS IT TO THE
// FLEET, and returns it.
//
// The binding happens here rather than at a call site because it is the whole
// safety property: an engine that exists while the prompt queue has never heard
// of it would hold a lease nothing enforces, park nothing, and then bounce a
// daemon in the middle of a turn it believed was not running. Constructing the
// engine and binding it are therefore one operation with no gap between them.
func NewShutdownScheduler(cfg ShutdownSchedulerConfig) (*ShutdownScheduler, error) {
	switch {
	case cfg.Store == nil:
		return nil, fmt.Errorf("server: the shutdown scheduler needs a durable store; without one a crash mid-drain would erase a lease every client is waiting on")
	case cfg.Holds == nil:
		return nil, fmt.Errorf("server: the shutdown scheduler needs a drain-hold source")
	case cfg.Evidence == nil:
		return nil, fmt.Errorf("server: the shutdown scheduler needs a drain-evidence source; without one a lease restored mid-drain would judge quiescence against a fleet that has not been wired yet, see zero holds, and bounce the daemon over every surviving mid-turn shim")
	case cfg.LiveTasks == nil:
		return nil, fmt.Errorf("server: the shutdown scheduler needs a live-task counter; without one a workspace running background tasks would read as quiescent")
	case cfg.Broadcast == nil:
		return nil, fmt.Errorf("server: the shutdown scheduler needs a broadcast func; an unbroadcast lease is invisible to every client it blocks")
	case cfg.Shutdown == nil:
		return nil, fmt.Errorf("server: the shutdown scheduler needs the graceful-shutdown func; without it a completed drain would never actually bounce the daemon")
	}
	s := &ShutdownScheduler{
		store: cfg.Store, holds: cfg.Holds, evidence: cfg.Evidence, tasks: cfg.LiveTasks,
		broadcast: cfg.Broadcast, shutdown: cfg.Shutdown,
		logf: cfg.Logf, now: cfg.Now, newID: cfg.NewScheduleID,
	}
	if s.logf == nil {
		s.logf = func(string, ...any) {}
	}
	if s.now == nil {
		s.now = func() int64 { return time.Now().UnixMilli() }
	}
	if s.newID == nil {
		s.newID = newScheduleID
	}
	if err := cfg.Holds.BindShutdownLease(s); err != nil {
		return nil, fmt.Errorf("server: binding the shutdown scheduler to the session fleet: %w", err)
	}
	return s, nil
}

// newScheduleID mints a schedule identity.
func newScheduleID() string {
	var b [12]byte
	if _, err := rand.Read(b[:]); err != nil {
		// A colliding schedule id would let a stale cancel kill a newer
		// schedule, which is exactly what the id exists to prevent. Not a
		// condition to paper over with a weaker id.
		panic(fmt.Sprintf("server: crypto/rand failed minting a shutdown schedule id: %v", err))
	}
	return "sd_" + hex.EncodeToString(b[:])
}

// HeldSchedule reports the live lease's id. It satisfies
// sessioncontroller.ShutdownLease and is on the prompt submit path, so it does
// nothing but read one field under one mutex.
func (s *ShutdownScheduler) HeldSchedule() (string, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.cur == nil {
		return "", false
	}
	return s.cur.id, true
}

// LeaseProvenance is the lease as a shim-stop log line needs it. It satisfies
// sessioncontroller.ShutdownLease.
func (s *ShutdownScheduler) LeaseProvenance() (string, string, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.cur == nil {
		return "", "", false
	}
	return s.cur.id, s.cur.cause, true
}

// View renders the lease for a broadcast or a connect snapshot. Exactly one arm
// is always set: idle is a REAL value, never an absent field, so a client can
// never confuse "no information" with "no lease".
func (s *ShutdownScheduler) View() *frontendv1.ShutdownScheduleView {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.viewLocked(s.lastHolds)
}

func (s *ShutdownScheduler) viewLocked(holds []sessioncontroller.DrainHold) *frontendv1.ShutdownScheduleView {
	if s.cur == nil {
		return &frontendv1.ShutdownScheduleView{
			State: &frontendv1.ShutdownScheduleView_Idle{Idle: &frontendv1.ShutdownScheduleIdle{}},
		}
	}
	return &frontendv1.ShutdownScheduleView{
		State: &frontendv1.ShutdownScheduleView_Draining{
			Draining: &frontendv1.ShutdownScheduleDraining{
				ScheduleId:    s.cur.id,
				ScheduledAtMs: s.cur.scheduledAtMs,
				Cause:         s.cur.cause,
				StopShims:     s.cur.stopShims,
				Holds:         holdViews(holds),
			},
		},
	}
}

// holdViews translates the fleet's holds to the wire.
//
// The two arms are SIBLINGS, not alternatives: a session can hold an in-flight
// turn and live background tasks at the same time, so both are set when both
// are true. A hold with neither is never produced by DrainHolds and is dropped
// here as well — the proto requires at least one, and a hold that explains
// nothing is worse than no hold.
func holdViews(holds []sessioncontroller.DrainHold) []*frontendv1.ShutdownHold {
	out := make([]*frontendv1.ShutdownHold, 0, len(holds))
	for _, h := range holds {
		if !h.TurnActive && h.LiveTasks <= 0 {
			continue
		}
		v := &frontendv1.ShutdownHold{Workspace: h.Workspace, SessionId: h.SessionID}
		if h.TurnActive {
			// The id may be empty for a turn this daemon adopted rather than
			// started. The arm being SET is the fact that a turn is running;
			// the id is what names it when this process knows the name.
			v.Turn = &frontendv1.ShutdownHoldTurn{TurnId: h.TurnID}
		}
		if h.LiveTasks > 0 {
			v.Tasks = &frontendv1.ShutdownHoldTasks{Count: int32(h.LiveTasks)}
		}
		out = append(out, v)
	}
	return out
}

// Schedule takes the drain lease. Reports the minted schedule id.
//
// A SECOND SCHEDULE IS A LOUD NACK, never a silent replace. Two deploy flows
// arriving at once mean two different intents about stop_shims and two
// different causes, and quietly keeping one of them would leave the other
// believing a bounce it never got is coming.
func (s *ShutdownScheduler) Schedule(stopShims bool, cause string) (string, error) {
	s.mu.Lock()
	if s.cur != nil {
		live := s.cur.id
		liveCause := s.cur.cause
		s.mu.Unlock()
		err := fmt.Errorf("server: a shutdown is already scheduled (schedule_id=%s, cause=%q); cancel it before scheduling another, because replacing it silently would drop one of the two intents", live, liveCause)
		s.logf("server: shutdown schedule REFUSED initiator=frontend requested_cause=%q requested_stop_shims=%v live_schedule=%s live_cause=%q reason=already_scheduled",
			cause, stopShims, live, liveCause)
		return "", err
	}
	sched := &shutdownSchedule{id: s.newID(), scheduledAtMs: s.now(), cause: cause, stopShims: stopShims}
	s.cur = sched
	s.mu.Unlock()

	// DURABLE BEFORE VISIBLE. A lease every client has been told about but no
	// record remembers is exactly the state a crash turns into a silent
	// cancellation, so the row goes down before the frame goes out. A write
	// failure releases the lease outright rather than standing on a promise the
	// daemon cannot keep.
	if err := s.store.PutSchedule(statedb.ShutdownSchedule{
		ScheduleID: sched.id, ScheduledAtMs: sched.scheduledAtMs, Cause: cause, StopShims: stopShims,
	}); err != nil {
		s.mu.Lock()
		if s.cur == sched {
			s.cur = nil
		}
		s.mu.Unlock()
		s.logf("server: shutdown schedule FAILED initiator=frontend schedule_id=%s cause=%q stop_shims=%v reason=durable_write_failed error=%v — the lease was released and nothing is draining",
			sched.id, cause, stopShims, err)
		return "", fmt.Errorf("server: recording the shutdown schedule durably failed, so the drain lease was not taken: %w", err)
	}

	parked := s.holds.AcquireShutdownHolds(sched.id)
	s.logf("server: shutdown SCHEDULED initiator=frontend schedule_id=%s cause=%q stop_shims=%v parked_prompts=%d drain_timeout=none — no new turn may start anywhere; the daemon bounces when every hold clears",
		sched.id, cause, stopShims, parked)

	s.reevaluate("scheduled")
	return sched.id, nil
}

// Cancel releases the lease and returns every parked prompt to ordinary flow.
//
// A STALE OR UNKNOWN id IS A LOUD NACK. The id is what stops a cancel aimed at
// a finished schedule from killing the one that replaced it, so a mismatch is
// the case the id exists for and is reported rather than absorbed.
func (s *ShutdownScheduler) Cancel(scheduleID string) error {
	s.mu.Lock()
	switch {
	case s.cur == nil:
		s.mu.Unlock()
		s.logf("server: shutdown schedule cancel REFUSED initiator=frontend requested_schedule=%s reason=no_schedule_held", scheduleID)
		return fmt.Errorf("server: no shutdown is scheduled, so schedule %q cannot be cancelled", scheduleID)
	case s.cur.id != scheduleID:
		live := s.cur.id
		s.mu.Unlock()
		s.logf("server: shutdown schedule cancel REFUSED initiator=frontend requested_schedule=%s live_schedule=%s reason=stale_schedule_id",
			scheduleID, live)
		return fmt.Errorf("server: cancel names schedule %q but the live one is %q; refusing, because a cancel aimed at an old schedule must never kill a newer one", scheduleID, live)
	case s.cur.executing:
		live := s.cur.id
		s.mu.Unlock()
		s.logf("server: shutdown schedule cancel REFUSED initiator=frontend requested_schedule=%s reason=already_executing", live)
		return fmt.Errorf("server: schedule %q has already drained and its shutdown is executing; it can no longer be cancelled", live)
	}
	s.cur = nil
	s.lastHolds = nil
	s.unresolved = nil
	view := s.viewLocked(nil)
	s.mu.Unlock()

	if _, err := s.store.ClearSchedule(); err != nil {
		// LOUD AND NOT FATAL. The lease is already released in memory and every
		// client is about to be told so; a surviving row would only resurrect a
		// cancelled schedule on the next boot, which Restore reports as the
		// anomaly it is rather than acting on silently.
		s.logf("server: shutdown schedule durable clear FAILED initiator=frontend schedule_id=%s error=%v — the lease is released in memory; the stale row will be reported at the next boot",
			scheduleID, err)
	}
	s.holds.ReleaseShutdownHolds(scheduleID)
	s.logf("server: shutdown schedule CANCELLED initiator=frontend schedule_id=%s — the drain lease is released and every prompt it parked rejoins ordinary delivery",
		scheduleID)
	s.broadcast(view)
	return nil
}

// NoteDrainActivity re-reads the holds and acts on what changed. It is the
// engine's ONLY reaction path: every trigger — a turn boundary, a workspace
// state forward carrying a new task count, a session coming up — funnels here,
// and none of them is trusted to describe the change it caused. The holds are
// always re-derived whole.
func (s *ShutdownScheduler) NoteDrainActivity() { s.reevaluate("activity") }

func (s *ShutdownScheduler) reevaluate(trigger string) {
	// THE HOLDS ARE READ BEFORE THE MUTEX. DrainHolds takes the session fleet's
	// lock, and the fleet reads this engine's lock on its submit path, so a read
	// of one under the other would invert them.
	if _, held := s.HeldSchedule(); !held {
		return
	}
	holds := s.holds.DrainHolds(s.tasks)
	// THE UNRESOLVED SET IS RESOLVED BEFORE THE MUTEX, alongside the holds read
	// and for the same reason: it reads the fleet and runs the two probes, and
	// doing either under this engine's lock would invert it against the fleet's.
	holds = append(holds, s.resolveUnresolved(trigger, holds)...)
	sortHolds(holds)

	s.mu.Lock()
	if s.cur == nil || s.cur.executing {
		s.mu.Unlock()
		return
	}
	if len(holds) > 0 {
		// THE NO-CHANGE SUPPRESSION APPLIES ONLY HERE, to the holding branch,
		// and deliberately not to the drained one below. A drain that is still
		// waiting on exactly what it was waiting on a moment ago has nothing new
		// to say, and rebroadcasting it would put a redundant frame on every
		// client's queue for every unrelated workspace-state forward. A drain
		// that is FINISHED has to act whether or not the holds list "changed":
		// it can reach zero holds without ever having been observed above zero
		// — a lease taken over an already-quiet daemon does exactly that — and
		// suppressing it there would leave the bounce waiting forever on an
		// edge that already happened.
		if sameHolds(s.lastHolds, holds) {
			s.mu.Unlock()
			return
		}
		s.lastHolds = holds
		view := s.viewLocked(holds)
		sched := *s.cur
		s.mu.Unlock()
		s.logf("server: shutdown drain HOLDING schedule_id=%s cause=%q stop_shims=%v trigger=%s holds=%d detail=%s drain_timeout=none",
			sched.id, sched.cause, sched.stopShims, trigger, len(holds), describeHolds(holds))
		s.broadcast(view)
		return
	}
	// DRAINED — and reaching here means BOTH the fleet's holds and the
	// unresolved set are empty, because the unresolved entries were folded into
	// holds above. Affirmative emptiness, not "nobody answered": every session
	// the registry remembers has either wired and been observed holding nothing,
	// or been proven to have no shim at all.
	//
	// The lease is retired here, before the shutdown runs, and the durable row
	// goes with it: a lease that survived its own successful shutdown would
	// block every prompt on the next daemon forever, with nobody left who
	// remembers asking for it.
	s.cur.executing = true
	sched := *s.cur
	s.cur = nil
	s.lastHolds = nil
	s.unresolved = nil
	view := s.viewLocked(nil)
	s.mu.Unlock()

	if _, err := s.store.ClearSchedule(); err != nil {
		s.logf("server: shutdown schedule durable clear FAILED schedule_id=%s phase=drained error=%v — the next boot would restore a schedule whose shutdown already ran; reported here so that restore is recognizable",
			sched.id, err)
	}
	s.logf("server: shutdown drain COMPLETE schedule_id=%s cause=%q stop_shims=%v trigger=%s holds=0 — every hold cleared, executing the graceful shutdown now",
		sched.id, sched.cause, sched.stopShims, trigger)
	// SHIM-STOP PROVENANCE. This is the one place a scheduled bounce decides
	// the fate of every session shim, so the decision is stated in full here
	// rather than inferred from a stop_shims flag three layers down.
	if sched.stopShims {
		s.logf("server: SHIM STOP DECIDED initiator=scheduled_shutdown schedule_id=%s cause=%q scope=all_sessions reason=schedule_requested_stop_shims — every session shim will be SIGTERMed on the way out because the schedule was taken with stop_shims set",
			sched.id, sched.cause)
	} else {
		s.logf("server: SHIM STOP DECLINED initiator=scheduled_shutdown schedule_id=%s cause=%q scope=all_sessions reason=schedule_preserves_shims — every session shim is PRESERVED and will redial the next daemon",
			sched.id, sched.cause)
	}
	s.broadcast(view)
	// Asynchronous, exactly as the ordinary shutdown command is: this call can
	// be reached from a shim read-loop goroutine, and the teardown it starts
	// waits on those goroutines.
	go s.shutdown(sched.stopShims)
}

// Restore reads the durable lease back at boot and re-takes it.
//
// A daemon that crashed mid-drain left clients holding a draining view and a
// deploy waiting for a bounce. Coming back idle would strand both: the deploy
// waits forever for a schedule nothing is driving, and the next prompt starts a
// turn under a lease the deployer still believes is held. So the schedule is
// re-taken, the queues it parked are restored per session as those sessions come
// up (sessioncontroller's restoreShutdownHolds), and the view is rebroadcast.
func (s *ShutdownScheduler) Restore() error {
	rec, ok, err := s.store.Schedule()
	if err != nil {
		return fmt.Errorf("server: reading the durable shutdown schedule at boot: %w", err)
	}
	if !ok {
		s.logf("server: shutdown schedule restore found none — the daemon boots with no drain lease held")
		return nil
	}
	s.mu.Lock()
	if s.cur != nil {
		live := s.cur.id
		s.mu.Unlock()
		return fmt.Errorf("server: refusing to restore shutdown schedule %q over the live schedule %q; Restore runs once, at boot, before anything can schedule", rec.ScheduleID, live)
	}
	s.cur = &shutdownSchedule{
		id: rec.ScheduleID, scheduledAtMs: rec.ScheduledAtMs,
		cause: rec.Cause, stopShims: rec.StopShims,
	}
	s.mu.Unlock()

	// THE PESSIMISTIC SEED, before anything can conclude the fleet is quiet.
	// Restore runs ahead of the boot sweeper and ahead of every reattach, so the
	// fleet is empty here by construction; seeding from the registry and the two
	// probes is what stops "empty fleet" from being read as "nothing running".
	seed := s.seedUnresolved()
	s.mu.Lock()
	s.unresolved = seed
	s.mu.Unlock()

	parked := s.holds.AcquireShutdownHolds(rec.ScheduleID)
	s.logf("server: shutdown schedule RESTORED initiator=boot schedule_id=%s cause=%q stop_shims=%v scheduled_at_ms=%d parked_prompts=%d unresolved_sessions=%d — a previous daemon took this lease and did not finish draining; it stands, and every session whose shim state is not yet known HOLDS it",
		rec.ScheduleID, rec.Cause, rec.StopShims, rec.ScheduledAtMs, parked, len(seed))
	s.reevaluate("restored")
	return nil
}

// seedUnresolved classifies every session the registry remembers into the
// pessimistic UNRESOLVED set. Called once, from Restore, with no lock held.
//
// The verdicts are the boot sweeper's verdicts, deliberately: a session whose
// shim is connected, whose lock is held, or whose probe FAILED is one this
// daemon cannot yet say anything about, and the lease treats all three the same
// way — as work it must not cut. Only the neither-connected-nor-locked verdict
// is an affirmative "there is no shim here", and it is the only one that keeps a
// session out of the set.
func (s *ShutdownScheduler) seedUnresolved() map[string]RegisteredSession {
	out := map[string]RegisteredSession{}
	for _, rs := range s.evidence.RegisteredSessions() {
		if !s.probeUnresolved(rs, "seed") {
			continue
		}
		out[rs.SessionID] = rs
	}
	return out
}

// probeUnresolved reports whether the durable evidence leaves this session's
// hold state UNKNOWN. An error is never read as free.
func (s *ShutdownScheduler) probeUnresolved(rs RegisteredSession, phase string) bool {
	connected, err := s.evidence.ShimConnected(rs.SessionID)
	if err != nil {
		s.logf("server: shutdown drain %s: session %s (ws %q) parked-connection probe FAILED, so whether a shim is alive is UNKNOWN; it HOLDS the drain rather than being counted quiescent: %v",
			phase, rs.SessionID, rs.Workspace, err)
		return true
	}
	if connected {
		s.logf("server: shutdown drain %s: session %s (ws %q) has a shim connected but not yet wired to this daemon; it HOLDS the drain until its real turn state is observed",
			phase, rs.SessionID, rs.Workspace)
		return true
	}
	held, err := s.evidence.ShimLockHeld(rs.SessionID)
	if err != nil {
		s.logf("server: shutdown drain %s: session %s (ws %q) lock probe FAILED, so whether a shim is alive is UNKNOWN; it HOLDS the drain rather than being counted quiescent: %v",
			phase, rs.SessionID, rs.Workspace, err)
		return true
	}
	if held {
		s.logf("server: shutdown drain %s: session %s (ws %q) has a live shim holding its lock but has not redialled yet; it HOLDS the drain until its real turn state is observed",
			phase, rs.SessionID, rs.Workspace)
		return true
	}
	return false
}

// resolveUnresolved clears every unresolved session that has since been
// AFFIRMATIVELY resolved, and returns the rest as holds. Called with no lock
// held: it reads the fleet and runs the probes.
//
// THE TWO RESOLUTIONS, and there are no others:
//
//   - THE SESSION WIRED. The fleet now holds a controller for it, so its real
//     hold state is in the holds list this was called with — present if it holds
//     something, absent because it genuinely holds nothing. Either way the
//     question has been answered by the authority that owns it.
//   - THE SESSION IS PROVEN SHIMLESS. Neither connected nor locked, the one
//     verdict that positively asserts there is nothing there.
//
// Elapsed time is not on the list. An entry nobody can resolve holds the drain
// forever, exactly as a hung turn does, and the broadcast holds are what make
// that visible rather than mysterious.
func (s *ShutdownScheduler) resolveUnresolved(trigger string, holds []sessioncontroller.DrainHold) []sessioncontroller.DrainHold {
	s.mu.Lock()
	if len(s.unresolved) == 0 {
		s.mu.Unlock()
		return nil
	}
	pending := make([]RegisteredSession, 0, len(s.unresolved))
	for _, rs := range s.unresolved {
		pending = append(pending, rs)
	}
	s.mu.Unlock()

	wired := make(map[string]bool)
	for _, id := range s.holds.WiredSessions() {
		wired[id] = true
	}
	held := make(map[string]bool, len(holds))
	for _, h := range holds {
		held[h.SessionID] = true
	}

	var resolved []string
	for _, rs := range pending {
		switch {
		case wired[rs.SessionID]:
			s.logf("server: shutdown drain resolve trigger=%s: session %s (ws %q) has WIRED; its real hold state is now observed and it no longer holds the drain as unresolved",
				trigger, rs.SessionID, rs.Workspace)
			resolved = append(resolved, rs.SessionID)
		case !s.probeUnresolved(rs, "resolve"):
			s.logf("server: shutdown drain resolve trigger=%s: session %s (ws %q) is PROVEN SHIMLESS (neither connected nor locked); it no longer holds the drain",
				trigger, rs.SessionID, rs.Workspace)
			resolved = append(resolved, rs.SessionID)
		}
	}

	s.mu.Lock()
	for _, id := range resolved {
		delete(s.unresolved, id)
	}
	out := make([]sessioncontroller.DrainHold, 0, len(s.unresolved))
	for _, rs := range s.unresolved {
		// A session already named in the real holds list is not named twice: it
		// wired between the holds read and this resolve, and the fleet's answer
		// is the better one.
		if held[rs.SessionID] {
			continue
		}
		// THE TURN ARM WITH AN EMPTY ID. The proto requires a hold to explain
		// itself with a turn and/or a task count, and neither is knowable for a
		// session nobody has talked to. The empty-id turn is the established
		// encoding for "work we cannot name but must not cut" — it is exactly
		// what an ADOPTED turn broadcasts — and that is precisely this case.
		out = append(out, sessioncontroller.DrainHold{
			Workspace: rs.Workspace, SessionID: rs.SessionID, TurnActive: true,
		})
	}
	s.mu.Unlock()
	return out
}

// sortHolds orders a holds list by workspace then session, so two reads of an
// unchanged fleet compare equal under sameHolds. DrainHolds already sorts what
// it returns; this re-sorts the union once the unresolved entries are folded in.
func sortHolds(holds []sessioncontroller.DrainHold) {
	sort.Slice(holds, func(i, j int) bool {
		if holds[i].Workspace != holds[j].Workspace {
			return holds[i].Workspace < holds[j].Workspace
		}
		return holds[i].SessionID < holds[j].SessionID
	})
}

// sameHolds reports whether two holds lists are identical. Both come from
// DrainHolds, which sorts by workspace, so this is an element-wise compare.
func sameHolds(a, b []sessioncontroller.DrainHold) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// describeHolds renders the holds for one log line: what the bounce is waiting
// on, in full, so a log trace alone answers "why has this daemon not bounced".
func describeHolds(holds []sessioncontroller.DrainHold) string {
	parts := make([]string, 0, len(holds))
	for _, h := range holds {
		parts = append(parts, fmt.Sprintf("ws=%q session=%s turn=%q live_tasks=%d",
			h.Workspace, h.SessionID, h.TurnID, h.LiveTasks))
	}
	return "[" + strings.Join(parts, " | ") + "]"
}
