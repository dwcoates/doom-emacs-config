package server

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"
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
}

// ShutdownSchedulerConfig collects the engine's dependencies. Every field is
// required; a nil one is a construction error rather than a nil-deref at
// schedule time, and — for the shutdown func especially — rather than a lease
// that drains perfectly and then never bounces anything.
type ShutdownSchedulerConfig struct {
	Store     ShutdownScheduleStore
	Holds     DrainHoldSource
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
	case cfg.LiveTasks == nil:
		return nil, fmt.Errorf("server: the shutdown scheduler needs a live-task counter; without one a workspace running background tasks would read as quiescent")
	case cfg.Broadcast == nil:
		return nil, fmt.Errorf("server: the shutdown scheduler needs a broadcast func; an unbroadcast lease is invisible to every client it blocks")
	case cfg.Shutdown == nil:
		return nil, fmt.Errorf("server: the shutdown scheduler needs the graceful-shutdown func; without it a completed drain would never actually bounce the daemon")
	}
	s := &ShutdownScheduler{
		store: cfg.Store, holds: cfg.Holds, tasks: cfg.LiveTasks,
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
	// DRAINED. The lease is retired here, before the shutdown runs, and the
	// durable row goes with it: a lease that survived its own successful
	// shutdown would block every prompt on the next daemon forever, with nobody
	// left who remembers asking for it.
	s.cur.executing = true
	sched := *s.cur
	s.cur = nil
	s.lastHolds = nil
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

	parked := s.holds.AcquireShutdownHolds(rec.ScheduleID)
	s.logf("server: shutdown schedule RESTORED initiator=boot schedule_id=%s cause=%q stop_shims=%v scheduled_at_ms=%d parked_prompts=%d — a previous daemon took this lease and did not finish draining; it stands",
		rec.ScheduleID, rec.Cause, rec.StopShims, rec.ScheduledAtMs, parked)
	s.reevaluate("restored")
	return nil
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
