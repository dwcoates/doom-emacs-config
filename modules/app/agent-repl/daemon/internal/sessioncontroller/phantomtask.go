package sessioncontroller

import (
	"context"
	"sort"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"claude-repld/internal/frontend"
)

// phantomtask.go — A TASK MAY NOT OUTLIVE ITS DRIVER.
//
// # The wedge this closes
//
// On 2026-08-10 workspace `marcos-pr-remediation` (session s_9de8689040244f34)
// rendered a footer reading "1 task" for the rest of the day with nothing
// running. The daemon's own record says exactly how: the task-catalog pushes are
// well paired up to seq=4910 and then the LAST one is
// `seq=4953 event=task_started tasks=39` at 18:29:17, with no `task_ended`
// after it, ever. The shim — which owns the live set on a single-threaded event
// loop — logged `live_sdk_task_count=0` with an empty `live_sdk_task_ids` at
// 18:35:12 and again at 18:38:38, long after that start. The catalog held a task
// the only authority on the question said did not exist.
//
// # The mechanism
//
// The catalog is DERIVED, and derived state was reconciled against nothing. It
// is folded event-by-event from the retained ring at every task boundary
// (sinks.go, the Event_TaskStarted/Event_TaskEnded case of Consume, and
// frontend/translate.go BuildTaskCatalog), and an entry leaves `running` on
// EXACTLY ONE input: a TaskEnded for the same id folding after its start. Every
// way that end can fail to arrive leaves the entry running forever:
//
//   - it was DEDUPED. A backlog the daemon catches up on after a bounce is
//     replayed through the same path as live events, and the SSM drops the
//     repeat (`ssm: duplicate event skipped kind=task_ended`, ssm/ssm.go:556).
//   - it ended WHILE THE DAEMON WAS DOWN and was never replayed as live at all.
//   - a SHIM ROLL orphaned it: the task tables are per query instance, so a
//     `--resume` attaches to a conversation whose tasks started under a previous
//     shim's lifetime and whose ends this daemon generation will never see.
//
// The one reconciliation that existed — a vendor `BackgroundTasksChanged`
// snapshot (sinks.go, reconcileTasks) — is LEVEL-TRIGGERED ON A VENDOR EVENT: it
// repairs the catalog only if the SDK happens to send another one. In this
// incident none came, so nothing ever re-examined the entry.
//
// # The invariant
//
// A catalog entry that reads `running` is a claim that a task is running. This
// file makes that claim checkable and then enforces it, on the two edges that
// can answer it:
//
//   - A BOUNDED SWEEP asks the shim. The shim's live-task list is the ONE
//     authority (QueryLiveTasks, shimclient/control.go), and an entry absent
//     from it after a bounded grace is closed and reported.
//   - A TURN END drops that grace for the tasks standing at it, the same edge
//     the turn's own claims are closed on. It does NOT close them itself:
//     detached work outliving its turn is a first-class state here
//     (RENDER_STATE_IDLE_ASYNC), so the boundary only makes each entry a
//     question for the next sweep, and the shim's answer still decides.
//
// A ROTATION drops the set whole, with the ring it mirrors (sinks.go,
// purgeRetained): those ids name tasks of a conversation the new seq space
// refers to nowhere.
//
// # Why the discriminator is the shim's list and never silence
//
// The obvious watchdog — "no events for this task in N minutes" — cannot tell a
// phantom from a subagent grinding through a long tool call, and closing the
// second is far worse than leaving the first. So nothing here is decided by a
// timeout: the sweep's grace only decides WHEN TO ASK, and the answer decides
// what to close. A shim that refuses, cannot be reached, or acks without a set
// closes nothing at all — that is a peer that did not answer, not a session with
// no tasks, and the two are kept distinguishable all the way down the wire
// (core.proto LiveTaskSet).

// phantomTaskGraceMs bounds how long a catalog entry may stand open before the
// sweep asks the shim about it.
//
// It is a WHEN-TO-ASK bound, not a lifetime: nothing is closed because this
// elapsed, only looked at. It has to be longer than the window in which a task's
// start is observed by the daemon before the shim's own set has it — which is
// zero, since the shim adds the id to its live set as it commits the very
// TaskStarted the daemon folds — so the minute is head-room, and short enough
// that a phantom is retired within two idle sweeps rather than within a day.
const phantomTaskGraceMs int64 = 60_000

// phantomTaskQueryTimeout bounds the one control round-trip the sweep makes.
//
// The sweep is a serial walk on the daemon's idle sweeper, so an unbounded ask
// against one wedged shim would stall every workspace behind it. A timeout here
// closes nothing: it is a shim that did not answer, and the entry stands until
// the next sweep asks again.
const phantomTaskQueryTimeout = 5 * time.Second

// phantomTaskDegradeProbes is how many CONSECUTIVE unanswered live-set probes a
// session's shim must run up before the sweep calls its answerability degraded.
//
// Below it an unanswered probe is a BLIP: the ordinary case is a sweep landing
// inside a shim roll, a reconnect, or a turn that briefly starved the shim's
// event loop, and the very next sweep gets an answer. At or above it the shim
// has declined to answer every time it was asked since, which is a condition an
// operator has to see.
//
// COUNTED IN PROBES, NEVER IN SECONDS, for the same reason the sweep itself is a
// comparison rather than a timer: an elapsed window is satisfied by wall time in
// which nothing was ever asked — a suspended laptop or a starved sweeper — and
// would report a shim as unanswerable on the strength of never having been
// questioned.
const phantomTaskDegradeProbes = 3

// taskLiveSetVerdict is what ONE live-set probe outcome did to a session's
// latched answerability. Every outcome has one, because every probe outcome is
// recorded; the verdict decides only the LEVEL the record is written at.
type taskLiveSetVerdict int

const (
	// taskLiveSetBlip — unanswered, but below the degrade threshold.
	taskLiveSetBlip taskLiveSetVerdict = iota
	// taskLiveSetDegradeEdge — the crossing INTO degraded. The one warn.
	taskLiveSetDegradeEdge
	// taskLiveSetStillDegraded — unanswered again while already latched.
	taskLiveSetStillDegraded
	// taskLiveSetRecoveryEdge — answered again after the latch had closed.
	taskLiveSetRecoveryEdge
	// taskLiveSetAnsweredAfterBlips — answered after failures that never
	// reached the threshold.
	taskLiveSetAnsweredAfterBlips
	// taskLiveSetAnswered — answered with no failure run outstanding.
	taskLiveSetAnswered
)

// noteLiveSetUnanswered records one unanswered probe and returns the verdict it
// reached with the consecutive-failure count behind it.
//
// NOTHING IS DROPPED. The counter advances on every unanswered probe and the
// caller writes a record for every one of them; the latch exists so that the
// WARN fires on the single degrade edge instead of on every idle sweep for as
// long as the shim stays mute.
func (c *consumer) noteLiveSetUnanswered() (taskLiveSetVerdict, int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.liveSetFailures++
	count := c.liveSetFailures
	switch {
	case c.liveSetDegraded:
		return taskLiveSetStillDegraded, count
	case count >= phantomTaskDegradeProbes:
		c.liveSetDegraded = true
		return taskLiveSetDegradeEdge, count
	default:
		return taskLiveSetBlip, count
	}
}

// noteLiveSetAnswered clears the failure run and returns the verdict the answer
// reached, with the run length it ended.
//
// A LATCH YOU CAN ENTER BUT NEVER WATCH LEAVE IS ITS OWN DEFECT, so the recovery
// edge is reported whenever the latch was closed, and a recovery from a
// sub-threshold run is reported too — those failures were recorded, and their
// end belongs beside them.
func (c *consumer) noteLiveSetAnswered() (taskLiveSetVerdict, int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	count := c.liveSetFailures
	degraded := c.liveSetDegraded
	c.liveSetFailures = 0
	c.liveSetDegraded = false
	switch {
	case degraded:
		return taskLiveSetRecoveryEdge, count
	case count > 0:
		return taskLiveSetAnsweredAfterBlips, count
	default:
		return taskLiveSetAnswered, 0
	}
}

// observeTaskLifecycle records the open/closed edge of one task, so the sweep
// has something to ask ABOUT without re-folding the whole ring.
//
// It mirrors the catalog exactly — opened by TaskStarted, retired by TaskEnded —
// because a set that disagreed with the catalog would either hide a phantom or
// invent one.
func (c *consumer) observeTaskLifecycle(ev *corev1.Event) {
	switch p := ev.GetPayload().(type) {
	case *corev1.Event_TaskStarted:
		id := p.TaskStarted.GetTaskId()
		if id == "" {
			return
		}
		c.mu.Lock()
		if c.openTasks == nil {
			c.openTasks = make(map[string]int64, 4)
		}
		// A RE-OBSERVED START DOES NOT RESTAMP THE CLOCK, for the reason the
		// undriven-turn watch does not (undriventurn.go): a replayed start whose
		// instant kept moving would never reach its own grace.
		if _, open := c.openTasks[id]; !open {
			c.openTasks[id] = c.instantOf(ev)
		}
		c.mu.Unlock()
	case *corev1.Event_TaskEnded:
		id := p.TaskEnded.GetTaskId()
		if id == "" {
			return
		}
		c.mu.Lock()
		delete(c.openTasks, id)
		c.mu.Unlock()
	}
}

// instantOf is the event's own produced-at instant, falling back to this
// consumer's clock for an event that carries none (a daemon-composed one).
func (c *consumer) instantOf(ev *corev1.Event) int64 {
	if at := ev.GetProducedAtMs(); at > 0 {
		return at
	}
	return c.now()
}

// adoptLiveTaskSet replaces the open-task set with an AUTHORITATIVE live list —
// the vendor's `BackgroundTasksChanged` snapshot (sinks.go, reconcileTasks),
// which states the whole live set at that point in the stream.
//
// An id already open KEEPS ITS INSTANT: the snapshot says the task is still
// running, not that it started again, and restamping would push its grace out on
// every snapshot the vendor sends.
func (c *consumer) adoptLiveTaskSet(ids []string, atMs int64) {
	c.mu.Lock()
	defer c.mu.Unlock()
	adopted := make(map[string]int64, len(ids))
	for _, id := range ids {
		if id == "" {
			continue
		}
		if at, open := c.openTasks[id]; open {
			adopted[id] = at
			continue
		}
		adopted[id] = atMs
	}
	c.openTasks = adopted
}

// taskEligibleNow is the observation instant of an entry whose grace has already
// been served out by an edge — a turn end. It is a sentinel rather than a second
// map so that "when was it observed" and "may it be asked about" cannot drift
// apart into two answers.
const taskEligibleNow int64 = 0

// sweepCandidates names every open task the sweep may ask the shim about as of
// nowMs, sorted so a log record reads the same way twice.
//
// ELIGIBILITY IS NOT A VERDICT. Every id returned here is a question, and the
// shim's answer is what closes anything.
func (c *consumer) sweepCandidates(nowMs int64) []string {
	c.mu.Lock()
	defer c.mu.Unlock()
	var ids []string
	for id, at := range c.openTasks {
		if at == taskEligibleNow || nowMs-at >= phantomTaskGraceMs {
			ids = append(ids, id)
		}
	}
	sort.Strings(ids)
	return ids
}

// openTaskIDs names every task the catalog currently holds open, whatever its
// grace.
func (c *consumer) openTaskIDs() []string {
	c.mu.Lock()
	defer c.mu.Unlock()
	ids := make([]string, 0, len(c.openTasks))
	for id := range c.openTasks {
		ids = append(ids, id)
	}
	sort.Strings(ids)
	return ids
}

// closeOpenTasks retires the named tasks on BOTH task planes and reports the
// ones it actually closed.
//
// IT CLOSES THEM THE WAY THE STREAM WOULD HAVE: a TaskEnded event, status LOST,
// retained on the ring the catalog is folded from. `lost`, never `done` — the
// session stopped running the task and never said how it finished, and claiming
// success would be a fabrication (it is the same status the vendor-snapshot
// reconciliation assigns a swept ghost, frontend/translate.go). Folding a real
// event rather than mutating a built catalog is what keeps this repair visible
// to every later rebuild and every resync, instead of being undone by the next
// one.
//
// IT IS IDEMPOTENT. The open set is the gate: an id already closed is not in it,
// so a second sweep over the same session synthesizes nothing and pushes
// nothing. Even a redundant fold would be a no-op — a terminal status set twice
// is the same terminal status — but the gate is what keeps the log honest.
//
// The events are EPHEMERAL (seq 0), deliberately: they are the daemon's own
// derivation and belong to no store seq space, exactly like the permission and
// failure items the ring already carries. Nothing is written to the durable
// stream, which the shim owns.
func (c *consumer) closeOpenTasks(ids []string, reason string) []string {
	if len(ids) == 0 {
		return nil
	}
	nowMs := c.now()
	var closed []string
	c.mu.Lock()
	for _, id := range ids {
		if _, open := c.openTasks[id]; !open {
			continue
		}
		delete(c.openTasks, id)
		closed = append(closed, id)
		c.ring = append(c.ring, &corev1.Event{
			SessionId:    c.sessionID,
			ProducedAtMs: nowMs,
			Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{
				TaskId: id,
				Status: corev1.TerminalStatus_TERMINAL_STATUS_LOST,
			}},
		})
	}
	c.mu.Unlock()
	if len(closed) == 0 {
		return nil
	}
	c.warn("session-controller: task catalog PHANTOM CLOSED session=%s ws=%s tasks=%v reason=%s — these entries stood `running` in the catalog with nothing behind them, so the footer reported work this session was not doing. They are retired as LOST (the session never said how they finished, and reporting `done` would invent an outcome)",
		c.sessionID, c.workspace, closed, reason)
	// THE SSM'S OWN TASK PLANE, reconciled from the SAME surviving set the
	// catalog now holds, so the derived live_task_count and the roster cannot
	// disagree about what is running. A failure here is loud and does not stop
	// the roster refresh: the two planes are independent, and losing both over
	// one failure would be worse (the argument reconcileTasks already makes).
	if err := c.ssm.ReconcileTasks(c.sessionID, c.openTaskIDs()); err != nil {
		c.warn("session-controller: task catalog PHANTOM CLOSE SSM RECONCILE FAILED session=%s ws=%s tasks=%v: %v — the roster is repaired but the SSM's live_task_count still counts the phantom until another reconciliation lands",
			c.sessionID, c.workspace, closed, err)
	}
	catalog := frontend.BuildTaskCatalog(c.workspace, c.sessionID, c.fence(), c.snapshotRing(), c.logf)
	c.logf("session-controller: task catalog push session=%s ws=%s seq=0 event=phantom_close tasks=%d",
		c.sessionID, c.workspace, len(catalog.GetTasks()))
	c.push.PushTaskCatalog(catalog)
	return closed
}

// noteTurnEndTaskEligibility makes every open catalog entry eligible for the
// next sweep IMMEDIATELY, at an accepted turn END.
//
// THE EDGE IS WHAT CLOSES A TURN'S TASKS, and it closes them through the sweep
// rather than by itself. The distinction is the whole safety argument:
//
//   - The turn boundary is the moment the tasks a turn drove are ENTITLED to be
//     re-examined, so the grace — which exists only to keep the sweep from
//     asking about a task the daemon has barely observed — has served its
//     purpose and is dropped.
//   - It is NOT evidence that they are over. DETACHED work outlives its turn by
//     design here: a session with a turn ended and tasks still running is the
//     first-class `RENDER_STATE_IDLE_ASYNC` state, and closing those entries on
//     this edge would delete a live subagent from the roster the moment its
//     launching turn returned.
//
// So the edge lowers the bar to asking, and the SHIM'S ANSWER still decides. A
// task the shim still lists survives the turn end untouched, however many turns
// it outlives; a task it does not list is closed on the next sweep instead of
// waiting out a grace that has nothing left to protect.
//
// The stamp is a floor rather than a flag: openTasksOlderThan compares against
// it, so one field carries both "observed at" and "eligible now".
func (c *consumer) noteTurnEndTaskEligibility(turnID string) {
	c.mu.Lock()
	eligible := make([]string, 0, len(c.openTasks))
	for id := range c.openTasks {
		c.openTasks[id] = taskEligibleNow
		eligible = append(eligible, id)
	}
	c.mu.Unlock()
	if len(eligible) == 0 {
		return
	}
	sort.Strings(eligible)
	c.logf("session-controller: task catalog entries eligible at turn end session=%s ws=%s turn_id=%s tasks=%v — the turn that drove them is over, so the next sweep asks the shim about them without waiting out the grace; whichever the shim still lists is left running",
		c.sessionID, c.workspace, turnID, eligible)
}

// SweepPhantomTasks asks every session with a long-open catalog entry which
// tasks its shim is ACTUALLY running, closes the ones it is not, and reports how
// many it closed.
//
// A COMPARISON AT A SWEEP rather than a timer, for the reason the undriven-turn
// watch is (undriventurn.go): a timer dies with the daemon, does not advance
// across a laptop sleep, and cannot tell "overdue" from "the deadline passed
// while the machine was asleep".
//
// A SESSION THAT CANNOT ANSWER LOSES NOTHING. A shim that is gone, refuses, or
// acks without a set leaves every entry exactly where it was: the answer is what
// closes a task, and its absence is not an answer.
func (m *Manager) SweepPhantomTasks() int {
	nowMs := m.now()
	type candidate struct {
		d   *sessionController
		ids []string
	}
	var candidates []candidate
	m.mu.Lock()
	for _, d := range m.byWS {
		if d.consumer == nil || d.client == nil {
			continue
		}
		if ids := d.consumer.sweepCandidates(nowMs); len(ids) > 0 {
			candidates = append(candidates, candidate{d: d, ids: ids})
		}
	}
	m.mu.Unlock()

	closed := 0
	for _, c := range candidates {
		closed += len(m.reconcilePhantomTasks(c.d, c.ids))
	}
	return closed
}

// reconcilePhantomTasks asks ONE session's shim for its live set and closes the
// candidates absent from it. It returns the ids it closed.
//
// Must be called with m.mu RELEASED: it makes a control round-trip and reaches
// the SSM and the frontend.
func (m *Manager) reconcilePhantomTasks(d *sessionController, candidates []string) []string {
	ctx, cancel := context.WithTimeout(m.rootCtx, phantomTaskQueryTimeout)
	defer cancel()
	live, err := d.client.QueryLiveTasks(ctx)
	if err != nil {
		// NEVER SWALLOWED, AND NEVER A CLOSE. This is the case the whole design
		// turns on: silence is not evidence that a task is gone, so an
		// unanswerable shim leaves the roster untouched and says so — on EVERY
		// sweep, at a level the latch decides.
		verdict, failures := d.consumer.noteLiveSetUnanswered()
		switch verdict {
		case taskLiveSetDegradeEdge:
			m.warnf("session-controller: task catalog LIVE-SET UNAVAILABLE ws=%q session=%s consecutive_failures=%d/%d open_tasks=%v: %v — the shim has not answered a live-set probe on any sweep since, so the catalog is now unreconcilable; nothing is closed, because a shim that did not answer is not a session with no tasks, and the entries stand until a later sweep gets an answer",
				d.workspace, d.sessionID, failures, phantomTaskDegradeProbes, candidates, err)
		case taskLiveSetStillDegraded:
			m.logf("session-controller: task catalog live-set STILL UNAVAILABLE ws=%q session=%s consecutive_failures=%d open_tasks=%v: %v — already reported as degraded on the crossing; nothing is closed",
				d.workspace, d.sessionID, failures, candidates, err)
		default:
			m.logf("session-controller: task catalog live-set probe unanswered ws=%q session=%s consecutive_failures=%d/%d (below the degrade threshold) open_tasks=%v: %v — nothing is closed; the next sweep asks again",
				d.workspace, d.sessionID, failures, phantomTaskDegradeProbes, candidates, err)
		}
		return nil
	}
	if verdict, failures := d.consumer.noteLiveSetAnswered(); verdict == taskLiveSetRecoveryEdge {
		m.logf("session-controller: task catalog live-set ANSWERABLE AGAIN ws=%q session=%s after consecutive_failures=%d — the degraded window that opened on the crossing is closed",
			d.workspace, d.sessionID, failures)
	} else if verdict == taskLiveSetAnsweredAfterBlips {
		m.logf("session-controller: task catalog live-set answered ws=%q session=%s after consecutive_failures=%d (never reached the degrade threshold)",
			d.workspace, d.sessionID, failures)
	}
	liveSet := make(map[string]struct{}, len(live))
	for _, id := range live {
		liveSet[id] = struct{}{}
	}
	var phantom []string
	for _, id := range candidates {
		if _, running := liveSet[id]; !running {
			phantom = append(phantom, id)
		}
	}
	if len(phantom) == 0 {
		// A LONG-RUNNING TASK IS NOT A PHANTOM, however long it runs. It is in
		// the shim's list, so it stays, and the sweep will ask again next time.
		m.logf("session-controller: task catalog reconciled ws=%q session=%s open_tasks=%d live_tasks=%d — every open entry is in the shim's live set; nothing is closed",
			d.workspace, d.sessionID, len(candidates), len(live))
		return nil
	}
	m.warnf("session-controller: task catalog PHANTOM ws=%q session=%s phantom_tasks=%v live_tasks=%v — the shim, which owns the live set, is not running these tasks, and the catalog has been rendering them as work in flight. They are being retired",
		d.workspace, d.sessionID, phantom, live)
	return d.consumer.closeOpenTasks(phantom, "shim_live_set")
}
