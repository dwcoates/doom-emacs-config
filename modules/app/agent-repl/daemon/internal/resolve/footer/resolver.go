package footer

import (
	"time"

	"fmt"
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
	"claude-repld/internal/resolve/ladder"
	"claude-repld/internal/resolve/turnfault"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/vocab"
)

// resolver is the footer resolver. One instance serves every workspace; each
// workspace owns one accumulation and one topic.
type resolver struct {
	log  dlog.Surfaces
	opts options

	mu sync.Mutex
	// daemonFaults are the DAEMON-scoped standing faults: they belong to no
	// workspace and so stand on every workspace's strip, because it is every
	// workspace that is owed the service the daemon cannot give.
	daemonFaults []Fault
	// deploy is the DEPLOY'S PROGRESS, resolver-wide like the daemon-scoped
	// faults: a deploy moves what serves every workspace, so its line stands
	// on every strip. Nil when no deploy is in flight. See update.go.
	deploy *deployState
	states map[ids.WorkspaceID]*wsState
	topics map[ids.WorkspaceID]*publish.Topic[*frontendv1.FooterView]
}

// newResolver builds the resolver with the injectable knobs resolved.
func newResolver(colors vocab.RenderColors, log dlog.Surfaces, opts ...Option) (*resolver, error) {
	if log == nil {
		return nil, fmt.Errorf("footer resolver needs log surfaces")
	}
	if err := colors.AssertFooterStatusArms(statusArms); err != nil {
		return nil, fmt.Errorf("footer resolver refuses to serve an unpainted state: %w", err)
	}
	o := options{
		clock:           SystemClock{},
		dwell:           DefaultMomentaryDwell,
		alarmTokens:     DefaultTokenAlarmThreshold,
		rateNewsworth:   DefaultRateLimitNewsworthyThreshold,
		transientWindow: DefaultTransientWindow,
	}
	for _, apply := range opts {
		apply(&o)
	}
	if o.clock == nil {
		return nil, fmt.Errorf("footer resolver needs a clock")
	}
	if o.transientWindow <= 0 {
		return nil, fmt.Errorf("footer resolver needs a positive transient window, got %s", o.transientWindow)
	}
	return &resolver{
		log:    log,
		opts:   o,
		states: map[ids.WorkspaceID]*wsState{},
		topics: map[ids.WorkspaceID]*publish.Topic[*frontendv1.FooterView]{},
	}, nil
}

// Prime publishes the workspace's current footer view at registration.
//
// THE FOOTER PER-WORKSPACE TOPIC MUST RE-PRIME ON RECONNECT, UNLIKE THE GLOBAL
// ROSTER. The roster topic is editor-global and always holds a current value,
// so a reconnecting subscriber replays it at once; the footer topic is
// per-workspace and is empty after a daemon restart rebuilds the resolver. An
// idle session produces no fresh live edge to drive a first publish, so without
// this prime serveTopic and the adoption Republish would have nothing to hand a
// reconnecting subscriber and the footer would stay blank. Registration is the
// one edge every workspace passes through on a reconnect, so priming here keeps
// the topic current — mirroring the topbar's SetNaming prime.
//
// It renders from the ACCUMULATED state, not a fresh one, and publishes through
// the same single site every other fact does. The render is always complete
// (the status bottoms out at idle, the tokens cell and every panel are always
// populated), so the completeness contract holds even for a workspace with no
// session fact yet. On a live daemon whose footer already stands, the prime
// renders the same view and the topic's value dedup drops it, so it never
// regresses a live footer to idle.
func (r *resolver) Prime(ws ids.WorkspaceID) {
	r.mutate(ws, "daemon.footer.prime",
		"the footer primed the workspace's view at registration so a reconnecting subscriber replays it",
		nil, func(*wsState) {})
}

// Topic is the workspace's footer publication. It exists before the first view
// does: a subscriber that arrives early receives the first view ever published
// rather than an empty one.
func (r *resolver) Topic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.FooterView] {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.topicLocked(ws)
}

// topicLocked resolves the workspace's topic under the resolver's lock.
func (r *resolver) topicLocked(ws ids.WorkspaceID) *publish.Topic[*frontendv1.FooterView] {
	t, ok := r.topics[ws]
	if !ok {
		// LATEST-ONLY: every view supersedes the last, and the activity cell
		// republishes per line of streamed reasoning and prose, so a slow
		// subscriber is handed the newest view rather than a backlog.
		t = publish.NewLatestOnly[*frontendv1.FooterView]()
		r.topics[ws] = t
	}
	return t
}

// SetWorkspaceDir binds the workspace's directory so its records reach that
// workspace's own sink.
func (r *resolver) SetWorkspaceDir(ws ids.WorkspaceID, dir string) error {
	log, err := r.log.Workspace(dir)
	if err != nil {
		return fmt.Errorf("bind footer resolver to workspace %s at %q: %w", ws, dir, err)
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.stateLocked(ws)
	s.dir = dir
	s.log = log.With(dlog.Context{"workspace_id": string(ws)})
	s.log.Debug("daemon.footer.bind", "the footer resolver bound a workspace to its log sink",
		dlog.Context{"workspace_dir": dir})
	return nil
}

// stateLocked resolves the workspace's accumulation under the lock.
func (r *resolver) stateLocked(ws ids.WorkspaceID) *wsState {
	s, ok := r.states[ws]
	if !ok {
		s = newWSState()
		s.id = ws
		r.states[ws] = s
	}
	return s
}

// RetryStanding reports a standing API retry holding the running turn
// (retryBlocks). A workspace the footer has never seen has none.
func (r *resolver) RetryStanding(ws ids.WorkspaceID) bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.states[ws]
	return ok && s.retryBlocks()
}

// logOf answers the workspace's logger. An unbound workspace is an invariant
// violation, recorded as one on the global sink — the only sink that exists
// before a directory is known — rather than silently dropped.
func (r *resolver) logOf(ws ids.WorkspaceID, s *wsState) dlog.Logger {
	if s.log != nil {
		return s.log
	}
	log := r.log.Global().With(dlog.Context{
		"workspace_id":        string(ws),
		"invariant_violation": "footer resolver frame for a workspace with no bound directory",
		"remediation":         "call SetWorkspaceDir at registration",
	})
	// THE VIOLATION IS AN ERROR, stated ONCE per workspace. Every record after
	// it keeps its own level and carries the violation as context; before
	// this, the only trace was that context on records logged at INFO and
	// DEBUG, so a level sweep never saw the invariant break.
	if !s.unboundReported {
		s.unboundReported = true
		log.Error("daemon.footer.unbound_workspace", "a footer record arrived for a workspace whose directory was never bound", nil)
	}
	return log
}

// mutate runs one accumulation change under the lock and republishes the whole
// view. Every sink method and every setter goes through it, so there is
// exactly one publication site.
func (r *resolver) mutate(ws ids.WorkspaceID, operation, message string, ctx dlog.Context, apply func(*wsState)) {
	r.mu.Lock()
	s := r.stateLocked(ws)
	s.seen = true
	blockBefore := s.vendorBlock()
	apply(s)
	view := r.render(ws, s)
	arm, armChanged, previousArm := s.observeArm(view)
	line, lineChanged, previousLine := s.observeLine(view)
	jumps := s.drainJumpNotes()
	log := r.logOf(ws, s)
	served, servedNow := servedEdge(ws, blockBefore, s, log)
	// PUBLISHED UNDER THE LOCK THAT RENDERED IT, so views reach the topic in
	// the order the changes were made. Published after the unlock, a change
	// rendered first could be published second, and a stale view overwrote a
	// newer one on the wire (2026-10-02: the roster walked submitting, ready,
	// thinking for an accepted prompt). Topic.Publish only enqueues, so it
	// never waits on a subscriber.
	r.topicLocked(ws).Publish(view)
	r.mu.Unlock()

	if ctx == nil {
		ctx = dlog.Context{}
	}
	log.Debug(operation, message, ctx)
	logArmChange(log, operation, arm, armChanged, previousArm)
	logLineChange(log, operation, arm, line, lineChanged, previousLine)
	logJumpNotes(log, jumps)
	if servedNow {
		r.tellVendorServes(operation, []vendorServed{served})
	}
}

// logArmChange records the PUBLISHED status arm whenever it changes, and only
// then. It is what makes a later disagreement between the strip and the roster
// diagnosable from the log alone: the arm, the arm it replaced, and the
// operation that moved it.
func logArmChange(log dlog.Logger, operation, arm string, changed bool, previous string) {
	if !changed {
		return
	}
	log.Info("daemon.footer.status_arm_changed", "the footer published a new status arm",
		dlog.Context{"arm": arm, "previous_arm": previous, "cause": operation})
}

// logLineChange records the PUBLISHED activity line whenever it changes — set,
// replaced or cleared — and only then. A line that stands longer than its act
// is diagnosable from the log alone only if the log says when it began
// standing, what put it there, and when (and by what) it went.
//
// A TRANSIENT LINE IS RECORDED AT DEBUG: it moves with every line of streamed
// reasoning and prose, ends on the client's clock, and its raise is already
// recorded (daemon.footer.transient_raised). Every other change — a salient
// line standing or going, the cell falling back to the enduring line — is
// INFO.
func logLineChange(log dlog.Logger, operation, arm string, line activityLine, changed bool, previous activityLine) {
	if !changed {
		return
	}
	record := log.Info
	if line.tier == "transient" {
		record = log.Debug
	}
	record("daemon.footer.activity_line_changed", "the footer published a new activity line",
		dlog.Context{
			"arm":           arm,
			"kind":          line.name(),
			"text":          line.text,
			"previous_kind": previous.name(),
			"previous_text": previous.text,
			"cause":         operation,
		})
}

// mutateAll applies a resolver-WIDE change and republishes every workspace
// that has a footer, because a daemon-scoped fact stands on all of them. A
// workspace that has observed nothing yet is left alone: its footer is not
// published at all until its first fact, and a daemon fault is not the fact
// that makes a workspace's strip exist.
func (r *resolver) mutateAll(operation, message string, ctx dlog.Context, apply func(*wsState), global func()) {
	type publication struct {
		topic        *publish.Topic[*frontendv1.FooterView]
		view         *frontendv1.FooterView
		log          dlog.Logger
		arm          string
		armChanged   bool
		previousArm  string
		line         activityLine
		lineChanged  bool
		previousLine activityLine
		jumps        []dlog.Context
	}
	r.mu.Lock()
	global()
	out := make([]publication, 0, len(r.states))
	var served []vendorServed
	for ws, s := range r.states {
		if !s.seen {
			continue
		}
		blockBefore := s.vendorBlock()
		apply(s)
		if edge, ok := servedEdge(ws, blockBefore, s, r.logOf(ws, s)); ok {
			served = append(served, edge)
		}
		view := r.render(ws, s)
		arm, armChanged, previousArm := s.observeArm(view)
		line, lineChanged, previousLine := s.observeLine(view)
		out = append(out, publication{
			topic: r.topicLocked(ws), view: view, log: r.logOf(ws, s),
			arm: arm, armChanged: armChanged, previousArm: previousArm,
			line: line, lineChanged: lineChanged, previousLine: previousLine,
			jumps: s.drainJumpNotes(),
		})
	}
	// Published under the lock that rendered them, for the reason mutate
	// states.
	for _, p := range out {
		p.topic.Publish(p.view)
	}
	r.mu.Unlock()

	if ctx == nil {
		ctx = dlog.Context{}
	}
	r.log.Global().Debug(operation, message, ctx)
	for _, p := range out {
		p.log.Debug(operation, message, ctx)
		logArmChange(p.log, operation, p.arm, p.armChanged, p.previousArm)
		logLineChange(p.log, operation, p.arm, p.line, p.lineChanged, p.previousLine)
		logJumpNotes(p.log, p.jumps)
	}
	r.tellVendorServes(operation, served)
}

// render builds the whole view from the accumulation. Nothing partial is ever
// built: every element resolves from state alone.
func (r *resolver) render(ws ids.WorkspaceID, s *wsState) *frontendv1.FooterView {
	return &frontendv1.FooterView{
		Strip: &frontendv1.FooterStrip{
			Status:   r.status(s, r.logOf(ws, s)),
			Clock:    r.clockCell(s),
			Tokens:   s.tok.cell(s.turn != nil),
			LiveWork: r.chips(s),
		},
		Expanded: r.expanded(ws, s),
		Focus:    focusView(s.focus),
	}
}

// clockCell renders the turn clock. UNSET is no turn in flight, which is what
// makes the cell render idle rather than a frozen duration.
func (r *resolver) clockCell(s *wsState) *frontendv1.FooterClock {
	out := &frontendv1.FooterClock{}
	if s.turn != nil {
		at := epochMs(s.turn.At)
		out.TurnStartedAtMs = &at
	}
	return out
}

// ---- daemon-fact setters --------------------------------------------------

// OnTurnOpened is the watcher handing over the turn-open edge. It raises
// `thinking submitting` and starts the strip's clock. The ACT is not on this
// edge — only the daemon's own caller knows a turn carries a /clear or a
// compaction — so a turn already installed by SetTurn keeps its act rather
// than being demoted to an ordinary prompt.
//
// A TURN ALREADY INSTALLED BY SetTurn KEEPS ITS START TIME TOO. The submitting
// phase begins the instant the daemon accepts the prompt, before the shim is
// asked; the turn-open edge that follows is the same turn, not a new one, so it
// must not reset the strip's clock to the later turn-open instant. Only a turn
// the edge is the FIRST to hear of (no SetTurn ran) starts the clock here.
//
// AND IT KEEPS EVERYTHING THE TURN HAS ALREADY SAID. The shim can put the
// turn's frames on the agent stream before StartTurn's answer is back, so the
// frames of an installed turn may already have been folded in when this edge
// arrives. Re-running the turn start here wiped them: the usage the first API
// response carried and the ledger that files it (so an ordinary prose turn
// reconciled `incomplete` — TestFooterTokensCellVerdictIsComplete... lost a
// full integration run to it), the activity seen, a permission block. SetTurn
// is the ONE start of a daemon-delivered turn, and it is published before the
// shim is asked, so it precedes every frame of the turn by construction. The
// edge re-takes only the context baseline, which is what it always owned.
func (r *resolver) OnTurnOpened(ws ids.WorkspaceID, turn ids.TurnID) {
	r.mutate(ws, "daemon.footer.on_turn_opened", "the footer took the turn-open edge",
		dlog.Context{"turn_id": string(turn)}, func(s *wsState) {
			if s.turn != nil {
				s.tok.ctx = s.tok.ctx.rebased()
				r.logOf(ws, s).Debug("daemon.footer.on_turn_opened",
					"the turn was already installed at acceptance; the edge re-took the context baseline and kept what the turn had said",
					dlog.Context{"turn_id": string(turn), "saw_activity": s.sawActivity})
				return
			}
			r.applyTurnStarted(s, &TurnStarted{At: r.opts.clock.Now(), Act: ActPrompt})
		})
}

// SetTurn installs the accepted turn. A delivered prompt is a next prompt, so
// it ends the agent's push notification. The delivery draws no activity line:
// the `submitting` substatus states the step (owner ruling, 2026-10-06).
func (r *resolver) SetTurn(ws ids.WorkspaceID, turn *TurnStarted) {
	r.mutate(ws, "daemon.footer.set_turn", "the footer took the accepted turn",
		dlog.Context{"in_flight": turn != nil}, func(s *wsState) {
			r.applyTurnStarted(s, turn)
			if turn != nil {
				r.endNotification(ws, s, "daemon.footer.set_turn")
			}
		})
}

// OnSubmission takes one move of a prompt's delivery and ends the agent's push
// notification: the prompt is the next prompt the notification stood until.
// The move draws no activity line (owner ruling, 2026-10-06); it is recorded.
func (r *resolver) OnSubmission(ws ids.WorkspaceID, sub Submission) {
	stage, declared := stageName(sub.Stage)
	r.mutate(ws, "daemon.footer.on_submission", "the footer took a move of a prompt's delivery",
		dlog.Context{"stage": stage, "position": sub.Position, "queued": sub.Queued}, func(s *wsState) {
			r.endNotification(ws, s, "daemon.footer.on_submission")
			if !declared {
				r.logOf(ws, s).Error("daemon.footer.on_submission", "a submission names no stage the footer declares",
					dlog.Context{"stage": int(sub.Stage), "invariant_violation": "every submission names one of the declared stages"})
			}
		})
}

// applyTurnStarted installs (or clears) the in-flight turn on an accumulation.
// It is shared by the daemon-fact setter and the watcher's turn-open edge so
// the two can never drift.
// OnTurnRunningAtAttach stands the turn an adopted shim was already running
// when its watcher attached. See sessionwatcher.FooterSink. A turn that already
// stands is this daemon's own and stays as it is. The turn is running, so it is
// past its submission and delivered; its clock counts from its row's start, or
// from now when the daemon holds no row for it.
func (r *resolver) OnTurnRunningAtAttach(ws ids.WorkspaceID, turn ids.TurnID, startedAt *time.Time) {
	r.mutate(ws, "daemon.footer.on_turn_running_at_attach", "the footer took a turn the adopted shim is running",
		dlog.Context{"turn_id": string(turn), "row_known": startedAt != nil}, func(s *wsState) {
			if s.turn != nil {
				r.logOf(ws, s).Debug("daemon.footer.on_turn_running_at_attach",
					"a turn already stands; the adopted one is this daemon's own", dlog.Context{"turn_id": string(turn)})
				return
			}
			at := r.opts.clock.Now()
			if startedAt != nil {
				at = *startedAt
			}
			r.applyTurnStarted(s, &TurnStarted{At: at, Act: ActPrompt})
			s.sawActivity = true
		})
}

func (r *resolver) applyTurnStarted(s *wsState, turn *TurnStarted) {
	s.turn = turn
	if turn == nil {
		return
	}
	s.turnEverRan = true
	// A NEW TURN ENDS THE LAST ONE'S FAULT (owner ruling, 2026-10-06).
	s.turnFault = nil
	s.pendingEnding = nil
	s.turnRefused = false
	s.sawActivity = false
	s.blocked = nil
	s.queryDied = nil
	s.interrupted = nil
	// COMPACTING IS OR-ED IN, NEVER ASSIGNED. `SessionUpdate.compacting` is the
	// vendor's own start signal and it lands BEFORE the turn-open edge it
	// belongs to, so assigning here would wipe a compaction the vendor had
	// already announced. The flag is cleared by the end signals — the
	// ContextCut that ends the compaction, and the turn's terminal — never by
	// a turn opening.
	s.compacting = s.compacting || turn.Act == ActCompact
	// A RETRY OF THE LAST TURN'S CALL IS OVER: the turn it held has ended.
	s.retrying = nil
	r.startTurnMotion(s)
	s.tok.reset(liveDetachedAgents(s))
	r.cancelMomentary(s)
}

// liveDetachedAgents is the set of created-agent ids for the subagents that are
// live AND detached right now — the runs still burning tokens in the background
// as this turn opens. It is what the token accounting carries across the turn
// reset so a detached agent's spend stays in the tokens panel while it runs;
// an in-turn subagent (no work handle) is the turn's own progress and resets
// with it. The key is the created-agent id, because that is the id an agent's
// own-book usage frames arrive under (chips.go OnActivity).
func liveDetachedAgents(s *wsState) map[string]struct{} {
	keep := make(map[string]struct{}, len(s.agents))
	for _, row := range s.agents {
		if row.work != "" && row.createdAgent != "" {
			keep[row.createdAgent] = struct{}{}
		}
	}
	return keep
}

// SetMerge installs the merge facts the footer draws.
//
// A NEW TESTING ROUND PUBLISHES THE MERGE TESTS PANEL AS THE FOCUS, under the
// next generation, through the same edge a detached launch uses: the owner's
// "that section opens and is selected when testing begins". The panel empties
// when testing ends, which unsets the chip, so the section falls back to
// whatever else stands.
func (r *resolver) SetMerge(ws ids.WorkspaceID, facts MergeFacts) {
	var minted *mintedFocus
	r.mutate(ws, "daemon.footer.set_merge", "the footer took the merge facts",
		dlog.Context{"state": facts.State, "step": string(facts.Step), "failed_area": string(facts.FailedArea), "tests_round": facts.TestsRound},
		func(s *wsState) {
			minted = mintMergeTestsFocus(s, facts.TestsRound)
			s.merge = facts
		})
	if minted != nil {
		r.workspaceLog(ws).Info("daemon.footer.focus_minted",
			"the merge began testing; the merge tests panel is the expanded footer's focus",
			dlog.Context{"panel": minted.panel.String(), "generation": minted.generation, "trigger": minted.trigger})
	}
}

// SetParked installs, or lifts, the idle sweep's park.
func (r *resolver) SetParked(ws ids.WorkspaceID, parked bool) {
	r.mutate(ws, "daemon.footer.set_parked", "the footer took the idle sweep's park",
		dlog.Context{"parked": parked}, func(s *wsState) { s.parked = parked })
}

// SetStateUnreported installs, or lifts, the fact that a shim taken back after
// a failed handover has not re-reported its session state. A session start
// lifts it too (OnSessionStarted), which is the re-report it waits for.
func (r *resolver) SetStateUnreported(ws ids.WorkspaceID, unreported bool) {
	r.mutate(ws, "daemon.footer.set_state_unreported", "the footer took whether the shim's session state is unreported",
		dlog.Context{"unreported": unreported}, func(s *wsState) { s.stateUnreported = unreported })
}

// SetClosing installs a close refusal.
func (r *resolver) SetClosing(ws ids.WorkspaceID, blocked *CloseBlocked) {
	ctx := dlog.Context{"blocked": blocked != nil}
	if blocked != nil {
		ctx["reason"] = blocked.Reason
	}
	r.mutate(ws, "daemon.footer.set_closing", "the footer took the close refusal", ctx,
		func(s *wsState) {
			s.closing = blocked
			// Each refusal is a line of its own, standing from when it was
			// refused.
			if blocked != nil {
				s.closingAt = r.opts.clock.Now()
			}
		})
}

// SetStartFailed installs, or clears, the standing bring-up failure.
func (r *resolver) SetStartFailed(ws ids.WorkspaceID, failure *StartFailed) {
	ctx := dlog.Context{"standing": failure != nil}
	if failure != nil {
		ctx["detail"] = failure.Detail
	}
	r.mutate(ws, "daemon.footer.set_start_failed", "the footer took the bring-up failure", ctx,
		func(s *wsState) {
			if failure == nil {
				s.startFailed = nil
				return
			}
			s.startFailed = &startFailedState{detail: failure.Detail, at: r.opts.clock.Now()}
		})
}

// SetColdGate installs the standing cold gate.
func (r *resolver) SetColdGate(ws ids.WorkspaceID, gate ColdGate) {
	r.mutate(ws, "daemon.footer.set_cold_gate", "the footer took the cold gate",
		dlog.Context{"standing": gate.Standing}, func(s *wsState) {
			// The gate's line stands from when the gate OPENED; restating a
			// standing gate keeps that instant.
			if gate.Standing && !s.coldGate.Standing {
				s.coldGateAt = r.opts.clock.Now()
			}
			s.coldGate = gate
		})
}

// SetColdGateAnswer installs the gate answer in flight, nil to clear it.
func (r *resolver) SetColdGateAnswer(ws ids.WorkspaceID, answer *ColdGateAnswer) {
	r.mutate(ws, "daemon.footer.set_cold_gate_answer", "the footer took the cold gate's answer",
		dlog.Context{"in_flight": answer != nil, "choice": choiceOf(answer), "line": lineOf(answer)},
		func(s *wsState) {
			s.coldAnswer = answer
			// THE ANSWER'S OWN LINE IS THE COMPACTION LINE while it stands, so
			// the gate's compaction and the vendor's read identically (owner
			// ruling, 2026-09-14). Clearing the answer clears the line with it:
			// a stale progress sentence outliving its act is the same defect
			// as no sentence at all.
			if answer == nil {
				r.endCompaction(ws, s, "daemon.footer.set_cold_gate_answer")
				return
			}
			r.standCompaction(ws, s, answer.Text, "daemon.footer.set_cold_gate_answer")
			// THE ANSWER'S COMPACTION CONCLUDING IS AN OUTCOME like the
			// vendor's own: announced, and the context-budget line settled by
			// it, beneath the answer's line that its verb ends.
			if answer.Progress != nil && concludedPhase(answer.Progress.GetPhase()) {
				r.concludeCompaction(ws, s, answer.Progress)
			}
		})
}

// choiceOf names the answered remediation for the record, "none" when no
// answer is in flight.
func choiceOf(answer *ColdGateAnswer) string {
	if answer == nil {
		return "none"
	}
	return answer.Choice
}

// lineOf is the answer's composed line for the record, empty when none stands.
func lineOf(answer *ColdGateAnswer) string {
	if answer == nil {
		return ""
	}
	return answer.Text
}

// SetInterrupting fires waiting·interrupting the moment an interrupt registers.
func (r *resolver) SetInterrupting(ws ids.WorkspaceID, on bool) {
	r.mutate(ws, "daemon.footer.set_interrupting", "the footer took the interrupt registration",
		dlog.Context{"interrupting": on}, func(s *wsState) {
			if on && !s.interrupting {
				s.interruptingAt = r.opts.clock.Now()
			}
			s.interrupting = on
		})
}

// ---- the R1 dwell ---------------------------------------------------------

// armMomentary schedules the one-shot successor push that retires a momentary
// status. Nothing on the wire ticks: the dwell is the DAEMON's timer and its
// only effect is one more whole-view publication.
func (r *resolver) armMomentary(ws ids.WorkspaceID, s *wsState) {
	r.cancelMomentary(s)
	s.momentary = r.opts.clock.AfterFunc(r.opts.dwell, func() { r.retireMomentary(ws) })
}

// cancelMomentary stops an in-flight dwell whose status was superseded.
func (r *resolver) cancelMomentary(s *wsState) {
	if s.momentary != nil {
		s.momentary.Stop()
		s.momentary = nil
	}
}

// retireMomentary clears the momentary statuses and republishes the successor.
func (r *resolver) retireMomentary(ws ids.WorkspaceID) {
	r.mutate(ws, "daemon.footer.retire_momentary",
		"the R1 dwell elapsed and the footer published the momentary status's successor",
		nil, func(s *wsState) {
			s.interrupted = nil
			s.loading = nil
			s.momentary = nil
		})
}

// ---- FooterSink -----------------------------------------------------------

// OnContextBudgetWarning stands the vendor's context-budget warning as the
// salient `context_budget` line (owner ruling, 2026-09-30). It is an
// AGENT-PLANE fact — a page line of the agent's book — and it stands until a
// cut shrinks the context; a SUBAGENT's warning also ends with that subagent's
// run, whose context it was about.
func (r *resolver) OnContextBudgetWarning(ws ids.WorkspaceID, agent *conversationv1.AgentId, warning *conversationv1.ContextBudgetWarning) {
	if warning == nil {
		return
	}
	r.mutate(ws, "daemon.footer.on_context_budget_warning", "the footer took a context-budget warning",
		dlog.Context{"agent_id": agent.GetValue()}, func(s *wsState) {
			owner := agent.GetValue()
			if owner == s.mainAgent {
				owner = ""
			}
			r.standContextBudget(ws, s, owner, warning.GetText())
		})
}

// OnLink is the connectivity change the footer reflects.
func (r *resolver) OnLink(ws ids.WorkspaceID, link sessionwatcher.LinkState) {
	r.mutate(ws, "daemon.footer.on_link", "the footer took a link state",
		dlog.Context{"link": linkName(link)}, func(s *wsState) {
			s.link = link
			s.linkSeen = true
			if link == shimclient.LinkConnected {
				s.everConnected = true
				// THE SUCCESSFUL LINK EDGE CLEARS THE BRING-UP FAILURE. A
				// session that is serving has come up, so the line explaining
				// why it would not is spent, and the status it hung under is
				// gone with it.
				s.startFailed = nil
			}
			// THE REVIVAL ENDS THE PARK. The session watcher latches a dead
			// link ("a later transition is a consequence of the death",
			// sessionwatcher/watcher.go) and publishes nothing more on it, so
			// any link state arriving after the park belongs to the shim the
			// reviving prompt spawned. Clearing it here — rather than only on
			// LinkConnected — is what makes a revival whose spawn then DIES
			// read `dead` again instead of staying masked as a park.
			s.parked = false
			// A DEAD SHIM RUNS NO TURN. The process that ran the vendor
			// query is gone, so whatever turn the strip was drawing ended
			// with it and no terminal will ever arrive for it. Kept, it drew
			// `thinking` over the session brought back in its place.
			if link == shimclient.LinkDead && s.turn != nil {
				s.turn = nil
			}
		})
}

// OnSessionStarted lifts a standing vendor or account block and a dead-query
// line: a session that has (re)started is one the vendor served, which is the
// roster's rule for its vendor_blocked on the same event, so the strip and
// the dot lift together.
//
// A START NAMING ANOTHER VENDOR SESSION IS A SWITCH: the context the
// context-budget line warned about is not this session's, so the line ends. A
// restart of the same session keeps it, because its context is unchanged.
func (r *resolver) OnSessionStarted(ws ids.WorkspaceID, started *conversationv1.SessionStarted) {
	r.mutate(ws, "daemon.footer.on_session_started", "the footer took a session start",
		dlog.Context{"vendor_session_id": started.GetVendorSessionId()}, func(s *wsState) {
			s.sessionStarted = true
			s.blocked = nil
			// A STARTED SESSION HAS A LIVE QUERY: the restart the death asked
			// for has landed, so its line comes down. An adopted shim whose
			// query had died re-announces the death after its start, which
			// raises the line again.
			s.queryDied = nil
			s.stateUnreported = false
			if id := started.GetVendorSessionId(); id != "" {
				if s.vendorSession != "" && s.vendorSession != id {
					r.endContextBudget(ws, s, "daemon.footer.on_session_started.switch")
				}
				s.vendorSession = id
			}
		})
}

// OnSessionUpdate carries the session-scoped facts the footer reflects.
func (r *resolver) OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	if update == nil {
		return
	}
	arm, apply := r.sessionArm(ws, update)
	r.mutate(ws, "daemon.footer.on_session_update", "the footer took a session update",
		dlog.Context{"arm": arm}, apply)
}

// deadQueryLine is the strip's sentence for a vendor query that died and has
// not been restarted, worded as footer.proto's FooterStatusActivityQueryDied
// arm words it. The daemon replaces the shim the moment the death is told
// (the lifecycle sink's OnQueryDied), and the session's next start lifts the
// line.
const deadQueryLine = "vendor query died — restarting the session"

// sessionArm names the update's arm and returns what it changes. Every arm has
// a branch, including the ones the footer deliberately draws nothing from.
func (r *resolver) sessionArm(ws ids.WorkspaceID, update *conversationv1.SessionUpdate) (string, func(*wsState)) {
	switch u := update.GetUpdate().(type) {
	case *conversationv1.SessionUpdate_QueryDied:
		return "query_died", func(s *wsState) {
			r.logSessionArm(ws, s, "query_died")
			now := r.opts.clock.Now()
			s.queryDied = &standing{text: deadQueryLine, at: now}
			// A DEAD QUERY IS AGENT-REPL'S FAULT, NOT A BLOCK (owner rulings,
			// 2026-09-28 and 2026-10-06): the daemon restarts it, and nothing
			// about the vendor or the account refuses the session. A turn the
			// death cut is recorded as dying of it, and its close raises the
			// `turn_died` fault; with no turn in flight, the last turn's end
			// stands as it was, under the dead-query line.
			if s.turn != nil {
				s.recordQueryDeath(u.QueryDied)
			}
			s.turn = nil
			// NO RETRY SURVIVES THE QUERY EITHER.
			s.retrying = nil
			// NO COMPACTION SURVIVES THE QUERY IT RAN IN.
			s.compacting = false
			r.endCompaction(ws, s, "daemon.footer.on_session_update.query_died")
			s.tok.settled = true
		}
	case *conversationv1.SessionUpdate_AccountUsage:
		// THE FIGURES' SOURCE. The sampled account usage carries both
		// windows' utilization and reset, complete from the first sample;
		// the rate-limit event carries the verdict.
		return "account_usage", func(s *wsState) {
			r.logSessionArm(ws, s, "account_usage")
			r.observeAccountUsage(ws, s, u.AccountUsage)
		}
	case *conversationv1.SessionUpdate_RateLimitStatus:
		return "rate_limit_status", func(s *wsState) {
			r.logSessionArm(ws, s, "rate_limit_status")
			r.observeRateLimitStatus(s, u.RateLimitStatus)
			// A REJECTED VERDICT IS THE ACCOUNT REFUSING THE SESSION, and any
			// other verdict lifts the block — the same rule, on the same event,
			// as the roster's vendor_blocked (ladder.RateLimitBlocks).
			if ladder.RateLimitBlocks(u.RateLimitStatus) {
				if s.blocked == nil {
					s.blocked = &blockedState{kind: blockedUsageLimit, at: r.opts.clock.Now()}
				}
			} else {
				s.blocked = nil
			}
		}
	case *conversationv1.SessionUpdate_Compacting:
		return "compacting", func(s *wsState) {
			r.logSessionArm(ws, s, "compacting")
			// A START SIGNAL OUTRUN BY ITS OWN CUT stands nothing: the start
			// rides WatchSession and the cut WatchAgent, with no order between
			// them, so the compaction it announces is already over.
			if id := u.Compacting.GetCompaction().GetValue(); compactionConcluded(s, id) {
				r.logOf(ws, s).Info("daemon.footer.compacting_after_its_cut",
					"a compaction's start signal arrived after the cut that ended it; it stands nothing",
					dlog.Context{"compaction_id": id})
				return
			}
			s.compacting = true
			// THE VENDOR'S COMPACTION GETS A LINE TOO. Presence is the whole
			// fact this arm carries — no phase, no figure — so the line says
			// exactly that and nothing it does not know (owner ruling,
			// 2026-09-14: both compactions read the same).
			r.standCompaction(ws, s, vendorCompactionLine, "daemon.footer.on_session_update.compacting")
		}
	case *conversationv1.SessionUpdate_CompactionProgress:
		return "compaction_progress", func(s *wsState) {
			r.logSessionArm(ws, s, "compaction_progress")
			progress := u.CompactionProgress
			// A CONCLUDED PHASE IS THE END OF THE COMPACTION, not a
			// compaction still running: `failed` cut nothing and `started`
			// is the resumed session up. Every other phase is one still in
			// flight, and stands as the salient line.
			if concludedPhase(progress.GetPhase()) {
				s.compacting = false
				r.concludeCompaction(ws, s, progress)
				return
			}
			s.compacting = true
			r.standCompaction(ws, s, CompactionLine(progress), "daemon.footer.on_session_update.compaction_progress")
		}
	case *conversationv1.SessionUpdate_Diagnostics:
		return "diagnostics", func(s *wsState) {
			r.logSessionArm(ws, s, "diagnostics")
			s.degraded = anyWindowOpen(u.Diagnostics)
		}
	case *conversationv1.SessionUpdate_ModelChanged,
		*conversationv1.SessionUpdate_PermissionModeChanged,
		*conversationv1.SessionUpdate_McpServer:
		// A SESSION SETTING CHANGED: announced as the transient
		// `session_change` line, composed here.
		arm := sessionChangeArm(update)
		return arm, func(s *wsState) {
			r.logSessionArm(ws, s, arm)
			if text, ok := sessionChangeText(update); ok {
				r.raiseSessionChange(ws, s, text)
			}
		}
	case *conversationv1.SessionUpdate_NetworkResumeWaits:
		return "network_resume_waits", func(s *wsState) {
			r.logSessionArm(ws, s, "network_resume_waits")
			r.observeResumeWaits(ws, s, u.NetworkResumeWaits)
		}
	case *conversationv1.SessionUpdate_NetworkResumeOutcome:
		return "network_resume_outcome", func(s *wsState) {
			r.logSessionArm(ws, s, "network_resume_outcome")
			r.observeResumeOutcome(ws, s, u.NetworkResumeOutcome)
		}
	case *conversationv1.SessionUpdate_IdentityRotated:
		return "identity_rotated", func(s *wsState) { r.logSessionArm(ws, s, "identity_rotated") }
	case *conversationv1.SessionUpdate_FastMode:
		return "fast_mode", func(s *wsState) { r.logSessionArm(ws, s, "fast_mode") }
	case *conversationv1.SessionUpdate_ContextUsage:
		// THE CELL'S SOURCE. The same reading the topbar's context chip draws,
		// delivered to both by one route, so the chip and the cell's growth
		// are two drawings of one fact.
		return "context_usage", func(s *wsState) {
			r.logSessionArm(ws, s, "context_usage")
			r.observeContextUsage(ws, s, u.ContextUsage)
		}
	default:
		return "unset", func(s *wsState) { r.logSessionArm(ws, s, "unset") }
	}
}

// sessionChangeArm names a session-setting arm for the record.
func sessionChangeArm(update *conversationv1.SessionUpdate) string {
	switch update.GetUpdate().(type) {
	case *conversationv1.SessionUpdate_ModelChanged:
		return "model_changed"
	case *conversationv1.SessionUpdate_PermissionModeChanged:
		return "permission_mode_changed"
	default:
		return "mcp_server"
	}
}

// logSessionArm records the update arm selected for this workspace frame.
func (r *resolver) logSessionArm(ws ids.WorkspaceID, s *wsState, arm string) {
	r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer session-update arm",
		dlog.Context{"arm": arm})
}

// anyWindowOpen reports whether the diagnostics carry an open degraded window,
// which is what makes a serving link read as degraded.
func anyWindowOpen(d *conversationv1.SessionDiagnostics) bool {
	for _, w := range d.GetDegradedWindows() {
		if _, open := w.GetExtent().(*conversationv1.SessionDegradedWindow_Open); open {
			return true
		}
	}
	return false
}

// observeAccountUsage takes one usage sample and files BOTH windows' figures:
// five_hour is the session allowance, seven_day the weekly one. A sample that
// could read no figure (the unavailable arm) leaves the figures on hand
// standing, and a sample whose seven_day window the vendor omitted leaves the
// weekly allowance unfigured, which draws it absent rather than invented.
//
// The strip renders the figures LAST READ; an unreadable attempt leaves the
// figures standing. The strip no longer draws a "usage unread" line (owner
// ruling), but the unreadable outcome is still surfaced to the logs here so a
// read that keeps failing does not vanish silently.
func (r *resolver) observeAccountUsage(ws ids.WorkspaceID, s *wsState, usage *conversationv1.SessionAccountUsage) {
	if usage == nil {
		return
	}
	available, ok := usage.GetOutcome().(*conversationv1.SessionAccountUsage_Available)
	if !ok {
		r.logUnreadableSample(ws, s, usage)
		return
	}
	at := usage.GetObservedAtMs()
	moved := false
	if five := available.Available.GetFiveHour(); five != nil {
		moved = s.rate.session.observeSampledFigures(five.GetUtilizationPercent(), five.GetResetsAtMs(), at) || moved
	}
	if seven := available.Available.GetSevenDay(); seven != nil {
		moved = s.rate.weekly.observeSampledFigures(seven.GetUtilizationPercent(), seven.GetResetsAtMs(), at) || moved
	}
	if moved {
		now := r.opts.clock.Now()
		s.rate.at = now
	}
}

// logUnreadableSample surfaces a usage sample that read NO figure. It drives
// no strip cell any more, so this Debug breadcrumb is the one place a
// persistently failing read stays visible; it never clears the figures on
// hand (the caller returns without touching them).
func (r *resolver) logUnreadableSample(ws ids.WorkspaceID, s *wsState, usage *conversationv1.SessionAccountUsage) {
	unavailable, ok := usage.GetOutcome().(*conversationv1.SessionAccountUsage_Unavailable)
	if !ok {
		// No outcome arm at all: nothing was stated, so there is nothing to
		// report and nothing was read.
		return
	}
	fields := dlog.Context{"reason": unavailableReason(unavailable.Unavailable)}
	// THE SHIM'S OWN CAUSE RIDES THE RECORD. Since the strip stopped drawing an
	// unread caveat (owner ruling, fc4917be4) this breadcrumb is the ONLY place
	// a sampling failure stays visible, and the reason arm alone says only
	// "something failed" — the cause is the shim's account of what.
	if failure := unavailable.Unavailable.GetSamplingFailure(); failure != nil {
		fields["cause"] = failure.GetCause()
	}
	r.logOf(ws, s).Debug("daemon.footer.usage_sample_unreadable",
		"an account-usage sample read no figure; the figures on hand stand", fields)
}

// unavailableReason names an unavailable sample's reason arm for the logs. An
// unavailable arm whose own reason oneof is UNSET is still an unavailability,
// reported as such rather than dropped.
func unavailableReason(unavailable *conversationv1.SessionAccountUsageUnavailable) string {
	switch unavailable.GetReason().(type) {
	case *conversationv1.SessionAccountUsageUnavailable_ServiceUnavailable:
		return "service_unavailable"
	case *conversationv1.SessionAccountUsageUnavailable_WindowUnavailable:
		return "window_unavailable"
	case *conversationv1.SessionAccountUsageUnavailable_UtilizationUnavailable:
		return "utilization_unavailable"
	case *conversationv1.SessionAccountUsageUnavailable_SamplingFailure:
		return "sampling_failure"
	default:
		return "unspecified"
	}
}

// observeRateLimitStatus takes one rate-limit event and files it under the
// window it is about: five_hour is the session allowance; seven_day and its
// per-model and overage-included aliases are all the weekly one. The event is
// the VERDICT's only source, so filing it is what lets an allowance's status
// arm join. An event that carries a utilization also supplies the figure, and
// as the newest sighting to arrive it is the one drawn — see `fileFigures`,
// which states why arrival rather than a timestamp orders the two sources.
// The overage window is its own allowance cell and files exactly like the
// other two; a status naming no window is not filable at all.
func (r *resolver) observeRateLimitStatus(s *wsState, status *conversationv1.SessionRateLimitStatus) {
	if status == nil {
		return
	}
	var window *allowanceWindow
	switch status.GetRateLimitType().GetWindow().(type) {
	case *conversationv1.SessionRateLimitType_FiveHour:
		window = &s.rate.session
	case *conversationv1.SessionRateLimitType_SevenDay,
		*conversationv1.SessionRateLimitType_SevenDayOpus,
		*conversationv1.SessionRateLimitType_SevenDaySonnet,
		*conversationv1.SessionRateLimitType_SevenDayOverageIncluded:
		window = &s.rate.weekly
	case *conversationv1.SessionRateLimitType_Overage:
		window = &s.rate.overage
	default:
		return
	}
	window.verdict = status
	if status.UtilizationPercent != nil {
		window.observeEventFigures(status.GetUtilizationPercent(), status.GetResetsAtMs())
	}
	s.rate.at = r.opts.clock.Now()
}

// OnQuestion moves the footer to waiting.
func (r *resolver) OnQuestion(ws ids.WorkspaceID, agent *conversationv1.AgentId, q *conversationv1.AgentQuestion) {
	if q == nil {
		return
	}
	id := q.GetId().GetValue()
	switch res := q.GetResult().(type) {
	case *conversationv1.AgentQuestion_Start:
		r.mutate(ws, "daemon.footer.on_question", "the footer opened a question batch",
			dlog.Context{"question_id": id}, func(s *wsState) {
				batch, seen := s.questions[id]
				if !seen {
					s.questionOrder = append(s.questionOrder, id)
					batch.at = r.opts.clock.Now()
				}
				batch.text = questionLead(res.Start.GetBatch())
				s.questions[id] = batch
			})
	default:
		r.mutate(ws, "daemon.footer.on_question", "the footer closed a question batch",
			dlog.Context{"question_id": id}, func(s *wsState) {
				r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected the question-close transition",
					dlog.Context{"question_id": id})
				s.dropQuestion(id)
			})
	}
}

// dropQuestion retires one answered or failed batch.
func (s *wsState) dropQuestion(id string) {
	delete(s.questions, id)
	s.questionOrder = without(s.questionOrder, id)
}

// questionLead composes the open batch's lead line.
func questionLead(batch *conversationv1.AgentQuestionBatch) string {
	questions := batch.GetQuestions()
	if len(questions) == 0 {
		return "a question is open"
	}
	head := questions[0].GetQuestion().GetText()
	if head == "" {
		head = questions[0].GetHeader()
	}
	return fmt.Sprintf("%s · %s", plural(len(questions), "question"), truncate(head, DefaultWarningRowWidth))
}

// OnPermission moves the footer to waiting.
func (r *resolver) OnPermission(ws ids.WorkspaceID, agent *conversationv1.AgentId, p *conversationv1.AgentPermission) {
	if p == nil {
		return
	}
	id := p.GetId().GetValue()
	switch res := p.GetResult().(type) {
	case *conversationv1.AgentPermission_Start:
		r.mutate(ws, "daemon.footer.on_permission", "the footer opened a consent ask",
			dlog.Context{"permission_id": id}, func(s *wsState) {
				ask, seen := s.permissions[id]
				if !seen {
					s.permissionOrder = append(s.permissionOrder, id)
					ask.at = r.opts.clock.Now()
				}
				ask.text = gatedCall(res.Start.GetPrompt())
				s.permissions[id] = ask
			})
	default:
		r.mutate(ws, "daemon.footer.on_permission", "the footer closed a consent ask",
			dlog.Context{"permission_id": id}, func(s *wsState) {
				r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected the permission-close transition",
					dlog.Context{"permission_id": id})
				s.dropPermission(id)
			})
	}
}

// dropPermission retires one decided or failed ask.
func (s *wsState) dropPermission(id string) {
	delete(s.permissions, id)
	s.permissionOrder = without(s.permissionOrder, id)
}

// gatedCall composes the gated call's line from the vendor's own rendered
// prompt, so no sentence is reconstructed from a tool and its arguments.
func gatedCall(prompt *conversationv1.AgentPermissionPrompt) string {
	if name := prompt.GetDisplayName(); name != "" && prompt.GetTitle() != "" {
		return truncate(fmt.Sprintf("%s: %s", name, prompt.GetTitle()), DefaultWarningRowWidth)
	}
	if title := prompt.GetTitle(); title != "" {
		return truncate(title, DefaultWarningRowWidth)
	}
	return "consent is needed to continue"
}

// OnApiError is mid-turn evidence the footer draws as the salient `retrying`
// line: the turn cannot advance while its call fails. It stands until the
// retried call's response lands (clearRetry) or the next turn opens.
func (r *resolver) OnApiError(ws ids.WorkspaceID, agent *conversationv1.AgentId, failed *conversationv1.ApiRequestFailed) {
	if failed == nil {
		return
	}
	r.mutate(ws, "daemon.footer.on_api_error", "the footer took mid-turn api failure evidence",
		dlog.Context{"kind": turnfault.OfApiFailure(failed).Cause, "agent_id": agent.GetValue()}, func(s *wsState) {
			next := &retryState{
				agent:  agent.GetValue(),
				status: truncate(failed.GetMessage(), DefaultWarningRowWidth),
				at:     r.opts.clock.Now(),
			}
			// THE VENDOR'S OWN SCHEDULE WHEN IT STATED ONE: its retry count,
			// its limit and the instant its next try starts. Counting failures
			// here instead ran one ahead of the vendor's own count (2026-09-30).
			if retry := failed.GetRetry(); retry != nil {
				nextAt := time.UnixMilli(retry.GetNextAttemptAtMs())
				next.attempt = int32(retry.GetAttempt()) + 1
				next.maxAttempt = int32(retry.GetMaxRetries()) + 1
				next.nextAt = &nextAt
			} else {
				next.attempt = 2
				if s.retrying != nil {
					next.attempt = s.retrying.attempt + 1
				}
			}
			s.retrying = next
		})
}

// line is the retrying state as the footer's salient line.
func (rs *retryState) line() *frontendv1.FooterStatusActivityRetrying {
	out := &frontendv1.FooterStatusActivityRetrying{Attempt: rs.attempt, Status: rs.status}
	if rs.nextAt != nil {
		out.NextAttempt = &frontendv1.FooterStatusActivityAt{AtMs: rs.nextAt.UnixMilli()}
	}
	if rs.maxAttempt > 0 {
		max := rs.maxAttempt
		out.MaxAttempt = &max
	}
	return out
}

// clearRetry ends the `retrying` line at the FIRST SUCCESSFUL RESPONSE of the
// agent whose call was being retried: a frame of its reasoning or prose, or
// one that carries an API response's usage, is the vendor answering. Another
// agent's frame says nothing about this call.
func (r *resolver) clearRetry(ws ids.WorkspaceID, s *wsState, agent string, act *conversationv1.AgentActivity) {
	if s.retrying == nil || s.retrying.agent != agent || !ladder.RetryAnswered(act) {
		return
	}
	r.logOf(ws, s).Debug("daemon.footer.retry_cleared", "the retried call's response landed; the retrying line ended",
		dlog.Context{"agent_id": agent, "attempt": s.retrying.attempt})
	failed := s.retrying.attempt - 1
	s.retrying = nil
	// RECOVERY IS ANNOUNCED, not only the failure: the line that stood while
	// the API was unreachable gives way to a transient saying it answered.
	r.raiseTransient(ws, s, "", &frontendv1.FooterActivityTransient{
		Kind: &frontendv1.FooterActivityTransient_ApiRestored{
			ApiRestored: &frontendv1.FooterActivityTransientApiRestored{FailedAttempts: failed}},
	})
}

// without removes one id from an order slice, preserving the rest.
func without(order []string, id string) []string {
	out := order[:0]
	for _, v := range order {
		if v != id {
			out = append(out, v)
		}
	}
	return out
}

// linkName is the link state's spelling in logs and in the render-colors
// tables.
func linkName(link sessionwatcher.LinkState) string {
	switch link {
	case shimclient.LinkDialing:
		return "dialing"
	case shimclient.LinkConnected:
		return "connected"
	case shimclient.LinkRedialing:
		return "redialing"
	case shimclient.LinkDead:
		return "dead"
	default:
		return "unknown"
	}
}

// OnContextCut takes the AgentUpdate.context_cut page line, which is what ENDS
// a clear or a compaction.
//
// THE ASYMMETRY IS THE POINT: `SessionUpdate.compacting` only STARTS
// `thinking · compacting` — no progress figure exists upstream — and this
// record is the only end signal there is. Whichever way the cut went the
// session is idle again afterwards, because a failed compaction cut nothing
// and the turn is over either way.
//
// A FAILED COMPACTION IS NOT SILENCE: nothing was cut and the context is still
// too large, which is exactly what the context-budget line says, so the
// producer's account stands as the salient `context_budget` line and is
// recorded at WARN. A cut that SUCCEEDED — a compaction or a /clear — shrank
// the context, so it ends the context-budget line, whatever stood it.
func (r *resolver) OnContextCut(ws ids.WorkspaceID, agent *conversationv1.AgentId, cut *conversationv1.ContextCut) {
	if cut == nil {
		return
	}
	arm := contextCutArm(cut)
	failed, _ := cut.GetCut().(*conversationv1.ContextCut_CompactionFailed)

	// THROUGH THE ONE PUBLICATION SITE, like every other fact: the cut moves
	// the status arm (the turn ends) and the activity line (the compaction's
	// line goes), and a change published past mutate is a change no record
	// states.
	r.mutate(ws, "daemon.footer.on_context_cut", "the footer took a context cut",
		dlog.Context{"arm": arm}, func(s *wsState) {
			s.turn = nil
			s.turnEverRan = true
			s.compacting = false
			concludeCompactionID(s, cutCompactionID(cut))
			// THE LINE GOES WITH THE ACT IT NARRATED. The cut is the
			// compaction's end signal, so its progress sentence stops standing
			// here; a failed cut still says what went wrong, through the
			// context-budget line below.
			r.endCompaction(ws, s, "daemon.footer.on_context_cut")
			s.tok.settled = true
			if failed == nil {
				r.endContextBudget(ws, s, "daemon.footer.on_context_cut."+arm)
				return
			}
			r.standContextBudget(ws, s, "",
				"compaction failed — "+truncate(failed.CompactionFailed.GetError(), DefaultWarningRowWidth))
			r.logOf(ws, s).Warn("daemon.footer.on_context_cut",
				"a compaction failed, so nothing was cut and the context is still too large",
				dlog.Context{"arm": arm, "error": failed.CompactionFailed.GetError()})
		})
}

// contextCutArm names the cut's arm for the record.
func contextCutArm(cut *conversationv1.ContextCut) string {
	switch cut.GetCut().(type) {
	case *conversationv1.ContextCut_Cleared:
		return "cleared"
	case *conversationv1.ContextCut_Compacted:
		return "compacted"
	case *conversationv1.ContextCut_CompactionFailed:
		return "compaction_failed"
	default:
		return "unset"
	}
}

// SetParticipants states whether this workspace's host and web streams are
// held. It is the other two hops of connectivity truth (daemon.md invariant
// 11); the footer draws not-connected while either is down.
func (r *resolver) SetParticipants(ws ids.WorkspaceID, host, web bool) {
	r.mutate(ws, "daemon.footer.set_participants", "the footer took the participant streams' liveness",
		dlog.Context{"host_stream": host, "web_stream": web}, func(s *wsState) {
			s.hostStream = host
			s.webStream = web
		})
}
