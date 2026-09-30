package footer

import (
	"fmt"
	"time"

	"google.golang.org/protobuf/reflect/protoreflect"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// THE WORKING STEP AND THE QUIET-STRETCH LINE.
//
// While a turn runs, the `working` status's step names what the MAIN agent is
// doing now, sync only: the latest of its tool calls still running names the
// step (reading, writing, searching, fetching, delegating, executing), and
// with none running an inference call is (thinking).
//
// A QUIET STRETCH is the period between the moment a feed item has FULLY
// LANDED and the moment the next feed item FIRST SURFACES. Through it the
// activity line says what just landed and what the turn does next
// ("✅ Bash finished — handling result..."), and it ends the moment the feed
// DRAWS the next item — a streaming response at its first fragment. The ended
// line is then stated with the row that ended it
// (FooterStatusQuietStretchEnding), and the client holds it until it has
// painted that row, so the line never clears before its successor is visible. While a
// turn is in flight only the main agent's items count: a subagent's items
// run inside its own feed item, and detached work is not surfaced on the line
// then. With no turn in flight and detached work live (`background`), every
// item that lands stands a line of its own ("✅ Subagent finished").

// workStep is what the main agent is doing now: the working status's step.
type workStep int

// The steps, in no order: the latest-surfaced running item decides.
const (
	// stepNone is an item that names no step: a hook runs around the agent's
	// work, never as it.
	stepNone workStep = iota
	stepThinking
	stepExecuting
	stepReading
	stepWriting
	stepSearching
	stepFetching
	stepDelegating
)

// feedKind is one AgentActivity item arm as the working step and the quiet
// stretch read it.
type feedKind struct {
	// label names the item on the quiet-stretch line.
	label string
	// step is the working step the item names while it runs.
	step workStep
	// feed reports whether the feed draws a row for the item (resolve/feed
	// drawActivity). Only a feed item surfaces or lands.
	feed bool
	// next is what the turn does after the item lands well.
	next string
	// detaches reports an item that leaves the turn at its first frame: it
	// surfaces and is handed off in one breath, and later frames of it are
	// detached work's.
	detaches bool
}

// The clauses a landed item's line ends with while a turn runs.
const (
	nextHandlingResult  = "handling result..."
	nextContinuing      = "continuing..."
	nextHandlingFailure = "handling failure..."
)

// feedKinds is every AgentActivity item arm, keyed by its oneof field name.
// An arm missing here is a contract the footer has not read, and is recorded
// at ERROR; TestEveryActivityArmHasAFeedKind pins that none is.
var feedKinds = map[protoreflect.Name]feedKind{
	"thinking":          {label: "Thinking", step: stepThinking, feed: true, next: nextContinuing},
	"response":          {label: "Response", step: stepThinking, feed: true, next: nextContinuing},
	"skill_use":         {label: "Skill", step: stepExecuting, feed: true, next: nextHandlingResult},
	"read":              {label: "Read", step: stepReading, feed: true, next: nextHandlingResult},
	"write":             {label: "Write", step: stepWriting, feed: true, next: nextHandlingResult},
	"edit":              {label: "Edit", step: stepWriting, feed: true, next: nextHandlingResult},
	"grep":              {label: "Grep", step: stepSearching, feed: true, next: nextHandlingResult},
	"glob":              {label: "Glob", step: stepSearching, feed: true, next: nextHandlingResult},
	"bash":              {label: "Bash", step: stepExecuting, feed: true, next: nextHandlingResult},
	"subagent":          {label: "Subagent", step: stepDelegating, feed: true, next: nextHandlingResult},
	"unmodeled":         {label: "Tool", step: stepExecuting},
	"send_message":      {label: "SendMessage", step: stepExecuting, feed: true, next: nextHandlingResult},
	"task_act":          {label: "Task update"},
	"hook":              {label: "Hook", step: stepNone, feed: true, next: nextContinuing},
	"context_injected":  {label: "Context"},
	"web_fetch":         {label: "WebFetch", step: stepFetching, feed: true, next: nextHandlingResult},
	"web_search":        {label: "WebSearch", step: stepSearching, feed: true, next: nextHandlingResult},
	"monitor":           {label: "Monitor", step: stepExecuting, feed: true, next: nextContinuing, detaches: true},
	"schedule_wakeup":   {label: "ScheduleWakeup", step: stepExecuting},
	"artifact":          {label: "Artifact", step: stepExecuting, feed: true, next: nextHandlingResult},
	"plan_mode":         {label: "Plan", step: stepExecuting, feed: true, next: nextHandlingResult},
	"report_findings":   {label: "Findings", step: stepExecuting, feed: true, next: nextHandlingResult},
	"worktree":          {label: "Worktree", step: stepExecuting, feed: true, next: nextHandlingResult},
	"cron":              {label: "Cron", step: stepExecuting},
	"push_notification": {label: "Notification", step: stepExecuting},
	"mcp_tool_call":     {label: "MCP tool", step: stepExecuting, feed: true, next: nextHandlingResult},
	"subagent_handback": {label: "Subagent report", step: stepExecuting, feed: true, next: nextContinuing},
}

// itemPhase is where one frame puts its item.
type itemPhase int

const (
	// phaseRunning: the item has surfaced (or keeps running).
	phaseRunning itemPhase = iota
	// phaseFinished: the item landed well.
	phaseFinished
	// phaseFailed: the item landed failed.
	phaseFailed
	// phaseCancelled: the item landed cancelled.
	phaseCancelled
	// phaseNone: the item has no lifecycle (a task act, injected context).
	phaseNone
)

// itemPhases reads an item's frame by the arm its own oneof carries. Every
// item message has exactly one oneof; its arms are spelled from this set.
var itemPhases = map[protoreflect.Name]itemPhase{
	"start":              phaseRunning,
	"update":             phaseRunning,
	"progress":           phaseRunning,
	"tail":               phaseRunning,
	"diagnostics":        phaseRunning,
	"success":            phaseFinished,
	"succeeded":          phaseFinished,
	"ended":              phaseFinished,
	"failure":            phaseFailed,
	"blocking_error":     phaseFailed,
	"non_blocking_error": phaseFailed,
	"cancelled":          phaseCancelled,
	// The lifecycle-less items: one frame states the whole fact.
	"created":  phaseNone,
	"changed":  phaseNone,
	"rejected": phaseNone,
	"memory":   phaseNone,
	"skills":   phaseNone,
}

// openItem is one surfaced item that has not landed.
type openItem struct {
	step workStep
	feed bool
	// seq orders surfacings, so the latest running item names the step.
	seq int
}

// feedMotion is the workspace's feed items as the working step and the quiet
// stretch read them.
type feedMotion struct {
	// open are the items that surfaced and have not landed, by unit.
	open map[string]openItem
	// seq is the next surfacing's order.
	seq int
	// delivered reports that the turn's prompt has been delivered (its turn
	// opened), which is where the turn's first quiet stretch can begin.
	delivered bool
	// line is the quiet-stretch line, nil when none stands.
	line *standing
	// left are the units that left the turn (detached): their later frames
	// are detached work's, never a surfacing or a step.
	left map[string]struct{}
	// drawn are the rows the feed drew for units that have not landed, by
	// unit (OnItemDrawn).
	drawn map[string]drawnRow
	// ending is the line the next feed item's drawing ended, nil when none.
	ending *lineEnding
}

// lineEnding is a quiet-stretch line ended by the drawing of the next feed
// item, and that item's row.
type lineEnding struct {
	text string
	// at is when the ended line began standing.
	at  time.Time
	row *frontendv1.FeedId
}

// drawnRow is where the feed drew one unit's row.
type drawnRow struct {
	row *frontendv1.FeedId
	// onRoot reports a row on the root feed, the one feed the client always
	// shows and so the only one it can promise to paint.
	onRoot bool
}

func newFeedMotion() feedMotion {
	return feedMotion{
		open:  map[string]openItem{},
		left:  map[string]struct{}{},
		drawn: map[string]drawnRow{},
	}
}

// surface ends the standing line if UNIT's row is drawn: the stretch ends when
// the next feed item is drawn, not merely surfaced. An item surfaced but not
// yet drawn leaves the line standing, and OnItemDrawn ends it at the draw.
//
// Only a ROOT-FEED row holds the ended line: a row on a sub-feed is painted
// only if the reader has that bubble open, so the client could not promise to
// clear a line held on it, and the line ends at the draw.
func (m *feedMotion) surface(unit string) {
	drawn, ok := m.drawn[unit]
	if !ok {
		return
	}
	if m.line != nil && drawn.onRoot {
		m.ending = &lineEnding{text: m.line.text, at: m.line.at, row: drawn.row}
	}
	m.line = nil
}

// step answers the working step: the latest-surfaced running item that names
// a step beyond thinking, else thinking.
func (m *feedMotion) step() workStep {
	best, bestSeq := stepThinking, -1
	for _, item := range m.open {
		if item.step > stepThinking && item.seq > bestSeq {
			best, bestSeq = item.step, item.seq
		}
	}
	return best
}

// feedItemOpen reports whether any feed item is surfacing or running.
func (m *feedMotion) feedItemOpen() bool {
	for _, item := range m.open {
		if item.feed {
			return true
		}
	}
	return false
}

// OnMainAgent names the session's main agent. See sessionwatcher.FooterSink.
func (r *resolver) OnMainAgent(ws ids.WorkspaceID, agent *conversationv1.AgentId) {
	r.mutate(ws, "daemon.footer.on_main_agent", "the footer was told the session's main agent",
		dlog.Context{"agent_id": agent.GetValue()}, func(s *wsState) {
			s.mainAgent = agent.GetValue()
		})
}

// OnItemDrawn records where the feed drew one activity unit's row. See
// Resolver.OnItemDrawn. CALLED UNDER THE FEED RESOLVER'S LOCK; it takes this
// resolver's lock and nothing else.
//
// The feed draws a frame before the footer takes it, so an item's row is
// usually recorded here before its surfacing is. An item the feed draws LATER
// than its surfacing (a spawn held for the frame naming its agent) is open
// already, and its draw ends the stretch here.
func (r *resolver) OnItemDrawn(ws ids.WorkspaceID, unit string, row *frontendv1.FeedId, onRoot bool) {
	if unit == "" || row.GetValue() == "" {
		r.workspaceLog(ws).Error("daemon.footer.item_drawn_unaddressed",
			"the feed announced a drawn row with no unit or no FeedId; no quiet stretch can end on it",
			dlog.Context{"unit": unit, "row": row.GetValue()})
		return
	}
	r.mutate(ws, "daemon.footer.on_item_drawn", "the footer took the feed's drawing of an activity row",
		dlog.Context{"unit": unit, "row": row.GetValue(), "on_root": onRoot}, func(s *wsState) {
			m := &s.motion
			m.drawn[unit] = drawnRow{row: row, onRoot: onRoot}
			if item, open := m.open[unit]; open && item.feed {
				m.surface(unit)
			}
		})
}

// classify reads one activity frame: the item's kind and the phase the frame
// puts it in. ok is false for a frame the footer cannot read, which is
// recorded at ERROR: the tables above must cover the whole contract.
func (r *resolver) classify(ws ids.WorkspaceID, s *wsState, act *conversationv1.AgentActivity) (feedKind, itemPhase, bool) {
	msg := act.ProtoReflect()
	field := msg.WhichOneof(msg.Descriptor().Oneofs().ByName("item"))
	if field == nil {
		return feedKind{}, phaseNone, false
	}
	kind, known := feedKinds[field.Name()]
	if !known {
		r.logOf(ws, s).Error("daemon.footer.quiet_stretch_unknown_item",
			"an activity arm has no working step or quiet-stretch reading", dlog.Context{
				"arm":                 string(field.Name()),
				"invariant_violation": "every AgentActivity item arm is in feedKinds",
				"remediation":         "add the arm to footer.feedKinds",
			})
		return feedKind{}, phaseNone, false
	}
	item := msg.Get(field).Message()
	oneofs := item.Descriptor().Oneofs()
	if oneofs.Len() != 1 {
		r.logOf(ws, s).Error("daemon.footer.quiet_stretch_unreadable_item",
			"an activity item does not carry exactly one oneof", dlog.Context{
				"arm": string(field.Name()), "oneofs": oneofs.Len(),
				"invariant_violation": "every AgentActivity item message has exactly one oneof",
			})
		return feedKind{}, phaseNone, false
	}
	arm := item.WhichOneof(oneofs.Get(0))
	if arm == nil {
		return kind, phaseNone, false
	}
	phase, known := itemPhases[arm.Name()]
	if !known {
		r.logOf(ws, s).Error("daemon.footer.quiet_stretch_unknown_phase",
			"an activity item's arm has no phase reading", dlog.Context{
				"arm": string(field.Name()), "phase_arm": string(arm.Name()),
				"invariant_violation": "every item oneof arm is in footer.itemPhases",
				"remediation":         "add the arm to footer.itemPhases",
			})
		return kind, phaseNone, false
	}
	return kind, phase, true
}

// trackFeed folds one activity frame into the working step and the quiet
// stretch. It runs inside OnActivity's mutation.
func (r *resolver) trackFeed(ws ids.WorkspaceID, s *wsState, agent *conversationv1.AgentId, unit string, act *conversationv1.AgentActivity) {
	if unit == "" {
		return
	}
	if _, gone := s.motion.left[unit]; gone {
		return
	}
	kind, phase, ok := r.classify(ws, s, act)
	if !ok || phase == phaseNone {
		return
	}
	if phase != phaseRunning {
		// A landed item's row ends nothing more.
		delete(s.motion.drawn, unit)
	}
	if s.turn != nil {
		if s.mainAgent == "" || agent.GetValue() != s.mainAgent {
			return
		}
		r.trackTurnItem(ws, s, unit, kind, phase)
		return
	}
	if kind.feed {
		r.trackBackgroundItem(s, unit, kind, phase)
	}
}

// trackTurnItem is one main-agent frame while a turn runs.
func (r *resolver) trackTurnItem(ws ids.WorkspaceID, s *wsState, unit string, kind feedKind, phase itemPhase) {
	m := &s.motion
	// ANY FRAME OF THE MAIN AGENT'S says the prompt was delivered, and it
	// delivers the turn exactly as the turn-open edge does -- the delivered
	// line included. The two arrive on different streams, so the edge can be
	// consumed before the turn is set (deliverTurn then has no turn to
	// deliver); flipping the flag alone left a delivered turn with no line,
	// and a non-feed item landing next (a wakeup) recorded the quiet stretch
	// with no line at ERROR (e2e TestScheduleWakeupScheduleAndStop under load,
	// 2026-09-29).
	if !m.delivered {
		r.deliverTurn(ws, s)
	}
	if phase == phaseRunning {
		if _, running := m.open[unit]; running {
			return
		}
		if kind.detaches {
			// Surfaced and handed off at once: the item is detached work
			// from here on, and its surfacing is also its landing.
			m.left[unit] = struct{}{}
			if !m.feedItemOpen() {
				r.standQuietLine(s, fmt.Sprintf("✅ %s started — %s", kind.label, kind.next))
			}
			r.checkQuietStretch(ws, s, "detached_at_surfacing")
			return
		}
		m.open[unit] = openItem{step: kind.step, feed: kind.feed, seq: m.seq}
		m.seq++
		if kind.feed {
			m.surface(unit)
		}
		return
	}
	delete(m.open, unit)
	if kind.feed && !m.feedItemOpen() {
		r.standQuietLine(s, turnLandedLine(kind, phase))
	}
	r.checkQuietStretch(ws, s, "landed")
}

// trackBackgroundItem is one feed item's frame with no turn in flight.
func (r *resolver) trackBackgroundItem(s *wsState, unit string, kind feedKind, phase itemPhase) {
	m := &s.motion
	if phase == phaseRunning {
		if _, running := m.open[unit]; running {
			return
		}
		m.open[unit] = openItem{step: kind.step, feed: true, seq: m.seq}
		m.seq++
		m.surface(unit)
		return
	}
	delete(m.open, unit)
	r.landBackground(s, kind.label, phase)
}

// landBackground stands the line for a background item that landed. With no
// detached work live there is no `background` status to draw it under.
func (r *resolver) landBackground(s *wsState, label string, phase itemPhase) {
	if s.turn != nil || !s.detachedLive() {
		return
	}
	r.standQuietLine(s, backgroundLandedLine(label, phase))
}

// detachedPhase reads a detached run's terminal: failed or finished.
func detachedPhase(failed bool) itemPhase {
	if failed {
		return phaseFailed
	}
	return phaseFinished
}

// landedHead words what landed, the head every landed line shares.
func landedHead(label string, phase itemPhase) string {
	switch phase {
	case phaseFailed:
		return fmt.Sprintf("❌ %s failed", label)
	case phaseCancelled:
		return fmt.Sprintf("❌ %s cancelled", label)
	default:
		return fmt.Sprintf("✅ %s finished", label)
	}
}

// turnLandedLine words a landed item's line while a turn runs: what landed,
// then what the turn does next.
func turnLandedLine(kind feedKind, phase itemPhase) string {
	next := kind.next
	switch phase {
	case phaseFailed:
		next = nextHandlingFailure
	case phaseCancelled:
		next = nextContinuing
	}
	return landedHead(kind.label, phase) + " — " + next
}

// backgroundLandedLine words a landed item's line with no turn in flight:
// nothing is handling it, so the line names the landing alone.
func backgroundLandedLine(label string, phase itemPhase) string {
	return landedHead(label, phase)
}

// deliveredLine words the line a delivered prompt stands, by the act the turn
// carries.
func deliveredLine(act SessionAct) string {
	switch act {
	case ActClear:
		return "✅ /clear delivered — clearing context..."
	case ActCompact:
		return "✅ /compact delivered — compacting context..."
	default:
		return "✅ Prompt delivered — awaiting response..."
	}
}

// standQuietLine stands the quiet-stretch line, the quiet tier's one line.
func (r *resolver) standQuietLine(s *wsState, text string) {
	s.motion.line = &standing{text: text, at: r.opts.clock.Now()}
	s.motion.ending = nil
}

// startTurnMotion resets the motion for a turn the daemon accepted: nothing of
// the previous turn's, or of background, carries into it.
func (r *resolver) startTurnMotion(s *wsState) {
	left := s.motion.left
	s.motion = newFeedMotion()
	s.motion.left = left
}

// deliverTurn is the turn-open edge: the prompt was delivered, and until the
// first item surfaces the turn is in a quiet stretch.
func (r *resolver) deliverTurn(ws ids.WorkspaceID, s *wsState) {
	if s.turn == nil {
		return
	}
	s.motion.delivered = true
	if !s.motion.feedItemOpen() {
		r.standQuietLine(s, deliveredLine(s.turn.Act))
	}
	r.checkQuietStretch(ws, s, "delivered")
}

// endTurnMotion clears the turn's motion at its terminal. Items still open
// never land (an interrupted call), and the turn's line belongs to the turn.
func (r *resolver) endTurnMotion(s *wsState) {
	r.startTurnMotion(s)
}

// leaveTurn records a unit that left the turn (detached): it is handed off,
// which lands it on the turn's feed.
func (r *resolver) leaveTurn(ws ids.WorkspaceID, s *wsState, unit string) {
	if unit == "" {
		return
	}
	m := &s.motion
	m.left[unit] = struct{}{}
	delete(m.drawn, unit)
	item, running := m.open[unit]
	if !running {
		return
	}
	delete(m.open, unit)
	if s.turn != nil && item.feed && !m.feedItemOpen() {
		r.standQuietLine(s, "✅ Moved to background — continuing...")
	}
	r.checkQuietStretch(ws, s, "detached")
}

// settleBackgroundLine drops a background line once no detached work is live:
// with no `background` status to stand under, a line kept would surface stale
// the next time detached work runs.
func (r *resolver) settleBackgroundLine(s *wsState) {
	if s.turn == nil && !s.detachedLive() {
		s.motion.line = nil
		s.motion.ending = nil
	}
}

// checkQuietStretch records at ERROR a quiet stretch with no line: while a
// delivered turn runs, either a feed item is surfacing or running, or the
// quiet-stretch line stands. The code above guarantees it, so a violation is
// a defect, and it is loud.
func (r *resolver) checkQuietStretch(ws ids.WorkspaceID, s *wsState, cause string) {
	m := &s.motion
	if s.turn == nil || !m.delivered || m.feedItemOpen() || m.line != nil {
		return
	}
	r.logOf(ws, s).Error("daemon.footer.quiet_stretch_without_line",
		"a quiet stretch stands with no activity line", dlog.Context{
			"cause":               cause,
			"open_items":          len(m.open),
			"invariant_violation": "a delivered turn has a feed item running or the quiet-stretch line standing",
		})
}

// quietStretchLine is the standing quiet-stretch line, or nil.
func (r *resolver) quietStretchLine(s *wsState) *frontendv1.FooterStatusActivityQuietStretch {
	if s.motion.line == nil {
		return nil
	}
	return &frontendv1.FooterStatusActivityQuietStretch{Text: s.motion.line.text}
}

// quietStretchEnding states the line the next feed item's drawing ended, or
// nil. SUPERSEDED reports that an activity stands now, which is newer than the
// ended line whatever its rank: it is drawn at once, and the ended line is
// never shown again in its place (owner ruling, 2026-09-29).
func quietStretchEnding(s *wsState, superseded bool) *frontendv1.FooterStatusQuietStretchEnding {
	if s.motion.ending == nil || superseded {
		return nil
	}
	return &frontendv1.FooterStatusQuietStretchEnding{
		Text: s.motion.ending.text, UntilPainted: s.motion.ending.row, At: stamp(s.motion.ending.at),
	}
}

// workingStep resolves the working status's step once the turn has said
// anything: the latest running tool call's step, else thinking.
func workingStep(s *wsState) *frontendv1.FooterStatusWorking {
	arm := &frontendv1.FooterStatusWorking{}
	switch s.motion.step() {
	case stepExecuting:
		arm.Substatus = &frontendv1.FooterStatusWorking_Executing{Executing: &frontendv1.FooterSubStatusWorkingExecuting{}}
	case stepReading:
		arm.Substatus = &frontendv1.FooterStatusWorking_Reading{Reading: &frontendv1.FooterSubStatusWorkingReading{}}
	case stepWriting:
		arm.Substatus = &frontendv1.FooterStatusWorking_Writing{Writing: &frontendv1.FooterSubStatusWorkingWriting{}}
	case stepSearching:
		arm.Substatus = &frontendv1.FooterStatusWorking_Searching{Searching: &frontendv1.FooterSubStatusWorkingSearching{}}
	case stepFetching:
		arm.Substatus = &frontendv1.FooterStatusWorking_Fetching{Fetching: &frontendv1.FooterSubStatusWorkingFetching{}}
	case stepDelegating:
		arm.Substatus = &frontendv1.FooterStatusWorking_Delegating{Delegating: &frontendv1.FooterSubStatusWorkingDelegating{}}
	default:
		arm.Substatus = &frontendv1.FooterStatusWorking_Thinking{Thinking: &frontendv1.FooterSubStatusWorkingThinking{}}
	}
	return arm
}
