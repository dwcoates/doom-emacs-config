package footer

import (
	"google.golang.org/protobuf/reflect/protoreflect"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// THE WORKING STEP.
//
// While a turn runs, the `working` status's step names what the MAIN agent is
// doing now, sync only: the latest of its tool calls still running names the
// step (reading, writing, searching, fetching, delegating, executing), and
// with none running an inference call is (thinking). A subagent's items run
// inside its own feed item, and detached work is not the turn's, so neither
// names the step.
//
// The activity cell carries NO line composed from a landed feed item: the
// quiet tier that did is retired (owner ruling, 2026-10-01).

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

// feedKind is one AgentActivity item arm as the working step reads it.
type feedKind struct {
	// step is the working step the item names while it runs.
	step workStep
	// detaches reports an item that leaves the turn at its first frame: it
	// surfaces and is handed off in one breath, and later frames of it are
	// detached work's.
	detaches bool
}

// feedKinds is every AgentActivity item arm, keyed by its oneof field name.
// An arm missing here is a contract the footer has not read, and is recorded
// at ERROR; TestEveryActivityArmHasAFeedKind pins that none is.
var feedKinds = map[protoreflect.Name]feedKind{
	"thinking":          {step: stepThinking},
	"response":          {step: stepThinking},
	"skill_use":         {step: stepExecuting},
	"read":              {step: stepReading},
	"write":             {step: stepWriting},
	"edit":              {step: stepWriting},
	"grep":              {step: stepSearching},
	"glob":              {step: stepSearching},
	"bash":              {step: stepExecuting},
	"subagent":          {step: stepDelegating},
	"unmodeled":         {step: stepExecuting},
	"send_message":      {step: stepExecuting},
	"task_act":          {},
	"hook":              {step: stepNone},
	"context_injected":  {},
	"web_fetch":         {step: stepFetching},
	"web_search":        {step: stepSearching},
	"monitor":           {step: stepExecuting, detaches: true},
	"schedule_wakeup":   {step: stepExecuting},
	"artifact":          {step: stepExecuting},
	"plan_mode":         {step: stepExecuting},
	"report_findings":   {step: stepExecuting},
	"worktree":          {step: stepExecuting},
	"cron":              {step: stepExecuting},
	"push_notification": {step: stepExecuting},
	"mcp_tool_call":     {step: stepExecuting},
	"subagent_handback": {step: stepExecuting},
}

// itemPhase is where one frame puts its item.
type itemPhase int

const (
	// phaseRunning: the item has surfaced (or keeps running).
	phaseRunning itemPhase = iota
	// phaseFinished: the item landed well.
	// phaseLanded: the item landed, however it ended.
	phaseLanded
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
	"success":            phaseLanded,
	"succeeded":          phaseLanded,
	"ended":              phaseLanded,
	"failure":            phaseLanded,
	"blocking_error":     phaseLanded,
	"non_blocking_error": phaseLanded,
	"cancelled":          phaseLanded,
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
	// seq orders surfacings, so the latest running item names the step.
	seq int
}

// feedMotion is the main agent's items as the working step reads them.
type feedMotion struct {
	// open are the items that surfaced and have not landed, by unit.
	open map[string]openItem
	// seq is the next surfacing's order.
	seq int
	// left are the units that left the turn (detached): their later frames
	// are detached work's, never a surfacing or a step.
	left map[string]struct{}
}

func newFeedMotion() feedMotion {
	return feedMotion{open: map[string]openItem{}, left: map[string]struct{}{}}
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

// OnMainAgent names the session's main agent. See sessionwatcher.FooterSink.
func (r *resolver) OnMainAgent(ws ids.WorkspaceID, agent *conversationv1.AgentId) {
	r.mutate(ws, "daemon.footer.on_main_agent", "the footer was told the session's main agent",
		dlog.Context{"agent_id": agent.GetValue()}, func(s *wsState) {
			s.mainAgent = agent.GetValue()
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
		r.logOf(ws, s).Error("daemon.footer.work_step_unknown_item",
			"an activity arm has no working-step reading", dlog.Context{
				"arm":                 string(field.Name()),
				"invariant_violation": "every AgentActivity item arm is in feedKinds",
				"remediation":         "add the arm to footer.feedKinds",
			})
		return feedKind{}, phaseNone, false
	}
	item := msg.Get(field).Message()
	oneofs := item.Descriptor().Oneofs()
	if oneofs.Len() != 1 {
		r.logOf(ws, s).Error("daemon.footer.work_step_unreadable_item",
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
		r.logOf(ws, s).Error("daemon.footer.work_step_unknown_phase",
			"an activity item's arm has no phase reading", dlog.Context{
				"arm": string(field.Name()), "phase_arm": string(arm.Name()),
				"invariant_violation": "every item oneof arm is in footer.itemPhases",
				"remediation":         "add the arm to footer.itemPhases",
			})
		return kind, phaseNone, false
	}
	return kind, phase, true
}

// trackFeed folds one activity frame into the working step. It runs inside
// OnActivity's mutation. Every frame is classified, so an arm the tables do
// not cover is recorded whatever the status; only the main agent's frames
// while a turn runs move the step.
func (r *resolver) trackFeed(ws ids.WorkspaceID, s *wsState, agent *conversationv1.AgentId, unit string, act *conversationv1.AgentActivity) {
	if unit == "" {
		return
	}
	m := &s.motion
	if _, gone := m.left[unit]; gone {
		return
	}
	kind, phase, ok := r.classify(ws, s, act)
	if !ok || phase == phaseNone {
		return
	}
	if s.turn == nil || s.mainAgent == "" || agent.GetValue() != s.mainAgent {
		return
	}
	if phase == phaseLanded {
		delete(m.open, unit)
		return
	}
	if _, running := m.open[unit]; running {
		return
	}
	if kind.detaches {
		// Surfaced and handed off at once: the item is detached work from
		// here on, and names no step.
		m.left[unit] = struct{}{}
		return
	}
	m.open[unit] = openItem{step: kind.step, seq: m.seq}
	m.seq++
}

// startTurnMotion resets the motion for a turn the daemon accepted: nothing of
// the previous turn's carries into it. The units that left a turn stay left.
func (r *resolver) startTurnMotion(s *wsState) {
	left := s.motion.left
	s.motion = newFeedMotion()
	s.motion.left = left
}

// endTurnMotion clears the turn's motion at its terminal. Items still open
// never land (an interrupted call).
func (r *resolver) endTurnMotion(s *wsState) {
	r.startTurnMotion(s)
}

// leaveTurn records a unit that left the turn (detached): it is detached work
// from here on, and stops naming the step.
func (r *resolver) leaveTurn(s *wsState, unit string) {
	if unit == "" {
		return
	}
	s.motion.left[unit] = struct{}{}
	delete(s.motion.open, unit)
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
