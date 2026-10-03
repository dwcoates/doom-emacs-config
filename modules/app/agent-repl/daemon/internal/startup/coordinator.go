package startup

import (
	"context"
	"errors"
	"sync"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/sidebar"
)

// op is the operation every record of a run is written under.
const op = "daemon.startup.run"

// Deps are a coordinator's collaborators.
type Deps struct {
	// Order answers the open workspaces in REGISTRY ORDER (sidebar.TabOrder
	// over the published roster).
	Order func() []sidebar.TabEntry
	// Live reports whether a workspace's session is up (or parked at its
	// cold gate) on this daemon right now.
	Live func(ws ids.WorkspaceID) bool
	// BringUp starts the sessions of the named workspaces through the one
	// shared bring-up (bringup.Run), off the caller's goroutine, and calls
	// done once per workspace with how its start ended (nil when the session
	// came up). It raises and lowers the roster's bring-up marker itself.
	BringUp func(pending []ids.WorkspaceID, done func(ws ids.WorkspaceID, err error))
	// Now stamps every event.
	Now func() time.Time
	// Log is the global logger: a run is the editor's, no workspace's.
	Log dlog.Logger
}

// phase is what the coordinator knows of one workspace's bring-up, whoever
// started it.
type phase struct {
	// inFlight is a bring-up under way: begun and not yet ended.
	inFlight bool
	// serving is agent-repl's services serving it in this bring-up.
	serving bool
	// failed is the service-level failure that ended it, "" when none.
	failed string
}

// Coordinator runs the editor's startups.
type Coordinator struct {
	deps Deps

	mu     sync.Mutex
	phases map[ids.WorkspaceID]*phase
	runs   map[*run]struct{}
}

// New builds a coordinator, refusing a missing collaborator.
func New(deps Deps) (*Coordinator, error) {
	switch {
	case deps.Order == nil:
		return nil, errors.New("startup: the registry order is required")
	case deps.Live == nil:
		return nil, errors.New("startup: the liveness test is required")
	case deps.BringUp == nil:
		return nil, errors.New("startup: the bring-up is required")
	case deps.Now == nil:
		return nil, errors.New("startup: a clock is required")
	case deps.Log == nil:
		return nil, errors.New("startup: a logger is required")
	}
	return &Coordinator{deps: deps, phases: map[ids.WorkspaceID]*phase{}, runs: map[*run]struct{}{}}, nil
}

// Step takes one bring-up step of one workspace from the fleet. It is called
// for EVERY bring-up, run or no run, so a run that begins mid-bring-up knows
// how far it has got.
func (c *Coordinator) Step(ws ids.WorkspaceID, step Step) {
	c.mu.Lock()
	p := c.phases[ws]
	if p == nil || step.begins() {
		p = &phase{}
		c.phases[ws] = p
	}
	if step.begins() {
		p.inFlight = true
	}
	if step.serves() {
		p.serving = true
	}
	if step.Kind == StepFailed {
		p.failed = step.Text
	}
	if step.ends() {
		p.inFlight = false
	}
	runs := make([]*run, 0, len(c.runs))
	for r := range c.runs {
		runs = append(runs, r)
	}
	c.mu.Unlock()
	c.deps.Log.Debug(op, "a workspace's bring-up moved a step", dlog.Context{
		dlog.KeyWorkspaceID: string(ws), "step": step.Kind.String(),
	})
	for _, r := range runs {
		r.post(note{ws: ws, step: &step})
	}
}

// note is one thing that happened to a workspace in a run.
type note struct {
	ws ids.WorkspaceID
	// step is a fleet step, nil for a start's end.
	step *Step
	// ended is a start this run asked for having returned, with its error.
	ended bool
	err   error
}

// run is one startup, for one stream.
type run struct {
	mu     sync.Mutex
	queue  []note
	signal chan struct{}
}

// post queues a note for the run; it never blocks.
func (r *run) post(n note) {
	r.mu.Lock()
	r.queue = append(r.queue, n)
	r.mu.Unlock()
	select {
	case r.signal <- struct{}{}:
	default:
	}
}

// take drains the run's queue.
func (r *run) take() []note {
	r.mu.Lock()
	defer r.mu.Unlock()
	out := r.queue
	r.queue = nil
	return out
}

// slot is one workspace's place in a run.
type slot struct {
	entry sidebar.TabEntry
	// ready is agent-repl serving it; failed the settled failure's reason.
	ready  bool
	failed string
	// settled is ready or failed: the go-ahead may be sent.
	settled bool
	// opened is its go-ahead sent.
	opened bool
	// waitingFor is the workspace it last said it waits on, "" when none.
	waitingFor string
}

// Run is one startup: it brings every open workspace up and sends emit the
// events, in order, until every workspace has had its go-ahead and the run has
// finished, or ctx ends. emit is called from Run's goroutine only.
func (c *Coordinator) Run(ctx context.Context, emit func(*agentreplv1.DaemonStartupEvent)) {
	order := c.deps.Order()
	r := &run{signal: make(chan struct{}, 1)}
	slots := make([]*slot, len(order))
	index := make(map[ids.WorkspaceID]int, len(order))
	for i, e := range order {
		slots[i] = &slot{entry: e}
		index[ids.WorkspaceID(e.Ref.GetId())] = i
	}
	// THE RUN JOINS BEFORE IT READS, so no step can fall between what it read
	// and what it is told.
	c.mu.Lock()
	c.runs[r] = struct{}{}
	var pending []ids.WorkspaceID
	for i, sl := range slots {
		ws := ids.WorkspaceID(sl.entry.Ref.GetId())
		p := c.phases[ws]
		switch {
		case p != nil && p.inFlight:
			// SOMEONE ELSE'S BRING-UP IS UNDER WAY (the boot's, a revival's):
			// its steps reach this run, and how far it has got is known.
			if p.serving {
				slots[i].ready, slots[i].settled = true, true
			}
		case c.deps.Live(ws):
			slots[i].ready, slots[i].settled = true, true
		default:
			pending = append(pending, ws)
		}
	}
	c.mu.Unlock()
	defer func() {
		c.mu.Lock()
		delete(c.runs, r)
		c.mu.Unlock()
	}()

	c.deps.Log.Info(op, "a new Emacs connected; bringing its workspaces up", dlog.Context{
		"workspaces": len(slots), "starting": len(pending),
	})
	emit(c.event(&agentreplv1.DaemonStartupEvent{Event: &agentreplv1.DaemonStartupEvent_Opening{
		Opening: &agentreplv1.DaemonStartupOpening{Workspaces: uint32(len(slots))}}}))
	if len(pending) > 0 {
		c.deps.BringUp(pending, func(ws ids.WorkspaceID, err error) {
			r.post(note{ws: ws, ended: true, err: err})
		})
	}

	next := 0
	for {
		next = c.advance(slots, next, emit)
		if next == len(slots) {
			c.finish(slots, emit)
			return
		}
		select {
		case <-ctx.Done():
			c.deps.Log.Info(op, "the editor's stream ended before every workspace had its go-ahead", dlog.Context{
				"opened": next, "workspaces": len(slots), "cause": ctx.Err().Error(),
			})
			return
		case <-r.signal:
		}
		for _, n := range r.take() {
			i, ok := index[n.ws]
			if !ok {
				continue
			}
			c.apply(slots[i], n, emit)
		}
	}
}

// apply folds one note into a slot, relaying a step the editor prints.
func (c *Coordinator) apply(sl *slot, n note, emit func(*agentreplv1.DaemonStartupEvent)) {
	ref := sl.entry.Ref
	if n.step != nil {
		if n.step.relayed() {
			emit(c.event(&agentreplv1.DaemonStartupEvent{Event: &agentreplv1.DaemonStartupEvent_WorkspaceStep{
				WorkspaceStep: workspaceStep(ref, *n.step)}}))
		}
		switch {
		case n.step.serves():
			sl.ready = true
		case n.step.Kind == StepFailed && !sl.ready:
			sl.failed = n.step.Text
		}
		sl.settled = sl.settled || sl.ready || sl.failed != ""
		return
	}
	// A START THIS RUN ASKED FOR HAS RETURNED. Its steps have said how it went
	// (they were posted before it returned); a start that ended with an error
	// and no step to say so (it never reached the fleet: the session record
	// could not be read, the daemon is leaving) settles failed here.
	if n.err != nil && !sl.ready && sl.failed == "" {
		sl.failed = n.err.Error()
		emit(c.event(&agentreplv1.DaemonStartupEvent{Event: &agentreplv1.DaemonStartupEvent_WorkspaceStep{
			WorkspaceStep: workspaceStep(ref, Step{Kind: StepFailed, Text: sl.failed})}}))
	}
	if n.err == nil {
		sl.ready = true
	}
	sl.settled = sl.ready || sl.failed != ""
}

// advance sends every go-ahead that is due, strictly in registry order, and
// tells each settled workspace behind the first unsettled one what it waits
// on. It answers the index of the first workspace still without its go-ahead.
func (c *Coordinator) advance(slots []*slot, next int, emit func(*agentreplv1.DaemonStartupEvent)) int {
	for next < len(slots) && slots[next].settled {
		sl := slots[next]
		sl.opened = true
		c.deps.Log.Info(op, "a workspace's tab may open", dlog.Context{
			dlog.KeyWorkspaceID: sl.entry.Ref.GetId(), "name": sl.entry.Name,
			"position": next + 1, "failed": sl.failed != "",
		})
		emit(c.event(&agentreplv1.DaemonStartupEvent{Event: &agentreplv1.DaemonStartupEvent_WorkspaceOpen{
			WorkspaceOpen: &agentreplv1.DaemonStartupWorkspaceOpen{Workspace: sl.entry.Ref}}}))
		next++
	}
	if next == len(slots) {
		return next
	}
	ahead := slots[next].entry
	for _, sl := range slots[next+1:] {
		if !sl.settled || sl.waitingFor == ahead.Ref.GetId() {
			continue
		}
		sl.waitingFor = ahead.Ref.GetId()
		emit(c.event(&agentreplv1.DaemonStartupEvent{Event: &agentreplv1.DaemonStartupEvent_WorkspaceStep{
			WorkspaceStep: &agentreplv1.DaemonStartupWorkspaceStep{
				Workspace: sl.entry.Ref,
				Step: &agentreplv1.DaemonStartupWorkspaceStep_WaitingFor{
					WaitingFor: &agentreplv1.DaemonStartupStepWaitingFor{Ahead: ahead.Ref}},
			}}}))
	}
	return next
}

// finish sends the run's last event.
func (c *Coordinator) finish(slots []*slot, emit func(*agentreplv1.DaemonStartupEvent)) {
	done := &agentreplv1.DaemonStartupFinished{Total: uint32(len(slots))}
	var failedNames []string
	for _, sl := range slots {
		if sl.failed != "" && !sl.ready {
			done.Failed = append(done.Failed, &agentreplv1.DaemonStartupFailedWorkspace{
				Workspace: sl.entry.Ref, Name: sl.entry.Name})
			failedNames = append(failedNames, sl.entry.Name)
			continue
		}
		done.Ready++
	}
	c.deps.Log.Info(op, "every workspace has had its go-ahead", dlog.Context{
		"ready": done.Ready, "total": done.Total, "failed": failedNames,
	})
	emit(c.event(&agentreplv1.DaemonStartupEvent{Event: &agentreplv1.DaemonStartupEvent_Finished{Finished: done}}))
}

// event stamps an event with the run's clock.
func (c *Coordinator) event(e *agentreplv1.DaemonStartupEvent) *agentreplv1.DaemonStartupEvent {
	e.AtMs = c.deps.Now().UnixMilli()
	return e
}

// workspaceStep renders a relayed step for ref.
func workspaceStep(ref *workspacev1.WorkspaceRef, step Step) *agentreplv1.DaemonStartupWorkspaceStep {
	out := &agentreplv1.DaemonStartupWorkspaceStep{Workspace: ref}
	switch step.Kind {
	case StepStartingSession:
		out.Step = &agentreplv1.DaemonStartupWorkspaceStep_StartingSession{StartingSession: &agentreplv1.DaemonStartupStepStartingSession{}}
	case StepWaking:
		out.Step = &agentreplv1.DaemonStartupWorkspaceStep_Waking{Waking: &agentreplv1.DaemonStartupStepWaking{}}
	case StepResuming:
		out.Step = &agentreplv1.DaemonStartupWorkspaceStep_Resuming{Resuming: &agentreplv1.DaemonStartupStepResuming{}}
	case StepVendorRetrying:
		out.Step = &agentreplv1.DaemonStartupWorkspaceStep_VendorRetrying{VendorRetrying: &agentreplv1.DaemonStartupStepVendorRetrying{Attempt: step.Attempt}}
	case StepVendorRejected:
		out.Step = &agentreplv1.DaemonStartupWorkspaceStep_VendorRejected{VendorRejected: &agentreplv1.DaemonStartupStepVendorRejected{Cause: step.Text}}
	case StepVendorFailed:
		out.Step = &agentreplv1.DaemonStartupWorkspaceStep_VendorFailed{VendorFailed: &agentreplv1.DaemonStartupStepVendorFailed{}}
	case StepColdGate:
		out.Step = &agentreplv1.DaemonStartupWorkspaceStep_ColdGate{ColdGate: &agentreplv1.DaemonStartupStepColdGate{}}
	case StepOffline:
		out.Step = &agentreplv1.DaemonStartupWorkspaceStep_Offline{Offline: &agentreplv1.DaemonStartupStepOffline{}}
	case StepFailed:
		out.Step = &agentreplv1.DaemonStartupWorkspaceStep_Failed{Failed: &agentreplv1.DaemonStartupStepFailed{Reason: step.Text}}
	}
	return out
}
