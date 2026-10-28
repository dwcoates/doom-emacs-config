package opentabs

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// op is the operation every record of an open is written under.
const op = "daemon.opentabs.open"

// opPage is the operation the page facts are recorded under.
const opPage = "daemon.opentabs.page"

// ErrUnknownPage is PageDrawn's answer for a page no attached stream has:
// ReportPageDrawnError.unknown_page.
var ErrUnknownPage = errors.New("opentabs: no attached page stream has this id")

// Deps are a coordinator's collaborators.
type Deps struct {
	// Opening raises (true) or lowers (false) the roster's `opening` arm
	// (sidebar.Resolver.SetOpening). It is called under the coordinator's
	// lock, so its edges reach the roster in the order the set changed; it
	// must not call back into the coordinator.
	Opening func(ws ids.WorkspaceID, opening bool)
	// ReadConversation reads the workspace's root feed's newest page whole,
	// through the feed resolver's own loading, so the page is held and the
	// page's OpenFeed serves it from memory (feed.Resolver.ReadNewest). It is
	// called as the open begins, off the run's goroutine.
	ReadConversation func(ctx context.Context, ws ids.WorkspaceID) error
	// Row answers the workspace's roster row as the roster last resolved it,
	// with that roster's revision (sidebar.Resolver.Row), for the answer.
	Row func(ws ids.WorkspaceID) (*frontendv1.RosterRow, uint64, bool)
	// Thread reports whether the roster last resolved lists the workspace as
	// a zettel chat thread, with that roster's revision
	// (sidebar.Resolver.Thread): a thread has no row, and is answered so.
	Thread func(ws ids.WorkspaceID) (uint64, bool)
	// Now stamps every frame.
	Now func() time.Time
	// Log is the global logger: an open is the editor's, no workspace's.
	Log dlog.Logger
}

// Workspace is one workspace of an open, as the server resolved it.
type Workspace struct {
	// Ref is the workspace as the request named it.
	Ref *workspacev1.WorkspaceRef
	// Name is its display name, for the finish's failed list.
	Name string
	// Refused, when set, is the refusal it is answered with at once.
	Refused *agentreplv1.OpenTabsOpenedRefused
	// Hidden is a workspace that is not visible (hidden, or in a collapsed
	// repository section): it is answered `hidden` at once, nothing started.
	Hidden bool
	// Page is the editor loading the workspace's web page
	// (OpenTabsWorkspace.page.loading): `ready` then waits for an attached
	// page of it to have drawn. False is `page.none`: `ready` once its
	// conversation is read, with no page step.
	Page bool
}

// page is one attached page stream's facts.
type page struct {
	// serves are the workspaces its subscriptions have named.
	serves map[ids.WorkspaceID]bool
	// drawn are the workspaces it reported drawn.
	drawn map[ids.WorkspaceID]bool
}

// Coordinator serves the editor's opens.
type Coordinator struct {
	deps Deps

	mu      sync.Mutex
	pages   map[string]*page
	opening map[ids.WorkspaceID]int
	runs    map[*run]struct{}
}

// New builds a coordinator, refusing a missing collaborator.
func New(deps Deps) (*Coordinator, error) {
	switch {
	case deps.Opening == nil:
		return nil, errors.New("opentabs: the roster's opening marker is required")
	case deps.ReadConversation == nil:
		return nil, errors.New("opentabs: the conversation read is required")
	case deps.Row == nil:
		return nil, errors.New("opentabs: the roster row is required")
	case deps.Thread == nil:
		return nil, errors.New("opentabs: the roster's thread listing is required")
	case deps.Now == nil:
		return nil, errors.New("opentabs: a clock is required")
	case deps.Log == nil:
		return nil, errors.New("opentabs: a logger is required")
	}
	return &Coordinator{
		deps:    deps,
		pages:   map[string]*page{},
		opening: map[ids.WorkspaceID]int{},
		runs:    map[*run]struct{}{},
	}, nil
}

// Step takes one bring-up step of one workspace from the fleet, for EVERY
// bring-up: an open answering that workspace relays it to the editor as
// progress, and never waits on it.
func (c *Coordinator) Step(ws ids.WorkspaceID, step Step) {
	c.mu.Lock()
	runs := c.runsLocked()
	c.mu.Unlock()
	c.deps.Log.Debug(op, "a workspace's bring-up moved a step", dlog.Context{
		dlog.KeyWorkspaceID: string(ws), "step": step.Kind.String(),
	})
	for _, r := range runs {
		r.post(note{ws: ws, step: &step})
	}
}

// ResumeKind is what a session-up release did about the cut turn the
// workspace is owed (promptqueue.ResumeKind, translated where it is wired).
type ResumeKind int

const (
	// ResumeStarting is the owed resumption claimed and being delivered.
	ResumeStarting ResumeKind = iota + 1
	// ResumeStarted is the resumed turn started.
	ResumeStarted
	// ResumeFailed is a resumption that could not be claimed or delivered.
	ResumeFailed
	// ResumeNotNow is a release that resumed nothing now.
	ResumeNotNow
)

// String names the kind for the records.
func (k ResumeKind) String() string {
	switch k {
	case ResumeStarting:
		return "starting"
	case ResumeStarted:
		return "started"
	case ResumeFailed:
		return "failed"
	case ResumeNotNow:
		return "not_now"
	default:
		return fmt.Sprintf("resume(%d)", int(k))
	}
}

// Resume is one release's account of the cut turn a workspace is owed.
type Resume struct {
	Kind ResumeKind
	// Text is a failure's cause, or why nothing was resumed now.
	Text string
}

// Resumption takes one session-up release's account of a workspace's owed
// cut turn: an open answering that workspace relays its start to the editor
// as progress, and never waits on it.
func (c *Coordinator) Resumption(ws ids.WorkspaceID, resume Resume) {
	c.mu.Lock()
	runs := c.runsLocked()
	c.mu.Unlock()
	c.deps.Log.Debug(op, "a session-up release accounted for the cut turn its workspace is owed", dlog.Context{
		dlog.KeyWorkspaceID: string(ws), "resume": resume.Kind.String(), "text": resume.Text,
	})
	for _, r := range runs {
		r.post(note{ws: ws, resume: &resume})
	}
}

// PageAttached records a page stream attached to this daemon. The server
// calls it under the same lock that registers the stream, so the attach and
// the detach of one page id reach here in order.
func (c *Coordinator) PageAttached(id string) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if _, held := c.pages[id]; held {
		c.deps.Log.Error(opPage, "a page attached under an id the coordinator already holds; its facts are reset", dlog.Context{
			"page":                id,
			"invariant_violation": "the server refuses a second stream for one page id",
		})
	}
	c.pages[id] = &page{serves: map[ids.WorkspaceID]bool{}, drawn: map[ids.WorkspaceID]bool{}}
}

// PageDetached forgets a page stream that ended, and every fact it held: a
// drawn report stands only while its page's stream does.
func (c *Coordinator) PageDetached(id string) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if _, held := c.pages[id]; !held {
		c.deps.Log.Error(opPage, "a page detached that the coordinator never saw attach", dlog.Context{
			"page":                id,
			"invariant_violation": "every page stream is attached before it is detached",
		})
		return
	}
	delete(c.pages, id)
	c.deps.Log.Debug(opPage, "a page stream ended; its facts are forgotten", dlog.Context{"page": id})
}

// PageServes records that an attached page's subscription names a workspace:
// the page stream SERVES it. The first time a page serves a workspace, every
// open waiting on that workspace's page hears `page_connected`.
func (c *Coordinator) PageServes(id string, ws ids.WorkspaceID) {
	c.mu.Lock()
	p, held := c.pages[id]
	if !held {
		c.mu.Unlock()
		c.deps.Log.Debug(opPage, "a subscription of a page that already detached named a workspace; nothing is recorded", dlog.Context{
			"page": id, dlog.KeyWorkspaceID: string(ws),
		})
		return
	}
	if p.serves[ws] {
		c.mu.Unlock()
		return
	}
	p.serves[ws] = true
	runs := c.runsLocked()
	c.mu.Unlock()
	c.deps.Log.Debug(opPage, "a page stream serves a workspace", dlog.Context{"page": id, dlog.KeyWorkspaceID: string(ws)})
	for _, r := range runs {
		r.post(note{ws: ws, connected: true})
	}
}

// PageDrawn records that an attached page of a workspace has drawn its
// conversation, answering ErrUnknownPage for a page no stream holds. Every
// open waiting on the workspace's page proceeds.
func (c *Coordinator) PageDrawn(id string, ws ids.WorkspaceID) error {
	c.mu.Lock()
	p, held := c.pages[id]
	if !held {
		c.mu.Unlock()
		return fmt.Errorf("%w: %q", ErrUnknownPage, id)
	}
	again := p.drawn[ws]
	p.drawn[ws] = true
	runs := c.runsLocked()
	c.mu.Unlock()
	c.deps.Log.Debug(opPage, "a page reported its workspace drawn", dlog.Context{
		"page": id, dlog.KeyWorkspaceID: string(ws), "again": again,
	})
	for _, r := range runs {
		r.post(note{ws: ws, drawn: true})
	}
	return nil
}

// runsLocked lists the open runs. The caller holds mu.
func (c *Coordinator) runsLocked() []*run {
	out := make([]*run, 0, len(c.runs))
	for r := range c.runs {
		out = append(out, r)
	}
	return out
}

// pageFactsLocked answers whether any attached page serves, and whether any
// has drawn, the workspace. The caller holds mu.
func (c *Coordinator) pageFactsLocked(ws ids.WorkspaceID) (connected, drawn bool) {
	for _, p := range c.pages {
		connected = connected || p.serves[ws]
		drawn = drawn || p.drawn[ws]
	}
	return connected, drawn
}

// raiseLocked counts one more open in flight for ws, raising the roster's
// marker on the first. The caller holds mu.
func (c *Coordinator) raiseLocked(ws ids.WorkspaceID) {
	c.opening[ws]++
	if c.opening[ws] == 1 {
		c.deps.Opening(ws, true)
	}
}

// lower counts one open of ws answered or abandoned, lowering the roster's
// marker on the last.
func (c *Coordinator) lower(ws ids.WorkspaceID) {
	c.mu.Lock()
	defer c.mu.Unlock()
	n, ok := c.opening[ws]
	if !ok {
		c.deps.Log.Error(op, "an open was lowered that was never raised", dlog.Context{
			dlog.KeyWorkspaceID:   string(ws),
			"invariant_violation": "every lowered open was raised by the run that lowers it",
		})
		return
	}
	if n <= 1 {
		delete(c.opening, ws)
		c.deps.Opening(ws, false)
		return
	}
	c.opening[ws] = n - 1
}

// note is one thing that happened to a workspace during an open.
type note struct {
	ws ids.WorkspaceID
	// step is a fleet step, nil for any other note.
	step *Step
	// connected is a page stream serving the workspace.
	connected bool
	// drawn is a page of the workspace having drawn.
	drawn bool
	// resume is a release's account of the workspace's owed cut turn.
	resume *Resume
	// read is the conversation read having returned, with its error.
	read    bool
	readErr error
}

// run is one open, for one stream.
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

// slot is one workspace's place in an open.
type slot struct {
	ws Workspace
	// connected and drawn are its page's facts, each told once.
	connected bool
	drawn     bool
	// answered is its `opened` sent; ready is that answer being `ready`.
	answered bool
	ready    bool
	// tracked is a workspace this open marks `opening` until it is answered.
	tracked bool
	// reading is the conversation read started; read is it done.
	reading bool
	read    bool
	// servedTold is `served` relayed: a bring-up's first step that proves
	// agent-repl's services serve the workspace.
	servedTold bool
	// resumingTold is `resuming_cut_turn` relayed.
	resumingTold bool
	// fault is a failure of the open's own work (its read), which answers the
	// workspace `failed`.
	fault string
	// failedWith is the reason the workspace was answered `failed`.
	failedWith string
}

// Emit sends one frame; an error is the client gone.
type Emit func(*agentreplv1.OpenTabsResponse) error

// Run serves one open: it answers each workspace `opened` as it opens, in
// whatever order that is, and then `finished`, sending each frame through emit
// from Run's goroutine only. It returns when every workspace is answered, or
// when ctx ends or emit fails (the client went away).
func (c *Coordinator) Run(ctx context.Context, workspaces []Workspace, emit Emit) {
	r := &run{signal: make(chan struct{}, 1)}
	slots := make([]*slot, len(workspaces))
	index := make(map[ids.WorkspaceID]*slot, len(workspaces))
	// THE RUN JOINS BEFORE IT READS, so no step or page fact can fall between
	// what it read and what it is told.
	c.mu.Lock()
	c.runs[r] = struct{}{}
	for i, w := range workspaces {
		sl := &slot{ws: w}
		slots[i] = sl
		if w.Refused != nil || w.Hidden {
			continue
		}
		id := ids.WorkspaceID(w.Ref.GetId())
		index[id] = sl
		sl.tracked = true
		c.raiseLocked(id)
		if w.Page {
			sl.connected, sl.drawn = c.pageFactsLocked(id)
		}
	}
	c.mu.Unlock()
	defer func() {
		c.mu.Lock()
		delete(c.runs, r)
		c.mu.Unlock()
		// AN OPEN NO LONGER IN FLIGHT DRAWS NO `opening`: a workspace this
		// open never answered is lowered as the open ends, so a client that
		// went away cannot leave a row grey.
		for _, sl := range slots {
			if sl.tracked && !sl.answered {
				c.lower(ids.WorkspaceID(sl.ws.Ref.GetId()))
			}
		}
	}()

	c.deps.Log.Info(op, "the editor asked for workspaces to be opened", dlog.Context{
		"workspaces": len(slots),
	})
	out := &sender{now: c.deps.Now, emit: emit}
	work := &openWork{ctx: ctx, run: r}
	for _, sl := range slots {
		c.begin(out, work, sl)
	}

	for !out.gone {
		if allAnswered(slots) {
			c.finish(out, slots)
			return
		}
		select {
		case <-ctx.Done():
			c.abandoned(slots, ctx.Err())
			return
		case <-r.signal:
		}
		for _, n := range r.take() {
			sl, ok := index[n.ws]
			if !ok || sl.answered {
				continue
			}
			c.apply(out, sl, n)
			c.answerIfOpen(out, sl)
		}
	}
	c.abandoned(slots, out.err)
}

// sender is one open's frame writer: it stamps every frame and remembers a
// client that went away, after which nothing more is sent.
type sender struct {
	now  func() time.Time
	emit Emit
	gone bool
	err  error
}

func (s *sender) send(frame *agentreplv1.OpenTabsResponse) {
	if s.gone {
		return
	}
	if err := s.emit(frame); err != nil {
		s.gone, s.err = true, err
	}
}

func (s *sender) step(ref *workspacev1.WorkspaceRef, set func(*agentreplv1.OpenTabsStep)) {
	step := &agentreplv1.OpenTabsStep{Workspace: ref, AtMs: s.now().UnixMilli()}
	set(step)
	s.send(&agentreplv1.OpenTabsResponse{Frame: &agentreplv1.OpenTabsResponse_Step{Step: step}})
}

func (s *sender) opened(ref *workspacev1.WorkspaceRef, set func(*agentreplv1.OpenTabsOpened)) {
	opened := &agentreplv1.OpenTabsOpened{Workspace: ref, AtMs: s.now().UnixMilli()}
	set(opened)
	s.send(&agentreplv1.OpenTabsResponse{Frame: &agentreplv1.OpenTabsResponse_Opened{Opened: opened}})
}

// begin sends what one workspace's open already knows at the run's start: a
// refusal or a hidden answer at once, else the facts already standing.
func (c *Coordinator) begin(out *sender, work *openWork, sl *slot) {
	ref := sl.ws.Ref
	fields := dlog.Context{dlog.KeyWorkspaceID: ref.GetId(), "name": sl.ws.Name}
	switch {
	case sl.ws.Refused != nil:
		sl.answered = true
		fields["cause"] = refusedCause(sl.ws.Refused)
		c.deps.Log.Info(op, "a workspace the editor asked to open was refused", fields)
		out.opened(ref, func(o *agentreplv1.OpenTabsOpened) {
			o.Outcome = &agentreplv1.OpenTabsOpened_Refused{Refused: sl.ws.Refused}
		})
		return
	case sl.ws.Hidden:
		sl.answered = true
		c.deps.Log.Info(op, "a workspace the editor asked to open is hidden or collapsed; nothing was started", fields)
		out.opened(ref, func(o *agentreplv1.OpenTabsOpened) {
			o.Outcome = &agentreplv1.OpenTabsOpened_Hidden{Hidden: &agentreplv1.OpenTabsOpenedHidden{}}
		})
		return
	}
	c.startRead(work, sl)
	if sl.connected || sl.drawn {
		sl.connected = true
		out.step(ref, pageConnectedStep)
	}
	if sl.drawn {
		out.step(ref, pageDrawnStep)
	}
	c.answerIfOpen(out, sl)
}

// apply folds one note into a slot, relaying the steps the editor prints.
func (c *Coordinator) apply(out *sender, sl *slot, n note) {
	ref := sl.ws.Ref
	switch {
	case n.step != nil:
		switch {
		case n.step.relayed():
			out.step(ref, func(s *agentreplv1.OpenTabsStep) { relay(s, *n.step) })
		case n.step.serves() && !sl.servedTold:
			sl.servedTold = true
			out.step(ref, servedStep)
		}
	case n.resume != nil:
		if n.resume.Kind == ResumeStarting && !sl.resumingTold {
			sl.resumingTold = true
			out.step(ref, resumingCutTurnStep)
		}
	case n.read:
		sl.read = true
		if n.readErr != nil {
			sl.fault = "the conversation could not be read: " + n.readErr.Error()
			return
		}
		out.step(ref, conversationReadStep)
	case n.connected:
		if sl.ws.Page && !sl.connected {
			sl.connected = true
			out.step(ref, pageConnectedStep)
		}
	case n.drawn:
		if !sl.ws.Page || sl.drawn {
			return
		}
		// A DRAWN PAGE IS A CONNECTED ONE: a page that reported drawn before
		// any of its subscriptions named the workspace still attached first.
		if !sl.connected {
			sl.connected = true
			out.step(ref, pageConnectedStep)
		}
		sl.drawn = true
		out.step(ref, pageDrawnStep)
	}
}

// openWork is what one open's own work runs under: its lifetime, and the run
// its results are posted to.
type openWork struct {
	ctx context.Context
	run *run
}

// startRead reads the workspace's conversation once, off the run's goroutine,
// posting the result to the run.
func (c *Coordinator) startRead(work *openWork, sl *slot) {
	if sl.reading {
		return
	}
	sl.reading = true
	ws := ids.WorkspaceID(sl.ws.Ref.GetId())
	go func() {
		err := c.deps.ReadConversation(work.ctx, ws)
		work.run.post(note{ws: ws, read: true, readErr: err})
	}()
}

// answerIfOpen sends the workspace's `opened` once it is open: its
// conversation read, and drawn when a page is loading; or failed when the
// read failed. No shim, vendor or network state gates it.
func (c *Coordinator) answerIfOpen(out *sender, sl *slot) {
	if sl.answered || out.gone {
		return
	}
	ref := sl.ws.Ref
	fields := dlog.Context{dlog.KeyWorkspaceID: ref.GetId(), "name": sl.ws.Name, "page": sl.ws.Page}
	ws := ids.WorkspaceID(ref.GetId())
	reason := ""
	switch {
	case sl.fault != "":
		reason = sl.fault
	case sl.read && (!sl.ws.Page || sl.drawn):
	default:
		return
	}
	// THE OPEN ENDS BEFORE THE ROW IS TAKEN: the answer's row is the one the
	// roster resolves once this open no longer marks the workspace opening,
	// with that roster's revision, taken together.
	sl.answered = true
	c.lower(ws)
	row, thread, revision := c.answerListing(ws)
	fields["roster_revision"] = revision
	fields["thread"] = thread
	if reason != "" {
		sl.failedWith = reason
		fields["reason"] = reason
		c.deps.Log.Info(op, "a workspace's open failed; its tab shows the fault", fields)
		failed := &agentreplv1.OpenTabsOpenedFailed{Reason: reason, RosterRevision: revision}
		switch {
		case thread:
			failed.Listing = &agentreplv1.OpenTabsOpenedFailed_Thread{Thread: &agentreplv1.OpenTabsOpenedThread{}}
		case row != nil:
			failed.Listing = &agentreplv1.OpenTabsOpenedFailed_Row{Row: row}
		}
		out.opened(ref, func(o *agentreplv1.OpenTabsOpened) {
			o.Outcome = &agentreplv1.OpenTabsOpened_Failed{Failed: failed}
		})
		return
	}
	sl.ready = true
	c.deps.Log.Info(op, "a workspace is open; its tab may show", fields)
	ready := &agentreplv1.OpenTabsOpenedReady{RosterRevision: revision}
	switch {
	case thread:
		ready.Listing = &agentreplv1.OpenTabsOpenedReady_Thread{Thread: &agentreplv1.OpenTabsOpenedThread{}}
	case row != nil:
		ready.Listing = &agentreplv1.OpenTabsOpenedReady_Row{Row: row}
	}
	out.opened(ref, func(o *agentreplv1.OpenTabsOpened) {
		o.Outcome = &agentreplv1.OpenTabsOpened_Ready{Ready: ready}
	})
}

// answerListing takes how the roster lists the workspace for an answer, with
// that roster's revision: its row, or (THREAD) a chat thread, which has none.
// A roster that lists it neither way is an invariant violation (an open names
// only registered workspaces): it is recorded, and the answer goes without a
// listing rather than never.
func (c *Coordinator) answerListing(ws ids.WorkspaceID) (*frontendv1.RosterRow, bool, uint64) {
	if row, revision, ok := c.deps.Row(ws); ok {
		return row, false, revision
	}
	if revision, ok := c.deps.Thread(ws); ok {
		return nil, true, revision
	}
	c.deps.Log.Error(op, "the roster lists a workspace being answered open neither as a row nor as a thread; the answer carries no listing", dlog.Context{
		dlog.KeyWorkspaceID:   string(ws),
		"invariant_violation": "every workspace an open answers is registered, so the roster lists it",
	})
	return nil, false, 0
}

// allAnswered reports whether every workspace has had its `opened`.
func allAnswered(slots []*slot) bool {
	for _, sl := range slots {
		if !sl.answered {
			return false
		}
	}
	return true
}

// abandoned records an open whose client went away before every workspace
// was answered.
func (c *Coordinator) abandoned(slots []*slot, cause error) {
	answered := 0
	for _, sl := range slots {
		if sl.answered {
			answered++
		}
	}
	reason := "the client went away"
	if cause != nil {
		reason = cause.Error()
	}
	c.deps.Log.Info(op, "the editor's open ended before every workspace was answered", dlog.Context{
		"answered": answered, "workspaces": len(slots), "cause": reason,
	})
}

// finish sends the open's last frame.
func (c *Coordinator) finish(out *sender, slots []*slot) {
	done := &agentreplv1.OpenTabsFinished{Total: uint32(len(slots))}
	var failedNames []string
	for _, sl := range slots {
		switch {
		case sl.ready:
			done.Ready++
		case sl.failedWith != "":
			done.Failed = append(done.Failed, &agentreplv1.OpenTabsFailedWorkspace{Workspace: sl.ws.Ref, Name: sl.ws.Name})
			failedNames = append(failedNames, sl.ws.Name)
		}
	}
	c.deps.Log.Info(op, "every workspace of the open was answered", dlog.Context{
		"ready": done.Ready, "total": done.Total, "failed": failedNames,
	})
	out.send(&agentreplv1.OpenTabsResponse{Frame: &agentreplv1.OpenTabsResponse_Finished{Finished: done}})
}

func servedStep(s *agentreplv1.OpenTabsStep) {
	s.Step = &agentreplv1.OpenTabsStep_Served{Served: &agentreplv1.OpenTabsStepServed{}}
}

func pageConnectedStep(s *agentreplv1.OpenTabsStep) {
	s.Step = &agentreplv1.OpenTabsStep_PageConnected{PageConnected: &agentreplv1.OpenTabsStepPageConnected{}}
}

func conversationReadStep(s *agentreplv1.OpenTabsStep) {
	s.Step = &agentreplv1.OpenTabsStep_ConversationRead{ConversationRead: &agentreplv1.OpenTabsStepConversationRead{}}
}

func resumingCutTurnStep(s *agentreplv1.OpenTabsStep) {
	s.Step = &agentreplv1.OpenTabsStep_ResumingCutTurn{ResumingCutTurn: &agentreplv1.OpenTabsStepResumingCutTurn{}}
}

func pageDrawnStep(s *agentreplv1.OpenTabsStep) {
	s.Step = &agentreplv1.OpenTabsStep_PageDrawn{PageDrawn: &agentreplv1.OpenTabsStepPageDrawn{}}
}

// relay renders a relayed fleet step as its OpenTabsStep arm.
func relay(out *agentreplv1.OpenTabsStep, step Step) {
	switch step.Kind {
	case StepStartingSession:
		out.Step = &agentreplv1.OpenTabsStep_StartingSession{StartingSession: &agentreplv1.OpenTabsStepStartingSession{}}
	case StepWaking:
		out.Step = &agentreplv1.OpenTabsStep_Waking{Waking: &agentreplv1.OpenTabsStepWaking{}}
	case StepResuming:
		out.Step = &agentreplv1.OpenTabsStep_Resuming{Resuming: &agentreplv1.OpenTabsStepResuming{}}
	case StepVendorRetrying:
		out.Step = &agentreplv1.OpenTabsStep_VendorRetrying{VendorRetrying: &agentreplv1.OpenTabsStepVendorRetrying{Attempt: step.Attempt}}
	case StepVendorRejected:
		out.Step = &agentreplv1.OpenTabsStep_VendorRejected{VendorRejected: &agentreplv1.OpenTabsStepVendorRejected{Cause: step.Text}}
	case StepVendorFailed:
		out.Step = &agentreplv1.OpenTabsStep_VendorFailed{VendorFailed: &agentreplv1.OpenTabsStepVendorFailed{}}
	case StepColdGate:
		out.Step = &agentreplv1.OpenTabsStep_ColdGate{ColdGate: &agentreplv1.OpenTabsStepColdGate{}}
	case StepOffline:
		out.Step = &agentreplv1.OpenTabsStep_Offline{Offline: &agentreplv1.OpenTabsStepOffline{}}
	}
}

// refusedCause names a refusal's arm for the record.
func refusedCause(r *agentreplv1.OpenTabsOpenedRefused) string {
	switch r.GetCause().(type) {
	case *agentreplv1.OpenTabsOpenedRefused_UnknownWorkspace:
		return "unknown_workspace"
	case *agentreplv1.OpenTabsOpenedRefused_WorkspaceRefMismatch:
		return "workspace_ref_mismatch"
	case *agentreplv1.OpenTabsOpenedRefused_TransferringAway:
		return "transferring_away"
	case *agentreplv1.OpenTabsOpenedRefused_NotYetAdopted:
		return "not_yet_adopted"
	default:
		return "unset"
	}
}
