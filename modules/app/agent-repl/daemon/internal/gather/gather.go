// Package gather runs WORKSPACE GATHERS (agentrepl.v1.GatherWorkspaces): one
// workspace (the CALLER) asks many workspaces the same question and is told
// their answers as ONE update, then each later answer as its own notice.
//
// A TARGET'S ANSWER IS THE FINAL RESPONSE OF THE TURN ITS PROMPT WENT INTO.
// The prompt is minted a turn when it is submitted; every move the daemon
// makes of it to another turn is a durable turn succession (wsm
// turn_successions), and the answer is the feed's concluded answer for the
// turn at the end of that chain (wsm.SucceedingTurn). Targets never reply on
// purpose.
//
// ONE LOOP, IN ORDER. Every input -- a gather being made, a fold, an answer, a
// close, a deadline, the boot's re-arm -- is one event on one loop, handled in
// the order reported. A gather is made ON the loop, so an answer to a prompt
// it just submitted queues behind the gather's own rows and is never seen
// before them; and the feed tells a fold before the joined turn's answer, so
// the fold's succession stands by the time the answer is matched.
//
// THE FIRST UPDATE IS A LATCH OR A DEADLINE, never a poll: it is told the
// moment the last target resolves, or when the deadline's timer fires, from
// whichever event comes first, and the gather's row records it told so the
// other never tells it again. Every later resolution is told as its own
// notice. Each notice is submitted under a key derived from the gather, so a
// notice a restart cut between its submission and its record is refused as a
// duplicate on the retry, never told twice.
//
// THE NOTICE TAKES THE WORKSPACE NOTICE PATH (PROMPT_ORIGIN_WORKSPACE_NOTICE)
// subscriptions and parent updates take: a deferred prompt that waits behind a
// running turn, wakes an idle agent, and draws no feed row. Its words carry
// the agentrepl.v1.GatherNotice as protojson.
package gather

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"time"

	"google.golang.org/protobuf/encoding/protojson"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/briefprompt"
	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/noticeloop"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/wsm"
)

// op is the operation every record of this package carries.
const op = "daemon.gather"

// Timeout is how long after a gather is made its first update waits for every
// target before it is told with what has arrived. It is the OWNER'S RULING
// (2026-11-10), not a measurement: it bounds how long a caller waits on its
// slowest target, which no measurement of the daemon decides.
const Timeout = 5 * time.Minute

// BriefNotice is the brief a gather's notice is composed from.
const BriefNotice = "gather-notice"

// Origin is every notice's prompt origin: the workspace notice path.
const Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_NOTICE

// Store is the registry, the gathers' rows and the turn successions, as the
// service reads and writes them.
type Store interface {
	Workspace(ctx context.Context, id wsm.WorkspaceID) (wsm.Workspace, error)
	ListWorkspaces(ctx context.Context) ([]wsm.Workspace, error)
	HeldPrompts(ctx context.Context, id wsm.WorkspaceID) ([]wsm.HeldPrompt, error)
	Turn(ctx context.Context, id wsm.WorkspaceID, turn wsm.TurnID) (wsm.Turn, error)
	RecordTurnSuccession(ctx context.Context, m wsm.TurnSuccession) error
	SucceedingTurn(ctx context.Context, turn wsm.TurnID) (wsm.TurnID, error)
	PutGather(ctx context.Context, g wsm.Gather) error
	Gather(ctx context.Context, id wsm.GatherID) (wsm.Gather, error)
	GatherByKey(ctx context.Context, key string) (wsm.Gather, bool, error)
	UnsettledGathers(ctx context.Context) ([]wsm.Gather, error)
	GathersOutstandingOn(ctx context.Context, ws wsm.WorkspaceID) ([]wsm.Gather, error)
	ResolveGatherTarget(ctx context.Context, id wsm.GatherID, ws wsm.WorkspaceID, r wsm.GatherResolution) (bool, error)
	MarkGatherFirstTold(ctx context.Context, id wsm.GatherID, told wsm.GatherFirstTold) error
	MarkGatherTargetTold(ctx context.Context, id wsm.GatherID, ws wsm.WorkspaceID, at time.Time) error
}

// Deps are the service's collaborators: everything but the prompt handler,
// which is handed to Run.
type Deps struct {
	DB Store
	// Status answers a workspace's status as an agent reads it
	// (workspace.Verbs.Status).
	Status func(ctx context.Context, ws ids.WorkspaceID) (*agentreplv1.WorkspaceStatus, error)
	// Briefs loads and splices a brief at use time.
	Briefs merge.BriefLoader
	// Clock stamps every instant and times every deadline.
	Clock clock.Clock
	Log   dlog.Surfaces
}

// Submit prompts one target with the gather's prompt under KEY, exactly as
// `workspace prompt` would (agentrepl.v1.SubmitPrompt), answering the turn it
// was minted, or the submission's typed refusal. An error is a failure to
// submit at all.
type Submit func(ctx context.Context, target wsm.Workspace, key string) (ids.TurnID, *agentreplv1.SubmitPromptError, error)

// Request is one gather to make.
type Request struct {
	Caller ids.WorkspaceID
	// Names are the targets, by workspace name, in the caller's order.
	Names []string
	// PromptText is the prompt's text, kept for the status read.
	PromptText string
	// Key is the request's idempotency key.
	Key    string
	Submit Submit
}

// Service runs every gather. Its event methods only append and never block.
type Service struct {
	deps Deps
	loop *noticeloop.Loop[event]
	// armed are the gathers whose deadline timer this process runs, touched
	// only on the loop.
	armed map[wsm.GatherID]bool
}

// New builds the service.
func New(deps Deps) *Service {
	s := &Service{deps: deps, armed: map[wsm.GatherID]bool{}}
	s.loop = noticeloop.New(func(e event) {
		if c, ok := e.(createEvent); ok {
			c.reply <- createReply{err: ErrStopped}
			return
		}
		deps.Log.Global().Info(op, "a gather event came after the service stopped, as the daemon stood down; the next boot re-arms from the rows", dlog.Context{"event": fmt.Sprintf("%T", e)})
	})
	return s
}

// ErrStopped is a gather asked for after the service stopped.
var ErrStopped = errors.New("gather: the gather service has stopped")

// event is one input to the loop.
type event interface {
	handle(ctx context.Context, s *Service, prompts prompthandler.Submitter)
}

// Run handles every reported event, in order, until ctx ends, re-arming every
// unsettled gather first, then handles every event still pending.
func (s *Service) Run(ctx context.Context, prompts prompthandler.Submitter) {
	s.loop.Report(bootEvent{})
	s.loop.Run(ctx, func(handleCtx context.Context, e event) {
		// THE TIMERS LIVE AS LONG AS THE RUN, not as one event's handling.
		e.handle(withRun(handleCtx, ctx), s, prompts)
	})
}

// runKey carries Run's own context to the handlers, which arm timers on it.
type runKey struct{}

func withRun(ctx, run context.Context) context.Context { return context.WithValue(ctx, runKey{}, run) }

func runOf(ctx context.Context) context.Context {
	if run, ok := ctx.Value(runKey{}).(context.Context); ok {
		return run
	}
	panic("gather: a handler ran outside Run")
}

// ---- the feed's answer sink (feed.AnswerSink) ----

// AnswerProse implements feed.AnswerSink: prose in flight answers nothing.
func (s *Service) AnswerProse(ids.WorkspaceID, ids.TurnID, string) {}

// PromptFolded implements feed.AnswerSink.
func (s *Service) PromptFolded(ws ids.WorkspaceID, turn, into ids.TurnID) {
	s.loop.Report(foldEvent{ws: ws, turn: turn, into: into})
}

// TurnAnswered implements feed.AnswerSink.
func (s *Service) TurnAnswered(ws ids.WorkspaceID, turn ids.TurnID, markdown string) {
	s.loop.Report(answerEvent{ws: ws, turn: turn, text: markdown})
}

// TurnEnded implements feed.AnswerSink: an end without an answer resolves
// nothing; a target whose turn ended so is told outstanding, with why.
func (s *Service) TurnEnded(ids.WorkspaceID, ids.TurnID) {}

// ---- the lifecycle edges (wsedges.Observer) ----

// Edges is the service's lifecycle observer (wsedges.Observer): a close
// resolves every outstanding target on the workspace. It is a value of its
// own because the feed's answer sink also has a TurnEnded, of another shape.
func (s *Service) Edges() Edges { return Edges{s: s} }

// Edges observes the lifecycle edges for the service.
type Edges struct{ s *Service }

// Closed implements wsedges.Observer.
func (e Edges) Closed(before wsm.Workspace) { e.s.loop.Report(closeEvent{ws: before.ID}) }

// TurnStarted implements wsedges.Observer.
func (Edges) TurnStarted(ids.WorkspaceID) {}

// TurnEnded implements wsedges.Observer: the feed's answer, not the edge, is
// what resolves a target.
func (Edges) TurnEnded(ids.WorkspaceID) {}

// TurnEndedUnobserved implements wsedges.Observer.
func (Edges) TurnEndedUnobserved(ids.WorkspaceID) {}

// NeedsInput implements wsedges.Observer.
func (Edges) NeedsInput(ids.WorkspaceID) {}

// Landed implements wsedges.Observer.
func (Edges) Landed(ids.WorkspaceID) {}

// PullRequestOwned implements wsedges.Observer.
func (Edges) PullRequestOwned(ids.WorkspaceID, wsm.PullRequestKey) {}

// ---- making a gather ----

// createReply is the loop's answer to a create.
type createReply struct {
	resp *agentreplv1.GatherWorkspacesResponse
	err  error
}

type createEvent struct {
	req   Request
	reply chan createReply
}

// Create makes a gather on the loop and answers it, or ctx's end.
func (s *Service) Create(ctx context.Context, req Request) (*agentreplv1.GatherWorkspacesResponse, error) {
	reply := make(chan createReply, 1)
	s.loop.Report(createEvent{req: req, reply: reply})
	select {
	case r := <-reply:
		return r.resp, r.err
	case <-ctx.Done():
		return nil, ctx.Err()
	}
}

func (e createEvent) handle(ctx context.Context, s *Service, prompts prompthandler.Submitter) {
	resp, err := s.create(ctx, e.req)
	e.reply <- createReply{resp: resp, err: err}
}

// create makes the gather REQ asks for.
func (s *Service) create(ctx context.Context, req Request) (*agentreplv1.GatherWorkspacesResponse, error) {
	log := s.deps.Log.Global()
	fields := dlog.Context{"caller": string(req.Caller), "key": req.Key, "targets": len(req.Names)}
	caller, err := s.deps.DB.Workspace(ctx, req.Caller)
	switch {
	case errors.Is(err, wsm.ErrNotFound):
		return gatherError(&agentreplv1.GatherWorkspacesError{Cause: &agentreplv1.GatherWorkspacesError_UnknownCaller{UnknownCaller: &agentreplv1.GatherWorkspacesUnknownCaller{}}}), nil
	case err != nil:
		log.Error(op, "could not read the gather's caller; no gather is made", dlog.WithCause(fields, err))
		return nil, fmt.Errorf("read the caller %q: %w", req.Caller, err)
	case caller.Closed:
		return gatherError(&agentreplv1.GatherWorkspacesError{Cause: &agentreplv1.GatherWorkspacesError_CallerClosed{CallerClosed: &agentreplv1.GatherWorkspacesCallerClosed{}}}), nil
	}
	if made, found, err := s.deps.DB.GatherByKey(ctx, req.Key); err != nil {
		log.Error(op, "could not read whether the key already made a gather; no gather is made", dlog.WithCause(fields, err))
		return nil, fmt.Errorf("read the gather of key %q: %w", req.Key, err)
	} else if found {
		log.Info(op, "the request repeats a key that already made a gather; it is answered and nobody is prompted again", dlog.Merge(fields, dlog.Context{"gather": string(made.ID)}))
		return gatherSuccess(made, nil), nil
	}
	registry, err := s.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error(op, "could not read the registry to resolve the targets' names; no gather is made", dlog.WithCause(fields, err))
		return nil, fmt.Errorf("read the registry: %w", err)
	}
	now := s.deps.Clock.Now()
	g := wsm.Gather{ID: wsm.NewGatherID(), Caller: caller.ID, CallerName: caller.Name, CallerDir: caller.Dir, Key: req.Key,
		PromptText: req.PromptText, CreatedAt: now, DeadlineAt: now.Add(Timeout)}
	fields["gather"] = string(g.ID)
	var refused []*agentreplv1.GatherRefusedTarget
	for _, name := range dedupe(req.Names) {
		target, cause := resolveName(registry, name, caller.ID)
		if cause == nil {
			cause, err = s.precheck(ctx, target)
			if err != nil {
				log.Error(op, "could not read a target's status; the gather is not made", dlog.WithCause(dlog.Merge(fields, dlog.Context{"target": name}), err))
				return nil, err
			}
		}
		if cause == nil {
			turn, refusal, err := req.Submit(ctx, target, req.Key+"/"+string(target.ID))
			switch {
			case err != nil:
				log.Error(op, "could not submit the gather's prompt to a target; the gather is not made, and the targets prompted before it answer nobody",
					dlog.WithCause(dlog.Merge(fields, dlog.Context{"target": name, "prompted": len(g.Targets)}), err))
				return nil, fmt.Errorf("submit to %q: %w", name, err)
			case refusal != nil:
				cause = &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_PromptRefused{PromptRefused: refusal}}
			default:
				g.Targets = append(g.Targets, wsm.GatherTarget{Workspace: target.ID, Name: target.Name, Dir: target.Dir, Turn: turn})
				continue
			}
		}
		refused = append(refused, &agentreplv1.GatherRefusedTarget{Name: name, Cause: cause})
	}
	if len(g.Targets) == 0 {
		log.Info(op, "every target refused the gather's prompt; no gather is made", dlog.Merge(fields, dlog.Context{"refused": len(refused)}))
		return gatherError(&agentreplv1.GatherWorkspacesError{Cause: &agentreplv1.GatherWorkspacesError_NoneAccepted{NoneAccepted: &agentreplv1.GatherWorkspacesNoneAccepted{Refused: refused}}}), nil
	}
	if err := s.deps.DB.PutGather(ctx, g); err != nil {
		log.Error(op, "could not record the gather; its targets were prompted but their answers reach nobody", dlog.WithCause(dlog.Merge(fields, dlog.Context{"prompted": len(g.Targets)}), err))
		return nil, fmt.Errorf("record the gather: %w", err)
	}
	log.Info(op, "made a gather", dlog.Merge(fields, dlog.Context{"accepted": len(g.Targets), "refused": len(refused), "deadline_at": g.DeadlineAt}))
	s.arm(ctx, g)
	return gatherSuccess(g, refused), nil
}

// dedupe answers NAMES with each repeat after the first dropped.
func dedupe(names []string) []string {
	seen := map[string]bool{}
	var out []string
	for _, n := range names {
		if !seen[n] {
			seen[n] = true
			out = append(out, n)
		}
	}
	return out
}

// resolveName answers the workspace NAME names in REGISTRY, or why it names
// none: an open workspace is preferred, and a name only closed workspaces have
// is closed.
func resolveName(registry []wsm.Workspace, name string, caller ids.WorkspaceID) (wsm.Workspace, *agentreplv1.GatherRefusal) {
	var open []wsm.Workspace
	closed := false
	for _, w := range registry {
		if w.Name != name {
			continue
		}
		if w.Closed {
			closed = true
		} else {
			open = append(open, w)
		}
	}
	switch {
	case len(open) == 1 && open[0].ID == caller:
		return wsm.Workspace{}, &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_IsCaller{IsCaller: &agentreplv1.GatherRefusalIsCaller{}}}
	case len(open) == 1:
		return open[0], nil
	case len(open) > 1:
		refs := make([]*workspacev1.WorkspaceRef, 0, len(open))
		for _, w := range open {
			refs = append(refs, w.Ref())
		}
		return wsm.Workspace{}, &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_AmbiguousName{AmbiguousName: &agentreplv1.GatherRefusalAmbiguousName{Workspaces: refs}}}
	case closed:
		return wsm.Workspace{}, &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_Closed{Closed: &agentreplv1.GatherRefusalClosed{}}}
	}
	return wsm.Workspace{}, &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_UnknownWorkspace{UnknownWorkspace: &agentreplv1.GatherRefusalUnknownWorkspace{}}}
}

// precheck answers why TARGET's status refuses the gather before anything is
// submitted: an ask waiting on the user, or a merge in progress, each of which
// would hold the prompt on something other than the target's own work.
func (s *Service) precheck(ctx context.Context, target wsm.Workspace) (*agentreplv1.GatherRefusal, error) {
	status, err := s.deps.Status(ctx, target.ID)
	if err != nil {
		return nil, fmt.Errorf("read the status of %q: %w", target.Name, err)
	}
	switch status.GetStanding().GetOpen().GetActivity().GetActivity().(type) {
	case *agentreplv1.WorkspaceStatusActivity_NeedsInput:
		return &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_NeedsInput{NeedsInput: &agentreplv1.GatherRefusalNeedsInput{}}}, nil
	case *agentreplv1.WorkspaceStatusActivity_Merging:
		return &agentreplv1.GatherRefusal{Cause: &agentreplv1.GatherRefusal_Merging{Merging: &agentreplv1.GatherRefusalMerging{}}}, nil
	}
	return nil, nil
}

func gatherError(e *agentreplv1.GatherWorkspacesError) *agentreplv1.GatherWorkspacesResponse {
	return &agentreplv1.GatherWorkspacesResponse{Result: &agentreplv1.GatherWorkspacesResponse_Error{Error: e}}
}

func gatherSuccess(g wsm.Gather, refused []*agentreplv1.GatherRefusedTarget) *agentreplv1.GatherWorkspacesResponse {
	accepted := make([]*agentreplv1.GatherTarget, 0, len(g.Targets))
	for _, t := range g.Targets {
		accepted = append(accepted, targetProto(t))
	}
	return &agentreplv1.GatherWorkspacesResponse{Result: &agentreplv1.GatherWorkspacesResponse_Success{Success: &agentreplv1.GatherWorkspacesSuccess{
		Gather: &agentreplv1.GatherRef{Id: string(g.ID)}, Accepted: accepted, Refused: refused, DeadlineAtUnixMs: g.DeadlineAt.UnixMilli(),
	}}}
}

// ---- the deadline ----

// arm starts G's deadline timer, once per process, unless its first update was
// told. It lives as long as Run.
func (s *Service) arm(ctx context.Context, g wsm.Gather) {
	if g.FirstTold != nil || s.armed[g.ID] {
		return
	}
	s.armed[g.ID] = true
	run := runOf(ctx)
	wait := g.DeadlineAt.Sub(s.deps.Clock.Now())
	fired := s.deps.Clock.After(wait)
	go func() {
		select {
		case <-fired:
			s.loop.Report(deadlineEvent{gather: g.ID})
		case <-run.Done():
		}
	}()
}

type deadlineEvent struct{ gather wsm.GatherID }

func (e deadlineEvent) handle(ctx context.Context, s *Service, prompts prompthandler.Submitter) {
	delete(s.armed, e.gather)
	g, err := s.deps.DB.Gather(ctx, e.gather)
	if err != nil {
		s.deps.Log.Global().Error(op, "could not read the gather whose deadline passed; its first update is told at the next boot", dlog.WithCause(dlog.Context{"gather": string(e.gather)}, err))
		return
	}
	if g.FirstTold != nil {
		return
	}
	s.tellFirst(ctx, prompts, g, wsm.GatherTimedOut)
}

// ---- the edges that resolve a target ----

type foldEvent struct {
	ws         ids.WorkspaceID
	turn, into ids.TurnID
}

func (e foldEvent) handle(ctx context.Context, s *Service, _ prompthandler.Submitter) {
	ws := e.ws
	fields := dlog.Context{"workspace": string(ws), "turn": string(e.turn), "folded_into": string(e.into)}
	// ONLY A WORKSPACE A GATHER WAITS ON RECORDS ITS FOLDS: a fold of a
	// gather's prompt can only come after the gather was made on this loop.
	gathers, err := s.deps.DB.GathersOutstandingOn(ctx, ws)
	if err != nil {
		s.deps.Log.Global().Error(op, "could not read whether a gather waits on the workspace; a fold of its prompt is not followed", dlog.WithCause(fields, err))
		return
	}
	if len(gathers) == 0 {
		return
	}
	if err := s.deps.DB.RecordTurnSuccession(ctx, wsm.TurnSuccession{Workspace: ws, Turn: e.turn, Kind: wsm.SuccessionFolded, Successor: e.into, At: s.deps.Clock.Now()}); err != nil {
		s.deps.Log.Global().Error(op, "could not record a fold; a gather waiting on the folded prompt does not see the joined turn's answer", dlog.WithCause(fields, err))
	}
}

type answerEvent struct {
	ws   ids.WorkspaceID
	turn ids.TurnID
	text string
}

func (e answerEvent) handle(ctx context.Context, s *Service, prompts prompthandler.Submitter) {
	fields := dlog.Context{"workspace": string(e.ws), "turn": string(e.turn)}
	s.resolveOn(ctx, prompts, e.ws, fields, func(t wsm.GatherTarget) (*wsm.GatherResolution, error) {
		carrying, err := s.deps.DB.SucceedingTurn(ctx, t.Turn)
		if err != nil || carrying != e.turn {
			return nil, err
		}
		return &wsm.GatherResolution{Kind: wsm.GatherAnswered, At: s.deps.Clock.Now(), Text: e.text}, nil
	})
}

type closeEvent struct{ ws ids.WorkspaceID }

func (e closeEvent) handle(ctx context.Context, s *Service, prompts prompthandler.Submitter) {
	s.resolveOn(ctx, prompts, e.ws, dlog.Context{"workspace": string(e.ws)}, func(wsm.GatherTarget) (*wsm.GatherResolution, error) {
		return &wsm.GatherResolution{Kind: wsm.GatherClosed, At: s.deps.Clock.Now()}, nil
	})
}

// resolveOn resolves, with what RESOLVE answers, every outstanding target on
// WS of every gather waiting on it, then tells what each gather now owes.
func (s *Service) resolveOn(ctx context.Context, prompts prompthandler.Submitter, ws ids.WorkspaceID, fields dlog.Context, resolve func(wsm.GatherTarget) (*wsm.GatherResolution, error)) {
	log := s.deps.Log.Global()
	gathers, err := s.deps.DB.GathersOutstandingOn(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the gathers waiting on the workspace; nothing is resolved", dlog.WithCause(fields, err))
		return
	}
	for _, g := range gathers {
		gfields := dlog.Merge(fields, dlog.Context{"gather": string(g.ID)})
		for _, t := range g.Targets {
			if t.Workspace != ws || !t.Outstanding() {
				continue
			}
			r, err := resolve(t)
			if err != nil {
				log.Error(op, "could not follow the target's prompt to the turn carrying it; it stays outstanding", dlog.WithCause(gfields, err))
				continue
			}
			if r == nil {
				continue
			}
			if _, err := s.deps.DB.ResolveGatherTarget(ctx, g.ID, ws, *r); err != nil {
				log.Error(op, "could not record the target's resolution; it stays outstanding", dlog.WithCause(gfields, err))
				continue
			}
			log.Info(op, "a gather's target resolved", dlog.Merge(gfields, dlog.Context{"resolution": string(r.Kind)}))
		}
		s.settle(ctx, prompts, g.ID)
	}
}

// ---- telling ----

// settle tells what gather ID now owes its caller, as its row now stands: the
// first update once every target resolved, its deadline armed while one is
// outstanding, and after the first update each resolution not yet told.
func (s *Service) settle(ctx context.Context, prompts prompthandler.Submitter, id wsm.GatherID) {
	g, err := s.deps.DB.Gather(ctx, id)
	if err != nil {
		s.deps.Log.Global().Error(op, "could not read the gather to tell what it owes; the next event or boot tells it", dlog.WithCause(dlog.Context{"gather": string(id)}, err))
		return
	}
	if g.FirstTold == nil {
		if g.AllResolved() {
			s.tellFirst(ctx, prompts, g, wsm.GatherAllResolved)
			return
		}
		s.arm(ctx, g)
		return
	}
	for _, t := range g.Targets {
		if !t.Outstanding() && t.ToldAt == nil {
			s.tellLater(ctx, prompts, g, t)
		}
	}
}

// tellFirst tells G's first update, sent by TRIGGER, and records it told.
func (s *Service) tellFirst(ctx context.Context, prompts prompthandler.Submitter, g wsm.Gather, trigger wsm.GatherTrigger) {
	fields := dlog.Context{"gather": string(g.ID), "caller": string(g.Caller), "trigger": string(trigger)}
	resolved, outstanding, err := s.standing(ctx, g)
	if err != nil {
		s.deps.Log.Global().Error(op, "could not read where the gather's targets stand; its first update is told at the next event or boot", dlog.WithCause(fields, err))
		return
	}
	notice := &agentreplv1.GatherNotice{Gather: &agentreplv1.GatherRef{Id: string(g.ID)}, Notice: &agentreplv1.GatherNotice_FirstUpdate{FirstUpdate: &agentreplv1.GatherNoticeFirstUpdate{
		Trigger: triggerProto(trigger), Resolved: resolved, Outstanding: outstanding,
	}}}
	headline := fmt.Sprintf("gather `%s`: %d of %d answered", g.ID, len(resolved), len(g.Targets))
	if !s.tell(ctx, prompts, g, notice, headline, "gather-"+string(g.ID)+"-first", fields) {
		return
	}
	if err := s.deps.DB.MarkGatherFirstTold(ctx, g.ID, wsm.GatherFirstTold{At: s.deps.Clock.Now(), Trigger: trigger}); err != nil {
		s.deps.Log.Global().Error(op, "the first update was told but could not be recorded told; a retry is refused as a duplicate", dlog.WithCause(fields, err))
		return
	}
	// A RESOLUTION THAT RACED THE DEADLINE is told on its own now.
	s.settle(ctx, prompts, g.ID)
}

// tellLater tells one resolution after the first update, and records it told.
func (s *Service) tellLater(ctx context.Context, prompts prompthandler.Submitter, g wsm.Gather, t wsm.GatherTarget) {
	fields := dlog.Context{"gather": string(g.ID), "caller": string(g.Caller), "target": string(t.Workspace)}
	remaining := 0
	for _, other := range g.Targets {
		if other.Outstanding() {
			remaining++
		}
	}
	notice := &agentreplv1.GatherNotice{Gather: &agentreplv1.GatherRef{Id: string(g.ID)}, Notice: &agentreplv1.GatherNotice_LaterAnswer{LaterAnswer: &agentreplv1.GatherNoticeLaterAnswer{
		Resolved: resolvedProto(t), OutstandingCount: uint32(remaining),
	}}}
	headline := fmt.Sprintf("gather `%s`: `%s` %s; %d still outstanding", g.ID, t.Name, resolvedWords(t), remaining)
	if !s.tell(ctx, prompts, g, notice, headline, "gather-"+string(g.ID)+"-"+string(t.Workspace), fields) {
		return
	}
	if err := s.deps.DB.MarkGatherTargetTold(ctx, g.ID, t.Workspace, s.deps.Clock.Now()); err != nil {
		s.deps.Log.Global().Error(op, "the later answer was told but could not be recorded told; a retry is refused as a duplicate", dlog.WithCause(fields, err))
	}
}

// tell submits NOTICE to G's caller under KEY and answers whether it now
// stands told: submitted, or refused as a duplicate of one already submitted.
// A caller that closed is told nothing and stands told, since a closed
// workspace runs no agent and the status read still answers it.
func (s *Service) tell(ctx context.Context, prompts prompthandler.Submitter, g wsm.Gather, notice *agentreplv1.GatherNotice, headline, key string, fields dlog.Context) bool {
	log := s.deps.Log.Global()
	caller, err := s.deps.DB.Workspace(ctx, g.Caller)
	if err != nil {
		log.Error(op, "could not read the gather's caller; the notice is told at the next event or boot", dlog.WithCause(fields, err))
		return false
	}
	if caller.Closed {
		log.Info(op, "the gather's caller is closed; the notice is not told, and `workspace gather-status` still reads it", fields)
		return true
	}
	words, err := protojson.MarshalOptions{Multiline: true, Indent: "  "}.Marshal(notice)
	if err != nil {
		panic(fmt.Sprintf("gather: a GatherNotice does not encode: %v", err))
	}
	turn, err := briefprompt.Composer{Briefs: s.deps.Briefs, MintKey: func(string) (string, error) { return key, nil }}.
		Submit(ctx, prompts, g.Caller, BriefNotice, map[string]string{"label": headline, "gather": string(g.ID), "notice": string(words)}, key, Origin)
	switch {
	case errors.Is(err, prompthandler.ErrDuplicateSubmission):
		log.Info(op, "the notice was already submitted before a restart; it stands told", fields)
		return true
	case briefprompt.StepOf(err) == briefprompt.StepCompose:
		log.Error(op, "could not compose a gather notice; it is told at the next event or boot", dlog.WithCause(fields, err))
		return false
	case err != nil:
		log.Error(op, "could not submit a gather notice to its caller; it is told at the next event or boot", dlog.WithCause(fields, err))
		return false
	}
	log.Info(op, "told the caller a gather notice", dlog.Merge(fields, dlog.Context{"turn": string(turn)}))
	return true
}

// ---- the boot ----

type bootEvent struct{}

// handle re-arms every unsettled gather: a target that closed or was forgotten
// while no daemon served resolves closed, a deadline that passed tells the
// first update, a latch that closed tells it, and what was resolved after it
// but never told is told.
func (bootEvent) handle(ctx context.Context, s *Service, prompts prompthandler.Submitter) {
	log := s.deps.Log.Global()
	gathers, err := s.deps.DB.UnsettledGathers(ctx)
	if err != nil {
		log.Error(op, "could not read the unsettled gathers; none is re-armed until the next boot", dlog.WithCause(nil, err))
		return
	}
	for _, g := range gathers {
		fields := dlog.Context{"gather": string(g.ID)}
		for _, t := range g.Targets {
			if !t.Outstanding() {
				continue
			}
			w, err := s.deps.DB.Workspace(ctx, t.Workspace)
			switch {
			case errors.Is(err, wsm.ErrNotFound), err == nil && w.Closed:
				if _, err := s.deps.DB.ResolveGatherTarget(ctx, g.ID, t.Workspace, wsm.GatherResolution{Kind: wsm.GatherClosed, At: s.deps.Clock.Now()}); err != nil {
					log.Error(op, "could not record a target that closed while no daemon served; it stays outstanding", dlog.WithCause(fields, err))
				}
			case err != nil:
				log.Error(op, "could not read a target while re-arming; it stays outstanding", dlog.WithCause(fields, err))
			}
		}
		g, err = s.deps.DB.Gather(ctx, g.ID)
		if err != nil {
			log.Error(op, "could not read a gather while re-arming it; it is re-armed at the next boot", dlog.WithCause(fields, err))
			continue
		}
		if g.FirstTold == nil && !g.AllResolved() && !s.deps.Clock.Now().Before(g.DeadlineAt) {
			s.tellFirst(ctx, prompts, g, wsm.GatherTimedOut)
			continue
		}
		s.settle(ctx, prompts, g.ID)
	}
	log.Info(op, "re-armed every unsettled gather", dlog.Context{"gathers": len(gathers)})
}

// ---- the status read ----

// Status answers one gather as it stands now.
func (s *Service) Status(ctx context.Context, id wsm.GatherID) (*agentreplv1.GetGatherStatusResponse, error) {
	g, err := s.deps.DB.Gather(ctx, id)
	switch {
	case errors.Is(err, wsm.ErrNotFound):
		return &agentreplv1.GetGatherStatusResponse{Result: &agentreplv1.GetGatherStatusResponse_Error{Error: &agentreplv1.GetGatherStatusError{
			Cause: &agentreplv1.GetGatherStatusError_UnknownGather{UnknownGather: &agentreplv1.GetGatherStatusUnknownGather{}}}}}, nil
	case err != nil:
		s.deps.Log.Global().Error(op, "could not read a gather for its status", dlog.WithCause(dlog.Context{"gather": string(id)}, err))
		return nil, err
	}
	resolved, outstanding, err := s.standing(ctx, g)
	if err != nil {
		s.deps.Log.Global().Error(op, "could not read where a gather's targets stand for its status", dlog.WithCause(dlog.Context{"gather": string(id)}, err))
		return nil, err
	}
	status := &agentreplv1.GatherStatus{
		Gather:          &agentreplv1.GatherRef{Id: string(g.ID)},
		Caller:          &agentreplv1.GatherTarget{Name: g.CallerName, Workspace: &workspacev1.WorkspaceRef{Id: string(g.Caller), Dir: g.CallerDir}},
		PromptText:      g.PromptText,
		CreatedAtUnixMs: g.CreatedAt.UnixMilli(),
		FirstUpdate:     firstStandingProto(g),
		Resolved:        resolved,
		Outstanding:     outstanding,
	}
	return &agentreplv1.GetGatherStatusResponse{Result: &agentreplv1.GetGatherStatusResponse_Success{Success: &agentreplv1.GetGatherStatusSuccess{Status: status}}}, nil
}

// standing answers G's resolved targets, in the order they resolved, and its
// outstanding ones with why, in the order named.
func (s *Service) standing(ctx context.Context, g wsm.Gather) ([]*agentreplv1.GatherResolvedTarget, []*agentreplv1.GatherOutstandingTarget, error) {
	var done []wsm.GatherTarget
	var outstanding []*agentreplv1.GatherOutstandingTarget
	for _, t := range g.Targets {
		if !t.Outstanding() {
			done = append(done, t)
			continue
		}
		o, err := s.outstanding(ctx, t)
		if err != nil {
			return nil, nil, err
		}
		outstanding = append(outstanding, o)
	}
	sort.SliceStable(done, func(i, j int) bool { return done[i].Resolution.At.Before(done[j].Resolution.At) })
	resolved := make([]*agentreplv1.GatherResolvedTarget, 0, len(done))
	for _, t := range done {
		resolved = append(resolved, resolvedProto(t))
	}
	return resolved, outstanding, nil
}

// outstanding answers why T has not answered, as it stands now: what waits on
// the user or stops the session first, then the prompt still held, then a turn
// that ended without a response, and otherwise its work running.
func (s *Service) outstanding(ctx context.Context, t wsm.GatherTarget) (*agentreplv1.GatherOutstandingTarget, error) {
	out := &agentreplv1.GatherOutstandingTarget{Target: targetProto(t)}
	status, err := s.deps.Status(ctx, t.Workspace)
	if err != nil {
		return nil, fmt.Errorf("read the status of %q: %w", t.Name, err)
	}
	switch a := status.GetStanding().GetOpen().GetActivity().GetActivity().(type) {
	case *agentreplv1.WorkspaceStatusActivity_ColdGate:
		out.Reason = &agentreplv1.GatherOutstandingTarget_ColdGate{ColdGate: &agentreplv1.GatherOutstandingTargetColdGate{}}
		return out, nil
	case *agentreplv1.WorkspaceStatusActivity_NeedsInput:
		out.Reason = &agentreplv1.GatherOutstandingTarget_NeedsInput{NeedsInput: &agentreplv1.GatherOutstandingTargetNeedsInput{}}
		return out, nil
	case *agentreplv1.WorkspaceStatusActivity_Fault:
		out.Reason = &agentreplv1.GatherOutstandingTarget_Faulted{Faulted: &agentreplv1.GatherOutstandingTargetFaulted{Description: a.Fault.GetDescription()}}
		return out, nil
	}
	carrying, err := s.deps.DB.SucceedingTurn(ctx, t.Turn)
	if err != nil {
		return nil, fmt.Errorf("follow the prompt of %q: %w", t.Name, err)
	}
	held, err := s.deps.DB.HeldPrompts(ctx, t.Workspace)
	if err != nil {
		return nil, fmt.Errorf("read the held prompts of %q: %w", t.Name, err)
	}
	for _, h := range held {
		if h.Turn == carrying {
			out.Reason = &agentreplv1.GatherOutstandingTarget_Held{Held: &agentreplv1.GatherOutstandingTargetHeld{}}
			return out, nil
		}
	}
	turn, err := s.deps.DB.Turn(ctx, t.Workspace, carrying)
	switch {
	case err == nil && turn.ClosedAt != nil:
		out.Reason = &agentreplv1.GatherOutstandingTarget_EndedWithoutResponse{EndedWithoutResponse: &agentreplv1.GatherOutstandingTargetEndedWithoutResponse{}}
		return out, nil
	case err != nil && !errors.Is(err, wsm.ErrNotFound):
		return nil, fmt.Errorf("read the turn carrying the prompt of %q: %w", t.Name, err)
	}
	out.Reason = &agentreplv1.GatherOutstandingTarget_Working{Working: &agentreplv1.GatherOutstandingTargetWorking{}}
	return out, nil
}

// ---- the contract's spellings ----

func targetProto(t wsm.GatherTarget) *agentreplv1.GatherTarget {
	return &agentreplv1.GatherTarget{Name: t.Name, Workspace: &workspacev1.WorkspaceRef{Id: string(t.Workspace), Dir: t.Dir}}
}

// resolvedProto spells a resolved target. A target with no resolution is a
// caller's defect, and fails hard.
func resolvedProto(t wsm.GatherTarget) *agentreplv1.GatherResolvedTarget {
	if t.Resolution == nil {
		panic(fmt.Sprintf("gather: target %q spelled resolved while outstanding", t.Name))
	}
	out := &agentreplv1.GatherResolvedTarget{Target: targetProto(t), ResolvedAtUnixMs: t.Resolution.At.UnixMilli()}
	switch t.Resolution.Kind {
	case wsm.GatherAnswered:
		out.Resolution = &agentreplv1.GatherResolvedTarget_Answered{Answered: &agentreplv1.GatherResolvedTargetAnswered{Text: t.Resolution.Text}}
	case wsm.GatherClosed:
		out.Resolution = &agentreplv1.GatherResolvedTarget_Closed{Closed: &agentreplv1.GatherResolvedTargetClosed{}}
	default:
		panic(fmt.Sprintf("gather: undeclared resolution %q", t.Resolution.Kind))
	}
	return out
}

func resolvedWords(t wsm.GatherTarget) string {
	if t.Resolution.Kind == wsm.GatherClosed {
		return "closed unanswered"
	}
	return "answered"
}

func triggerProto(t wsm.GatherTrigger) *agentreplv1.GatherFirstUpdateTrigger {
	switch t {
	case wsm.GatherAllResolved:
		return &agentreplv1.GatherFirstUpdateTrigger{Trigger: &agentreplv1.GatherFirstUpdateTrigger_AllResolved{AllResolved: &agentreplv1.GatherFirstUpdateTriggerAllResolved{}}}
	case wsm.GatherTimedOut:
		return &agentreplv1.GatherFirstUpdateTrigger{Trigger: &agentreplv1.GatherFirstUpdateTrigger_TimedOut{TimedOut: &agentreplv1.GatherFirstUpdateTriggerTimedOut{}}}
	}
	panic(fmt.Sprintf("gather: undeclared trigger %q", t))
}

func firstStandingProto(g wsm.Gather) *agentreplv1.GatherFirstUpdateStanding {
	if g.FirstTold == nil {
		return &agentreplv1.GatherFirstUpdateStanding{Standing: &agentreplv1.GatherFirstUpdateStanding_Pending{Pending: &agentreplv1.GatherFirstUpdateStandingPending{DeadlineAtUnixMs: g.DeadlineAt.UnixMilli()}}}
	}
	return &agentreplv1.GatherFirstUpdateStanding{Standing: &agentreplv1.GatherFirstUpdateStanding_Told{Told: &agentreplv1.GatherFirstUpdateStandingTold{
		ToldAtUnixMs: g.FirstTold.At.UnixMilli(), Trigger: triggerProto(g.FirstTold.Trigger)}}}
}
