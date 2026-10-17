// Package workspacesubs tells each WORKSPACE SUBSCRIBER's agent about the
// edges of the workspaces it follows (agentrepl.v1.SubscribeWorkspace).
//
// It observes the lifecycle edges (wsedges.Observer). Each edge is kept in
// order in memory, and one loop delivers it: it reads the followed
// workspace's subscribers at delivery time, and submits ONE notice to each
// whose subscription asks for that event, carrying the followed workspace's
// status as it then stands (agentrepl.v1.WorkspaceStatus), so a notice can
// never describe a workspace differently from a read.
//
// THE NOTICE TAKES THE PATH EVERY UNDER-THE-HOOD REPORT TO AN AGENT TAKES: a
// deferred prompt submitted through the prompt handler, under its own origin
// (PROMPT_ORIGIN_WORKSPACE_NOTICE), which the feed draws no row for. It waits
// behind a running turn and wakes an idle agent.
//
// NOTHING THAT REPORTS EVER WAITS ON A DELIVERY: an edge only appends, and the
// loop delivers everything pending before it ends.
package workspacesubs

import (
	"context"
	"fmt"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/noticeloop"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/wsm"
)

// op is the operation every record of this package carries.
const op = "daemon.workspacesubs"

// BriefNotice is the brief a notice is composed from. Its first line is the
// notice's headline.
const BriefNotice = "workspace-notice"

// Origin is every notice's prompt origin.
const Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_NOTICE

// Subscribers reads a followed workspace's subscriptions.
type Subscribers interface {
	WorkspaceSubscribers(ctx context.Context, followed wsm.WorkspaceID) ([]wsm.WorkspaceSubscription, error)
}

// Deps are the service's collaborators: everything but the prompt handler,
// which is handed to Run.
type Deps struct {
	Subscribers Subscribers
	// Status answers a workspace's status as an agent reads it
	// (workspace.Verbs.Status).
	Status func(ctx context.Context, ws ids.WorkspaceID) (*agentreplv1.WorkspaceStatus, error)
	// Briefs loads and splices a brief at use time.
	Briefs merge.BriefLoader
	// MintKey mints an idempotency key for one submission.
	MintKey func(prefix string) (string, error)
	Log     dlog.Surfaces
}

// edge is one lifecycle edge of a followed workspace.
type edge struct {
	ws    ids.WorkspaceID
	event wsm.WorkspaceEvent
}

// Service is the notifier. It implements wsedges.Observer.
type Service struct {
	deps Deps
	loop *noticeloop.Loop[edge]
}

// New builds the service.
func New(deps Deps) *Service {
	s := &Service{deps: deps}
	s.loop = noticeloop.New(func(e edge) {
		deps.Log.Global().Info(op, "a workspace's edge came after the notifier stopped, as the daemon stood down; its subscribers are not told, and the next notice carries its effect", edgeFields(e))
	})
	return s
}

// TurnStarted implements wsedges.Observer.
func (s *Service) TurnStarted(ws ids.WorkspaceID) {
	s.loop.Report(edge{ws, wsm.WorkspaceEventTurnStarted})
}

// TurnEnded implements wsedges.Observer.
func (s *Service) TurnEnded(ws ids.WorkspaceID) { s.loop.Report(edge{ws, wsm.WorkspaceEventTurnEnded}) }

// TurnEndedUnobserved implements wsedges.Observer: an end no daemon watched
// is never told as news.
func (s *Service) TurnEndedUnobserved(ids.WorkspaceID) {}

// NeedsInput implements wsedges.Observer.
func (s *Service) NeedsInput(ws ids.WorkspaceID) {
	s.loop.Report(edge{ws, wsm.WorkspaceEventNeedsInput})
}

// Landed implements wsedges.Observer.
func (s *Service) Landed(ws ids.WorkspaceID) { s.loop.Report(edge{ws, wsm.WorkspaceEventLanded}) }

// Closed implements wsedges.Observer.
func (s *Service) Closed(before wsm.Workspace) {
	s.loop.Report(edge{before.ID, wsm.WorkspaceEventClosed})
}

// Run delivers every reported edge, in order, through PROMPTS until ctx ends,
// then delivers every edge still pending before it returns.
func (s *Service) Run(ctx context.Context, prompts prompthandler.Submitter) {
	s.loop.Run(ctx, func(ctx context.Context, e edge) { s.deliver(ctx, prompts, e) })
}

// deliver tells E to every subscriber of its workspace that asked for it.
func (s *Service) deliver(ctx context.Context, prompts prompthandler.Submitter, e edge) {
	log := s.deps.Log.Global()
	fields := edgeFields(e)
	subs, err := s.deps.Subscribers.WorkspaceSubscribers(ctx, e.ws)
	if err != nil {
		log.Error(op, "could not read the workspace's subscribers; nobody is told of the edge", dlog.WithCause(fields, err))
		return
	}
	var told []ids.WorkspaceID
	for _, sub := range subs {
		if sub.Events.Has(e.event) {
			told = append(told, sub.Subscriber)
		}
	}
	if len(told) == 0 {
		return
	}
	status, err := s.deps.Status(ctx, e.ws)
	if err != nil {
		log.Error(op, "could not read the followed workspace's status; its subscribers are not told of the edge", dlog.WithCause(fields, err))
		return
	}
	text, err := s.deps.Briefs(BriefNotice, NoticeValues(e.event, status))
	if err != nil {
		log.Error(op, "could not compose a workspace notice; its subscribers are not told of the edge", dlog.WithCause(fields, err))
		return
	}
	for _, subscriber := range told {
		s.notify(ctx, prompts, subscriber, text, fields)
	}
}

// notify submits one composed notice to SUBSCRIBER.
func (s *Service) notify(ctx context.Context, prompts prompthandler.Submitter, subscriber ids.WorkspaceID, text string, edgeFields dlog.Context) {
	log := s.deps.Log.Global()
	fields := dlog.Merge(edgeFields, dlog.Context{"subscriber": string(subscriber)})
	key, err := s.deps.MintKey("workspace-notice")
	if err != nil {
		log.Error(op, "could not mint a workspace notice's idempotency key; the subscriber is not told", dlog.WithCause(fields, err))
		return
	}
	outcome, err := prompts.Submit(ctx, subscriber, feed.SaidText(text), key, Origin, wsm.DeliveryDeferred, nil)
	if err != nil {
		log.Error(op, "could not submit a workspace notice to its subscriber", dlog.WithCause(fields, err))
		return
	}
	log.Info(op, "told a subscriber about the followed workspace's edge", dlog.Merge(fields, dlog.Context{"turn": string(outcome.Turn)}))
}

// NoticeValues are the brief's placeholders for EVENT of the workspace whose
// status is STATUS.
func NoticeValues(event wsm.WorkspaceEvent, status *agentreplv1.WorkspaceStatus) map[string]string {
	name := status.GetName()
	return map[string]string{
		"label":    "workspace `" + name + "`: " + strings.ReplaceAll(event.String(), "_", " "),
		"followed": name,
		"event":    eventSentence(event),
		"status":   StatusWords(status),
	}
}

func eventSentence(event wsm.WorkspaceEvent) string {
	switch event {
	case wsm.WorkspaceEventTurnStarted:
		return "started a turn."
	case wsm.WorkspaceEventTurnEnded:
		return "ended a turn."
	case wsm.WorkspaceEventNeedsInput:
		return "is waiting on the user: a question or a permission is open."
	case wsm.WorkspaceEventLanded:
		return "landed its branch."
	case wsm.WorkspaceEventClosed:
		return "closed."
	}
	panic(fmt.Sprintf("workspacesubs: undeclared event %d", int(event)))
}

// StatusWords states a workspace's status in plain words, the sentence a
// notice carries.
func StatusWords(status *agentreplv1.WorkspaceStatus) string {
	var b strings.Builder
	fmt.Fprintf(&b, "Its status now: `%s`, on branch `%s`, ", status.GetName(), status.GetBranch())
	if owner := status.GetMergeTarget().GetOwner(); owner != nil {
		lands := "lands itself"
		if owner.GetOwnerLands() {
			lands = "is landed by its owner"
		}
		fmt.Fprintf(&b, "lands into the workspace `%s` and %s; ", owner.GetOwner().GetId(), lands)
	} else {
		b.WriteString("lands on the primary branch; ")
	}
	if status.GetStanding().GetClosed() != nil {
		b.WriteString("closed")
	} else {
		b.WriteString("open, " + activityWords(status.GetStanding().GetOpen().GetActivity()))
	}
	if status.GetHidden() {
		b.WriteString(", hidden")
	}
	b.WriteString(".")
	return b.String()
}

func activityWords(a *agentreplv1.WorkspaceStatusActivity) string {
	switch arm := a.GetActivity().(type) {
	case *agentreplv1.WorkspaceStatusActivity_Starting:
		return "starting"
	case *agentreplv1.WorkspaceStatusActivity_Idle:
		return "idle"
	case *agentreplv1.WorkspaceStatusActivity_TurnRunning:
		return "running a turn"
	case *agentreplv1.WorkspaceStatusActivity_NeedsInput:
		if arm.NeedsInput.GetPermission() != nil {
			return "waiting on the user's permission"
		}
		return "waiting on the user's answer to a question"
	case *agentreplv1.WorkspaceStatusActivity_Merging:
		return "merging"
	case *agentreplv1.WorkspaceStatusActivity_ColdGate:
		return "parked at its cold gate"
	case *agentreplv1.WorkspaceStatusActivity_Fault:
		return "faulted (" + arm.Fault.GetDescription() + ")"
	}
	panic(fmt.Sprintf("workspacesubs: undeclared activity %T", a.GetActivity()))
}

func edgeFields(e edge) dlog.Context {
	return dlog.Context{"workspace": string(e.ws), "event": e.event.String()}
}
