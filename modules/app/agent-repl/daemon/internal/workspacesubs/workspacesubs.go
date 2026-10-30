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
// A SUBSCRIBER IS NEVER TOLD A WORKSPACE THAT NO LONGER STANDS, AND ITS
// NOTICES NEVER PILE UP: each subscriber holds at most one untold notice per
// followed workspace, recorded durably (wsm UntoldWorkspaceNotice), and the
// next edge supersedes it and says its changes again first
// (internal/untoldnotice, the supersession the merge queue notifier shares).
// A busy subscriber was otherwise told each edge as its own turn, frozen at
// its submission, long after the workspace moved on (2026-10-22: nine
// standing for one subscriber, one telling a closed workspace "open, running
// a turn").
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
	"agent-repld/internal/untoldnotice"
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

// Untold reads and records each subscriber's untold notice per followed
// workspace (wsm UntoldWorkspaceNotice), the durable half of the supersession.
type Untold interface {
	UntoldWorkspaceNotice(ctx context.Context, subscriber, followed ids.WorkspaceID) (wsm.UntoldWorkspaceNotice, bool, error)
	RecordUntoldWorkspaceNotice(ctx context.Context, n wsm.UntoldWorkspaceNotice) error
}

// Deps are the service's collaborators: everything but the prompt handler,
// which is handed to Run.
type Deps struct {
	Subscribers Subscribers
	// Holds retires a notice a newer one supersedes.
	Holds untoldnotice.Holds
	// Untold keeps each subscriber's untold notice across restarts.
	Untold Untold
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
	var told []wsm.WorkspaceSubscription
	for _, sub := range subs {
		if sub.Events.Has(e.event) {
			told = append(told, sub)
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
	for _, sub := range told {
		s.notify(ctx, prompts, sub, e, status, fields)
	}
}

// notify composes and submits one notice of edge E to SUBSCRIBER, saying first
// every change of the subscriber's untold notice it supersedes, and records it
// as the subscriber's untold notice about the workspace.
func (s *Service) notify(ctx context.Context, prompts prompthandler.Submitter, sub wsm.WorkspaceSubscription, e edge, status *agentreplv1.WorkspaceStatus, edgeFields dlog.Context) {
	log := s.deps.Log.Global()
	subscriber := sub.Subscriber
	fields := dlog.Merge(edgeFields, dlog.Context{"subscriber": string(subscriber), "parent_follows": sub.ParentFollows})
	sentences := append(s.carried(ctx, subscriber, e.ws), EventSentence(status.GetName(), e.event))
	fields["changes"] = len(sentences)
	turn, failure, err := s.submit(ctx, prompts, sub, e.event, sentences, status)
	if err != nil {
		log.Error(op, failure, dlog.WithCause(fields, err))
	} else {
		log.Info(op, "told a subscriber about the followed workspace's edge", dlog.Merge(fields, dlog.Context{"turn": string(turn)}))
	}
	untold := wsm.UntoldWorkspaceNotice{Subscriber: subscriber, Followed: e.ws, Turn: turn, Sentences: sentences}
	if err := s.deps.Untold.RecordUntoldWorkspaceNotice(ctx, untold); err != nil {
		log.Error(op, "could not record the subscriber's untold notice; the next notice cannot supersede it", dlog.WithCause(fields, err))
	}
}

// submit composes the notice of SENTENCES, the latest being EVENT's, about the
// workspace standing STATUS, and submits it to SUBSCRIBER, answering the turn
// it was submitted as, or the record a failure is written with and why.
func (s *Service) submit(ctx context.Context, prompts prompthandler.Submitter, sub wsm.WorkspaceSubscription, event wsm.WorkspaceEvent, sentences []string, status *agentreplv1.WorkspaceStatus) (ids.TurnID, string, error) {
	subscriber := sub.Subscriber
	text, err := s.deps.Briefs(BriefNotice, NoticeValues(event, sentences, status, sub.ParentFollows))
	if err != nil {
		return "", "could not compose a workspace notice; the subscriber is not told of the edge", err
	}
	key, err := s.deps.MintKey("workspace-notice")
	if err != nil {
		return "", "could not mint a workspace notice's idempotency key; the subscriber is not told", err
	}
	outcome, err := prompts.Submit(ctx, subscriber, feed.SaidText(text), key, Origin, nil, wsm.DeliveryDeferred, nil)
	if err != nil {
		return "", "could not submit a workspace notice to its subscriber", err
	}
	return outcome.Turn, "", nil
}

// carried answers the sentences SUBSCRIBER's untold notice about FOLLOWED said
// that the next notice must say again (untoldnotice.Carried). A notice that
// could not be read stands and is told, so nothing it said is carried.
func (s *Service) carried(ctx context.Context, subscriber, followed ids.WorkspaceID) []string {
	log := s.deps.Log.Global()
	fields := dlog.Context{"subscriber": string(subscriber), "workspace": string(followed)}
	prev, found, err := s.deps.Untold.UntoldWorkspaceNotice(ctx, subscriber, followed)
	if err != nil {
		log.Error(op, "could not read the subscriber's untold notice; the next one says only the new change", dlog.WithCause(fields, err))
		return nil
	}
	return untoldnotice.Carried(ctx, s.deps.Holds, log, op, subscriber, untoldnotice.Notice{Turn: prev.Turn, Sentences: prev.Sentences}, found, fields)
}

// NoticeValues are the brief's placeholders for a notice of SENTENCES, oldest
// first, the last being EVENT's, about the workspace standing STATUS.
//
// A PARENT IS TOLD AS THE PARENT (PARENTFOLLOWS): it follows its child without
// subscribing, coordinates the child's work, and cannot unsubscribe from it --
// the notices end when the child closes or stops being its child.
func NoticeValues(event wsm.WorkspaceEvent, sentences []string, status *agentreplv1.WorkspaceStatus, parentFollows bool) map[string]string {
	name := status.GetName()
	why := "A workspace you subscribed to changed."
	stop := "Act on it only as far as the work you subscribed for calls for, and otherwise carry on with whatever you were doing. To stop these notices, unsubscribe through `agent-repld workspace unsubscribe`."
	if parentFollows {
		why = "Your child workspace `" + name + "` changed; as its parent you follow it without subscribing."
		stop = "Coordinate the child as its work calls for (answer it with `agent-repld workspace prompt`, review and land or close it when it is done), and otherwise carry on with whatever you were doing. These notices end when the child closes or stops being this workspace's child."
	}
	return map[string]string{
		"label":  "workspace `" + name + "`: " + strings.ReplaceAll(event.String(), "_", " "),
		"why":    why,
		"event":  untoldnotice.Sentences(sentences),
		"status": StatusWords(status),
		"stop":   stop,
	}
}

// EventSentence states EVENT of the workspace named NAME as a sentence that
// stands alone in a list of changes.
func EventSentence(name string, event wsm.WorkspaceEvent) string {
	subject := "`" + name + "` "
	switch event {
	case wsm.WorkspaceEventTurnStarted:
		return subject + "started a turn."
	case wsm.WorkspaceEventTurnEnded:
		return subject + "ended a turn."
	case wsm.WorkspaceEventNeedsInput:
		return subject + "is waiting on the user: a question or a permission is open."
	case wsm.WorkspaceEventLanded:
		return subject + "landed its branch."
	case wsm.WorkspaceEventClosed:
		return subject + "closed."
	}
	panic(fmt.Sprintf("workspacesubs: undeclared event %d", int(event)))
}

// StatusWords states a workspace's status in plain words, the sentence a
// notice carries.
func StatusWords(status *agentreplv1.WorkspaceStatus) string {
	var b strings.Builder
	b.WriteString(fmt.Sprintf("Its status now: `%s`, on branch `%s`, ", status.GetName(), status.GetBranch()))
	if into := status.GetMergeTarget().GetWorkspace(); into != nil {
		b.WriteString(fmt.Sprintf("lands into the workspace at `%s`", into.GetWorkspace().GetDir()))
	} else {
		b.WriteString("lands on the primary branch")
	}
	if status.GetOwner().GetOwnerLands() {
		b.WriteString(" and is landed by its owner")
	}
	b.WriteString("; ")
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
