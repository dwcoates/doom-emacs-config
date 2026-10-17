// Package workspacenotice tells workspaces about the workspaces DERIVED from
// them (agentrepl.v1.CreateWorkspaceRequest): the workspace whose branch a new
// workspace was cut from, and the workspace whose conversation it forked.
//
// WHO IS TOLD WHAT. A derived workspace has up to two sources -- its owner
// (wsm.Workspace.Parent, the branch it was cut from and lands into) and its
// context source (wsm.Workspace.ContextFrom) -- and each source is told,
// briefly, when the derived workspace is created, when it lands, and when it
// closes without landing. A workspace that is both sources gets ONE notice.
// The requester (wsm.Workspace.RequestedBy) is never told about its own act,
// and a closed source is not told: it runs no session to tell.
//
// AN OWNER THAT LANDS ITS CHILD gets the OWNER NOTICE instead of the brief
// created notice: the requester's own words and the daemon's addendum that
// landing the child is the owner's responsibility (PROMPT_ORIGIN_OWNER_NOTICE,
// drawn as an ordinary prompt). The create reopens such an owner before it
// reports, so the owner always has a session to receive it.
//
// THE NOTICE TAKES THE PATH EVERY UNDER-THE-HOOD REPORT TO AN AGENT TAKES: a
// deferred prompt submitted through the prompt handler, which waits behind a
// running turn and wakes an idle agent. The brief notices ride
// PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE, which the feed draws no row for.
//
// NOTHING THAT REPORTS EVER WAITS ON A DELIVERY. Report only appends; one loop
// (Run) delivers, and it delivers everything pending before it ends, so no
// event reported before the daemon's exit is lost to it.
package workspacenotice

import (
	"context"

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
const op = "daemon.workspacenotice"

// The briefs a notice is composed from.
const (
	// BriefDerived is the brief a source is told a derived workspace's event
	// by. Its first line is the notice's headline.
	BriefDerived = "derived-workspace-notice"
	// BriefOwner is the brief an owner that lands its child is told by.
	BriefOwner = "workspace-owner-notice"
)

// The notices' prompt origins.
const (
	OriginDerived = conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE
	OriginOwner   = conversationv1.PromptOrigin_PROMPT_ORIGIN_OWNER_NOTICE
)

// Kind is what happened to a derived workspace.
type Kind int

const (
	// Created: the derived workspace was created.
	Created Kind = iota
	// Landed: the derived workspace's branch landed in its target.
	Landed
	// ClosedWithoutLanding: the derived workspace closed and never landed.
	ClosedWithoutLanding
)

// String is the kind's name in records.
func (k Kind) String() string {
	switch k {
	case Created:
		return "created"
	case Landed:
		return "landed"
	case ClosedWithoutLanding:
		return "closed_without_landing"
	}
	panic("workspacenotice: undeclared event kind")
}

// Event is one thing that happened to a derived workspace.
type Event struct {
	Kind Kind
	// Workspace is the derived workspace as it stood when the event happened.
	Workspace wsm.Workspace
	// OwnerNotice is the requester's notice to the owner, set only on the
	// Created event of a workspace its owner lands.
	OwnerNotice string
	// unread marks an event whose Workspace carries only its ID (a landing,
	// told by id): the delivery reads the rest from the registry.
	unread bool
}

// Workspaces reads the registry.
type Workspaces interface {
	Workspace(ctx context.Context, id wsm.WorkspaceID) (wsm.Workspace, error)
}

// Deps are the service's collaborators: everything but the prompt handler,
// which is handed to Run.
type Deps struct {
	Workspaces Workspaces
	// Briefs loads and splices a brief at use time.
	Briefs merge.BriefLoader
	// MintKey mints an idempotency key for one submission.
	MintKey func(prefix string) (string, error)
	Log     dlog.Surfaces
}

// Service is the notifier.
type Service struct {
	deps Deps
	loop *noticeloop.Loop[Event]
}

// New builds the service.
func New(deps Deps) *Service {
	s := &Service{deps: deps}
	s.loop = noticeloop.New(func(event Event) {
		deps.Log.Global().Info(op, "a derived workspace's event came after the notifier stopped, as the daemon stood down; its sources are not told", eventFields(event))
	})
	return s
}

// Report appends EVENT for the loop and never blocks.
func (s *Service) Report(event Event) { s.loop.Report(event) }

// TurnStarted implements wsedges.Observer: a turn is no derived event.
func (s *Service) TurnStarted(ids.WorkspaceID) {}

// TurnEnded implements wsedges.Observer: a turn is no derived event.
func (s *Service) TurnEnded(ids.WorkspaceID) {}

// TurnEndedUnobserved implements wsedges.Observer: an end no daemon watched
// is never told as news.
func (s *Service) TurnEndedUnobserved(ids.WorkspaceID) {}

// NeedsInput implements wsedges.Observer: an ask is no derived event.
func (s *Service) NeedsInput(ids.WorkspaceID) {}

// Landed implements wsedges.Observer: WS's branch landed.
func (s *Service) Landed(ws ids.WorkspaceID) {
	s.Report(Event{Kind: Landed, Workspace: wsm.Workspace{ID: ws}, unread: true})
}

// Closed implements wsedges.Observer: a workspace closing tells its sources
// only when its branch never landed, since its landing was told at the edge.
func (s *Service) Closed(before wsm.Workspace) {
	if before.MergedAt == nil {
		s.Report(Event{Kind: ClosedWithoutLanding, Workspace: before})
	}
}

// Run delivers every reported event, in order, until ctx ends, then delivers
// every event still pending before it returns.
func (s *Service) Run(ctx context.Context, prompts prompthandler.Submitter) {
	s.loop.Run(ctx, func(ctx context.Context, event Event) { s.deliver(ctx, prompts, event) })
}

// Recipient is one source an event is told to.
type Recipient struct {
	Source ids.WorkspaceID
	// Owner is set when the source owns the derived workspace's branch.
	Owner bool
	// Context is set when the source's conversation was forked into it.
	Context bool
}

// Recipients answers who is told about WS: its owner and its context source,
// once each, never its requester and never itself.
func Recipients(ws wsm.Workspace) []Recipient {
	var out []Recipient
	add := func(id *wsm.WorkspaceID, owner bool) {
		if id == nil || *id == ws.ID || (ws.RequestedBy != nil && *id == *ws.RequestedBy) {
			return
		}
		for i := range out {
			if out[i].Source == *id {
				if owner {
					out[i].Owner = true
				} else {
					out[i].Context = true
				}
				return
			}
		}
		out = append(out, Recipient{Source: *id, Owner: owner, Context: !owner})
	}
	add(ws.Parent, true)
	add(ws.ContextFrom, false)
	return out
}

// deliver tells EVENT to each of its recipients.
func (s *Service) deliver(ctx context.Context, prompts prompthandler.Submitter, event Event) {
	if event.unread {
		record, err := s.deps.Workspaces.Workspace(ctx, event.Workspace.ID)
		if err != nil {
			s.deps.Log.Global().Error(op, "could not read the workspace a derived event is about; its sources are not told", dlog.WithCause(eventFields(event), err))
			return
		}
		event.Workspace, event.unread = record, false
	}
	for _, told := range Recipients(event.Workspace) {
		s.notify(ctx, prompts, told, event)
	}
}

// notify composes and submits one notice, unless the source is closed.
func (s *Service) notify(ctx context.Context, prompts prompthandler.Submitter, told Recipient, event Event) {
	log := s.deps.Log.Global()
	fields := eventFields(event)
	fields["source"], fields["owner"], fields["context"] = string(told.Source), told.Owner, told.Context
	source, err := s.deps.Workspaces.Workspace(ctx, told.Source)
	if err != nil {
		log.Error(op, "could not read a derived workspace's source; it is not told", dlog.WithCause(fields, err))
		return
	}
	if source.Closed {
		log.Info(op, "a derived workspace's source is closed; it runs no session to tell", fields)
		return
	}
	ownerNotice := event.Kind == Created && told.Owner && event.Workspace.OwnerLands
	brief, origin, values := BriefDerived, OriginDerived, DerivedValues(told, event)
	if ownerNotice {
		brief, origin, values = BriefOwner, OriginOwner, OwnerValues(event, s.requesterName(ctx, event.Workspace))
	}
	text, err := s.deps.Briefs(brief, values)
	if err != nil {
		log.Error(op, "could not compose a derived workspace notice; the source is not told", dlog.WithCause(fields, err))
		return
	}
	key, err := s.deps.MintKey("derived-workspace-notice")
	if err != nil {
		log.Error(op, "could not mint a derived workspace notice's idempotency key; the source is not told", dlog.WithCause(fields, err))
		return
	}
	outcome, err := prompts.Submit(ctx, told.Source, feed.SaidText(text), key, origin, wsm.DeliveryDeferred, nil)
	if err != nil {
		log.Error(op, "could not submit a derived workspace notice to its source", dlog.WithCause(fields, err))
		return
	}
	fields["turn"], fields["origin"] = string(outcome.Turn), origin.String()
	log.Info(op, "told a source workspace about a workspace derived from it", fields)
}

// requesterName is how the owner notice names who asked: the requesting
// workspace's name, or the user.
func (s *Service) requesterName(ctx context.Context, ws wsm.Workspace) string {
	if ws.RequestedBy == nil {
		return "the user"
	}
	requester, err := s.deps.Workspaces.Workspace(ctx, *ws.RequestedBy)
	if err != nil {
		s.deps.Log.Global().Error(op, "could not read an owner notice's requester; the notice names it by id", dlog.WithCause(eventFields(Event{Workspace: ws}), err))
		return "the workspace `" + string(*ws.RequestedBy) + "`"
	}
	return "the workspace `" + requester.Name + "`"
}

// DerivedValues are the derived brief's placeholders.
func DerivedValues(told Recipient, event Event) map[string]string {
	name := event.Workspace.Name
	return map[string]string{
		"label":    "workspace `" + name + "`: " + kindWords(event.Kind),
		"derived":  name,
		"relation": relationWords(told),
		"event":    eventSentence(event),
	}
}

// OwnerValues are the owner brief's placeholders.
func OwnerValues(event Event, requester string) map[string]string {
	return map[string]string{
		"child":     event.Workspace.Name,
		"requester": requester,
		"notice":    event.OwnerNotice,
	}
}

func kindWords(k Kind) string {
	switch k {
	case Created:
		return "created"
	case Landed:
		return "landed"
	case ClosedWithoutLanding:
		return "closed without landing"
	}
	panic("workspacenotice: undeclared event kind")
}

func relationWords(told Recipient) string {
	switch {
	case told.Owner && told.Context:
		return "was cut from this workspace's branch, with this conversation forked, and lands into this workspace"
	case told.Owner:
		return "was cut from this workspace's branch and lands into this workspace"
	default:
		return "was created with this conversation forked"
	}
}

func eventSentence(event Event) string {
	switch event.Kind {
	case Created:
		return "It was just created, at branch `" + event.Workspace.Branch + "`."
	case Landed:
		return "Its branch `" + event.Workspace.Branch + "` just landed."
	case ClosedWithoutLanding:
		return "It just closed without its branch `" + event.Workspace.Branch + "` landing."
	}
	panic("workspacenotice: undeclared event kind")
}

func eventFields(event Event) dlog.Context {
	return dlog.Context{"workspace": string(event.Workspace.ID), "event": event.Kind.String(),
		"owner": optionalID(event.Workspace.Parent), "context_from": optionalID(event.Workspace.ContextFrom),
		"requested_by": optionalID(event.Workspace.RequestedBy), "owner_lands": event.Workspace.OwnerLands}
}

func optionalID(id *wsm.WorkspaceID) string {
	if id == nil {
		return ""
	}
	return string(*id)
}
