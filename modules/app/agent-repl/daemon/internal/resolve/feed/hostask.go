package feed

import (
	"context"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// THE HOST VIEW'S ASK (agentrepl.v1.HostWorkspace.ask): the root feed's oldest
// standing ask, as Emacs answers it by number. It is the same ask a typed
// answer goes to (oldestStandingAsk), projected onto the choices a number
// names.
//
// THE SNAPSHOT LIVES OFF THE FEED'S LOCK. The server composes the host view
// from it, and the composition must never wait on the feed's mutex: the feed
// calls out while holding it, and a reader taking it from the other side is a
// lock-order inversion waiting to happen. So every card draw recomputes the
// view under the feed's lock (refreshHostAsk), stores it under askMu, and
// emits the workspace on askChanges when it moved; the server follows that
// stream and republishes from HostAsk.

const opHostAsk = "daemon.feed.host_ask"

// HostAsk answers the workspace's standing ask as the host view carries it,
// nil when none stands. The value is IMMUTABLE: refreshHostAsk replaces a
// workspace's entry whole and never edits one, so the answer is shared rather
// than copied, and no caller may edit it.
func (r *resolver) HostAsk(ws ids.WorkspaceID) *agentreplv1.HostAsk {
	r.askMu.Lock()
	defer r.askMu.Unlock()
	return r.hostAsks[ws]
}

// WatchHostAsks yields every workspace whose standing ask moved, from this
// call on, until ctx is cancelled.
func (r *resolver) WatchHostAsks(ctx context.Context) <-chan ids.WorkspaceID {
	return r.askChanges.Subscribe(ctx)
}

// refreshHostAsk recomputes s's host-view ask and, when it moved, stores it
// and announces the workspace. Called under the feed's lock after every edge
// that can change which ask stands or what it offers: a card's draw, a draft
// edit, a delivery's release, a reset.
func (r *resolver) refreshHostAsk(s *wsState) {
	view := r.hostAskView(s)
	r.askMu.Lock()
	previous, had := r.hostAsks[s.id]
	moved := had != (view != nil) || (view != nil && !proto.Equal(previous, view))
	if moved {
		if view == nil {
			delete(r.hostAsks, s.id)
		} else {
			r.hostAsks[s.id] = view
		}
	}
	r.askMu.Unlock()
	if !moved {
		return
	}
	r.logger(s.id).Info(opHostAsk, "the host view's standing ask moved", dlog.Context{
		"standing": view != nil,
		"row":      view.GetRow().GetValue(),
		"kind":     hostAskKind(view),
	})
	r.askChanges.Emit(s.id)
}

// hostAskView projects the root feed's oldest standing ask, nil when none
// stands.
func (r *resolver) hostAskView(s *wsState) *agentreplv1.HostAsk {
	ask, found := r.oldestStandingAsk(s, r.feedKey(s.id, feedid.Feed{Root: true}))
	if !found {
		return nil
	}
	switch ask.Kind {
	case AskPermission:
		p := s.permissionRows[ask.ID]
		permission := &agentreplv1.HostAskPermission{}
		if p.card.GetStandingOffered() != nil {
			permission.StandingOffered = &agentreplv1.HostAskPermissionStandingOffered{}
		}
		return &agentreplv1.HostAsk{
			Row:  p.row,
			Kind: &agentreplv1.HostAsk_Permission{Permission: permission},
		}
	case AskQuestion:
		q := s.questionAsks[ask.ID]
		question, ok := r.hostAskQuestion(s, q)
		if !ok {
			return nil
		}
		return &agentreplv1.HostAsk{
			Row:  q.row,
			Kind: &agentreplv1.HostAsk_Question{Question: question},
		}
	}
	panic("feed: oldestStandingAsk answered an unnamed ask kind")
}

// hostAskQuestion projects a standing batch's SHOWN question. A question the
// vendor served with neither selection arm offers nothing to number: it is
// recorded at ERROR and the ask is left out of the host view, so a typed
// number stays text rather than naming a choice the card does not draw.
func (r *resolver) hostAskQuestion(s *wsState, q *questionState) (*agentreplv1.HostAskQuestion, bool) {
	asked := q.batch.GetQuestions()[q.active]
	options := hostAskOptions(servedLabels(asked))
	question := &agentreplv1.HostAskQuestion{QuestionText: asked.GetQuestion().GetText()}
	switch {
	case asked.GetSingleSelect() != nil:
		question.Selection = &agentreplv1.HostAskQuestion_SingleSelect{
			SingleSelect: &agentreplv1.HostAskQuestionSingleSelect{Options: options},
		}
	case asked.GetMultiSelect() != nil:
		question.Selection = &agentreplv1.HostAskQuestion_MultiSelect{
			MultiSelect: &agentreplv1.HostAskQuestionMultiSelect{Options: options},
		}
	default:
		r.logger(s.id).Error(opHostAsk, "the shown question carries no selection arm; the host view names no ask",
			dlog.Context{"row": q.row.GetValue(), "question": q.active})
		return nil, false
	}
	return question, true
}

// hostAskOptions wraps served labels as the host view's numbered options, in
// the served order.
func hostAskOptions(labels []string) []*agentreplv1.HostAskQuestionOption {
	options := make([]*agentreplv1.HostAskQuestionOption, 0, len(labels))
	for _, label := range labels {
		options = append(options, &agentreplv1.HostAskQuestionOption{Label: label})
	}
	return options
}

// hostAskKind names a host-view ask's arm for the log, "" when none stands.
func hostAskKind(ask *agentreplv1.HostAsk) string {
	switch ask.GetKind().(type) {
	case *agentreplv1.HostAsk_Permission:
		return "permission"
	case *agentreplv1.HostAsk_Question:
		return "question"
	}
	return ""
}
