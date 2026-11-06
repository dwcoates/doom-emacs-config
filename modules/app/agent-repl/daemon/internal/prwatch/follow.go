package prwatch

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strconv"
	"time"

	"google.golang.org/protobuf/types/known/timestamppb"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ghclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// Anchor reads PR as it stands now and answers the subscription a workspace
// takes to it: told everything already there, or ended when the pull request
// already merged or closed. A read that fails answers an UNANCHORED
// subscription (Told empty), whose first successful read tells everything on
// the pull request once, and the failure's error, for the caller to report.
func (w *Watcher) Anchor(ctx context.Context, subscriber ids.WorkspaceID, pr wsm.PullRequestKey, events wsm.PullRequestEvents, origin wsm.PullRequestOrigin) (wsm.PullRequestSubscription, *agentreplv1.PullRequestStatus, error) {
	sub := wsm.PullRequestSubscription{Subscriber: subscriber, PullRequest: pr, Events: events, Origin: origin, SubscribedAt: w.deps.Clock.Now()}
	var queue *queueAsk
	if events.Has(wsm.PullRequestEventMergeQueue) {
		queue = w.readQueueFacts(ctx, []wsm.PullRequestKey{pr}).ask(pr)
	}
	snap, err := w.readSnapshot(ctx, pr, events.Has(wsm.PullRequestEventChecks), queue, newQueueReads(w.deps.GH))
	if err != nil {
		return sub, nil, err
	}
	sub.Told = encodeTold(anchor(snap))
	sub.Ended = endOf(snap.PR)
	return sub, statusProto(snap), nil
}

// FailureProto is a call's error as the contract's GitHub failure.
func FailureProto(err error) *agentreplv1.PullRequestGitHubFailure {
	return failureProtoOf(failureOf(err, time.Time{}), err)
}

func failureProtoOf(f wsm.PullRequestFailure, err error) *agentreplv1.PullRequestGitHubFailure {
	out := &agentreplv1.PullRequestGitHubFailure{}
	var gh *ghclient.Failure
	if err != nil && errors.As(err, &gh) && gh.Kind == ghclient.KindRefused {
		out.Cause = &agentreplv1.PullRequestGitHubFailure_Refused{Refused: &agentreplv1.PullRequestGitHubFailureRefused{HttpStatus: uint32(gh.Status), Message: gh.Message}}
		return out
	}
	switch f.Kind {
	case wsm.PullRequestFailureUnauthorized:
		out.Cause = &agentreplv1.PullRequestGitHubFailure_Unauthorized{Unauthorized: &agentreplv1.PullRequestGitHubFailureUnauthorized{}}
	case wsm.PullRequestFailureNotFound:
		out.Cause = &agentreplv1.PullRequestGitHubFailure_NotFound{NotFound: &agentreplv1.PullRequestGitHubFailureNotFound{}}
	case wsm.PullRequestFailureRateLimited:
		out.Cause = &agentreplv1.PullRequestGitHubFailure_RateLimited{RateLimited: &agentreplv1.PullRequestGitHubFailureRateLimited{ResetsAt: timestamppb.New(f.ResetsAt)}}
	default:
		out.Cause = &agentreplv1.PullRequestGitHubFailure_Unreachable{Unreachable: &agentreplv1.PullRequestGitHubFailureUnreachable{Detail: f.Detail}}
	}
	return out
}

// SubscriptionProto renders a stored subscription as the contract spells it.
// Its subscriber names the workspace by id; the caller that holds the
// registry fills the directory.
func SubscriptionProto(sub wsm.PullRequestSubscription) *agentreplv1.PullRequestSubscription {
	out := &agentreplv1.PullRequestSubscription{
		Subscriber:  &workspacev1.WorkspaceRef{Id: string(sub.Subscriber)},
		PullRequest: RefProto(sub.PullRequest),
		Origin:      &agentreplv1.PullRequestSubscriptionOrigin{},
	}
	for _, e := range sub.Events.Members() {
		out.Events = append(out.Events, EventProto(e))
	}
	if sub.Origin == wsm.PullRequestOriginOwnedLink {
		out.Origin.Origin = &agentreplv1.PullRequestSubscriptionOrigin_OwnedLink{OwnedLink: &agentreplv1.PullRequestSubscriptionOriginOwnedLink{}}
	} else {
		out.Origin.Origin = &agentreplv1.PullRequestSubscriptionOrigin_Requested{Requested: &agentreplv1.PullRequestSubscriptionOriginRequested{}}
	}
	out.Health = healthProto(sub.LastRead, sub.Failing)
	return out
}

// healthProto is a GitHub subscription's read health as the contract spells
// it: failing while FAILING is set, healthy since LASTREAD otherwise.
func healthProto(lastRead time.Time, failing *wsm.PullRequestFailure) *agentreplv1.PullRequestSubscriptionHealth {
	if failing != nil {
		return &agentreplv1.PullRequestSubscriptionHealth{Health: &agentreplv1.PullRequestSubscriptionHealth_Failing{Failing: &agentreplv1.PullRequestSubscriptionFailing{
			Since: timestamppb.New(failing.Since), Cause: failureProtoOf(*failing, nil)}}}
	}
	healthy := &agentreplv1.PullRequestSubscriptionHealthy{}
	if !lastRead.IsZero() {
		healthy.LastRead = timestamppb.New(lastRead)
	}
	return &agentreplv1.PullRequestSubscriptionHealth{Health: &agentreplv1.PullRequestSubscriptionHealth_Healthy{Healthy: healthy}}
}

// RefProto is a key as the contract spells it.
func RefProto(pr wsm.PullRequestKey) *agentreplv1.PullRequestRef {
	return &agentreplv1.PullRequestRef{Owner: pr.Owner, Repo: pr.Repo, Number: pr.Number}
}

// KeyOf is a contract ref as the store keys it.
func KeyOf(ref *agentreplv1.PullRequestRef) wsm.PullRequestKey {
	return wsm.PullRequestKey{Owner: ref.GetOwner(), Repo: ref.GetRepo(), Number: ref.GetNumber()}
}

// EventProto is a stored event as the contract spells it.
func EventProto(e wsm.PullRequestEvent) agentreplv1.PullRequestNoticeEvent {
	switch e {
	case wsm.PullRequestEventComment:
		return agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_COMMENT
	case wsm.PullRequestEventReview:
		return agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_REVIEW
	case wsm.PullRequestEventReviewComment:
		return agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_REVIEW_COMMENT
	case wsm.PullRequestEventState:
		return agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_STATE
	case wsm.PullRequestEventChecks:
		return agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_CHECKS
	case wsm.PullRequestEventMergeQueue:
		return agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_MERGE_QUEUE
	}
	panic(fmt.Sprintf("prwatch: event %d has no contract spelling", int(e)))
}

// EventsOf reads a request's events. The request's validation refused an
// unspecified one, so an event this switch does not know is a contract arm
// added without its mapping, and fails hard.
func EventsOf(events []agentreplv1.PullRequestNoticeEvent) wsm.PullRequestEvents {
	var out []wsm.PullRequestEvent
	for _, e := range events {
		switch e {
		case agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_COMMENT:
			out = append(out, wsm.PullRequestEventComment)
		case agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_REVIEW:
			out = append(out, wsm.PullRequestEventReview)
		case agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_REVIEW_COMMENT:
			out = append(out, wsm.PullRequestEventReviewComment)
		case agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_STATE:
			out = append(out, wsm.PullRequestEventState)
		case agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_CHECKS:
			out = append(out, wsm.PullRequestEventChecks)
		case agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_MERGE_QUEUE:
			out = append(out, wsm.PullRequestEventMergeQueue)
		default:
			panic(fmt.Sprintf("prwatch: notice event %s has no store event", e))
		}
	}
	return wsm.NewPullRequestEvents(out...)
}

// ErrUnknownThread is a reply naming no line comment of the pull request.
var ErrUnknownThread = errors.New("prwatch: the reply names no line comment of the pull request")

// Posted is a comment the daemon posted for a workspace.
type Posted struct {
	ID  uint64
	URL string
}

// Comment posts MARKDOWN on PR for WS, as a conversation comment, or as a
// reply in the thread of line comment IN_REPLY_TO when it is set, and
// records the created comment as told in every subscription WS holds to PR,
// so the comment is never told back to it. A reply naming no line comment is
// ErrUnknownThread; any other failure is the leaf's *ghclient.Failure.
func (w *Watcher) Comment(ctx context.Context, ws ids.WorkspaceID, pr wsm.PullRequestKey, inReplyTo *uint64, markdown string) (Posted, error) {
	fields := dlog.Context{"workspace": string(ws), "pull_request": pr.String(), "reply": inReplyTo != nil}
	path := fmt.Sprintf("repos/%s/%s/issues/%d/comments", pr.Owner, pr.Repo, pr.Number)
	line := false
	if inReplyTo != nil {
		path = fmt.Sprintf("repos/%s/%s/pulls/%d/comments/%d/replies", pr.Owner, pr.Repo, pr.Number, *inReplyTo)
		line = true
	}
	resp, err := w.deps.GH.Post(ctx, path, map[string]string{"body": markdown})
	var gh *ghclient.Failure
	if inReplyTo != nil && errors.As(err, &gh) && gh.Kind == ghclient.KindNotFound {
		w.deps.Log.Info(opComment, "the reply names no line comment of the pull request; nothing was posted", fields)
		return Posted{}, ErrUnknownThread
	}
	if err != nil {
		return Posted{}, err
	}
	var created struct {
		ID        uint64    `json:"id"`
		HTMLURL   string    `json:"html_url"`
		UpdatedAt time.Time `json:"updated_at"`
	}
	if err := json.Unmarshal(resp.Body, &created); err != nil || created.ID == 0 {
		err = errors.Join(err, fmt.Errorf("prwatch: GitHub's answer to the post names no comment: %s", resp.Body))
		w.deps.Log.Error(opComment, "posted a comment whose answer cannot be read; it may be told back to the workspace", dlog.WithCause(fields, err))
		return Posted{}, err
	}
	fields["comment_id"] = created.ID
	posted := Posted{ID: created.ID, URL: created.HTMLURL}
	if err := w.recordOwn(ctx, ws, pr, created.ID, created.UpdatedAt, line); err != nil {
		w.deps.Log.Error(opComment, "posted the comment but could not record it as the workspace's own; it will be told back to it", dlog.WithCause(fields, err))
		return posted, nil
	}
	w.deps.Log.Info(opComment, "posted a comment for the workspace and recorded it as its own", fields)
	return posted, nil
}

// recordOwn adds comment ID, last updated at UPDATED, to the told snapshot
// of every live subscription WS holds to PR, comparing and setting until the
// write lands on the snapshot it read.
func (w *Watcher) recordOwn(ctx context.Context, ws ids.WorkspaceID, pr wsm.PullRequestKey, id uint64, updated time.Time, line bool) error {
	for {
		subs, err := w.deps.Store.WorkspacePullRequestSubscriptions(ctx, ws)
		if err != nil {
			return err
		}
		var sub *wsm.PullRequestSubscription
		for i := range subs {
			if subs[i].PullRequest == pr && subs[i].Ended == nil {
				sub = &subs[i]
			}
		}
		// A subscription never anchored is told everything on the pull
		// request at its first read, this comment with it: recording one
		// item here would make every other item read as already told.
		if sub == nil || sub.Told == "" {
			return nil
		}
		t, _, err := decodeTold(sub.Told)
		if err != nil {
			return err
		}
		key, stamp := strconv.FormatUint(id, 10), updated.Format(time.RFC3339Nano)
		if line {
			t.ReviewComments[key] = stamp
		} else {
			t.Comments[key] = stamp
		}
		applied, err := w.deps.Store.AdvancePullRequestTold(ctx, wsm.PullRequestAdvance{Subscriber: ws, PullRequest: pr, From: sub.Told, To: encodeTold(t)})
		if err != nil || applied {
			return err
		}
	}
}
