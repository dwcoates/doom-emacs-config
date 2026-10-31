package prwatch

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"sort"
	"strconv"
	"strings"
	"time"

	"google.golang.org/protobuf/types/known/timestamppb"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/wsm"
)

// told is what one subscriber was told of a pull request: the snapshot a
// subscription's row holds (wsm.PullRequestSubscription.Told), encoded by
// encodeTold. Maps marshal with their keys sorted, so one state encodes one
// way and the store's compare-and-set compares states, not spellings.
type told struct {
	// State is "draft", "open", "merged" or "closed".
	State string `json:"state"`
	// Comments and ReviewComments map each item's id to when it was last
	// updated; an item whose time moves was edited, and one that is gone was
	// deleted.
	Comments       map[string]string `json:"comments"`
	ReviewComments map[string]string `json:"review_comments"`
	// Reviews maps each submitted review's id to its state and a digest of
	// its body.
	Reviews map[string]string `json:"reviews"`
	// Head is the commit Checks were told for; a new head starts its checks
	// afresh.
	Head string `json:"head"`
	// Checks maps each check run's name to the latest finished run's id and
	// conclusion.
	Checks map[string]string `json:"checks"`
}

// decodeTold reads a row's snapshot. Empty is a subscription never anchored:
// nothing was told yet.
func decodeTold(raw string) (told, bool, error) {
	t := told{Comments: map[string]string{}, ReviewComments: map[string]string{}, Reviews: map[string]string{}, Checks: map[string]string{}}
	if raw == "" {
		return t, false, nil
	}
	if err := json.Unmarshal([]byte(raw), &t); err != nil {
		return t, false, fmt.Errorf("prwatch: the told snapshot is unreadable: %w", err)
	}
	for _, m := range []*map[string]string{&t.Comments, &t.ReviewComments, &t.Reviews, &t.Checks} {
		if *m == nil {
			*m = map[string]string{}
		}
	}
	return t, true, nil
}

// encodeTold writes a snapshot. A snapshot is maps of strings, so it always
// encodes; a failure is a defect.
func encodeTold(t told) string {
	b, err := json.Marshal(t)
	if err != nil {
		panic(fmt.Sprintf("prwatch: encode a told snapshot: %v", err))
	}
	return string(b)
}

// stateOf is the pull request's state as told spells it.
func stateOf(pr remotePR) string {
	switch {
	case pr.Merged:
		return "merged"
	case !pr.Open:
		return "closed"
	case pr.Draft:
		return "draft"
	}
	return "open"
}

// anchor is the snapshot a fresh subscription starts from: everything SNAP
// holds counts as already told.
func anchor(snap snapshot) told {
	t, _, _ := decodeTold("")
	next, _ := diff(snap, t, false)
	return next
}

// diff answers the activity SNAP holds that PREV did not tell, in the order
// GitHub recorded it, and the snapshot after it. ANCHORED is false for a
// subscription that never had a successful read, whose every item is told
// once and whose state is taken as it stands.
func diff(snap snapshot, prev told, anchored bool) (told, []*agentreplv1.PullRequestActivity) {
	next, _, _ := decodeTold("")
	var items []timedActivity

	for _, c := range snap.Comments {
		id := strconv.FormatUint(c.ID, 10)
		stamp := c.Updated.Format(time.RFC3339Nano)
		next.Comments[id] = stamp
		switch was, known := prev.Comments[id]; {
		case !known:
			items = append(items, timedActivity{c.Created, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_Comment{Comment: commentOf(c)}}})
		case was != stamp:
			items = append(items, timedActivity{c.Updated, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_Edited{Edited: &agentreplv1.PullRequestActivityEdited{
				Item: &agentreplv1.PullRequestActivityEdited_Comment{Comment: commentOf(c)}, EditedAt: timestamppb.New(c.Updated)}}}})
		}
	}
	for _, c := range snap.ReviewComments {
		id := strconv.FormatUint(c.ID, 10)
		stamp := c.Updated.Format(time.RFC3339Nano)
		next.ReviewComments[id] = stamp
		switch was, known := prev.ReviewComments[id]; {
		case !known:
			items = append(items, timedActivity{c.Created, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_ReviewComment{ReviewComment: reviewCommentOf(c)}}})
		case was != stamp:
			items = append(items, timedActivity{c.Updated, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_Edited{Edited: &agentreplv1.PullRequestActivityEdited{
				Item: &agentreplv1.PullRequestActivityEdited_ReviewComment{ReviewComment: reviewCommentOf(c)}, EditedAt: timestamppb.New(c.Updated)}}}})
		}
	}
	for _, r := range snap.Reviews {
		if r.State == "PENDING" {
			continue
		}
		id := strconv.FormatUint(r.ID, 10)
		value := r.State + ":" + digest(r.Body)
		next.Reviews[id] = value
		was, known := prev.Reviews[id]
		switch {
		case !known:
			items = append(items, timedActivity{r.Submitted, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_Review{Review: reviewOf(r)}}})
		case was == value:
		case stateOfReview(was) != r.State:
			// The verdict moved (a dismissal): told as the review now stands.
			items = append(items, timedActivity{r.Submitted, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_Review{Review: reviewOf(r)}}})
		default:
			items = append(items, timedActivity{r.Submitted, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_Edited{Edited: &agentreplv1.PullRequestActivityEdited{
				Item: &agentreplv1.PullRequestActivityEdited_Review{Review: reviewOf(r)}, EditedAt: timestamppb.New(r.Submitted)}}}})
		}
	}

	// Deletions are learned by absence, so they have no time; they follow
	// the timed items.
	var deleted []*agentreplv1.PullRequestActivity
	for _, id := range sortedIDs(prev.Comments) {
		if _, still := next.Comments[id]; !still {
			n, _ := strconv.ParseUint(id, 10, 64)
			deleted = append(deleted, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_Deleted{Deleted: &agentreplv1.PullRequestActivityDeleted{
				Item: &agentreplv1.PullRequestActivityDeleted_Comment{Comment: &agentreplv1.PullRequestActivityDeletedComment{CommentId: n}}}}})
		}
	}
	for _, id := range sortedIDs(prev.ReviewComments) {
		if _, still := next.ReviewComments[id]; !still {
			n, _ := strconv.ParseUint(id, 10, 64)
			deleted = append(deleted, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_Deleted{Deleted: &agentreplv1.PullRequestActivityDeleted{
				Item: &agentreplv1.PullRequestActivityDeleted_ReviewComment{ReviewComment: &agentreplv1.PullRequestActivityDeletedReviewComment{CommentId: n}}}}})
		}
	}

	next.State = stateOf(snap.PR)
	var stateChanged []*agentreplv1.PullRequestActivity
	if next.State != prev.State && (anchored || next.State == "merged" || next.State == "closed") {
		change := &agentreplv1.PullRequestActivityStateChanged{State: stateProto(snap.PR)}
		if snap.PR.Merged && snap.PR.MergedBy != "" {
			change.Actor = &agentreplv1.PullRequestActivityAuthor{Login: snap.PR.MergedBy}
		}
		stateChanged = append(stateChanged, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_StateChanged{StateChanged: change}})
	}

	var checks []*agentreplv1.PullRequestActivity
	next.Head, next.Checks = prev.Head, prev.Checks
	if snap.ChecksRead {
		if snap.PR.Head != prev.Head {
			next.Head, next.Checks = snap.PR.Head, map[string]string{}
		} else {
			next.Checks = copyMap(prev.Checks)
		}
		latest := map[string]remoteCheck{}
		for _, c := range snap.Checks {
			if cur, ok := latest[c.Name]; !ok || c.ID > cur.ID {
				latest[c.Name] = c
			}
		}
		for _, name := range sortedNames(latest) {
			c := latest[name]
			if !c.Completed {
				continue
			}
			value := strconv.FormatUint(c.ID, 10) + ":" + c.Conclusion
			if was := next.Checks[name]; was == value {
				continue
			}
			next.Checks[name] = value
			checks = append(checks, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_CheckFinished{CheckFinished: &agentreplv1.PullRequestActivityCheckFinished{
				Name: c.Name, Conclusion: conclusionProto(c.Conclusion)}}})
		}
	}

	sort.SliceStable(items, func(i, j int) bool { return items[i].at.Before(items[j].at) })
	out := make([]*agentreplv1.PullRequestActivity, 0, len(items)+len(deleted)+len(checks)+len(stateChanged))
	for _, item := range items {
		out = append(out, item.activity)
	}
	out = append(append(append(out, deleted...), checks...), stateChanged...)
	return next, out
}

// timedActivity is an item and when GitHub recorded it.
type timedActivity struct {
	at       time.Time
	activity *agentreplv1.PullRequestActivity
}

// eventOf is the activity kind a subscription asks for to be told A.
func eventOf(a *agentreplv1.PullRequestActivity) wsm.PullRequestEvent {
	switch arm := a.GetActivity().(type) {
	case *agentreplv1.PullRequestActivity_Comment:
		return wsm.PullRequestEventComment
	case *agentreplv1.PullRequestActivity_Review:
		return wsm.PullRequestEventReview
	case *agentreplv1.PullRequestActivity_ReviewComment:
		return wsm.PullRequestEventReviewComment
	case *agentreplv1.PullRequestActivity_Edited:
		switch arm.Edited.GetItem().(type) {
		case *agentreplv1.PullRequestActivityEdited_Comment:
			return wsm.PullRequestEventComment
		case *agentreplv1.PullRequestActivityEdited_Review:
			return wsm.PullRequestEventReview
		case *agentreplv1.PullRequestActivityEdited_ReviewComment:
			return wsm.PullRequestEventReviewComment
		}
	case *agentreplv1.PullRequestActivity_Deleted:
		switch arm.Deleted.GetItem().(type) {
		case *agentreplv1.PullRequestActivityDeleted_Comment:
			return wsm.PullRequestEventComment
		case *agentreplv1.PullRequestActivityDeleted_ReviewComment:
			return wsm.PullRequestEventReviewComment
		}
	case *agentreplv1.PullRequestActivity_StateChanged:
		return wsm.PullRequestEventState
	case *agentreplv1.PullRequestActivity_CheckFinished:
		return wsm.PullRequestEventChecks
	}
	panic(fmt.Sprintf("prwatch: activity %T has no event", a.GetActivity()))
}

func commentOf(c remoteComment) *agentreplv1.PullRequestActivityComment {
	return &agentreplv1.PullRequestActivityComment{CommentId: c.ID, Author: authorProto(c.Author), Text: &agentreplv1.PullRequestActivityText{Markdown: c.Body},
		Url: c.URL, PostedAt: timestamppb.New(c.Created)}
}

func reviewCommentOf(c remoteReviewComment) *agentreplv1.PullRequestActivityReviewComment {
	out := &agentreplv1.PullRequestActivityReviewComment{CommentId: c.ID, Author: authorProto(c.Author), Text: &agentreplv1.PullRequestActivityText{Markdown: c.Body},
		Location: &agentreplv1.PullRequestActivityReviewCommentLocation{Path: c.Path, Line: c.Line}, InReplyToCommentId: c.InReplyTo, ReviewId: c.ReviewID,
		Url: c.URL, PostedAt: timestamppb.New(c.Created)}
	return out
}

func reviewOf(r remoteReview) *agentreplv1.PullRequestActivityReview {
	out := &agentreplv1.PullRequestActivityReview{ReviewId: r.ID, Author: authorProto(r.Author), Verdict: verdictProto(r.State), Url: r.URL, SubmittedAt: timestamppb.New(r.Submitted)}
	if r.Body != "" {
		out.Text = &agentreplv1.PullRequestActivityText{Markdown: r.Body}
	}
	return out
}

func authorProto(a author) *agentreplv1.PullRequestActivityAuthor {
	return &agentreplv1.PullRequestActivityAuthor{Login: a.Login, Association: associationProto(a.Association)}
}

func associationProto(s string) agentreplv1.PullRequestAuthorAssociation {
	switch s {
	case "OWNER":
		return agentreplv1.PullRequestAuthorAssociation_PULL_REQUEST_AUTHOR_ASSOCIATION_OWNER
	case "MEMBER":
		return agentreplv1.PullRequestAuthorAssociation_PULL_REQUEST_AUTHOR_ASSOCIATION_MEMBER
	case "COLLABORATOR":
		return agentreplv1.PullRequestAuthorAssociation_PULL_REQUEST_AUTHOR_ASSOCIATION_COLLABORATOR
	case "CONTRIBUTOR":
		return agentreplv1.PullRequestAuthorAssociation_PULL_REQUEST_AUTHOR_ASSOCIATION_CONTRIBUTOR
	case "FIRST_TIME_CONTRIBUTOR", "FIRST_TIMER":
		return agentreplv1.PullRequestAuthorAssociation_PULL_REQUEST_AUTHOR_ASSOCIATION_FIRST_TIME_CONTRIBUTOR
	case "NONE":
		return agentreplv1.PullRequestAuthorAssociation_PULL_REQUEST_AUTHOR_ASSOCIATION_NONE
	}
	return agentreplv1.PullRequestAuthorAssociation_PULL_REQUEST_AUTHOR_ASSOCIATION_UNSPECIFIED
}

// verdictProto is a review state as its verdict; a state this schema does
// not name leaves the oneof unassigned.
func verdictProto(state string) *agentreplv1.PullRequestReviewVerdict {
	v := &agentreplv1.PullRequestReviewVerdict{}
	switch state {
	case "APPROVED":
		v.Verdict = &agentreplv1.PullRequestReviewVerdict_Approved{Approved: &agentreplv1.PullRequestReviewVerdictApproved{}}
	case "CHANGES_REQUESTED":
		v.Verdict = &agentreplv1.PullRequestReviewVerdict_ChangesRequested{ChangesRequested: &agentreplv1.PullRequestReviewVerdictChangesRequested{}}
	case "COMMENTED":
		v.Verdict = &agentreplv1.PullRequestReviewVerdict_Commented{Commented: &agentreplv1.PullRequestReviewVerdictCommented{}}
	case "DISMISSED":
		v.Verdict = &agentreplv1.PullRequestReviewVerdict_Dismissed{Dismissed: &agentreplv1.PullRequestReviewVerdictDismissed{}}
	}
	return v
}

// conclusionProto is a check run's conclusion; one this schema does not name
// (`stale`) leaves the oneof unassigned.
func conclusionProto(conclusion string) *agentreplv1.PullRequestCheckConclusion {
	c := &agentreplv1.PullRequestCheckConclusion{}
	switch conclusion {
	case "success":
		c.Conclusion = &agentreplv1.PullRequestCheckConclusion_Success{Success: &agentreplv1.PullRequestCheckConclusionSuccess{}}
	case "failure":
		c.Conclusion = &agentreplv1.PullRequestCheckConclusion_Failure{Failure: &agentreplv1.PullRequestCheckConclusionFailure{}}
	case "cancelled":
		c.Conclusion = &agentreplv1.PullRequestCheckConclusion_Cancelled{Cancelled: &agentreplv1.PullRequestCheckConclusionCancelled{}}
	case "timed_out":
		c.Conclusion = &agentreplv1.PullRequestCheckConclusion_TimedOut{TimedOut: &agentreplv1.PullRequestCheckConclusionTimedOut{}}
	case "skipped":
		c.Conclusion = &agentreplv1.PullRequestCheckConclusion_Skipped{Skipped: &agentreplv1.PullRequestCheckConclusionSkipped{}}
	case "neutral":
		c.Conclusion = &agentreplv1.PullRequestCheckConclusion_Neutral{Neutral: &agentreplv1.PullRequestCheckConclusionNeutral{}}
	case "action_required":
		c.Conclusion = &agentreplv1.PullRequestCheckConclusion_ActionRequired{ActionRequired: &agentreplv1.PullRequestCheckConclusionActionRequired{}}
	}
	return c
}

// stateProto is the pull request's state.
func stateProto(pr remotePR) *agentreplv1.PullRequestState {
	switch stateOf(pr) {
	case "merged":
		return &agentreplv1.PullRequestState{State: &agentreplv1.PullRequestState_Merged{Merged: &agentreplv1.PullRequestStateMerged{MergeCommitSha: pr.MergeSHA}}}
	case "closed":
		return &agentreplv1.PullRequestState{State: &agentreplv1.PullRequestState_Closed{Closed: &agentreplv1.PullRequestStateClosed{}}}
	case "draft":
		return &agentreplv1.PullRequestState{State: &agentreplv1.PullRequestState_Draft{Draft: &agentreplv1.PullRequestStateDraft{}}}
	}
	return &agentreplv1.PullRequestState{State: &agentreplv1.PullRequestState_Open{Open: &agentreplv1.PullRequestStateOpen{}}}
}

// statusProto is the pull request as it stands.
func statusProto(pr remotePR) *agentreplv1.PullRequestStatus {
	return &agentreplv1.PullRequestStatus{Title: pr.Title, Url: pr.URL, State: stateProto(pr), HeadSha: pr.Head}
}

// endOf is how the pull request ended; nil while it is open.
func endOf(pr remotePR) *wsm.PullRequestEnd {
	switch {
	case pr.Merged:
		return &wsm.PullRequestEnd{Merged: true, MergeSHA: pr.MergeSHA}
	case !pr.Open:
		return &wsm.PullRequestEnd{}
	}
	return nil
}

// stateOfReview is the review state a told review value carries.
func stateOfReview(value string) string {
	state, _, _ := strings.Cut(value, ":")
	return state
}

// digest is a short fingerprint of a body, so a snapshot carries no text.
func digest(body string) string {
	sum := sha256.Sum256([]byte(body))
	return hex.EncodeToString(sum[:8])
}

func sortedIDs(m map[string]string) []string {
	ids := make([]string, 0, len(m))
	for id := range m {
		ids = append(ids, id)
	}
	sort.Slice(ids, func(i, j int) bool {
		a, _ := strconv.ParseUint(ids[i], 10, 64)
		b, _ := strconv.ParseUint(ids[j], 10, 64)
		return a < b
	})
	return ids
}

func sortedNames(m map[string]remoteCheck) []string {
	names := make([]string, 0, len(m))
	for name := range m {
		names = append(names, name)
	}
	sort.Strings(names)
	return names
}

func copyMap(m map[string]string) map[string]string {
	out := make(map[string]string, len(m))
	for k, v := range m {
		out[k] = v
	}
	return out
}
