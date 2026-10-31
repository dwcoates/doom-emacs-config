package prwatch

import (
	"fmt"
	"strings"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/wsm"
)

// The words a notice is written in. Each piece of activity is one sentence
// that stands alone in a list, since a busy subscriber's untold notice is
// superseded by one saying every sentence since it was last told
// (untoldnotice.Carried).

// sentencesOf renders ACTIVITY as the sentences a notice says, in order. The
// skipped checks among them are said in one sentence, after the others: a
// pull request runs many, and a skip is rarely news.
func sentencesOf(activity []*agentreplv1.PullRequestActivity) []string {
	var out, skipped []string
	for _, a := range activity {
		if c := a.GetCheckFinished(); c != nil && c.GetConclusion().GetSkipped() != nil {
			skipped = append(skipped, "`"+c.GetName()+"`")
			continue
		}
		out = append(out, activitySentence(a))
	}
	switch len(skipped) {
	case 0:
	case 1:
		out = append(out, "Check "+skipped[0]+" was skipped.")
	default:
		out = append(out, fmt.Sprintf("%d checks were skipped: %s.", len(skipped), strings.Join(skipped, ", ")))
	}
	return out
}

// activitySentence renders one piece of activity.
func activitySentence(a *agentreplv1.PullRequestActivity) string {
	switch arm := a.GetActivity().(type) {
	case *agentreplv1.PullRequestActivity_Comment:
		c := arm.Comment
		return fmt.Sprintf("%s commented (%s):\n%s", authorWords(c.GetAuthor()), c.GetUrl(), untrusted(c.GetText()))
	case *agentreplv1.PullRequestActivity_Review:
		r := arm.Review
		s := fmt.Sprintf("%s %s (%s).", authorWords(r.GetAuthor()), verdictWords(r.GetVerdict()), r.GetUrl())
		if r.GetText() != nil {
			s = strings.TrimSuffix(s, ".") + ":\n" + untrusted(r.GetText())
		}
		return s
	case *agentreplv1.PullRequestActivity_ReviewComment:
		c := arm.ReviewComment
		return fmt.Sprintf("%s commented on %s%s (%s):\n%s", authorWords(c.GetAuthor()), locationWords(c.GetLocation()), replyWords(c.InReplyToCommentId), c.GetUrl(), untrusted(c.GetText()))
	case *agentreplv1.PullRequestActivity_Edited:
		switch item := arm.Edited.GetItem().(type) {
		case *agentreplv1.PullRequestActivityEdited_Comment:
			return fmt.Sprintf("%s edited a comment (%s); it now reads:\n%s", authorWords(item.Comment.GetAuthor()), item.Comment.GetUrl(), untrusted(item.Comment.GetText()))
		case *agentreplv1.PullRequestActivityEdited_Review:
			return fmt.Sprintf("%s edited their review (%s); it now reads:\n%s", authorWords(item.Review.GetAuthor()), item.Review.GetUrl(), untrusted(item.Review.GetText()))
		case *agentreplv1.PullRequestActivityEdited_ReviewComment:
			return fmt.Sprintf("%s edited a comment on %s (%s); it now reads:\n%s", authorWords(item.ReviewComment.GetAuthor()), locationWords(item.ReviewComment.GetLocation()), item.ReviewComment.GetUrl(), untrusted(item.ReviewComment.GetText()))
		}
	case *agentreplv1.PullRequestActivity_Deleted:
		switch item := arm.Deleted.GetItem().(type) {
		case *agentreplv1.PullRequestActivityDeleted_Comment:
			return fmt.Sprintf("Comment %d was deleted.", item.Comment.GetCommentId())
		case *agentreplv1.PullRequestActivityDeleted_ReviewComment:
			return fmt.Sprintf("Line comment %d was deleted.", item.ReviewComment.GetCommentId())
		}
	case *agentreplv1.PullRequestActivity_StateChanged:
		return stateChangeWords(arm.StateChanged)
	case *agentreplv1.PullRequestActivity_CheckFinished:
		return fmt.Sprintf("Check `%s`: %s.", arm.CheckFinished.GetName(), conclusionWords(arm.CheckFinished.GetConclusion()))
	}
	panic(fmt.Sprintf("prwatch: activity %T has no words", a.GetActivity()))
}

// untrusted renders TEXT as a quoted block the brief tells the agent to weigh
// and never to obey. The fence is longer than any run of tildes in the text,
// so the text cannot close it.
func untrusted(text *agentreplv1.PullRequestActivityText) string {
	body := text.GetMarkdown()
	fence := "~~~"
	for strings.Contains(body, fence) {
		fence += "~"
	}
	return fence + "untrusted\n" + body + "\n" + fence
}

func authorWords(a *agentreplv1.PullRequestActivityAuthor) string {
	login := a.GetLogin()
	if login == "" {
		login = "an unknown account"
	} else {
		login = "@" + login
	}
	switch a.GetAssociation() {
	case agentreplv1.PullRequestAuthorAssociation_PULL_REQUEST_AUTHOR_ASSOCIATION_OWNER:
		return login + " (the repository's owner)"
	case agentreplv1.PullRequestAuthorAssociation_PULL_REQUEST_AUTHOR_ASSOCIATION_MEMBER:
		return login + " (a member of the organization)"
	case agentreplv1.PullRequestAuthorAssociation_PULL_REQUEST_AUTHOR_ASSOCIATION_COLLABORATOR:
		return login + " (a collaborator)"
	case agentreplv1.PullRequestAuthorAssociation_PULL_REQUEST_AUTHOR_ASSOCIATION_CONTRIBUTOR:
		return login + " (a past contributor)"
	case agentreplv1.PullRequestAuthorAssociation_PULL_REQUEST_AUTHOR_ASSOCIATION_FIRST_TIME_CONTRIBUTOR:
		return login + " (a first-time contributor)"
	case agentreplv1.PullRequestAuthorAssociation_PULL_REQUEST_AUTHOR_ASSOCIATION_NONE:
		return login + " (no standing in the repository)"
	}
	return login
}

func verdictWords(v *agentreplv1.PullRequestReviewVerdict) string {
	switch v.GetVerdict().(type) {
	case *agentreplv1.PullRequestReviewVerdict_Approved:
		return "approved"
	case *agentreplv1.PullRequestReviewVerdict_ChangesRequested:
		return "requested changes"
	case *agentreplv1.PullRequestReviewVerdict_Commented:
		return "reviewed without a verdict"
	case *agentreplv1.PullRequestReviewVerdict_Dismissed:
		return "had their review dismissed"
	}
	return "submitted a review"
}

func locationWords(l *agentreplv1.PullRequestActivityReviewCommentLocation) string {
	if l.Line != nil {
		return fmt.Sprintf("`%s` line %d", l.GetPath(), l.GetLine())
	}
	return "`" + l.GetPath() + "`"
}

func replyWords(inReplyTo *uint64) string {
	if inReplyTo == nil {
		return ""
	}
	return fmt.Sprintf(", replying in the thread of comment %d", *inReplyTo)
}

func stateChangeWords(c *agentreplv1.PullRequestActivityStateChanged) string {
	by := ""
	if c.GetActor() != nil {
		by = " by @" + c.GetActor().GetLogin()
	}
	switch s := c.GetState().GetState().(type) {
	case *agentreplv1.PullRequestState_Merged:
		return fmt.Sprintf("The pull request was merged%s, as commit %s.", by, short(s.Merged.GetMergeCommitSha()))
	case *agentreplv1.PullRequestState_Closed:
		return "The pull request was closed without merging" + by + "."
	case *agentreplv1.PullRequestState_Draft:
		return "The pull request was marked a draft" + by + "."
	case *agentreplv1.PullRequestState_Open:
		return "The pull request is ready for review" + by + "."
	}
	panic(fmt.Sprintf("prwatch: state %T has no words", c.GetState().GetState()))
}

func conclusionWords(c *agentreplv1.PullRequestCheckConclusion) string {
	switch c.GetConclusion().(type) {
	case *agentreplv1.PullRequestCheckConclusion_Success:
		return "passed"
	case *agentreplv1.PullRequestCheckConclusion_Failure:
		return "failed"
	case *agentreplv1.PullRequestCheckConclusion_Cancelled:
		return "cancelled"
	case *agentreplv1.PullRequestCheckConclusion_TimedOut:
		return "timed out"
	case *agentreplv1.PullRequestCheckConclusion_Skipped:
		return "skipped"
	case *agentreplv1.PullRequestCheckConclusion_Neutral:
		return "finished neutral"
	case *agentreplv1.PullRequestCheckConclusion_ActionRequired:
		return "waits on a person's action"
	}
	return "finished"
}

// statusWords states the pull request as it stands, the line every notice
// ends with.
func statusWords(s *agentreplv1.PullRequestStatus) string {
	var state string
	switch st := s.GetState().GetState().(type) {
	case *agentreplv1.PullRequestState_Merged:
		state = "merged as " + short(st.Merged.GetMergeCommitSha())
	case *agentreplv1.PullRequestState_Closed:
		state = "closed without merging"
	case *agentreplv1.PullRequestState_Draft:
		state = "an open draft"
	default:
		state = "open"
	}
	return fmt.Sprintf("It now stands %s, its head at %s: %s", state, short(s.GetHeadSha()), s.GetUrl())
}

// failureSentence says that reads of a pull request began failing, and why.
func failureSentence(f wsm.PullRequestFailure) string {
	switch f.Kind {
	case wsm.PullRequestFailureUnauthorized:
		return "The daemon can no longer read this pull request: GitHub refused the `gh` login's credentials. Ask the user to run `gh auth login`; reads resume on their own after."
	case wsm.PullRequestFailureNotFound:
		return "The daemon can no longer read this pull request: GitHub answers that it does not exist, or is not visible to the `gh` login."
	case wsm.PullRequestFailureRateLimited:
		return "The daemon's reads of this pull request are paused: GitHub's rate limit is spent until " + f.ResetsAt.Format(time.RFC3339) + "."
	}
	return "The daemon's reads of this pull request are failing: " + f.Detail + ". It keeps reading on its schedule."
}

// recoveredSentence says reads work again.
const recoveredSentence = "The daemon's reads of this pull request work again; what follows is everything since they failed."

// endedSentence closes a subscription's last notice.
const endedSentence = "The pull request has ended, so this subscription ended with this notice."

func short(sha string) string {
	if len(sha) > 12 {
		return sha[:12]
	}
	return sha
}
