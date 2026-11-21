package main

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"google.golang.org/protobuf/proto"
)

// notFound is GitHub's not-found failure, the one each GitHub arm's case
// carries: sayGithubFailure's own cases say every failure.
var notFound = &agentreplv1.PullRequestGitHubFailure{Cause: &agentreplv1.PullRequestGitHubFailure_NotFound{NotFound: &agentreplv1.PullRequestGitHubFailureNotFound{}}}

// subscriptionRefusals is each subscription verb's describer with its cases.
var subscriptionRefusals = func() []describedRefusal {
	follow := &agentreplv1.SubscribeWorkspaceError{}
	unfollow := &agentreplv1.UnsubscribeWorkspaceError{}
	pr := &agentreplv1.SubscribePullRequestError{}
	unpr := &agentreplv1.UnsubscribePullRequestError{}
	prs := &agentreplv1.ListPullRequestSubscriptionsError{}
	queue := &agentreplv1.SubscribeGithubMergeQueueError{}
	unqueue := &agentreplv1.UnsubscribeGithubMergeQueueError{}
	queues := &agentreplv1.ListGithubMergeQueueSubscriptionsError{}
	run := &agentreplv1.SubscribeGithubWorkflowRunError{}
	unrun := &agentreplv1.UnsubscribeGithubWorkflowRunError{}
	runs := &agentreplv1.ListGithubWorkflowRunSubscriptionsError{}
	comment := &agentreplv1.CommentOnPullRequestError{}
	const closed = "ws is closed: it runs no agent to tell, and a subscription taken while closed would never end"
	const notFoundSaid = "GitHub answered not found: it was deleted, renamed, or never readable to the gh login"
	unknown := func(empty proto.Message) refusalCase {
		return refusalCase{name: "unknown_subscriber", err: armed(empty, "unknown_subscriber", nil), want: "the daemon has no workspace ws"}
	}
	return []describedRefusal{
		{name: "follow", empty: follow, oneof: "cause",
			cases: []refusalCase{
				unknown(follow),
				{name: "unknown_followed", err: armed(follow, "unknown_followed", nil), want: "the daemon has no workspace peer"},
				{name: "subscriber_closed", err: armed(follow, "subscriber_closed", nil), want: closed},
				{name: "follows_itself", err: armed(follow, "follows_itself", nil), want: "peer is the subscriber itself, which never follows itself"},
			},
			describe: func(e proto.Message) (string, error) {
				return describeWorkspaceSubscribeRefusal("ws", "peer", e.(*agentreplv1.SubscribeWorkspaceError))
			}},
		{name: "unfollow", empty: unfollow, oneof: "cause", cases: []refusalCase{unknown(unfollow)},
			describe: func(e proto.Message) (string, error) {
				return describeWorkspaceUnsubscribeRefusal("ws", e.(*agentreplv1.UnsubscribeWorkspaceError))
			}},
		{name: "pull request", empty: pr, oneof: "cause",
			cases: []refusalCase{
				unknown(pr),
				{name: "subscriber_closed", err: armed(pr, "subscriber_closed", nil), want: closed},
				{name: "unparsable", err: armedWith(pr, "unparsable", map[string]any{"text": "#x", "reason": "no number"}), want: `"#x" names no pull request (no number)`},
				{name: "unreadable", err: armed(pr, "unreadable", &agentreplv1.SubscribePullRequestUnreadable{Cause: notFound}),
					want: "the pull request could not be read, so whether it can be followed is unknown: " + notFoundSaid},
			},
			describe: func(e proto.Message) (string, error) {
				return describePullRequestSubscribeRefusal("ws", e.(*agentreplv1.SubscribePullRequestError))
			}},
		{name: "pull request unsubscribe", empty: unpr, oneof: "cause", cases: []refusalCase{unknown(unpr)},
			describe: func(e proto.Message) (string, error) {
				return describePullRequestUnsubscribeRefusal("ws", e.(*agentreplv1.UnsubscribePullRequestError))
			}},
		{name: "pull request subscriptions", empty: prs, oneof: "cause", cases: []refusalCase{unknown(prs)},
			describe: func(e proto.Message) (string, error) {
				return describePullRequestSubscriptionsRefusal("ws", e.(*agentreplv1.ListPullRequestSubscriptionsError))
			}},
		{name: "github queue", empty: queue, oneof: "cause",
			cases: []refusalCase{
				unknown(queue),
				{name: "subscriber_closed", err: armed(queue, "subscriber_closed", nil), want: closed},
				{name: "not_on_github", err: armedWith(queue, "not_on_github", map[string]any{"origin": "git@gitlab.com:o/r"}),
					want: "ws's repository is not on github.com (its origin is git@gitlab.com:o/r)"},
				{name: "no_merge_queue", err: armed(queue, "no_merge_queue", &agentreplv1.SubscribeGithubMergeQueueNoMergeQueue{MergeQueue: &agentreplv1.GithubMergeQueueRef{
					Repository: &agentreplv1.GithubRepositoryRef{Owner: "o", Repo: "r"}, Branch: "main"}}),
					want: "GitHub runs no merge queue for o/r@main, so there is nothing to follow"},
				{name: "unreadable", err: armed(queue, "unreadable", &agentreplv1.SubscribeGithubMergeQueueUnreadable{Cause: notFound}),
					want: "the merge queue could not be read, so whether it can be followed is unknown: " + notFoundSaid},
			},
			describe: func(e proto.Message) (string, error) {
				return describeGithubQueueSubscribeRefusal("ws", e.(*agentreplv1.SubscribeGithubMergeQueueError))
			}},
		{name: "github queue unsubscribe", empty: unqueue, oneof: "cause", cases: []refusalCase{unknown(unqueue)},
			describe: func(e proto.Message) (string, error) {
				return describeGithubQueueUnsubscribeRefusal("ws", e.(*agentreplv1.UnsubscribeGithubMergeQueueError))
			}},
		{name: "github queue subscriptions", empty: queues, oneof: "cause", cases: []refusalCase{unknown(queues)},
			describe: func(e proto.Message) (string, error) {
				return describeGithubQueueSubscriptionsRefusal("ws", e.(*agentreplv1.ListGithubMergeQueueSubscriptionsError))
			}},
		{name: "github workflow run", empty: run, oneof: "cause",
			cases: []refusalCase{
				unknown(run),
				{name: "subscriber_closed", err: armed(run, "subscriber_closed", nil), want: closed},
				{name: "unparsable", err: armedWith(run, "unparsable", map[string]any{"text": "runs/x", "reason": "no run id"}), want: `"runs/x" names no workflow run (no run id)`},
				{name: "not_on_github", err: armedWith(run, "not_on_github", map[string]any{"origin": "git@gitlab.com:o/r"}),
					want: "ws's repository is not on github.com (its origin is git@gitlab.com:o/r)"},
				{name: "unreadable", err: armed(run, "unreadable", &agentreplv1.SubscribeGithubWorkflowRunUnreadable{Cause: notFound}),
					want: "the workflow run could not be read, so whether it can be followed is unknown: " + notFoundSaid},
			},
			describe: func(e proto.Message) (string, error) {
				return describeGithubRunSubscribeRefusal("ws", e.(*agentreplv1.SubscribeGithubWorkflowRunError))
			}},
		{name: "github workflow run unsubscribe", empty: unrun, oneof: "cause", cases: []refusalCase{unknown(unrun)},
			describe: func(e proto.Message) (string, error) {
				return describeGithubRunUnsubscribeRefusal("ws", e.(*agentreplv1.UnsubscribeGithubWorkflowRunError))
			}},
		{name: "github workflow run subscriptions", empty: runs, oneof: "cause", cases: []refusalCase{unknown(runs)},
			describe: func(e proto.Message) (string, error) {
				return describeGithubRunSubscriptionsRefusal("ws", e.(*agentreplv1.ListGithubWorkflowRunSubscriptionsError))
			}},
		{name: "comment", empty: comment, oneof: "cause",
			cases: []refusalCase{
				{name: "unknown_workspace", err: armed(comment, "unknown_workspace", nil), want: "the daemon has no workspace ws"},
				{name: "unknown_thread", err: armed(comment, "unknown_thread", nil), want: "the pull request has no line comment by the id -in-reply-to named"},
				{name: "not_posted", err: armed(comment, "not_posted", &agentreplv1.CommentOnPullRequestNotPosted{Cause: notFound}),
					want: "GitHub did not take the comment: " + notFoundSaid},
			},
			describe: func(e proto.Message) (string, error) {
				return describeCommentRefusal("ws", e.(*agentreplv1.CommentOnPullRequestError))
			}},
	}
}()

func TestDescribeSubscriptionRefusals(t *testing.T) {
	testDescribedRefusals(t, subscriptionRefusals)
}
