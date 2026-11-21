package main

import (
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// THE SUBSCRIPTION VERBS' REFUSALS: following a workspace, a pull request or
// a GitHub merge queue, listing those subscriptions, and commenting on a pull
// request. Each describer says every arm of its verb's error in one plain
// sentence about SUBSCRIBER, the workspace as the caller named it.

// describeWorkspaceSubscribeRefusal says in one plain sentence why SUBSCRIBER
// was not subscribed to FOLLOWED. Every SubscribeWorkspaceError arm has its
// own sentence.
func describeWorkspaceSubscribeRefusal(subscriber, followed string, e *agentreplv1.SubscribeWorkspaceError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.SubscribeWorkspaceError_UnknownSubscriber:
		return sayNoWorkspace(subscriber), nil
	case *agentreplv1.SubscribeWorkspaceError_UnknownFollowed:
		return sayNoWorkspace(followed), nil
	case *agentreplv1.SubscribeWorkspaceError_SubscriberClosed:
		return saySubscriberClosed(subscriber), nil
	case *agentreplv1.SubscribeWorkspaceError_FollowsItself:
		return fmt.Sprintf("%s is the subscriber itself, which never follows itself", followed), nil
	}
	return "", undescribed("subscribing "+subscriber+" to "+followed, e)
}

// describeWorkspaceUnsubscribeRefusal says in one plain sentence why
// SUBSCRIBER was not unsubscribed. Every UnsubscribeWorkspaceError arm has
// its own sentence.
func describeWorkspaceUnsubscribeRefusal(subscriber string, e *agentreplv1.UnsubscribeWorkspaceError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.UnsubscribeWorkspaceError_UnknownSubscriber:
		return sayNoWorkspace(subscriber), nil
	}
	return "", undescribed("unsubscribing "+subscriber, e)
}

// describePullRequestSubscribeRefusal says in one plain sentence why
// SUBSCRIBER was not subscribed to a pull request. Every
// SubscribePullRequestError arm has its own sentence.
func describePullRequestSubscribeRefusal(subscriber string, e *agentreplv1.SubscribePullRequestError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SubscribePullRequestError_UnknownSubscriber:
		return sayNoWorkspace(subscriber), nil
	case *agentreplv1.SubscribePullRequestError_SubscriberClosed:
		return saySubscriberClosed(subscriber), nil
	case *agentreplv1.SubscribePullRequestError_Unparsable:
		return fmt.Sprintf("%q names no pull request (%s)", r.Unparsable.GetText(), r.Unparsable.GetReason()), nil
	case *agentreplv1.SubscribePullRequestError_Unreadable:
		why, err := sayGithubFailure(r.Unreadable.GetCause())
		if err != nil {
			return "", err
		}
		return "the pull request could not be read, so whether it can be followed is unknown: " + why, nil
	}
	return "", undescribed("subscribing "+subscriber+" to a pull request", e)
}

// describePullRequestUnsubscribeRefusal says in one plain sentence why
// SUBSCRIBER was not unsubscribed from a pull request. Every
// UnsubscribePullRequestError arm has its own sentence.
func describePullRequestUnsubscribeRefusal(subscriber string, e *agentreplv1.UnsubscribePullRequestError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.UnsubscribePullRequestError_UnknownSubscriber:
		return sayNoWorkspace(subscriber), nil
	}
	return "", undescribed("unsubscribing "+subscriber+" from a pull request", e)
}

// describePullRequestSubscriptionsRefusal says in one plain sentence why
// SUBSCRIBER's pull-request subscriptions were not listed. Every
// ListPullRequestSubscriptionsError arm has its own sentence.
func describePullRequestSubscriptionsRefusal(subscriber string, e *agentreplv1.ListPullRequestSubscriptionsError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.ListPullRequestSubscriptionsError_UnknownSubscriber:
		return sayNoWorkspace(subscriber), nil
	}
	return "", undescribed("listing the pull requests "+subscriber+" follows", e)
}

// describeGithubQueueSubscribeRefusal says in one plain sentence why
// SUBSCRIBER was not subscribed to a GitHub merge queue. Every
// SubscribeGithubMergeQueueError arm has its own sentence.
func describeGithubQueueSubscribeRefusal(subscriber string, e *agentreplv1.SubscribeGithubMergeQueueError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SubscribeGithubMergeQueueError_UnknownSubscriber:
		return sayNoWorkspace(subscriber), nil
	case *agentreplv1.SubscribeGithubMergeQueueError_SubscriberClosed:
		return saySubscriberClosed(subscriber), nil
	case *agentreplv1.SubscribeGithubMergeQueueError_NotOnGithub:
		return fmt.Sprintf("%s's repository is not on github.com (its origin is %s)", subscriber, r.NotOnGithub.GetOrigin()), nil
	case *agentreplv1.SubscribeGithubMergeQueueError_NoMergeQueue:
		q := r.NoMergeQueue.GetMergeQueue()
		return fmt.Sprintf("GitHub runs no merge queue for %s/%s@%s, so there is nothing to follow", q.GetRepository().GetOwner(), q.GetRepository().GetRepo(), q.GetBranch()), nil
	case *agentreplv1.SubscribeGithubMergeQueueError_Unreadable:
		why, err := sayGithubFailure(r.Unreadable.GetCause())
		if err != nil {
			return "", err
		}
		return "the merge queue could not be read, so whether it can be followed is unknown: " + why, nil
	}
	return "", undescribed("subscribing "+subscriber+" to a GitHub merge queue", e)
}

// describeGithubQueueUnsubscribeRefusal says in one plain sentence why
// SUBSCRIBER was not unsubscribed from a GitHub merge queue. Every
// UnsubscribeGithubMergeQueueError arm has its own sentence.
func describeGithubQueueUnsubscribeRefusal(subscriber string, e *agentreplv1.UnsubscribeGithubMergeQueueError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.UnsubscribeGithubMergeQueueError_UnknownSubscriber:
		return sayNoWorkspace(subscriber), nil
	}
	return "", undescribed("unsubscribing "+subscriber+" from a GitHub merge queue", e)
}

// describeGithubQueueSubscriptionsRefusal says in one plain sentence why
// SUBSCRIBER's GitHub merge-queue subscriptions were not listed. Every
// ListGithubMergeQueueSubscriptionsError arm has its own sentence.
func describeGithubQueueSubscriptionsRefusal(subscriber string, e *agentreplv1.ListGithubMergeQueueSubscriptionsError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.ListGithubMergeQueueSubscriptionsError_UnknownSubscriber:
		return sayNoWorkspace(subscriber), nil
	}
	return "", undescribed("listing the GitHub merge queues "+subscriber+" follows", e)
}

// describeCommentRefusal says in one plain sentence why no comment was posted
// for WS. Every CommentOnPullRequestError arm has its own sentence.
func describeCommentRefusal(ws string, e *agentreplv1.CommentOnPullRequestError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.CommentOnPullRequestError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.CommentOnPullRequestError_UnknownThread:
		return "the pull request has no line comment by the id -in-reply-to named", nil
	case *agentreplv1.CommentOnPullRequestError_NotPosted:
		why, err := sayGithubFailure(r.NotPosted.GetCause())
		if err != nil {
			return "", err
		}
		return "GitHub did not take the comment: " + why, nil
	}
	return "", undescribed("commenting for "+ws, e)
}
