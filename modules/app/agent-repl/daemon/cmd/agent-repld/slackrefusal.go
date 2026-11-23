package main

import (
	"fmt"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// describeSlackSubscribeRefusal says in one plain sentence why WS was not
// subscribed to the thread. Every SubscribeSlackThreadError arm has its own
// sentence.
func describeSlackSubscribeRefusal(ws string, e *agentreplv1.SubscribeSlackThreadError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SubscribeSlackThreadError_UnknownSubscriber:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SubscribeSlackThreadError_SubscriberClosed:
		return fmt.Sprintf("%s is closed, so it follows no thread", ws), nil
	case *agentreplv1.SubscribeSlackThreadError_Unreachable:
		return saySlackUnreachable("the thread could not be followed", r.Unreachable.GetDetail()), nil
	}
	return "", undescribed("subscribing "+ws+" to a Slack thread", e)
}

// describeSlackUnsubscribeRefusal says why WS's subscription was not ended.
func describeSlackUnsubscribeRefusal(ws string, e *agentreplv1.UnsubscribeSlackThreadError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.UnsubscribeSlackThreadError_UnknownSubscriber:
		return sayNoWorkspace(ws), nil
	}
	return "", undescribed("unsubscribing "+ws+" from a Slack thread", e)
}

// describeSlackModeRefusal says why WS's subscription kept its mode.
func describeSlackModeRefusal(ws string, e *agentreplv1.SetSlackThreadModeError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.SetSlackThreadModeError_UnknownSubscriber:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SetSlackThreadModeError_NotSubscribed:
		return fmt.Sprintf("%s does not follow the thread; slack-subscribe it first", ws), nil
	}
	return "", undescribed("setting "+ws+"'s Slack thread mode", e)
}

// describeSlackSubscriptionsRefusal says why WS's subscriptions were not
// listed.
func describeSlackSubscriptionsRefusal(ws string, e *agentreplv1.ListSlackThreadSubscriptionsError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.ListSlackThreadSubscriptionsError_UnknownSubscriber:
		return sayNoWorkspace(ws), nil
	}
	return "", undescribed("listing the Slack threads "+ws+" follows", e)
}

// describeSlackReadRefusal says why the thread was not read for WS.
func describeSlackReadRefusal(ws string, e *agentreplv1.ReadSlackThreadError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.ReadSlackThreadError_UnknownReader:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.ReadSlackThreadError_Unreachable:
		return saySlackUnreachable("the thread could not be read", r.Unreachable.GetDetail()), nil
	}
	return "", undescribed("reading a Slack thread for "+ws, e)
}

// describeSlackPostRefusal says why WS's post was not made. Every
// PostToSlackThreadError arm has its own sentence.
func describeSlackPostRefusal(ws string, e *agentreplv1.PostToSlackThreadError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.PostToSlackThreadError_UnknownPoster:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.PostToSlackThreadError_NotSubscribed:
		return fmt.Sprintf("%s does not follow the thread; it posts only in threads it follows", ws), nil
	case *agentreplv1.PostToSlackThreadError_NoEmojiPrefix:
		return "a post opens with exactly one emoji shortcode and a space (`:white_check_mark: tests pass`)", nil
	case *agentreplv1.PostToSlackThreadError_ReservedEmoji:
		return fmt.Sprintf("%s is reserved for the daemon's own posts; open with another emoji", r.ReservedEmoji.GetEmoji()), nil
	case *agentreplv1.PostToSlackThreadError_ListenMode:
		return fmt.Sprintf("%s follows the thread in listen mode, which posts nothing; draft the reply for the user, or -force once they approve it", ws), nil
	case *agentreplv1.PostToSlackThreadError_PostLimitReached:
		posts := r.PostLimitReached.GetPosts()
		return fmt.Sprintf("%s made %d of its %d posts in the thread and now listens; ask the user before posting again with -force", ws, posts.GetMade(), posts.GetLimit()), nil
	case *agentreplv1.PostToSlackThreadError_Secret:
		return fmt.Sprintf("the post carries what reads as a secret (%s); nothing was posted", strings.Join(r.Secret.GetKinds(), ", ")), nil
	case *agentreplv1.PostToSlackThreadError_Unreachable:
		return saySlackUnreachable("nothing was posted", r.Unreachable.GetDetail()), nil
	}
	return "", undescribed("posting to a Slack thread for "+ws, e)
}

// describeSlackReactionsRefusal says why the reaction settings were left as
// they stood.
func describeSlackReactionsRefusal(e *agentreplv1.UpdateSlackReactionSettingsError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.UpdateSlackReactionSettingsError_UnknownRepository:
		return "the daemon has no repository by that directory; register it, or name another", nil
	case *agentreplv1.UpdateSlackReactionSettingsError_NotBound:
		return fmt.Sprintf(":%s: is bound to nothing, so there is nothing to unbind", r.NotBound.GetEmoji()), nil
	}
	return "", undescribed("changing the Slack reaction settings", e)
}

// saySlackUnreachable is every unreachable arm's sentence: what did not
// happen, and Slack's own words for why.
func saySlackUnreachable(outcome, detail string) string {
	if detail == "" {
		return outcome + ": Slack could not be reached through gns"
	}
	return outcome + ": Slack could not be reached through gns (" + detail + ")"
}
