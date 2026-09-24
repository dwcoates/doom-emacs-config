package feedid

import (
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// owner.go — THE ONE RULE for which feed an agent's work is drawn in. The feed
// resolver places rows by it and the footer addresses its jump targets by it,
// so the row a footer chip points at and the row the feed drew can never be on
// two different feeds.

// ErrOwnerUnknown is the answer when nothing states whose work a thing is.
var ErrOwnerUnknown = errors.New("feedid: no owning agent is known")

// ErrMainAgentUnknown is the answer when the session's main agent has not been
// named yet, so no agent can be told apart from it.
var ErrMainAgentUnknown = errors.New("feedid: the session's main agent is not named yet")

// AgentFeed answers the feed an agent's own work is drawn in: the ROOT for the
// session's main agent, and the agent's own sub-feed for any other.
//
// THE ROOT IS NEVER A DEFAULT. An agent is on the root only because it IS the
// main agent; an empty owner or an unnamed main agent is an error, never a
// reason to fall back to the root.
func AgentFeed(owner, mainAgent string) (Feed, error) {
	switch {
	case owner == "":
		return Feed{}, ErrOwnerUnknown
	case mainAgent == "":
		return Feed{}, ErrMainAgentUnknown
	case owner == mainAgent:
		return Feed{Root: true}, nil
	default:
		return Feed{Agent: &conversationv1.AgentId{Value: owner}}, nil
	}
}

// DetachedOwner answers whose work a detached item is, from the two places that
// can say: the owner the announcement STATES, and the agent whose stream CARRIED
// the spawning call. Either alone answers; both must agree.
//
// NEITHER IS A DEFAULT FOR THE OTHER'S DISAGREEMENT. Two sources naming two
// different owners is a contradiction the caller reports, not one it settles by
// preference.
func DetachedOwner(stated, carrier string) (string, error) {
	switch {
	case stated != "" && carrier != "" && stated != carrier:
		return "", fmt.Errorf("feedid: the announcement names owner %q but the spawning call was carried by %q", stated, carrier)
	case stated != "":
		return stated, nil
	case carrier != "":
		return carrier, nil
	default:
		return "", ErrOwnerUnknown
	}
}
