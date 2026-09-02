package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// The accumulation. Every one of these exists because a frame of an upserted
// unit is SELF-DESCRIBING but not self-sufficient for DRAWING: a settled read
// restates its path but not the instant it began, a prose fragment carries
// only what arrived, a subagent's settled bubble must keep the clock it
// started. Resolver memory holds exactly what the wire cannot restate, and
// nothing here is ever persisted.

// unitState is one activity unit's carried facts.
type unitState struct {
	// startedAtMs is the instant the call was issued, kept so a settled card
	// can state its runtime.
	startedAtMs int64
	// lastProgressMs is the last sign of life the daemon observed.
	lastProgressMs int64
	// input is the composed input line, kept so a permission card can name the
	// call it gates.
	input string
	// inputForm is how that line is drawn.
	inputForm inputForm
	// denied records that the permission gate refused this call, so a late
	// frame never redraws it as running.
	denied bool
	// diagnostics are the IDE findings raised against this change, composed.
	diagnostics []string
	// row is the last row this unit drew, so a post-terminal frame (an
	// injected diagnostics report) can amend the settled card rather than
	// composing a second one.
	row *frontendv1.FeedRow
	// feedKey is the feed that row landed on.
	feedKey string
}

// unit resolves a unit's accumulation, creating it on first sight.
func (s *wsState) unit(id string) *unitState {
	u, ok := s.units[id]
	if ok {
		return u
	}
	u = &unitState{}
	s.units[id] = u
	return u
}

// proseState is one response bubble's fold: the fragments the daemon
// accumulates so the client accumulates nothing and a missed push
// self-corrects on the next one.
type proseState struct {
	// markdown is the prose so far.
	markdown string
	// usage is the formatted cost corner, kept across pushes because usage is
	// stated when the response opens and restated as it grows.
	usage string
	// settled records that the terminal frame restated the whole, so a late
	// fragment can never re-open a closed bubble.
	settled bool
}

// prose resolves a response's fold, creating it on first sight.
func (s *wsState) prose(id string) *proseState {
	p, ok := s.responses[id]
	if ok {
		return p
	}
	p = &proseState{}
	s.responses[id] = p
	return p
}

// planState is one agent's OPEN plan episode. An agent has at most one at a
// time, which is the invariant the enter/exit coalescing rests on: both calls
// key onto the episode's single FeedId.
type planState struct {
	// episode numbers the agent's episodes so a second one never collides with
	// the first's identity.
	episode uint64
	// row is the bubble's identity.
	row *frontendv1.FeedId
	// feed is where the bubble landed.
	feed placement
	// turn is the turn the bubble belongs to.
	turn *conversationv1.TurnId
}

// shellState is one detached shell's accumulation: the spool the daemon caps
// and replaces whole on every push.
type shellState struct {
	// command is the command line, drawn verbatim.
	command string
	// startedAtMs is the ORIGINAL instant; detaching does not reset it.
	startedAtMs int64
	// spool is the accumulated output.
	spool string
	// nextOffset is the byte offset the next update must start at. A frame
	// that does not is a GAP, and the resolver refuses it rather than
	// concatenating across a hole.
	nextOffset uint64
	// lastProgressMs is the last append the daemon observed.
	lastProgressMs int64
	// row is the bubble's identity.
	row *frontendv1.FeedId
	// feed is where the bubble landed.
	feed placement
}

// shell resolves a detached shell's accumulation, creating it on first sight.
func (s *wsState) shell(id string) *shellState {
	sh, ok := s.shells[id]
	if ok {
		return sh
	}
	sh = &shellState{}
	s.shells[id] = sh
	return sh
}

// permissionState is one consent card's carried facts, kept so the decision
// frame upserts the same row on the same feed.
type permissionState struct {
	// row is the card's identity.
	row *frontendv1.FeedId
	// feed is where the card landed.
	feed placement
	// card is the card as last drawn, replaced whole on a decision.
	card *frontendv1.FeedPermission
	// turn is the turn the card belongs to.
	turn *conversationv1.TurnId
	// agent is who asked. THE ANSWER VERB NEEDS IT: an answer is delivered to
	// the agent that is blocked, and the client sends only the ask's identity.
	agent *conversationv1.AgentId
}

// questionState is what the daemon SERVED for one question ask, which is what
// an answer is echoed against.
type questionState struct {
	// agent is who asked; the answer is delivered back to it.
	agent *conversationv1.AgentId
	// batch is the batch as served, so an answer naming a question the batch
	// never carried is refused rather than forwarded.
	batch *conversationv1.AgentQuestionBatch
}

// subagentState is one bubble's carried facts across its frames.
type subagentState struct {
	// row is the bubble's identity.
	row *frontendv1.FeedId
	// created is the agent the spawn produced — the sub-feed's address.
	created *conversationv1.AgentId
	// bubble is the head as last drawn.
	bubble *frontendv1.FeedSubagent
	// detached records that the bubble is drawn through the detached wrapper.
	detached bool
	// feed is where the bubble landed.
	feed placement
}
