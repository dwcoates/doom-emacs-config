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
	// sendAddressedTo is WHO a send addressed, exactly as the caller wrote it.
	// Kept because the send's success arm resolves an identity but never
	// restates the addressed string, and the address line prefers a name a
	// reader recognizes.
	sendAddressedTo string
	// drawsNoRow records that this unit's KIND draws no feed row at all —
	// a monitor, a wakeup, an unmodeled tool. It is not "has not drawn yet":
	// nothing will ever draw it, so a detachment naming it can never be
	// claimed by a row and is retired rather than held.
	drawsNoRow bool
	// sendSummary is the one-line preview a send's caller supplied. Kept
	// because it arrives only on the start arm and the row is recomposed from
	// scratch on every later frame. EMPTY MEANS NONE WAS GIVEN, which draws no
	// body rather than falling back to the message itself.
	sendSummary string
	// sendDelivery is HOW the send settled -- queued, resumed, or REFUSED --
	// kept for the same reason `denied` is kept: a late frame must never
	// redraw a settled unit as unsettled.
	//
	// A SEND'S UNIT IS DELIVERED TWICE, and that is by design rather than by
	// accident. The shim's stream plane converts the SDK's events live, and
	// the sidecar's file plane replays the SAME units out of the vendor
	// transcript under the SAME key, so a send arrives as start-then-terminal
	// and then, ~160ms later, as start-then-terminal again. Drawn from the
	// current frame alone, the replayed START un-stated a refusal the first
	// pass had already drawn: for the ~2ms between the replay's two frames
	// the row said nothing about delivery -- which feed.proto's `refused` arm
	// exists precisely to distinguish from "the producer stated nothing", and
	// which any reader that opened the feed in that window read as a message
	// still on its way to an agent that will never receive it. That window is
	// what `TestSendMessageRefused` caught, once in eight in-container runs.
	sendDelivery deliveryArm
	// sendResolved is the recipient identity the success arm resolved, kept
	// for the same reason: the replayed start restates the ADDRESSED string
	// and never the resolved id, so drawing from the frame alone walked the
	// address line back from the recipient's own bubble label to the raw
	// string the caller typed.
	sendResolved *conversationv1.AgentId
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
	// opener is the ACTIVITY ID of the call that opened the episode, and it is
	// what the bubble's FeedId is made of.
	//
	// A COUNTER CANNOT BE USED HERE, and that is a measurement rather than a
	// preference. Every other unit in this package keys on
	// `act.GetActivityId()`, which is why the SAME vendor record arriving on
	// both planes — the shim's stream and the sidecar's file tail — collapses
	// onto one row instead of drawing twice. The plan bubble alone numbered
	// its episodes, so the file plane's replay of one `!plan` turn found no
	// open episode, took the next number, and drew a SECOND identical plan
	// card: observed in the playtest's own picture and pinned by
	// TestPlanModeCoalescesOntoOneBubble. Keyed on the opener's activity id,
	// a re-delivery of the same call lands on the same FeedId by construction.
	opener string
	// row is the bubble's identity.
	row *frontendv1.FeedId
	// feed is where the bubble landed.
	feed placement
	// closed says the episode has reached its final state. A closed episode is
	// KEPT rather than forgotten, so the other plane's copies of its calls are
	// recognized as re-deliveries instead of opening a second episode.
	closed bool
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

// stateCommand records WHAT WAS RUN, once.
//
// THE FIRST STATEMENT OF A COMMAND IS THE BINDING ONE, and a later frame can
// only fill a gap it left. Two reasons, both load-bearing:
//
//   - An EMPTY restatement is not a fact. A terminal that omits the line says
//     nothing about the command, and letting it through would blank a bubble
//     that had been drawing the command correctly all along.
//   - A DISAGREEING restatement is not the command either. A run's terminal is
//     composed by whoever observed the run END — for a detached shell that is
//     the sidecar tailing a spool file, which knows the run by its handle and
//     not by the line the caller typed. The line the CALL itself stated (the
//     foreground unit's input, or the run's own start frame) is the one thing
//     that ever saw the command, so it is the one that stands.
//
// AgentBashSuccess.command is "what was run, repeated here so a settled frame
// describes itself" — a restatement, by the proto's own word, and a
// restatement never redefines.
func (sh *shellState) stateCommand(line string) {
	if sh.command != "" || line == "" {
		return
	}
	sh.command = line
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
	// held is the spawn's frames that arrived BEFORE its start, in arrival
	// order.
	//
	// ONLY THE START NAMES THE CREATED AGENT, and the bubble's own row id IS
	// that agent's sub-feed address, so a frame folded before the start lands
	// would publish a row nothing can open — and the start would then mint a
	// SECOND row beside it. The two planes carrying one run under one upsert
	// key (the shim's stream, the sidecar's file tail) make that order real
	// rather than hypothetical, so a pre-start frame waits here and is folded
	// the moment the start supplies the identity.
	held []*conversationv1.AgentSubagent
}

// markDetached remembers that a unit this resolver has not drawn yet has
// already left the turn, and under which handle the work is addressed.
func (s *wsState) markDetached(unit, work string) {
	s.detachedUnits[unit] = work
}

// markUndrawable records that a unit's kind draws no feed row, ever.
func (s *wsState) markUndrawable(unit string) {
	s.unit(unit).drawsNoRow = true
}

// undrawable reports whether a unit's kind is one the feed never draws.
func (s *wsState) undrawable(unit string) bool {
	u, ok := s.units[unit]
	return ok && u.drawsNoRow
}

// claimDetached answers the work handle a detachment announced for this unit
// before it drew, consuming the mark: the placement is now carried by the
// drawn element's own state, and a mark left standing would be reported as a
// detachment naming a unit nothing ever drew.
func (s *wsState) claimDetached(unit string) (string, bool) {
	work, ok := s.detachedUnits[unit]
	if !ok {
		return "", false
	}
	delete(s.detachedUnits, unit)
	return work, true
}
