package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
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
	// startHeld records that this process drew the unit's START frame, so a
	// settled frame that fails to restate what its call named can still be
	// drawn from what the start said. A replay that serves the unit's latest
	// frame alone never sets it: the store keeps one row per unit, so once the
	// call settles its start is gone. See restatedOrHeld.
	startHeld bool
	// denied records that the permission gate refused this call, so a late
	// frame never redraws it as running.
	denied bool
	// name is the tool's drawn name, remembered because a card is sometimes
	// RESTATED by something that is not one of the tool's own frames -- a
	// detachment announcement, which names the unit and nothing else -- and
	// re-minting the card needs the name the unit already drew under.
	name string
	// moved records that this call's WORK LEFT for the background, so no later
	// frame ever redraws it.
	//
	// A MOVE IS NOT AN ENDING. The foreground running card is RETIRED when the
	// work detaches, and the run's head becomes its detached shell bubble
	// (KindShellHead) — a canonical, expandable bubble whose spool streams on
	// its own sub-feed. Kept for the same reason `denied` and `sendDelivery` are
	// kept: the producer restates the unit's own frames after the move (the
	// vendor's receipt for the launch, a replay from the other plane, the next
	// turn's live-work reconciliation), and none of them says the work moved, so
	// drawn from the frame alone a second, stale card drew beside the bubble.
	// A moved unit draws nothing.
	moved bool
	// movedTo is the detached work a moved unit's head became, so the unit's
	// OWN ending can settle that head (see ending).
	movedTo string
	// ending is the call's own terminal frame (success or failure), kept
	// because a shell's ending and its move can arrive in EITHER order. A
	// call's work ends once: when the move lands after the result, the head it
	// is redrawn as must be drawn settled from this, not live.
	ending *conversationv1.AgentBash
	// diagnostics are the IDE findings raised against this change, composed.
	diagnostics []string
	// row is the last row this unit drew, so a post-terminal frame (an
	// injected diagnostics report) can amend the settled card rather than
	// composing a second one.
	row *frontendv1.FeedRow
	// feedKey is the feed that row landed on.
	feedKey string
	// carrier is the agent whose stream carried this unit's call: the unit's
	// OWNER, the agent a detachment from it belongs to. Recorded at the unit's
	// first drawn row.
	carrier string
	// at is where that row was drawn. A detached shell's head is drawn HERE,
	// in place of the card, and nowhere else.
	at placement
	// artifactFavicon is the emoji the PUBLISH announced. Kept because the
	// published outcome restates the title and never the favicon, and
	// feed.proto words the artifact heading as "favicon emoji + title" — so a
	// heading recomposed from the outcome alone would lose the glyph the
	// bubble had while it was publishing.
	artifactFavicon string
	// sendAddressedTo is WHO a send addressed, exactly as the caller wrote it.
	// Kept because the send's success arm resolves an identity but never
	// restates the addressed string, and the address line prefers a name a
	// reader recognizes.
	sendAddressedTo string
	// monitor records that this unit is a MONITOR's tool-call card, which is
	// the monitor's feed entry itself: a detachment naming it continues
	// nothing, and it is never redrawn as a shell head.
	monitor bool
	// drawsNoRow records that this unit's KIND draws no feed row at all —
	// a wakeup, an unmodeled tool. It is not "has not drawn yet":
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
	// settledAtMs is the instant the fold settled, epoch ms, stamped ONCE from
	// the resolver's clock at the first terminal frame. The cost corner carries
	// it so the client can reveal a relative timestamp counted back from it;
	// stamping once keeps a re-delivery of the terminal (the file plane after
	// the stream plane) serving the same instant rather than the replay time.
	settledAtMs int64
	// turn is the turn this response block belongs to, learned when the fold is
	// first drawn. It scopes CROSS-UNIT reconciliation: one turn's response
	// block can reach the resolver under two DIFFERENT activity ids when the two
	// store planes disagree on the unit — the shim's stream pays out start+delta
	// updates under one id while the settling whole (the stream's own reconciled
	// final id, or the sidecar's transcript id) lands under another — and a fold
	// only ever reconciles against a sibling of the SAME turn.
	turn string
	// feed is where this fold's row landed, kept so a divergent sibling can be
	// retired on the feed it actually drew on.
	feed feedid.Feed
	// row is the fold's own row identity, so a settled sibling's whole can retire
	// this fragment's row when the two are the same block under divergent ids.
	row *frontendv1.FeedId
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

// thinkingProse resolves a reasoning block's fold, creating it on first sight.
// It reuses proseState — a thinking bubble folds its text deltas exactly as a
// response does — but is filed in its OWN map so it never crosses paths with a
// response bubble's fold.
func (s *wsState) thinkingProse(id string) *proseState {
	p, ok := s.thinking[id]
	if ok {
		return p
	}
	p = &proseState{}
	s.thinking[id] = p
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
	// card: observed in a headless sandbox run's own picture and pinned by
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
	// startedAtMs is the ORIGINAL instant; detaching does not reset it. It is
	// AUTHORITATIVE: only a source that actually saw the command issued sets it
	// — the foreground unit's own start, or a `start` frame on the run's stream.
	startedAtMs int64
	// firstObservedMs is the daemon-clock instant this run was FIRST seen, kept
	// only as the clock's fallback when no authoritative start has arrived yet.
	//
	// A DETACHED RUN'S FIRST FRAME NEED NOT BE ITS `start`. Two producers write
	// one run under one key — the shim's stream and the sidecar's spool tail —
	// so a reconnect or replay legitimately delivers an `update`/`progress`
	// BEFORE the re-announced `start`. Drawn from startedAtMs alone that window
	// stamped the runtime at zero, and the live clock counted up from the epoch
	// — an absurd age (observed as ~56 years). The daemon stamps this once on
	// first sight so the clock counts from a sane instant until the real start
	// lands, at which point startedAtMs takes over and the clock corrects.
	firstObservedMs int64
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
	// feed is where the bubble landed: the spawning card's own placement, and
	// the zero value until that card is known. A run whose head has nowhere to
	// land accumulates its spool and publishes nothing.
	feed placement
	// turn is the SPAWNING TURN — the turn the replaced card was stamped with —
	// which the head carries rather than whatever turn is running when it is
	// drawn.
	turn *conversationv1.TurnId
	// settled is HOW THE RUN ENDED, once it has, kept for the same reason
	// `denied` and `sendDelivery` are kept: a later frame must never redraw a
	// settled bubble as unsettled.
	//
	// A RUN ENDS ONCE, and every push after that one is a restatement of a
	// finished run: an announcement replayed on the next turn's live-work
	// reconciliation, a spool replay from the other plane, a beat. None of them
	// carries the settled state — only the run's own terminal does — so drawn
	// from the frame alone the bubble walked BACK to live, an orange dot and a
	// stop button over a spool holding `EXIT=0`. Owner 13's F43 pictures caught
	// it: every per-row assertion passed, each reading the row a moment after
	// it settled, and the LATER captures showed the finished runs live again.
	settled *frontendv1.FeedShellSettled
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
	// carrier is the agent whose stream carried the spawn: the bubble's owner.
	carrier string
	// created is the agent the spawn produced — the sub-feed's address.
	created *conversationv1.AgentId
	// bubble is the head as last drawn.
	bubble *frontendv1.FeedSubagent
	// firstObservedMs is the daemon-clock instant this bubble was FIRST
	// composed, kept only as the LIVE clock's fallback when no authoritative
	// start has arrived. It mirrors shellState.firstObservedMs and exists for
	// the same reason: two producers write one spawn under one key (the shim's
	// stream and the sidecar's file tail), so a reconnect or replay can deliver
	// an update BEFORE the re-announced start. Drawn from a zero start that
	// window stamped the runtime at the epoch and the live clock counted up from
	// it (the reported ~492762h). Stamped ONCE, off the daemon clock, so the
	// clock counts from a sane instant until the real start lands and takes over.
	firstObservedMs int64
	// durationMs is the settled run's own wall-clock span, from
	// AgentSubagentTotals.duration_ms. It reconstructs a SETTLED-ONLY bubble's
	// start (end − duration) when no start frame was ever delivered — a replayed
	// history is exactly that — so the settled clock shows the run's real
	// elapsed rather than the whole age of the epoch.
	durationMs uint64
	// detached records that the bubble is drawn through the detached wrapper.
	detached bool
	// work is the run's detached-work id once it is detached work, empty while
	// the spawn is the turn's own progress. The head draws it verbatim
	// (FeedSubagent.work_id).
	work string
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

// heldDetachment is what a detachment held against an undrawn unit said about
// the work, for the claim's owner check and for the report if it never lands.
type heldDetachment struct {
	// work is the handle.
	work string
	// owner is the owner the announcement stated, empty when it stated none.
	owner string
	// announcer is the agent whose stream carried the announcement.
	announcer string
	// plane is the ordering plane the announcement arrived on. A detachment
	// REPLAYED from history names a unit that may lie in older history this
	// daemon never replayed, which is not a unit that failed to draw.
	plane rowPlane
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
