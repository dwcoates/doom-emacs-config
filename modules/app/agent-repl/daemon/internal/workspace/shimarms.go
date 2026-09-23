package workspace

import (
	"errors"
	"fmt"

	shimv1 "agentrepl/proto/shim/v1"
)

// The shim's own refusal arm names, spelled exactly as the shim.v1 failure
// oneofs spell them. They are constants because each one becomes a row in
// daemon/ERROR-ARMS.md and a verb's answer to the caller, and the three must
// not drift.
const (
	// ArmShimUnknownAgent is an agent the shim does not know: the addressed row
	// is stale.
	ArmShimUnknownAgent = "unknown_agent"
	// ArmShimNoOpenAsk is an answer to an ask the shim has already closed.
	ArmShimNoOpenAsk = "no_open_ask"
	// ArmShimAnswerMismatch is an answer whose shape the shim rejected.
	ArmShimAnswerMismatch = "answer_mismatch"
	// ArmShimNothingRunning is a stop with nothing to stop.
	ArmShimNothingRunning = "nothing_running"
	// ArmShimNoSession is a shim verb against a session that is not up.
	ArmShimNoSession = "no_session"
	// ArmShimNotDeliverable is the SDK limit landed in landing 3: a prompt
	// addressed to a subagent has NO SDK ROUTE. It is answered honestly rather
	// than swallowed — the control is not hidden this wave, so the caller is
	// told the route does not exist.
	ArmShimNotDeliverable = "not_deliverable"
	// ArmShimAgentBusy is the shim's UpdateAgentFailure.agent_busy (landing 7):
	// the addressed subagent's OWN turn is already open, so the prompt has no
	// place to land. The daemon never judges a subagent's turn itself; it
	// relays the shim's verdict.
	ArmShimAgentBusy = "agent_busy"
	// ArmShimUnknownWork is a detached shell the shim does not know.
	ArmShimUnknownWork = "unknown_work"
	// ArmShimAlreadyEnded is a detached shell that has already finished.
	ArmShimAlreadyEnded = "already_ended"
	// ArmShimTurnLive is a kill the shim refused because the turn is still
	// live.
	ArmShimTurnLive = "live"
	// ArmShimNotTheOpenTurn is a kill naming a turn that is not the open one.
	ArmShimNotTheOpenTurn = "not_the_open_turn"
	// ArmShimNoTurnOpen is a kill with no turn open.
	ArmShimNoTurnOpen = "no_turn_open"
	// ArmShimQueryRefusedToEnd is a session kill the vendor query would not
	// honor.
	ArmShimQueryRefusedToEnd = "query_refused_to_end"
	// ArmShimModelNotInCatalog is SetSessionModelFailure.model_not_in_catalog:
	// the shim's own catalog does not carry the model. The daemon's own
	// `SetModelError` spells the SAME condition `not_in_catalog`, so the rpc
	// handler RENAMES this arm onto that one rather than let the shim's
	// spelling answer as an unlanded arm.
	ArmShimModelNotInCatalog = "model_not_in_catalog"
	// ArmShimCold is SetSessionModelFailure.cold: the switch would discard a
	// warm cache above the threshold the daemon stated. `SetModelError` spells
	// it with the SAME name (landing 10), so it relays by name; the daemon's
	// own policy keeps it from arising (see sender.SetModel), and the
	// remediation menu (pay | clear | compact) is the cold gate row's.
	ArmShimCold = "cold"
	// ArmShimUnspecified is a failure whose kind oneof is unset, which is
	// illegal on the wire and is surfaced rather than guessed at.
	ArmShimUnspecified = "unspecified"
	// ArmShimRefused is InterruptError.shim_refused: the shim would not perform
	// the kill and named no arm the contract carries — an unset kind oneof, or
	// a failure at the transport under it. It carries the shim's own words as
	// `detail`, and is the FALLTHROUGH, never a substitute for an arm the shim
	// did name.
	ArmShimRefused = "shim_refused"
)

// ShimRefusal is one shim verb's TYPED refusal, carried up to the verb that
// asked so the caller learns WHICH refusal it was rather than a sentence.
//
// It exists because the shim's failure oneofs are the only place that
// distinguishes "the row you clicked is stale" from "the SDK has no route for
// this at all", and collapsing both into one error string would have the verbs
// answer every shim refusal identically.
type ShimRefusal struct {
	// Verb is the shim verb that refused.
	Verb string
	// Arm is the failure oneof's arm name.
	Arm string
	// Detail is the shim's own sentence, kept as evidence.
	Detail string
	// TransientKeepalive is set only on a StartTurn turn_already_open refusal
	// whose open turn is one of the shim's OWN keep-alive pings. Such a
	// collision is transient — the ping closes on its own — so the queue
	// re-drives the prompt rather than surfacing a terminal error. It stays
	// false for a genuine daemon double-submit, which is the daemon's own bug.
	TransientKeepalive bool
}

// KeepaliveTurnAlreadyOpen reports that this refusal is a StartTurn that a
// KEEP-ALIVE turn momentarily blocked — the one turn_already_open case the
// queue re-drives rather than treating as terminal. It is the method the
// prompt queue matches structurally (via a package-local interface) so it can
// classify the refusal without importing this package.
func (r *ShimRefusal) KeepaliveTurnAlreadyOpen() bool {
	return r.Verb == "StartTurn" && r.Arm == "turn_already_open" && r.TransientKeepalive
}

// KillRefusedLive reports that this refusal is a KillTurn the shim declined
// because the turn is still LIVE — it spawned detached work the shim will not
// tear down under an ordinary interrupt. It is an expected domain outcome, not
// a fault: the prompt queue matches it structurally (via a package-local
// interface, as it does KeepaliveTurnAlreadyOpen) so a refused interjection
// is narrated at the level its nature earns.
func (r *ShimRefusal) KillRefusedLive() bool {
	return r.Verb == "KillTurn" && r.Arm == ArmShimTurnLive
}

// Error renders the verb, the arm and the shim's own words, because the
// evidence is the point.
func (r *ShimRefusal) Error() string {
	if r.Detail == "" {
		return fmt.Sprintf("shim %s refused: %s", r.Verb, r.Arm)
	}
	return fmt.Sprintf("shim %s refused: %s: %s", r.Verb, r.Arm, r.Detail)
}

// AsShimRefusal reports whether err is a typed shim refusal, which is how a
// verb decides between propagating a named arm and reporting a transport
// failure.
func AsShimRefusal(err error) (*ShimRefusal, bool) {
	var refusal *ShimRefusal
	if errors.As(err, &refusal) {
		return refusal, true
	}
	return nil, false
}

// Benign reports whether this refusal is a DOMAIN OUTCOME rather than a
// failure: the thing the caller asked to stop was already not running. Those
// answer as success — "nothing running" is an answer — so a verb never turns
// one into a refusal.
func (r *ShimRefusal) Benign() bool {
	switch r.Arm {
	case ArmShimNothingRunning, ArmShimAlreadyEnded, ArmShimNoTurnOpen:
		return true
	default:
		return false
	}
}

// GoneFromTheSweep reports whether this refusal means the swept item is no
// longer there to stop: the shim has forgotten the agent or the shell run
// entirely. It is DISTINCT from Benign, which is the answer a SINGLE addressed
// stop gives its caller — an addressed row the shim has forgotten is a stale
// row the caller clicked, and it is told so by name.
//
// A FAN-WIDE sweep never addresses a specific item, so it has no such row to
// report on: it walks the freeness read's snapshot, and an item the shim has
// already dropped between that read and the stop is exactly the state the
// caller asked for. Answering it with a per-agent arm the Interrupt contract
// does not carry (endpoint_interrupt.proto's InterruptError has no
// `unknown_agent`) would fail a stop that in fact succeeded.
func (r *ShimRefusal) GoneFromTheSweep() bool {
	switch r.Arm {
	case ArmShimUnknownAgent, ArmShimUnknownWork:
		return true
	default:
		return r.Benign()
	}
}

// updateAgentArm names an UpdateAgent failure's arm.
func updateAgentArm(failure *shimv1.UpdateAgentFailure) string {
	switch failure.GetKind().(type) {
	case *shimv1.UpdateAgentFailure_UnknownAgent:
		return ArmShimUnknownAgent
	case *shimv1.UpdateAgentFailure_NoOpenAsk:
		return ArmShimNoOpenAsk
	case *shimv1.UpdateAgentFailure_AnswerMismatch:
		return ArmShimAnswerMismatch
	case *shimv1.UpdateAgentFailure_NothingRunning:
		return ArmShimNothingRunning
	case *shimv1.UpdateAgentFailure_NoSession:
		return ArmShimNoSession
	case *shimv1.UpdateAgentFailure_NotDeliverable:
		return ArmShimNotDeliverable
	case *shimv1.UpdateAgentFailure_AgentBusy:
		return ArmShimAgentBusy
	default:
		return ArmShimUnspecified
	}
}

// stopBashArm names a StopBash failure's arm.
func stopBashArm(failure *shimv1.StopBashFailure) string {
	switch failure.GetKind().(type) {
	case *shimv1.StopBashFailure_UnknownWork:
		return ArmShimUnknownWork
	case *shimv1.StopBashFailure_AlreadyEnded:
		return ArmShimAlreadyEnded
	default:
		return ArmShimUnspecified
	}
}

// killTurnArm names a KillTurn failure's cause.
func killTurnArm(failure *shimv1.KillTurnFailure) string {
	switch failure.GetCause().(type) {
	case *shimv1.KillTurnFailure_Live:
		return ArmShimTurnLive
	case *shimv1.KillTurnFailure_NotTheOpenTurn:
		return ArmShimNotTheOpenTurn
	case *shimv1.KillTurnFailure_NoTurnOpen:
		return ArmShimNoTurnOpen
	case *shimv1.KillTurnFailure_NoSession:
		return ArmShimNoSession
	default:
		return ArmShimUnspecified
	}
}

// killSessionArm names a KillSession failure's cause.
func killSessionArm(failure *shimv1.KillSessionFailure) string {
	switch failure.GetCause().(type) {
	case *shimv1.KillSessionFailure_Live:
		return ArmShimTurnLive
	case *shimv1.KillSessionFailure_NoSession:
		return ArmShimNoSession
	case *shimv1.KillSessionFailure_QueryRefusedToEnd:
		return ArmShimQueryRefusedToEnd
	default:
		return ArmShimUnspecified
	}
}
