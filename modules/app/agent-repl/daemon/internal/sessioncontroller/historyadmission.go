package sessioncontroller

import (
	"fmt"

	"claude-repld/internal/errclass"
	"claude-repld/internal/ssm"
)

// THE ONE ELIGIBILITY LADDER every frontend history request climbs.
//
// # ONE TOKEN, ONE COMPARISON
//
// A fence crosses the wire as ONE opaque token, and the contract says what a
// receiver does with it: byte-compare it against the workspace's current
// fence, never parse it (conversation-page.proto, feed.proto). This ladder is
// the daemon-side half of that comparison, and it makes it the SAME way.
//
// It used to reconstruct a session/generation pair out of the echo and compare
// the pair instead, which made the token have two readers with different
// semantics — a byte comparison for fenced views, a structural one here — and
// that divergence was load-bearing without anyone intending it. A workspace
// with no controller generation used to publish `Fence(session, "")`, which
// split back to `(session, "")` and matched, so an unwired workspace's durable
// history was reachable BY ACCIDENT of the two readers agreeing. When the mint
// was corrected to publish an absent fence for an absent generation (ssm's
// connectivity.go), the accident ended: `""` splits to `("", "")`, which
// cannot match `(session, "")`, and every durable history request started
// being refused — the blank feed after a daemon bounce, back again, this time
// through the admission ladder.
//
// Byte-comparing collapses the two readers into one. `""` echoed against `""`
// published matches, so an unwired workspace's history is reachable again
// without inventing anything, and a later change to how the fence is minted
// can no longer satisfy one consumer while silently breaking the other.
//
// THE SPLIT SURVIVES FOR EXACTLY ONE RUNG, below: a client whose SESSION id
// rotated under a still-live generation. That request is admitted on purpose,
// its token legitimately differs byte-wise from the live one, and no byte
// comparison can express it. It is the single place this daemon reads inside
// the token, and it is guarded by a live controller and a non-empty
// generation.
//
// # Why it is one ladder and not two
//
// A frontend can ask this daemon for its conversation in exactly two ways — a
// RESYNC from a mark (sessioncontroller.go) and a PAGE from an anchor
// (conversationpage.go) — and both ask the same admission question first: is
// the identity this client echoed still the one that owns the workspace? The
// question has one right answer, and the ladder that reaches it is
// intricate: a hibernation transition revokes eligibility outright, a live
// controller's non-empty generation outranks a rotated session field, and a
// workspace with no controller is judged against the SSM snapshot instead.
//
// Written twice, those are two places for that answer to drift — and drift
// here is not cosmetic. Each rung decides whether a client is served somebody
// else's conversation, so a ladder that admitted a page where it refused a
// resync would be a silent cross-generation read on one surface only.
//
// # The lock discipline is part of the contract
//
// The caller holds m.mu on entry and MUST `defer release()`. The two routes
// need the lock for different durations and the release closure is what makes
// that difference unrepresentable at the call site:
//
//   - The LIVE route releases immediately: the controller it hands back is
//     pinned by its own lifetime, and holding the manager lock across a shim
//     round-trip would serialize every workspace behind one read.
//   - The DURABLE route KEEPS the lock through the caller's read. bringUp
//     installs the next controller under this same mutex, so holding it is
//     what makes "no controller exists" still true when the read begins. A
//     durable reader must therefore never re-enter m.mu.
//
// # Every refusal is loud, and says which rung refused it
//
// The `decision=` token on each record is what makes a refused view
// diagnosable from the log alone, and it is deliberately the same vocabulary
// for both request kinds so one grep finds every refusal of either.

// historyRoute names where an admitted history request is served from.
type historyRoute int

const (
	// historyRouteLiveController serves through the workspace's live shim.
	historyRouteLiveController historyRoute = iota
	// historyRouteDurableHistory serves an UNWIRED workspace from the store.
	historyRouteDurableHistory
)

// historyAdmission is the ladder's verdict for an admitted request.
type historyAdmission struct {
	route historyRoute
	// controller is the live session controller, set only on the live route.
	controller *sessionController
	// sessionID and generationID are the LIVE identity the request was
	// admitted against — never the client's echo, which may name a session
	// that has since rotated under a still-current generation.
	sessionID    string
	generationID string
	// fence is the workspace's live fence AS THE LADDER READ IT — the live
	// controller's composed token on one route, the published WorkspaceState's
	// token on the other. Anything this request answers with stamps THIS,
	// rather than recomposing a fence from the pair above: an ungenerated
	// workspace publishes an ABSENT fence, and `Fence(session, "")` is a
	// different token that no client was ever shown and none can match.
	fence string
}

// admitHistoryRequest rules on whether a frontend history request may be
// served, and by which route.
//
// kind names the request for the log ("resync", "conversation page"), so the
// two surfaces are distinguishable in a record without being judged by
// different rules.
//
// echoedFence is the token the client copied out of the WorkspaceState it was
// reading when it decided to ask. It arrives WHOLE rather than pre-split,
// because splitting it is what this ladder must not do in the general case —
// see the header.
//
// detail carries the REQUEST-SHAPED context only the caller holds — a resync's
// `from_seq`, a page's anchor and limit — onto every record this ladder
// writes. It is a parameter rather than something reconstructed here because
// the ladder deliberately knows nothing about what is being asked for, and a
// refusal record that could not name the request would be diagnosable only as
// far as the workspace.
//
// The caller MUST hold m.mu and MUST defer the returned release. On refusal
// the release is still returned and still must be deferred.
func (m *Manager) admitHistoryRequest(kind, detail, workspace, echoedFence string) (historyAdmission, func(), error) {
	unlockOnce := false
	unlock := func() {
		if unlockOnce {
			return
		}
		unlockOnce = true
		m.mu.Unlock()
	}

	d, live := m.byWS[workspace]
	// HIBERNATION REVOKES ELIGIBILITY OUTRIGHT, ahead of every other rung. A
	// workspace mid-hibernation is one whose controller is being torn down on
	// purpose, so serving history against it would replay a generation that is
	// deliberately ending.
	if m.hibernating[workspace] {
		liveFence := ""
		if live {
			liveFence = ssm.Fence(d.sessionID, d.generationID)
		}
		unlock()
		return historyAdmission{}, unlock, m.rejectHistoryRequest(kind, detail, workspace, echoedFence,
			liveFence, "hibernation_transition", "eligibility_revoked")
	}


	if live {
		liveSessionID, liveGenerationID := d.sessionID, d.generationID
		// The live controller's own fence, composed exactly as the consumer
		// composes the one it stamps on every delta it pushes (verdict.go), so
		// what this compares against is the token the client was actually
		// shown. A live controller always has a non-empty generation, so the
		// absent-fence rule the mint applies to an ungenerated workspace
		// cannot reach this branch.
		liveFence := ssm.Fence(liveSessionID, liveGenerationID)
		unlock()
		decision, ok := admitAgainstFence(echoedFence, liveFence, liveGenerationID)
		if !ok {
			return historyAdmission{}, unlock, m.rejectHistoryRequest(kind, detail, workspace, echoedFence,
				liveFence, "live_controller", "identity_mismatch")
		}
		m.logf("session-controller: %s eligibility ACCEPTED ws=%q request_fence=%q live_fence=%q replay_source=%q %s decision=%s",
			kind, workspace, echoedFence, liveFence, "live_controller", detail, decision)
		return historyAdmission{
			route:        historyRouteLiveController,
			controller:   d,
			sessionID:    liveSessionID,
			generationID: liveGenerationID,
			fence:        liveFence,
		}, unlock, nil
	}

	// NO CONTROLLER. The workspace is judged against the SSM snapshot instead,
	// and the lock is KEPT so no controller can appear between this verdict and
	// the caller's read.
	reader, ok := m.cfg.SSM.(WorkspaceStateReader)
	if !ok {
		err := fmt.Errorf("session-controller: %s ws=%q has no workspace-state reader for durable-history identity validation", kind, workspace)
		m.logf("session-controller: %s eligibility REJECTED ws=%q request_fence=%q replay_source=%q %s decision=missing_workspace_state_reader error=%v",
			kind, workspace, echoedFence, "durable_history", detail, err)
		return historyAdmission{}, unlock, err
	}
	state, found, err := reader.Current(workspace)
	if err != nil {
		m.logf("session-controller: %s eligibility FAILED ws=%q request_fence=%q replay_source=%q %s decision=workspace_state_read_failed error=%v",
			kind, workspace, echoedFence, "durable_history", detail, err)
		return historyAdmission{}, unlock, fmt.Errorf("session-controller: read authoritative workspace state for durable %s ws %q: %w", kind, workspace, err)
	}
	if !found || state == nil {
		err := fmt.Errorf("session-controller: no authoritative workspace state for durable %s ws %q", kind, workspace)
		m.logf("session-controller: %s eligibility REJECTED ws=%q request_fence=%q replay_source=%q %s decision=missing_workspace_state error=%v",
			kind, workspace, echoedFence, "durable_history", detail, err)
		return historyAdmission{}, unlock, err
	}
	liveSessionID, liveGenerationID := state.GetSessionId(), state.GetControllerGenerationId()
	// THE PUBLISHED TOKEN, read straight off the authoritative WorkspaceState
	// rather than recomposed from the two identities beside it. Recomposition
	// is what made this rung wrong: an ungenerated workspace publishes an
	// ABSENT fence, and `Fence(session, "")` is not that — it is a different
	// token entirely, which no client was ever shown.
	liveFence := state.GetFence()
	// The rebind rung is deliberately NOT offered here. It exists because a
	// live controller's generation identifies that controller; a workspace with
	// no controller has no such guarantee to stand on, so the durable route
	// admits an exact match and nothing else.
	//
	// The identity-less wildcard above applies here too, and on this route it is
	// the ordinary case rather than the exception: an unwired workspace with no
	// controller generation publishes an ABSENT fence, so the client echoing it
	// and the state publishing it agree at "" and the exact comparison would
	// have admitted it anyway. Naming the wildcard explicitly is what keeps the
	// two routes' verdicts written the same way.
	decision := "current_durable_snapshot"
	if echoedFence == "" {
		decision = "identityless_request_wildcard"
	} else if echoedFence != liveFence {
		return historyAdmission{}, unlock, m.rejectHistoryRequest(kind, detail, workspace, echoedFence,
			liveFence, "durable_history", "identity_mismatch")
	}
	m.logf("session-controller: %s eligibility ACCEPTED ws=%q request_fence=%q live_fence=%q replay_source=%q %s decision=%s",
		kind, workspace, echoedFence, liveFence, "durable_history", detail, decision)
	return historyAdmission{
		route:        historyRouteDurableHistory,
		sessionID:    liveSessionID,
		generationID: liveGenerationID,
		fence:        liveFence,
	}, unlock, nil
}

// rejectHistoryRequest refuses a request whose echoed identity is not the live
// one, in the one vocabulary both surfaces share.
func (m *Manager) rejectHistoryRequest(kind, detail, workspace, requestFence, liveFence, replaySource, rejectionCause string) error {
	err := fmt.Errorf("%w: %s ws=%q request_fence=%q live_fence=%q %s replay_source=%q rejection_cause=%q",
		errclass.ErrSessionSuperseded, kind, workspace, requestFence, liveFence, detail, replaySource, rejectionCause)
	m.logf("session-controller: %s eligibility REJECTED ws=%q request_fence=%q live_fence=%q %s replay_source=%q decision=superseded rejection_cause=%q error=%v",
		kind, workspace, requestFence, liveFence, detail, replaySource, rejectionCause, err)
	return err
}

// admitAgainstFence is the comparison itself: byte-first, with the one rung
// that cannot be expressed byte-wise behind it.
//
// It returns the decision token for the log alongside the verdict, so the
// record always says WHICH rung admitted a request rather than only that one
// did.
//
// liveGenerationID is the LIVE controller's generation, and it is what gates
// the fallback: an empty one identifies nothing, so a client whose token
// merely happens to end in the same empty suffix has established nothing and
// is refused.
func admitAgainstFence(echoedFence, liveFence, liveGenerationID string) (decision string, ok bool) {
	// AN ABSENT ECHO IS A WILDCARD, not a mismatch. A client sending no fence
	// at all holds no claim about which generation it is reading — it predates
	// fenced chrome, or it connected in a window where the authoritative
	// WorkspaceState honestly published an ABSENT fence (ssm/connectivity.go).
	// It has nothing to be stale about, so there is nothing to compare and
	// nothing to refuse; it is served under whatever identity is current.
	//
	// THIS IS NOT THE ADOPTED-EMPTY-GENERATION CASE. A client echoing a
	// non-empty token that merely ends in an empty generation DID adopt a
	// fence, and it is compared and refused exactly as any other.
	if echoedFence == "" {
		return "identityless_request_wildcard", true
	}
	// THE CONTRACT'S OWN COMPARISON, and the answer in every ordinary case.
	if echoedFence == liveFence {
		return "current_fence", true
	}
	// THE ONE RUNG THE BYTES CANNOT EXPRESS. A non-empty controller generation
	// uniquely identifies THIS live controller, so a client carrying it is
	// current on the pushed plane and only its session half is stale — the
	// exact shape a webview ends up in when a session id rotates underneath a
	// store that already took the new generation. Refusing it deadlocks the
	// view: a replay is a view's only recovery mechanism, so a refused one is a
	// permanent stale banner.
	if liveGenerationID == "" {
		return "", false
	}
	if _, echoedGenerationID := ssm.SplitFence(echoedFence); echoedGenerationID == liveGenerationID {
		return "current_generation_session_rebound", true
	}
	return "", false
}
