package sessioncontroller

import (
	"fmt"

	"claude-repld/internal/errclass"
)

// THE ONE ELIGIBILITY LADDER every frontend history request climbs.
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
}

// admitHistoryRequest rules on whether a frontend history request may be
// served, and by which route.
//
// kind names the request for the log ("resync", "conversation page"), so the
// two surfaces are distinguishable in a record without being judged by
// different rules.
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
func (m *Manager) admitHistoryRequest(kind, detail, workspace, expectedSessionID, expectedGenerationID string) (historyAdmission, func(), error) {
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
		liveSessionID, liveGenerationID := "", ""
		if live {
			liveSessionID, liveGenerationID = d.sessionID, d.generationID
		}
		unlock()
		return historyAdmission{}, unlock, m.rejectHistoryRequest(kind, detail, workspace, expectedSessionID, expectedGenerationID,
			liveSessionID, liveGenerationID, "hibernation_transition", "eligibility_revoked")
	}

	// AN IDENTITY-LESS REQUEST IS A WILDCARD, not a mismatch. A request carrying
	// NEITHER a session nor a generation is a client that holds no fence at all
	// — it predates fenced chrome, or it connected in a window where the
	// authoritative WorkspaceState honestly published an ABSENT fence (see
	// ssm/connectivity.go: an absent controller generation yields an absent
	// fence rather than an unmatchable minted one). Such a client has nothing to
	// be stale about, so there is no identity to compare and nothing to refuse;
	// it is served under whatever identity is current.
	//
	// THIS IS NOT THE ADOPTED-EMPTY-GENERATION CASE. A request carrying a
	// session with an empty generation DID adopt a fence, and it is compared and
	// refused exactly as before — the wildcard is only for a request that
	// carries no identity whatsoever.
	identityless := expectedSessionID == "" && expectedGenerationID == ""

	if live {
		liveSessionID, liveGenerationID := d.sessionID, d.generationID
		unlock()
		if identityless {
			m.logf("session-controller: %s eligibility ACCEPTED ws=%q request_session=%q request_generation=%q live_session=%q live_generation=%q replay_source=%q %s decision=identityless_request_wildcard",
				kind, workspace, expectedSessionID, expectedGenerationID, liveSessionID, liveGenerationID, "live_controller", detail)
			return historyAdmission{
				route:        historyRouteLiveController,
				controller:   d,
				sessionID:    liveSessionID,
				generationID: liveGenerationID,
			}, unlock, nil
		}
		if expectedGenerationID != liveGenerationID {
			return historyAdmission{}, unlock, m.rejectHistoryRequest(kind, detail, workspace, expectedSessionID, expectedGenerationID,
				liveSessionID, liveGenerationID, "live_controller", "identity_mismatch")
		}
		decision := "current_live_controller"
		if expectedSessionID != liveSessionID {
			// A NON-EMPTY controller generation uniquely identifies THIS live
			// controller, so a client carrying it is current on the pushed
			// plane and only its session field is stale — the exact shape a
			// webview ends up in when a session id rotates underneath a store
			// that already took the new generation. Refusing it deadlocked the
			// view: a replay is a view's only recovery mechanism, so a refused
			// one is a permanent stale banner.
			if expectedGenerationID == "" {
				return historyAdmission{}, unlock, m.rejectHistoryRequest(kind, detail, workspace, expectedSessionID, expectedGenerationID,
					liveSessionID, liveGenerationID, "live_controller", "identity_mismatch")
			}
			decision = "current_generation_session_rebound"
		}
		m.logf("session-controller: %s eligibility ACCEPTED ws=%q request_session=%q request_generation=%q live_session=%q live_generation=%q replay_source=%q %s decision=%s",
			kind, workspace, expectedSessionID, expectedGenerationID, liveSessionID, liveGenerationID, "live_controller", detail, decision)
		return historyAdmission{
			route:        historyRouteLiveController,
			controller:   d,
			sessionID:    liveSessionID,
			generationID: liveGenerationID,
		}, unlock, nil
	}

	// NO CONTROLLER. The workspace is judged against the SSM snapshot instead,
	// and the lock is KEPT so no controller can appear between this verdict and
	// the caller's read.
	reader, ok := m.cfg.SSM.(WorkspaceStateReader)
	if !ok {
		err := fmt.Errorf("session-controller: %s ws=%q has no workspace-state reader for durable-history identity validation", kind, workspace)
		m.logf("session-controller: %s eligibility REJECTED ws=%q request_session=%q request_generation=%q replay_source=%q %s decision=missing_workspace_state_reader error=%v",
			kind, workspace, expectedSessionID, expectedGenerationID, "durable_history", detail, err)
		return historyAdmission{}, unlock, err
	}
	state, found, err := reader.Current(workspace)
	if err != nil {
		m.logf("session-controller: %s eligibility FAILED ws=%q request_session=%q request_generation=%q replay_source=%q %s decision=workspace_state_read_failed error=%v",
			kind, workspace, expectedSessionID, expectedGenerationID, "durable_history", detail, err)
		return historyAdmission{}, unlock, fmt.Errorf("session-controller: read authoritative workspace state for durable %s ws %q: %w", kind, workspace, err)
	}
	if !found || state == nil {
		err := fmt.Errorf("session-controller: no authoritative workspace state for durable %s ws %q", kind, workspace)
		m.logf("session-controller: %s eligibility REJECTED ws=%q request_session=%q request_generation=%q replay_source=%q %s decision=missing_workspace_state error=%v",
			kind, workspace, expectedSessionID, expectedGenerationID, "durable_history", detail, err)
		return historyAdmission{}, unlock, err
	}
	liveSessionID, liveGenerationID := state.GetSessionId(), state.GetControllerGenerationId()
	if !identityless && (expectedSessionID != liveSessionID || expectedGenerationID != liveGenerationID) {
		return historyAdmission{}, unlock, m.rejectHistoryRequest(kind, detail, workspace, expectedSessionID, expectedGenerationID,
			liveSessionID, liveGenerationID, "durable_history", "identity_mismatch")
	}
	decision := "current_durable_snapshot"
	if identityless {
		decision = "identityless_request_wildcard"
	}
	m.logf("session-controller: %s eligibility ACCEPTED ws=%q request_session=%q request_generation=%q live_session=%q live_generation=%q replay_source=%q %s decision=%s",
		kind, workspace, expectedSessionID, expectedGenerationID, liveSessionID, liveGenerationID, "durable_history", detail, decision)
	return historyAdmission{
		route:        historyRouteDurableHistory,
		sessionID:    liveSessionID,
		generationID: liveGenerationID,
	}, unlock, nil
}

// rejectHistoryRequest refuses a request whose echoed identity is not the live
// one, in the one vocabulary both surfaces share.
func (m *Manager) rejectHistoryRequest(kind, detail, workspace, requestSessionID, requestGenerationID, liveSessionID, liveGenerationID, replaySource, rejectionCause string) error {
	err := fmt.Errorf("%w: %s ws=%q request_session=%q request_generation=%q live_session=%q live_generation=%q %s replay_source=%q rejection_cause=%q",
		errclass.ErrSessionSuperseded, kind, workspace, requestSessionID, requestGenerationID, liveSessionID, liveGenerationID, detail, replaySource, rejectionCause)
	m.logf("session-controller: %s eligibility REJECTED ws=%q request_session=%q request_generation=%q live_session=%q live_generation=%q %s replay_source=%q decision=superseded rejection_cause=%q error=%v",
		kind, workspace, requestSessionID, requestGenerationID, liveSessionID, liveGenerationID, detail, replaySource, rejectionCause, err)
	return err
}
