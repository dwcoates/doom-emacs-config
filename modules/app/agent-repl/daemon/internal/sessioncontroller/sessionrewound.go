package sessioncontroller

import (
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/ssm"
)

// sessionrewound.go — CONSUMING THE SHIM'S REWIND RECORD.
//
// The daemon ORCHESTRATES the rewind (rewind.go) and the shim ANNOUNCES it: on
// the spawn that follows, the shim reads the lineage argv and emits one
// SessionRewound into the new seq space. That event is correlation evidence in
// TurnClaimBridge's sense — the identity rotation itself is announced by the
// ordinary handshake bounce, and this is the durable explanation of WHY the
// identity changed, so a vendor-session lineage can be reconstructed from the
// store alone without replaying daemon logs.
//
// IT ALSO DOES ONE THING THAT MATTERS TO LIVE STATE. The turns the rewind
// dropped had claims in the durable turn ledger, and those claims live in the
// seq space the rewind just retired. Nothing in the new space will ever deliver
// their TurnEnded, so an untouched claim would stay active forever and the
// workspace would read as THINKING with no turn behind it. The claims are
// closed here, attributed to the rewind.

// TurnClaimSuperseder closes the claims of turns a rewind discarded, and
// attributes the ones their own ends already closed. Satisfied by *ssm.Manager.
type TurnClaimSuperseder interface {
	SupersedeTurnClaims(workspace, claimantSessionID string, turnIDs []string, cause string) (closed, attributed []string, err error)
}

// ApplySessionRewound records one rewind's lineage and supersedes the claims of
// the turns it dropped.
//
// THE EVENT IS VALIDATED BEFORE IT IS ACTED ON. A rewind naming no predecessor,
// or naming itself as its own predecessor, describes something that cannot have
// happened; acting on it would close claims against a lineage that is not real.
func (c *consumer) ApplySessionRewound(ev *corev1.Event, rewound *corev1.SessionRewound) error {
	if rewound == nil {
		return fmt.Errorf("session-controller: SessionRewound session=%s seq=%d carries no payload", c.sessionID, ev.GetSeq())
	}
	previous, next := rewound.GetPreviousVendorSessionId(), rewound.GetNewVendorSessionId()
	if previous == "" || next == "" {
		return fmt.Errorf("session-controller: SessionRewound session=%s seq=%d names an incomplete lineage previous=%q new=%q",
			c.sessionID, ev.GetSeq(), previous, next)
	}
	if previous == next {
		return fmt.Errorf("session-controller: SessionRewound session=%s seq=%d names %q as its own predecessor; a rewind always produces a NEW vendor session",
			c.sessionID, ev.GetSeq(), previous)
	}
	dropped := rewound.GetKeepAliveDiscard().GetDroppedTurnIds()
	c.logf("session-controller: SESSION REWOUND session=%s ws=%q seq=%d previous_vendor_session=%s new_vendor_session=%s retained_leaf=%s dropped_turns=%d — the truncated copy is byte-identical to the prefix the vendor already had, so the next prompt lands on the cache the dropped turns kept warm",
		c.sessionID, c.workspace, ev.GetSeq(), previous, next, rewound.GetRetainedLeafUuid(), len(dropped))

	if len(dropped) == 0 {
		// The shim rejects an empty dropped list, so reaching here means a
		// producer that got past its own validation. Loud rather than ignored:
		// a rewind that discarded nothing is not a rewind.
		return fmt.Errorf("session-controller: SessionRewound session=%s seq=%d previous_vendor_session=%s dropped no turns; a rewind that discards nothing has no reason to exist",
			c.sessionID, ev.GetSeq(), previous)
	}
	if c.turnSuperseders == nil {
		return fmt.Errorf("session-controller: SessionRewound session=%s seq=%d has no turn-claim superseder wired, so %d discarded turn(s) would keep their claims open and the workspace would read as thinking with no turn behind it",
			c.sessionID, ev.GetSeq(), len(dropped))
	}
	closed, attributed, err := c.turnSuperseders.SupersedeTurnClaims(c.workspace, c.sessionID, dropped, ssm.TurnCloseCauseSupersededByRewind)
	if err != nil {
		return fmt.Errorf("session-controller: SessionRewound session=%s seq=%d: superseding %d discarded turn claim(s): %w",
			c.sessionID, ev.GetSeq(), len(dropped), err)
	}
	c.logf("session-controller: rewind SUPERSEDED %d of %d discarded turn claim(s) session=%s ws=%q closed_still_open=%d attributed_already_closed=%d cause=%s — attributing already-closed claims is the ORDINARY outcome, because a keep-alive ping ends normally before the rewind that discards it runs; a claim closed here is the exception, a turn the rewind caught still open",
		len(closed)+len(attributed), len(dropped), c.sessionID, c.workspace, len(closed), len(attributed), ssm.TurnCloseCauseSupersededByRewind)
	return nil
}
