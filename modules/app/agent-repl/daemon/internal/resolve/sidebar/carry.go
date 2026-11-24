package sidebar

import (
	"agentrepl/logging/logsite"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// A JOINING DAEMON PUBLISHES NO ARM IT HAS NOT THE FACTS FOR (2026-11-24).
//
// While a successor joins, a row it has not adopted draws the incumbent's own
// arm (the roster mirror, SetIncumbentArms). The mirror lets go of a row the
// moment its adoption reaches ownership, but the facts the row's arm is
// derived from reach this daemon only through the rest of the adoption: its
// merges are taken back, its cold gate raised, its session started. Drawn
// from what this daemon held in between, rows in a merge queue read `done`
// (unviewed, so shown under their fold, so opened as tabs) and cold-gated rows
// read `init`, then went back seconds later: 84 status changes and 16
// visibility changes at the 2026-11-24 06:20 handover, nothing about any of
// the workspaces having changed.
//
// So a row the mirror lets go of keeps the arm it last drew, the incumbent's,
// as its CARRIED arm, and draws it (for its status and its visibility alike)
// until its facts are settled: the adoption declared them in (SettleFacts,
// or SettleAllFacts once the takeover settled) AND no bring-up of the row
// stands. A row this daemon never mirrored carries nothing.

// heldArm answers the arm WS draws from the incumbent rather than from its own
// facts, and where it was read: the mirror's, for a row not yet adopted, or
// the carried one, for a row whose facts are not settled. Caller holds mu.
func (r *resolver) heldArm(ws ids.WorkspaceID, s *wsState) (arm, source string, held bool) {
	if arm, mirrored := r.state.incumbentArms[ws]; mirrored {
		return arm, "incumbent", true
	}
	if s.carried != "" {
		return s.carried, "carried", true
	}
	return "", "", false
}

// carryLetGo carries, for every row BEFORE mirrored and AFTER does not, the
// arm BEFORE drew, unless the row's facts are already declared in. Caller
// holds mu.
func (r *resolver) carryLetGo(before, after map[ids.WorkspaceID]string) {
	for ws, arm := range before {
		if _, still := after[ws]; still {
			continue
		}
		s := r.state.workspace(ws)
		if s.settleAsked {
			continue
		}
		s.carried = arm
		r.state.touch(ws)
		r.logFor(ws).Debug("daemon.sidebar.arm_carried",
			"the mirror let go of the row before its facts were settled; it keeps the incumbent's arm",
			dlog.Context{"arm": arm})
	}
}

// releaseCarry lets go of WS's carried arm once its facts are settled. Caller
// holds mu.
func (r *resolver) releaseCarry(ws ids.WorkspaceID, s *wsState) {
	if s.carried == "" || !s.settleAsked || s.bringingUp {
		return
	}
	r.logFor(ws).Info("daemon.sidebar.arm_carry_released",
		"the row's facts are settled; it is drawn from this daemon's own facts from here", dlog.Context{"carried": s.carried})
	s.carried = ""
	r.state.touch(ws)
}

// SettleFacts implements Resolver.
func (r *resolver) SettleFacts(ws ids.WorkspaceID) {
	r.mutateWorkspaceIf(ws, "daemon.sidebar.settle_facts",
		logsite.Msg("the adoption declared the row's facts in"), nil, func(s *wsState, _ dlog.Logger) bool {
			if s.settleAsked {
				return false
			}
			s.settleAsked = true
			r.releaseCarry(ws, s)
			return true
		})
}

// SettleAllFacts implements Resolver.
func (r *resolver) SettleAllFacts() {
	r.mutateIf("daemon.sidebar.settle_all_facts",
		logsite.Msg("the takeover settled; every row's facts are declared in"), nil, r.log.Global(), func() bool {
			changed := false
			for _, rec := range r.state.reg.Workspaces {
				s := r.state.workspace(rec.ID)
				if s.settleAsked {
					continue
				}
				s.settleAsked = true
				r.releaseCarry(rec.ID, s)
				changed = true
			}
			return changed
		})
}
