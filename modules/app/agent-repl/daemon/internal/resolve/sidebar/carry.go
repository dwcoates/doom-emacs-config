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
// until ITS OWN facts are settled AND no bring-up of the row stands. A row
// this daemon never mirrored carries nothing.
//
// EACH ROW IS SETTLED BY THE EVENT THAT ESTABLISHES ITS FACTS, never by the
// takeover wholesale (2026-11-25). The takeover used to settle every row the
// moment its adoptions finished, but a row nobody handed over is decided only
// by the takeover's orphan pass, which waits for the older daemons to end: at
// the 12:32:53 handover three folded rows at a cold gate were settled at
// 12:32:56, drawn from a durable turn end as `done`, unviewed, so shown under
// their fold and opened as tabs, until the orphan pass raised their gates
// seven seconds later. The rollout settles a row (SettleFacts) once its
// adoption has taken its merges and its session's facts, once its start has
// ended, or once the orphan pass decided nothing will be started for it.
//
// A START OF A CARRIED ROW'S SESSION THAT THIS DAEMON RAN SETTLES IT too
// when it ends, unless an adoption owes the row's facts (OweFacts): the
// session's facts are then this daemon's own, so a row the user answers or
// prompts before the orphan pass reaches it is drawn as it runs. An adoption
// owes the facts from its start to its settle, because a start it drains
// can end before it has taken the row's merges back.

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

// startEnded settles WS when a start of its session that this daemon ran has
// ended while the row is carried and no adoption owes its facts (the
// bring-up marker's falling edge). Caller holds mu.
func (r *resolver) startEnded(ws ids.WorkspaceID, s *wsState) {
	if s.carried == "" || s.settleAsked || s.adoptionOwes {
		return
	}
	r.logFor(ws).Info("daemon.sidebar.facts_settled_by_start",
		"a start of the carried row's session that this daemon ran has ended; its session's facts are this daemon's own",
		dlog.Context{"carried": s.carried})
	s.settleAsked = true
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
		logsite.Msg("the row's facts are established"), nil, func(s *wsState, _ dlog.Logger) bool {
			if s.settleAsked && !s.adoptionOwes {
				return false
			}
			s.settleAsked = true
			s.adoptionOwes = false
			r.releaseCarry(ws, s)
			return true
		})
}

// OweFacts implements Resolver.
func (r *resolver) OweFacts(ws ids.WorkspaceID, owed bool) {
	r.mutateWorkspaceIf(ws, "daemon.sidebar.owe_facts",
		logsite.Msg("the roster took whether an adoption owes the row's facts"), dlog.Context{"owed": owed},
		func(s *wsState, _ dlog.Logger) bool {
			if s.adoptionOwes == owed {
				return false
			}
			s.adoptionOwes = owed
			return true
		})
}
