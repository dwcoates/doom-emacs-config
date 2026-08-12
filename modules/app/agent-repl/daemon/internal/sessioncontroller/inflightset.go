package sessioncontroller

import (
	"fmt"

	"claude-repld/internal/inflight"
)

// inflightset.go — WHERE THE IN-FLIGHT SET IS RESOLVED, once, for everyone.
//
// # The rule this file enforces
//
// The daemon resolves what "this workspace has work in flight" means and every
// consumer adopts the verdict. This codebase already follows that discipline
// for everything a frontend renders (figma→idl: the daemon resolves, the client
// renders verbatim); a bounce decision is the same shape of question and had
// five derivations instead of one.
//
// # The three planes, and why each is a member
//
//   - TURNS — the durable claim ledger's open claims. Already consulted by the
//     stale-shim roll and the hibernation guard, from two different reads.
//   - TASKS — the live background-task set, BY IDENTITY (ssm.LiveTaskIDs), the
//     same population ShimHello.live_task_set answers in and the same rows
//     live_task_count is folded from. This is the member every turn-shaped gate
//     missed: a shim running only detached work has no turn and looked idle at
//     exactly the instant the daemon decided whether to kill it.
//   - THE SDK QUERY INSTANCE — ShimHello.query_instance_id, bound by the
//     accounting reducer. It is the thing that actually dies when a query
//     terminates unexpectedly, and nothing tracked it as in-flight STATE.
//
// # THE QUERY IS THE VEHICLE, NOT THE CARGO — a deliberate narrowing
//
// A shim holds ONE query() for its whole life, idle or busy. Admitting the
// query as a member unconditionally would make every wired workspace
// permanently non-empty, which would not "protect work" — it would disable
// hibernation, the idle sweep and every close for the whole fleet, and a gate
// that always refuses is a gate nobody keeps.
//
// So the query joins the set exactly when it is CARRYING work: a turn or a task
// is live. That is enough to do the job it was named for — a bounce's manifest
// records the query's identity beside the work it was carrying, so an
// interrupted query is reported under the uuid the termination card names —
// without turning an idle session into a workspace nobody may ever reclaim.
//
// # UNKNOWN IS EVERYWHERE THE EVIDENCE STOPS
//
// Four separate conditions produce an UNKNOWN rather than a smaller set: an
// unreadable state, a workspace with no resolved state at all, a turn that is
// in flight under no resolvable identity, and a live background task whose
// start carried no identity. Each understates what is running if folded into an
// answer, and understating is the direction that kills somebody's work.

// InFlight is THE daemon's answer for one workspace.
//
// It never returns an error. Every failure it could have returned is instead an
// UNANSWERED set carrying that failure as its reason, because a caller that
// received (Set{}, err) would have to decide for itself what an error means —
// and "an error means not busy" is precisely the reading this package exists to
// make unavailable.
func (m *Manager) InFlight(workspace string) inflight.Set {
	if workspace == "" {
		return inflight.Unanswered(workspace, "the in-flight set was asked for with no workspace, so no evidence could be gathered")
	}
	st, found, err := m.cfg.SSM.Current(workspace)
	if err != nil {
		set := inflight.Unanswered(workspace, fmt.Sprintf("the resolved workspace state could not be read: %v", err))
		m.logf("session-controller: in-flight set UNKNOWN ws=%q — %s", workspace, set.Reason())
		return set
	}
	if !found {
		// AN UNKNOWN WORKSPACE IS NOT A QUIET ONE. This is the same ruling
		// Server.sweepable already made for its own read ("reaping on absent
		// evidence is precisely how a bring-up in flight got hibernated before
		// its first event landed"), lifted here so every consumer inherits it.
		set := inflight.Unanswered(workspace, "the SSM holds no resolved state for this workspace, so what it is running is unobserved rather than nothing")
		m.logf("session-controller: in-flight set UNKNOWN ws=%q — %s", workspace, set.Reason())
		return set
	}

	turns := m.inFlightTurns(workspace, st.GetSessionId(), st.GetTurnActive())
	tasks := m.inFlightTasks(workspace)
	set := inflight.Union(workspace, turns, tasks)
	set = m.withCarryingQuery(workspace, set)

	blocked, why := set.Blocks()
	m.logf("session-controller: in-flight set ws=%q session=%s state=%s blocked=%v — %s",
		workspace, st.GetSessionId(), st.GetState(), blocked, why)
	return set
}

// LiveTaskSet is the TASK plane of InFlight, on its own, for the one consumer
// that must not read the other two: the scheduled-shutdown drain.
//
// WHY THE PLANE IS TAKEN ALONE. The drain already reports its turn separately —
// TurnActive and TurnID exist precisely so a turn this daemon ADOPTED, running
// under no id this process ever saw, still holds — and folding the turn plane
// in here would make an adopted turn poison the task answer into UNKNOWN, which
// would say nothing about background work at all. The query plane is likewise
// excluded: it is the vehicle the work runs inside, not work of its own.
//
// THE MISS SURVIVES, in the same shape InFlight gives it. A workspace the SSM
// holds no resolved state for is UNANSWERED, never an empty set: "runs no
// background task" and "nobody ever told this resolver about this workspace"
// are different facts, and a consumer that could not tell them apart would read
// silence as quiet.
func (m *Manager) LiveTaskSet(workspace string) inflight.Set {
	if workspace == "" {
		return inflight.Unanswered(workspace, "the live background-task set was asked for with no workspace, so no evidence could be gathered")
	}
	_, found, err := m.cfg.SSM.Current(workspace)
	if err != nil {
		set := inflight.Unanswered(workspace, fmt.Sprintf("the resolved workspace state could not be read: %v", err))
		m.logf("session-controller: live-task set UNKNOWN ws=%q — %s", workspace, set.Reason())
		return set
	}
	if !found {
		set := inflight.Unanswered(workspace, "the SSM holds no resolved state for this workspace, so what it is running in the background is unobserved rather than nothing")
		m.logf("session-controller: live-task set UNKNOWN ws=%q — %s", workspace, set.Reason())
		return set
	}
	return m.inFlightTasks(workspace)
}

// inFlightTurns names the workspace's open turn claims.
//
// A TURN IN FLIGHT UNDER NO RESOLVABLE IDENTITY IS UNKNOWN, not a member with a
// made-up id and not an absence. The drain hold already documents how this
// arises: a turn this daemon ADOPTED rather than started — a shim that outlived
// the previous daemon and reattached mid-turn — is unambiguously running and
// this process never saw an id for it. Naming it would be a fabricated
// identity that could not be matched after a bounce; omitting it would report a
// running turn as no turn at all, "which is the one reading that lets a bounce
// cut live work".
func (m *Manager) inFlightTurns(workspace, sessionID string, turnActive bool) inflight.Set {
	if sessionID == "" {
		if !turnActive {
			return inflight.MustAnswered(workspace)
		}
		return inflight.Unanswered(workspace, "a turn is in flight and the workspace's resolved state names no owning session, so the turn cannot be identified")
	}
	ids, err := m.cfg.SSM.ActiveTurnIDs(workspace, sessionID)
	if err != nil {
		return inflight.Unanswered(workspace, fmt.Sprintf("the durable turn claims could not be read: %v", err))
	}
	if len(ids) == 0 && turnActive {
		return inflight.Unanswered(workspace, fmt.Sprintf("the workspace reads turn_active under session %q and the claim ledger names no open turn for it, so a running turn exists that cannot be identified", sessionID))
	}
	items := make([]inflight.Item, 0, len(ids))
	for _, id := range ids {
		items = append(items, inflight.Item{Kind: inflight.KindTurn, ID: id})
	}
	set, err := inflight.Answered(workspace, items...)
	if err != nil {
		return inflight.Unanswered(workspace, fmt.Sprintf("an open turn claim carried no usable identity: %v", err))
	}
	return set
}

// inFlightTasks names the workspace's live background tasks.
//
// AN ANONYMOUS LIVE START POISONS THE ANSWER. ssm.LiveTaskIDs reports those
// separately precisely so this decision can be made here rather than by
// silently returning a shorter list: work is running that cannot be named, so
// the honest answer about the SET is that it is not known.
func (m *Manager) inFlightTasks(workspace string) inflight.Set {
	ids, anonymous, err := m.cfg.SSM.LiveTaskIDs(workspace)
	if err != nil {
		return inflight.Unanswered(workspace, fmt.Sprintf("the live background-task set could not be read: %v", err))
	}
	if anonymous > 0 {
		return inflight.Unanswered(workspace, fmt.Sprintf("%d live background task(s) started without an identity, so the set of what is running cannot be completed", anonymous))
	}
	items := make([]inflight.Item, 0, len(ids))
	for _, id := range ids {
		items = append(items, inflight.Item{Kind: inflight.KindTask, ID: id})
	}
	set, err := inflight.Answered(workspace, items...)
	if err != nil {
		return inflight.Unanswered(workspace, fmt.Sprintf("a live background task carried no usable identity: %v", err))
	}
	return set
}

// withCarryingQuery adds the SDK query instance to a set that already holds
// work, so the query the work is running INSIDE is named by the same authority
// the work is.
//
// It adds nothing to an empty or unknown set: see the file comment on why the
// query is the vehicle rather than the cargo. A workspace holding work whose
// query identity is unresolvable is left as it is rather than made unknown —
// the work itself is already named, already blocks every gate, and the missing
// query id costs the manifest one identity rather than costing the decision its
// correctness. It is loud-logged so the manifest's gap has an account.
func (m *Manager) withCarryingQuery(workspace string, set inflight.Set) inflight.Set {
	if !set.Known() || len(set.Items()) == 0 {
		return set
	}
	queryID := m.boundQueryInstanceID(workspace)
	if queryID == "" {
		m.logf("session-controller: in-flight set carries no query identity ws=%q — this workspace holds work and no live SDK query instance could be named for it, so a bounce manifest cannot report which query was carrying it",
			workspace)
		return set
	}
	joined, err := inflight.Answered(workspace, append(set.Items(), inflight.Item{
		Kind:   inflight.KindQuery,
		ID:     queryID,
		Detail: "the SDK query instance the live work is running inside",
	})...)
	if err != nil {
		return inflight.Unanswered(workspace, fmt.Sprintf("the live SDK query instance could not join the set: %v", err))
	}
	return joined
}

// boundQueryInstanceID reports the query identity this workspace's live
// controller bound at its handshake, or "" when there is no live controller or
// no bound identity.
func (m *Manager) boundQueryInstanceID(workspace string) string {
	m.mu.Lock()
	defer m.mu.Unlock()
	d, live := m.byWS[workspace]
	if !live {
		return ""
	}
	return d.queryInstanceID
}
