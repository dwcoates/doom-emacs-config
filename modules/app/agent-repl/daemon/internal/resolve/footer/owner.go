package footer

import (
	"errors"
	"fmt"
	"sort"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// THE FOOTER DRAWS ONLY THE MAIN AGENT'S WORK (owner ruling, 2026-09-30:
// "work the subagents started should not appear in the expanded footer. only
// work detached by the main agent should appear there"). A subagent the main
// agent spawned is drawn; a subagent, shell or monitor a SUBAGENT started is
// not, wherever it entered the footer from.
//
// ONE PREDICATE DECIDES IT: drawsWork. Every reader that draws a live-work row
// — the ⚙, $ and 👁 chips and their expanded panels (through agentRowsDrawn,
// shellRowsDrawn and monitorRowsDrawn), and the focus a launch mints (through
// mainAgentWork) — asks it, and nothing else draws a row. So a nested item
// cannot reach the view by any path that opens a row: an announcement, a spawn
// frame, a detached run's own frames, the live-work set's minimal row, or a
// network-resume wait.
//
// THE ROWS THEMSELVES STAY. A nested row is still the DESCRIPTION its frames
// built — the usage attribution and a transient's agent label read it — and it
// is the drawing that refuses it. Counting is not drawing either: the
// `background` status arm and the deploy's "waiting: background N" read the
// watcher's whole set (wsState.detachedCount), because both say that work is
// RUNNING, and a subagent's shell is running work the roster and the deploy's
// drain wait on too.
//
// OWNERSHIP IS RECORDED, NEVER GUESSED. It is learned from the two places that
// can say, the same two feedid.DetachedOwner reads for the feed: the owner an
// announcement STATES, and the agent whose stream CARRIED the spawning call (a
// subagent, shell or monitor start frame). Work no source names an owner for,
// and work two sources name two owners for, are invariant violations: each is
// recorded at ERROR, once, and the item is kept out.
//
// AN UNNAMED MAIN AGENT IS AN ORDERING, NOT A VIOLATION. A boot's adoption
// announces the work the session restored before the main watch names the
// main agent (sessionwatcher.adoptLiveWorkLocked runs first), exactly as the
// feed holds a detachment it cannot place yet. Until the naming arrives no
// item can be told apart from the main agent's, so none is drawn; the refusal
// is recorded at DEBUG, and OnMainAgent's republish draws what is the main
// agent's.

// ownerRecord is what the footer knows about one identity's owner.
type ownerRecord struct {
	// agent is the owning agent.
	agent string
	// source names what stated it first, for the record.
	source string
	// conflict is set when a second source named a different owner. A
	// contradicted identity is drawn by neither reading.
	conflict string
}

// The sources an owner is recorded from.
const (
	// ownerFromAnnouncement is a detachment announcement's stated owner, or
	// the spawning call's carrier when the announcement stated none.
	ownerFromAnnouncement = "announcement"
	// ownerFromSpawnFrame is the agent whose stream carried a spawn frame.
	ownerFromSpawnFrame = "spawn_frame"
)

// errOwnerConflict is the answer for an identity two sources disagree about.
var errOwnerConflict = errors.New("footer: two sources name different owners for this work")

// recordOwner records one identity's owner. An empty identity or owner records
// nothing. A source naming a DIFFERENT owner than the one on record is a
// contradiction: recorded at ERROR, and the identity is drawn by neither.
func (r *resolver) recordOwner(s *wsState, id, owner, source string) {
	if id == "" || owner == "" {
		return
	}
	held, ok := s.workOwners[id]
	if !ok {
		s.workOwners[id] = &ownerRecord{agent: owner, source: source}
		return
	}
	if held.agent == owner || held.conflict != "" {
		return
	}
	held.conflict = owner
	r.logOf(s.id, s).Error("daemon.footer.work_owner_conflict",
		"two sources name different owners for one piece of work; it is kept out of the live-work chips and the expanded footer",
		dlog.Context{
			"work_id": id, "owner": held.agent, "owner_source": held.source,
			"contradicting_owner": owner, "contradicting_source": source,
		})
}

// recordSpawnOwner records the CARRIER of a spawn frame as the owner of the
// work it starts: a subagent spawn (under its unit and its created agent), a
// shell's run and a monitor's watch, each under its unit.
func (r *resolver) recordSpawnOwner(s *wsState, agent *conversationv1.AgentId, act *conversationv1.AgentActivity) {
	unit := act.GetActivityId().GetValue()
	carrier := agent.GetValue()
	switch item := act.GetItem().(type) {
	case *conversationv1.AgentActivity_Subagent:
		if start := item.Subagent.GetStart(); start != nil {
			r.recordOwner(s, unit, carrier, ownerFromSpawnFrame)
			r.recordOwner(s, start.GetCreatedAgentId().GetValue(), carrier, ownerFromSpawnFrame)
		}
	case *conversationv1.AgentActivity_Bash:
		if item.Bash.GetStart() != nil {
			r.recordOwner(s, unit, carrier, ownerFromSpawnFrame)
		}
	case *conversationv1.AgentActivity_Monitor:
		if item.Monitor.GetStart() != nil {
			r.recordOwner(s, unit, carrier, ownerFromSpawnFrame)
		}
	}
}

// recordAnnouncedOwner records the owner a detachment announcement places the
// work with — its stated owner, or the carrier of the call it detached from
// when it stated none (feedid.DetachedOwner) — under every identity the work
// is addressed by: its handle, the unit it detached from, and its agent. An
// announcement nothing places records nothing; the predicate reports it when
// the work would be drawn.
func (r *resolver) recordAnnouncedOwner(s *wsState, work *conversationv1.AgentDetachedWork) {
	handle := work.GetWork().GetValue()
	identities := []string{
		handle,
		work.GetDetached().GetDetachedFromId().GetValue(),
		work.GetKind().GetSubagent().GetAgentId().GetValue(),
		work.GetCreated().GetWorkCreated().GetSubagent().GetStart().GetCreatedAgentId().GetValue(),
	}
	carrier := ""
	for _, id := range identities {
		if held, ok := s.workOwners[id]; ok && held.source == ownerFromSpawnFrame {
			carrier = held.agent
			break
		}
	}
	owner, err := feedid.DetachedOwner(work.GetOwner().GetValue(), carrier)
	if err != nil {
		// A stated owner the carrier contradicts is recorded as stated, so
		// recordOwner names the contradiction; an announcement nothing places
		// is left to the predicate.
		owner = work.GetOwner().GetValue()
	}
	for _, id := range identities {
		r.recordOwner(s, id, owner, ownerFromAnnouncement)
	}
}

// ownerOf answers the one owner on record for work addressed by these
// identities: ErrOwnerUnknown when none records one, errOwnerConflict when they
// disagree or one is contradicted.
func (s *wsState) ownerOf(identities ...string) (string, error) {
	owner := ""
	for _, id := range identities {
		held, ok := s.workOwners[id]
		if id == "" || !ok {
			continue
		}
		if held.conflict != "" || (owner != "" && owner != held.agent) {
			return "", errOwnerConflict
		}
		owner = held.agent
	}
	if owner == "" {
		return "", feedid.ErrOwnerUnknown
	}
	return owner, nil
}

// drawsWork is THE ONE PREDICATE for whether a live-work row is drawn: only
// when the work's recorded owner is the session's MAIN agent, which is what
// places it on the root feed (feedid.AgentFeed). Work owned by any other agent
// is a subagent's and is kept out without a record; work that cannot be
// decided is kept out and reported at ERROR, once per identity and cause; and
// work waiting on the main agent's naming is kept out until it arrives,
// recorded at DEBUG.
func (r *resolver) drawsWork(s *wsState, kind string, identities ...string) bool {
	owner, err := s.ownerOf(identities...)
	if err == nil {
		var feed feedid.Feed
		feed, err = feedid.AgentFeed(owner, s.mainAgent)
		if err == nil {
			return feed.Root
		}
	}
	key := fmt.Sprintf("%s:%v:%s", kind, identities, err)
	if _, done := s.unownedReported[key]; done {
		return false
	}
	s.unownedReported[key] = struct{}{}
	if errors.Is(err, feedid.ErrMainAgentUnknown) {
		r.logOf(s.id, s).Debug("daemon.footer.work_awaits_main_agent",
			"live work is held out of the footer until the session's main agent is named",
			dlog.Context{"kind": kind, "identities": identities, "owner": owner})
		return false
	}
	r.logOf(s.id, s).Error("daemon.footer.work_unowned",
		"live work reached the footer without an owner it can be decided by; it is kept out of the live-work chips and the expanded footer",
		dlog.Context{
			"kind": kind, "identities": identities, "owner": owner,
			"main_agent": s.mainAgent, "reason": err.Error(),
		})
	return false
}

// shellRowsDrawn is the $ chip's count and panel's rows: the main agent's live
// shells, in announcement order.
func (r *resolver) shellRowsDrawn(s *wsState) []*shellRow {
	rows := make([]*shellRow, 0, len(s.shells))
	for _, row := range s.shells {
		if r.drawsWork(s, "shell", row.work) {
			rows = append(rows, row)
		}
	}
	sort.Slice(rows, func(i, j int) bool { return rows[i].order < rows[j].order })
	return rows
}

// monitorRowsDrawn is the 👁 chip's count and panel's rows: the main agent's
// live monitors, in arming order.
func (r *resolver) monitorRowsDrawn(s *wsState) []*monitorRow {
	rows := make([]*monitorRow, 0, len(s.monitors))
	for _, row := range s.monitors {
		if r.drawsWork(s, "monitor", row.unit) {
			rows = append(rows, row)
		}
	}
	sort.Slice(rows, func(i, j int) bool { return rows[i].order < rows[j].order })
	return rows
}

// mainAgentWork is the part of a live-work set the footer DRAWS: the items
// drawsWork admits. It is what a launch is seen in and a focus is minted from,
// so a subagent's own launch never opens a panel it is not drawn in.
func (r *resolver) mainAgentWork(s *wsState, live LiveWorkSet) LiveWorkSet {
	var out LiveWorkSet
	for _, a := range live.Agents {
		if r.drawsWork(s, "agent", a.GetValue()) {
			out.Agents = append(out.Agents, a)
		}
	}
	for _, w := range live.Shells {
		if r.drawsWork(s, "shell", w.GetValue()) {
			out.Shells = append(out.Shells, w)
		}
	}
	for _, w := range live.Monitors {
		if r.drawsWork(s, "monitor", w.GetValue()) {
			out.Monitors = append(out.Monitors, w)
		}
	}
	return out
}
