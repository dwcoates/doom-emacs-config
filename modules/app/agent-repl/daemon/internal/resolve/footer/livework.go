package footer

import (
	"sort"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
)

// LiveWorkSet is the watcher's set of live detached work. It aliases the
// watcher's spelling — exactly as the roster's does — so the strip's
// `background` arm and the roster's `idle_async` arm cannot drift apart by
// holding two shapes of the same fact.
type LiveWorkSet = sessionwatcher.LiveWorkSet

// OnLiveWorkChanged takes the watcher's AUTHORITATIVE live-work set.
//
// THE SET DECIDES WHICH DETACHED ITEMS ARE LIVE; the footer's frames decide
// only how each one is DESCRIBED. Before this edge existed the footer kept a
// parallel ledger and retired a row only when a terminal reached it in a form
// the ledger matched — so a detached item whose terminal did not, stood
// forever, and the strip reported a background task for a workspace the roster
// and the tab-bar both called ready (owner's report, workspace
// random-puzzle-analysis). The ledger had been patched per delivery path twice
// before (see chips.go OnSubagent, the G50 reading); this is the structural
// answer: liveness has ONE owner, the watcher, which reaps each item's watch at
// its own terminal.
//
// AN IN-TURN SUBAGENT IS NOT LIVE WORK and is deliberately left alone. The
// watcher excludes it from the set by the same reasoning (sessionwatcher's
// liveWorkLocked: "a SYNC subagent's watch carries no detached-work handle: it
// is the turn's own progress"), while the ⚙ chip counts it by the contract's
// own words ("live agent-spawned subagents (the ones with feed bubbles)"). Its
// retirement stays with its spawning call's terminal, where it has always been.
//
// THE SET IS ALSO WHERE A LAUNCH IS SEEN. An item the set lists for the first
// time is detached work that has just started, and each such change mints the
// view's focus (see launchedWork and mintFocus) -- read off the MAIN AGENT'S
// part of the set alone, because only the main agent's work is drawn
// (owner.go). The whole set is still what the footer holds and counts.
func (r *resolver) OnLiveWorkChanged(ws ids.WorkspaceID, live LiveWorkSet) {
	var dropped, added, readded []string
	var minted *mintedFocus
	r.mutate(ws, "daemon.footer.on_live_work_changed", "the footer took the live-work set",
		dlog.Context{
			"agents": len(live.Agents), "shells": len(live.Shells), "monitors": len(live.Monitors),
		}, func(s *wsState) {
			// A LAUNCH IS SEEN, AND A FOCUS MINTED, ONLY IN THE MAIN AGENT'S
			// WORK: a subagent's own launch would open a panel it is not
			// drawn in (owner.go). The whole set is still what s.liveWork
			// holds, because `background` and the deploy's wait count it.
			drawn := r.mainAgentWork(s, live)
			minted = mintFocus(s, launchedWork(s, r.mainAgentWork(s, s.liveWork), drawn), drawn)
			s.liveWork = live
			s.liveWorkSeen = true
			dropped, added, readded = reconcileLiveWork(s, live, r.opts.clock.Now())
		})
	r.workspaceLog(ws).Info("daemon.footer.live_work_taken",
		"the footer took the watcher's live-work set as the authority for which detached work is live",
		dlog.Context{
			"agents":   len(live.Agents),
			"shells":   len(live.Shells),
			"monitors": len(live.Monitors),
			"dropped":  dropped,
			"added":    added,
			// A MINIMAL ROW FOR A RUN WHOSE OWN TERMINAL THE FOOTER ALREADY
			// SAW: the set still lists work the footer retired, so the row is
			// re-opened with no description and no tokens (label "subagent",
			// 0 tok) and nothing will ever describe it again. Named here so a
			// zero-token row is explained by the log alone.
			"readded_retired": readded,
		})
	if minted != nil {
		r.workspaceLog(ws).Info("daemon.footer.focus_minted",
			"detached work started, so the footer focused the expanded section on the highest-priority live kind",
			dlog.Context{
				"panel":      minted.panel.String(),
				"generation": minted.generation,
				"work_id":    minted.trigger,
				"launched":   minted.launched,
			})
	}
}

// reconcileLiveWork makes the footer's DETACHED rows agree with the set: a row
// the set does not list is dropped, and an id the set lists with no row of its
// own gains a minimal one so the chip counts it while its descriptive frame is
// still on its way. It answers what it dropped and what it added, and which of
// the added ids the footer had already seen settle, for the record the caller
// writes.
func reconcileLiveWork(s *wsState, live LiveWorkSet, now time.Time) (dropped, added, readded []string) {
	agents := agentIDSet(live.Agents)
	shells := workIDSet(live.Shells)
	monitors := workIDSet(live.Monitors)

	for unit, row := range s.agents {
		if row.work == "" {
			// The turn's own progress: not live work, not the set's to govern.
			continue
		}
		if rowIsListed(agents, unit, row.work, row.spawnUnit, row.createdAgent) {
			continue
		}
		// A dropped run's token units stop counting with it, exactly as its
		// own terminal would have stopped them.
		s.tok.forgetAgent(row.createdAgent)
		s.rememberRetired(row)
		delete(s.agents, unit)
		dropped = append(dropped, "agent:"+unit)
	}
	for id := range s.shells {
		if rowIsListed(shells, id) {
			continue
		}
		delete(s.shells, id)
		dropped = append(dropped, "shell:"+id)
	}
	for unit := range s.monitors {
		if rowIsListed(monitors, unit) {
			continue
		}
		delete(s.monitors, unit)
		dropped = append(dropped, "monitor:"+unit)
	}

	for _, id := range sortedKeys(agents) {
		if s.subagentRow(id) != nil {
			continue
		}
		// THE HANDLE, THE SPAWN UNIT AND THE CREATED AGENT ARE ONE VALUE by
		// the contract's own ruling (`DetachedWorkId.value ==
		// AgentActivityId.value`, and for a subagent that is its `AgentId`
		// too), so a minimal row can be keyed and addressed from the single id
		// the set carries. `startedAt` is the instant the footer learned of the
		// run rather than a zero time, which would draw a runtime measured from
		// the epoch; the run's own start frame installs the true instant.
		s.agents[id] = s.minimalAgentRow(id, id, now, provenanceLiveWorkSet)
		added = append(added, "agent:"+id)
		if retiredAny(s, id) {
			readded = append(readded, "agent:"+id)
		}
	}
	for _, id := range sortedKeys(shells) {
		if _, held := s.shells[id]; held {
			continue
		}
		s.shells[id] = &shellRow{work: id, startedAt: now, order: s.nextOrder()}
		added = append(added, "shell:"+id)
		if retiredAny(s, id) {
			readded = append(readded, "shell:"+id)
		}
	}
	for _, id := range sortedKeys(monitors) {
		if _, held := s.monitors[id]; held {
			continue
		}
		s.monitors[id] = &monitorRow{unit: id, startedAt: now, order: s.nextOrder()}
		added = append(added, "monitor:"+id)
	}
	return dropped, added, readded
}

// rowIsListed reports whether the set lists any of the identities a row is
// addressed by. An EMPTY identity never matches: a row with no handle is not
// addressed by the empty string.
func rowIsListed(set map[string]struct{}, identities ...string) bool {
	for _, id := range identities {
		if id == "" {
			continue
		}
		if _, listed := set[id]; listed {
			return true
		}
	}
	return false
}

// agentIDSet is the set's live subagent ids.
func agentIDSet(agents []*conversationv1.AgentId) map[string]struct{} {
	out := make(map[string]struct{}, len(agents))
	for _, a := range agents {
		if v := a.GetValue(); v != "" {
			out[v] = struct{}{}
		}
	}
	return out
}

// workIDSet is the set's live detached-work handles.
func workIDSet(work []*conversationv1.DetachedWorkId) map[string]struct{} {
	out := make(map[string]struct{}, len(work))
	for _, w := range work {
		if v := w.GetValue(); v != "" {
			out[v] = struct{}{}
		}
	}
	return out
}

// sortedKeys is the set's ids in a stable order, so the rows a set adds are
// ordered by id rather than by map iteration.
func sortedKeys(set map[string]struct{}) []string {
	out := make([]string, 0, len(set))
	for id := range set {
		out = append(out, id)
	}
	sort.Strings(out)
	return out
}

// workspaceLog answers the workspace's logger without mutating anything, for
// the records written outside a mutation.
func (r *resolver) workspaceLog(ws ids.WorkspaceID) dlog.Logger {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.logOf(ws, r.stateLocked(ws))
}
