package footer

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// liveSet builds the watcher's set from the ids it lists.
func liveSet(agents, shells, monitors []string) LiveWorkSet {
	var out LiveWorkSet
	for _, id := range agents {
		out.Agents = append(out.Agents, &conversationv1.AgentId{Value: id})
	}
	for _, id := range shells {
		out.Shells = append(out.Shells, &conversationv1.DetachedWorkId{Value: id})
	}
	for _, id := range monitors {
		out.Monitors = append(out.Monitors, &conversationv1.DetachedWorkId{Value: id})
	}
	return out
}

// createdMonitor is a monitor announced already detached, as the main agent's.
func createdMonitor(work, description string) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Owner: mainAgent,
		Work:  &conversationv1.DetachedWorkId{Value: work},
		Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{
			WorkCreated: &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Monitor{
				Monitor: monitorStart(work, description, false).GetMonitor(),
			}},
		}},
	}
}

// announceMain announces every item a set lists as the MAIN agent's work,
// which is the order the watcher delivers in: the announcement first, then the
// set that lists it. Only the main agent's work is drawn (owner.go), so a test
// of what a drawn item does states whose it is.
func announceMain(h *harness, set LiveWorkSet) {
	for _, a := range set.Agents {
		h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork(a.GetValue(), a.GetValue(), "Explore"))
	}
	for _, w := range set.Shells {
		h.r.OnDetachedWork(testWS, mainAgent, createdShell(w.GetValue(), "npm test"))
	}
	for _, w := range set.Monitors {
		h.r.OnDetachedWork(testWS, mainAgent, createdMonitor(w.GetValue(), "watch the build"))
	}
}

// viewChips is the strip's live-work chip counts, which is what a reader sees.
type viewChips struct {
	agents   uint32
	shells   uint32
	monitors uint32
}

// chipsOf reads the counts off a published view.
func chipsOf(view *frontendv1.FooterView) *viewChips {
	chips := view.GetStrip().GetLiveWork()
	return &viewChips{
		agents:   chips.GetAgents().GetCount(),
		shells:   chips.GetShells().GetCount(),
		monitors: chips.GetMonitors().GetCount(),
	}
}

// lastRecord is the newest record the operation wrote, failing when none did.
func lastRecord(t *testing.T, h *harness, operation string) dlog.Record {
	t.Helper()
	records := h.log.Records()
	for i := len(records) - 1; i >= 0; i-- {
		if records[i].Operation == operation {
			return records[i]
		}
	}
	t.Fatalf("no %s record was written; records = %+v", operation, records)
	return dlog.Record{}
}

// TestAnUnlistedDetachedRowIsRetiredByTheSet is the defect the watcher's set
// exists to close: a detached row whose terminal NEVER reached the footer in a
// form its ledger matched used to stand for the rest of the session. The
// watcher reaps each item at its own terminal, so a set that no longer lists
// the item retires the row whatever the footer heard.
func TestAnUnlistedDetachedRowIsRetiredByTheSet(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		live    func(view *viewChips) bool
	}{
		{
			name: "detached subagent",
			arrange: func(h *harness) {
				h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", "agent-2", "Explore"))
			},
			live: func(c *viewChips) bool { return c.agents > 0 },
		},
		{
			name: "detached shell",
			arrange: func(h *harness) {
				h.r.OnDetachedWork(testWS, mainAgent, createdShell("work-1", "npm test"))
			},
			live: func(c *viewChips) bool { return c.shells > 0 },
		},
		{
			name: "monitor",
			arrange: func(h *harness) {
				h.r.OnActivity(testWS, mainAgent, monitorStart("work-1", "watch the build", false))
			},
			live: func(c *viewChips) bool { return c.monitors > 0 },
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the item stands, with no terminal of any kind.
			h := newHarness(t)
			connected(h)
			tc.arrange(h)
			if !tc.live(chipsOf(h.view(t))) {
				t.Fatalf("the arrangement did not raise a chip; the test would assert nothing")
			}

			// Act: the watcher states a set that no longer lists it.
			h.r.OnLiveWorkChanged(testWS, LiveWorkSet{})

			// Assert
			if tc.live(chipsOf(h.view(t))) {
				t.Fatalf("a row the live-work set no longer lists survived; the set is the authority for liveness")
			}
		})
	}
}

// TestAnEmptyLiveWorkSetFallsBackToIdle is the arm half of the same defect: the
// strip reported a background task for a workspace the roster called ready.
func TestAnEmptyLiveWorkSetFallsBackToIdle(t *testing.T) {
	// Arrange: a detached run standing with no terminal, so the strip is background.
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", "agent-2", "Explore"))
	if got := h.status(t); got != "background" {
		t.Fatalf("status before the set = %q, want background", got)
	}

	// Act
	h.r.OnLiveWorkChanged(testWS, LiveWorkSet{})

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle once the authoritative set is empty", got)
	}
}

// TestAnItemTheSetListsWithNoRecordedOwnerIsKeptOut is the invariant half of
// the main-agent rule (owner ruling, 2026-09-30): whose work an item is is
// decided by recorded ownership, never guessed, so an item the set lists that
// no announcement or spawn frame placed is drawn by no chip, and the violation
// is recorded at ERROR.
func TestAnItemTheSetListsWithNoRecordedOwnerIsKeptOut(t *testing.T) {
	tests := []struct {
		name  string
		live  LiveWorkSet
		chips func(c *viewChips) uint32
	}{
		{"agent", liveSet([]string{"work-1"}, nil, nil), func(c *viewChips) uint32 { return c.agents }},
		{"shell", liveSet(nil, []string{"work-1"}, nil), func(c *viewChips) uint32 { return c.shells }},
		{"monitor", liveSet(nil, nil, []string{"work-1"}), func(c *viewChips) uint32 { return c.monitors }},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act: the set names an item nothing stated an owner for.
			h.r.OnLiveWorkChanged(testWS, tc.live)

			// Assert
			if got := tc.chips(chipsOf(h.view(t))); got != 0 {
				t.Fatalf("chip = %d, want 0: work with no recorded owner is never drawn", got)
			}
			rec := lastRecord(t, h, "daemon.footer.work_unowned")
			if rec.Level != dlog.LevelError || rec.Context["kind"] != tc.name || rec.Context["reason"] != feedid.ErrOwnerUnknown.Error() {
				t.Fatalf("record = %+v, want an ERROR naming the %s and the unknown owner", rec, tc.name)
			}
		})
	}
}

// TestAnItemInTheSetWithNoFrameRaisesTheBackgroundArm is the arm half of the
// minimal row: the status answers from the set, not from what was described.
func TestAnItemInTheSetWithNoFrameRaisesTheBackgroundArm(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{"work-1"}, nil, nil))

	// Assert
	if got := h.status(t); got != "background" {
		t.Fatalf("status = %q, want background for a set with a live item", got)
	}
}

// TestAMinimalRowIsEnrichedByTheFrameThatFollows locks the division of labor:
// the set decides WHICH items are live, the frames decide how each is DRAWN.
func TestAMinimalRowIsEnrichedByTheFrameThatFollows(t *testing.T) {
	// Arrange: the set opens a minimal row.
	h := newHarness(t)
	connected(h)
	h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"work-1"}, nil))

	// Act: the descriptive frame arrives afterwards.
	h.r.OnDetachedWork(testWS, mainAgent, createdShell("work-1", "npm test"))

	// Assert
	rows := h.view(t).GetExpanded().GetShells().GetRows()
	if len(rows) != 1 {
		t.Fatalf("shell rows = %d, want the one row the set opened, enriched rather than duplicated", len(rows))
	}
	if got := rows[0].GetCommand().GetText(); got != "npm test" {
		t.Fatalf("command = %q, want the announced command line on the row the set opened", got)
	}
}

// TestAnInTurnSubagentRowSurvivesAnEmptySet holds the line the watcher itself
// draws: a SYNC subagent carries no detached-work handle and is the turn's own
// progress, so the set never lists it and the set never retires it. The ⚙ chip
// counts it by the contract's own words.
func TestAnInTurnSubagentRowSurvivesAnEmptySet(t *testing.T) {
	// Arrange: an in-turn spawn, never detached.
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", "sweep the tree"))

	// Act
	h.r.OnLiveWorkChanged(testWS, LiveWorkSet{})

	// Assert
	if got := chipsOf(h.view(t)).agents; got != 1 {
		t.Fatalf("agents chip = %d, want the in-turn spawn still counted; it is not live work", got)
	}
}

// TestTakingTheSetIsRecordedWithItsDelta — an invisible action is a logging
// defect. A later disagreement between the strip and the roster is diagnosed
// from this record: the counts the footer took and the rows it moved.
func TestTakingTheSetIsRecordedWithItsDelta(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, createdShell("work-1", "npm test"))

	// Act: one shell leaves, another the footer never heard of arrives.
	h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"work-2"}, nil))

	// Assert
	rec := lastRecord(t, h, "daemon.footer.live_work_taken")
	if rec.Level != "info" {
		t.Fatalf("level = %q, want info", rec.Level)
	}
	if got := rec.Context["shells"]; got != 1 {
		t.Fatalf("shells = %v, want the set's own count", got)
	}
	if got := dropped(rec); len(got) != 1 || got[0] != "shell:work-1" {
		t.Fatalf("dropped = %v, want the row the set no longer lists", got)
	}
	if got := added(rec); len(got) != 1 || got[0] != "shell:work-2" {
		t.Fatalf("added = %v, want the row the set opened", got)
	}
}

// dropped is the record's dropped-row list.
func dropped(rec dlog.Record) []string {
	out, _ := rec.Context["dropped"].([]string)
	return out
}

// added is the record's added-row list.
func added(rec dlog.Record) []string {
	out, _ := rec.Context["added"].([]string)
	return out
}

// THE ZERO-TOKEN "subagent" ROW, reproduced: a detached run's own terminal
// retires its described row, the watcher's set still lists the run, and the
// reconcile re-opens a MINIMAL row for it — no type, no description, no tokens,
// and nothing left to describe it, because the retirement makes every later
// start a replay. The log now names it as such.
func TestASetThatReListsARetiredRunReopensAMinimalRowAndSaysSo(t *testing.T) {
	// Arrange: a described detached subagent, settled at its own terminal.
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", "work-1", "sonnet-medium"))
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{"work-1"}, nil, nil))
	h.r.OnSubagent(testWS, workID("work-1"), subagentSettled(false))

	// Act: the set still lists it.
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{"work-1"}, nil, nil))

	// Assert
	rows := h.view(t).GetExpanded().GetAgents().GetRows()
	if len(rows) != 1 || rows[0].GetLabel().GetText() != "subagent" || rows[0].GetTokens().GetText() != "0" {
		t.Fatalf("rows = %+v, want one minimal row labelled subagent with 0 tokens", rows)
	}
	rec := lastRecord(t, h, "daemon.footer.live_work_taken")
	if got, _ := rec.Context["readded_retired"].([]string); len(got) != 1 || got[0] != "agent:work-1" {
		t.Fatalf("readded_retired = %v, want the retired run the set re-listed", got)
	}
	jumps := recordsOf(h.log.Records(), "daemon.footer.jump_resolution")
	last := jumps[len(jumps)-1]
	if last.Context["provenance"] != "live_work_set" || last.Context["retired_before"] != true {
		t.Fatalf("jump record = %+v, want provenance live_work_set and retired_before true", last.Context)
	}
}
