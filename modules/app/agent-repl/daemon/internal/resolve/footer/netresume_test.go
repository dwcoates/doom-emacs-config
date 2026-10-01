package footer

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/deployprogress"
	"claude-repld/internal/dlog"
)

// resumeWaitOf is one standing wait for a piece of work.
func resumeWaitOf(work string, givesUpIn time.Duration, resumes uint32) *conversationv1.SessionNetworkResumeWait {
	return &conversationv1.SessionNetworkResumeWait{
		Work:             &conversationv1.DetachedWorkId{Value: work},
		FailedAtMs:       instant.UnixMilli(),
		GivesUpAtMs:      instant.Add(givesUpIn).UnixMilli(),
		ResumesDelivered: resumes,
	}
}

// resumeWaits is the shim's standing set, whole.
func resumeWaits(waits ...*conversationv1.SessionNetworkResumeWait) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_NetworkResumeWaits{
		NetworkResumeWaits: &conversationv1.SessionNetworkResumeWaits{Waits: waits}}}
}

// resumeOutcome is one ended wait.
func resumeOutcome(work string, arm string) *conversationv1.SessionUpdate {
	outcome := &conversationv1.SessionNetworkResumeOutcome{Work: &conversationv1.DetachedWorkId{Value: work}}
	switch arm {
	case "resumed":
		outcome.Outcome = &conversationv1.SessionNetworkResumeOutcome_Resumed{Resumed: &conversationv1.SessionNetworkResumeResumed{}}
	case "gave_up":
		outcome.Outcome = &conversationv1.SessionNetworkResumeOutcome_GaveUp{GaveUp: &conversationv1.SessionNetworkResumeGaveUp{}}
	case "abandoned":
		outcome.Outcome = &conversationv1.SessionNetworkResumeOutcome_Abandoned{
			Abandoned: &conversationv1.SessionNetworkResumeAbandoned{Reason: "session closing"}}
	}
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_NetworkResumeOutcome{NetworkResumeOutcome: outcome}}
}

// failedDetachedAgent runs a detached subagent "w-1" (label Explore,
// description "scan the repo", 900 tokens, drawn in the feed) to its failure
// terminal, which retires its live row.
func failedDetachedAgent(h *harness) {
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("w-1", "agent-w1", "Explore"))
	h.r.OnSubagent(testWS, workID("w-1"), &conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Update{
		Update: &conversationv1.AgentSubagentUpdate{
			Prompt:   &conversationv1.AgentSubagentPrompt{Description: ptr("scan the repo")},
			Progress: &conversationv1.AgentSubagentProgress{TotalTokens: 900},
		}}})
	h.r.OnEntryPlaced(testWS, "w-1", &frontendv1.FeedId{Value: "feed-w1"})
	h.r.OnSubagent(testWS, workID("w-1"), subagentSettled(true))
}

// agentRows is the published agents panel's rows.
func agentRows(t *testing.T, h *harness) []*frontendv1.FooterAgentRow {
	t.Helper()
	return h.view(t).GetExpanded().GetAgents().GetRows()
}

func TestAWaitKeepsTheFailedAgentsRowData(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	failedDetachedAgent(h)

	// Act
	h.r.OnSessionUpdate(testWS, resumeWaits(resumeWaitOf("w-1", 30*time.Minute, 0)))

	// Assert
	rows := agentRows(t, h)
	if len(rows) != 1 {
		t.Fatalf("%d agent rows, want the waiting agent's row", len(rows))
	}
	row := rows[0]
	if row.GetLabel().GetText() != "Explore" || row.GetDescription().GetText() != "scan the repo" ||
		row.GetTokens().GetText() != "900 tok" || row.GetJump().GetEntry().GetValue() != "feed-w1" {
		t.Fatalf("row = %+v, want the failed run's label, description, tokens and jump", row)
	}
}

func TestAWaitingRowIsDrawnWaitingForTheApi(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	failedDetachedAgent(h)

	// Act
	h.r.OnSessionUpdate(testWS, resumeWaits(resumeWaitOf("w-1", 30*time.Minute, 2)))

	// Assert
	waiting := agentRows(t, h)[0].GetWaitingForApi()
	if waiting.GetFailedAtMs() != instant.UnixMilli() || waiting.GetGivesUpAtMs() != instant.Add(30*time.Minute).UnixMilli() ||
		waiting.GetResumesDelivered() != 2 {
		t.Fatalf("waiting_for_api = %+v, want the wait's instants and resume count", waiting)
	}
}

func TestALiveRowIsDrawnRunning(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("w-1", "agent-w1", "Explore"))

	// Assert
	if agentRows(t, h)[0].GetRunning() == nil {
		t.Fatalf("row state = %+v, want running", agentRows(t, h)[0].GetState())
	}
}

func TestALiveRowAWaitNamesIsDrawnWaiting(t *testing.T) {
	// Arrange: the wait's statement beat the failure terminal.
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("w-1", "agent-w1", "Explore"))

	// Act
	h.r.OnSessionUpdate(testWS, resumeWaits(resumeWaitOf("w-1", 30*time.Minute, 0)))

	// Assert
	rows := agentRows(t, h)
	if len(rows) != 1 || rows[0].GetWaitingForApi() == nil {
		t.Fatalf("rows = %+v, want one row drawn waiting", rows)
	}
}

func TestTheAgentsChipCountsWaitingRowsAndCarriesTheGlyph(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	failedDetachedAgent(h)
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("w-2", "agent-w2", "Plan"))

	// Act
	h.r.OnSessionUpdate(testWS, resumeWaits(resumeWaitOf("w-1", 30*time.Minute, 0)))

	// Assert
	chip := h.view(t).GetStrip().GetLiveWork().GetAgents()
	if chip.GetCount() != 2 || chip.GetWaitingForApi().GetCount() != 1 {
		t.Fatalf("agents chip = %+v, want count 2 with one waiting", chip)
	}
}

func TestTheAgentsChipCarriesNoGlyphWhenNothingWaits(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("w-2", "agent-w2", "Plan"))

	// Assert
	if glyph := h.view(t).GetStrip().GetLiveWork().GetAgents().GetWaitingForApi(); glyph != nil {
		t.Fatalf("waiting glyph = %+v, want unset", glyph)
	}
}

func TestTheWaitingRowLeavesWhenTheWaitEnds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	failedDetachedAgent(h)
	h.r.OnSessionUpdate(testWS, resumeWaits(resumeWaitOf("w-1", 30*time.Minute, 0)))

	// Act
	h.r.OnSessionUpdate(testWS, resumeWaits())

	// Assert
	if rows := agentRows(t, h); len(rows) != 0 {
		t.Fatalf("rows = %+v, want none once the wait ended", rows)
	}
	if chip := h.view(t).GetStrip().GetLiveWork().GetAgents(); chip != nil {
		t.Fatalf("agents chip = %+v, want unset", chip)
	}
}

// TestAWaitForWorkTheFooterNeverDescribedIsKeptOut: a daemon that came up
// mid-wait has no record of whose work the wait names, and the footer draws
// only the main agent's work by recorded ownership (owner.go), so the wait
// draws no row and the violation is recorded at ERROR.
func TestAWaitForWorkTheFooterNeverDescribedIsKeptOut(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, resumeWaits(resumeWaitOf("w-9", 30*time.Minute, 0)))

	// Assert
	if rows := agentRows(t, h); len(rows) != 0 {
		t.Fatalf("rows = %+v, want none for work no owner was recorded for", rows)
	}
	rec := lastRecord(t, h, "daemon.footer.work_unowned")
	if rec.Level != dlog.LevelError || rec.Context["kind"] != "agent" {
		t.Fatalf("record = %+v, want an ERROR naming the unowned agent", rec)
	}
}

func TestANewWaitRaisesTheWaitingEdgeUnderTheAgentsLabel(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	failedDetachedAgent(h)

	// Act
	h.r.OnSessionUpdate(testWS, resumeWaits(resumeWaitOf("w-1", 30*time.Minute, 0)))

	// Assert
	got := transientOf(t, h)
	if got.GetNetworkResume().GetWaiting().GetGivesUpAtMs() != instant.Add(30*time.Minute).UnixMilli() {
		t.Fatalf("transient = %+v, want the waiting edge with its give-up instant", got)
	}
	if got.GetAgent().GetLabel() != "scan the repo" {
		t.Fatalf("agent = %+v, want the waiting subagent's label", got.GetAgent())
	}
}

func TestARestatedWaitRaisesNoNewEdge(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	failedDetachedAgent(h)
	h.r.OnSessionUpdate(testWS, resumeWaits(resumeWaitOf("w-1", 30*time.Minute, 0)))
	before := raisedCount(h)

	// Act: the window restarted; the wait itself is not new.
	h.r.OnSessionUpdate(testWS, resumeWaits(resumeWaitOf("w-1", 45*time.Minute, 1)))

	// Assert
	if after := raisedCount(h); after != before {
		t.Fatalf("%d transients raised by a restated wait, want 0", after-before)
	}
}

func TestEachOutcomeRaisesItsEdge(t *testing.T) {
	tests := []struct {
		arm  string
		edge func(*frontendv1.FooterActivityTransientNetworkResume) bool
	}{
		{"resumed", func(n *frontendv1.FooterActivityTransientNetworkResume) bool { return n.GetResumed() != nil }},
		{"gave_up", func(n *frontendv1.FooterActivityTransientNetworkResume) bool { return n.GetGaveUp() != nil }},
		{"abandoned", func(n *frontendv1.FooterActivityTransientNetworkResume) bool {
			return n.GetAbandoned().GetReason() == "session closing"
		}},
	}
	for _, tt := range tests {
		t.Run(tt.arm, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			failedDetachedAgent(h)
			h.r.OnSessionUpdate(testWS, resumeWaits(resumeWaitOf("w-1", 30*time.Minute, 0)))

			// Act
			h.r.OnSessionUpdate(testWS, resumeOutcome("w-1", tt.arm))

			// Assert
			got := transientOf(t, h)
			if !tt.edge(got.GetNetworkResume()) || got.GetAgent().GetLabel() != "scan the repo" {
				t.Fatalf("transient = %+v, want the %s edge under the agent's label", got, tt.arm)
			}
		})
	}
}

func TestAWaitNamingNoWorkRefusesTheWholeStatement(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	failedDetachedAgent(h)
	h.r.OnSessionUpdate(testWS, resumeWaits(resumeWaitOf("w-1", 30*time.Minute, 0)))

	// Act
	h.r.OnSessionUpdate(testWS, resumeWaits(resumeWaitOf("w-2", time.Minute, 0), resumeWaitOf("", time.Minute, 0)))

	// Assert
	if waits := h.r.states[testWS].resumeWaits; len(waits) != 1 || waits[0].work != "w-1" {
		t.Fatalf("waits = %+v, want the set on hand standing", waits)
	}
	if !hasLevel(h.log.Records(), "error", "daemon.footer.network_resume_waits_refused") {
		t.Fatalf("no ERROR daemon.footer.network_resume_waits_refused was recorded")
	}
}

func TestAMalformedOutcomeRaisesNothing(t *testing.T) {
	tests := []struct {
		name   string
		update *conversationv1.SessionUpdate
	}{
		{name: "no work", update: resumeOutcome("", "resumed")},
		{name: "no outcome arm", update: resumeOutcome("w-1", "")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.OnSessionUpdate(testWS, tt.update)

			// Assert
			if got := transientOf(t, h); got != nil {
				t.Fatalf("transient = %+v, want none for a malformed outcome", got)
			}
			if !hasLevel(h.log.Records(), "error", "daemon.footer.network_resume_outcome_refused") {
				t.Fatalf("no ERROR daemon.footer.network_resume_outcome_refused was recorded")
			}
		})
	}
}

// TestAWaitCountsTowardNoOtherRule pins the owner's visibility-only ruling: a
// standing wait moves no status and no deploy count, only the agents chip,
// panel and the transient.
func TestAWaitCountsTowardNoOtherRule(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	failedDetachedAgent(h)
	h.r.OnLiveWorkChanged(testWS, liveSet(nil, nil, nil))
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.HandingOver, Draining: true})

	// Act
	h.r.OnSessionUpdate(testWS, resumeWaits(resumeWaitOf("w-1", 30*time.Minute, 0)))

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle: a wait is not background work", got)
	}
	if got := updatePhase(h.view(t).GetStrip().GetStatus()); got != "handing_over" {
		t.Fatalf("update phase = %q, want handing_over: a wait is nothing the drain waits on", got)
	}
}
