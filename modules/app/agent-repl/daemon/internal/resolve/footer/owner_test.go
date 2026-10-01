package footer

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/deployprogress"
	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/shimclient"
)

// productionSubagent is the subagent whose own monitor and shell the owner saw
// in the expanded footer (2026-09-30). A subagent's AgentId is minted from its
// spawning call's tool_use_id, which is also its handle.
var productionSubagent = &conversationv1.AgentId{Value: "toolu_01TsWGUArWwd8sDoupKFwNBw"}

// bashStartFrame is a shell call starting on whichever agent's stream carries
// it.
func bashStartFrame(unit, command string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{
			Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
				Command:   &conversationv1.AgentBashCommand{Line: command},
				StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: instant.UnixMilli()},
			}},
		}},
	}
}

// ownedBy restates an announcement as OWNER's work; a nil owner states none.
func ownedBy(owner *conversationv1.AgentId, work *conversationv1.AgentDetachedWork) *conversationv1.AgentDetachedWork {
	work.Owner = owner
	return work
}

// movedShell is a shell call LEAVING the turn under its own handle, with the
// owner the producer stated.
func movedShell(unit string, owner *conversationv1.AgentId) *conversationv1.AgentDetachedWork {
	work := movedSubagent(unit)
	work.Owner = owner
	return work
}

// mainSpawnsProductionSubagent is the main agent spawning the background
// subagent the regression is about, and the set listing it.
func mainSpawnsProductionSubagent(h *harness) {
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork(productionSubagent.GetValue(), productionSubagent.GetValue(), "general-purpose"))
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{productionSubagent.GetValue()}, nil, nil))
}

// drawnCounts is the strip's chip counts and the expanded panels' row counts,
// which is everything a reader can see of live work.
type drawnCounts struct {
	agentChip, shellChip, monitorChip uint32
	agentRows, shellRows, monitorRows int
}

// drawnOf reads every live-work count off the latest view.
func drawnOf(t *testing.T, h *harness) drawnCounts {
	t.Helper()
	view := h.view(t)
	chips := view.GetStrip().GetLiveWork()
	return drawnCounts{
		agentChip:   chips.GetAgents().GetCount(),
		shellChip:   chips.GetShells().GetCount(),
		monitorChip: chips.GetMonitors().GetCount(),
		agentRows:   len(view.GetExpanded().GetAgents().GetRows()),
		shellRows:   len(view.GetExpanded().GetShells().GetRows()),
		monitorRows: len(view.GetExpanded().GetMonitors().GetRows()),
	}
}

func TestASubagentsOwnDetachedSubagentIsNeverDrawn(t *testing.T) {
	// Arrange: the main agent's subagent runs, and is drawn.
	h := newHarness(t)
	connected(h)
	mainSpawnsProductionSubagent(h)

	// Act: that subagent detaches a subagent of its own.
	h.r.OnDetachedWork(testWS, mainAgent, ownedBy(productionSubagent, detachedSubagentWork("toolu_nested", "toolu_nested", "Explore")))
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{productionSubagent.GetValue(), "toolu_nested"}, nil, nil))

	// Assert
	if got, want := drawnOf(t, h), (drawnCounts{agentChip: 1, agentRows: 1}); got != want {
		t.Fatalf("drawn = %+v, want %+v: only the main agent's subagent is drawn", got, want)
	}
}

func TestASubagentsOwnInTurnSpawnIsNeverDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	mainSpawnsProductionSubagent(h)

	// Act: the subagent spawns a subagent in its own turn, on its own stream.
	h.r.OnActivity(testWS, productionSubagent, subagentStart("toolu_inner", "toolu_inner", "Explore", ""))

	// Assert
	if got, want := drawnOf(t, h), (drawnCounts{agentChip: 1, agentRows: 1}); got != want {
		t.Fatalf("drawn = %+v, want %+v: a subagent's own spawn is not the main agent's", got, want)
	}
}

func TestASubagentsOwnShellIsNeverDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	mainSpawnsProductionSubagent(h)
	h.r.OnActivity(testWS, productionSubagent, bashStartFrame("toolu_shell", "npm test"))

	// Act
	h.r.OnDetachedWork(testWS, mainAgent, movedShell("toolu_shell", productionSubagent))
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{productionSubagent.GetValue()}, []string{"toolu_shell"}, nil))

	// Assert
	if got, want := drawnOf(t, h), (drawnCounts{agentChip: 1, agentRows: 1}); got != want {
		t.Fatalf("drawn = %+v, want %+v: a subagent's shell is not drawn", got, want)
	}
}

func TestASubagentsOwnMonitorIsNeverDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	mainSpawnsProductionSubagent(h)

	// Act: the monitor's own start frame rides the subagent's stream.
	h.r.OnActivity(testWS, productionSubagent, monitorStart("toolu_monitor", "watch the build", false))
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{productionSubagent.GetValue()}, nil, []string{"toolu_monitor"}))

	// Assert
	if got, want := drawnOf(t, h), (drawnCounts{agentChip: 1, agentRows: 1}); got != want {
		t.Fatalf("drawn = %+v, want %+v: a subagent's monitor is not drawn", got, want)
	}
}

func TestTheMainAgentsOwnWorkIsDrawn(t *testing.T) {
	tests := []struct {
		name string
		act  func(h *harness)
		want drawnCounts
	}{
		{
			name: "an in-turn spawn",
			act: func(h *harness) {
				h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", ""))
			},
			want: drawnCounts{agentChip: 1, agentRows: 1},
		},
		{
			name: "a detached subagent",
			act: func(h *harness) {
				h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", "work-1", "Explore"))
				h.r.OnLiveWorkChanged(testWS, liveSet([]string{"work-1"}, nil, nil))
			},
			want: drawnCounts{agentChip: 1, agentRows: 1},
		},
		{
			name: "a detached shell",
			act: func(h *harness) {
				h.r.OnActivity(testWS, mainAgent, bashStartFrame("bash-1", "npm test"))
				h.r.OnDetachedWork(testWS, mainAgent, movedShell("bash-1", mainAgent))
				h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"bash-1"}, nil))
			},
			want: drawnCounts{shellChip: 1, shellRows: 1},
		},
		{
			name: "a monitor",
			act: func(h *harness) {
				h.r.OnActivity(testWS, mainAgent, monitorStart("mon-1", "watch the build", false))
				h.r.OnLiveWorkChanged(testWS, liveSet(nil, nil, []string{"mon-1"}))
			},
			want: drawnCounts{monitorChip: 1, monitorRows: 1},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			tc.act(h)

			// Assert
			if got := drawnOf(t, h); got != tc.want {
				t.Fatalf("drawn = %+v, want %+v", got, tc.want)
			}
		})
	}
}

func TestAnAnnouncementNoSourcePlacesIsKeptOutAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act: a created shell whose announcement states no owner, from no call
	// the footer saw.
	h.r.OnDetachedWork(testWS, mainAgent, ownedBy(nil, createdShell("shell-1", "npm test")))

	// Assert
	if got := drawnOf(t, h); got != (drawnCounts{}) {
		t.Fatalf("drawn = %+v, want nothing for work with no owner", got)
	}
	rec := lastRecord(t, h, "daemon.footer.work_unowned")
	if rec.Level != dlog.LevelError || rec.Context["kind"] != "shell" || rec.Context["reason"] != feedid.ErrOwnerUnknown.Error() {
		t.Fatalf("record = %+v, want an ERROR naming the shell and the unknown owner", rec)
	}
}

func TestAnUnownedItemIsRecordedOnceHoweverOftenItRenders(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, ownedBy(nil, createdShell("shell-1", "npm test")))

	// Act: three more pushes render the same refusal.
	for range 3 {
		h.r.OnActivity(testWS, mainAgent, taskAct("task-1", pendingTask(stated("write the tests"))))
	}

	// Assert
	if got := countOf(h.log.Records(), "error", "daemon.footer.work_unowned"); got != 1 {
		t.Fatalf("work_unowned ERROR records = %d, want exactly one", got)
	}
}

func TestAnUnstatedOwnerIsTakenFromTheSpawningCallsCarrier(t *testing.T) {
	// Arrange: the main agent's shell call is seen.
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, bashStartFrame("bash-1", "npm test"))

	// Act: its detachment states no owner.
	h.r.OnDetachedWork(testWS, mainAgent, movedShell("bash-1", nil))

	// Assert
	if got, want := drawnOf(t, h), (drawnCounts{shellChip: 1, shellRows: 1}); got != want {
		t.Fatalf("drawn = %+v, want %+v: the call's carrier is a recorded owner", got, want)
	}
}

func TestAStatedOwnerTheCarrierContradictsIsKeptOutAtError(t *testing.T) {
	// Arrange: the shell call rides the subagent's stream.
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, productionSubagent, bashStartFrame("bash-1", "npm test"))

	// Act: its detachment names the main agent.
	h.r.OnDetachedWork(testWS, mainAgent, movedShell("bash-1", mainAgent))

	// Assert
	if got := drawnOf(t, h); got != (drawnCounts{}) {
		t.Fatalf("drawn = %+v, want nothing for contradicted ownership", got)
	}
	rec := lastRecord(t, h, "daemon.footer.work_owner_conflict")
	if rec.Level != dlog.LevelError || rec.Context["owner"] != productionSubagent.GetValue() || rec.Context["contradicting_owner"] != mainAgent.GetValue() {
		t.Fatalf("record = %+v, want an ERROR naming both owners", rec)
	}
}

// mainUnnamedLink brings the link up without naming the main agent, which is
// where a boot's adoption announces restored work.
func mainUnnamedLink(h *harness) {
	h.r.SetParticipants(testWS, true, true)
	h.r.OnLink(testWS, shimclient.LinkConnected)
}

func TestWorkIsHeldOutWhileTheMainAgentIsUnnamed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	mainUnnamedLink(h)

	// Act: a boot's adoption announces the main agent's restored shell.
	h.r.OnDetachedWork(testWS, nil, createdShell("shell-1", "npm test"))

	// Assert
	if got := drawnOf(t, h); got != (drawnCounts{}) {
		t.Fatalf("drawn = %+v, want nothing while no agent can be told apart from the main one", got)
	}
	if !hasLevel(h.log.Records(), "debug", "daemon.footer.work_awaits_main_agent") || hasLevel(h.log.Records(), "error", "daemon.footer.work_unowned") {
		t.Fatalf("records = %+v, want the hold at DEBUG and no ERROR: an unnamed main agent is an ordering", h.log.Records())
	}
}

func TestHeldWorkIsDrawnOnceTheMainAgentIsNamed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	mainUnnamedLink(h)
	h.r.OnDetachedWork(testWS, nil, createdShell("shell-1", "npm test"))

	// Act
	h.r.OnMainAgent(testWS, mainAgent)

	// Assert
	if got, want := drawnOf(t, h), (drawnCounts{shellChip: 1, shellRows: 1}); got != want {
		t.Fatalf("drawn = %+v, want %+v once the main agent is named", got, want)
	}
}

func TestASubagentsLaunchMintsNoFocus(t *testing.T) {
	// Arrange: the main agent's subagent launched and focused the agents.
	h := newHarness(t)
	connected(h)
	mainSpawnsProductionSubagent(h)
	before := focusOfView(h.view(t))

	// Act: the subagent starts a monitor.
	h.r.OnActivity(testWS, productionSubagent, monitorStart("toolu_monitor", "watch the build", false))
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{productionSubagent.GetValue()}, nil, []string{"toolu_monitor"}))

	// Assert
	if got := focusOfView(h.view(t)); got != before || before != (focusRead{"agents", 1}) {
		t.Fatalf("focus = %+v (before %+v), want the main agent's launch's focus to stand", got, before)
	}
}

func TestASubagentsRunningShellStillHoldsTheBackgroundArm(t *testing.T) {
	// Arrange: the subagent has finished; its shell still runs.
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, productionSubagent, bashStartFrame("toolu_shell", "npm test"))
	h.r.OnDetachedWork(testWS, mainAgent, movedShell("toolu_shell", productionSubagent))

	// Act
	h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"toolu_shell"}, nil))

	// Assert: running work is background whoever started it, as the roster says.
	if got := h.status(t); got != "background" {
		t.Fatalf("status = %q, want background while a subagent's shell runs", got)
	}
}

func TestADrainingDeployWaitsOnASubagentsRunningShell(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, productionSubagent, bashStartFrame("toolu_shell", "npm test"))
	h.r.OnDetachedWork(testWS, mainAgent, movedShell("toolu_shell", productionSubagent))
	h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"toolu_shell"}, nil))

	// Act
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.HandingOver, Draining: true})

	// Assert
	waiting := updateOf(h.view(t).GetStrip().GetStatus()).GetWaiting()
	if waiting.GetBackground() != 1 {
		t.Fatalf("waiting = %+v, want the subagent's running shell counted", waiting)
	}
}

// TestAMonitorAndAShellASubagentStartedAreNeverDrawn is the production shape
// the owner ruled on (2026-09-30): the main agent's background subagent
// toolu_01TsWGUArWwd8sDoupKFwNBw started a monitor and a shell, and both were
// drawn in the expanded footer beside it.
func TestAMonitorAndAShellASubagentStartedAreNeverDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	mainSpawnsProductionSubagent(h)

	// Act: the subagent's own calls, each announced on the main agent's book
	// (the vendor's task stream is session-wide) with the subagent as owner.
	h.r.OnActivity(testWS, productionSubagent, monitorStart("toolu_monitor", "watch the deploy log", true))
	h.r.OnDetachedWork(testWS, mainAgent, ownedBy(productionSubagent, createdMonitor("toolu_monitor", "watch the deploy log")))
	h.r.OnActivity(testWS, productionSubagent, bashStartFrame("toolu_shell", "npm run test:integration"))
	h.r.OnDetachedWork(testWS, mainAgent, movedShell("toolu_shell", productionSubagent))
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{productionSubagent.GetValue()}, []string{"toolu_shell"}, []string{"toolu_monitor"}))

	// Assert
	if got, want := drawnOf(t, h), (drawnCounts{agentChip: 1, agentRows: 1}); got != want {
		t.Fatalf("drawn = %+v, want %+v: only the subagent the main agent spawned", got, want)
	}
	if got := focusOfView(h.view(t)); got != (focusRead{"agents", 1}) {
		t.Fatalf("focus = %+v, want the main agent's launch alone to have focused", got)
	}
}
