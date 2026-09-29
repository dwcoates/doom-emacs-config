package footer

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/deployprogress"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
)

// updateOf reads the update line whatever status arm carries it, nil when the
// strip draws some other line or none.
func updateOf(status *frontendv1.FooterStatus) *frontendv1.FooterStatusActivityUpdate {
	switch arm := status.GetStatus().(type) {
	case *frontendv1.FooterStatus_Idle:
		return arm.Idle.GetActivity().GetUpdate()
	case *frontendv1.FooterStatus_Working:
		return arm.Working.GetActivity().GetUpdate()
	case *frontendv1.FooterStatus_Waiting:
		return arm.Waiting.GetActivity().GetUpdate()
	case *frontendv1.FooterStatus_Background:
		return arm.Background.GetActivity().GetUpdate()
	case *frontendv1.FooterStatus_Blocked:
		return arm.Blocked.GetActivity().GetUpdate()
	case *frontendv1.FooterStatus_Disconnected:
		return arm.Disconnected.GetActivity().GetUpdate()
	default:
		return nil
	}
}

// updatePhase names the drawn update line's phase arm, "" when none stands.
func updatePhase(status *frontendv1.FooterStatus) string {
	switch updateOf(status).GetPhase().(type) {
	case *frontendv1.FooterStatusActivityUpdate_Building:
		return "building"
	case *frontendv1.FooterStatusActivityUpdate_Installing:
		return "installing"
	case *frontendv1.FooterStatusActivityUpdate_RestartingServices:
		return "restarting_services"
	case *frontendv1.FooterStatusActivityUpdate_HandingOver:
		return "handing_over"
	case *frontendv1.FooterStatusActivityUpdate_Waiting:
		return "waiting"
	case *frontendv1.FooterStatusActivityUpdate_Updated:
		return "updated"
	default:
		return ""
	}
}

func TestEachDeployPhaseDrawsItsOwnArm(t *testing.T) {
	tests := []struct {
		name  string
		phase deployprogress.Phase
		want  string
	}{
		{"the build", deployprogress.Building, "building"},
		{"the install", deployprogress.Installing, "installing"},
		{"the service restarts", deployprogress.RestartingServices, "restarting_services"},
		{"the handover", deployprogress.HandingOver, "handing_over"},
		{"the end", deployprogress.Updated, "updated"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.SetDeployProgress(&deployprogress.Progress{Phase: tt.phase})

			// Assert
			if got := updatePhase(h.view(t).GetStrip().GetStatus()); got != tt.want {
				t.Fatalf("update phase = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestTheUpdateLineStandsOnEveryWorkspacesStrip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	other := ids.WorkspaceID("ws-2")
	if err := h.r.SetWorkspaceDir(other, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}
	h.r.Prime(other)

	// Act
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.Installing})

	// Assert
	for _, ws := range []ids.WorkspaceID{testWS, other} {
		view, ok := h.r.Topic(ws).Latest()
		if !ok || updatePhase(view.GetStrip().GetStatus()) != "installing" {
			t.Fatalf("workspace %s drew %+v, want the installing line", ws, view.GetStrip().GetStatus())
		}
	}
}

func TestTheBuildNamesEveryComponentItBuilds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	components := []deployprogress.Component{
		deployprogress.Shim, deployprogress.Webapp, deployprogress.Daemon,
		deployprogress.Store, deployprogress.Sidecar,
	}

	// Act
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.Building, Components: components})

	// Assert
	drawn := updateOf(h.view(t).GetStrip().GetStatus()).GetBuilding().GetComponents()
	if len(drawn) != len(components) {
		t.Fatalf("components = %d, want %d", len(drawn), len(components))
	}
	for i, c := range drawn {
		if c.GetComponent() == nil {
			t.Fatalf("component %d (%s) drew no arm", i, components[i])
		}
	}
}

func TestTheServiceRestartNamesTheServices(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetDeployProgress(&deployprogress.Progress{
		Phase:      deployprogress.RestartingServices,
		Components: []deployprogress.Component{deployprogress.Store, deployprogress.Sidecar},
	})

	// Assert
	services := updateOf(h.view(t).GetStrip().GetStatus()).GetRestartingServices().GetServices()
	if len(services) != 2 || services[0].GetStore() == nil || services[1].GetSidecar() == nil {
		t.Fatalf("services = %+v, want the store then the sidecar", services)
	}
}

func TestABusyWorkspaceUnderADrainingMoveDrawsWaitingWithItsCounts(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{"agent-1"}, []string{"shell-1"}, nil))

	// Act
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.HandingOver, Draining: true})

	// Assert
	waiting := updateOf(h.view(t).GetStrip().GetStatus()).GetWaiting()
	if waiting == nil || waiting.GetTurns() != 1 || waiting.GetBackground() != 2 {
		t.Fatalf("waiting = %+v, want one turn and two background items", waiting)
	}
}

func TestTheWaitingCountsFallAsTheWorkDrains(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-1", "shell-2"}, nil))
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.HandingOver, Draining: true})

	// Act
	h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-2"}, nil))

	// Assert
	waiting := updateOf(h.view(t).GetStrip().GetStatus()).GetWaiting()
	if waiting.GetBackground() != 1 || waiting.GetTurns() != 0 {
		t.Fatalf("waiting = %+v, want the one shell still running", waiting)
	}
}

func TestADrainedWorkspaceDrawsTheHandoverAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-1"}, nil))
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.HandingOver, Draining: true})

	// Act
	h.r.OnLiveWorkChanged(testWS, LiveWorkSet{})

	// Assert
	if got := updatePhase(h.view(t).GetStrip().GetStatus()); got != "handing_over" {
		t.Fatalf("update phase = %q, want handing_over once nothing is left to wait on", got)
	}
}

func TestAForcedMoveNeverDrawsWaiting(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.HandingOver})

	// Assert
	if got := updatePhase(h.view(t).GetStrip().GetStatus()); got != "handing_over" {
		t.Fatalf("update phase = %q, want handing_over: a forced move waits on nothing", got)
	}
}

func TestTheNotesAreThisWorkspacesOwn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	other := ids.WorkspaceID("ws-2")
	if err := h.r.SetWorkspaceDir(other, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}
	h.r.Prime(other)

	// Act
	h.r.SetDeployProgress(&deployprogress.Progress{
		Phase: deployprogress.Updated,
		Notes: map[ids.WorkspaceID][]deployprogress.Note{other: {deployprogress.ShimWhenIdle}},
	})

	// Assert
	mine := updateOf(h.view(t).GetStrip().GetStatus()).GetNotes()
	view, _ := h.r.Topic(other).Latest()
	theirs := updateOf(view.GetStrip().GetStatus()).GetNotes()
	if len(mine) != 0 || len(theirs) != 1 || theirs[0].GetShimWhenIdle() == nil {
		t.Fatalf("notes = %+v here and %+v there, want shim_when_idle on the other workspace alone", mine, theirs)
	}
}

func TestTheUpdatedLineIsRetiredByTheDwell(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.Updated})

	// Act
	h.clock.Advance(DefaultMomentaryDwell)

	// Assert
	if got := updatePhase(h.view(t).GetStrip().GetStatus()); got != "" {
		t.Fatalf("update phase = %q, want the updated line retired", got)
	}
}

func TestTheUpdatedLineIsNotRetiredEarly(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.Updated})

	// Act
	h.clock.Advance(DefaultMomentaryDwell / 2)

	// Assert
	if got := updatePhase(h.view(t).GetStrip().GetStatus()); got != "updated" {
		t.Fatalf("update phase = %q, want updated still standing inside its dwell", got)
	}
}

func TestAnEarlierUpdatedDwellDoesNotRetireALaterDeploy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.Updated})
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.Building})

	// Act
	h.clock.Advance(DefaultMomentaryDwell)

	// Assert
	if got := updatePhase(h.view(t).GetStrip().GetStatus()); got != "building" {
		t.Fatalf("update phase = %q, want the later deploy's building line", got)
	}
}

func TestANilProgressClearsTheLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.HandingOver})

	// Act
	h.r.SetDeployProgress(nil)

	// Assert
	if got := updatePhase(h.view(t).GetStrip().GetStatus()); got != "" {
		t.Fatalf("update phase = %q, want no update line", got)
	}
}

func TestAProgressWithNoPhaseIsRefusedAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetDeployProgress(&deployprogress.Progress{})

	// Assert
	if got := updatePhase(h.view(t).GetStrip().GetStatus()); got != "" {
		t.Fatalf("update phase = %q, want nothing drawn", got)
	}
	if !hasLevel(h.log.Records(), "error", opDeployProgress) {
		t.Fatalf("no ERROR %s was recorded", opDeployProgress)
	}
}

func TestEveryUpdateLineChangeIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.Installing})

	// Assert
	for _, rec := range h.log.Records() {
		if rec.Operation == "daemon.footer.activity_line_changed" && rec.Level == "info" &&
			rec.Context["kind"] == "update" && rec.Context["cause"] == opDeployProgress {
			return
		}
	}
	t.Fatalf("no INFO activity_line_changed record named the update line")
}

func TestAFaultOutranksTheUpdateLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.HandingOver})

	// Act
	h.r.OpenFault(testWS, faultOf(t, "fault-1", health.KindConversationAbandoned, false))

	// Assert
	if h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetFault() == nil {
		t.Fatalf("activity = %+v, want the fault over the update line",
			h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetKind())
	}
}

func TestAStandingDeployFaultStandsAboveTheNextDeploysUpdateLine(t *testing.T) {
	// Arrange: an earlier deploy's failure stands on every strip.
	h := newHarness(t)
	connected(h)
	h.r.OpenFault("", faultOf(t, "fault-1", health.KindDeployFailed, true))

	// Act: the next deploy starts building.
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.Building})

	// Assert: the fault keeps the activity line, and the status stays idle.
	idle := h.view(t).GetStrip().GetStatus().GetIdle()
	if idle == nil {
		t.Fatalf("status = %+v, want idle: a failed deploy does not escalate", h.view(t).GetStrip().GetStatus())
	}
	if got := idle.GetActivity().GetFault().GetKind(); got != health.KindDeployFailed {
		t.Fatalf("activity = %+v, want the deploy_failed fault over the update line", idle.GetActivity().GetKind())
	}
}

func TestTheUpdateLineOutranksANotification(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, notificationFrame("the agent needs you"))

	// Act
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.Building})

	// Assert
	if got := updatePhase(h.view(t).GetStrip().GetStatus()); got != "building" {
		t.Fatalf("update phase = %q, want the update line over the notification", got)
	}
}

func TestTheUpdateLineRidesEveryStatusArm(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		arm     string
	}{
		{"idle", func(h *harness) {}, "idle"},
		{"a turn in flight", func(h *harness) { h.r.SetTurn(testWS, &TurnStarted{At: instant}) }, "working"},
		{"a consent ask", func(h *harness) { h.r.OnPermission(testWS, mainAgent, permissionStart("p-1", "rm -rf")) }, "waiting"},
		{"detached work", func(h *harness) { h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-1"}, nil)) }, "background"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			tt.arrange(h)

			// Act
			h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.Installing})

			// Assert
			status := h.view(t).GetStrip().GetStatus()
			if statusName(status) != tt.arm || updatePhase(status) != "installing" {
				t.Fatalf("status %q drew %q, want %q with the installing line", statusName(status), updatePhase(status), tt.arm)
			}
		})
	}
}

func TestEveryComponentDrawsAnArm(t *testing.T) {
	for _, c := range []deployprogress.Component{
		deployprogress.Store, deployprogress.Sidecar, deployprogress.Daemon,
		deployprogress.Shim, deployprogress.Webapp,
	} {
		t.Run(string(c), func(t *testing.T) {
			// Arrange
			component := c

			// Act
			drawn := updateComponent(component)

			// Assert
			if drawn.GetComponent() == nil {
				t.Fatalf("component %q drew no arm", component)
			}
		})
	}
}
