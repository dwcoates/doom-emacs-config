package deploy

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agentrepl/logging/buildreport"
	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// failInstall makes the fresh daemon uninstallable: its live directory is
// read-only.
func failInstall(t *testing.T, h *harness) {
	t.Helper()
	dir := filepath.Dir(h.live.DaemonBin())
	if err := os.Chmod(dir, 0o555); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.Chmod(dir, 0o755) })
}

// deployFault seeds a standing `deploy_failed` fault of one step.
func deployFault(t *testing.T, h *harness, step string) ids.FaultID {
	t.Helper()
	failure := health.DeployFailure{Step: step, BuildStep: "webapp", Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE, Detail: "earlier"}
	return h.faults.seed(t, wsm.Fault{Kind: health.KindDeployFailed, Evidence: failure.Evidence()})
}

// standingSteps names the step of every standing `deploy_failed` fault.
func standingSteps(t *testing.T, h *harness) []string {
	t.Helper()
	var steps []string
	for _, f := range h.faults.standing(t) {
		if f.Kind == health.KindDeployFailed {
			steps = append(steps, health.DeployFailureOf(f).Step)
		}
	}
	return steps
}

func TestEachFailingStepOpensTheFaultNamingIt(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		want    health.DeployFailure
	}{
		{"a failed build names its step, its output and its log", func(t *testing.T, h *harness) {
			h.builder.fail = &BuildFailed{Step: "webapp", Detail: "tsc: 3 errors", Log: "/logs/build.log"}
		}, health.DeployFailure{Step: health.DeployStepBuild, BuildStep: "webapp", Detail: "tsc: 3 errors", Log: "/logs/build.log"}},
		{"a build that staged nothing names the artifact it could not hash", func(t *testing.T, h *harness) {
			h.builder.build = artifacts{}
			h.builder.stageNothing = true
		}, health.DeployFailure{Step: health.DeployStepBuild, BuildStep: "shim"}},
		{"a failed install names the component", func(t *testing.T, h *harness) {
			failInstall(t, h)
		}, health.DeployFailure{Step: health.DeployStepInstall, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON}},
		{"a failed store restart names the store", func(t *testing.T, h *harness) {
			h.report(t, buildreport.ServiceStore, 101, hashOf(t, theOld.store))
			h.services.storeErr = errors.New("launchctl refused")
			// The restart fails once, so the rollback's own restart restores the
			// previous build and the step's fault stands alone.
			h.services.recovers = true
		}, health.DeployFailure{Step: health.DeployStepRestartServices, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE, Detail: "launchctl refused"}},
		{"a failed sidecar restart names the sidecar", func(t *testing.T, h *harness) {
			h.report(t, buildreport.ServiceSidecar, 102, hashOf(t, theOld.sidecar))
			h.services.sidecarErr = errors.New("bootstrap refused")
			// The restart fails once, so the rollback's own restart restores the
			// previous build and the step's fault stands alone.
			h.services.recovers = true
		}, health.DeployFailure{Step: health.DeployStepRestartServices, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_SIDECAR, Detail: "bootstrap refused"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.arrange(t, h)

			// Act
			_, err := h.d.Deploy(context.Background(), false)

			// Assert
			if err == nil {
				t.Fatalf("Deploy succeeded, want the failure")
			}
			standing := h.faults.standing(t)
			if len(standing) != 1 || standing[0].Kind != health.KindDeployFailed || standing[0].Workspace != nil {
				t.Fatalf("standing faults = %+v, want one daemon-scoped deploy_failed", standing)
			}
			got := health.DeployFailureOf(standing[0])
			if got.Step != tc.want.Step || got.BuildStep != tc.want.BuildStep || got.Component != tc.want.Component || got.Log != tc.want.Log {
				t.Fatalf("recorded failure = %+v, want %+v", got, tc.want)
			}
			if tc.want.Detail != "" && got.Detail != tc.want.Detail {
				t.Fatalf("recorded detail = %q, want %q", got.Detail, tc.want.Detail)
			}
			if !logged(h.log, "info", opFault, "recorded the failed deploy as a fault") {
				t.Fatalf("records = %+v, want the open at INFO", h.log.Records())
			}
		})
	}
}

// opHealthCloseOnEdge is the operation the health package records every
// recovery-edge close under: a deploy fault leaves by the same one door every
// fault does (health/faultclose.go), so its close is recorded there.
const opHealthCloseOnEdge = "daemon.health.close_on_edge"

func TestALaterDeployThatGetsThroughTheStepClosesItsFault(t *testing.T) {
	tests := []struct {
		name string
		step string
	}{
		{"the build", health.DeployStepBuild},
		{"the install", health.DeployStepInstall},
		{"the service restart", health.DeployStepRestartServices},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			id := deployFault(t, h, tc.step)

			// Act
			if _, err := h.d.Deploy(context.Background(), false); err != nil {
				t.Fatalf("Deploy: %v", err)
			}

			// Assert
			if !h.faults.closed[id] {
				t.Fatalf("the %s fault still stands after a deploy got through it", tc.step)
			}
			if !logged(h.log, "info", opHealthCloseOnEdge, "a recovery edge closed a standing fault") {
				t.Fatalf("records = %+v, want the close at INFO", h.log.Records())
			}
		})
	}
}

func TestAStepTheDeployNeverReachedKeepsItsFault(t *testing.T) {
	// Arrange: an earlier install failed, and this deploy fails to build.
	h := newHarness(t)
	id := deployFault(t, h, health.DeployStepInstall)
	h.builder.fail = &BuildFailed{Step: "proto", Detail: "protoc"}

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatalf("Deploy succeeded, want the build failure")
	}
	if h.faults.closed[id] {
		t.Fatalf("the install fault was closed by a deploy that never installed")
	}
	if got := standingSteps(t, h); len(got) != 2 {
		t.Fatalf("standing steps = %v, want the install's and the build's", got)
	}
}

func TestAStepThatFailsAgainSupersedesItsOwnFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	earlier := deployFault(t, h, health.DeployStepBuild)
	h.builder.fail = &BuildFailed{Step: "webapp", Detail: "tsc"}

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatalf("Deploy succeeded, want the build failure")
	}
	if !h.faults.closed[earlier] {
		t.Fatalf("the earlier build fault still stands beside the new one")
	}
	if got := standingSteps(t, h); len(got) != 1 {
		t.Fatalf("standing steps = %v, want the one newest build fault", got)
	}
}

func TestAFailureThatIsNoStepOpensNoFault(t *testing.T) {
	// Arrange: the handover is refused after every step got through.
	h := newHarness(t)
	h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
	h.rollout.handErr = errors.New("the successor never proved it was serving")

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatalf("Deploy succeeded, want the refused handover")
	}
	if got := h.faults.standing(t); len(got) != 0 {
		t.Fatalf("standing faults = %+v, want none: a refused handover is the rollout's fault", got)
	}
}

func TestAFailedDeployWhoseFaultCannotBeRecordedSaysSoAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.builder.fail = &BuildFailed{Step: "webapp", Detail: "tsc"}
	h.faults.openErr = errors.New("state client closed")

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	var failed *BuildFailed
	if !errors.As(err, &failed) {
		t.Fatalf("Deploy = %v, want the build failure still the answer", err)
	}
	if !logged(h.log, "error", opFault, "could not record the failed deploy as a fault") {
		t.Fatalf("records = %+v, want the unrecorded fault at ERROR", h.log.Records())
	}
}

func TestAStandingFaultThatCannotBeReadSaysSoAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.faults.readErr = errors.New("state client closed")

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	if !logged(h.log, "error", opHealthCloseOnEdge, "could not read the standing faults a recovery edge closes; they stand") {
		t.Fatalf("records = %+v, want the unreadable faults at ERROR", h.log.Records())
	}
}

func TestAStandingFaultThatCannotBeClosedSaysSoAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id := deployFault(t, h, health.DeployStepBuild)
	h.faults.closeErr = errors.New("state client closed")

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	if h.faults.closed[id] {
		t.Fatalf("a close that failed closed the fault")
	}
	if !logged(h.log, "error", opHealthCloseOnEdge, "could not close a standing fault on its recovery edge; it stands") {
		t.Fatalf("records = %+v, want the failed close at ERROR", h.log.Records())
	}
}

func TestABootingDaemonClosesEveryDeployFaultAnEarlierOneLeft(t *testing.T) {
	// Arrange
	h := newHarness(t)
	build := deployFault(t, h, health.DeployStepBuild)
	install := deployFault(t, h, health.DeployStepInstall)
	other := h.faults.seed(t, wsm.Fault{Kind: health.KindSuccessorSpawnFailed})

	// Act
	h.d.CloseEarlierFailures(context.Background())

	// Assert
	if !h.faults.closed[build] || !h.faults.closed[install] {
		t.Fatalf("closed = %v, want both deploy faults closed", h.faults.closed)
	}
	if h.faults.closed[other] {
		t.Fatalf("a fault of another kind was closed")
	}
}

// THE BOOT IS A RECOVERY EDGE of every daemon-scoped kind whose lifetime ends
// there (health/lifetime.go), not of the deploy's faults alone.
func TestABootingDaemonClosesEveryFaultWhoseLifetimeEndsAtABoot(t *testing.T) {
	tests := []struct {
		name   string
		kind   string
		closes bool
	}{
		{"an earlier process's poisoned sink", health.KindLogSinkPoisoned, true},
		{"an earlier process's read-only handle", health.KindWsmReadOnly, true},
		{"a missing prompts directory waits for a served brief", health.KindPromptsDirMissing, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			id := h.faults.seed(t, wsm.Fault{Kind: tt.kind})

			// Act
			h.d.CloseEarlierFailures(context.Background())

			// Assert
			if h.faults.closed[id] != tt.closes {
				t.Fatalf("%s closed = %v, want %v", tt.kind, h.faults.closed[id], tt.closes)
			}
		})
	}
}

func TestAWorkspaceScopedDeployFaultIsNotTheDeploys(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := ids.WorkspaceID("ws-a")
	id := h.faults.seed(t, wsm.Fault{Kind: health.KindDeployFailed, Workspace: &ws,
		Evidence: health.DeployFailure{Step: health.DeployStepBuild}.Evidence()})

	// Act
	h.d.CloseEarlierFailures(context.Background())

	// Assert
	if h.faults.closed[id] {
		t.Fatalf("a workspace-scoped fault was closed as the deploy's")
	}
}

func TestNewRefusesAMissingFaultRecorder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := h.d.deps
	deps.Faults = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil {
		t.Fatalf("New accepted a Deployer with no fault recorder")
	}
}

func TestAFailedStepsFaultSaysWhatBecameOfTheInstall(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		want    string
		clause  string
	}{
		{"a failed build installed nothing", func(t *testing.T, h *harness) {
			h.builder.fail = &BuildFailed{Step: "webapp", Detail: "tsc"}
		}, health.RollbackNone, "nothing was installed"},
		{"a failed install was rolled back", func(t *testing.T, h *harness) {
			failInstall(t, h)
		}, health.RollbackRestored, "it was rolled back to the previous build"},
		{"a failed restart was rolled back", func(t *testing.T, h *harness) {
			h.report(t, buildreport.ServiceSidecar, 102, hashOf(t, theOld.sidecar))
			h.services.sidecarErr = errors.New("kickstart refused")
			h.services.recovers = true
		}, health.RollbackRestored, "it was rolled back to the previous build"},
		{"a failed restart whose rollback failed", func(t *testing.T, h *harness) {
			h.report(t, buildreport.ServiceSidecar, 102, hashOf(t, theOld.sidecar))
			h.services.sidecarErr = errors.New("kickstart refused")
		}, health.RollbackIncomplete, "its rollback did NOT restore the previous build"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.arrange(t, h)

			// Act
			_, err := h.d.Deploy(context.Background(), false)

			// Assert
			if err == nil {
				t.Fatalf("Deploy succeeded, want the failure")
			}
			fault, ok := standingOfStep(t, h, "", health.DeployStepRollback)
			if !ok {
				t.Fatalf("standing faults = %+v, want the step's fault", h.faults.standing(t))
			}
			if got := health.DeployFailureOf(fault).Rollback; got != tc.want {
				t.Fatalf("rollback = %q, want %q", got, tc.want)
			}
			if !strings.Contains(fault.Detail, tc.clause) {
				t.Fatalf("fault detail = %q, want it to say %q", fault.Detail, tc.clause)
			}
		})
	}
}

// standingOfStep answers the one standing `deploy_failed` fault of step, or
// of any step but notStep when step is empty.
func standingOfStep(t *testing.T, h *harness, step, notStep string) (wsm.Fault, bool) {
	t.Helper()
	for _, f := range h.faults.standing(t) {
		got := health.DeployFailureOf(f).Step
		if f.Kind == health.KindDeployFailed && (got == step || (step == "" && got != notStep)) {
			return f, true
		}
	}
	return wsm.Fault{}, false
}

func TestARollbackThatFailsOpensItsOwnFault(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(t *testing.T, h *harness)
		component agentreplv1.DeployComponent
	}{
		{"an artifact that cannot be restored", func(t *testing.T, h *harness) {
			staleDaemon(t, h)
			h.rollout.handErr = errors.New("a handover is in flight")
			h.rollout.onHandOver = func() { failInstall(t, h) }
		}, agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON},
		{"a service that will not restart onto the restored build", func(t *testing.T, h *harness) {
			h.report(t, buildreport.ServiceStore, 101, hashOf(t, theOld.store))
			h.services.storeErr = errors.New("launchctl refused")
		}, agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.arrange(t, h)

			// Act
			_, err := h.d.Deploy(context.Background(), false)

			// Assert
			if err == nil {
				t.Fatalf("Deploy succeeded, want the failure")
			}
			fault, ok := standingOfStep(t, h, health.DeployStepRollback, "")
			if !ok || fault.Workspace != nil {
				t.Fatalf("standing faults = %+v, want a daemon-scoped rollback fault", h.faults.standing(t))
			}
			if got := health.DeployFailureOf(fault).Component; got != tc.component {
				t.Fatalf("rollback fault component = %v, want %v", got, tc.component)
			}
		})
	}
}

func TestARefusedHandoverWhoseRollbackFailedStandsAsTheRollbackAlone(t *testing.T) {
	// Arrange: a refused handover is the rollout's to say; its rollback is ours.
	h := newHarness(t)
	staleDaemon(t, h)
	h.rollout.handErr = errors.New("a handover is in flight")
	h.rollout.onHandOver = func() { failInstall(t, h) }

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatalf("Deploy succeeded, want the refused handover")
	}
	if got := standingSteps(t, h); len(got) != 1 || got[0] != health.DeployStepRollback {
		t.Fatalf("standing steps = %v, want the rollback's alone", got)
	}
}

func TestADeployThatGetsAllTheWayThroughClosesTheRollbackFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id := deployFault(t, h, health.DeployStepRollback)

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	if !h.faults.closed[id] {
		t.Fatalf("the rollback fault still stands after a deploy got all the way through")
	}
}

func TestADeployThatFailsAgainKeepsTheRollbackFault(t *testing.T) {
	// Arrange: an earlier rollback failed, and this deploy's build fails.
	h := newHarness(t)
	id := deployFault(t, h, health.DeployStepRollback)
	h.builder.fail = &BuildFailed{Step: "webapp", Detail: "tsc"}

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatalf("Deploy succeeded, want the build failure")
	}
	if h.faults.closed[id] {
		t.Fatalf("the rollback fault was closed by a deploy that installed nothing")
	}
}
