package rollout

import (
	"context"
	"reflect"
	"testing"
	"time"

	"claude-repld/internal/gitclient"
)

// runTriggerThroughHandover starts Trigger, waits for every workspace's
// adoption window and expires it, so a handover-shaped trigger reaches its
// exit without anything being waited out.
func runTriggerThroughHandover(t *testing.T, h *harness, workspaces int) error {
	t.Helper()
	done := make(chan error, 1)
	go func() { done <- h.c.Trigger(context.Background(), landed()) }()
	for range workspaces {
		h.clock.awaitArmed(t, adoptionWindow)
	}
	h.clock.Fire(adoptionWindow)
	select {
	case err := <-done:
		return err
	case <-time.After(10 * time.Second):
		t.Fatalf("Trigger never returned")
		return nil
	}
}

// landed is the two-commit range every trigger test classifies.
func landed() []gitclient.Commit {
	return []gitclient.Commit{{SHA: "aaa111"}, {SHA: "bbb222"}}
}

func TestTriggerInvokesTheOneDeployChainExactlyOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.git.paths = []string{ModuleRoot + "webapp/src/App.tsx"}

	// Act
	if err := h.c.Trigger(context.Background(), landed()); err != nil {
		t.Fatalf("Trigger: %v", err)
	}

	// Assert
	runs := h.runner.Runs()
	if len(runs) != 1 {
		t.Fatalf("deploy runs = %d, want exactly 1", len(runs))
	}
	if !reflect.DeepEqual(runs[0], []string{"bin/deploy-all.sh", DeployNoBounce}) {
		t.Fatalf("deploy argv = %v, want the one chain with --no-bounce", runs[0])
	}
}

func TestTriggerClassifiesTheWholeLandedRange(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.git.paths = []string{ModuleRoot + "webapp/src/App.tsx"}

	// Act
	if err := h.c.Trigger(context.Background(), landed()); err != nil {
		t.Fatalf("Trigger: %v", err)
	}

	// Assert
	ranges := h.git.Ranges()
	if len(ranges) != 1 || ranges[0] != "aaa111^..bbb222" {
		t.Fatalf("changed-path ranges = %v, want the whole landed range", ranges)
	}
}

func TestADaemonChangeHandsOver(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.git.paths = []string{ModuleRoot + "daemon/internal/server/api.go"}

	// Act
	if err := runTriggerThroughHandover(t, h, 1); err != nil {
		t.Fatalf("Trigger: %v", err)
	}

	// Assert
	if len(h.announcer.Sent()) != 1 {
		t.Fatalf("shutdown announcements = %d, want the handover's one", len(h.announcer.Sent()))
	}
}

func TestACombinedDaemonAndWebappChangeHandsOverAndPushesNoReload(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.participants.Set(ws, Participants{Host: true, Web: true})
	h.git.paths = []string{
		ModuleRoot + "daemon/internal/server/api.go",
		ModuleRoot + "webapp/src/App.tsx",
	}

	// Act
	if err := runTriggerThroughHandover(t, h, 1); err != nil {
		t.Fatalf("Trigger: %v", err)
	}

	// Assert
	for _, call := range h.pusher.Calls() {
		if call.Kind == "reload_webapp" {
			t.Fatalf("a combined rollout pushed reload_webapp; the handover's fresh attach is the whole recovery")
		}
	}
}

func TestAShimChangeRelaunchesEveryLiveWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	first, _ := h.workspace(t)
	second, _ := h.workspace(t)
	h.fleet.live[first].Reap()
	h.fleet.live[second].Reap()
	h.git.paths = []string{ModuleRoot + "agent-shim/claude/shim/src/main.ts"}

	// Act
	if err := h.c.Trigger(context.Background(), landed()); err != nil {
		t.Fatalf("Trigger: %v", err)
	}

	// Assert
	if len(h.fleet.resumes) != 2 {
		t.Fatalf("resumes = %d, want one per live workspace", len(h.fleet.resumes))
	}
}

func TestAShimChangeLeavesAParkedWorkspaceAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	delete(h.fleet.live, ws)
	h.git.paths = []string{ModuleRoot + "agent-shim/claude/shim/src/main.ts"}

	// Act
	if err := h.c.Trigger(context.Background(), landed()); err != nil {
		t.Fatalf("Trigger: %v", err)
	}

	// Assert
	if len(h.fleet.resumes) != 0 {
		t.Fatalf("resumes = %d, want none: the next implicit revival spawns the new binary", len(h.fleet.resumes))
	}
}

func TestAWebappChangePushesTheReloadToEveryOpenWebview(t *testing.T) {
	// Arrange
	h := newHarness(t)
	withWeb, _ := h.workspace(t)
	withoutWeb, _ := h.workspace(t)
	h.participants.Set(withWeb, Participants{Web: true})
	h.participants.Set(withoutWeb, Participants{Host: true})
	h.git.paths = []string{ModuleRoot + "webapp/src/App.tsx"}

	// Act
	if err := h.c.Trigger(context.Background(), landed()); err != nil {
		t.Fatalf("Trigger: %v", err)
	}

	// Assert
	calls := h.pusher.Calls()
	if len(calls) != 1 || calls[0].WS != withWeb || calls[0].Kind != "reload_webapp" {
		t.Fatalf("pushes = %+v, want one reload_webapp for the workspace with a webview", calls)
	}
}

func TestAnElispChangeIsLoggedAndNothingElse(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.git.paths = []string{ModuleRoot + "lisp/services.el"}

	// Act
	if err := h.c.Trigger(context.Background(), landed()); err != nil {
		t.Fatalf("Trigger: %v", err)
	}

	// Assert
	if len(h.announcer.Sent()) != 0 || len(h.pusher.Calls()) != 0 || len(h.fleet.resumes) != 0 {
		t.Fatalf("an elisp change acted on the stack; Emacs hot-loads its own elisp")
	}
}

func TestAnUnhandledSubsystemIsNamedInAWarning(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.git.paths = []string{
		ModuleRoot + "agent-shim/shim-store/main.go",
		ModuleRoot + "agent-shim/claude/shim-sidecar/main.go",
	}

	// Act
	if err := h.c.Trigger(context.Background(), landed()); err != nil {
		t.Fatalf("Trigger: %v", err)
	}

	// Assert
	warns := levelRecords(records(h.log, opClassify), "warn")
	if len(warns) != 1 {
		t.Fatalf("classification warnings = %d, want one naming the unhandled subsystems", len(warns))
	}
	named, ok := warns[0].Context["unhandled"].([]string)
	if !ok || !reflect.DeepEqual(named, []string{"sidecar", "store"}) {
		t.Fatalf("unhandled = %v, want both named", warns[0].Context["unhandled"])
	}
}

func TestAFailedDeployChainStopsTheRollout(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.runner.code = 3
	h.git.paths = []string{ModuleRoot + "daemon/internal/server/api.go"}

	// Act
	err := h.c.Trigger(context.Background(), landed())

	// Assert
	if err == nil {
		t.Fatalf("Trigger succeeded on a failed deploy chain")
	}
	if len(h.announcer.Sent()) != 0 {
		t.Fatalf("a handover was announced after the deploy chain failed")
	}
}

func TestTriggerDoesNothingForAnEmptyLandedRange(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := h.c.Trigger(context.Background(), nil); err != nil {
		t.Fatalf("Trigger: %v", err)
	}

	// Assert
	if len(h.runner.Runs()) != 0 {
		t.Fatalf("the deploy chain ran for an empty landed range")
	}
}

func TestTriggerSkipsTheDeployWhenNoDeployableSubsystemChanged(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.git.paths = []string{"README.md"}

	// Act
	if err := h.c.Trigger(context.Background(), landed()); err != nil {
		t.Fatalf("Trigger: %v", err)
	}

	// Assert
	if len(h.runner.Runs()) != 0 {
		t.Fatalf("the deploy chain ran for a range that touched no deployable subsystem")
	}
}

func TestTheDeployScriptEnvironmentOverrideBeatsTheWiredScript(t *testing.T) {
	// Arrange
	t.Setenv(DeployScriptEnv, "/tmp/fake-deploy.sh")

	// Act
	got := ResolveDeployScript("bin/deploy-all.sh")

	// Assert
	if got != "/tmp/fake-deploy.sh" {
		t.Fatalf("deploy script = %q, want the environment's override", got)
	}
}

func TestTheWiredDeployScriptIsUsedWithNoOverride(t *testing.T) {
	// Arrange
	t.Setenv(DeployScriptEnv, "")

	// Act
	got := ResolveDeployScript("bin/deploy-all.sh")

	// Assert
	if got != "bin/deploy-all.sh" {
		t.Fatalf("deploy script = %q, want the wired one", got)
	}
}

func TestTheDefaultDeployScriptIsTheOneChain(t *testing.T) {
	// Arrange
	t.Setenv(DeployScriptEnv, "")

	// Act
	got := ResolveDeployScript("")

	// Assert
	if got != DefaultDeployScript {
		t.Fatalf("deploy script = %q, want %q", got, DefaultDeployScript)
	}
}
