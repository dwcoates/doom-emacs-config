package stack

import (
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/integration/harness"
)

func TestALifecycleFamilyIsOneFamilyPerName(t *testing.T) {
	// Act
	first, again, other := familyNamed("unit-deploy"), familyNamed("unit-deploy"), familyNamed("unit-stop")

	// Assert
	if first != again {
		t.Fatal("asking for one family twice answered two families; want the same one")
	}
	if first == other {
		t.Fatal("two families answered the same one; want one per name")
	}
}

func TestEachLifecycleStepGetsItsOwnStateRootLockingInTheFamilysLockDir(t *testing.T) {
	// Arrange
	parent := t.TempDir()
	svc := &lifecycleServices{parent: parent, lockDir: harness.LockDirFor(filepath.Join(parent, "s0"))}

	// Act
	first, second := svc.nextStateRoot(t), svc.nextStateRoot(t)

	// Assert
	if first == second {
		t.Fatalf("two steps got one state root %s; want one each", first)
	}
	for _, root := range []string{first, second} {
		if got := harness.LockDirFor(root); got != svc.lockDir {
			t.Fatalf("step root %s locks in %s, want the family's %s", root, got, svc.lockDir)
		}
	}
}

func TestALifecycleStepThatNamesItsOwnStateRootIsRefused(t *testing.T) {
	// Act
	refused := lifecycleOptsAllowed(WorldOpts{DaemonOpts: harness.Opts{StateDir: "/elsewhere"}})
	allowed := lifecycleOptsAllowed(WorldOpts{DaemonOpts: harness.Opts{DurableState: true}})

	// Assert
	if refused == nil || !strings.Contains(refused.Error(), "state root is its family's") {
		t.Fatalf("lifecycleOptsAllowed(own state root) = %v, want it refused", refused)
	}
	if allowed != nil {
		t.Fatalf("lifecycleOptsAllowed(DurableState) = %v, want it allowed", allowed)
	}
}

func TestAStepsLifecycleFamilyIsItsScenario(t *testing.T) {
	tests := map[string]string{
		"TestLanding/ADeployOverARunningTurn": "TestLanding",
		"TestDaemonLifecycle/ColdGate/Pay":    "TestDaemonLifecycle",
		"TestHarness":                         "TestHarness",
	}
	for name, want := range tests {
		if got := scenarioOf(name); got != want {
			t.Errorf("scenarioOf(%q) = %q, want %q", name, got, want)
		}
	}
}
