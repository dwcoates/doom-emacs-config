//go:build realtest

package realtest

import (
	"strings"
	"testing"
)

func TestBootstrapCountIsZeroWhenTheRegistryAlreadyHasEnough(t *testing.T) {
	// Arrange / Act
	got := rt4BootstrapCount(rt4MinimumWorkspaces)

	// Assert
	if got != 0 {
		t.Errorf("a registry that already holds the minimum needs no bootstrap; it asked for %d", got)
	}
}

func TestBootstrapCountIsZeroWhenTheRegistryHasMoreThanEnough(t *testing.T) {
	// Arrange / Act
	got := rt4BootstrapCount(rt4MinimumWorkspaces + 4)

	// Assert
	if got != 0 {
		t.Errorf("a registry above the minimum needs no bootstrap; it asked for %d", got)
	}
}

func TestBootstrapCountMakesUpTheOneMissingWorkspace(t *testing.T) {
	// Arrange: the 2026-09-12 registry — two open workspaces, three needed.
	open := rt4MinimumWorkspaces - 1

	// Act
	got := rt4BootstrapCount(open)

	// Assert
	if got != 1 {
		t.Errorf("a registry one short must bootstrap exactly one workspace; it asked for %d", got)
	}
}

func TestBootstrapCountMakesUpAnEmptyRegistry(t *testing.T) {
	// Arrange / Act
	got := rt4BootstrapCount(0)

	// Assert
	if got != rt4MinimumWorkspaces {
		t.Errorf("an empty registry must bootstrap the whole minimum (%d); it asked for %d",
			rt4MinimumWorkspaces, got)
	}
}

func TestBootstrapRepoNamesAreDistinct(t *testing.T) {
	// Arrange / Act
	first, second := rt4BootstrapRepoName(0), rt4BootstrapRepoName(1)

	// Assert
	if first == second {
		t.Errorf("two bootstrap repositories would share the path %q, so the second would register the "+
			"first again", first)
	}
}

func TestBootstrapRepoNameCountsFromOne(t *testing.T) {
	// Arrange / Act
	got := rt4BootstrapRepoName(0)

	// Assert
	if got != "rt4-bootstrap-1" {
		t.Errorf("the first bootstrap repository reads as number one to whoever finds it on disk; it is %q", got)
	}
}

func TestBootstrapGuardMessageNamesTheVendorGuard(t *testing.T) {
	// Arrange / Act
	message := rt4BootstrapGuardMessage("/tmp/run/rt4-bootstrap-1", "no record appeared")

	// Assert
	if !strings.Contains(message, "VENDOR GUARD") {
		t.Errorf("a failed bootstrap must name the vendor guard as the first suspect; it said %q", message)
	}
}

func TestBootstrapGuardMessageNamesTheFakeShimsHook(t *testing.T) {
	// Arrange / Act
	message := rt4BootstrapGuardMessage("/tmp/run/rt4-bootstrap-1", "no record appeared")

	// Assert
	if !strings.Contains(message, "AGENT_REPL_FAKE_SHIMS=1") {
		t.Errorf("a failed bootstrap must name the remedy the lead sets on the launch; it said %q", message)
	}
}

func TestBootstrapGuardMessageCarriesTheDirectoryAndTheCause(t *testing.T) {
	// Arrange
	const dir = "/tmp/run/rt4-bootstrap-2"
	const cause = "the daemon refused"

	// Act
	message := rt4BootstrapGuardMessage(dir, cause)

	// Assert
	if !strings.Contains(message, dir) || !strings.Contains(message, cause) {
		t.Errorf("a failed bootstrap must carry the directory it tried and what went wrong; it said %q", message)
	}
}
