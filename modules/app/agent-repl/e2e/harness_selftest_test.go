package e2e

import (
	"os"
	"path/filepath"
	"testing"
)

// The harness's own unit tests: the two helpers whose silent misbehavior does
// not fail visibly, but as a whole suite of timeouts and no_session prompts.

func TestResolveBuildIdentityPrefersTheShimBuildStamp(t *testing.T) {
	// Arrange: a checkout whose shim bundle carries a build stamp, which is
	// what the daemon exports in preference to SHIM_BUILD_SHA.
	root := t.TempDir()
	writeShimStamp(t, root, "  stamped-sha\n")

	// Act.
	got, err := resolveBuildIdentity(root)

	// Assert.
	if err != nil {
		t.Fatalf("resolveBuildIdentity = error %v", err)
	}
	if got != "stamped-sha" {
		t.Fatalf("resolveBuildIdentity = %q, want the stamp's trimmed value", got)
	}
}

func TestResolveBuildIdentityFallsBackToTheFixedShaWithoutAStamp(t *testing.T) {
	// Arrange: a checkout that has never built the shim.
	root := t.TempDir()

	// Act.
	got, err := resolveBuildIdentity(root)

	// Assert.
	if err != nil {
		t.Fatalf("resolveBuildIdentity = error %v", err)
	}
	if got != shimBuildSHA {
		t.Fatalf("resolveBuildIdentity = %q, want %q", got, shimBuildSHA)
	}
}

func TestResolveBuildIdentityRejectsAnEmptyStamp(t *testing.T) {
	// Arrange: an empty stamp, which the daemon itself refuses to boot on.
	root := t.TempDir()
	writeShimStamp(t, root, "   \n")

	// Act.
	_, err := resolveBuildIdentity(root)

	// Assert.
	if err == nil {
		t.Fatalf("resolveBuildIdentity = nil error, want a refusal on an empty stamp")
	}
}

func TestBuildIdentityEnvNamesOneShaInBothRoles(t *testing.T) {
	// Arrange: the identity runSuite resolved for this run.
	env := buildIdentityEnv()

	// Act.
	reported, deployed := valueOf(env, "SHIM_BUILD_SHA"), valueOf(env, "AGENT_REPL_DEPLOY_STAMP")

	// Assert: the shim's reported identity and the daemon's deployed build
	// are one string, or the daemon bounces every shim it spawns.
	if reported == "" {
		t.Fatalf("buildIdentityEnv sets no SHIM_BUILD_SHA: %v", env)
	}
	if reported != deployed {
		t.Fatalf("SHIM_BUILD_SHA=%q but AGENT_REPL_DEPLOY_STAMP=%q, want one sha", reported, deployed)
	}
}

func TestBuildIdentityEnvPinsTheCheckout(t *testing.T) {
	// Arrange/Act.
	got := valueOf(buildIdentityEnv(), checkoutEnv)

	// Assert: the daemon reads its stamps from the worktree the harness read.
	if got != repo.repoDir {
		t.Fatalf("%s=%q, want this worktree's module root %q", checkoutEnv, got, repo.repoDir)
	}
}

func TestCheckBuildIdentityAgreesPassesForTheResolvedRun(t *testing.T) {
	// Arrange/Act/Assert: the invariant runSuite already enforced holds.
	if err := checkBuildIdentityAgrees(); err != nil {
		t.Fatalf("checkBuildIdentityAgrees = %v", err)
	}
}

func writeShimStamp(t *testing.T, root, content string) {
	t.Helper()
	dir := filepath.Join(root, "agent-shim", "claude", "shim", "dist")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("make the shim dist dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, ".built-sha"), []byte(content), 0o644); err != nil {
		t.Fatalf("write the shim build stamp: %v", err)
	}
}
