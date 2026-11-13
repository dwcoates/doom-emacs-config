package kit

import "testing"

func TestWorldDaemonEnvLeavesTheCheckoutToTheHarness(t *testing.T) {
	t.Parallel()
	// Arrange: a caller that states nothing about the checkout.
	extra := []string{"AGENT_REPL_TEST_ALL_SCRIPT=/gate"}

	// Act.
	env := WorldDaemonEnv(extra, "/spool", "/shim-lock")

	// Assert: no checkout override rides the world's env, so the harness's
	// own pinned checkout stands and a deploy never installs into this
	// worktree.
	if got := ValueOf(env, CheckoutEnv); got != "" {
		t.Fatalf("WorldDaemonEnv sets %s=%q, want it left to harness.StartDaemon's pinned checkout", CheckoutEnv, got)
	}
}
