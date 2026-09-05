package e2e

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// The harness's own unit tests: the two helpers whose silent misbehavior does
// not fail visibly, but as a whole suite of timeouts and no_session prompts.

func TestResolvedPathResolvesASymlinkedDirectory(t *testing.T) {
	t.Parallel()
	// Arrange: a directory reached through a symlink, exactly the shape a
	// macOS temp root has (/tmp -> /private/tmp).
	real := filepath.Join(t.TempDir(), "real")
	if err := os.Mkdir(real, 0o755); err != nil {
		t.Fatalf("make the real dir: %v", err)
	}
	link := filepath.Join(t.TempDir(), "link")
	if err := os.Symlink(real, link); err != nil {
		t.Fatalf("symlink: %v", err)
	}

	// Act.
	got, err := resolvedPath(link)

	// Assert: the resolved target, not the link.
	if err != nil {
		t.Fatalf("resolvedPath(%s) = error %v", link, err)
	}
	want, err := filepath.EvalSymlinks(real)
	if err != nil {
		t.Fatalf("resolve the real dir: %v", err)
	}
	if got != want {
		t.Fatalf("resolvedPath(%s) = %q, want %q", link, got, want)
	}
}

func TestResolvedPathLeavesAnUnsymlinkedDirectoryUnchanged(t *testing.T) {
	t.Parallel()
	// Arrange: an already-resolved directory.
	dir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("resolve the temp dir: %v", err)
	}

	// Act.
	got, err := resolvedPath(dir)

	// Assert.
	if err != nil {
		t.Fatalf("resolvedPath(%s) = error %v", dir, err)
	}
	if got != dir {
		t.Fatalf("resolvedPath(%s) = %q, want it unchanged", dir, got)
	}
}

func TestResolvedPathFailsLoudlyOnAMissingDirectory(t *testing.T) {
	t.Parallel()
	// Arrange: a path nothing created.
	missing := filepath.Join(t.TempDir(), "absent")

	// Act.
	_, err := resolvedPath(missing)

	// Assert: an error, never the path itself.
	if err == nil {
		t.Fatalf("resolvedPath(%s) = nil error, want a loud failure", missing)
	}
}

func TestResolveBuildIdentityPrefersTheShimBuildStamp(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
	// Arrange/Act.
	got := valueOf(buildIdentityEnv(), checkoutEnv)

	// Assert: the daemon reads its stamps from the worktree the harness read.
	if got != repo.repoDir {
		t.Fatalf("%s=%q, want this worktree's module root %q", checkoutEnv, got, repo.repoDir)
	}
}

func TestCheckBuildIdentityAgreesPassesForTheResolvedRun(t *testing.T) {
	t.Parallel()
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

// TestShortStateRootKeepsTheLockDirectoryOffTheSharedTempRoot pins the reason
// shortStateRoot nests its root one level down.
//
// harness.StartDaemon derives the kernel-lock directory as a SIBLING of the
// state root, so a state root minted directly under /tmp gives every world in
// the package — and every concurrent run, and every other checkout on the box
// — the one shared directory /tmp/locks, which nothing ever cleans. The lock
// directory must instead live inside the per-world root the test's own cleanup
// removes.
func TestShortStateRootKeepsTheLockDirectoryOffTheSharedTempRoot(t *testing.T) {
	t.Parallel()
	// Arrange + Act.
	stateRoot := shortStateRoot(t)

	// Assert: the sibling harness.StartDaemon would name is inside a
	// per-world directory, never the OS temp root every other run shares.
	lockDir := filepath.Join(filepath.Dir(stateRoot), "locks")
	if shared := filepath.Join(os.TempDir(), "locks"); lockDir == shared {
		t.Fatalf("the world's lock directory is %q, the temp root every run shares; it must be per-world", lockDir)
	}
	if parent := filepath.Dir(stateRoot); parent == os.TempDir() {
		t.Fatalf("shortStateRoot minted %q directly under the OS temp root; its lock sibling would be shared", stateRoot)
	}
}

// THE WEBAPP LAYER'S WRITABILITY PRECONDITION.
//
// The layer went red in eleven places at once, in the sandbox only, because
// vite's default cache directory sat inside a read-only `node_modules`. The
// cache moved out (webapp/vite-cache.ts); wlWebappWritable is what makes the
// NEXT such move say so once, by name, instead of eleven times in a node
// process. These two tests are its guarantee and its violation.

func TestWebappWritableAcceptsAWritablePackageDirectory(t *testing.T) {
	t.Parallel()
	// Arrange: an ordinary writable directory, as a checkout is.
	dir := t.TempDir()

	// Act.
	err := wlWebappWritable(dir)

	// Assert.
	if err != nil {
		t.Fatalf("wlWebappWritable(%s) = %v, want nil", dir, err)
	}
}

func TestWebappWritableRejectsADirectoryItCannotCreateTheCacheIn(t *testing.T) {
	t.Parallel()
	// root writes through a mode bit, so the read-only case cannot be staged
	// as this uid. It is recorded rather than silently passed over: a run that
	// did not exercise this must not look like one that did.
	if os.Geteuid() == 0 {
		noteEnvironmentSkip(t, "e2e/webapp-layer: the read-only-directory probe cannot be staged as root, "+
			"which writes through the mode bit; the sandbox runs as uid 1000, where it does run")
	}

	// Arrange: a directory nothing may create inside, which is what a
	// read-only working copy (or a link into a read-only image layer) is.
	dir := t.TempDir()
	readonly := filepath.Join(dir, "readonly")
	if err := os.Mkdir(readonly, 0o555); err != nil {
		t.Fatalf("make the read-only dir: %v", err)
	}
	t.Cleanup(func() {
		// Restored so t.TempDir's own cleanup can remove it.
		if err := os.Chmod(readonly, 0o755); err != nil {
			t.Fatalf("restore the mode on %s: %v", readonly, err)
		}
	})

	// Act.
	err := wlWebappWritable(readonly)

	// Assert: the failure names the directory, because that name is the whole
	// value of catching this here.
	if err == nil {
		t.Fatalf("wlWebappWritable(%s) = nil, want a failure naming the directory", readonly)
	}
	if !strings.Contains(err.Error(), readonly) {
		t.Fatalf("wlWebappWritable(%s) = %v, want the message to name the directory", readonly, err)
	}
}
