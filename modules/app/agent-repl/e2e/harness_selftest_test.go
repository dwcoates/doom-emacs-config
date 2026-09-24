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

func TestWorldDaemonEnvLeavesTheCheckoutToTheHarness(t *testing.T) {
	t.Parallel()
	// Arrange: a caller that states nothing about the checkout.
	extra := []string{"AGENT_REPL_TEST_ALL_SCRIPT=/gate"}

	// Act.
	env := worldDaemonEnv(extra, "/spool", "/shim-lock")

	// Assert: no checkout override rides the world's env, so the harness's
	// own pinned checkout stands and a deploy never installs into this
	// worktree.
	if got := valueOf(env, checkoutEnv); got != "" {
		t.Fatalf("worldDaemonEnv sets %s=%q, want it left to harness.StartDaemon's pinned checkout", checkoutEnv, got)
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

// TestPreserveWorkspaceElispLogsFollowsTheCanonicalSymlink covers the whole
// reason the sweep exists: what a workspace holds is a SYMLINK, and the
// records live in the durable target it names.
func TestPreserveWorkspaceElispLogsFollowsTheCanonicalSymlink(t *testing.T) {
	// Arrange: a workspace under the Emacs root whose canonical elisp sink is
	// a symlink to a target OUTSIDE the root, exactly as
	// `agent-repl--workspace-emacs-log-target` mints it.
	root := t.TempDir()
	outside := t.TempDir()
	target := filepath.Join(outside, "agent-repl-emacs-abc.log")
	const record = `{"operation":"agent-repl.elisp-host-transferred"}`
	if err := os.WriteFile(target, []byte(record), 0o600); err != nil {
		t.Fatalf("write the durable target: %v", err)
	}
	sink := filepath.Join(root, "repo-a", ".claude", "emacs")
	if err := os.MkdirAll(sink, 0o755); err != nil {
		t.Fatalf("create the workspace sink directory: %v", err)
	}
	if err := os.Symlink(target, filepath.Join(sink, "emacs.log")); err != nil {
		t.Fatalf("link the canonical path: %v", err)
	}
	out := t.TempDir()

	// Act
	(&Emacs{t: t}).preserveWorkspaceElispLogs(out, root)

	// Assert
	got, err := os.ReadFile(filepath.Join(out, "workspace-elisp-logs", "repo-a", ".claude", "emacs", "emacs.log"))
	if err != nil {
		t.Fatalf("the preserved workspace sink could not be read: %v", err)
	}
	if string(got) != record {
		t.Fatalf("preserved sink = %q, want the durable target's own bytes %q", got, record)
	}
}

// TestPreserveWorkspaceElispLogsIgnoresAnEmacsDirectoryOutsideDotClaude keeps
// the sweep to the ONE shape it is about. `emacs` is an ordinary directory
// name — the staged `~/.emacs.d` lives under this same root — and copying
// every one of them would put hundreds of megabytes of Doom into a failure's
// artifacts.
func TestPreserveWorkspaceElispLogsIgnoresAnEmacsDirectoryOutsideDotClaude(t *testing.T) {
	// Arrange
	root := t.TempDir()
	stray := filepath.Join(root, "state", "emacs")
	if err := os.MkdirAll(stray, 0o755); err != nil {
		t.Fatalf("create the stray directory: %v", err)
	}
	if err := os.WriteFile(filepath.Join(stray, "not-a-sink.log"), []byte("x"), 0o600); err != nil {
		t.Fatalf("write the stray file: %v", err)
	}
	out := t.TempDir()

	// Act
	(&Emacs{t: t}).preserveWorkspaceElispLogs(out, root)

	// Assert
	if _, err := os.Stat(filepath.Join(out, "workspace-elisp-logs")); !os.IsNotExist(err) {
		t.Fatalf("stat of the preserved tree = %v, want it never created for a non-.claude `emacs` directory", err)
	}
}

// TestPreserveWorkspaceElispLogsFindsASinkBesideTheEmacsRoot is the defect the
// first version of this sweep had: it walked `e.Root`, which is
// `<scratch>/emacs`, while the workspaces live beside it under the same
// scratch directory — so a red run's sweep found nothing and said nothing.
func TestPreserveWorkspaceElispLogsFindsASinkBesideTheEmacsRoot(t *testing.T) {
	// Arrange: the real layout — the Emacs HOME and a registered workspace as
	// siblings under one scratch root.
	scratch := t.TempDir()
	if err := os.MkdirAll(filepath.Join(scratch, "emacs", ".emacs.d", "lisp"), 0o755); err != nil {
		t.Fatalf("stage the Emacs home: %v", err)
	}
	sink := filepath.Join(scratch, "repo-a", ".claude", "emacs")
	if err := os.MkdirAll(sink, 0o755); err != nil {
		t.Fatalf("create the workspace sink directory: %v", err)
	}
	if err := os.WriteFile(filepath.Join(sink, "emacs.log"), []byte("{}"), 0o600); err != nil {
		t.Fatalf("write the sink: %v", err)
	}
	out := t.TempDir()

	// Act
	(&Emacs{t: t}).preserveWorkspaceElispLogs(out, scratch)

	// Assert
	if _, err := os.Stat(filepath.Join(out, "workspace-elisp-logs", "repo-a", ".claude", "emacs", "emacs.log")); err != nil {
		t.Fatalf("stat of the preserved workspace sink = %v, want it collected from beside the Emacs root", err)
	}
}
