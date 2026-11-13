package stack

import (
	"agentrepl/e2e/kit"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// The harness's own unit tests: the two helpers whose silent misbehavior does
// not fail visibly, but as a whole suite of timeouts and no_session prompts.

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
// The layer went red in eleven places at once, in the e2e sandbox only (the
// sandbox was removed 2026-11-13), because vite's default cache directory sat
// inside a read-only `node_modules`. The cache moved out
// (webapp/vite-cache.ts); kit.WlWebappWritable is what makes the NEXT such
// move say so once, by name, instead of eleven times in a node process. These two tests are its guarantee and its violation.

func TestWebappWritableRejectsADirectoryItCannotCreateTheCacheIn(t *testing.T) {
	t.Parallel()
	// root writes through a mode bit, so the read-only case cannot be staged
	// as this uid. It is recorded rather than silently passed over: a run that
	// did not exercise this must not look like one that did.
	if os.Geteuid() == 0 {
		noteEnvironmentSkip(t, "e2e/webapp-layer: the read-only-directory probe cannot be staged as root, "+
			"which writes through the mode bit; run it as an ordinary user")
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
	err := kit.WlWebappWritable(readonly)

	// Assert: the failure names the directory, because that name is the whole
	// value of catching this here.
	if err == nil {
		t.Fatalf("kit.WlWebappWritable(%s) = nil, want a failure naming the directory", readonly)
	}
	if !strings.Contains(err.Error(), readonly) {
		t.Fatalf("kit.WlWebappWritable(%s) = %v, want the message to name the directory", readonly, err)
	}
}
