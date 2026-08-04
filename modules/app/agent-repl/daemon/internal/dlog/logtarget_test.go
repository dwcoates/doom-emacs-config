package dlog

import (
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// THE BORROW and the fd contract derived from it (logtarget.go).
// ---------------------------------------------------------------------------

// A BORROWED HANDLE CANNOT BE CLOSED BY ITS HOLDER. This is the property the
// type exists for, and it is a compile-time one: the assertion here is that the
// method set carries no Close, expressed as the interface the type must NOT
// satisfy.
func TestABorrowedLogTargetHasNoCloseInItsMethodSet(t *testing.T) {
	// Arrange.
	manager := NewTargetManager()
	t.Cleanup(func() { _ = manager.Close() })
	workspace := Workspace{Directory: t.TempDir(), ID: "ws-borrow"}
	borrowed, err := manager.BorrowWorkspaceRuntime(workspace, RuntimeShim)
	if err != nil {
		t.Fatalf("borrow: %v", err)
	}

	// Act.
	var closer any = borrowed

	// Assert.
	if _, isCloser := closer.(interface{ Close() error }); isCloser {
		t.Fatal("a borrowed log target exposes Close; a caller could end a descriptor every writer for the workspace shares")
	}
	if !borrowed.Valid() || borrowed.Runtime() != RuntimeShim {
		t.Fatalf("borrowed = %v, want a valid shim target", borrowed)
	}
}

// THE FD FLAG'S VALUE IS THE SLICE POSITION, so the two halves of the contract
// cannot drift.
func TestChildLogBindingDerivesTheDescriptorFromTheSlice(t *testing.T) {
	// Arrange.
	manager := NewTargetManager()
	t.Cleanup(func() { _ = manager.Close() })
	workspace := Workspace{Directory: t.TempDir(), ID: "ws-fd"}
	borrowed, err := manager.BorrowWorkspaceRuntime(workspace, RuntimeShim)
	if err != nil {
		t.Fatalf("borrow: %v", err)
	}

	// Act.
	extraFiles, argv, err := ChildLogBinding(borrowed)

	// Assert.
	if err != nil {
		t.Fatalf("ChildLogBinding: %v", err)
	}
	if len(extraFiles) != 1 {
		t.Fatalf("extra files = %v, want exactly the log target", extraFiles)
	}
	if len(argv) != 2 || argv[0] != ChildLogFDFlag || argv[1] != "3" {
		t.Fatalf("argv = %v, want [%s 3]", argv, ChildLogFDFlag)
	}
}

// AN UNBORROWED TARGET IS REFUSED rather than producing an empty inheritance
// the child would fail on later and less legibly.
func TestChildLogBindingRefusesAnUnborrowedTarget(t *testing.T) {
	// Arrange / Act.
	_, _, err := ChildLogBinding(LogTarget{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "unborrowed target") {
		t.Fatalf("error = %v, want the unborrowed-target refusal", err)
	}
}

// A TARGET CLOSED UNDERNEATH THE DAEMON IS REFUSED, naming the runtime, the
// workspace and the file — the double-use failure, made legible.
func TestChildLogBindingRefusesAClosedTarget(t *testing.T) {
	// Arrange.
	manager := NewTargetManager()
	t.Cleanup(func() { _ = manager.Close() })
	workspace := Workspace{Directory: t.TempDir(), ID: "ws-closed"}
	borrowed, err := manager.BorrowWorkspaceRuntime(workspace, RuntimeShim)
	if err != nil {
		t.Fatalf("borrow: %v", err)
	}
	if err := CloseBorrowedTargetForTest(borrowed); err != nil {
		t.Fatalf("force-close: %v", err)
	}

	// Act.
	_, _, err = ChildLogBinding(borrowed)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "CLOSED") {
		t.Fatalf("error = %v, want the closed-target refusal", err)
	}
}

// THE TARGET'S LIFETIME IS THE WORKSPACE'S: closing a workspace releases every
// runtime target it held, and the gauge reflects it.
func TestEvictWorkspaceReleasesEveryRuntimeTargetOfThatWorkspace(t *testing.T) {
	// Arrange — two runtimes of one workspace, and a second workspace that must
	// be left alone.
	manager := NewTargetManager()
	t.Cleanup(func() { _ = manager.Close() })
	closing := Workspace{Directory: t.TempDir(), ID: "ws-closing"}
	other := Workspace{Directory: t.TempDir(), ID: "ws-other"}
	for _, runtime := range []Runtime{RuntimeDaemon, RuntimeShim} {
		if _, err := manager.BorrowWorkspaceRuntime(closing, runtime); err != nil {
			t.Fatalf("borrow %s: %v", runtime, err)
		}
	}
	if _, err := manager.BorrowWorkspaceRuntime(other, RuntimeDaemon); err != nil {
		t.Fatalf("borrow other: %v", err)
	}
	if got := manager.ActiveTargets(); got != 3 {
		t.Fatalf("active targets = %d, want 3", got)
	}

	// Act.
	evicted, err := manager.EvictWorkspace(closing)

	// Assert.
	if err != nil {
		t.Fatalf("EvictWorkspace: %v", err)
	}
	if evicted != 2 {
		t.Fatalf("evicted = %d, want the closed workspace's two runtime targets", evicted)
	}
	if got := manager.ActiveTargets(); got != 1 {
		t.Fatalf("active targets = %d, want only the other workspace's", got)
	}
}

// A REOPEN AFTER EVICTION IS A FRESH TARGET, not the closed descriptor handed
// back: the eviction must not make a workspace permanently unloggable.
func TestAWorkspaceReopenedAfterEvictionGetsALiveTarget(t *testing.T) {
	// Arrange.
	manager := NewTargetManager()
	t.Cleanup(func() { _ = manager.Close() })
	workspace := Workspace{Directory: t.TempDir(), ID: "ws-reopen"}
	if _, err := manager.BorrowWorkspaceRuntime(workspace, RuntimeShim); err != nil {
		t.Fatalf("borrow: %v", err)
	}
	if _, err := manager.EvictWorkspace(workspace); err != nil {
		t.Fatalf("EvictWorkspace: %v", err)
	}

	// Act.
	reopened, err := manager.BorrowWorkspaceRuntime(workspace, RuntimeShim)

	// Assert.
	if err != nil {
		t.Fatalf("reopen after eviction: %v", err)
	}
	if _, _, err := ChildLogBinding(reopened); err != nil {
		t.Fatalf("the reopened target is not usable: %v", err)
	}
}

// AN UNKNOWN WORKSPACE EVICTS NOTHING and says so with a zero count rather than
// an error: closing a workspace that never logged is not a failure.
func TestEvictWorkspaceOnAWorkspaceWithNoTargetsIsAZeroCount(t *testing.T) {
	// Arrange.
	manager := NewTargetManager()
	t.Cleanup(func() { _ = manager.Close() })

	// Act.
	evicted, err := manager.EvictWorkspace(Workspace{Directory: t.TempDir(), ID: "ws-unknown"})

	// Assert.
	if err != nil {
		t.Fatalf("EvictWorkspace: %v", err)
	}
	if evicted != 0 {
		t.Fatalf("evicted = %d, want 0", evicted)
	}
}
