package dlog

import (
	"fmt"
	"os"
	"strconv"
)

// logtarget.go — THE BORROW, and the fd contract derived from it.
//
// # Why a borrow type exists
//
// A workspace log target is owned by the TargetManager and SHARED: the daemon's
// own writer and every shim spawned for that workspace write through the same
// inode, and the manager closes it exactly once. Handing callers the raw
// *os.File made "close it when you are done with it" a thing a caller could
// plausibly do — and doing it once poisons the descriptor for every other
// writer, with the next spawn inheriting a closed fd 3.
//
// LogTarget is that handle with the close taken away: its method set has no
// Close and no accessor that yields one. A caller can name the target, ask
// whether it is live, and hand it to the fd contract below. It cannot end it.
// Only the manager can, and it does so on workspace close (EvictWorkspace) or
// daemon shutdown (Close).

// LogTarget is a borrowed, non-closeable handle on a manager-owned workspace
// runtime log target. The zero value is INVALID and every use of it fails
// loudly; a usable one comes only from TargetManager.BorrowWorkspaceRuntime.
type LogTarget struct {
	file      *os.File
	workspace Workspace
	runtime   Runtime
}

// Valid reports whether this handle came from the manager.
func (t LogTarget) Valid() bool { return t.file != nil }

// Name is the path of the external file the canonical workspace symlink names.
func (t LogTarget) Name() string {
	if t.file == nil {
		return ""
	}
	return t.file.Name()
}

// Workspace is the workspace this target belongs to.
func (t LogTarget) Workspace() Workspace { return t.workspace }

// Runtime is the runtime whose records this target carries.
func (t LogTarget) Runtime() Runtime { return t.runtime }

func (t LogTarget) String() string {
	if t.file == nil {
		return "dlog.LogTarget(invalid)"
	}
	return fmt.Sprintf("dlog.LogTarget(%s %s)", t.runtime, t.file.Name())
}

// BorrowWorkspaceRuntime opens (or reuses) a workspace runtime target and
// returns a BORROW of it: usable for a child's inherited descriptor, and
// impossible to close from the outside.
func (m *TargetManager) BorrowWorkspaceRuntime(workspace Workspace, runtime Runtime) (LogTarget, error) {
	file, err := m.OpenWorkspaceRuntime(workspace, runtime)
	if err != nil {
		return LogTarget{}, err
	}
	return LogTarget{file: file, workspace: workspace, runtime: runtime}, nil
}

// CloseBorrowedTargetForTest forcibly closes a borrowed target's descriptor
// while the manager still holds it.
//
// TEST-ONLY, and it exists precisely BECAUSE the borrow makes this
// unreachable: the double-use failure — a caller closing a shared target and
// the next spawn inheriting a dead fd — has to be provable, and no legitimate
// caller can arrange it any more. Nothing in the daemon calls this.
func CloseBorrowedTargetForTest(t LogTarget) error {
	if !t.Valid() {
		return fmt.Errorf("dlog: cannot force-close an unborrowed target")
	}
	return t.file.Close()
}

// ChildLogFDFlag is the argv flag naming which inherited descriptor carries a
// child runtime's canonical log target.
const ChildLogFDFlag = "--log-fd"

// firstChildExtraFD is the descriptor number the FIRST entry of an ExtraFiles
// slice lands on in the child. It is fixed by os/exec: 0, 1 and 2 are the
// standard streams, so extras start at 3.
const firstChildExtraFD = 3

// ChildLogBinding derives the whole fd contract for a spawned runtime from the
// target itself: the ExtraFiles slice the child inherits, and the argv pair
// that tells it which descriptor to write to.
//
// THE DESCRIPTOR NUMBER IS COMPUTED, NEVER WRITTEN DOWN. Three spawn sites each
// appended a literal "--log-fd", "3" beside a hand-built one-element ExtraFiles
// slice; the two halves of that contract could drift independently, and a
// second extra file added at any one of them would have silently pointed the
// child at the wrong descriptor. Here the flag's value IS the slice position.
//
// A closed or unborrowed target is REFUSED with a diagnostic naming it, rather
// than passed to the child as a dead descriptor it would fail on later, deeper,
// and less legibly.
func ChildLogBinding(target LogTarget) (extraFiles []*os.File, argv []string, err error) {
	if !target.Valid() {
		return nil, nil, fmt.Errorf("dlog: cannot bind a child log fd to an unborrowed target; it must come from TargetManager.BorrowWorkspaceRuntime")
	}
	if _, statErr := target.file.Stat(); statErr != nil {
		return nil, nil, fmt.Errorf("dlog: cannot bind a child log fd to a CLOSED %s target for workspace %q (%s): %w",
			target.runtime, target.workspace.Directory, target.file.Name(), statErr)
	}
	extraFiles = []*os.File{target.file}
	fd := firstChildExtraFD + len(extraFiles) - 1
	return extraFiles, []string{ChildLogFDFlag, strconv.Itoa(fd)}, nil
}
