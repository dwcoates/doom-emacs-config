package account

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"agent-repld/internal/dlog"
)

// opMoveProject is the operation a project folder's move is recorded under.
const opMoveProject = "daemon.account.move_project"

// MoveProject moves the vendor's project folder of FROMCWD to the one of
// TOCWD under every account root (Roots), for a workspace whose worktree
// directory moved (a rename, agentrepl.v1.SetWorkspaceName). The vendor files
// a directory's conversations under a folder named for it, so a session
// resumed in the moved directory finds its transcript only once the folder
// has moved too.
//
// THE MOVE IS A RENAME, so every file keeps its inode: the sidecar's cursors
// are keyed by it (store.v1.CursorState.file_id), and a moved transcript is
// never read again from its start.
//
// IT IS IDEMPOTENT: a root holding no folder for FROMCWD has nothing to move,
// which is also how a move made before reads. A root that already holds a
// folder for TOCWD (an earlier workspace once worked there) takes each entry
// of FROMCWD's folder beside its own; an entry both hold is refused, and
// nothing of that root moves further.
func (r *resolver) MoveProject(ctx context.Context, fromCWD, toCWD string) error {
	if err := ctx.Err(); err != nil {
		return err
	}
	for _, root := range r.Roots() {
		from, to := ProjectDir(root, fromCWD), ProjectDir(root, toCWD)
		fields := dlog.Context{"root": root, "from": from, "to": to}
		if err := r.moveProjectIn(from, to, fields); err != nil {
			r.log.Error(opMoveProject, "the vendor's project folder could not be moved", dlog.WithCause(fields, err))
			return fmt.Errorf("account: move the project folder %s to %s: %w", from, to, err)
		}
	}
	return nil
}

// moveProjectIn moves FROM to TO within one root.
func (r *resolver) moveProjectIn(from, to string, fields dlog.Context) error {
	if _, err := os.Lstat(from); os.IsNotExist(err) {
		r.log.Debug(opMoveProject, "the root holds no project folder to move", fields)
		return nil
	} else if err != nil {
		return err
	}
	if _, err := os.Lstat(to); os.IsNotExist(err) {
		if err := os.Rename(from, to); err != nil {
			return err
		}
		r.log.Info(opMoveProject, "moved the vendor's project folder", fields)
		return nil
	} else if err != nil {
		return err
	}
	entries, err := os.ReadDir(from)
	if err != nil {
		return err
	}
	for _, entry := range entries {
		dest := filepath.Join(to, entry.Name())
		if _, err := os.Lstat(dest); err == nil {
			return fmt.Errorf("both folders hold %s", entry.Name())
		} else if !os.IsNotExist(err) {
			return err
		}
	}
	for _, entry := range entries {
		if err := os.Rename(filepath.Join(from, entry.Name()), filepath.Join(to, entry.Name())); err != nil {
			return err
		}
	}
	if err := os.Remove(from); err != nil {
		return err
	}
	r.log.Info(opMoveProject, "moved the vendor's project folder's entries beside the ones its destination held", dlog.Merge(fields, dlog.Context{"entries": len(entries)}))
	return nil
}
