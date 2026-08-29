package workspace

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// Select records the user's switch to a workspace. It does three things and
// nothing else, and it is idempotent — selecting the current workspace again is
// success:
//
//   - WSM records the selection instant, which is what "current" means;
//   - the ATTENTION MARKER is cleared, because the user has now looked at what
//     raised it;
//   - the roster is told, so every webview's selection agrees at once.
func (v *verbs) Select(ctx context.Context, ws ids.WorkspaceID) error {
	_, log, err := v.owned(ctx, "SelectWorkspace", ws)
	if err != nil {
		return err
	}

	at := v.now()
	if err := v.deps.DB.SetCurrent(ctx, ws, at); err != nil {
		log.Error(opSelect, "could not record the selection", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("select %q: %w", ws, err)
	}
	if err := v.deps.DB.SetAttention(ctx, ws, false); err != nil {
		log.Error(opSelect, "could not clear the attention marker", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("select %q: clear attention: %w", ws, err)
	}
	v.deps.Sidebar.SetSelected(ws)

	log.Debug(opSelect, "selected the workspace", dlog.Context{"at": at.UTC()})
	v.republishRegistry(ctx, log, opSelect)
	return nil
}

// SetPriority sets or clears the roster's ordering priority. The roster orders
// by it; clients — Emacs tabs included — follow roster order strictly, so this
// is the only place the order is decided.
func (v *verbs) SetPriority(ctx context.Context, ws ids.WorkspaceID, p *wsm.Priority) error {
	_, log, err := v.owned(ctx, "SetWorkspacePriority", ws)
	if err != nil {
		return err
	}
	if err := v.deps.DB.SetPriority(ctx, ws, p); err != nil {
		log.Error(opSetPriority, "could not record the priority", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("set priority on %q: %w", ws, err)
	}
	log.Debug(opSetPriority, "recorded the priority", dlog.Context{"cleared": p == nil})
	v.republishRegistry(ctx, log, opSetPriority)
	return nil
}
