package workspace

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// Select records the user's switch to a workspace, and it is idempotent —
// selecting the current workspace again is success. In this order:
//
//  1. THE SELECTION, FIRST AND AT ONCE (selectCurrent): WSM records the
//     selection instant, which is what "current" means; the ATTENTION MARKER
//     is cleared, because the user has now looked at what raised it; and the
//     roster is told, so every webview's selection agrees at once.
//  2. THEN, IF THE WORKSPACE IS PARKED, THE REVIVAL. Switching to a workspace
//     is looking at it, and an open workspace the user is looking at is never
//     session-less (owner ruling, 2026-09-13). `reviveIfParked` is a no-op for
//     anything the idle sweep did not stand down, so a select on a live
//     workspace changes nothing but the selection.
//
// THE SELECTION NEVER WAITS ON THE REVIVAL (owner ruling, 2026-09-19). A
// bring-up takes most of a second, and the revival used to run first: the
// sidebar lagged every switch by one to four seconds, and concurrent selects
// stamped `current` in the order their revivals FINISHED, so a user's last
// switch was overwritten by an earlier one whose bring-up happened to end
// later. Now the selection is stamped on arrival, in request order, and
// nothing after it touches `current` again: a revival that completes never
// re-stamps anything. While the revival runs the row carries the REVIVING
// marker.
//
// A REVIVAL THAT FAILS STILL FAILS THE SELECT, because a selected workspace
// with no session is exactly the state the revival exists to abolish and
// answering success would hide it. The selection it already made stands: the
// user did switch, and the error says what did not come up.
func (v *verbs) Select(ctx context.Context, ws ids.WorkspaceID) error {
	_, log, err := v.owned(ctx, "SelectWorkspace", ws)
	if err != nil {
		return err
	}
	if err := v.selectCurrent(ctx, log, ws); err != nil {
		return err
	}
	if _, err := v.reviveIfParked(ctx, log, opSelect, ws); err != nil {
		return fmt.Errorf("select %q: %w", ws, err)
	}
	return nil
}

// selectCurrent is Select's selection section: stamp the selection, clear the
// attention marker, push the roster. It holds the selection lock throughout,
// so concurrent selects land WHOLE in the order they took it — a select's
// read of the current workspace, its stamp and its roster push can never
// interleave with another's, which would leave WSM naming one workspace and
// the roster's selection another.
func (v *verbs) selectCurrent(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) error {
	v.selection.Lock()
	defer v.selection.Unlock()

	current, err := v.deps.DB.Current(ctx)
	if err != nil {
		log.Error(opSelect, "could not read the current workspace", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("select %q: read the current workspace: %w", ws, err)
	}
	// RE-SELECTION CHANGES NO VIEW. The selection instant is a roster column,
	// so re-stamping it on the workspace already being looked at would push a
	// different roster for a switch that never happened. The instant answers
	// "when did the user last switch to this", and the user did not switch.
	reselected := current != nil && *current == ws

	if !reselected {
		at := v.now()
		if err := v.deps.DB.SetCurrent(ctx, ws, at); err != nil {
			log.Error(opSelect, "could not record the selection", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("select %q: %w", ws, err)
		}
		// A WORKSPACE SWITCH IS A PRODUCTION-VISIBLE ACTION, so its receipt
		// stands at info: an info-level log that carried no trace of a switch
		// left the roster/tab-bar divergence bug with nothing to read.
		log.Info(opSelect, "selected the workspace", dlog.Context{"at": at.UTC()})
	} else {
		log.Info(opSelect, "the workspace was already current; the selection instant stands", nil)
	}
	if err := v.deps.DB.SetAttention(ctx, ws, false); err != nil {
		log.Error(opSelect, "could not clear the attention marker", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("select %q: clear attention: %w", ws, err)
	}

	// THE REGISTRY GOES FIRST. Both facts the selection changes — the current
	// id and the selection instant — are WSM's, so a roster rendered before
	// the refresh would carry the selection with an unstamped when-column, and
	// the client would see the switch land twice.
	v.republishRegistry(ctx, log, opSelect)
	v.deps.Sidebar.SetSelected(ws)
	return nil
}

// MarkViewed records that the user has SEEN the workspace, which reads the
// last turn's result and draws its roster row PARTIAL (the name recedes; the
// status dot keeps its colour) — only when the row is on a turn-end arm, done
// or interrupted, which the roster decides (`sidebar.SetViewed`).
//
// It does ONE thing, and the things it deliberately does NOT do are the point:
// it writes no durable record, because the display mode is a view fact that
// should not survive a restart; it does not revive a parked session, because
// looking at something is not working on it; and it has no companion "unview"
// verb, because the roster makes the next result unread itself when the next
// turn ends. Idempotent — marking an already-viewed workspace changes nothing.
func (v *verbs) MarkViewed(ctx context.Context, ws ids.WorkspaceID) error {
	_, log, err := v.owned(ctx, "MarkWorkspaceViewed", ws)
	if err != nil {
		return err
	}
	log.Debug(opMarkViewed, "the user has seen the workspace; its row goes PARTIAL if it is on a turn-end arm", nil)
	v.deps.Sidebar.SetViewed(ws)
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
