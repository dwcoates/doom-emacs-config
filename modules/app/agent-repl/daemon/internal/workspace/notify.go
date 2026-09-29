package workspace

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
)

// opNotify is the operation the notification hook's records carry.
const opNotify = "daemon.workspace.notify"

// opAsksSettled is the operation the asks-settled hook's records carry.
const opAsksSettled = "daemon.workspace.asks_settled"

// Notify raises one host notification. It is the LifecycleSink's notification
// hook, wired by the server, and it does exactly two things:
//
//   - raises the workspace's DESKTOP BANNER, which the notifier posts only
//     while Emacs is not focused;
//   - sets the roster's ATTENTION MARKER, which SelectWorkspace and
//     AsksSettled clear.
//
// A permission ask fires this like any other notification: the ask is the
// notification, and there is no second path for it.
func (v *verbs) Notify(ctx context.Context, ws ids.WorkspaceID, note sessionwatcher.HostNotification) error {
	_, log, err := v.owned(ctx, "WatchHostWorkspace", ws)
	if err != nil {
		return err
	}
	if note.Kind == "" {
		return refuse(log, "WatchHostWorkspace", ArmUnservedAnswer,
			"a notification must name its kind", false)
	}

	v.deps.Banners.Raise(ws, string(note.Kind), note.Text)

	if err := v.deps.DB.SetAttention(ctx, ws, true); err != nil {
		log.Error(opNotify, "could not set the attention marker", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("notify %q: set attention: %w", ws, err)
	}
	log.Info(opNotify, "raised a host notification", dlog.Context{
		"kind": string(note.Kind), "tool": note.ToolName,
	})
	v.republishRegistry(ctx, log, opNotify)
	return nil
}

// AsksSettled clears the roster's ATTENTION MARKER because every ask that
// raised it has been decided.
//
// It is Notify's counterpart. The marker means an unseen notification
// (frontend.v1.RosterRow.attention), and a permission or question the user has
// answered — or that policy decided without them — is seen: the watcher, which
// is the one party that knows whether another ask is still open, calls this
// only when the last one settles. SelectWorkspace remains the other clear; a
// workspace nobody ever selects would otherwise wear the marker forever.
func (v *verbs) AsksSettled(ctx context.Context, ws ids.WorkspaceID) error {
	_, log, err := v.owned(ctx, "WatchHostWorkspace", ws)
	if err != nil {
		return err
	}
	if err := v.deps.DB.SetAttention(ctx, ws, false); err != nil {
		log.Error(opAsksSettled, "could not clear the attention marker", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("asks settled %q: clear attention: %w", ws, err)
	}
	log.Info(opAsksSettled, "the last open ask settled; the attention marker is cleared", nil)
	v.republishRegistry(ctx, log, opAsksSettled)
	return nil
}
