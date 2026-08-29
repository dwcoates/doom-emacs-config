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

// Notify raises one host notification. It is the LifecycleSink's notification
// hook, wired by the server, and it does exactly two things:
//
//   - relays the TYPED notification onto the workspace's host stream, so Emacs's
//     own focus policy can act on it;
//   - sets the roster's ATTENTION MARKER, which SelectWorkspace clears.
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

	v.deps.Host.Notify(ws, note.Text, string(note.Kind), note.ToolName)

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
