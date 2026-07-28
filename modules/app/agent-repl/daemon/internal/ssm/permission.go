package ssm

import (
	"database/sql"
	"fmt"
)

// ApplyPermission opens or closes the workspace's PERMISSION row: the agent has
// asked the user a canUseTool question and is parked until it is answered.
//
// THIS IS THE ONLY PRODUCER of RENDER_STATE_PERMISSION. The render side of it
// was complete on every plane — the token, rank 40 on the agent axis, the green
// color, the webapp and elisp mappings — and nothing ever wrote the row, so a
// workspace parked on a question the user had not seen kept the `thinking` row
// of the turn that asked it and painted RED. The one state whose whole job is
// "the agent is waiting on YOU" read as "the agent is busy".
//
// IT IS AN AGENT-AXIS ROW, not an axis of its own, and it supersedes rather
// than latches — the same shape `done`, `interrupted` and `vendor_blocked`
// have. That is what makes every one of these true without a special case:
//
//   - A TurnEnded arriving while a permission is pending WINS, because its row
//     is the later one. A turn that died holding a question is over, and the
//     question died with it.
//   - A rotation or a restart cannot wedge it, because whatever the agent does
//     next replaces it.
//
// PENDING is the workspace's pending-permission count folded to a boolean; the
// caller derives it from the live rendezvous registry (sessiondrv's permRegistry)
// rather than keeping a second ledger. So the opening edge is the FIRST pending
// permission and the closing edge is the count returning to zero — grant, deny
// and abandonment alike, because all three release the waiter.
//
// REASON is a short note recorded as the cause detail.
func (m *Manager) ApplyPermission(workspace string, pending bool, reason string) error {
	if workspace == "" {
		return fmt.Errorf("ssm: ApplyPermission got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.applyPermissionLocked(workspace, pending, reason)
}

// applyPermissionLocked moves the permission row, appending only on a real
// edge. Caller holds mu.
//
// EVERY NO-OP IS LOUD. A second open means the caller believes a permission
// opened that this log already shows open; a close with nothing open means the
// row was superseded by something else (a turn end, a rotation) before the
// answer came back. Both are ordinary, both are worth seeing when a workspace's
// color is being explained.
func (m *Manager) applyPermissionLocked(workspace string, pending bool, reason string) error {
	top, beneath, err := agentAxisTop(m.db, workspace)
	if err != nil {
		return err
	}
	if (top == sigPermission) == pending {
		m.logf("ssm: permission row unchanged ws=%s pending=%t agent_top=%q reason=%q — no row appended",
			workspace, pending, top, reason)
		return nil
	}
	cause := causePermission
	if reason != "" {
		cause = causePermission + ":" + reason
	}
	if pending {
		if err := appendRow(m.db, workspace, "", sigPermission, cause, sql.NullInt64{}, m.nextAt(), ""); err != nil {
			return err
		}
		return m.reresolveLocked(workspace, cause, 0)
	}

	// CLOSING. The row it buried is the only record of whether the turn that
	// asked the question is still running, so it decides what the axis says now.
	//
	// A turn that is still in flight gets its `thinking` back — the agent went
	// straight from waiting on the user to working again, and leaving the green
	// row standing would report a settled workspace that is mid-turn.
	//
	// Anything else is left ALONE. `thinking` would be a claim about a turn this
	// log has no evidence for, and fabricating one to close a window is exactly
	// the wedge (a red workspace nothing can release) this whole vocabulary is
	// built to avoid. The permission row stands until the agent's next row
	// supersedes it, which is the honest reading: the question is answered, and
	// nothing since has said what the agent is doing.
	if beneath != sigThinking {
		m.logf("ssm: permission answered with no live turn beneath it ws=%s beneath=%q reason=%q — the agent axis is left as it stands rather than claiming a turn the log cannot see",
			workspace, beneath, reason)
		return nil
	}
	// The restored row carries the bare `turn_started` cause kind, unadorned,
	// because resolve() reads turn_active off an EXACT match on it — a decorated
	// detail here would report a running turn as inactive. The edge that caused
	// the restore is named in the log line below instead.
	if err := appendRow(m.db, workspace, "", sigThinking, causeTurnStarted, sql.NullInt64{}, m.nextAt(), ""); err != nil {
		return err
	}
	m.logf("ssm: permission closed ws=%s reason=%q — the turn that asked is still in flight, so the agent axis returns to `thinking`",
		workspace, reason)
	return m.reresolveLocked(workspace, cause, 0)
}

// closePermissionLocked releases an open permission row on a bounding edge that
// no pending permission can outlive (a vendor session rotation, a daemon
// restart). Caller holds mu.
//
// THE PENDINGS DIE WITH THE SHIM CONNECTION. A rotation bounces the shim and
// sessiondrv's permRegistry.fail abandons every waiter on it; a restart takes
// the whole rendezvous with it. In both cases the question is gone and the shim
// re-asks on reattach, so a row left standing would describe a prompt nobody is
// looking at — and, on the rotation path specifically, would hide the `thinking`
// underneath it from the reconciliation that exists to unstick it.
//
// A failure is loud-logged rather than returned, exactly as the compacting
// window's bounding close is: the caller is a rotation or a warm whose own work
// must still complete. It is never absorbed silently.
func (m *Manager) closePermissionLocked(workspace, reason string) {
	top, _, err := agentAxisTop(m.db, workspace)
	if err != nil {
		m.logf("ssm: reading the agent axis FAILED ws=%s reason=%s: %v — a pending permission row may stay open", workspace, reason, err)
		return
	}
	if top != sigPermission {
		return
	}
	m.logf("ssm: permission row released by %s ws=%s — the pending request cannot outlive it, and the shim re-asks on reattach", reason, workspace)
	if err := m.applyPermissionLocked(workspace, false, reason); err != nil {
		m.logf("ssm: releasing the permission row FAILED ws=%s reason=%s: %v", workspace, reason, err)
	}
}

// releasePersistedPermissionsLocked closes every permission row still standing
// in the log at Open. Caller holds mu (warm).
//
// The row is durable and the rendezvous behind it is not, so a daemon that
// restarted while a question was pending would come back claiming a prompt no
// process is holding and no frontend can answer. The shim re-asks on reattach
// and re-opens the row honestly.
func (m *Manager) releasePersistedPermissionsLocked() error {
	open, err := permissionOpenWorkspaces(m.db)
	if err != nil {
		return err
	}
	for _, ws := range open {
		m.closePermissionLocked(ws, "daemon_restart")
	}
	return nil
}
