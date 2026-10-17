package workspace

import (
	"context"
	"errors"
	"fmt"
	"slices"

	"agent-repld/internal/ctxend"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// Select records the user's switch to a workspace, and it is idempotent —
// selecting the current workspace again is success. In this order:
//
//  1. THE SELECTION, FIRST AND AT ONCE (selectCurrent): WSM records the
//     selection instant, which is what "current" means; the ATTENTION MARKER
//     is cleared, because the user has now looked at what raised it; and the
//     roster is told, so every webview's selection agrees at once.
//  2. THEN, IF THE WORKSPACE HAS NO SESSION, THE REVIVAL. Switching to a
//     workspace is looking at it, and an open workspace the user is looking
//     at is never session-less (owner rulings, 2026-09-13 and 2026-10-02).
//     `reviveIfSessionless` is a no-op for a live or closed workspace, so a
//     select on a live workspace changes nothing but the selection.
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
	if _, err := v.selectCurrent(ctx, log, ws, nil); err != nil {
		return err
	}
	return v.reviveSelected(ctx, log, ws)
}

// reviveSelected brings a just-selected workspace's parked session back up, as
// every selection does.
func (v *verbs) reviveSelected(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) error {
	if _, err := v.reviveIfSessionless(ctx, log, opSelect, ws); err != nil {
		// A DAEMON THAT IS LEAVING ANSWERS `standing_down`. The selection
		// above already stands; only the revival was not attempted, because
		// this daemon starts no session for a workspace it is about to stop
		// serving. The client re-asserts the selection on the daemon that
		// serves next, which revives it. Recorded at INFO by refuse.
		if errors.Is(err, shimclient.ErrStandingDown) {
			return refuse(log, "SelectWorkspace", ArmStandingDown,
				"this daemon is standing down; the selection is recorded and the next daemon revives the workspace", false)
		}
		return fmt.Errorf("select %q: %w", ws, err)
	}
	return nil
}

// selectCurrent is Select's selection section: stamp the selection, clear the
// attention marker, push the roster ONCE, and — on a SWITCH, never a re-selection —
// return the workspace's feed to its tail (HostRelay.ReturnFeedToTail).
//
// A SWITCH RETURNS THE FEED TO ITS TAIL HERE, because every switch path —
// Emacs's tab chords and pickers, a sidebar row click, a merge-queue entry —
// is this one verb. A re-selection of the workspace already current changes
// no view (Emacs re-asserts after a sidebar click, after a reconnect, after a
// relink), so it ends no selection either. It holds the selection lock throughout,
// so concurrent selects land WHOLE in the order they took it — a select's
// read of the current workspace, its stamp and its roster push can never
// interleave with another's, which would leave WSM naming one workspace and
// the roster's selection another.
//
// FROM, when set, makes the select CONDITIONAL: it applies only while FROM is
// still the current workspace, read under the same lock, so a select made on
// the user's behalf (the landing after a hide) can never undo a switch the
// user made meanwhile. It answers whether the select applied.
func (v *verbs) selectCurrent(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, from *ids.WorkspaceID) (bool, error) {
	v.selection.Lock()
	defer v.selection.Unlock()

	current, err := v.deps.DB.Current(ctx)
	if err != nil {
		log.Error(opSelect, "could not read the current workspace", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("select %q: read the current workspace: %w", ws, err)
	}
	if from != nil && (current == nil || *current != *from) {
		standing := ""
		if current != nil {
			standing = string(*current)
		}
		log.Info(opSelect, "the user moved off the workspace this select was conditional on; the selection stands",
			dlog.Context{"from": string(*from), "current": standing})
		return false, nil
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
			return false, fmt.Errorf("select %q: %w", ws, err)
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
		return false, fmt.Errorf("select %q: clear attention: %w", ws, err)
	}

	// THE REGISTRY AND THE SELECTION REACH THE ROSTER AS ONE PUSH. Both facts
	// the selection changes — the current id and the selection instant — are
	// WSM's, so the registry is re-read after the stamp, and the roster takes
	// it together with the selection in ONE mutation
	// (Sidebar.SetRegistrySelected). Told apart, the registry and then the
	// selection were two whole-roster pushes per switch, which Emacs and every
	// page decoded, applied and repainted twice (2026-10-08, the laggy-switch
	// report).
	//
	// A REGISTRY THAT COULD NOT BE READ still leaves the selection pushed: the
	// read already recorded its failure, and the roster takes the selection
	// alone, so every client's highlight agrees with the switch anyway.
	if reg, ok := v.readRegistry(ctx, log, opSelect); ok {
		v.deps.Sidebar.SetRegistrySelected(reg, ws)
		logRepublished(log, opSelect, reg)
	} else {
		v.deps.Sidebar.SetSelected(ws)
	}
	if !reselected {
		v.deps.Host.ReturnFeedToTail(ws)
		// THE SELECTION IS A VISIBILITY EDGE (wsm.Workspace.Runs): the
		// workspace the user looks at runs even hidden, and the one they left
		// is withheld again if it is. The row is written above, before either
		// edge is queued, so the keeper reads the selection either way.
		v.deps.CacheLapses.VisibilityChanged(ws)
		if current != nil {
			v.deps.CacheLapses.VisibilityChanged(*current)
		}
	}
	return true, nil
}

// FollowStoredSelection is the roster's half of a selection ANOTHER daemon
// made.
//
// TWO DAEMONS SERVE ROSTERS ACROSS A HANDOVER, and a select lands on the one
// that owns the workspace: the successor for every workspace already handed
// over, the predecessor for one it took back mid-turn. Each stamps the
// selection into the state file both share, but only the stamping daemon's
// roster heard of it, so a client still watching the other daemon's roster
// kept the old selection and never moved (2026-10-11: Emacs's roster stayed on
// the predecessor through a long handover, and every switch was deferred
// against a selection that no longer stood). WSM is the one place both
// daemons write, so it is the selection's one source: this reads it and, when
// the roster names anything else, pushes the registry WSM holds, which carries
// that selection.
//
// It does NOT return the feed to its tail or revive anything: the daemon that
// took the select did both for the workspace it serves.
func (v *verbs) FollowStoredSelection(ctx context.Context) error {
	v.selection.Lock()
	defer v.selection.Unlock()

	log := v.deps.Log.Global()
	stored, err := v.deps.DB.Current(ctx)
	if err != nil {
		if ctxend.Is(err) {
			log.Info(opFollowSelected, "the stored selection's read ended when its context was cancelled", dlog.Context{"cause": err.Error()})
			return err
		}
		log.Error(opFollowSelected, "could not read the stored selection", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("follow the stored selection: %w", err)
	}
	if stored == nil {
		log.Debug(opFollowSelected, "WSM stores no selection; the roster's stands", nil)
		return nil
	}
	shown, ok := v.deps.Sidebar.Selected()
	if ok && shown == *stored {
		log.Debug(opFollowSelected, "the roster already names the stored selection", dlog.Context{"workspace": string(shown)})
		return nil
	}
	reg, readable := v.readRegistry(ctx, log, opFollowSelected)
	if !readable {
		// readRegistry recorded why; the roster keeps the selection it had
		// until the next commit tries again.
		return fmt.Errorf("follow the stored selection %q: the registry could not be read", *stored)
	}
	v.deps.Sidebar.SetRegistrySelected(reg, *stored)
	previous := ""
	if ok {
		previous = string(shown)
	}
	log.Info(opFollowSelected, "the roster took the selection another daemon stored", dlog.Context{
		"workspace": string(*stored), "previous": previous,
	})
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

// SetHidden moves a workspace into the roster's hidden section, or back out of
// it, stamping when it was hidden. The roster hoists a hidden workspace's row
// out of every grouping, and the editor's tab bar stops drawing its tab off
// that same push. HIDING HOLDS ITS KEEP-ALIVES (cachelapse.Keeper): its cache
// is left to lapse, and its session is stood down when it does, so the user
// can still show it again warm until then. SHOWING runs them again, and starts
// nothing by itself; the editor asks with OpenTabs. HIDING THE WORKSPACE THE
// USER STANDS ON LANDS THEM on its neighbour in the tab bar
// (landAfterHiding). Asking for the place it already holds is success.
func (v *verbs) SetHidden(ctx context.Context, ws ids.WorkspaceID, hidden bool) error {
	_, log, err := v.owned(ctx, "SetWorkspaceHidden", ws)
	if err != nil {
		return err
	}
	if err := v.recordHidden(ctx, log, ws, hidden); err != nil {
		return err
	}
	if !hidden {
		return nil
	}
	return v.landAfterHiding(ctx, log, ws)
}

// recordHidden is THE ONE WRITE of a workspace's hidden flag: the row, the
// roster push, and the cache keeper's visibility edge. SetHidden and an
// answered cold gate (AnswerColdGate) both reach it.
func (v *verbs) recordHidden(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, hidden bool) error {
	if err := v.deps.DB.SetHidden(ctx, ws, hidden, v.now()); err != nil {
		log.Error(opSetHidden, "could not record whether the workspace is hidden", dlog.Context{"hidden": hidden, "cause": err.Error()})
		return fmt.Errorf("set hidden on %q: %w", ws, err)
	}
	log.Info(opSetHidden, "recorded whether the workspace is hidden", dlog.Context{"hidden": hidden})
	v.republishRegistry(ctx, log, opSetHidden)
	// THE ROW IS WRITTEN BEFORE THE EDGE IS QUEUED, which is what lets the
	// keeper's reconcile catch a hide that races a session coming up.
	v.deps.CacheLapses.VisibilityChanged(ws)
	return nil
}

// landAfterHiding moves the user, when they stand on the workspace just
// hidden, to its neighbour in the tab bar (owner request, 2026-10-14): the
// nearest tab to its LEFT the bar still draws, or, when none is drawn left of
// it, the nearest drawn to its right. The select is conditional on the hidden
// workspace still being current, under the selection lock, so a switch the
// user made meanwhile stands. With no drawn tab either side the selection
// stays where it is, and the record says so.
func (v *verbs) landAfterHiding(ctx context.Context, log dlog.Logger, hidden ids.WorkspaceID) error {
	reg, ok := v.readRegistry(ctx, log, opSetHidden)
	if !ok {
		return fmt.Errorf("land after hiding %q: the registry could not be read", hidden)
	}
	target, found := hideLandingTarget(sidebar.Tabs(reg), hidden)
	if !found {
		log.Info(opSetHidden, "the tab bar draws no other workspace beside the hidden one; the selection stays", nil)
		return nil
	}
	// THE LANDING IS A SELECT LIKE ANY OTHER, so it is taken only by the
	// daemon that serves the workspace it lands on (owned refuses it, and
	// records why, during a handover).
	_, targetLog, err := v.owned(ctx, "SelectWorkspace", target.ID)
	if err != nil {
		return err
	}
	targetLog = targetLog.With(dlog.Context{"hidden": string(hidden)})
	applied, err := v.selectCurrent(ctx, targetLog, target.ID, &hidden)
	if err != nil {
		return fmt.Errorf("land after hiding %q: %w", hidden, err)
	}
	if !applied {
		return nil
	}
	targetLog.Info(opSetHidden, "landed the user on the hidden workspace's neighbour in the tab bar",
		dlog.Context{"place": target.Place})
	return v.reviveSelected(ctx, targetLog, target.ID)
}

// hideLandingTarget picks where hiding HIDDEN lands the user among TABS, the
// tab bar in registry order (sidebar.Tabs): the nearest drawn tab left of
// HIDDEN's own, else the nearest drawn to its right. It answers false when
// HIDDEN has no tab, or no other tab is drawn.
func hideLandingTarget(tabs []sidebar.Tab, hidden ids.WorkspaceID) (wsm.Workspace, bool) {
	at := slices.IndexFunc(tabs, func(t sidebar.Tab) bool { return t.Workspace.ID == hidden })
	if at < 0 {
		return wsm.Workspace{}, false
	}
	for i := at - 1; i >= 0; i-- {
		if tabs[i].Withheld == wsm.Drawn {
			return tabs[i].Workspace, true
		}
	}
	for i := at + 1; i < len(tabs); i++ {
		if tabs[i].Withheld == wsm.Drawn {
			return tabs[i].Workspace, true
		}
	}
	return wsm.Workspace{}, false
}

// RevealMergeWorkspace draws a hidden merge workspace in the tab bar and the
// sidebar from now on (Verbs). It answers whether this call revealed it; a
// merge workspace already revealed is no change.
func (v *verbs) RevealMergeWorkspace(ctx context.Context, ws ids.WorkspaceID) (bool, error) {
	log := v.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	revealed, err := v.deps.DB.RevealMergeWorkspace(ctx, ws)
	if err != nil {
		log.Error(opRevealMergeWorkspace, "could not record that the merge workspace is revealed", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("reveal merge workspace %q: %w", ws, err)
	}
	if !revealed {
		return false, nil
	}
	log.Info(opRevealMergeWorkspace, "revealed the merge workspace: it is drawn in the tab bar and the sidebar from now on", nil)
	v.republishRegistry(ctx, log, opRevealMergeWorkspace)
	return true, nil
}

// SetPriority sets or clears the roster's priority. Setting one moves the
// workspace to second-to-last within that priority (wsm's place.go).
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

// Move moves workspace ws onto target's place in the registry order, the
// workspaces between shifting one step toward the place ws left (wsm's
// MovePlace). The sidebar and the tab bar are both views of the registry, so
// one move shows on both; any two workspaces can move.
func (v *verbs) Move(ctx context.Context, ws, target ids.WorkspaceID) error {
	const rpc = "MoveWorkspace"
	_, log, err := v.owned(ctx, rpc, ws)
	if err != nil {
		return err
	}
	if _, _, err := v.owned(ctx, rpc, target); err != nil {
		return err
	}
	log = log.With(dlog.Context{"target": string(target)})
	if err := v.deps.DB.MovePlace(ctx, ws, target); err != nil {
		log.Error(opMove, "could not move the workspace", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("move %q onto %q: %w", ws, target, err)
	}
	log.Info(opMove, "moved the workspace onto the target's place", nil)
	v.republishRegistry(ctx, log, opMove)
	return nil
}
