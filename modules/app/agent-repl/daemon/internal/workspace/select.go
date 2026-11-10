package workspace

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"time"

	"agent-repld/internal/ctxend"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// Select records the user's switch to a workspace, and it is idempotent —
// selecting the current workspace again is success. In this order:
//
//  1. THE SELECTION, FIRST AND AT ONCE (selectCurrent): WSM records the
//     selection instant, which is what "current" means; the ATTENTION MARKER
//     is cleared, because the user has now looked at what raised it; and the
//     roster is told, so every webview's selection agrees at once.
//
// A SELECT STARTS NO SHIM (owner ruling, 2026-10-28, reversing the look-driven
// revival of 2026-09-13 and 2026-10-02): the workspace's conversation is read
// from the store, so a workspace switched to with no session -- parked,
// hibernated, at its cold gate, never started -- shows its conversation with
// no shim behind it, and the next prompt, from the user or another
// workspace, brings the shim up.
func (v *verbs) Select(ctx context.Context, ws ids.WorkspaceID, sw Switch) error {
	_, log, err := v.owned(ctx, "SelectWorkspace", ws)
	if err != nil {
		return err
	}
	_, err = v.selectCurrent(ctx, log, ws, nil, sw)
	return err
}

// selectCurrent is Select's selection section: record the selection and
// clear the attention marker in ONE store write, push the roster ONCE, and —
// on a SWITCH, never a re-selection — return the workspace's feed to its tail
// (HostRelay.ReturnFeedToTail).
//
// A SWITCH RETURNS THE FEED TO ITS TAIL HERE, because every switch path —
// Emacs's tab chords and pickers, a sidebar row click, a merge-queue entry —
// is this one verb. A re-selection of the workspace already current changes
// no view (Emacs re-asserts after a sidebar click, after a reconnect, after a
// relink), so it ends no selection either. It holds the selection lock
// throughout, so concurrent selects land WHOLE in the order they took it, and
// so does every whole registry read and its install (installRegistry): a
// registry read before this select's write can never be installed after its
// roster push, which would put the selection it left back on the roster.
//
// THE ROSTER TAKES THE STORE'S OWN ANSWER, NOT A REGISTRY READ. The store's
// write answers the rows it changed, read back in its own transaction
// (wsm.Selection), and the roster installs them into the registry it holds
// (Sidebar.SetSelection). Re-reading the whole registry after every switch
// cost 15-230ms with 86 tabs before the sidebar could draw the switch, while
// the page had already painted it (2026-11-08). A roster that cannot take the
// change -- no registry yet, or one missing a row the change names -- takes a
// whole registry read instead.
//
// FROM, when set, makes the select CONDITIONAL: it applies only while FROM is
// still the current workspace, read under the same lock, so a select made on
// the user's behalf (the landing after a hide) can never undo a switch the
// user made meanwhile. It answers whether the select applied.
//
// SW says whether the switch stamps the selection instant (RecordSelection)
// or leaves it standing (KeepSelectionHistory, a step of Emacs's recency
// walk), and when the user's switch began, which the roster carries to the
// sidebar with the select's own id (sidebar.SelectStamp).
func (v *verbs) selectCurrent(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, from *ids.WorkspaceID, sw Switch) (bool, error) {
	v.selection.Lock()
	defer v.selection.Unlock()

	if from != nil {
		current, err := v.deps.DB.Current(ctx)
		if err != nil {
			log.Error(opSelect, "could not read the current workspace", dlog.Context{"cause": err.Error()})
			return false, fmt.Errorf("select %q: read the current workspace: %w", ws, err)
		}
		if current == nil || *current != *from {
			standing := ""
			if current != nil {
				standing = string(*current)
			}
			log.Info(opSelect, "the user moved off the workspace this select was conditional on; the selection stands",
				dlog.Context{"from": string(*from), "current": standing})
			return false, nil
		}
	}
	stamp := sidebar.SelectStamp{ID: v.mintSelection(), SwitchStartedAt: sw.StartedAt}
	fields := dlog.Context{"history": sw.History.String(), "selection_id": stamp.ID}
	if sw.StartedAt != nil {
		fields["switch_started_at"] = sw.StartedAt.UTC()
	}
	at := v.now()
	sel, err := v.deps.DB.Select(ctx, ws, sw.History.write(), at)
	if err != nil {
		log.Error(opSelect, "could not record the selection", dlog.WithCause(fields, err))
		return false, fmt.Errorf("select %q: %w", ws, err)
	}

	// RE-SELECTION CHANGES NO VIEW. The selection instant is a roster column,
	// so re-stamping it on the workspace already being looked at would push a
	// different roster for a switch that never happened. The instant answers
	// "when did the user last switch to this", and the user did not switch.
	// The store decides it in the same transaction as the write.
	switch {
	case sel.Again:
		log.Info(opSelect, "the workspace was already current; the selection instant stands", fields)
	case sw.History == KeepSelectionHistory:
		// A STEP OF A RECENCY WALK is not a choice: stamping the instant
		// would move the workspace to the front of the history the walk is
		// stepping along, and the next step would land somewhere else.
		log.Info(opSelect, "selected the workspace as a step of a recency walk; the selection instant stands", fields)
	default:
		// A WORKSPACE SWITCH IS A PRODUCTION-VISIBLE ACTION, so its receipt
		// stands at info: an info-level log that carried no trace of a switch
		// left the roster/tab-bar divergence bug with nothing to read.
		log.Info(opSelect, "selected the workspace", dlog.Merge(fields, dlog.Context{"at": at.UTC()}))
	}

	v.publishSelection(ctx, log, sel, stamp, fields)
	if !sel.Again {
		v.deps.Host.ReturnFeedToTail(ws)
		// THE SWITCH ENDS EVERY SETTINGS VISIT IT LEAVES: the shim a pick
		// started is stood down unless a turn has run since (settingsvisits.go).
		for visited, since := range v.deps.SettingsVisits.LeftFor(ws) {
			log.Info(opSelect, "the switch left a workspace whose shim a settings pick started; it is asked to stand down",
				dlog.Context{"visited": string(visited), "since": since.UTC()})
			v.deps.Drain.VisitDown(visited, since)
		}
		// THE SELECTION IS A VISIBILITY EDGE (wsm.Workspace.Runs): the
		// workspace the user looks at runs even hidden, and the one they left
		// is withheld again if it is. The row is written above, before either
		// edge is queued, so the keeper reads the selection either way.
		v.deps.CacheLapses.VisibilityChanged(ws)
		if sel.Previous != nil {
			v.deps.CacheLapses.VisibilityChanged(*sel.Previous)
		}
	}
	return true, nil
}

// publishSelection hands the roster a select's change SEL under STAMP, in ONE
// push: the store's own answer when the roster's registry can take it
// (Sidebar.SetSelection), else a whole registry read with the selection
// (Sidebar.SetRegistrySelected). A REGISTRY THAT COULD NOT BE READ still
// leaves the selection pushed: the read already recorded its failure, and the
// roster takes the selection alone, so every client's highlight agrees with
// the switch anyway. It runs under the selection lock.
func (v *verbs) publishSelection(ctx context.Context, log dlog.Logger, sel wsm.Selection, stamp sidebar.SelectStamp, fields dlog.Context) {
	if v.deps.Sidebar.SetSelection(sel, stamp) {
		log.Info(opSelect, "published the selection to the roster from the store's answer", fields)
		return
	}
	if reg, ok := v.readRegistry(ctx, log, opSelect); ok {
		v.deps.Sidebar.SetRegistrySelected(reg, sel.Current, stamp)
		logRepublished(log, opSelect, reg)
		return
	}
	v.deps.Sidebar.SetSelected(sel.Current)
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
	v.deps.Sidebar.SetRegistrySelected(reg, *stored, sidebar.SelectStamp{})
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
// USER STANDS ON LANDS THEM on the workspace they selected before it
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
		// A FAMILY IS HIDDEN CHILD FIRST AND SHOWN PARENT FIRST
		// (wsm/family.go): the refusal is the answer.
		if refusedByFamily(log, opSetHidden, err) {
			return err
		}
		log.Error(opSetHidden, "could not record whether the workspace is hidden", dlog.Context{"hidden": hidden, "cause": err.Error()})
		return fmt.Errorf("set hidden on %q: %w", ws, err)
	}
	log.Info(opSetHidden, "recorded whether the workspace is hidden", dlog.Context{"hidden": hidden})
	v.hiddenRecorded(ctx, log, ws, opSetHidden)
	return nil
}

// hiddenRecorded is what follows every recorded move into or out of the
// hidden section (recordHidden, BacklogColdGate): the roster push and the
// cache keeper's visibility edge.
func (v *verbs) hiddenRecorded(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, op string) {
	v.republishRegistry(ctx, log, op)
	// THE ROW IS WRITTEN BEFORE THE EDGE IS QUEUED, which is what lets the
	// keeper's reconcile catch a hide that races a session coming up.
	v.deps.CacheLapses.VisibilityChanged(ws)
}

// BacklogColdGate moves a shown workspace whose cold gate has stood too long
// into the hidden section, drawn as the Backlog (owner request, 2026-10-26;
// coldbacklog.Sweeper), once for the gate whose cache lapsed at GATELAPSEDAT.
// It is a hide in every other respect: the roster drops its tab, and the cache
// keeper takes the gate's lapse and stands the parked session down.
//
// THE STORE JUDGES IN THE SAME TRANSACTION AS IT WRITES (wsm.BacklogColdGate),
// so a workspace the user selected, hid or closed meanwhile is left as it is.
// A workspace with open children stays where it is, as a hide would; that is
// an answer, not a failure, so it answers nil.
func (v *verbs) BacklogColdGate(ctx context.Context, ws ids.WorkspaceID, gateLapsedAt time.Time) error {
	_, log, err := v.owned(ctx, "BacklogColdGate", ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"gate_lapsed_at": gateLapsedAt})
	moved, err := v.deps.DB.BacklogColdGate(ctx, ws, gateLapsedAt, v.now())
	if err != nil {
		if wsm.IsFamilyRefusal(err) {
			log.Debug(opBacklogColdGate, "the workspace has open children, so it stays shown as a hide would", dlog.Context{"cause": err.Error()})
			return nil
		}
		log.Error(opBacklogColdGate, "could not move the workspace into the backlog", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("backlog %q: %w", ws, err)
	}
	if !moved {
		log.Debug(opBacklogColdGate, "the workspace was selected, hidden, closed or already backlogged for this gate; it is left as it is", nil)
		return nil
	}
	log.Info(opBacklogColdGate, "moved the workspace into the backlog: its cold gate stood for over a weekday", nil)
	v.hiddenRecorded(ctx, log, ws, opBacklogColdGate)
	return nil
}

// landAfterHiding moves the user, when they stand on the workspace just
// hidden, to the workspace they selected before it (owner request,
// 2026-10-20, superseding the tab-bar neighbour of 2026-10-14): the most
// recently selected tab the bar still draws (hideLandingTarget). The select is
// conditional on the hidden workspace still being current, under the
// selection lock, so a switch the user made meanwhile stands. With no other
// drawn tab the selection stays where it is, and the record says so.
func (v *verbs) landAfterHiding(ctx context.Context, log dlog.Logger, hidden ids.WorkspaceID) error {
	reg, ok := v.readRegistry(ctx, log, opSetHidden)
	if !ok {
		return fmt.Errorf("land after hiding %q: the registry could not be read", hidden)
	}
	// THE BAR IS THE ONE THE USER SAW: a hidden workspace's tab follows its
	// run's drawn tabs, so the candidates (and, among never-selected ones,
	// the neighbours) are read off the bar as it stood with the workspace
	// still shown.
	target, by, found := hideLandingTarget(sidebar.Tabs(asShown(reg, hidden), log), hidden)
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
	applied, err := v.selectCurrent(ctx, targetLog, target.ID, &hidden, Switch{History: RecordSelection})
	if err != nil {
		return fmt.Errorf("land after hiding %q: %w", hidden, err)
	}
	if !applied {
		return nil
	}
	targetLog.Info(opSetHidden, "landed the user on the workspace selected before the hidden one",
		dlog.Context{"place": target.Place, "by": string(by)})
	return nil
}

// asShown answers REG with workspace WS not hidden, for the bar as it stood
// before WS was hidden. It copies the workspaces: the slice is the
// registry's.
func asShown(reg sidebar.Registry, ws ids.WorkspaceID) sidebar.Registry {
	reg.Workspaces = slices.Clone(reg.Workspaces)
	for i := range reg.Workspaces {
		if reg.Workspaces[i].ID == ws {
			reg.Workspaces[i].Hidden = false
			reg.Workspaces[i].HiddenAt = nil
		}
	}
	return reg
}

// hideLanding names which rule chose a hide's landing, for the record.
type hideLanding string

const (
	// landedByRecency: the most recently selected drawn tab.
	landedByRecency hideLanding = "recency"
	// landedByNeighbour: no other drawn tab was ever selected, so the
	// nearest drawn tab left of the hidden one, else right of it.
	landedByNeighbour hideLanding = "neighbour"
)

// hideLandingTarget picks where hiding HIDDEN lands the user among TABS, the
// tab bar in registry order (sidebar.Tabs): the drawn tab other than HIDDEN
// selected most recently (wsm.Workspace.LastSelectedAt), the one the user
// was on before. Tab order decides only when no other drawn tab was ever
// selected: the nearest drawn tab left of HIDDEN's own, else the nearest to
// its right. It answers false when HIDDEN has no tab, or no other tab is
// drawn.
func hideLandingTarget(tabs []sidebar.Tab, hidden ids.WorkspaceID) (wsm.Workspace, hideLanding, bool) {
	at := slices.IndexFunc(tabs, func(t sidebar.Tab) bool { return t.Workspace.ID == hidden })
	if at < 0 {
		return wsm.Workspace{}, "", false
	}
	var recent *wsm.Workspace
	for i := range tabs {
		ws := &tabs[i].Workspace
		if i == at || tabs[i].Withheld != wsm.Drawn || ws.LastSelectedAt == nil {
			continue
		}
		if recent == nil || ws.LastSelectedAt.After(*recent.LastSelectedAt) {
			recent = ws
		}
	}
	if recent != nil {
		return *recent, landedByRecency, true
	}
	for i := at - 1; i >= 0; i-- {
		if tabs[i].Withheld == wsm.Drawn {
			return tabs[i].Workspace, landedByNeighbour, true
		}
	}
	for i := at + 1; i < len(tabs); i++ {
		if tabs[i].Withheld == wsm.Drawn {
			return tabs[i].Workspace, landedByNeighbour, true
		}
	}
	return wsm.Workspace{}, "", false
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

// ownedPair refuses RPC unless this daemon serves both ws and target, as
// `owned' does for one workspace, and answers ws's logger naming target.
func (v *verbs) ownedPair(ctx context.Context, rpc string, ws, target ids.WorkspaceID) (dlog.Logger, error) {
	_, log, err := v.owned(ctx, rpc, ws)
	if err != nil {
		return nil, err
	}
	if _, _, err := v.owned(ctx, rpc, target); err != nil {
		return nil, err
	}
	return log.With(dlog.Context{"target": string(target)}), nil
}

// Swap exchanges the places of workspaces ws and target in the registry order,
// every other workspace keeping its own (wsm's SwapPlaces). The sidebar and the
// tab bar are both views of the registry, so one swap shows on both; two
// workspaces of different repositories are refused other_repository, and two
// that are not siblings not_siblings.
func (v *verbs) Swap(ctx context.Context, ws, target ids.WorkspaceID) error {
	log, err := v.ownedPair(ctx, "SwapWorkspaces", ws, target)
	if err != nil {
		return err
	}
	if err := v.deps.DB.SwapPlaces(ctx, ws, target); err != nil {
		if refused := placeRefusal(log, "SwapWorkspaces", err); refused != nil {
			return refused
		}
		log.Error(opSwap, "could not swap the workspaces' places", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("swap %q with %q: %w", ws, target, err)
	}
	log.Info(opSwap, "swapped the workspace's place with the target's", nil)
	v.republishRegistry(ctx, log, opSwap)
	return nil
}

// placeRefusal answers RPC's refusal for a store error that refuses taking
// another workspace's place (two repositories, two workspaces that are not
// siblings), nil for any other error. Swap and Move share it, so the two
// verbs answer one condition with one arm.
func placeRefusal(log dlog.Logger, rpc string, err error) *Refusal {
	switch {
	case errors.Is(err, wsm.ErrOtherRepository):
		return refuse(log, rpc, ArmOtherRepository, err.Error(), false)
	case errors.Is(err, wsm.ErrNotSiblings):
		return refuse(log, rpc, ArmNotSiblings, err.Error(), false)
	}
	return nil
}

// Move puts workspace ws onto target's tab in the arrangement's walk, the
// tabs between shifting one family toward the tab ws left (wsm's
// MoveWorkspace). The two must be siblings there: two workspaces of
// different repositories are refused other_repository, and any other
// pairing not_siblings.
func (v *verbs) Move(ctx context.Context, ws, target ids.WorkspaceID) error {
	const rpc = "MoveWorkspace"
	log, err := v.ownedPair(ctx, rpc, ws, target)
	if err != nil {
		return err
	}
	if err := v.deps.DB.MoveWorkspace(ctx, ws, target); err != nil {
		if refused := placeRefusal(log, rpc, err); refused != nil {
			return refused
		}
		log.Error(opMove, "could not move the workspace onto the target's tab", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("move %q onto %q: %w", ws, target, err)
	}
	log.Info(opMove, "moved the workspace onto the target's tab", nil)
	v.republishRegistry(ctx, log, opMove)
	return nil
}
