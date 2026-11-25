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
	"agent-repld/internal/resolve/nameprefix"
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
	return v.selectLocked(ctx, log, ws, from, sw)
}

// selectLocked is selectCurrent's body, for a caller that already holds the
// selection lock (a collapse that lands the user off the section it folds,
// recordFold).
func (v *verbs) selectLocked(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, from *ids.WorkspaceID, sw Switch) (bool, error) {

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
	v.selected()

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
		// THE SELECTION IS A VISIBILITY EDGE (wsm.Workspace.Runs): the
		// workspace the user looks at runs even backlogged, and the one they left
		// is withheld again if it is. It is also what ends a settings visit the
		// switch left (cachelapse.Keeper.own). The row is written above, before either
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
	if reg, err := v.readRegistry(ctx, log, opSelect); err == nil {
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
	reg, err := v.readRegistry(ctx, log, opFollowSelected)
	if err != nil {
		// readRegistry recorded why; the roster keeps the selection it had
		// until the next commit tries again.
		return fmt.Errorf("follow the stored selection %q: %w", *stored, err)
	}
	v.deps.Sidebar.SetRegistrySelected(reg, *stored, sidebar.SelectStamp{})
	v.selected()
	previous := ""
	if ok {
		previous = string(shown)
	}
	log.Info(opFollowSelected, "the roster took the selection another daemon stored", dlog.Context{
		"workspace": string(*stored), "previous": previous,
	})
	return nil
}

// Interaction is how the user interacted with a workspace, for the record:
// every kind has the same effect (RecordInteraction).
type Interaction string

// The interactions a workspace's interaction is recorded for: the three an
// editor or a page reports (agentrepl.v1.UserInteraction), and the link a
// reader followed in its page, which the daemon receives itself.
const (
	InteractionFeedScrolled  Interaction = "feed_scrolled"
	InteractionComposerTyped Interaction = "composer_typed"
	InteractionFeedNavigated Interaction = "feed_navigated"
	InteractionLinkFollowed  Interaction = "link_followed"
)

// RecordInteraction records the user's interaction with the workspace, in
// this order: the interaction instant is written durably first
// (wsm.SetInteractedAt), so the recency the roster then states is one a
// restart keeps, and the roster is told (Sidebar.SetInteracted), which draws
// the row viewed until its next turn starts or ends.
//
// It does not revive a parked session, because interacting with a page is not
// prompting it, and it is idempotent: interacting again with a viewed
// workspace only moves its instant.
func (v *verbs) RecordInteraction(ctx context.Context, ws ids.WorkspaceID, kind Interaction) error {
	_, log, err := v.owned(ctx, "ReportUserInteraction", ws)
	if err != nil {
		return err
	}
	at := v.now()
	fields := dlog.Context{"interaction": string(kind), "at": at.UTC()}
	if err := v.deps.DB.SetInteractedAt(ctx, ws, at); err != nil {
		log.Error(opInteraction, "could not record the user's interaction instant", dlog.WithCause(fields, err))
		return fmt.Errorf("record interaction with %q: %w", ws, err)
	}
	// EVERY INTERACTION IS RECORDED AT INFO: it orders the recency walk and
	// marks the row viewed, so a walk that lands somewhere unexpected is read
	// off these records.
	log.Info(opInteraction, "the user interacted with the workspace; it counts toward recency and may view its row", fields)
	v.deps.Sidebar.SetInteracted(ws, at)
	return nil
}

// SetBacklogged moves a workspace into the roster's backlog, or back out of
// it, stamping when it was backlogged. The roster hoists a backlogged workspace's row
// out of every grouping, and the editor's tab bar stops drawing its tab off
// that same push. HIDING HOLDS ITS KEEP-ALIVES (cachelapse.Keeper): its cache
// is left to lapse, and its session is stood down when it does, so the user
// can still show it again warm until then. SHOWING runs them again, and starts
// nothing by itself; the editor asks with OpenTabs. HIDING THE WORKSPACE THE
// USER STANDS ON LANDS THEM on the workspace they selected before it
// (landAfterHiding). Asking for the place it already holds is success.
//
// A SHOW ENDS WITH THE WORKSPACE DRAWN, OR IT IS AN ERROR. Two separate facts
// keep a workspace out of the Open section: the backlogged flag, and a merge
// workspace's unrevealed standing (wsm.Workspace.MergeHidden). A show clears
// both (revealMergeHidden, whose reveal answers an error unless the workspace
// ends drawable, then the flag), so it never answers success over a workspace
// the sidebar still draws nowhere (2026-11-21: a show of a backlogged
// merge workspace wrote only the flag and left it undrawn).
//
// A REVEAL IS ONE-WAY, SO HIDING A REVEALED MERGE WORKSPACE IS AN ORDINARY
// HIDE: it writes the backlogged flag and the workspace is listed in the backlog
// like any other, where the user can see it and show it again. It
// never goes back to being drawn nowhere, which only its async merge does to
// it, before anyone has asked to see it.
func (v *verbs) SetBacklogged(ctx context.Context, ws ids.WorkspaceID, backlogged bool) error {
	record, log, err := v.owned(ctx, "SetWorkspaceBacklogged", ws)
	if err != nil {
		return err
	}
	if !backlogged {
		if err := v.revealMergeHidden(ctx, record); err != nil {
			return err
		}
	} else if _, err := v.interruptBeforeLeaving(ctx, log, ws, opSetBacklogged, "backlog"); err != nil {
		// A RUNNING TURN IS INTERRUPTED AHEAD OF THE BACKLOG (owner ruling,
		// 2026-11-24): its page unloads and its shim parks.
		return err
	}
	if err := v.recordBacklogged(ctx, log, ws, backlogged); err != nil {
		return err
	}
	if !backlogged {
		return nil
	}
	// HIDING PASSES A STANDING COLD GATE WITHOUT AN ANSWER (owner ruling,
	// 2026-11-11): a merge kept at its place at the gate leaves its queue.
	v.deps.Merge.OnBacklogged(ws)
	return v.landAfterHiding(ctx, log, ws)
}

// revealMergeHidden is a show's reveal of a hidden merge workspace, through
// the same reveal a select makes (merge.Orchestrator.RevealMergeWorkspace), so
// the requester's bubble head gains its switch either way. Any other workspace
// is left as it is. The orchestrator owns the reveal's guarantee and its
// records: it answers an error, recorded at ERROR, unless the workspace ends
// drawable, and that error refuses the show before the flag is written.
func (v *verbs) revealMergeHidden(ctx context.Context, record wsm.Workspace) error {
	if !record.MergeHidden() {
		return nil
	}
	if err := v.deps.Merge.RevealMergeWorkspace(ctx, record.ID, "it was shown"); err != nil {
		return fmt.Errorf("show %q: %w", record.ID, err)
	}
	return nil
}

// recordBacklogged is THE ONE WRITE of a workspace's backlogged flag: the row, the
// roster push, and the cache keeper's visibility edge. SetBacklogged and an
// answered cold gate (AnswerColdGate) both reach it.
func (v *verbs) recordBacklogged(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, backlogged bool) error {
	if err := v.deps.DB.SetBacklogged(ctx, ws, backlogged, v.now()); err != nil {
		// A FAMILY IS BACKLOGGED CHILD FIRST AND SHOWN PARENT FIRST
		// (wsm/family.go): the refusal is the answer.
		if refusedByFamily(log, opSetBacklogged, err) {
			return err
		}
		log.Error(opSetBacklogged, "could not record whether the workspace is backlogged", dlog.Context{"backlogged": backlogged, "cause": err.Error()})
		return fmt.Errorf("set backlogged on %q: %w", ws, err)
	}
	log.Info(opSetBacklogged, "recorded whether the workspace is backlogged", dlog.Context{"backlogged": backlogged})
	v.backloggedRecorded(ctx, log, ws, opSetBacklogged)
	return nil
}

// backloggedRecorded is what follows every recorded move into or out of the
// backlog (recordBacklogged, BacklogColdGate): the roster push and the
// cache keeper's visibility edge.
func (v *verbs) backloggedRecorded(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, op string) {
	v.republishRegistry(ctx, log, op)
	// THE ROW IS WRITTEN BEFORE THE EDGE IS QUEUED, which is what lets the
	// keeper's reconcile catch a hide that races a session coming up.
	v.deps.CacheLapses.VisibilityChanged(ws)
}

// BacklogColdGate moves a shown workspace whose cold gate has stood too long
// into the backlog, drawn as the Backlog (owner request, 2026-10-26;
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
		log.Debug(opBacklogColdGate, "the workspace was selected, backlogged, closed or already backlogged for this gate; it is left as it is", nil)
		return nil
	}
	log.Info(opBacklogColdGate, "moved the workspace into the backlog: its cold gate stood for over a weekday", nil)
	v.backloggedRecorded(ctx, log, ws, opBacklogColdGate)
	return nil
}

// landAfterHiding moves the user, when they stand on the workspace just
// backlogged, to the drawn tab nearest it they selected most recently
// (sidebar.LandingTarget: its ancestors, then its subsection, then its
// repository, then its repository's main workspace). The select is
// conditional on the backlogged workspace still being current, under the
// selection lock, so a switch the user made meanwhile stands. With no other
// drawn tab the selection stays where it is, and the record says so.
func (v *verbs) landAfterHiding(ctx context.Context, log dlog.Logger, backlogged ids.WorkspaceID) error {
	reg, err := v.readRegistry(ctx, log, opSetBacklogged)
	if err != nil {
		return fmt.Errorf("land after hiding %q: %w", backlogged, err)
	}
	// THE BAR IS THE ONE THE USER SAW: a backlogged workspace's tab follows its
	// run's drawn tabs, so the candidates (and, among never-selected ones,
	// the neighbours) are read off the bar as it stood with the workspace
	// still shown.
	shown := asShown(reg, backlogged)
	at := slices.IndexFunc(shown.Workspaces, func(ws wsm.Workspace) bool { return ws.ID == backlogged })
	if at < 0 {
		log.Error(opSetBacklogged, "the registry does not carry the workspace just backlogged", nil)
		return fmt.Errorf("land after hiding %q: the registry does not carry it", backlogged)
	}
	target, by, found := sidebar.LandingTarget(shown, sidebar.Tabs(shown, log), shown.Workspaces[at], nil)
	if !found {
		log.Info(opSetBacklogged, "the tab bar draws no other workspace beside the backlogged one; the selection stays", nil)
		return nil
	}
	// THE LANDING IS A SELECT LIKE ANY OTHER, so it is taken only by the
	// daemon that serves the workspace it lands on (owned refuses it, and
	// records why, during a handover).
	_, targetLog, err := v.owned(ctx, "SelectWorkspace", target.ID)
	if err != nil {
		return err
	}
	targetLog = targetLog.With(dlog.Context{"backlogged": string(backlogged)})
	applied, err := v.selectCurrent(ctx, targetLog, target.ID, &backlogged, Switch{History: RecordSelection})
	if err != nil {
		return fmt.Errorf("land after hiding %q: %w", backlogged, err)
	}
	if !applied {
		return nil
	}
	targetLog.Info(opSetBacklogged, "landed the user on the workspace selected before the backlogged one",
		dlog.Context{"place": target.Place, "by": string(by)})
	return nil
}

// asShown answers REG with workspace WS not backlogged, for the bar as it stood
// before WS was backlogged. It copies the workspaces: the slice is the
// registry's.
func asShown(reg sidebar.Registry, ws ids.WorkspaceID) sidebar.Registry {
	reg.Workspaces = slices.Clone(reg.Workspaces)
	for i := range reg.Workspaces {
		if reg.Workspaces[i].ID == ws {
			reg.Workspaces[i].Backlogged = false
			reg.Workspaces[i].BackloggedAt = nil
		}
	}
	return reg
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
	record, log, err := v.owned(ctx, "SetWorkspacePriority", ws)
	if err != nil {
		return err
	}
	if err := v.deps.DB.SetPriority(ctx, ws, p); err != nil {
		log.Error(opSetPriority, "could not record the priority", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("set priority on %q: %w", ws, err)
	}
	log.Debug(opSetPriority, "recorded the priority", dlog.Context{"cleared": p == nil})
	v.republishRegistry(ctx, log, opSetPriority)
	// THE PRIORITY ICON IS A PREFIX OF THE NAME on every surface, so the
	// topbar's name line takes the same prefix the roster row now wears.
	record.Priority = p
	v.deps.Topbar.SetNamePrefix(ws, nameprefix.Of(record))
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
