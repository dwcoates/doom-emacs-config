package sidebar

import (
	"maps"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE ROSTER DERIVES EVERY WORKSPACE'S VISIBILITY (wsm.Visibilities), on every
// render, because it alone holds every input: the registry (sections, folds,
// parents, hides, closes), the selection, and each row's status and read
// result. Each render states it on every row (frontend.v1.RosterRowVisibility)
// and hands every change to the visibility sink, which keeps it in wsm, so a
// boot reads it before it allocates anything. No other layer works it out:
// the editor, the web streams, the deploy and the run rule all read it.

// startingArm is the status arm of a workspace whose session is booting: the
// grey initializing mark. A fold leaves such a workspace visible.
const startingArm = "init"

// noSessionYetArm is the status arm of a workspace registered with no session
// created yet: the gap between its creation and its session's first dial. It
// is initializing too, and as unviewed as a workspace can be, so a fold leaves
// it visible (owner request, 2026-11-23).
const noSessionYetArm = "none"

// workingArms are the status arms of a workspace that WORKS: a turn or
// detached work runs, or it asks the user something. A fold leaves such a
// workspace visible.
var workingArms = map[string]bool{
	"submitting":    true,
	"thinking":      true,
	"vendor_silent": true,
	"clearing":      true,
	"compacting":    true,
	"permission":    true,
	"question":      true,
	"idle_async":    true,
	"api_retrying":  true,
}

// VisibilitySink is told every change of a workspace's visibility a render
// found, in one map per combined commit. The daemon keeps it durable
// (wsm.SetVisibilities). It is called off the resolver's lock, in the report
// every caller of a combined commit waits for, so it returns at once: the
// daemon's only submits the change.
type VisibilitySink func(changes map[ids.WorkspaceID]wsm.Visibility)

// WithVisibilitySink installs the sink the resolver tells its visibility
// changes.
func WithVisibilitySink(sink VisibilitySink) Option {
	return func(r *resolver) { r.visibilities = sink }
}

// VisibilityTelemetry is where the roster counts why each workspace is
// visible (runtimemetrics.Surface). Every method is a handful of atomic
// metric updates, called under the resolver's lock, so it never blocks.
type VisibilityTelemetry interface {
	// SetVisibilityCounts states how many workspaces stand under each reason
	// now, every reason named (0 for one none has).
	SetVisibilityCounts(counts map[wsm.VisibilityReason]int)
	// CountVisibilityChange counts one workspace's visibility changing from
	// FROM to TO, under REASON, the new visibility's reason.
	CountVisibilityChange(from, to wsm.Visibility, reason wsm.VisibilityReason)
	// CountResultRead counts one last turn's result made read, by what read
	// it (ResultReadBy).
	CountResultRead(by ResultReadBy)
}

// ResultReadBy is what marked a workspace's row viewed (its last result read):
// the closed vocabulary agentrepl_workspace_results_read_total counts by.
type ResultReadBy string

// The markers of a viewed row.
const (
	// ReadByInteraction is the user's interaction with the workspace
	// (SetInteracted) viewing its row on its status.
	ReadByInteraction ResultReadBy = "interaction"
)

// ResultReadersAll is every marker, for a caller that serves each from 0.
var ResultReadersAll = []ResultReadBy{ReadByInteraction}

// WithVisibilityTelemetry installs where the resolver counts visibilities,
// their changes and the results read.
func WithVisibilityTelemetry(t VisibilityTelemetry) Option {
	return func(r *resolver) { r.telemetry = t }
}

// countResultRead counts one result read BY. The caller holds r.mu.
func (r *resolver) countResultRead(by ResultReadBy) {
	if r.telemetry != nil {
		r.telemetry.CountResultRead(by)
	}
}

// resolveArms answers every registry workspace's status arm, once per render,
// so every row draws it, and the session arm the visibility is judged by
// (sessionArm: the status arm but for the opening marker), known before any
// row is drawn. The caller holds r.mu.
func (r *resolver) resolveArms(rc rowContext) (arms, judged map[ids.WorkspaceID]string) {
	arms = make(map[ids.WorkspaceID]string, len(r.state.reg.Workspaces))
	judged = make(map[ids.WorkspaceID]string, len(r.state.reg.Workspaces))
	for i := range r.state.reg.Workspaces {
		rec := r.state.reg.Workspaces[i]
		// AN ARM WHOSE INPUTS DID NOT MOVE IS KEPT (kept.go), so a change
		// derives only the arms it moved.
		k, key := r.state.keptOf(rec.ID), r.state.keyOf(rec.ID)
		if k.armKept && k.armKey == key && !armReadsClock(rec) {
			arms[rec.ID], judged[rec.ID] = k.arm, k.judged
			r.stats.armsKept++
			continue
		}
		s := r.state.workspace(rec.ID)
		rowLog := r.logFor(rec.ID)
		if s.restoreResult(rec) {
			rowLog.Info("daemon.sidebar.result_restored",
				"the roster drew the last turn result from the durable record, as it stood before this daemon", dlog.Context{
					"end": s.restoredEnd, "result": s.result.String(),
				})
		}
		s.restoreViewed(rec)
		if arm, source, held := r.heldArm(rec.ID, s); held {
			// A ROW THIS JOINING DAEMON HAS NOT ADOPTED YET, OR WHOSE FACTS ARE
			// NOT SETTLED, DRAWS THE INCUMBENT'S ARM (carry.go), for its
			// visibility as for its status. The incumbent's arm is its own
			// footer's resolved claim, checked there (statusArm), so the
			// projection is skipped here rather than run against facts this
			// daemon does not hold yet.
			rowLog.Debug("daemon.sidebar.status_decision", "the roster drew the incumbent's status arm for a workspace whose facts this daemon does not hold",
				dlog.Context{"arm": arm, "source": source})
			arms[rec.ID], judged[rec.ID] = arm, arm
			k.arm, k.judged, k.armKey, k.armKept = arm, arm, key, true
			continue
		}
		r.stats.armsDerived++
		judged[rec.ID] = sessionArm(s, rec, rc.sessions[rec.ID], r.footer.Status(rec.ID), r.now(), rowLog)
		arms[rec.ID] = openingOver(s, judged[rec.ID])
		k.arm, k.judged, k.armKey, k.armKept = arms[rec.ID], judged[rec.ID], key, true
	}
	return arms, judged
}

// visibilityInputs states every registry workspace's visibility inputs. An
// Open section row's parent is the row it is drawn under (the Open tree's
// nesting, transparent ancestors and repository blocks included), and it is
// covered when its region is folded or an ancestor's children are; every
// other workspace (hidden, closed, a hidden merge workspace, a thread) is not
// open. The caller holds r.mu.
func (r *resolver) visibilityInputs(repos []openRepo, rc rowContext, arms map[ids.WorkspaceID]string) []wsm.VisibilityInput {
	inputs := make([]wsm.VisibilityInput, 0, len(r.state.reg.Workspaces))
	seen := make(map[ids.WorkspaceID]bool, len(r.state.reg.Workspaces))
	topPriority := make(map[ids.WorkspaceID]bool, len(r.state.reg.Workspaces))
	for _, rec := range r.state.reg.Workspaces {
		topPriority[rec.ID] = rec.TopPriority()
	}
	input := func(id ids.WorkspaceID) wsm.VisibilityInput {
		arm := arms[id]
		s := r.state.workspace(id)
		return wsm.VisibilityInput{
			ID:          id,
			Working:     workingArms[arm],
			Unviewed:    isTurnEndArm(arm) && !s.viewed(),
			Starting:    arm == startingArm || arm == noSessionYetArm,
			TopPriority: topPriority[id],
			Selected:    rc.selected != nil && *rc.selected == id,
		}
	}
	var walk func(nodes []openNode, parent *ids.WorkspaceID, covered bool)
	walk = func(nodes []openNode, parent *ids.WorkspaceID, covered bool) {
		for _, node := range nodes {
			in := input(node.ws.ID)
			in.Open, in.Covered, in.Parent = true, covered, parent
			inputs = append(inputs, in)
			seen[node.ws.ID] = true
			id := node.ws.ID
			walk(flatten(node.children, node.blocks), &id, childrenFolded(node, covered))
		}
	}
	for _, repo := range repos {
		for _, region := range repo.regions {
			walk(region.rows, nil, region.folded(repo.repo))
		}
	}
	for _, ws := range r.state.reg.Workspaces {
		if !seen[ws.ID] {
			inputs = append(inputs, input(ws.ID))
		}
	}
	return inputs
}

// resolveVisibility derives every workspace's visibility for this render,
// keeps it for the rows and for Visibility, and collects each change since the
// value last handed to the sink (seeded from the registry's persisted one),
// so only a real change is written. Each change is recorded once the commit
// knows the roster it reaches (recordVisibilityMoves). The caller holds r.mu.
func (r *resolver) resolveVisibility(repos []openRepo, rc rowContext, arms map[ids.WorkspaceID]string) {
	inputs := r.visibilityInputs(repos, rc, arms)
	reasons := wsm.VisibilityReasons(inputs)
	derived := make(map[ids.WorkspaceID]wsm.Visibility, len(reasons))
	for id, reason := range reasons {
		derived[id] = reason.Visibility()
	}
	inputOf := make(map[ids.WorkspaceID]wsm.VisibilityInput, len(inputs))
	for _, in := range inputs {
		inputOf[in.ID] = in
	}
	r.state.visibility = derived
	r.countReasons(reasons)
	if r.state.visibilityKept == nil {
		r.state.visibilityKept = map[ids.WorkspaceID]wsm.Visibility{}
	}
	for _, rec := range r.state.reg.Workspaces {
		v := derived[rec.ID]
		kept, ok := r.state.visibilityKept[rec.ID]
		if !ok {
			kept = rec.Visibility
		}
		if v == kept {
			r.state.visibilityKept[rec.ID] = v
			continue
		}
		r.state.visibilityKept[rec.ID] = v
		r.visibilityMoves = append(r.visibilityMoves, visibilityMove{
			ws: rec.ID, to: v, from: kept, reason: reasons[rec.ID], in: inputOf[rec.ID], arm: arms[rec.ID], cause: rc.causeOf(rec.ID),
		})
		if r.telemetry != nil {
			r.telemetry.CountVisibilityChange(kept, v, reasons[rec.ID])
		}
		if r.visibilities == nil {
			continue
		}
		if r.pendingVisibilities == nil {
			r.pendingVisibilities = map[ids.WorkspaceID]wsm.Visibility{}
		}
		r.pendingVisibilities[rec.ID] = v
	}
}

// visibilityMove is one workspace's visibility change a render found, with
// everything that decided it: the derivation's inputs, the status arm they
// were read off, and the operation whose fact was being rendered.
type visibilityMove struct {
	ws       ids.WorkspaceID
	to, from wsm.Visibility
	// reason is the rule of the derivation that decided TO.
	reason wsm.VisibilityReason
	in     wsm.VisibilityInput
	arm    string
	cause  string
}

// recordVisibilityMoves records every visibility change the commit's renders
// found, each with its cause and the inputs that decided it, and with the
// REVISION of the roster that carries it when PUBLISHED; a commit the quiet
// state held back publishes none, and its changes reach the roster the lid's
// opening publishes. Every editor opens and tears down a workspace's tab by
// its row's visibility, so these records are what says why a tab came or
// went (2026-11-23: six tabless workspaces opened and torn down within 3.5s
// of a handover, and no record said what made them visible). The caller
// holds r.mu.
func (r *resolver) recordVisibilityMoves(revision uint64, published bool) {
	for _, m := range r.visibilityMoves {
		ctx := dlog.Context{
			"visibility":   m.to.String(),
			"previous":     m.from.String(),
			"reason":       m.reason.String(),
			"cause":        m.cause,
			"arm":          m.arm,
			"open":         m.in.Open,
			"covered":      m.in.Covered,
			"working":      m.in.Working,
			"unviewed":     m.in.Unviewed,
			"starting":     m.in.Starting,
			"top_priority": m.in.TopPriority,
			"selected":     m.in.Selected,
			"published":    published,
		}
		if m.in.Parent != nil {
			ctx["parent"] = string(*m.in.Parent)
		}
		if published {
			ctx["revision"] = revision
		}
		r.logFor(m.ws).Info("daemon.sidebar.visibility_changed", "the roster derived a new visibility for the workspace", ctx)
	}
	r.visibilityMoves = nil
}

// countReasons hands telemetry how many registry workspaces stand under
// each reason, when that changed since the last render. The caller holds
// r.mu.
func (r *resolver) countReasons(reasons map[ids.WorkspaceID]wsm.VisibilityReason) {
	if r.telemetry == nil {
		return
	}
	counts := make(map[wsm.VisibilityReason]int, len(wsm.VisibilityReasonsAll))
	for _, reason := range wsm.VisibilityReasonsAll {
		counts[reason] = 0
	}
	for _, rec := range r.state.reg.Workspaces {
		counts[reasons[rec.ID]]++
	}
	if maps.Equal(counts, r.reasonCounts) {
		return
	}
	r.reasonCounts = counts
	r.telemetry.SetVisibilityCounts(counts)
}

// visibilityOf answers V on the wire.
func visibilityOf(v wsm.Visibility) *frontendv1.RosterRowVisibility {
	switch v {
	case wsm.UnselectedVisible:
		return &frontendv1.RosterRowVisibility{Visibility: &frontendv1.RosterRowVisibility_UnselectedVisible{UnselectedVisible: &frontendv1.RosterVisibilityUnselectedVisible{}}}
	case wsm.Selected:
		return &frontendv1.RosterRowVisibility{Visibility: &frontendv1.RosterRowVisibility_Selected{Selected: &frontendv1.RosterVisibilitySelected{}}}
	default:
		return &frontendv1.RosterRowVisibility{Visibility: &frontendv1.RosterRowVisibility_NotVisible{NotVisible: &frontendv1.RosterVisibilityNotVisible{}}}
	}
}

// Visibility answers workspace WS's visibility as the last render derived it:
// the value the roster just published, which the web stream's gate reads so
// it never races the store's write of the same value. A workspace the
// registry carries but no render has derived yet (a daemon that just booted or
// joined) answers the visibility the store kept for it, the one fact there is
// until the first render. A workspace the registry does not carry is
// NotVisible.
func (r *resolver) Visibility(ws ids.WorkspaceID) wsm.Visibility {
	r.awaitApplied()
	r.mu.Lock()
	defer r.mu.Unlock()
	if v, derived := r.state.visibility[ws]; derived {
		return v
	}
	if rec, ok := r.state.record(ws); ok {
		return rec.Visibility
	}
	return wsm.NotVisible
}
