package sidebar

import (
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

// resolveArms answers every registry workspace's status arm, once per render,
// so the visibility (which reads it) is known before any row is drawn and
// every row draws the arm it was judged by. The caller holds r.mu.
func (r *resolver) resolveArms(rc rowContext) map[ids.WorkspaceID]string {
	arms := make(map[ids.WorkspaceID]string, len(r.state.reg.Workspaces))
	for _, rec := range r.state.reg.Workspaces {
		s := r.state.workspace(rec.ID)
		rowLog := r.logFor(rec.ID)
		if s.restoreResult(rec) {
			rowLog.Info("daemon.sidebar.result_restored",
				"the roster drew the last turn result from the durable record, as it stood before this daemon", dlog.Context{
					"end": s.restoredEnd, "result": s.result.String(),
				})
		}
		if arm, mirrored := r.state.incumbentArms[rec.ID]; mirrored {
			// A ROW THIS JOINING DAEMON HAS NOT ADOPTED YET DRAWS THE
			// INCUMBENT'S OWN ARM. This daemon's footer state for the workspace
			// is frozen at what it last read, since the incumbent still serves
			// it; the incumbent's arm is its own footer's resolved claim, checked
			// there against that footer (statusArm), so the projection is
			// skipped here rather than run against a frozen footer.
			rowLog.Debug("daemon.sidebar.status_decision", "the roster drew the incumbent's status arm for a workspace not yet adopted",
				dlog.Context{"arm": arm, "source": "incumbent"})
			arms[rec.ID] = arm
			continue
		}
		arms[rec.ID] = statusArm(s, rec, rc.sessions[rec.ID], r.footer.Status(rec.ID), r.now(), rowLog)
	}
	return arms
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
	input := func(id ids.WorkspaceID) wsm.VisibilityInput {
		arm := arms[id]
		s := r.state.workspace(id)
		return wsm.VisibilityInput{
			ID:       id,
			Working:  workingArms[arm],
			Unviewed: isTurnEndArm(arm) && !s.viewedOn(arm),
			Selected: rc.selected != nil && *rc.selected == id,
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
// keeps it for the rows and for Visibility, and records each change since the
// value last handed to the sink (seeded from the registry's persisted one),
// so only a real change is written. The caller holds r.mu.
func (r *resolver) resolveVisibility(repos []openRepo, rc rowContext, arms map[ids.WorkspaceID]string) {
	derived := wsm.Visibilities(r.visibilityInputs(repos, rc, arms))
	r.state.visibility = derived
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
		r.logFor(rec.ID).Info("daemon.sidebar.visibility_changed", "the roster derived a new visibility for the workspace",
			dlog.Context{"visibility": v.String(), "previous": kept.String()})
		if r.visibilities == nil {
			continue
		}
		if r.pendingVisibilities == nil {
			r.pendingVisibilities = map[ids.WorkspaceID]wsm.Visibility{}
		}
		r.pendingVisibilities[rec.ID] = v
	}
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
// it never races the store's write of the same value. A workspace no render
// has derived is NotVisible.
func (r *resolver) Visibility(ws ids.WorkspaceID) wsm.Visibility {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.state.visibility[ws]
}
