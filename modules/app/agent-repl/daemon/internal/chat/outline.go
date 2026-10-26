package chat

import (
	"context"

	frontendv1 "agentrepl/proto/frontend/v1"
	zettelv1 "agentrepl/proto/zettel/v1"

	"agent-repld/internal/ids"
)

// THE OUTLINE: a thread's branches around its shown path, drawn under its row
// in the sidebar (frontend.v1.RosterThreadOutline; owner ruling, 2026-10-25:
// chain collapsing with re-rooting). Composed here because this service holds
// the graph and the shown path, and handed to the roster (Deps.Outline).

const (
	// OutlineLift is how many runs above the shown run the outline's rows
	// start; every run above them folds into the breadcrumb.
	OutlineLift = 3
	// OutlineWindow is the deepest row's depth below the first row's. A run
	// at that depth folds whatever branches below it into a count.
	OutlineWindow = 3
	// BreadcrumbWhole is the most folded runs the breadcrumb names whole;
	// past it, it names the oldest and the newest and counts the rest.
	BreadcrumbWhole = 3
)

// run is a chain of nodes each followed by exactly one more: one outline row.
type run struct {
	nodes  []NodeID
	parent *run
	kids   []*run
}

// runs splits the thread's graph into runs: the root run and each node's run.
// Called under Service.mu.
func (t *thread) runs() (*run, map[NodeID]*run) {
	roots := t.children("")
	if len(roots) == 0 {
		return nil, nil
	}
	of := map[NodeID]*run{}
	var build func(head NodeID, parent *run) *run
	build = func(head NodeID, parent *run) *run {
		r := &run{parent: parent}
		for at := head; ; {
			r.nodes = append(r.nodes, at)
			of[at] = r
			kids := t.children(at)
			if len(kids) != 1 {
				for _, k := range kids {
					r.kids = append(r.kids, build(k, r))
				}
				return r
			}
			at = kids[0]
		}
	}
	return build(roots[0], nil), of
}

// outline composes the thread's outline from its shown path, or nil while the
// path is empty. Called under Service.mu, after walk.
func (t *thread) outline() *frontendv1.RosterThreadOutline {
	root, of := t.runs()
	if root == nil || len(t.path) == 0 {
		return nil
	}
	onPath := map[NodeID]bool{}
	for _, id := range t.path {
		onPath[id] = true
	}
	shown := of[t.path[len(t.path)-1]]
	focus := shown
	for i := 0; i < OutlineLift && focus.parent != nil; i++ {
		focus = focus.parent
	}
	out := &frontendv1.RosterThreadOutline{}
	var above []*run
	for r := focus.parent; r != nil; r = r.parent {
		above = append([]*run{r}, above...)
	}
	if len(above) > 0 {
		out.Breadcrumb = t.breadcrumb(above)
	}
	var draw func(r *run, depth uint32)
	draw = func(r *run, depth uint32) {
		row := &frontendv1.RosterThreadOutlineRow{
			Node:  &zettelv1.NodeId{Id: string(r.nodes[0])},
			Label: t.runLabel(r),
			Depth: depth,
			Turns: uint32(len(r.nodes)),
			Place: runPlace(r, shown, onPath),
		}
		out.Rows = append(out.Rows, row)
		if depth == OutlineWindow {
			if n := runsBelow(r); n > 0 {
				row.Folded = &frontendv1.RosterThreadOutlineFolded{Runs: n}
			}
			return
		}
		for _, k := range r.kids {
			draw(k, depth+1)
		}
	}
	draw(focus, 0)
	return out
}

// breadcrumb names the runs ABOVE the rows, oldest first: all of them, or the
// oldest and the newest with the rest counted.
func (t *thread) breadcrumb(above []*run) *frontendv1.RosterThreadOutlineBreadcrumb {
	crumb := func(r *run) *frontendv1.RosterThreadOutlineCrumb {
		return &frontendv1.RosterThreadOutlineCrumb{Node: &zettelv1.NodeId{Id: string(r.nodes[0])}, Label: t.runLabel(r)}
	}
	if len(above) <= BreadcrumbWhole {
		whole := &frontendv1.RosterThreadOutlineBreadcrumbWhole{}
		for _, r := range above {
			whole.Crumbs = append(whole.Crumbs, crumb(r))
		}
		return &frontendv1.RosterThreadOutlineBreadcrumb{Extent: &frontendv1.RosterThreadOutlineBreadcrumb_Whole{Whole: whole}}
	}
	return &frontendv1.RosterThreadOutlineBreadcrumb{Extent: &frontendv1.RosterThreadOutlineBreadcrumb_Elided{Elided: &frontendv1.RosterThreadOutlineBreadcrumbElided{
		First: crumb(above[0]), Omitted: uint32(len(above) - 2), Last: crumb(above[len(above)-1]),
	}}}
}

// runLabel is a run's first question: its abbreviation when it has one,
// else its text, and its text whole for the tooltip.
func (t *thread) runLabel(r *run) *frontendv1.RosterThreadOutlineLabel {
	q := t.nodes[r.nodes[0]].GetQuestion()
	text := q.GetText()
	if q.Abbreviation != nil {
		text = q.GetAbbreviation()
	}
	return &frontendv1.RosterThreadOutlineLabel{Text: text, Question: q.GetText()}
}

// runPlace is where a run stands against the shown path.
func runPlace(r, shown *run, onPath map[NodeID]bool) *frontendv1.RosterThreadOutlinePlace {
	if r == shown {
		return &frontendv1.RosterThreadOutlinePlace{Place: &frontendv1.RosterThreadOutlinePlace_Shown{Shown: &frontendv1.RosterThreadOutlineShown{}}}
	}
	for _, id := range r.nodes {
		if onPath[id] {
			return &frontendv1.RosterThreadOutlinePlace{Place: &frontendv1.RosterThreadOutlinePlace_OnPath{OnPath: &frontendv1.RosterThreadOutlineOnPath{}}}
		}
	}
	return &frontendv1.RosterThreadOutlinePlace{Place: &frontendv1.RosterThreadOutlinePlace_Off{Off: &frontendv1.RosterThreadOutlineOff{}}}
}

// runsBelow counts every run below R.
func runsBelow(r *run) uint32 {
	var n uint32
	for _, k := range r.kids {
		n += 1 + runsBelow(k)
	}
	return n
}

// Show moves a thread's shown path through NODE, each ancestor showing the
// child on the way to it, and names NODE as the view's reveal.
func (s *Service) Show(ctx context.Context, id ids.WorkspaceID, node NodeID) error {
	if err := s.ensureLoaded(ctx, id); err != nil {
		return err
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	t := s.state(id)
	if _, ok := t.nodes[node]; !ok {
		return ErrUnknownNode
	}
	for at := node; ; {
		parent := parentOf(t.nodes[at])
		if parent == "" {
			break
		}
		t.picks[parent] = at
		at = parent
	}
	t.revealSequence++
	t.reveal = node
	s.publish(t)
	return nil
}
