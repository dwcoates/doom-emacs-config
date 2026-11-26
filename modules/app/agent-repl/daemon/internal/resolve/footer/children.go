package footer

import (
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agentrepl/logging/logsite"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE ⇄ CHIP AND ITS PANEL ARE THE WORKSPACE'S OPEN CHILDREN
// (frontend.v1.FooterExpandedChildren). The children tracker (internal/children)
// is the one reader of which workspaces are a parent's children and how each
// stands; it hands the footer each parent's OPEN children WHOLE on every
// change (SetOpenChildren), so the panel draws exactly the children the
// tracker holds and never derives one of its own. A child that is not open is
// the topbar summary's menu's (frontend.v1.TopbarTitleChildMenu), so a child is
// drawn in exactly one of the two.

// OpenChild is one open child of a parent, as the footer draws it.
type OpenChild struct {
	// Ref is the child, echoed on a click that selects it.
	Ref *workspacev1.WorkspaceRef
	// Name is the child's workspace name.
	Name string
	// Status is the child's status word, as its sidebar row draws it.
	Status string
	// Tone is the child's canonical status color name, from
	// render-colors.json's `colors`, as its sidebar row draws it.
	Tone string
	// ShortSummary is the child's short summary, empty while it has none.
	ShortSummary string
	// WaitingOnUser reports that the child has a permission or a question
	// open for the user.
	WaitingOnUser bool
}

// SetOpenChildren installs PARENT's open children WHOLE, in the order they
// were created, oldest first. An empty list retires the chip and empties the
// panel.
//
// EVERY CHILD HANDED HAS A STATUS WORD AND A TONE. The children tracker lists
// a child only once the sidebar has decided its arm, and a row's tone is a
// color name of render-colors.json, which the webapp refuses when empty
// (2026-11-26: every WatchFooter push carrying a child the tracker had no arm
// for was skipped). A child without either is recorded at ERROR and the
// resolver fails hard rather than draw it.
func (r *resolver) SetOpenChildren(parent ids.WorkspaceID, children []OpenChild) {
	for _, child := range children {
		if child.Status != "" && child.Tone != "" {
			continue
		}
		r.mu.Lock()
		log := r.logOf(parent, r.stateLocked(parent))
		r.mu.Unlock()
		log.Error("daemon.footer.set_open_children", "an open child was handed to the footer without its status word or its tone",
			dlog.Context{
				"parent":              string(parent),
				"child_id":            child.Ref.GetId(),
				"child_name":          child.Name,
				"status":              child.Status,
				"tone":                child.Tone,
				"invariant_violation": "the children tracker hands only children whose arm is decided",
			})
		panic(fmt.Sprintf("footer: open child %q of %q handed without its status word or its tone", child.Ref.GetId(), parent))
	}
	r.mutate(parent, "daemon.footer.set_open_children", logsite.Msg("the footer took the workspace's open children"),
		dlog.Context{"children": len(children)}, func(s *wsState) { s.children = append([]OpenChild(nil), children...) })
}

// childrenChip is the ⇄ chip, nil when the workspace has no open child.
func (s *wsState) childrenChip() *frontendv1.FooterChipChildren {
	if len(s.children) == 0 {
		return nil
	}
	chip := &frontendv1.FooterChipChildren{Count: uint32(len(s.children))}
	waiting := 0
	for _, child := range s.children {
		if child.WaitingOnUser {
			waiting++
		}
	}
	if waiting > 0 {
		chip.WaitingOnUser = &frontendv1.FooterChipChildrenWaitingOnUser{Count: uint32(waiting)}
	}
	return chip
}

// childrenPanel is the ⇄ panel: one row per open child, in the tracker's
// order.
func (s *wsState) childrenPanel() *frontendv1.FooterExpandedChildren {
	panel := &frontendv1.FooterExpandedChildren{}
	for _, child := range s.children {
		row := &frontendv1.FooterChildRow{
			Name:   &frontendv1.FooterChildRowName{Text: child.Name},
			Status: &frontendv1.FooterChildRowStatus{Text: child.Status, Tone: child.Tone},
			Select: &frontendv1.FooterChildRowSelect{Target: child.Ref},
		}
		if child.ShortSummary != "" {
			row.ShortSummary = &frontendv1.FooterChildRowShortSummary{Text: child.ShortSummary}
		}
		panel.Rows = append(panel.Rows, row)
	}
	return panel
}
