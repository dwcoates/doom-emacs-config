// Package prioritywire is the ONE mapping between a workspace's roster
// priority as the registry stores it (wsm.Priority) and as the contract and
// the command line spell it (agentrepl.v1.WorkspacePriority, `-priority
// p05|p1|p2|p3`). Every reader and writer of a priority goes through it, so
// a level added to one spelling cannot be missed by another.
package prioritywire

import (
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/wsm"
)

// Of reads a contract priority, false when its level is unset.
func Of(p *agentreplv1.WorkspacePriority) (wsm.Priority, bool) {
	switch p.GetLevel().(type) {
	case *agentreplv1.WorkspacePriority_P05:
		return wsm.PriorityP05, true
	case *agentreplv1.WorkspacePriority_P1:
		return wsm.PriorityP1, true
	case *agentreplv1.WorkspacePriority_P2:
		return wsm.PriorityP2, true
	case *agentreplv1.WorkspacePriority_P3:
		return wsm.PriorityP3, true
	}
	return 0, false
}

// Proto spells P as the contract does. A level the registry does not declare
// is a defect, and panics.
func Proto(p wsm.Priority) *agentreplv1.WorkspacePriority {
	switch p {
	case wsm.PriorityP05:
		return &agentreplv1.WorkspacePriority{Level: &agentreplv1.WorkspacePriority_P05{P05: &agentreplv1.WorkspacePriorityP05{}}}
	case wsm.PriorityP1:
		return &agentreplv1.WorkspacePriority{Level: &agentreplv1.WorkspacePriority_P1{P1: &agentreplv1.WorkspacePriorityP1{}}}
	case wsm.PriorityP2:
		return &agentreplv1.WorkspacePriority{Level: &agentreplv1.WorkspacePriority_P2{P2: &agentreplv1.WorkspacePriorityP2{}}}
	case wsm.PriorityP3:
		return &agentreplv1.WorkspacePriority{Level: &agentreplv1.WorkspacePriority_P3{P3: &agentreplv1.WorkspacePriorityP3{}}}
	}
	panic(fmt.Sprintf("prioritywire: no priority %d", int(p)))
}

// spellings are the command line's, in level order.
var spellings = map[wsm.Priority]string{wsm.PriorityP05: "p05", wsm.PriorityP1: "p1", wsm.PriorityP2: "p2", wsm.PriorityP3: "p3"}

// Spelled spells P as `-priority` does. A level the registry does not
// declare is a defect, and panics.
func Spelled(p wsm.Priority) string {
	text, ok := spellings[p]
	if !ok {
		panic(fmt.Sprintf("prioritywire: no priority %d", int(p)))
	}
	return text
}

// Parse reads a `-priority` spelling, false for one that names no level.
func Parse(text string) (wsm.Priority, bool) {
	for p, spelled := range spellings {
		if spelled == text {
			return p, true
		}
	}
	return 0, false
}
