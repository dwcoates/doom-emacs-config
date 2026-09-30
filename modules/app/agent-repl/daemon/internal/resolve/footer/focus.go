package footer

import (
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// focusPanel is the expanded panel a focus names. The zero value is no focus
// at all: nothing has launched in this workspace yet.
type focusPanel int

const (
	focusNone focusPanel = iota
	focusAgents
	focusShells
	focusMonitors
	focusMergeTests
)

// String is the panel's name in the log.
func (p focusPanel) String() string {
	switch p {
	case focusAgents:
		return "agents"
	case focusShells:
		return "shells"
	case focusMonitors:
		return "monitors"
	case focusMergeTests:
		return "merge_tests"
	default:
		return "none"
	}
}

// focusState is the workspace's standing focus: the panel the last launch
// named and the generation it was minted under.
type focusState struct {
	panel      focusPanel
	generation uint64
}

// mintedFocus is one focus a set change minted, for the record the caller
// writes outside the lock.
type mintedFocus struct {
	panel      focusPanel
	generation uint64
	// trigger is the first launched item, in the live-work log's spelling.
	trigger string
	// launched is every item the set change launched.
	launched []string
}

// markAdopted records a detached item the watcher took up by ADOPTION rather
// than because it just started.
//
// ADOPTION IS THE ONE ANNOUNCEMENT WITH NO ANNOUNCER. The watcher routes an
// item the session says is ALREADY live (a crash boot, a handover, a pure
// attach learning the facts from the shim's re-announcement) through the same
// path a fresh announcement takes, with no announcing agent, and only then
// republishes the set. So the set change that follows lists the item as new to
// this resolver though nothing started: the mark is what keeps it from
// minting a focus. A subagent is marked under its created agent too, because
// the set names a subagent by its agent id.
func markAdopted(s *wsState, id string, work *conversationv1.AgentDetachedWork) {
	if id != "" {
		s.adoptedWork[id] = struct{}{}
	}
	created := work.GetCreated().GetWorkCreated().GetSubagent().GetStart().GetCreatedAgentId().GetValue()
	if created != "" {
		s.adoptedWork[created] = struct{}{}
	}
}

// launchedWork answers the items a set change LAUNCHED: ids the new set lists
// that the previous set did not, less the ones that did not just start.
//
// THE PREVIOUS SET IS THE BASELINE, NOT THE ROW LEDGER. A launch's descriptive
// announcement reaches the footer before the set that lists it, so the row
// already stands when the set arrives; reconcileLiveWork's `added` is empty
// for exactly the launches that matter. Diffing set against set means a
// re-take of the same set (a watcher republish, a reopened link) launches
// nothing.
//
// Two ids new to the set are still not launches: an ADOPTED item (see
// markAdopted), whose mark is consumed here since the set now carries it, and
// a RETIRED one, which is a replay of a run whose terminal already landed.
func launchedWork(s *wsState, previous, next LiveWorkSet) []string {
	var out []string
	take := func(prefix string, before, after map[string]struct{}) {
		for _, id := range sortedKeys(after) {
			if _, held := before[id]; held {
				continue
			}
			if _, adopted := s.adoptedWork[id]; adopted {
				delete(s.adoptedWork, id)
				continue
			}
			if _, done := s.retiredWork[id]; done {
				continue
			}
			out = append(out, prefix+id)
		}
	}
	take("agent:", agentIDSet(previous.Agents), agentIDSet(next.Agents))
	take("shell:", workIDSet(previous.Shells), workIDSet(next.Shells))
	take("monitor:", workIDSet(previous.Monitors), workIDSet(next.Monitors))
	return out
}

// focusOf is the highest-priority kind the set holds live: AGENTS, then
// SHELLS, then MONITORS. It is read off the whole set rather than the launched
// item, so a shell started while a subagent runs re-focuses the agents.
func focusOf(live LiveWorkSet) focusPanel {
	switch {
	case len(live.Agents) > 0:
		return focusAgents
	case len(live.Shells) > 0:
		return focusShells
	case len(live.Monitors) > 0:
		return focusMonitors
	default:
		return focusNone
	}
}

// mintFocus sets a new focus for the launches a set change carried, under the
// next generation. No launch, no focus: the standing one is left alone so the
// user's own clicks stand until the next launch.
func mintFocus(s *wsState, launched []string, live LiveWorkSet) *mintedFocus {
	if len(launched) == 0 {
		return nil
	}
	panel := focusOf(live)
	if panel == focusNone {
		return nil
	}
	s.focus.generation++
	s.focus.panel = panel
	return &mintedFocus{panel: panel, generation: s.focus.generation, trigger: launched[0], launched: launched}
}

// mintMergeTestsFocus sets the merge tests panel as the focus, under the next
// generation, when round is a testing round the footer has not seen yet; it
// answers nil otherwise, leaving the standing focus alone.
func mintMergeTestsFocus(s *wsState, round int) *mintedFocus {
	if round <= s.merge.TestsRound {
		return nil
	}
	s.focus.generation++
	s.focus.panel = focusMergeTests
	trigger := fmt.Sprintf("merge_tests:%d", round)
	return &mintedFocus{panel: focusMergeTests, generation: s.focus.generation, trigger: trigger, launched: []string{trigger}}
}

// focusView renders the standing focus. UNSET until the first launch.
func focusView(f focusState) *frontendv1.FooterExpandedFocus {
	out := &frontendv1.FooterExpandedFocus{Generation: f.generation}
	switch f.panel {
	case focusAgents:
		out.Panel = &frontendv1.FooterExpandedFocus_Agents{Agents: &frontendv1.FooterFocusAgents{}}
	case focusShells:
		out.Panel = &frontendv1.FooterExpandedFocus_Shells{Shells: &frontendv1.FooterFocusShells{}}
	case focusMonitors:
		out.Panel = &frontendv1.FooterExpandedFocus_Monitors{Monitors: &frontendv1.FooterFocusMonitors{}}
	case focusMergeTests:
		out.Panel = &frontendv1.FooterExpandedFocus_MergeTests{MergeTests: &frontendv1.FooterFocusMergeTests{}}
	default:
		return nil
	}
	return out
}
