package merge

import (
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
)

// permissionEdge is a consent ask's edge: open, or decided.
func permissionEdge(id string, open bool) *conversationv1.AgentPermission {
	p := &conversationv1.AgentPermission{Id: &conversationv1.AgentPermissionId{Value: id}}
	if open {
		p.Result = &conversationv1.AgentPermission_Start{Start: &conversationv1.AgentPermissionStart{}}
	} else {
		p.Result = &conversationv1.AgentPermission_Success{Success: &conversationv1.AgentPermissionSuccess{}}
	}
	return p
}

// questionEdge is a question batch's edge: open, or answered.
func questionEdge(id string, open bool) *conversationv1.AgentQuestion {
	q := &conversationv1.AgentQuestion{Id: &conversationv1.AgentQuestionId{Value: id}}
	if open {
		q.Result = &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{}}
	} else {
		q.Result = &conversationv1.AgentQuestion_Success{Success: &conversationv1.AgentQuestionSuccess{}}
	}
	return q
}

func TestAsksWaitWhileAnyAskIsOpen(t *testing.T) {
	tests := []struct {
		name  string
		edges func(a *Asks)
		want  bool
	}{
		{"nothing asked", func(*Asks) {}, false},
		{"an open permission", func(a *Asks) { a.OnPermission(theWorkspace, nil, permissionEdge("p", true)) }, true},
		{"a decided permission", func(a *Asks) {
			a.OnPermission(theWorkspace, nil, permissionEdge("p", true))
			a.OnPermission(theWorkspace, nil, permissionEdge("p", false))
		}, false},
		{"an open question", func(a *Asks) { a.OnQuestion(theWorkspace, nil, questionEdge("q", true)) }, true},
		{"an answered question", func(a *Asks) {
			a.OnQuestion(theWorkspace, nil, questionEdge("q", true))
			a.OnQuestion(theWorkspace, nil, questionEdge("q", false))
		}, false},
		{"one of two asks decided", func(a *Asks) {
			a.OnPermission(theWorkspace, nil, permissionEdge("p", true))
			a.OnQuestion(theWorkspace, nil, questionEdge("q", true))
			a.OnPermission(theWorkspace, nil, permissionEdge("p", false))
		}, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			a := NewAsks()

			// Act
			tt.edges(a)

			// Assert
			if got := a.Waiting(theWorkspace); got != tt.want {
				t.Fatalf("waiting = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAsksTellTheListenerOnlyWhenTheStandingChanges(t *testing.T) {
	// Arrange
	a := NewAsks()
	var told []bool
	a.bind(func(_ ids.WorkspaceID, waiting bool) { told = append(told, waiting) })

	// Act
	a.OnPermission(theWorkspace, nil, permissionEdge("p", true))
	a.OnQuestion(theWorkspace, nil, questionEdge("q", true))
	a.OnPermission(theWorkspace, nil, permissionEdge("p", false))
	a.OnQuestion(theWorkspace, nil, questionEdge("q", false))

	// Assert
	if want := []bool{true, false}; !slices.Equal(told, want) {
		t.Fatalf("told = %v, want %v", told, want)
	}
}

// tabStates names the state arm of every push of one tab kind, in order.
func (f *fakeFeed) tabStates(kind string) []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	var out []string
	for _, row := range f.rows {
		tab := row.Row.GetMergeTab()
		if tab == nil || tabKindOf(tab) != kind {
			continue
		}
		var state any
		switch k := tab.GetKind().(type) {
		case *frontendv1.FeedMergeTab_Conflicts:
			state = k.Conflicts.GetState()
		case *frontendv1.FeedMergeTab_Fixes:
			state = k.Fixes.GetState()
		}
		switch state.(type) {
		case *frontendv1.FeedMergeTabConflicts_Live, *frontendv1.FeedMergeTabFixes_Live:
			out = append(out, "live")
		case *frontendv1.FeedMergeTabConflicts_WaitingOnUser, *frontendv1.FeedMergeTabFixes_WaitingOnUser:
			out = append(out, "waiting")
		default:
			out = append(out, "settled")
		}
	}
	return out
}

// askDuringTurn opens a permission while the merge's ORIGIN turn runs, and
// decides it again when decide is set.
func askDuringTurn(h *harness, origin conversationv1.PromptOrigin, decide bool) {
	h.queue.onSubmit = func(sub promptqueue.Submission) {
		if sub.Origin != origin {
			return
		}
		h.asks.OnPermission(theWorkspace, nil, permissionEdge("p-1", true))
		if decide {
			h.asks.OnPermission(theWorkspace, nil, permissionEdge("p-1", false))
		}
	}
}

func TestAConflictResolutionThatAsksIsDrawnWaitingOnTheUser(t *testing.T) {
	// Arrange
	h := newHarness(t)
	landing(h, 2)
	h.git.rebaseConflicts[2] = []string{"a.go"}
	askDuringTurn(h, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR, false)

	// Act
	admitted(t, h)

	// Assert
	if got, want := h.feed.tabStates(TabConflicts), []string{"live", "waiting", "settled"}; !slices.Equal(got, want) {
		t.Fatalf("conflicts tab states = %v, want %v", got, want)
	}
}

func TestAnAnsweredAskDrawsTheConflictsTabLiveAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	landing(h, 2)
	h.git.rebaseConflicts[2] = []string{"a.go"}
	askDuringTurn(h, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR, true)

	// Act
	admitted(t, h)

	// Assert
	if got, want := h.feed.tabStates(TabConflicts), []string{"live", "waiting", "live", "settled"}; !slices.Equal(got, want) {
		t.Fatalf("conflicts tab states = %v, want %v", got, want)
	}
}

func TestAFixingAttemptThatAsksIsDrawnWaitingOnTheUser(t *testing.T) {
	// Arrange
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.gateFails("daemon")
	h.gatePasses("daemon")
	askDuringTurn(h, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR, false)

	// Act
	admitted(t, h)

	// Assert
	if got, want := h.feed.tabStates(TabFixes), []string{"live", "waiting", "settled"}; !slices.Equal(got, want) {
		t.Fatalf("fixes tab states = %v, want %v", got, want)
	}
}

func TestAnAskAlreadyOpenDrawsTheConflictsTabWaitingAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	landing(h, 2)
	h.git.rebaseConflicts[2] = []string{"a.go"}
	h.asks.OnQuestion(theWorkspace, nil, questionEdge("q-1", true))

	// Act
	admitted(t, h)

	// Assert
	if got := h.feed.tabStates(TabConflicts); len(got) == 0 || got[0] != "waiting" {
		t.Fatalf("conflicts tab states = %v, want it drawn waiting first", got)
	}
}

func TestAnAskChangeWithNoAgenticTabLiveDrawsNoTab(t *testing.T) {
	// Arrange
	h := newHarness(t)
	landing(h, 1)
	h.feed.onUpsert = func(row *frontendv1.FeedRow) {
		if tab := row.GetMergeTab(); tab != nil && tabKindOf(tab) == TabTests {
			h.asks.OnPermission(theWorkspace, nil, permissionEdge("p-1", true))
		}
	}

	// Act
	admitted(t, h)

	// Assert
	if got := h.feed.tabStates(TabConflicts); len(got) != 0 {
		t.Fatalf("conflicts tab states = %v, want none drawn", got)
	}
}
