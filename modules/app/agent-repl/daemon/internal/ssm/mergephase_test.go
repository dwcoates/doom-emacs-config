package ssm

import (
	"regexp"
	"strconv"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"claude-repld/internal/workspace/merge"
)

// --- the phase read-back -----------------------------------------------------

// applyPhases drives a workspace through a sequence of merge phases, in order.
func applyPhases(t *testing.T, m *Manager, ws string, phases ...merge.Phase) {
	t.Helper()
	for _, phase := range phases {
		if err := m.ApplyMergeTransition(ws, string(phase), "test arrangement"); err != nil {
			t.Fatalf("ApplyMergeTransition(%s, %s): %v", ws, phase, err)
		}
	}
}

func TestWorkspacesAtMergePhaseFindsAPinnedWorkspace(t *testing.T) {
	// Arrange
	m, _, _ := openUnwiredTest(t, fakeResolver{})
	applyPhases(t, m, "ws1", merge.PhaseMergeEnqueuing)

	// Act
	got, err := m.WorkspacesAtMergePhase(merge.PhaseMergeEnqueuing)

	// Assert
	if err != nil {
		t.Fatalf("WorkspacesAtMergePhase: %v", err)
	}
	if len(got) != 1 || got[0] != "ws1" {
		t.Fatalf("workspaces = %v, want [ws1]", got)
	}
}

func TestWorkspacesAtMergePhaseIgnoresASupersededPhase(t *testing.T) {
	// Arrange — the workspace WAS enqueuing and has since moved on. Its
	// merge_enqueuing row lives in the log forever; only the newest merge row
	// says where it rests now.
	m, _, _ := openUnwiredTest(t, fakeResolver{})
	applyPhases(t, m, "ws1", merge.PhaseMergeEnqueuing, merge.PhaseMerging)

	// Act
	got, err := m.WorkspacesAtMergePhase(merge.PhaseMergeEnqueuing)

	// Assert — sweeping this workspace would fail a cherry-pick that is running.
	if err != nil {
		t.Fatalf("WorkspacesAtMergePhase: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("workspaces = %v, want none", got)
	}
}

func TestWorkspacesAtMergePhaseSeesAReEnqueuedWorkspace(t *testing.T) {
	// Arrange — a workspace that merged and is now attempting again rests on
	// the mark once more.
	m, _, _ := openUnwiredTest(t, fakeResolver{})
	applyPhases(t, m, "ws1", merge.PhaseMergeEnqueuing, merge.PhaseMergeFailed, merge.PhaseMergeEnqueuing)

	// Act
	got, err := m.WorkspacesAtMergePhase(merge.PhaseMergeEnqueuing)

	// Assert
	if err != nil {
		t.Fatalf("WorkspacesAtMergePhase: %v", err)
	}
	if len(got) != 1 || got[0] != "ws1" {
		t.Fatalf("workspaces = %v, want [ws1]", got)
	}
}

func TestWorkspacesAtMergePhaseSeparatesWorkspaces(t *testing.T) {
	// Arrange — one pinned, one that moved on.
	m, _, _ := openUnwiredTest(t, fakeResolver{})
	applyPhases(t, m, "ws-pinned", merge.PhaseMergeEnqueuing)
	applyPhases(t, m, "ws-running", merge.PhaseMergeEnqueuing, merge.PhaseMerging)

	// Act
	got, err := m.WorkspacesAtMergePhase(merge.PhaseMergeEnqueuing)

	// Assert
	if err != nil {
		t.Fatalf("WorkspacesAtMergePhase: %v", err)
	}
	if len(got) != 1 || got[0] != "ws-pinned" {
		t.Fatalf("workspaces = %v, want [ws-pinned]", got)
	}
}

func TestWorkspacesAtMergePhaseRefusesAnUnknownPhase(t *testing.T) {
	// Arrange
	m, _, _ := openUnwiredTest(t, fakeResolver{})

	// Act
	got, err := m.WorkspacesAtMergePhase(merge.Phase("merge_sideways"))

	// Assert — an unknown token would find nothing and report a clean system.
	if err == nil {
		t.Fatalf("WorkspacesAtMergePhase(%q) error = nil, want the unknown phase refused (got %v)", "merge_sideways", got)
	}
}

// --- the precedence placement ------------------------------------------------

// mergeRank reads a state's rank out of resolveQuery's prec VALUES table. The
// table is the SOLE precedence authority (no Go cond-ladder restates it), so
// pinning the placement means reading it from there.
func mergeRank(t *testing.T, state string) int {
	t.Helper()
	re := regexp.MustCompile(`\('` + regexp.QuoteMeta(state) + `','merge',(\d+)\)`)
	match := re.FindStringSubmatch(resolveQuery)
	if match == nil {
		t.Fatalf("resolveQuery has no merge-axis rank for %q", state)
	}
	rank, err := strconv.Atoi(match[1])
	if err != nil {
		t.Fatalf("rank for %q is not a number: %v", state, err)
	}
	return rank
}

func TestMergeEnqueuingRanksDirectlyBelowMergeQueued(t *testing.T) {
	// Arrange / Act — the weakest claim the merge axis can make: the attempt is
	// not even on a queue yet.
	enqueuing, queued := mergeRank(t, sigMergeEnqueuing), mergeRank(t, sigMergeQueued)

	// Assert — a HIGHER rank number is a weaker claim.
	if enqueuing != queued+1 {
		t.Fatalf("merge_enqueuing rank = %d, merge_queued rank = %d; want merge_enqueuing directly below", enqueuing, queued)
	}
}

func TestMergeEnqueuingIsTheWeakestMergeState(t *testing.T) {
	// Arrange / Act
	enqueuing := mergeRank(t, sigMergeEnqueuing)

	// Assert — every other merge state describes a merge something durable is
	// already known about, so each is the more specific truth.
	for _, state := range []string{sigMerging, sigMergeQueued, sigMergeConflict, sigMergeFailed, sigMerged} {
		if got := mergeRank(t, state); got >= enqueuing {
			t.Fatalf("%s rank = %d, merge_enqueuing rank = %d; want merge_enqueuing weakest", state, got, enqueuing)
		}
	}
}

// THE GUARANTEE for the two agent-driven phases: each is a merge-axis token the
// resolver reads back, so a run parked on one is not invisible to the boot sweep
// or to any other phase query.
func TestWorkspacesAtMergePhaseFindsTheAgentDrivenPhases(t *testing.T) {
	// Arrange.
	tests := []struct {
		name  string
		phase merge.Phase
	}{
		{name: "before action", phase: merge.PhaseMergeBeforeAction},
		{name: "after action", phase: merge.PhaseMergeAfterAction},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			m, _, _ := openUnwiredTest(t, fakeResolver{})
			applyPhases(t, m, "ws1", merge.PhaseMerging, tc.phase)

			// Act.
			got, err := m.WorkspacesAtMergePhase(tc.phase)

			// Assert.
			if err != nil {
				t.Fatalf("WorkspacesAtMergePhase(%s): %v", tc.phase, err)
			}
			if len(got) != 1 || got[0] != "ws1" {
				t.Fatalf("workspaces = %v, want [ws1]", got)
			}
		})
	}
}

// THE VIOLATION EDGE: a run that reached the after-action and then merged must
// no longer read as resting on the after-action. Without the token in the
// resolver's latest_merge CTE the newest row would be invisible and the sweep
// would see a phase the run has left.
func TestWorkspacesAtMergePhaseDropsASupersededAfterAction(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{})
	applyPhases(t, m, "ws1", merge.PhaseMergeAfterAction, merge.PhaseMerged)

	// Act.
	got, err := m.WorkspacesAtMergePhase(merge.PhaseMergeAfterAction)

	// Assert.
	if err != nil {
		t.Fatalf("WorkspacesAtMergePhase: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("workspaces = %v, want none", got)
	}
}

// The two agent-driven phases make the same actionability claim `merging` does
// — the merge run owns this session — so they share its rank.
func TestTheAgentDrivenPhasesShareMergingsRank(t *testing.T) {
	// Arrange / Act.
	merging := mergeRank(t, sigMerging)

	// Assert.
	for _, token := range []string{sigMergeBeforeAction, sigMergeAfterAction} {
		if got := mergeRank(t, token); got != merging {
			t.Fatalf("%s rank = %d, want merging's %d", token, got, merging)
		}
	}
}

// Both phases project to MERGING: the render state answers what the user can
// do, and a merge run owns the session in either.
func TestTheAgentDrivenPhasesRenderAsMerging(t *testing.T) {
	// Arrange / Act / Assert.
	for _, token := range []string{sigMergeBeforeAction, sigMergeAfterAction} {
		if got := renderStateOf(token); got != frontendv1.RenderState_RENDER_STATE_MERGING {
			t.Fatalf("renderStateOf(%q) = %v, want RENDER_STATE_MERGING", token, got)
		}
	}
}

func TestTheMergeAxisSelectionCoversEveryMergeToken(t *testing.T) {
	// Arrange — resolveQuery's latest_merge CTE and the sweep's own axis list
	// must name the same tokens, or "the latest merge row" means two different
	// things in the resolver and in the boot sweep.
	// Act / Assert
	for _, token := range mergeAxisStates {
		if !regexp.MustCompile(`'` + regexp.QuoteMeta(token) + `'`).MatchString(resolveQuery) {
			t.Fatalf("resolveQuery never mentions the merge token %q", token)
		}
	}
}
