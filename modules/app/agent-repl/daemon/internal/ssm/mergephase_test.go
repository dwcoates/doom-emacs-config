package ssm

import (
	"regexp"
	"strconv"
	"testing"

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
