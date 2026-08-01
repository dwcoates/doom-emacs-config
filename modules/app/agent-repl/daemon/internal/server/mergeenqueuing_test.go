package server

import (
	"context"
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/workspace/merge"
)

// The command handler's own half of the merge pipeline: the merge_enqueuing
// mark it emits on receipt, and the merge_failed it owes when the attempt never
// reaches the queue.
//
// The mark exists because merge_queued and merging both describe a merge the
// daemon has already accepted. Nothing described the window before that, so a
// merge_workspace that died in it — an unresolvable geometry, a refused
// enqueue — left the user with a command that simply vanished.

func TestMergeMarksEnqueuingBeforeItAsksTheCoordinator(t *testing.T) {
	// Arrange — the runner reads the recorded phases from INSIDE Enqueue, so
	// the ordering is observed rather than assumed.
	h, m, _, states := newMergeTestHandler(t)
	var atEnqueue []merge.Phase
	m.onEnqueue = func() { atEnqueue = states.phases() }

	// Act
	if err := h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{}); err != nil {
		t.Fatalf("MergeWorkspace: %v", err)
	}

	// Assert
	if len(atEnqueue) != 1 || atEnqueue[0] != merge.PhaseMergeEnqueuing {
		t.Fatalf("phases at Enqueue = %v, want exactly [%s]", atEnqueue, merge.PhaseMergeEnqueuing)
	}
}

func TestMergeMarksEnqueuingBeforeItResolvesTheGeometry(t *testing.T) {
	// Arrange — the geometry lookup records itself, and an unrecorded workspace
	// makes the lookup the first thing that can fail.
	h, _, g, states := newMergeTestHandler(t)

	// Act
	_ = h.MergeWorkspace(context.Background(), "ws-unknown", "r1", &frontendv1.MergeWorkspaceCmd{})

	// Assert — the lookup happened, and the mark is the phase that preceded it.
	if len(g.lookups) != 1 {
		t.Fatalf("geometry lookups = %v, want exactly one", g.lookups)
	}
	if got := states.phases(); len(got) == 0 || got[0] != merge.PhaseMergeEnqueuing {
		t.Fatalf("phases = %v, want %s first", got, merge.PhaseMergeEnqueuing)
	}
}

func TestMergeEnqueuingCarriesTheRequestIDAsItsCause(t *testing.T) {
	// Arrange
	h, _, _, states := newMergeTestHandler(t)

	// Act
	if err := h.MergeWorkspace(context.Background(), "ws1", "req-77", &frontendv1.MergeWorkspaceCmd{}); err != nil {
		t.Fatalf("MergeWorkspace: %v", err)
	}

	// Assert — the cause is what ties a pushed phase back to the command line
	// in the log.
	if len(states.got) == 0 || !strings.Contains(states.got[0].cause, "req-77") {
		t.Fatalf("first transition = %+v, want a cause naming the request id", states.got)
	}
}

func TestMergeEnqueuingIsKeyedOnTheEnvelopeWorkspace(t *testing.T) {
	// Arrange — the display name rides its own field and must never key state.
	h, _, _, states := newMergeTestHandler(t)

	// Act
	if err := h.MergeWorkspace(context.Background(), "ws1", "r1",
		&frontendv1.MergeWorkspaceCmd{WorkspaceName: "feature-one"}); err != nil {
		t.Fatalf("MergeWorkspace: %v", err)
	}

	// Assert
	if len(states.got) == 0 || states.got[0].workspace != "ws1" {
		t.Fatalf("first transition = %+v, want it keyed on ws1", states.got)
	}
}

func TestMergeResumeNeverMarksEnqueuing(t *testing.T) {
	// Arrange — a conflict_resolved_continue continues a merge that is already
	// queued and already holds its lease.
	h, _, _, states := newMergeTestHandler(t)

	// Act
	if err := h.MergeWorkspace(context.Background(), "ws1", "r1",
		&frontendv1.MergeWorkspaceCmd{ConflictResolvedContinue: true}); err != nil {
		t.Fatalf("MergeWorkspace: %v", err)
	}

	// Assert — marking it would walk a live merge_conflict backwards.
	if got := states.phases(); len(got) != 0 {
		t.Fatalf("phases = %v, want none on the resume path", got)
	}
}

func TestMergeEnqueueFailureRecordsMergeFailed(t *testing.T) {
	// Arrange
	h, m, _, states := newMergeTestHandler(t)
	m.enqueueErr = errors.New("disk full")

	// Act
	err := h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{})

	// Assert — the phase sequence ends terminal, never at merge_enqueuing.
	if err == nil {
		t.Fatalf("MergeWorkspace error = nil, want the enqueue refusal surfaced")
	}
	want := []merge.Phase{merge.PhaseMergeEnqueuing, merge.PhaseMergeFailed}
	if got := states.phases(); len(got) != 2 || got[0] != want[0] || got[1] != want[1] {
		t.Fatalf("phases = %v, want %v", got, want)
	}
}

func TestMergeEnqueueFailureCarriesTheCauseOntoTheFailedPhase(t *testing.T) {
	// Arrange
	h, m, _, states := newMergeTestHandler(t)
	m.enqueueErr = errors.New("disk full")

	// Act
	_ = h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{})

	// Assert — the pushed cause is the only explanation a UI ever gets.
	last := states.got[len(states.got)-1]
	if !strings.Contains(last.cause, "disk full") {
		t.Fatalf("merge_failed cause = %q, want the enqueue error in it", last.cause)
	}
}

func TestMergeEnqueueFailureStillNacksTheCommand(t *testing.T) {
	// Arrange
	h, m, _, _ := newMergeTestHandler(t)
	sentinel := errors.New("disk full")
	m.enqueueErr = sentinel

	// Act
	err := h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{})

	// Assert — the terminal phase is additional to the nack, never instead of it.
	if !errors.Is(err, sentinel) {
		t.Fatalf("MergeWorkspace error = %v, want the enqueue error preserved", err)
	}
}

func TestMergeGeometryFailureRecordsMergeFailed(t *testing.T) {
	// Arrange — an unrecorded workspace is refused before anything is enqueued.
	h, m, _, states := newMergeTestHandler(t)

	// Act
	err := h.MergeWorkspace(context.Background(), "ws-unknown", "r1", &frontendv1.MergeWorkspaceCmd{})

	// Assert
	if err == nil {
		t.Fatalf("MergeWorkspace error = nil, want the unrecorded geometry refused")
	}
	if len(m.merged) != 0 {
		t.Fatalf("merged = %v, want nothing enqueued", m.merged)
	}
	want := []merge.Phase{merge.PhaseMergeEnqueuing, merge.PhaseMergeFailed}
	if got := states.phases(); len(got) != 2 || got[0] != want[0] || got[1] != want[1] {
		t.Fatalf("phases = %v, want %v", got, want)
	}
}

func TestMergeIsRefusedWhenTheEnqueuingMarkCannotBeRecorded(t *testing.T) {
	// Arrange — the sink refuses everything.
	g := &fakeGeometry{}
	m := &fakeMerges{}
	h, err := newCommandHandler(&fakePrompts{}, m, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{MergeGeometry: g, MergeStates: &fakeMergeStates{err: errors.New("sink down")}})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act
	got := h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{})

	// Assert — a merge nothing can see is the state the mark exists to end, so
	// it is refused rather than run unmarked.
	if got == nil || !strings.Contains(got.Error(), string(merge.PhaseMergeEnqueuing)) {
		t.Fatalf("MergeWorkspace error = %v, want the unrecordable mark refused", got)
	}
	if len(m.merged) != 0 {
		t.Fatalf("merged = %v, want nothing enqueued", m.merged)
	}
}

func TestMergeWithoutAWiredStateSinkIsRefused(t *testing.T) {
	// Arrange — no sink at all.
	m := &fakeMerges{}
	h, err := newCommandHandler(&fakePrompts{}, m, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{MergeGeometry: &fakeGeometry{}})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act
	got := h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{})

	// Assert — an unwired capability is a loud ack, never a silently unmarked
	// merge.
	if got == nil || !strings.Contains(got.Error(), "merge state sink is not wired") {
		t.Fatalf("MergeWorkspace error = %v, want the unwired sink refused", got)
	}
	if len(m.merged) != 0 {
		t.Fatalf("merged = %v, want nothing enqueued", m.merged)
	}
}

func TestMergeFailedRecordFailureIsJoinedOntoTheNack(t *testing.T) {
	// Arrange — the mark lands, the terminal record does not, and the enqueue
	// is refused.
	g := &fakeGeometry{}
	m := &fakeMerges{enqueueErr: errors.New("disk full")}
	states := &fakeMergeStates{failPhase: merge.PhaseMergeFailed}
	h, err := newCommandHandler(&fakePrompts{}, m, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{MergeGeometry: g, MergeStates: states})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	// Act
	got := h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{
		SourceBranch: "DWC/x", SourceDir: "/worktrees/x", TargetDir: "/repo",
	})

	// Assert — neither failure hides the other.
	if got == nil || !strings.Contains(got.Error(), "disk full") ||
		!strings.Contains(got.Error(), string(merge.PhaseMergeFailed)) {
		t.Fatalf("MergeWorkspace error = %v, want both the enqueue refusal and the record failure", got)
	}
}
