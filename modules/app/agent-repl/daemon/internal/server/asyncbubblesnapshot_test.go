package server

import (
	"fmt"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// fakeAsyncBubbles is a TaskCatalogSource whose bubble half returns a fixed
// list and whose roster half is empty — these tests are about the bubbles, and
// the one interface carries both because a source of one is always a source of
// the other.
type fakeAsyncBubbles struct{ bubbles []*frontendv1.AsyncBubble }

func (f fakeAsyncBubbles) AsyncBubbles() []*frontendv1.AsyncBubble { return f.bubbles }
func (f fakeAsyncBubbles) TaskCatalogs() []*frontendv1.TaskCatalog { return nil }

func TestSnapshotCarriesEveryOpenBubble(t *testing.T) {
	provider := &ssmSnapshotProvider{
		catalogs:          fakeAsyncBubbles{bubbles: []*frontendv1.AsyncBubble{{Id: "bubble:t1", Workspace: "/ws"}}},
		workspaceCreation: newFakeWorkspaceCreation(),
	}
	if got := len(provider.Snapshot().GetAsyncBubbles()); got != 1 {
		t.Fatalf("a reconnecting client must be told about every open bubble, got %d", got)
	}
}

func TestSnapshotRefusesABubbleThatNamesNoWorkspace(t *testing.T) {
	provider := &ssmSnapshotProvider{
		catalogs:          fakeAsyncBubbles{bubbles: []*frontendv1.AsyncBubble{{Id: "bubble:t1"}}},
		workspaceCreation: newFakeWorkspaceCreation(),
	}
	if got := len(provider.Snapshot().GetAsyncBubbles()); got != 0 {
		t.Fatalf("a workspace-less bubble would reach every scoped client and must be refused, got %d", got)
	}
}

func TestSnapshotRecordsWhyItRefusedAWorkspacelessBubble(t *testing.T) {
	var lines []string
	provider := &ssmSnapshotProvider{
		catalogs:          fakeAsyncBubbles{bubbles: []*frontendv1.AsyncBubble{{Id: "bubble:t1"}}},
		workspaceCreation: newFakeWorkspaceCreation(),
		logf:              func(format string, args ...any) { lines = append(lines, format) },
	}
	provider.Snapshot()
	var found bool
	for _, line := range lines {
		if strings.Contains(line, "REFUSING async bubble") {
			found = true
		}
	}
	if !found {
		t.Fatalf("the refusal must be loud, got %v", lines)
	}
}

func TestSnapshotKeepsAWellFormedBubbleBesideARefusedOne(t *testing.T) {
	provider := &ssmSnapshotProvider{
		catalogs: fakeAsyncBubbles{bubbles: []*frontendv1.AsyncBubble{
			{Id: "bubble:bad"},
			{Id: "bubble:good", Workspace: "/ws"},
		}},
		workspaceCreation: newFakeWorkspaceCreation(),
	}
	got := provider.Snapshot().GetAsyncBubbles()
	if len(got) != 1 || got[0].GetId() != "bubble:good" {
		t.Fatalf("one defective bubble costs its own row, never the whole session view, got %v", got)
	}
}

// heldWorkspaceCreation is a fakeWorkspaceCreation whose materialization latch
// still holds the named workspace.
func heldWorkspaceCreation(workspace, jobID string) *fakeWorkspaceCreation {
	creation := newFakeWorkspaceCreation()
	creation.decisions[workspace+"\x00"] = SessionPublicationDecision{
		JobID:        jobID,
		WorktreePath: workspace,
		Materialized: false,
	}
	return creation
}

func TestSnapshotWithholdsABubbleOfAPublicationHeldWorkspace(t *testing.T) {
	provider := &ssmSnapshotProvider{
		catalogs:          fakeAsyncBubbles{bubbles: []*frontendv1.AsyncBubble{{Id: "bubble:t1", Workspace: "/held"}}},
		workspaceCreation: heldWorkspaceCreation("/held", "job-held"),
	}
	if got := len(provider.Snapshot().GetAsyncBubbles()); got != 0 {
		t.Fatalf("a bubble of a workspace the materialization latch holds must not reach the snapshot, got %d", got)
	}
}

func TestSnapshotRecordsTheLatchHoldingABubbleBack(t *testing.T) {
	var lines []string
	provider := &ssmSnapshotProvider{
		catalogs:          fakeAsyncBubbles{bubbles: []*frontendv1.AsyncBubble{{Id: "bubble:t1", Workspace: "/held"}}},
		workspaceCreation: heldWorkspaceCreation("/held", "job-held"),
		logf:              func(format string, args ...any) { lines = append(lines, fmt.Sprintf(format, args...)) },
	}
	provider.Snapshot()
	var found bool
	for _, line := range lines {
		if strings.Contains(line, "session publication HELD") && strings.Contains(line, "job-held") {
			found = true
		}
	}
	if !found {
		t.Fatalf("the latch's hold on the bubble must be recorded, got %v", lines)
	}
}

func TestSnapshotKeepsABubbleOfAMaterializedWorkspaceBesideAHeldOne(t *testing.T) {
	provider := &ssmSnapshotProvider{
		catalogs: fakeAsyncBubbles{bubbles: []*frontendv1.AsyncBubble{
			{Id: "bubble:held", Workspace: "/held"},
			{Id: "bubble:open", Workspace: "/open"},
		}},
		workspaceCreation: heldWorkspaceCreation("/held", "job-held"),
	}
	got := provider.Snapshot().GetAsyncBubbles()
	if len(got) != 1 || got[0].GetId() != "bubble:open" {
		t.Fatalf("the latch holds back one workspace's bubble, never every workspace's, got %v", got)
	}
}

func TestSnapshotHasNoBubblesWithoutASource(t *testing.T) {
	provider := &ssmSnapshotProvider{workspaceCreation: newFakeWorkspaceCreation()}
	if got := len(provider.Snapshot().GetAsyncBubbles()); got != 0 {
		t.Fatalf("a nil source leaves the field empty rather than nil-derefing, got %d", got)
	}
}

// bothHalvesSource answers for a session's DETACHED WORK whole: the roster and
// the bubbles. It is what *sessioncontroller.Manager is, and what the one
// TaskCatalogSource interface now requires.
type bothHalvesSource struct{}

func (bothHalvesSource) TaskCatalogs() []*frontendv1.TaskCatalog {
	return []*frontendv1.TaskCatalog{{Workspace: "/ws"}}
}
func (bothHalvesSource) AsyncBubbles() []*frontendv1.AsyncBubble {
	return []*frontendv1.AsyncBubble{{Id: "bubble:t1", Workspace: "/ws"}}
}

// The defect this collapse repairs: the roster and the bubbles were two sources
// and two config fields, and a caller could wire one and forget the other — as
// the daemon's own e2e harness did, serving zero bubbles on every reconnect
// with a live bubble outstanding. Wiring the ONE source must now produce both.

func TestOneDetachedWorkSourceServesTheRosterHalf(t *testing.T) {
	// Arrange
	provider := &ssmSnapshotProvider{catalogs: bothHalvesSource{}, workspaceCreation: newFakeWorkspaceCreation()}

	// Act
	got := len(provider.Snapshot().GetCatalogs())

	// Assert
	if got != 1 {
		t.Fatalf("catalogs = %d, want 1 from the one wired detached-work source", got)
	}
}

func TestOneDetachedWorkSourceServesTheBubbleHalf(t *testing.T) {
	// Arrange
	provider := &ssmSnapshotProvider{catalogs: bothHalvesSource{}, workspaceCreation: newFakeWorkspaceCreation()}

	// Act
	got := len(provider.Snapshot().GetAsyncBubbles())

	// Assert
	if got != 1 {
		t.Fatalf("async_bubbles = %d, want 1: wiring the roster and getting no bubbles is the reconnect defect this source collapse removes", got)
	}
}
