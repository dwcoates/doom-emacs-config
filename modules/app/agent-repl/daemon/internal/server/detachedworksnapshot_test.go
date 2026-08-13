package server

import (
	"fmt"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// fakeDetachedWorks is a TaskCatalogSource whose work half returns a fixed
// list and whose roster half is empty — these tests are about the work, and
// the one interface carries both because a source of one is always a source of
// the other.
type fakeDetachedWorks struct{ work []*frontendv1.Message }

func (f fakeDetachedWorks) DetachedWork() []*frontendv1.Message     { return f.work }
func (f fakeDetachedWorks) TaskCatalogs() []*frontendv1.TaskCatalog { return nil }

// detachedWorkMessage is one piece of detached work as the MESSAGE that IS it:
// the uuid is the work's identity now that DetachedWork carries no id of its
// own, and the workspace stays on the payload, which is where the snapshot's
// routing refusal reads it from.
func detachedWorkMessage(uuid, workspace string) *frontendv1.Message {
	return &frontendv1.Message{
		Uuid:    uuid,
		Lineage: &frontendv1.MessageLineage{TopLevelMessageId: uuid},
		Payload: &frontendv1.Message_DetachedWork{DetachedWork: &frontendv1.DetachedWork{Workspace: workspace}},
	}
}

func TestSnapshotCarriesEveryOpenDetachedWork(t *testing.T) {
	provider := &ssmSnapshotProvider{
		catalogs:          fakeDetachedWorks{work: []*frontendv1.Message{detachedWorkMessage("detached-work:t1", "/ws")}},
		workspaceCreation: newFakeWorkspaceCreation(),
	}
	if got := len(provider.Snapshot().GetDetachedWork()); got != 1 {
		t.Fatalf("a reconnecting client must be told about every open work, got %d", got)
	}
}

func TestSnapshotRefusesADetachedWorkThatNamesNoWorkspace(t *testing.T) {
	provider := &ssmSnapshotProvider{
		catalogs:          fakeDetachedWorks{work: []*frontendv1.Message{detachedWorkMessage("detached-work:t1", "")}},
		workspaceCreation: newFakeWorkspaceCreation(),
	}
	if got := len(provider.Snapshot().GetDetachedWork()); got != 0 {
		t.Fatalf("a workspace-less work would reach every scoped client and must be refused, got %d", got)
	}
}

func TestSnapshotRecordsWhyItRefusedAWorkspacelessDetachedWork(t *testing.T) {
	var lines []string
	provider := &ssmSnapshotProvider{
		catalogs:          fakeDetachedWorks{work: []*frontendv1.Message{detachedWorkMessage("detached-work:t1", "")}},
		workspaceCreation: newFakeWorkspaceCreation(),
		logf:              func(format string, args ...any) { lines = append(lines, format) },
	}
	provider.Snapshot()
	var found bool
	for _, line := range lines {
		if strings.Contains(line, "REFUSING detached work") {
			found = true
		}
	}
	if !found {
		t.Fatalf("the refusal must be loud, got %v", lines)
	}
}

// NEW with the message vocabulary: StateSnapshot.detached_work is a list of
// MESSAGES now, so a message whose payload arm is not detached work is
// representable where a *DetachedWork list made it impossible. It carries no
// workspace at all, so it is refused for the same reason a workspace-less work
// is.
func TestSnapshotRefusesAMessageWhosePayloadIsNotDetachedWork(t *testing.T) {
	// Arrange
	provider := &ssmSnapshotProvider{
		catalogs: fakeDetachedWorks{work: []*frontendv1.Message{
			{Uuid: "m1", Lineage: &frontendv1.MessageLineage{TopLevelMessageId: "m1"}},
		}},
		workspaceCreation: newFakeWorkspaceCreation(),
	}

	// Act
	got := len(provider.Snapshot().GetDetachedWork())

	// Assert
	if got != 0 {
		t.Fatalf("detached_work = %d, want 0: a message with no detached-work arm carries no workspace and would reach every scoped client", got)
	}
}

func TestSnapshotRecordsWhyItRefusedAMessageWhosePayloadIsNotDetachedWork(t *testing.T) {
	// Arrange
	var lines []string
	provider := &ssmSnapshotProvider{
		catalogs: fakeDetachedWorks{work: []*frontendv1.Message{
			{Uuid: "m1", Lineage: &frontendv1.MessageLineage{TopLevelMessageId: "m1"}},
		}},
		workspaceCreation: newFakeWorkspaceCreation(),
		logf:              func(format string, args ...any) { lines = append(lines, fmt.Sprintf(format, args...)) },
	}

	// Act
	provider.Snapshot()

	// Assert
	var found bool
	for _, line := range lines {
		if strings.Contains(line, "REFUSING message") && strings.Contains(line, "m1") {
			found = true
		}
	}
	if !found {
		t.Fatalf("the refusal must name the message it dropped, got %v", lines)
	}
}

func TestSnapshotKeepsAWellFormedDetachedWorkBesideARefusedOne(t *testing.T) {
	provider := &ssmSnapshotProvider{
		catalogs: fakeDetachedWorks{work: []*frontendv1.Message{
			detachedWorkMessage("detached-work:bad", ""),
			detachedWorkMessage("detached-work:good", "/ws"),
		}},
		workspaceCreation: newFakeWorkspaceCreation(),
	}
	got := provider.Snapshot().GetDetachedWork()
	if len(got) != 1 || got[0].GetUuid() != "detached-work:good" {
		t.Fatalf("one defective work costs its own row, never the whole session view, got %v", got)
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

func TestSnapshotWithholdsADetachedWorkOfAPublicationHeldWorkspace(t *testing.T) {
	provider := &ssmSnapshotProvider{
		catalogs:          fakeDetachedWorks{work: []*frontendv1.Message{detachedWorkMessage("detached-work:t1", "/held")}},
		workspaceCreation: heldWorkspaceCreation("/held", "job-held"),
	}
	if got := len(provider.Snapshot().GetDetachedWork()); got != 0 {
		t.Fatalf("a work of a workspace the materialization latch holds must not reach the snapshot, got %d", got)
	}
}

func TestSnapshotRecordsTheLatchHoldingADetachedWorkBack(t *testing.T) {
	var lines []string
	provider := &ssmSnapshotProvider{
		catalogs:          fakeDetachedWorks{work: []*frontendv1.Message{detachedWorkMessage("detached-work:t1", "/held")}},
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
		t.Fatalf("the latch's hold on the work must be recorded, got %v", lines)
	}
}

func TestSnapshotKeepsADetachedWorkOfAMaterializedWorkspaceBesideAHeldOne(t *testing.T) {
	provider := &ssmSnapshotProvider{
		catalogs: fakeDetachedWorks{work: []*frontendv1.Message{
			detachedWorkMessage("detached-work:held", "/held"),
			detachedWorkMessage("detached-work:open", "/open"),
		}},
		workspaceCreation: heldWorkspaceCreation("/held", "job-held"),
	}
	got := provider.Snapshot().GetDetachedWork()
	if len(got) != 1 || got[0].GetUuid() != "detached-work:open" {
		t.Fatalf("the latch holds back one workspace's work, never every workspace's, got %v", got)
	}
}

func TestSnapshotHasNoDetachedWorkWithoutASource(t *testing.T) {
	provider := &ssmSnapshotProvider{workspaceCreation: newFakeWorkspaceCreation()}
	if got := len(provider.Snapshot().GetDetachedWork()); got != 0 {
		t.Fatalf("a nil source leaves the field empty rather than nil-derefing, got %d", got)
	}
}

// bothHalvesSource answers for a session's DETACHED WORK whole: the roster and
// the work. It is what *sessioncontroller.Manager is, and what the one
// TaskCatalogSource interface now requires.
type bothHalvesSource struct{}

func (bothHalvesSource) TaskCatalogs() []*frontendv1.TaskCatalog {
	return []*frontendv1.TaskCatalog{{Workspace: "/ws"}}
}
func (bothHalvesSource) DetachedWork() []*frontendv1.Message {
	return []*frontendv1.Message{detachedWorkMessage("detached-work:t1", "/ws")}
}

// The defect this collapse repairs: the roster and the work were two sources
// and two config fields, and a caller could wire one and forget the other — as
// the daemon's own e2e harness did, serving zero work on every reconnect
// with a live work outstanding. Wiring the ONE source must now produce both.

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

func TestOneDetachedWorkSourceServesTheDetachedWorkHalf(t *testing.T) {
	// Arrange
	provider := &ssmSnapshotProvider{catalogs: bothHalvesSource{}, workspaceCreation: newFakeWorkspaceCreation()}

	// Act
	got := len(provider.Snapshot().GetDetachedWork())

	// Assert
	if got != 1 {
		t.Fatalf("detached_works = %d, want 1: wiring the roster and getting no work is the reconnect defect this source collapse removes", got)
	}
}
