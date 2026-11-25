package sidebar_test

import (
	"testing"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

func TestTheRosterRevisionRisesWithEveryResolvedRoster(t *testing.T) {
	// Arrange.
	r := arrange(t)
	before := latestPush(t, r).GetRevision()

	// Act.
	r.SetOpening(theWS, true)

	// Assert.
	if got := latestPush(t, r).GetRevision(); got != before+1 {
		t.Fatalf("revision = %d after one more resolve, want %d", got, before+1)
	}
}

func TestTheFirstResolvedRosterCarriesARevision(t *testing.T) {
	// Arrange, Act.
	r := arrange(t)

	// Assert.
	if got := latestPush(t, r).GetRevision(); got == 0 {
		t.Fatal("revision = 0, want the first resolved roster's revision raised")
	}
}

func TestRowAnswersTheLastResolvedRowWithItsRostersRevision(t *testing.T) {
	// Arrange.
	r := arrange(t)
	r.SetOpening(theWS, true)
	roster := latestPush(t, r)

	// Act.
	row, revision, ok := r.Row(theWS)

	// Assert.
	if !ok {
		t.Fatal("Row found no row for a registered workspace")
	}
	if revision != roster.GetRevision() {
		t.Fatalf("revision = %d, want the roster's %d", revision, roster.GetRevision())
	}
	if !proto.Equal(row, onlyRow(t, r)) {
		t.Fatalf("row = %v, want the roster's own row %v", row, onlyRow(t, r))
	}
}

func TestRevisionAnswersTheLastResolvedRostersRevision(t *testing.T) {
	// Arrange.
	r := arrange(t)

	// Act.
	r.SetOpening(theWS, true)

	// Assert.
	if got, want := r.Revision(), latestPush(t, r).GetRevision(); got != want || got == 0 {
		t.Fatalf("Revision = %d, want the published roster's %d", got, want)
	}
}

func TestRevisionIsZeroBeforeAnyRoster(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act / Assert.
	if got := r.Revision(); got != 0 {
		t.Fatalf("Revision = %d before any roster, want 0", got)
	}
}

func TestRowFollowsALaterResolve(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetOpening(theWS, true)

	// Act.
	r.SetOpening(theWS, false)
	row, revision, _ := r.Row(theWS)

	// Assert.
	if got := statusName(row); got != "ready" {
		t.Fatalf("status = %q, want the row as of the lowered opening", got)
	}
	if revision != latestPush(t, r).GetRevision() {
		t.Fatalf("revision = %d, want the latest roster's %d", revision, latestPush(t, r).GetRevision())
	}
}

func TestRowFindsABackloggedWorkspace(t *testing.T) {
	// Arrange.
	backlogged := backloggedWorkspace(string(theWS), "one")
	r := arrange(t, backlogged)

	// Act.
	_, _, ok := r.Row(theWS)

	// Assert.
	if !ok {
		t.Fatal("Row found no row for a backlogged workspace the roster carries")
	}
}

func TestRowFindsAWorkspaceInASubsection(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	later := subsection("sub-later", "later", 1)
	reg := registry(in(workspace(string(theWS), "one"), later))
	reg.Subsections = []wsm.Subsection{later}
	r.SetRegistry(reg)

	// Act.
	_, _, ok := r.Row(theWS)

	// Assert.
	if !ok {
		t.Fatal("Row found no row for a workspace in a subsection")
	}
}

func TestRowAnswersFalseForAWorkspaceTheRosterDoesNotCarry(t *testing.T) {
	// Arrange.
	r := arrange(t, workspace(string(theWS), "one"))

	// Act.
	_, _, ok := r.Row(ids.WorkspaceID("absent"))

	// Assert.
	if ok {
		t.Fatal("Row answered a row for a workspace the roster does not carry")
	}
}

func TestRowAnswersFalseBeforeAnyRegistry(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	_, _, ok := r.Row(theWS)

	// Assert.
	if ok {
		t.Fatal("Row answered a row before any roster was resolved")
	}
}

func TestRowAnswersACloneNotThePublishedRow(t *testing.T) {
	// Arrange.
	r := arrange(t, workspace(string(theWS), "one"))
	row, _, _ := r.Row(theWS)

	// Act.
	row.Name.Text = "changed"

	// Assert.
	if got := onlyRow(t, r).GetName().GetText(); got != "one" {
		t.Fatalf("published name = %q, want it untouched by a change to the answered row", got)
	}
}

func TestAnIdenticalRenderKeepsTheRevision(t *testing.T) {
	// Arrange.
	r := arrange(t, workspace(string(theWS), "one"))
	before := latestPush(t, r).GetRevision()

	// Act.
	r.SetRegistry(registry(workspace(string(theWS), "one")))

	// Assert.
	if _, got, _ := r.Row(theWS); got != before {
		t.Fatalf("revision = %d after an identical render, want %d kept", got, before)
	}
}

func TestRowOfAMergedWorkspaceIsResolvedOnDemand(t *testing.T) {
	// Arrange: a merged workspace beside an open one.
	merged := workspace("m", "merged")
	merged.MergedAt = at(0)
	merged.Closed = true
	r := arrange(t, merged, workspace(string(theWS), "one"))

	// Act.
	row, revision, ok := r.Row("m")

	// Assert: the page's row, stated with the last push's revision.
	if !ok {
		t.Fatal("Row found no row for a merged workspace")
	}
	if revision != latestPush(t, r).GetRevision() {
		t.Fatalf("revision = %d, want the last push's %d", revision, latestPush(t, r).GetRevision())
	}
	if !proto.Equal(row, rowFor(mergedRows(t, r), "m")) {
		t.Fatalf("row = %v, want the page's row", row)
	}
}

func TestRowOfAnUnknownWorkspaceAnswersNothing(t *testing.T) {
	// Arrange.
	r := arrange(t, workspace(string(theWS), "one"))

	// Act.
	_, _, ok := r.Row("nobody")

	// Assert.
	if ok {
		t.Fatal("Row answered a row for a workspace the registry does not hold")
	}
}
