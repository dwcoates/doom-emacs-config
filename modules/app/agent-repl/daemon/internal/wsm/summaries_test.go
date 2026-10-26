package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
)

const (
	boundaryNone  = shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE
	boundaryClear = shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_CLEAR
)

// distilled is a summary with the digest it was distilled from.
func distilled(title string, prompts int, boundary shimv1.TitleDigestBoundary) WorkspaceSummary {
	return WorkspaceSummary{Title: title, Digest: &SummaryDigest{Hash: "h-" + title, Prompts: prompts, Boundary: boundary}}
}

// mustPutSummary records SUM for WS, failing the test on any error.
func mustPutSummary(t *testing.T, s *store, ws WorkspaceID, sum WorkspaceSummary) {
	t.Helper()
	if err := s.PutWorkspaceSummary(context.Background(), ws, sum); err != nil {
		t.Fatalf("PutWorkspaceSummary(%+v): %v", sum, err)
	}
}

// summaryOf reads WS's summary back, failing the test on an error.
func summaryOf(t *testing.T, s *store, ws WorkspaceID) (WorkspaceSummary, bool) {
	t.Helper()
	sum, found, err := s.WorkspaceSummaryOf(context.Background(), ws)
	if err != nil {
		t.Fatalf("WorkspaceSummaryOf: %v", err)
	}
	return sum, found
}

func TestAWorkspaceSummaryRoundTrips(t *testing.T) {
	tests := []struct {
		name string
		sum  WorkspaceSummary
	}{
		{name: "with the digest it was distilled from", sum: distilled("Fix the topbar", 3, boundaryNone)},
		{name: "after a clear", sum: distilled("Fix the topbar", 0, boundaryClear)},
		{name: "with its digest reset", sum: WorkspaceSummary{Title: "Fix the topbar"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			mustPutSummary(t, s, ws.ID, tt.sum)

			// Assert
			got, found := summaryOf(t, s, ws.ID)
			if !found || !reflect.DeepEqual(got, tt.sum) {
				t.Fatalf("summary = %+v (found=%v), want %+v", got, found, tt.sum)
			}
		})
	}
}

func TestAWorkspaceWithNoSummaryReadsNone(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, found := summaryOf(t, s, ws.ID)

	// Assert
	if found {
		t.Fatalf("a workspace never summarized read a summary")
	}
}

func TestPuttingASummaryReplacesTheOneStanding(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustPutSummary(t, s, ws.ID, distilled("Fix the topbar", 1, boundaryNone))
	want := distilled("Persist the topbar summary", 6, boundaryNone)

	// Act
	mustPutSummary(t, s, ws.ID, want)

	// Assert
	if got, _ := summaryOf(t, s, ws.ID); !reflect.DeepEqual(got, want) {
		t.Fatalf("summary = %+v, want the replacement %+v", got, want)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM workspace_summaries`); n != 1 {
		t.Fatalf("%d summary rows stand, want 1", n)
	}
}

func TestASummaryOfAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.PutWorkspaceSummary(context.Background(), "ws-gone", distilled("Fix the topbar", 1, boundaryNone))

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("PutWorkspaceSummary = %v, want ErrNotFound", err)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM workspace_summaries`); n != 0 {
		t.Fatalf("%d summary rows stand, want none", n)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "debug", Operation: "daemon.wsm.put_workspace_summary"}); !ok {
		t.Fatalf("no debug record of the unknown workspace: %+v", log.Records())
	}
}

func TestAMalformedSummaryIsRefused(t *testing.T) {
	tests := []struct {
		name string
		sum  WorkspaceSummary
	}{
		{name: "an empty title", sum: WorkspaceSummary{Title: ""}},
		{name: "a digest with no hash", sum: WorkspaceSummary{Title: "t", Digest: &SummaryDigest{Prompts: 1, Boundary: boundaryNone}}},
		{name: "a negative prompt count", sum: WorkspaceSummary{Title: "t", Digest: &SummaryDigest{Hash: "h", Prompts: -1, Boundary: boundaryNone}}},
		{name: "an unspecified boundary", sum: WorkspaceSummary{Title: "t", Digest: &SummaryDigest{Hash: "h", Prompts: 1}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := s.PutWorkspaceSummary(context.Background(), ws.ID, tt.sum)

			// Assert
			if err == nil {
				t.Fatalf("PutWorkspaceSummary accepted %+v, want a refusal", tt.sum)
			}
			if _, found := summaryOf(t, s, ws.ID); found {
				t.Fatalf("a refused summary was stored")
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.put_workspace_summary"})
			if !ok || rec.Context["workspace"] != string(ws.ID) {
				t.Fatalf("no error record naming the workspace: %+v", log.Records())
			}
		})
	}
}

func TestResettingTheDigestKeepsTheTitle(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustPutSummary(t, s, ws.ID, distilled("Fix the topbar", 4, boundaryNone))

	// Act
	if err := s.ResetWorkspaceSummaryDigest(context.Background(), ws.ID); err != nil {
		t.Fatalf("ResetWorkspaceSummaryDigest: %v", err)
	}

	// Assert
	want := WorkspaceSummary{Title: "Fix the topbar"}
	if got, found := summaryOf(t, s, ws.ID); !found || !reflect.DeepEqual(got, want) {
		t.Fatalf("summary = %+v (found=%v), want %+v", got, found, want)
	}
}

func TestResettingTheDigestOfAnUnsummarizedWorkspaceWritesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.ResetWorkspaceSummaryDigest(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("ResetWorkspaceSummaryDigest = %v, want nil", err)
	}
	if _, found := summaryOf(t, s, ws.ID); found {
		t.Fatalf("a reset created a summary")
	}
}

func TestDroppingASummaryDeletesIt(t *testing.T) {
	tests := []struct {
		name string
		seed bool
	}{
		{name: "one stands", seed: true},
		{name: "none stands", seed: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if tt.seed {
				mustPutSummary(t, s, ws.ID, distilled("Fix the topbar", 1, boundaryNone))
			}

			// Act
			err := s.DropWorkspaceSummary(context.Background(), ws.ID)

			// Assert
			if err != nil {
				t.Fatalf("DropWorkspaceSummary = %v, want nil", err)
			}
			if _, found := summaryOf(t, s, ws.ID); found {
				t.Fatalf("a dropped summary still stands")
			}
		})
	}
}

func TestDroppingOneSummaryLeavesAnotherWorkspaces(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	other := testWorkspaceNamed(t, s, "other")
	mustPutSummary(t, s, ws.ID, distilled("Fix the topbar", 1, boundaryNone))
	want := distilled("Fix the footer", 2, boundaryNone)
	mustPutSummary(t, s, other.ID, want)

	// Act
	if err := s.DropWorkspaceSummary(context.Background(), ws.ID); err != nil {
		t.Fatalf("DropWorkspaceSummary: %v", err)
	}

	// Assert
	if got, found := summaryOf(t, s, other.ID); !found || !reflect.DeepEqual(got, want) {
		t.Fatalf("the other workspace's summary = %+v (found=%v), want %+v", got, found, want)
	}
}

func TestForgettingAWorkspaceDeletesItsSummary(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustPutSummary(t, s, ws.ID, distilled("Fix the topbar", 1, boundaryNone))

	// Act
	if _, err := s.Forget(context.Background(), ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM workspace_summaries`); n != 0 {
		t.Fatalf("%d summary rows outlived their workspace, want none", n)
	}
}

func TestACorruptStoredSummaryIsADecodeError(t *testing.T) {
	tests := []struct {
		name  string
		query string
	}{
		{name: "a digest half NULL", query: `UPDATE workspace_summaries SET digest_hash = NULL`},
		{name: "an unknown boundary", query: `UPDATE workspace_summaries SET digest_boundary = 9`},
		{name: "an empty title", query: `UPDATE workspace_summaries SET title = ''`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			mustPutSummary(t, s, ws.ID, distilled("Fix the topbar", 1, boundaryNone))
			corrupt(t, s, tt.query)

			// Act
			_, found, err := s.WorkspaceSummaryOf(context.Background(), ws.ID)

			// Assert
			var decode *DecodeError
			if !errors.As(err, &decode) || decode.Table != "workspace_summaries" {
				t.Fatalf("WorkspaceSummaryOf = %v, want a DecodeError on workspace_summaries", err)
			}
			if found {
				t.Fatalf("a corrupt summary read as found")
			}
		})
	}
}

func TestListingSummariesAnswersEveryStoredSummaryByWorkspace(t *testing.T) {
	tests := []struct {
		name   string
		stored map[string]WorkspaceSummary
	}{
		{name: "none stored", stored: map[string]WorkspaceSummary{}},
		{name: "one stored beside an unsummarized workspace", stored: map[string]WorkspaceSummary{"first": distilled("Fix the topbar", 1, boundaryNone)}},
		{name: "two stored, one with its digest reset", stored: map[string]WorkspaceSummary{
			"first":  distilled("Fix the topbar", 1, boundaryNone),
			"second": {Title: "Fix the footer"},
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			want := map[WorkspaceID]WorkspaceSummary{}
			for _, name := range []string{"first", "second", "third"} {
				ws := testWorkspaceNamed(t, s, name)
				if sum, ok := tt.stored[name]; ok {
					mustPutSummary(t, s, ws.ID, sum)
					want[ws.ID] = sum
				}
			}

			// Act
			got, err := s.ListWorkspaceSummaries(context.Background())

			// Assert
			if err != nil {
				t.Fatalf("ListWorkspaceSummaries: %v", err)
			}
			if !reflect.DeepEqual(got, want) {
				t.Fatalf("ListWorkspaceSummaries = %+v, want %+v", got, want)
			}
		})
	}
}

func TestListingSummariesKeepsAClosedWorkspacesSummary(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	want := distilled("Fix the topbar", 1, boundaryNone)
	mustPutSummary(t, s, ws.ID, want)
	if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: time.Unix(1_700_000_000, 0), By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	got, err := s.ListWorkspaceSummaries(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("ListWorkspaceSummaries: %v", err)
	}
	if !reflect.DeepEqual(got[ws.ID], want) {
		t.Fatalf("a closed workspace's summary = %+v, want %+v", got[ws.ID], want)
	}
}

func TestListingSummariesFailsWholeOnACorruptRow(t *testing.T) {
	// Arrange
	s, sink := testStore(t)
	ws := testWorkspace(t, s)
	mustPutSummary(t, s, ws.ID, distilled("Fix the topbar", 1, boundaryNone))
	mustPutSummary(t, s, testWorkspaceNamed(t, s, "other").ID, distilled("Fix the footer", 1, boundaryNone))
	corrupt(t, s, `UPDATE workspace_summaries SET title = '' WHERE workspace_id = '`+string(ws.ID)+`'`)

	// Act
	got, err := s.ListWorkspaceSummaries(context.Background())

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Table != "workspace_summaries" || decode.Row != string(ws.ID) {
		t.Fatalf("ListWorkspaceSummaries = %v, want a DecodeError on workspace_summaries row %s", err, ws.ID)
	}
	if got != nil {
		t.Fatalf("a failed read answered %+v, want nothing", got)
	}
	if !loggedOperation(sink, "daemon.wsm.list_workspace_summaries", "error") {
		t.Fatalf("the failed read was not recorded at ERROR: %v", sink.Records())
	}
}
