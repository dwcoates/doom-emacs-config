package wsm

import (
	"context"
	"database/sql"
	"errors"
	"reflect"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// summaryFor is a whole summary whose short summary is SHORT.
func summaryFor(short string) WorkspaceSummary {
	return WorkspaceSummary{Long: "The long form of " + short + ". It has two sentences.", Short: short}
}

// mustPutSummary records the whole summary whose short summary is SHORT for
// WS, failing the test on any error.
func mustPutSummary(t *testing.T, s *store, ws WorkspaceID, short string) {
	t.Helper()
	if err := s.PutWorkspaceSummary(context.Background(), ws, summaryFor(short)); err != nil {
		t.Fatalf("PutWorkspaceSummary(%q): %v", short, err)
	}
}

// summaryOf reads WS's summary back, failing the test on an error.
func summaryOf(t *testing.T, s *store, ws WorkspaceID) (WorkspaceSummary, bool) {
	t.Helper()
	summary, found, err := s.WorkspaceSummaryOf(context.Background(), ws)
	if err != nil {
		t.Fatalf("WorkspaceSummaryOf: %v", err)
	}
	return summary, found
}

func TestAWorkspaceSummaryRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	mustPutSummary(t, s, ws.ID, "Fix the topbar")

	// Assert
	if got, found := summaryOf(t, s, ws.ID); !found || got != summaryFor("Fix the topbar") {
		t.Fatalf("summary = %+v (found=%v), want %+v", got, found, summaryFor("Fix the topbar"))
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
	mustPutSummary(t, s, ws.ID, "Fix the topbar")

	// Act
	mustPutSummary(t, s, ws.ID, "Persist the topbar summary")

	// Assert
	if got, _ := summaryOf(t, s, ws.ID); got != summaryFor("Persist the topbar summary") {
		t.Fatalf("summary = %+v, want the replacement", got)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM workspace_summaries`); n != 1 {
		t.Fatalf("%d summary rows stand, want 1", n)
	}
}

func TestASummaryOfAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.PutWorkspaceSummary(context.Background(), "ws-gone", summaryFor("Fix the topbar"))

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

func TestASummaryMissingAPartIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		summary WorkspaceSummary
		want    error
	}{
		{name: "no short summary", summary: WorkspaceSummary{Long: "Fix it. Now."}, want: errEmptySummary},
		{name: "no long summary", summary: WorkspaceSummary{Short: "Fix the topbar"}, want: errEmptyLongSummary},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := s.PutWorkspaceSummary(context.Background(), ws.ID, tt.summary)

			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("PutWorkspaceSummary = %v, want %v", err, tt.want)
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

func TestASummaryStoredBeforeLongSummariesReadsWithNoLongSummary(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustPutSummary(t, s, ws.ID, "Fix the topbar")
	corrupt(t, s, `UPDATE workspace_summaries SET long_summary = NULL`)

	// Act
	got, found := summaryOf(t, s, ws.ID)

	// Assert
	if want := (WorkspaceSummary{Short: "Fix the topbar"}); !found || got != want {
		t.Fatalf("summary = %+v (found=%v), want %+v", got, found, want)
	}
	if got.Complete() {
		t.Fatalf("a summary with no long summary reads complete")
	}
}

func TestAWholeSummaryIsComplete(t *testing.T) {
	// Assert
	if !summaryFor("Fix the topbar").Complete() {
		t.Fatalf("a summary with both parts reads incomplete")
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
				mustPutSummary(t, s, ws.ID, "Fix the topbar")
			}

			// Act
			dropped, err := s.DropWorkspaceSummary(context.Background(), ws.ID)

			// Assert
			if err != nil {
				t.Fatalf("DropWorkspaceSummary = %v, want nil", err)
			}
			if dropped != tt.seed {
				t.Fatalf("DropWorkspaceSummary reported dropped=%v, want %v", dropped, tt.seed)
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
	mustPutSummary(t, s, ws.ID, "Fix the topbar")
	mustPutSummary(t, s, other.ID, "Fix the footer")

	// Act
	if _, err := s.DropWorkspaceSummary(context.Background(), ws.ID); err != nil {
		t.Fatalf("DropWorkspaceSummary: %v", err)
	}

	// Assert
	if got, found := summaryOf(t, s, other.ID); !found || got != summaryFor("Fix the footer") {
		t.Fatalf("the other workspace's summary = %+v (found=%v), want %+v", got, found, summaryFor("Fix the footer"))
	}
}

func TestForgettingAWorkspaceDeletesItsSummary(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustPutSummary(t, s, ws.ID, "Fix the topbar")

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
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustPutSummary(t, s, ws.ID, "Fix the topbar")
	corrupt(t, s, `UPDATE workspace_summaries SET title = ''`)

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
}

func TestListingSummariesAnswersEveryStoredSummaryByWorkspace(t *testing.T) {
	tests := []struct {
		name   string
		stored map[string]string
	}{
		{name: "none stored", stored: map[string]string{}},
		{name: "one stored beside an unsummarized workspace", stored: map[string]string{"first": "Fix the topbar"}},
		{name: "two stored", stored: map[string]string{"first": "Fix the topbar", "second": "Fix the footer"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			want := map[WorkspaceID]WorkspaceSummary{}
			for _, name := range []string{"first", "second", "third"} {
				ws := testWorkspaceNamed(t, s, name)
				if summary, ok := tt.stored[name]; ok {
					mustPutSummary(t, s, ws.ID, summary)
					want[ws.ID] = summaryFor(summary)
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
	mustPutSummary(t, s, ws.ID, "Fix the topbar")
	if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: time.Unix(1_700_000_000, 0), By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	got, err := s.ListWorkspaceSummaries(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("ListWorkspaceSummaries: %v", err)
	}
	if got[ws.ID] != summaryFor("Fix the topbar") {
		t.Fatalf("a closed workspace's summary = %+v, want %+v", got[ws.ID], summaryFor("Fix the topbar"))
	}
}

func TestListingSummariesFailsWholeOnACorruptRow(t *testing.T) {
	// Arrange
	s, sink := testStore(t)
	ws := testWorkspace(t, s)
	mustPutSummary(t, s, ws.ID, "Fix the topbar")
	mustPutSummary(t, s, testWorkspaceNamed(t, s, "other").ID, "Fix the footer")
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

func TestDroppingTheDigestColumnsKeepsEveryTitle(t *testing.T) {
	// Arrange — the table as layout 56 shipped it, one summary with its digest
	// and one whose digest a /clear had reset.
	db, err := sql.Open("sqlite", ":memory:")
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	t.Cleanup(func() { _ = db.Close() })
	ctx := context.Background()
	for _, q := range []string{
		`CREATE TABLE workspaces (id TEXT PRIMARY KEY)`,
		`INSERT INTO workspaces (id) VALUES ('ws-a'), ('ws-b')`,
		workspaceSummariesLayout56DDL,
		`INSERT INTO workspace_summaries VALUES ('ws-a', 'Fix the topbar', 'h', 3, 1), ('ws-b', 'Fix the footer', NULL, NULL, NULL)`,
	} {
		if _, err := db.ExecContext(ctx, q); err != nil {
			t.Fatalf("seed %q: %v", q, err)
		}
	}

	// Act
	if _, err := db.ExecContext(ctx, workspaceSummariesDropDigestDDL); err != nil {
		t.Fatalf("the layout-110 step: %v", err)
	}

	// Assert
	rows, err := db.QueryContext(ctx, `SELECT workspace_id, title FROM workspace_summaries ORDER BY workspace_id`)
	if err != nil {
		t.Fatalf("read back: %v", err)
	}
	defer rows.Close()
	got := map[string]string{}
	for rows.Next() {
		var id, title string
		if err := rows.Scan(&id, &title); err != nil {
			t.Fatalf("scan: %v", err)
		}
		got[id] = title
	}
	want := map[string]string{"ws-a": "Fix the topbar", "ws-b": "Fix the footer"}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("titles after the step = %v, want %v", got, want)
	}
}

func TestTheLongSummaryStepKeepsEveryTitleWithNoLongSummary(t *testing.T) {
	// Arrange — the table as layout 110 left it, holding one summary.
	db, err := sql.Open("sqlite", ":memory:")
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	t.Cleanup(func() { _ = db.Close() })
	ctx := context.Background()
	for _, q := range []string{
		`CREATE TABLE workspaces (id TEXT PRIMARY KEY)`,
		`INSERT INTO workspaces (id) VALUES ('ws-a')`,
		workspaceSummariesDDL,
		`INSERT INTO workspace_summaries VALUES ('ws-a', 'Fix the topbar')`,
	} {
		if _, err := db.ExecContext(ctx, q); err != nil {
			t.Fatalf("seed %q: %v", q, err)
		}
	}

	// Act
	if _, err := db.ExecContext(ctx, workspaceSummariesLongDDL); err != nil {
		t.Fatalf("the layout-217 step: %v", err)
	}

	// Assert
	_, got, err := scanSummary(db.QueryRowContext(ctx, `SELECT `+summaryColumns+` FROM workspace_summaries`))
	if err != nil {
		t.Fatalf("read back: %v", err)
	}
	if want := (WorkspaceSummary{Short: "Fix the topbar"}); got != want {
		t.Fatalf("summary after the step = %+v, want %+v", got, want)
	}
}

func TestAnEmptyStoredLongSummaryIsRefusedByTheTable(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustPutSummary(t, s, ws.ID, "Fix the topbar")

	// Act
	_, err := s.db().ExecContext(context.Background(), `UPDATE workspace_summaries SET long_summary = ''`)

	// Assert
	if err == nil {
		t.Fatalf("the table stored an empty long summary; no long summary is NULL")
	}
}
