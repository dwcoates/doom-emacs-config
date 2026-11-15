package wsm

import (
	"context"
	"errors"
	"maps"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// mustPutVendorTitle records TITLE for WS, failing the test on any error.
func mustPutVendorTitle(t *testing.T, s *store, ws WorkspaceID, title string) {
	t.Helper()
	if err := s.PutVendorTitle(context.Background(), ws, title); err != nil {
		t.Fatalf("PutVendorTitle(%q): %v", title, err)
	}
}

// vendorTitles lists every stored vendor title, failing the test on an error.
func vendorTitles(t *testing.T, s *store) map[WorkspaceID]string {
	t.Helper()
	got, err := s.ListVendorTitles(context.Background())
	if err != nil {
		t.Fatalf("ListVendorTitles: %v", err)
	}
	return got
}

func TestAVendorTitleIsReplacedByTheNextAndKeptPastTheClose(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustPutVendorTitle(t, s, ws.ID, "Fix the topbar")
	mustPutVendorTitle(t, s, ws.ID, "Fix the footer")
	if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: time.Unix(1_700_000_000, 0), By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	got := vendorTitles(t, s)

	// Assert
	if want := map[WorkspaceID]string{ws.ID: "Fix the footer"}; !maps.Equal(got, want) {
		t.Fatalf("ListVendorTitles = %v, want %v", got, want)
	}
}

func TestAVendorTitleIsNoSummary(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	mustPutVendorTitle(t, s, ws.ID, "Fix the topbar")

	// Assert
	if _, found := summaryOf(t, s, ws.ID); found {
		t.Fatal("a vendor title stood as the workspace's summary")
	}
}

func TestDroppingAVendorTitleReportsWhetherOneStood(t *testing.T) {
	tests := []struct {
		name string
		seed bool
		want bool
	}{
		{name: "a standing title is dropped", seed: true, want: true},
		{name: "no title is no error", seed: false, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if tt.seed {
				mustPutVendorTitle(t, s, ws.ID, "Fix the topbar")
			}

			// Act
			dropped, err := s.DropVendorTitle(context.Background(), ws.ID)

			// Assert
			if err != nil || dropped != tt.want {
				t.Fatalf("DropVendorTitle = %v, %v; want %v, nil", dropped, err, tt.want)
			}
			if got := vendorTitles(t, s); len(got) != 0 {
				t.Fatalf("titles after the drop = %v, want none", got)
			}
		})
	}
}

func TestAVendorTitleOfAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.PutVendorTitle(context.Background(), "ws-gone", "Fix the topbar")

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("PutVendorTitle = %v, want ErrNotFound", err)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM workspace_vendor_titles`); n != 0 {
		t.Fatalf("%d title rows stand, want none", n)
	}
}

func TestAnEmptyVendorTitleIsRefused(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.PutVendorTitle(context.Background(), ws.ID, "")

	// Assert
	if !errors.Is(err, errEmptyVendorTitle) {
		t.Fatalf("PutVendorTitle = %v, want errEmptyVendorTitle", err)
	}
	if got := vendorTitles(t, s); len(got) != 0 {
		t.Fatalf("a refused title was stored: %v", got)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.put_vendor_title"})
	if !ok || rec.Context["workspace"] != string(ws.ID) {
		t.Fatalf("no error record naming the workspace: %+v", log.Records())
	}
}

func TestListingVendorTitlesFailsWholeOnACorruptRow(t *testing.T) {
	// Arrange
	s, sink := testStore(t)
	ws := testWorkspace(t, s)
	mustPutVendorTitle(t, s, ws.ID, "Fix the topbar")
	mustPutVendorTitle(t, s, testWorkspaceNamed(t, s, "other").ID, "Fix the footer")
	corrupt(t, s, `UPDATE workspace_vendor_titles SET title = '' WHERE workspace_id = '`+string(ws.ID)+`'`)

	// Act
	got, err := s.ListVendorTitles(context.Background())

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Table != "workspace_vendor_titles" || decode.Row != string(ws.ID) {
		t.Fatalf("ListVendorTitles = %v, want a DecodeError on workspace_vendor_titles row %s", err, ws.ID)
	}
	if got != nil {
		t.Fatalf("a failed read answered %+v, want nothing", got)
	}
	if !loggedOperation(sink, "daemon.wsm.list_vendor_titles", "error") {
		t.Fatalf("the failed read was not recorded at ERROR: %v", sink.Records())
	}
}
