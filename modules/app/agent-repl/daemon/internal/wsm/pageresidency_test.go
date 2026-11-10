package wsm

import (
	"context"
	"testing"
	"time"
)

func TestPageLoadedIsUnknownUntilReported(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	loaded, known, err := s.PageLoaded(context.Background(), ws.ID)

	// Assert
	if err != nil || known || loaded {
		t.Fatalf("PageLoaded = (%v, %v, %v), want unknown", loaded, known, err)
	}
}

func TestSetPageLoadedRecordsTheLatestReport(t *testing.T) {
	tests := []struct {
		name    string
		reports []bool
		want    bool
	}{
		{"a loaded page", []bool{true}, true},
		{"an unloaded page", []bool{false}, false},
		{"a page unloaded after it was loaded", []bool{true, false}, false},
		{"a page loaded again", []bool{false, true}, true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			ctx := context.Background()

			// Act
			for i, loaded := range tc.reports {
				if err := s.SetPageLoaded(ctx, ws.ID, loaded, instant.Add(time.Duration(i)*time.Minute)); err != nil {
					t.Fatalf("SetPageLoaded: %v", err)
				}
			}

			// Assert
			loaded, known, err := s.PageLoaded(ctx, ws.ID)
			if err != nil || !known || loaded != tc.want {
				t.Fatalf("PageLoaded = (%v, %v, %v), want (%v, true)", loaded, known, err, tc.want)
			}
		})
	}
}

func TestSetPageLoadedRefusesAnUnknownWorkspaceAndLogs(t *testing.T) {
	// Arrange
	s, logs := testStore(t)

	// Act
	err := s.SetPageLoaded(context.Background(), "ws-nope", true, instant)

	// Assert
	if err == nil {
		t.Fatal("SetPageLoaded accepted a workspace the registry does not hold")
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM workspace_page_residency`); n != 0 {
		t.Fatalf("workspace_page_residency holds %d rows after a refusal", n)
	}
	if !loggedError(logs.Records(), "daemon.wsm.set_page_loaded") {
		t.Fatalf("the refusal was not logged at error: %+v", logs.Records())
	}
}
