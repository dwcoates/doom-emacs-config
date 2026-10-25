package wsm

import (
	"context"
	"errors"
	"testing"
	"time"
)

var (
	hiddenAtInstant = time.Unix(1_700_000_000, 0)
	lapseInstant    = time.Unix(1_700_003_600, 0).UTC()
)

// hiddenWorkspace is a registered workspace the user has hidden.
func hiddenWorkspace(t *testing.T, s *store) Workspace {
	t.Helper()
	ws := testWorkspace(t, s)
	if err := s.SetHidden(context.Background(), ws.ID, true, hiddenAtInstant); err != nil {
		t.Fatalf("SetHidden: %v", err)
	}
	return ws
}

// closedWorkspace is a registered workspace the user has closed.
func closedWorkspace(t *testing.T, s *store) Workspace {
	t.Helper()
	ws := testWorkspace(t, s)
	if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: hiddenAtInstant, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
	return ws
}

// closedHiddenWorkspace is a registered workspace the user hid, then closed.
func closedHiddenWorkspace(t *testing.T, s *store) Workspace {
	t.Helper()
	ws := hiddenWorkspace(t, s)
	if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: hiddenAtInstant, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
	return ws
}

func TestSetCacheLapseIsReadBack(t *testing.T) {
	tokens := uint64(120_000)
	tests := []struct {
		name      string
		workspace func(*testing.T, *store) Workspace
		lapse     CacheLapse
	}{
		{name: "a hidden workspace's gated lapse", workspace: hiddenWorkspace, lapse: CacheLapse{LapsesAt: lapseInstant, GatedTokens: &tokens}},
		{name: "a hidden workspace's ungated lapse", workspace: hiddenWorkspace, lapse: CacheLapse{LapsesAt: lapseInstant}},
		{name: "a closed workspace's gated lapse", workspace: closedWorkspace, lapse: CacheLapse{LapsesAt: lapseInstant, GatedTokens: &tokens}},
		{name: "a closed workspace's ungated lapse", workspace: closedWorkspace, lapse: CacheLapse{LapsesAt: lapseInstant}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := tt.workspace(t, s)

			// Act
			recorded, err := s.SetCacheLapse(context.Background(), ws.ID, tt.lapse)

			// Assert
			if err != nil || !recorded {
				t.Fatalf("SetCacheLapse = %v, %v; want recorded", recorded, err)
			}
			got, err := s.Workspace(context.Background(), ws.ID)
			if err != nil || got.CacheLapse == nil || !got.CacheLapse.LapsesAt.Equal(tt.lapse.LapsesAt) {
				t.Fatalf("CacheLapse = %+v, %v; want %+v", got.CacheLapse, err, tt.lapse)
			}
			if (got.CacheLapse.GatedTokens == nil) != (tt.lapse.GatedTokens == nil) ||
				got.CacheLapse.GatedTokens != nil && *got.CacheLapse.GatedTokens != *tt.lapse.GatedTokens {
				t.Fatalf("GatedTokens = %v, want %v", got.CacheLapse.GatedTokens, tt.lapse.GatedTokens)
			}
		})
	}
}

func TestSetCacheLapseOnAVisibleWorkspaceRecordsNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	recorded, err := s.SetCacheLapse(context.Background(), ws.ID, CacheLapse{LapsesAt: lapseInstant})

	// Assert
	if err != nil || recorded {
		t.Fatalf("SetCacheLapse = %v, %v; want not recorded and no error", recorded, err)
	}
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil || got.CacheLapse != nil {
		t.Fatalf("CacheLapse = %+v, %v; want none", got.CacheLapse, err)
	}
}

func TestSetCacheLapseOnTheCurrentHiddenWorkspaceRecordsNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := hiddenWorkspace(t, s)
	if err := s.SetCurrent(context.Background(), ws.ID, hiddenAtInstant); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Act
	recorded, err := s.SetCacheLapse(context.Background(), ws.ID, CacheLapse{LapsesAt: lapseInstant})

	// Assert
	if err != nil || recorded {
		t.Fatalf("SetCacheLapse = %v, %v; want not recorded and no error", recorded, err)
	}
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil || got.CacheLapse != nil {
		t.Fatalf("CacheLapse = %+v, %v; want none", got.CacheLapse, err)
	}
}

func TestSetCurrentClearsTheCacheLapse(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := hiddenWorkspace(t, s)
	if _, err := s.SetCacheLapse(context.Background(), ws.ID, CacheLapse{LapsesAt: lapseInstant}); err != nil {
		t.Fatalf("SetCacheLapse: %v", err)
	}

	// Act
	if err := s.SetCurrent(context.Background(), ws.ID, hiddenAtInstant); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil || got.CacheLapse != nil || !got.Current {
		t.Fatalf("CacheLapse = %+v, Current = %v, %v; want none and current", got.CacheLapse, got.Current, err)
	}
}

func TestSetCurrentKeepsAClosedWorkspacesLapse(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := closedWorkspace(t, s)
	if _, err := s.SetCacheLapse(context.Background(), ws.ID, CacheLapse{LapsesAt: lapseInstant}); err != nil {
		t.Fatalf("SetCacheLapse: %v", err)
	}

	// Act
	if err := s.SetCurrent(context.Background(), ws.ID, hiddenAtInstant); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil || got.CacheLapse == nil {
		t.Fatalf("CacheLapse = %+v, %v; want the recorded one", got.CacheLapse, err)
	}
}

func TestSetCurrentLeavesTheDeselectedWorkspacesLapse(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	other := testWorkspace(t, s)
	ws := hiddenWorkspace(t, s)
	if _, err := s.SetCacheLapse(context.Background(), ws.ID, CacheLapse{LapsesAt: lapseInstant}); err != nil {
		t.Fatalf("SetCacheLapse: %v", err)
	}

	// Act
	if err := s.SetCurrent(context.Background(), other.ID, hiddenAtInstant); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil || got.CacheLapse == nil {
		t.Fatalf("CacheLapse = %+v, %v; want the recorded one", got.CacheLapse, err)
	}
}

func TestSetCacheLapseOnAnUnknownWorkspaceIsRefused(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.SetCacheLapse(context.Background(), "no-such-workspace", CacheLapse{LapsesAt: lapseInstant})

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetCacheLapse = %v, want ErrNotFound", err)
	}
}

func TestSetHiddenClearsTheCacheLapse(t *testing.T) {
	tests := []struct {
		name   string
		hidden bool
	}{
		{name: "showing the workspace again", hidden: false},
		{name: "hiding it again", hidden: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := hiddenWorkspace(t, s)
			if _, err := s.SetCacheLapse(context.Background(), ws.ID, CacheLapse{LapsesAt: lapseInstant}); err != nil {
				t.Fatalf("SetCacheLapse: %v", err)
			}

			// Act
			if err := s.SetHidden(context.Background(), ws.ID, tt.hidden, hiddenAtInstant); err != nil {
				t.Fatalf("SetHidden: %v", err)
			}

			// Assert
			got, err := s.Workspace(context.Background(), ws.ID)
			if err != nil || got.CacheLapse != nil {
				t.Fatalf("CacheLapse = %+v, %v; want none", got.CacheLapse, err)
			}
		})
	}
}

func TestReopenClearsAClosedWorkspacesCacheLapse(t *testing.T) {
	tests := []struct {
		name      string
		workspace func(*testing.T, *store) Workspace
		kept      bool
	}{
		{name: "a closed workspace", workspace: closedWorkspace, kept: false},
		{name: "a closed workspace still hidden keeps the hide's", workspace: closedHiddenWorkspace, kept: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := tt.workspace(t, s)
			if recorded, err := s.SetCacheLapse(context.Background(), ws.ID, CacheLapse{LapsesAt: lapseInstant}); err != nil || !recorded {
				t.Fatalf("SetCacheLapse = %v, %v; want recorded", recorded, err)
			}

			// Act
			if err := s.Reopen(context.Background(), ws.ID, testServing); err != nil {
				t.Fatalf("Reopen: %v", err)
			}

			// Assert
			got, err := s.Workspace(context.Background(), ws.ID)
			if err != nil || (got.CacheLapse != nil) != tt.kept {
				t.Fatalf("CacheLapse = %+v, %v; want kept %v", got.CacheLapse, err, tt.kept)
			}
		})
	}
}

func TestACorruptCacheLapseIsRefused(t *testing.T) {
	tests := []struct {
		name   string
		hide   bool
		update string
		field  string
	}{
		{name: "a lapse on an open visible workspace", update: `UPDATE workspaces SET cache_lapses_at = 1 WHERE id = ?`, field: "cache_lapses_at"},
		{name: "a gated verdict with no lapse", hide: true, update: `UPDATE workspaces SET cache_lapse_gated_tokens = 5 WHERE id = ?`, field: "cache_lapse_gated_tokens"},
		{name: "a gated verdict re-reading nothing", hide: true, update: `UPDATE workspaces SET cache_lapses_at = 1, cache_lapse_gated_tokens = 0 WHERE id = ?`, field: "cache_lapse_gated_tokens"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if tt.hide {
				ws = hiddenWorkspace(t, s)
			}
			corrupt(t, s, tt.update, ws.ID)

			// Act
			_, err := s.Workspace(context.Background(), ws.ID)

			// Assert
			var decode *DecodeError
			if !errors.As(err, &decode) || decode.Field != tt.field {
				t.Fatalf("Workspace = %v, want a %s decode error", err, tt.field)
			}
		})
	}
}

func TestCacheLapseLapsed(t *testing.T) {
	tests := []struct {
		name string
		now  time.Time
		want bool
	}{
		{name: "before the lapse", now: lapseInstant.Add(-time.Second), want: false},
		{name: "at the lapse", now: lapseInstant, want: true},
		{name: "after the lapse", now: lapseInstant.Add(time.Second), want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := CacheLapse{LapsesAt: lapseInstant}.Lapsed(tt.now)

			// Assert
			if got != tt.want {
				t.Fatalf("Lapsed(%v) = %v, want %v", tt.now, got, tt.want)
			}
		})
	}
}
