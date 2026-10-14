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

func TestSetCacheLapseIsReadBack(t *testing.T) {
	tokens := uint64(120_000)
	tests := []struct {
		name  string
		lapse CacheLapse
	}{
		{name: "a gated lapse", lapse: CacheLapse{LapsesAt: lapseInstant, GatedTokens: &tokens}},
		{name: "an ungated lapse", lapse: CacheLapse{LapsesAt: lapseInstant}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := hiddenWorkspace(t, s)

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

func TestACorruptCacheLapseIsRefused(t *testing.T) {
	tests := []struct {
		name   string
		hide   bool
		update string
		field  string
	}{
		{name: "a lapse on a visible workspace", update: `UPDATE workspaces SET cache_lapses_at = 1 WHERE id = ?`, field: "cache_lapses_at"},
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
