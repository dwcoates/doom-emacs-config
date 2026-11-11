package wsm

import (
	"testing"
	"time"
)

func TestStamps(t *testing.T) {
	base := time.Date(2026, 11, 11, 12, 0, 0, 0, time.UTC)
	created, activity, selected, merged := base.Add(-4*time.Hour), base.Add(-3*time.Hour), base.Add(-2*time.Hour), base.Add(-time.Hour)
	cases := []struct {
		name string
		ws   Workspace
		want []string
	}{
		{name: "a bare record is only its creation", ws: Workspace{CreatedAt: created}, want: []string{"workspace_created"}},
		{
			name: "every set stamp is a signal",
			ws:   Workspace{CreatedAt: created, LastActivityAt: &activity, LastSelectedAt: &selected, MergedAt: &merged},
			want: []string{"workspace_created", "workspace_activity", "workspace_selected", "workspace_merged"},
		},
		{name: "an unset stamp is no signal", ws: Workspace{CreatedAt: created, MergedAt: &merged}, want: []string{"workspace_created", "workspace_merged"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := tc.ws.Stamps()

			// Assert.
			if len(got) != len(tc.want) {
				t.Fatalf("Stamps = %+v, want signals %v", got, tc.want)
			}
			for i, signal := range tc.want {
				if got[i].Signal != signal {
					t.Fatalf("stamp %d = %+v, want %s", i, got[i], signal)
				}
			}
		})
	}
}

func TestLastTouchedIsTheNewestStamp(t *testing.T) {
	base := time.Date(2026, 11, 11, 12, 0, 0, 0, time.UTC)
	older, newer := base.Add(-2*time.Hour), base.Add(-time.Hour)
	cases := []struct {
		name string
		ws   Workspace
		want Stamp
	}{
		{name: "a bare record was last touched at its creation", ws: Workspace{CreatedAt: older}, want: Stamp{At: older, Signal: "workspace_created"}},
		{name: "a later selection wins", ws: Workspace{CreatedAt: older, LastSelectedAt: &newer}, want: Stamp{At: newer, Signal: "workspace_selected"}},
		{name: "an earlier activity loses to the creation", ws: Workspace{CreatedAt: newer, LastActivityAt: &older}, want: Stamp{At: newer, Signal: "workspace_created"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := tc.ws.LastTouched()

			// Assert.
			if !got.At.Equal(tc.want.At) || got.Signal != tc.want.Signal {
				t.Fatalf("LastTouched = %+v, want %+v", got, tc.want)
			}
		})
	}
}
