package wsm

import (
	"context"
	"slices"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
)

func sortedSelections(ss []ComposerSelection) []ComposerSelection {
	out := slices.Clone(ss)
	slices.SortFunc(out, func(a, b ComposerSelection) int {
		return strings.Compare(string(a.Kind)+"/"+a.Name, string(b.Kind)+"/"+b.Name)
	})
	return out
}

func TestComposerSelections(t *testing.T) {
	tests := []struct {
		name string
		puts []ComposerSelection
		want []ComposerSelection
	}{
		{name: "a store with none recorded has none"},
		{
			name: "a selection is read back whole",
			puts: []ComposerSelection{{Kind: ComposerSelectionCommand, Name: "compact", SelectedAtMs: 10}},
			want: []ComposerSelection{{Kind: ComposerSelectionCommand, Name: "compact", SelectedAtMs: 10}},
		},
		{
			name: "a second taking replaces the instant",
			puts: []ComposerSelection{
				{Kind: ComposerSelectionChannel, Name: "eng", SelectedAtMs: 20},
				{Kind: ComposerSelectionChannel, Name: "eng", SelectedAtMs: 30},
			},
			want: []ComposerSelection{{Kind: ComposerSelectionChannel, Name: "eng", SelectedAtMs: 30}},
		},
		{
			name: "one name of each kind is separate records",
			puts: []ComposerSelection{
				{Kind: ComposerSelectionWorkspace, Name: "clear", SelectedAtMs: 5},
				{Kind: ComposerSelectionCommand, Name: "clear", SelectedAtMs: 6},
				{Kind: ComposerSelectionChannel, Name: "clear", SelectedAtMs: 7},
			},
			want: []ComposerSelection{
				{Kind: ComposerSelectionChannel, Name: "clear", SelectedAtMs: 7},
				{Kind: ComposerSelectionCommand, Name: "clear", SelectedAtMs: 6},
				{Kind: ComposerSelectionWorkspace, Name: "clear", SelectedAtMs: 5},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			for _, put := range tt.puts {
				if err := s.PutComposerSelection(context.Background(), put); err != nil {
					t.Fatalf("PutComposerSelection: %v", err)
				}
			}

			// Act
			got, err := s.ComposerSelections(context.Background())

			// Assert
			if err != nil {
				t.Fatalf("ComposerSelections: %v", err)
			}
			if !slices.Equal(sortedSelections(got), sortedSelections(tt.want)) {
				t.Fatalf("selections = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestAMalformedComposerSelectionIsRefused(t *testing.T) {
	tests := []struct {
		name string
		sel  ComposerSelection
	}{
		{name: "an unknown kind", sel: ComposerSelection{Kind: "skill", Name: "x", SelectedAtMs: 1}},
		{name: "an empty name", sel: ComposerSelection{Kind: ComposerSelectionCommand, SelectedAtMs: 1}},
		{name: "no instant", sel: ComposerSelection{Kind: ComposerSelectionCommand, Name: "x"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, rec := testStore(t)

			// Act
			err := s.PutComposerSelection(context.Background(), tt.sel)

			// Assert
			if err == nil {
				t.Fatal("PutComposerSelection accepted a malformed selection")
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM composer_selections`); n != 0 {
				t.Fatalf("%d rows written, want none", n)
			}
			records := dlog.FilterRecords(rec.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.put_composer_selection"})
			if len(records) != 1 || records[0].Context["name"] != tt.sel.Name || records[0].Context["error"] == nil {
				t.Fatalf("records = %+v, want one ERROR naming the selection and its cause", rec.Records())
			}
		})
	}
}
