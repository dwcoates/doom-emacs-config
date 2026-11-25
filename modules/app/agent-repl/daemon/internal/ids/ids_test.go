package ids

import (
	"slices"
	"testing"
)

func TestOptionalSpellsTheIdOrNothing(t *testing.T) {
	id := WorkspaceID("ws-1")
	tests := []struct {
		name string
		id   *WorkspaceID
		want string
	}{
		{name: "an id", id: &id, want: "ws-1"},
		{name: "none", id: nil, want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Optional(tt.id)

			// Assert
			if got != tt.want {
				t.Fatalf("Optional = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestStringsSpellsEachIdInOrder(t *testing.T) {
	tests := []struct {
		name string
		in   []WorkspaceID
		want []string
	}{
		{name: "ids", in: []WorkspaceID{"ws-2", "ws-1"}, want: []string{"ws-2", "ws-1"}},
		{name: "none", in: nil, want: []string{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Strings(tt.in)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("Strings = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestStringsAnswersAnEmptySliceRatherThanNil(t *testing.T) {
	// Act
	got := Strings[WorkspaceID](nil)

	// Assert: a record states none as [] rather than null.
	if got == nil {
		t.Fatal("Strings(nil) = nil, want an empty slice")
	}
}
