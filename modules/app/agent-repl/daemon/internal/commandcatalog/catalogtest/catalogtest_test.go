package catalogtest

import (
	"testing"
)

func TestNamed(t *testing.T) {
	tests := []struct {
		name  string
		names []string
		want  []string
	}{
		{name: "no names is an empty catalog", names: nil, want: nil},
		{name: "one entry per name, in order", names: []string{"merge-queue", "compact"}, want: []string{"merge-queue", "compact"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Named(tt.names...)

			// Assert.
			if got == nil {
				t.Fatal("Named answered nil, want a catalog")
			}
			var names []string
			for _, entry := range got.GetCommands() {
				if entry.Description != nil || entry.ArgumentHint != nil || len(entry.GetAliases()) != 0 {
					t.Fatalf("entry %q carries more than its name: %v", entry.GetName(), entry)
				}
				names = append(names, entry.GetName())
			}
			if len(names) != len(tt.want) {
				t.Fatalf("names = %v, want %v", names, tt.want)
			}
			for i := range names {
				if names[i] != tt.want[i] {
					t.Fatalf("names = %v, want %v", names, tt.want)
				}
			}
		})
	}
}
