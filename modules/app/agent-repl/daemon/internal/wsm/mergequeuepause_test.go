package wsm

import "testing"

func TestAPauserKindIsNamedAsItsColumnNamesIt(t *testing.T) {
	tests := []struct {
		kind MergeQueuePauserKind
		want string
	}{
		{kind: MergeQueuePauserUnrecorded, want: "unrecorded"},
		{kind: MergeQueuePauserUser, want: "user"},
		{kind: MergeQueuePauserWorkspace, want: "workspace"},
		{kind: MergeQueuePauserHost, want: "host"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act
			got := tt.kind.String()

			// Assert
			if got != tt.want {
				t.Fatalf("String() = %q, want %q", got, tt.want)
			}
		})
	}
}
