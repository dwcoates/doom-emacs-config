package shimclient

import (
	"strings"
	"testing"
)

func TestParseProcStatExiting(t *testing.T) {
	tests := []struct {
		name    string
		stat    string
		want    bool
		wantErr string
	}{
		{name: "a running process", stat: "42 (node) S 1 42 42 0 -1 4194560 100 0 0 0", want: false},
		{name: "a process marked PF_EXITING", stat: "42 (node) S 1 42 42 0 -1 4194564 100 0 0 0", want: true},
		{name: "a zombie", stat: "42 (node) Z 1 42 42 0 -1 4194560 100 0 0 0", want: true},
		{name: "a dead process", stat: "42 (node) X 1 42 42 0 -1 4194560 100 0 0 0", want: true},
		{name: "a command name holding spaces and parentheses", stat: "42 (a (b) c) R 1 42 42 0 -1 4194560 1 0 0 0", want: false},
		{name: "a line with no command name", stat: "42 node R", wantErr: "no command name"},
		{name: "a short line", stat: "42 (node) R 1 2", wantErr: "is short"},
		{name: "non-numeric flags", stat: "42 (node) R 1 42 42 0 -1 x 1 0 0 0", wantErr: "flags"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := parseProcStatExiting(tt.stat)

			// Assert.
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("parseProcStatExiting error = %v, want one containing %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("parseProcStatExiting = %v, %v; want %v", got, err, tt.want)
			}
		})
	}
}
