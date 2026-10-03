package e2e

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestLogTailOfNamesTheChildsLastRecords pins what a failed stop prints: the
// end of the child's own log, or why it cannot be read -- never nothing.
func TestLogTailOfNamesTheChildsLastRecords(t *testing.T) {
	dir := t.TempDir()
	present := filepath.Join(dir, "store.log")
	if err := os.WriteFile(present, []byte(strings.Repeat("x", stopLogTailBytes)+"\nreceived signal=terminated\n"), 0o644); err != nil {
		t.Fatalf("write the log: %v", err)
	}
	tests := []struct {
		name string
		path string
		want string
	}{
		{name: "a readable log answers its last record", path: present, want: "received signal=terminated"},
		{name: "a missing log answers why it cannot be read", path: filepath.Join(dir, "absent.log"), want: "cannot read"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := logTailOf(tt.path)

			// Assert
			if !strings.Contains(got, tt.want) {
				t.Errorf("logTailOf(%s) = %q, want it to contain %q", tt.path, got, tt.want)
			}
		})
	}
}
