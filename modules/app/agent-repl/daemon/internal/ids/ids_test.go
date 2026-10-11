package ids

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
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

// TestNoPackageHandRollsOptional fails a non-test source that spells
// an optional id by hand instead of calling Optional.
func TestNoPackageHandRollsOptional(t *testing.T) {
	// Arrange
	handRolled := regexp.MustCompile(`if (\w+(\.\w+)?) == nil \{\s*return ""\s*\}\s*return string\(\*(\w+(\.\w+)?)\)`)
	var offenders []string

	// Act
	err := filepath.WalkDir("..", func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return err
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for _, m := range handRolled.FindAllStringSubmatch(string(raw), -1) {
			if m[1] == m[3] && !strings.HasSuffix(path, "ids/ids.go") {
				offenders = append(offenders, path)
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon's packages: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("hand-rolled optional ids in %v; call ids.Optional", offenders)
	}
}
