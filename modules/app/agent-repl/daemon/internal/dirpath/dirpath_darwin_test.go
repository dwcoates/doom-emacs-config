//go:build darwin

package dirpath

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestOnDiskPathAnswersTheStoredCaseOnAFoldingVolume(t *testing.T) {
	// Arrange
	base, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("EvalSymlinks: %v", err)
	}
	stored := filepath.Join(base, "ChessCom")
	if err := os.Mkdir(stored, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	folded := filepath.Join(base, "chesscom")
	if _, err := os.Stat(folded); errors.Is(err, os.ErrNotExist) {
		t.Skip("the temporary volume is case-sensitive, so no folded spelling exists to answer")
	}

	// Act
	got, err := onDiskPath(folded)

	// Assert
	if err != nil {
		t.Fatalf("onDiskPath: %v", err)
	}
	if !strings.HasSuffix(got, "/ChessCom") {
		t.Fatalf("onDiskPath(%q) = %q, want the stored ChessCom", folded, got)
	}
}

func TestOnDiskPathReportsAMissingPath(t *testing.T) {
	// Act
	_, err := onDiskPath(filepath.Join(t.TempDir(), "absent"))

	// Assert
	if !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("onDiskPath of an absent path = %v, want os.ErrNotExist", err)
	}
}
