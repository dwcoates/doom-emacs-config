package harness

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestRecordProfiledWorkspaceAppendsOneLinePerWorkspace(t *testing.T) {
	// Arrange
	dir := t.TempDir()

	// Act
	errA := recordProfiledWorkspace(dir, "ws-a", "TestA")
	errB := recordProfiledWorkspace(dir, "ws-b", "TestB/case")

	// Assert
	if errA != nil || errB != nil {
		t.Fatalf("errors = %v, %v", errA, errB)
	}
	got, err := os.ReadFile(filepath.Join(dir, ProfiledWorkspacesFile))
	if err != nil {
		t.Fatal(err)
	}
	if want := "ws-a\tTestA\nws-b\tTestB/case\n"; string(got) != want {
		t.Fatalf("%s = %q, want %q", ProfiledWorkspacesFile, got, want)
	}
}

func TestRecordProfiledWorkspaceFailsWhereItCannotWrite(t *testing.T) {
	// Arrange
	missing := filepath.Join(t.TempDir(), "gone")

	// Act
	err := recordProfiledWorkspace(missing, "ws-a", "TestA")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "open the profiled workspaces of "+missing) {
		t.Fatalf("err = %v, want the directory named", err)
	}
}
