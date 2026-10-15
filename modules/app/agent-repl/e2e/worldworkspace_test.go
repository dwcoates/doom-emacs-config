package e2e

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

func TestNewRepoWorkspaceRegistersTheRepositoryItMinted(t *testing.T) {
	t.Parallel()
	// Arrange
	w := SharedWorld(t)

	// Act
	repo, ws := w.NewRepoWorkspace(t)

	// Assert
	if ws.GetId() == "" {
		t.Errorf("workspace = %v, want a daemon-minted id", ws)
	}
	if _, err := os.Stat(filepath.Join(repo.Dir, ".git")); err != nil {
		t.Errorf("repository %s has no .git marker: %v", repo.Dir, err)
	}
	if got, _ := filepath.EvalSymlinks(ws.GetDir()); got != repo.Dir && ws.GetDir() != repo.Dir {
		t.Errorf("workspace dir = %s, want the minted repository %s", ws.GetDir(), repo.Dir)
	}
}

func TestNewWorkspaceMintsAFreshWorkspaceEachTime(t *testing.T) {
	t.Parallel()
	// Arrange
	w := SharedWorld(t)

	// Act
	first, second := w.NewWorkspace(t), w.NewWorkspace(t)

	// Assert
	if first.GetId() == second.GetId() || first.GetDir() == second.GetDir() {
		t.Fatalf("workspaces %v and %v, want two distinct ones", first, second)
	}
}

// TestNoTestMintsAWorkspaceByHand pins the extraction: a fresh fake-git
// workspace is minted through World.NewWorkspace or NewRepoWorkspace, never by
// pairing harness.NewRepo with harness.Register at the call site.
func TestNoTestMintsAWorkspaceByHand(t *testing.T) {
	// Arrange
	handRolled := regexp.MustCompile(`(\w+) := harness\.NewRepo\(t\)\n\s*(?:ws :=|return) harness\.Register\(t, \w+\.Daemon, (\w+)\.Dir\)`)
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	// Act
	var found []string
	for _, f := range files {
		body, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("read %s: %v", f, err)
		}
		for _, m := range handRolled.FindAllStringSubmatch(string(body), -1) {
			if m[1] == m[2] && f != "world_test.go" {
				found = append(found, f)
			}
		}
	}

	// Assert
	if len(found) > 0 {
		t.Fatalf("files minting a workspace by hand: %s; use World.NewWorkspace or NewRepoWorkspace", strings.Join(found, ", "))
	}
}
