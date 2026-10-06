package e2e

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// tailArtifactsSaid runs tailArtifacts over root and answers what it said.
func tailArtifactsSaid(root string) []string {
	var said []string
	tailArtifacts(func(format string, args ...any) { said = append(said, fmt.Sprintf(format, args...)) }, root)
	return said
}

// writeArtifact writes one collected file under root.
func writeArtifact(t *testing.T, root, rel, body string) {
	t.Helper()
	path := filepath.Join(root, rel)
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

func TestTailArtifactsPrintsEveryCollectedLog(t *testing.T) {
	// Arrange
	root := t.TempDir()
	writeArtifact(t, root, "state/logs/daemon.run.log", "{\"operation\":\"daemon.marker\"}\n")

	// Act
	said := tailArtifactsSaid(root)

	// Assert
	if !strings.Contains(strings.Join(said, "\n"), "daemon.marker") {
		t.Fatalf("said %q, want the daemon run log's tail", said)
	}
}

func TestTailArtifactsPrintsEmacsMessagesFirst(t *testing.T) {
	// Arrange
	root := t.TempDir()
	writeArtifact(t, root, "a-first-by-name.log", "other\n")
	writeArtifact(t, root, messagesFile, "messages\n")

	// Act
	said := tailArtifactsSaid(root)

	// Assert
	if len(said) == 0 || !strings.Contains(said[0], messagesFile) {
		t.Fatalf("first tail = %q, want Emacs's *Messages*", said)
	}
}

func TestTailArtifactsSkipsWhatIsNotALog(t *testing.T) {
	// Arrange
	root := t.TempDir()
	writeArtifact(t, root, "fakegit.json", "{\"marker\":\"fixture\"}")

	// Act
	said := tailArtifactsSaid(root)

	// Assert
	if strings.Contains(strings.Join(said, "\n"), "fixture") {
		t.Fatalf("said %q, want the fixture file left out of the tails", said)
	}
}

func TestTailArtifactsBoundsOneLog(t *testing.T) {
	// Arrange
	root := t.TempDir()
	writeArtifact(t, root, "big.log", strings.Repeat("0123456789abcde\n", 4*artifactTailFileBytes/16))

	// Act
	said := tailArtifactsSaid(root)

	// Assert
	if len(said) == 0 || len(said[0]) > artifactTailFileBytes+256 {
		t.Fatalf("one tail is %d bytes, want at most %d plus its header", len(said[0]), artifactTailFileBytes)
	}
}

func TestTailArtifactsNamesALogPastTheBudget(t *testing.T) {
	// Arrange
	root := t.TempDir()
	line := strings.Repeat("x", 1023) + "\n"
	for i := 0; i <= 2*artifactTailBudget/artifactTailFileBytes; i++ {
		writeArtifact(t, root, fmt.Sprintf("sink-%03d.log", i), strings.Repeat(line, artifactTailFileBytes/1024+1))
	}

	// Act
	said := tailArtifactsSaid(root)

	// Assert
	if !strings.Contains(strings.Join(said, "\n"), "not printed: the tail budget") {
		t.Fatalf("no log was named as past the budget; a dropped tail must still be said")
	}
}
