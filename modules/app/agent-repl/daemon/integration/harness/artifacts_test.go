package harness

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// writeLogs makes a logs directory holding the named files.
func writeLogs(t *testing.T, files map[string]string) string {
	t.Helper()
	dir := t.TempDir()
	for name, body := range files {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o600); err != nil {
			t.Fatalf("write %s: %v", name, err)
		}
	}
	return dir
}

func TestPreserveFailureLogsCopiesEveryLogIntoTheArtifactsDir(t *testing.T) {
	// Arrange
	logs := writeLogs(t, map[string]string{"daemon.run.log": "{\"operation\":\"daemon.a\"}\n"})
	dest := filepath.Join(t.TempDir(), "TestX")

	// Act
	preserveFailureLogs(func(string, ...any) {}, logs, dest)

	// Assert
	got, err := os.ReadFile(filepath.Join(dest, "daemon.run.log"))
	if err != nil || string(got) != "{\"operation\":\"daemon.a\"}\n" {
		t.Fatalf("preserved run log = %q, %v; want the log copied whole", got, err)
	}
}

func TestPreserveFailureLogsSkipsWhatIsNotALog(t *testing.T) {
	// Arrange
	logs := writeLogs(t, map[string]string{"daemon.addr": "127.0.0.1:1"})
	dest := filepath.Join(t.TempDir(), "TestX")

	// Act
	preserveFailureLogs(func(string, ...any) {}, logs, dest)

	// Assert
	if _, err := os.Stat(filepath.Join(dest, "daemon.addr")); !os.IsNotExist(err) {
		t.Fatalf("a non-log file was preserved (stat err %v); only .log files are artifacts", err)
	}
}

func TestPreserveFailureLogsTailsEachLogWithoutAnArtifactsDir(t *testing.T) {
	// Arrange
	logs := writeLogs(t, map[string]string{"daemon.run.log": "{\"operation\":\"daemon.marker\"}\n"})
	var said []string

	// Act
	preserveFailureLogs(func(format string, args ...any) { said = append(said, fmt.Sprintf(format, args...)) }, logs, "")

	// Assert
	if len(said) != 1 || !strings.Contains(said[0], "daemon.marker") {
		t.Fatalf("logged %q, want one tail carrying the run log's record", said)
	}
}

func TestPreserveFailureLogsSaysWhenThereAreNoLogs(t *testing.T) {
	// Arrange
	var said []string

	// Act
	preserveFailureLogs(func(format string, args ...any) { said = append(said, fmt.Sprintf(format, args...)) },
		filepath.Join(t.TempDir(), "absent"), "")

	// Assert
	if len(said) != 1 || !strings.Contains(said[0], "no logs to preserve") {
		t.Fatalf("logged %q, want the absence said", said)
	}
}

func TestTailBytesOpensOnARecordBoundary(t *testing.T) {
	// Arrange
	body := []byte("first record\nsecond record\n")

	// Act
	got := tailBytes(body, 20)

	// Assert
	if got != "second record\n" {
		t.Fatalf("tailBytes = %q, want the tail cut forward to the next whole record", got)
	}
}
