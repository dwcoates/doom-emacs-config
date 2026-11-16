package singletonsocket

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
	"agentrepl/logging/recordlog"
)

func TestListenRecordedNamesItsRuntimeInEveryOutcome(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(t *testing.T, path string)
		wantLevel string
		wantMsg   string
	}{
		{name: "a free path is bound", arrange: func(*testing.T, string) {}, wantLevel: "info", wantMsg: "listening on the tests socket"},
		{name: "a path whose directory is a file is refused", arrange: func(t *testing.T, path string) {
			if err := os.WriteFile(filepath.Dir(path), nil, 0o644); err != nil {
				t.Fatal(err)
			}
		}, wantLevel: "error", wantMsg: "binding the tests socket failed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			// A SHORT ROOT: a unix socket's path is capped near 104 bytes,
			// past which the test's own temp directory already runs.
			root, err := os.MkdirTemp("/tmp", "ss")
			if err != nil {
				t.Fatal(err)
			}
			t.Cleanup(func() { _ = os.RemoveAll(root) })
			path := filepath.Join(root, "sock", "tests.sock")
			tt.arrange(t, path)
			var sink bytes.Buffer
			log := recordlog.New("tests", &sink, &bytes.Buffer{}, sharedlogging.FixedWindow(sharedlogging.LevelDebug), time.Now)

			// Act
			ln, _ := ListenRecorded(path, time.Second, log)
			if ln != nil {
				_ = ln.Close()
			}

			// Assert
			lines := strings.Split(strings.TrimSpace(sink.String()), "\n")
			var rec map[string]any
			if err := json.Unmarshal([]byte(lines[len(lines)-1]), &rec); err != nil {
				t.Fatalf("record %q: %v", sink.String(), err)
			}
			if rec["level"] != tt.wantLevel || rec["message"] != tt.wantMsg || rec["operation"] != "tests.listen" {
				t.Fatalf("record = %v, want %s %q under tests.listen", rec, tt.wantLevel, tt.wantMsg)
			}
		})
	}
}
