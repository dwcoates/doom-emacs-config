package recordlog

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
)

func fixedClock() time.Time { return time.Date(2026, 10, 24, 12, 0, 0, 0, time.UTC) }

func decode(t *testing.T, line string) map[string]any {
	t.Helper()
	var out map[string]any
	if err := json.Unmarshal([]byte(line), &out); err != nil {
		t.Fatalf("record is not JSON: %v: %q", err, line)
	}
	return out
}

func TestRecordShape(t *testing.T) {
	// Arrange
	var sink, terminal bytes.Buffer
	log := New("zettel", &sink, &terminal, sharedlogging.FixedWindow(sharedlogging.LevelDebug), fixedClock).With(Context{"thread": "t1"})

	// Act
	log.Error("zettel.exchange.begin", "the write failed", Context{"cause": "disk full"})

	// Assert
	rec := decode(t, strings.TrimSpace(sink.String()))
	for key, want := range map[string]any{"runtime": "zettel", "level": "error", "verbosity": "normal", "operation": "zettel.exchange.begin", "message": "the write failed"} {
		if rec[key] != want {
			t.Errorf("%s = %v, want %v", key, rec[key], want)
		}
	}
	ctx := rec["context"].(map[string]any)
	if ctx["thread"] != "t1" || ctx["cause"] != "disk full" {
		t.Errorf("context = %v, want the base and the call's fields merged", ctx)
	}
	if terminal.Len() != 0 {
		t.Errorf("terminal got %q, want nothing while the sink accepts", terminal.String())
	}
}

func TestLevelWindowDropsBelowThreshold(t *testing.T) {
	// Arrange
	var sink, terminal bytes.Buffer
	log := New("zettel", &sink, &terminal, sharedlogging.FixedWindow(sharedlogging.LevelInfo), fixedClock)

	// Act
	log.Debug("zettel.test", "dropped", nil)

	// Assert
	if sink.Len() != 0 {
		t.Errorf("sink got %q, want a debug record dropped at info", sink.String())
	}
}

type refusingWriter struct{}

func (refusingWriter) Write([]byte) (int, error) { return 0, errors.New("read-only file system") }

func TestRefusingSinkFallsToTerminal(t *testing.T) {
	// Arrange
	var terminal bytes.Buffer
	log := New("zettel", refusingWriter{}, &terminal, sharedlogging.FixedWindow(sharedlogging.LevelDebug), fixedClock)

	// Act
	log.Warn("zettel.test", "kept", nil)

	// Assert
	if !strings.Contains(terminal.String(), "read-only file system") || !strings.Contains(terminal.String(), `"message":"kept"`) {
		t.Errorf("terminal = %q, want the refusal and the record", terminal.String())
	}
}

func TestWithDoesNotLeakIntoParent(t *testing.T) {
	// Arrange
	var sink, terminal bytes.Buffer
	parent := New("zettel", &sink, &terminal, sharedlogging.FixedWindow(sharedlogging.LevelDebug), fixedClock)
	_ = parent.With(Context{"child": true})

	// Act
	parent.Info("zettel.test", "parent", nil)

	// Assert
	ctx := decode(t, strings.TrimSpace(sink.String()))["context"].(map[string]any)
	if _, leaked := ctx["child"]; leaked {
		t.Errorf("parent record carries the child's field: %v", ctx)
	}
}

func TestEmptyOperationPanics(t *testing.T) {
	// Arrange
	var sink, terminal bytes.Buffer
	log := New("zettel", &sink, &terminal, sharedlogging.FixedWindow(sharedlogging.LevelDebug), fixedClock)
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("an empty operation did not panic")
		}
	}()

	// Act
	log.Info("", "no operation", nil)
}

func TestEveryRecordNamesTheRuntimeItWasMadeFor(t *testing.T) {
	tests := []struct {
		name    string
		runtime string
	}{
		{name: "zettel", runtime: "zettel"},
		{name: "the test service", runtime: "tests"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var sink, terminal bytes.Buffer
			log := New(tt.runtime, &sink, &terminal, sharedlogging.FixedWindow(sharedlogging.LevelDebug), fixedClock)

			// Act
			log.Info(tt.runtime+".test", "said", nil)

			// Assert
			if rec := decode(t, strings.TrimSpace(sink.String())); rec["runtime"] != tt.runtime {
				t.Fatalf("runtime = %v, want %q", rec["runtime"], tt.runtime)
			}
		})
	}
}

func TestANewLoggerNamingNoRuntimeIsRefused(t *testing.T) {
	// Arrange
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("New with no runtime did not panic")
		}
	}()

	// Act
	New("", &bytes.Buffer{}, &bytes.Buffer{}, sharedlogging.FixedWindow(sharedlogging.LevelDebug), fixedClock)
}

func TestOpenWritesToARotatingFileAtTheChosenLevel(t *testing.T) {
	tests := []struct {
		name      string
		level     string
		until     string
		wantDebug bool
	}{
		{name: "the default level drops a debug record", level: ""},
		{name: "a debug level with an end keeps it", level: "debug", until: fmt.Sprint(fixedClock().Add(time.Minute).Unix()), wantDebug: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "log", "tests.log")
			getenv := func(name string) string {
				switch name {
				case "AGENT_REPL_LOG_LEVEL":
					return tt.level
				case sharedlogging.UntilEnvironment:
					return tt.until
				}
				return ""
			}

			// Act
			log, closeFile, err := Open("tests", path, &bytes.Buffer{}, getenv, fixedClock)
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			log.Debug("tests.test", "a debug record", nil)
			log.Info("tests.test", "an info record", nil)
			if err := closeFile(); err != nil {
				t.Fatal(err)
			}

			// Assert
			body, err := os.ReadFile(path)
			if err != nil {
				t.Fatal(err)
			}
			if !strings.Contains(string(body), "an info record") || strings.Contains(string(body), "a debug record") != tt.wantDebug {
				t.Fatalf("log = %q, want the info record and the debug record %v", body, tt.wantDebug)
			}
		})
	}
}

func TestOpenRefusesALevelItCannotRead(t *testing.T) {
	// Arrange
	getenv := func(name string) string {
		if name == "AGENT_REPL_LOG_LEVEL" {
			return "loud"
		}
		return ""
	}

	// Act
	_, _, err := Open("tests", filepath.Join(t.TempDir(), "tests.log"), &bytes.Buffer{}, getenv, fixedClock)

	// Assert
	if err == nil {
		t.Fatal("Open accepted a level it cannot read")
	}
}

func TestOpenWritesAWarningOnlyToTheWarningsFile(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "log", "tests.log")
	log, closeFile, err := Open("tests", path, &bytes.Buffer{}, func(string) string { return "" }, fixedClock)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act
	log.Warn("tests.warning", "a warning", nil)
	log.Error("tests.error", "an error", nil)
	log.Info("tests.info", "an info record", nil)
	if err := closeFile(); err != nil {
		t.Fatal(err)
	}

	// Assert
	body, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	warnings, err := os.ReadFile(sharedlogging.WarningsPath(path))
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(string(body), "tests.warning") || strings.Contains(string(body), "tests.error") || !strings.Contains(string(body), "tests.info") {
		t.Fatalf("log = %q, want only the info record", body)
	}
	if !strings.Contains(string(warnings), "tests.warning") || !strings.Contains(string(warnings), "tests.error") || strings.Contains(string(warnings), "tests.info") {
		t.Fatalf("warnings = %q, want the warning and the error alone", warnings)
	}
}
