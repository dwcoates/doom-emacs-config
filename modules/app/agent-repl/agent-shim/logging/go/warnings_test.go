package logging

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// warningsFixturePath is the cross-language warnings-sink contract.
const warningsFixturePath = "../../../proto/vocab/log-warnings-sink.json"

type warningsFixture struct {
	WarningsLevels []string `json:"warnings_levels"`
	LogLevels      []string `json:"log_levels"`
	Paths          []struct {
		Path     string `json:"path"`
		Warnings string `json:"warnings"`
	} `json:"paths"`
	Sinks []struct {
		Name     string `json:"name"`
		Warnings string `json:"warnings"`
	} `json:"sinks"`
}

func loadWarningsFixture(t *testing.T) warningsFixture {
	t.Helper()
	raw, err := os.ReadFile(filepath.FromSlash(warningsFixturePath))
	if err != nil {
		t.Fatalf("read the warnings-sink fixture: %v", err)
	}
	var f warningsFixture
	if err := json.Unmarshal(raw, &f); err != nil {
		t.Fatalf("decode the warnings-sink fixture: %v", err)
	}
	return f
}

func TestWarningsPathNamesEachContractPathsWarningsFile(t *testing.T) {
	for _, tc := range loadWarningsFixture(t).Paths {
		t.Run(tc.Path, func(t *testing.T) {
			// Act
			got := WarningsPath(tc.Path)

			// Assert
			if got != tc.Warnings {
				t.Fatalf("WarningsPath(%q) = %q, want %q", tc.Path, got, tc.Warnings)
			}
		})
	}
}

func TestWarningsSinkNamesEachContractSinksTwin(t *testing.T) {
	for _, tc := range loadWarningsFixture(t).Sinks {
		t.Run(tc.Name, func(t *testing.T) {
			// Act
			got := WarningsSink(tc.Name)

			// Assert
			if got != tc.Warnings {
				t.Fatalf("WarningsSink(%q) = %q, want %q", tc.Name, got, tc.Warnings)
			}
		})
	}
}

func TestIsWarningsLevelRoutesEachContractLevel(t *testing.T) {
	f := loadWarningsFixture(t)
	tests := []struct {
		levels []string
		want   bool
	}{{f.WarningsLevels, true}, {f.LogLevels, false}}
	for _, tt := range tests {
		for _, level := range tt.levels {
			t.Run(level, func(t *testing.T) {
				// Act
				got := IsWarningsLevel(level)

				// Assert
				if got != tt.want {
					t.Fatalf("IsWarningsLevel(%q) = %v, want %v", level, got, tt.want)
				}
			})
		}
	}
}

func TestIsWarningsLevelPanicsOnAnUnknownLevel(t *testing.T) {
	// Arrange
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatalf("IsWarningsLevel(\"fatal\") did not panic")
		}
	}()

	// Act
	IsWarningsLevel("fatal")
}

func TestRecordLevelReadsTheTopLevelLevel(t *testing.T) {
	tests := []struct {
		name    string
		line    string
		want    string
		wantErr string
	}{
		{name: "the level after other keys", line: `{"timestamp":"t","runtime":"daemon","level":"warn","context":{"level":"debug"}}` + "\n", want: "warn"},
		{name: "a nested level before the real one is skipped", line: `{"context":{"level":"error"},"level":"info"}`, want: "info"},
		{name: "no level is refused", line: `{"timestamp":"t"}`, wantErr: "names no level"},
		{name: "a level that is not a string is refused", line: `{"level":3}`, wantErr: "not a string"},
		{name: "a line that is not an object is refused", line: `not json`, wantErr: "not a JSON object"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := RecordLevel([]byte(tt.line))

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("RecordLevel = %q, %v; want an error containing %q", got, err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("RecordLevel = %q, %v; want %q", got, err, tt.want)
			}
		})
	}
}

func TestSplitSinkRoutesEachRecordByItsLevel(t *testing.T) {
	tests := []struct {
		level        string
		wantWarnings bool
	}{{"debug", false}, {"info", false}, {"warn", true}, {"error", true}}
	for _, tt := range tests {
		t.Run(tt.level, func(t *testing.T) {
			// Arrange
			var log, warnings bytes.Buffer
			sink := SplitSink{Log: &log, Warnings: &warnings}
			line := []byte(`{"timestamp":"t","level":"` + tt.level + `","message":"m"}` + "\n")

			// Act
			n, err := sink.Write(line)

			// Assert
			if err != nil || n != len(line) {
				t.Fatalf("Write = %d, %v; want %d, nil", n, err, len(line))
			}
			got, other := log.String(), warnings.String()
			if tt.wantWarnings {
				got, other = other, got
			}
			if got != string(line) || other != "" {
				t.Fatalf("the record went to the wrong file: log %q, warnings %q", log.String(), warnings.String())
			}
		})
	}
}

func TestSplitSinkRefusesAnUnknownLevelAndWritesNothing(t *testing.T) {
	// Arrange
	var log, warnings bytes.Buffer
	sink := SplitSink{Log: &log, Warnings: &warnings}

	// Act
	_, err := sink.Write([]byte(`{"level":"fatal"}` + "\n"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), `unknown level "fatal"`) {
		t.Fatalf("Write = %v, want a refusal naming the level", err)
	}
	if log.Len() != 0 || warnings.Len() != 0 {
		t.Fatalf("a refused record was written: log %q, warnings %q", log.String(), warnings.String())
	}
}

func TestSplitSinkRefusesALineThatIsNotARecord(t *testing.T) {
	// Arrange
	var log, warnings bytes.Buffer
	sink := SplitSink{Log: &log, Warnings: &warnings}

	// Act
	_, err := sink.Write([]byte("plain text\n"))

	// Assert
	if err == nil || log.Len() != 0 || warnings.Len() != 0 {
		t.Fatalf("Write = %v, log %q, warnings %q; want a refusal that writes nothing", err, log.String(), warnings.String())
	}
}

func TestOpenRotatingPairWritesEachLevelToItsOwnFile(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "svc.log")
	pair, err := OpenRotatingPair(path, 0, 0)
	if err != nil {
		t.Fatalf("OpenRotatingPair: %v", err)
	}
	t.Cleanup(func() { _ = pair.Close() })

	// Act
	for _, level := range []string{"info", "error"} {
		if _, err := pair.Write([]byte(`{"level":"` + level + `"}` + "\n")); err != nil {
			t.Fatalf("Write(%s): %v", level, err)
		}
	}

	// Assert
	for file, want := range map[string]string{path: `{"level":"info"}` + "\n", WarningsPath(path): `{"level":"error"}` + "\n"} {
		got, err := os.ReadFile(file)
		if err != nil || string(got) != want {
			t.Fatalf("%s = %q, %v; want %q", file, got, err, want)
		}
	}
}
