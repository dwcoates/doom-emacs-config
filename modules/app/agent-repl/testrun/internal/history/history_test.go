package history

import (
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
)

func TestLoadOfAMissingFileIsEmpty(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "nested", "h.json")

	// Act
	s, err := Load(path)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if _, ok := s.Get("x"); ok {
		t.Fatal("an empty history answered a key")
	}
}

func TestRecord(t *testing.T) {
	tests := []struct {
		name    string
		prior   map[string]float64
		measure float64
		want    Entry
	}{
		{"a first measurement is taken whole", nil, 4, Entry{Seconds: 4, Samples: 1}},
		{"a later measurement moves the estimate by Weight", map[string]float64{"k": 4}, 8, Entry{Seconds: 6, Samples: 2}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "h.json")
			s, err := Load(path)
			if err != nil {
				t.Fatal(err)
			}
			if tt.prior != nil {
				if err := s.Record(tt.prior); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			err = s.Record(map[string]float64{"k": tt.measure})

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			reloaded, err := Load(path)
			if err != nil {
				t.Fatal(err)
			}
			if got := reloaded.entries["k"]; got != tt.want {
				t.Fatalf("entry = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestRecordKeepsAConcurrentRunsMeasurements(t *testing.T) {
	// Arrange: two runs load the same (empty) history, then each records.
	path := filepath.Join(t.TempDir(), "h.json")
	a, err := Load(path)
	if err != nil {
		t.Fatal(err)
	}
	b, err := Load(path)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	var wg sync.WaitGroup
	errs := make(chan error, 2)
	for _, run := range []struct {
		s   *Store
		key string
	}{{a, "from-a"}, {b, "from-b"}} {
		wg.Add(1)
		go func() {
			defer wg.Done()
			errs <- run.s.Record(map[string]float64{run.key: 1})
		}()
	}
	wg.Wait()
	close(errs)

	// Assert
	for err := range errs {
		if err != nil {
			t.Fatal(err)
		}
	}
	final, err := Load(path)
	if err != nil {
		t.Fatal(err)
	}
	for _, key := range []string{"from-a", "from-b"} {
		if _, ok := final.Get(key); !ok {
			t.Fatalf("%s's measurement was lost", key)
		}
	}
}

func TestRecordRefusesANegativeMeasurementAndWritesNothing(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "h.json")
	s, err := Load(path)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	err = s.Record(map[string]float64{"good": 1, "bad": -1})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "negative") {
		t.Fatalf("Record error = %v, want a refusal", err)
	}
	if _, statErr := os.Stat(path); !os.IsNotExist(statErr) {
		t.Fatalf("a refused record wrote the history: %v", statErr)
	}
}

func TestLoadRefusesAnUnreadableHistory(t *testing.T) {
	tests := []struct {
		name     string
		contents string
		want     string
	}{
		{"not json", "{nope", "not a test history"},
		{"another version", `{"version": 99, "entries": {}}`, "version 99"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "h.json")
			if err := os.WriteFile(path, []byte(tt.contents), 0o644); err != nil {
				t.Fatal(err)
			}

			// Act
			_, err := Load(path)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("Load error = %v, want it to mention %q", err, tt.want)
			}
		})
	}
}

func TestDefaultPathHonorsTheOverride(t *testing.T) {
	// Arrange
	t.Setenv("AGENT_REPL_TEST_HISTORY", "/x/h.json")

	// Act
	got, err := DefaultPath()

	// Assert
	if err != nil || got != "/x/h.json" {
		t.Fatalf("DefaultPath = %q, %v", got, err)
	}
}

func TestDefaultPathIsTheModulesHostCache(t *testing.T) {
	// Arrange
	t.Setenv("AGENT_REPL_TEST_HISTORY", "")
	t.Setenv("HOME", "/home/u")

	// Act
	got, err := DefaultPath()

	// Assert
	if err != nil || got != "/home/u/.cache/agent-repl/test-history.json" {
		t.Fatalf("DefaultPath = %q, %v", got, err)
	}
}

func TestDefaultPathWithoutAHomeFails(t *testing.T) {
	// Arrange
	t.Setenv("AGENT_REPL_TEST_HISTORY", "")
	t.Setenv("HOME", "")

	// Act
	_, err := DefaultPath()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "resolve the home directory") {
		t.Fatalf("err = %v", err)
	}
}
