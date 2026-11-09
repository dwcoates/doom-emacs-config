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

// entry is an Entry with a peak, for comparing.
type entry struct {
	Seconds float64
	Samples int
	Peak    float64
}

func flat(e Entry) entry {
	out := entry{Seconds: e.Seconds, Samples: e.Samples, Peak: -1}
	if e.Peak != nil {
		out.Peak = *e.Peak
	}
	return out
}

// once is a measurement whose typical time is its peak.
func once(secs float64) Sample { return Sample{Seconds: secs, Peak: secs} }

func TestRecord(t *testing.T) {
	tests := []struct {
		name    string
		prior   *Sample
		measure Sample
		want    entry
	}{
		{"a first measurement is taken whole", nil, once(4), entry{Seconds: 4, Samples: 1, Peak: 4}},
		{"a later measurement moves the estimate by Weight", &Sample{4, 4}, once(8), entry{Seconds: 6, Samples: 2, Peak: 8}},
		{"a slower measurement raises the peak to itself", &Sample{4, 4}, once(5), entry{Seconds: 4.5, Samples: 2, Peak: 5}},
		{"a faster measurement decays the peak by PeakDecay", &Sample{10, 10}, once(2), entry{Seconds: 6, Samples: 2, Peak: 10 * PeakDecay}},
		{"a measurement whose peak is above its typical time keeps both", nil, Sample{Seconds: 2, Peak: 5}, entry{Seconds: 2, Samples: 1, Peak: 5}},
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
				if err := s.Record(map[string]Sample{"k": *tt.prior}); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			err = s.Record(map[string]Sample{"k": tt.measure})

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			reloaded, err := Load(path)
			if err != nil {
				t.Fatal(err)
			}
			if got := flat(reloaded.entries["k"]); got != tt.want {
				t.Fatalf("entry = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestRecordGivesAnEntryFromBeforePeaksItsFirstPeak(t *testing.T) {
	// Arrange: an entry recorded before peaks were kept.
	path := filepath.Join(t.TempDir(), "h.json")
	if err := os.WriteFile(path, []byte(`{"version":1,"entries":{"k":{"seconds":4,"samples":7}}}`), 0o644); err != nil {
		t.Fatal(err)
	}
	s, err := Load(path)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	err = s.Record(map[string]Sample{"k": once(2)})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got, want := flat(s.entries["k"]), (entry{Seconds: 3, Samples: 8, Peak: 2}); got != want {
		t.Fatalf("entry = %+v, want %+v", got, want)
	}
}

func TestPeak(t *testing.T) {
	// Arrange: one entry with a peak, one from before peaks were kept.
	path := filepath.Join(t.TempDir(), "h.json")
	if err := os.WriteFile(path, []byte(`{"version":1,"entries":{"old":{"seconds":4,"samples":7},"new":{"seconds":4,"samples":7,"peak":9}}}`), 0o644); err != nil {
		t.Fatal(err)
	}
	s, err := Load(path)
	if err != nil {
		t.Fatal(err)
	}
	tests := []struct {
		key    string
		want   float64
		wantOK bool
	}{
		{"new", 9, true},
		{"old", 0, false},
		{"missing", 0, false},
	}
	for _, tt := range tests {
		t.Run(tt.key, func(t *testing.T) {
			// Act
			got, ok := s.Peak(tt.key)

			// Assert
			if got != tt.want || ok != tt.wantOK {
				t.Fatalf("Peak(%q) = %v, %v; want %v, %v", tt.key, got, ok, tt.want, tt.wantOK)
			}
		})
	}
}

func TestLongest(t *testing.T) {
	tests := []struct {
		name    string
		entries string
		want    float64
		wantOK  bool
	}{
		{"no key under the prefix is unknown", `{"item:x":{"seconds":50,"samples":1}}`, 0, false},
		{"the largest estimate counts", `{"unit:a":{"seconds":5,"samples":1},"unit:b":{"seconds":7,"samples":1}}`, 7, true},
		{"a peak above every estimate counts", `{"unit:a":{"seconds":5,"samples":1,"peak":12},"unit:b":{"seconds":7,"samples":1}}`, 12, true},
		{"a key outside the prefix is ignored", `{"unit:a":{"seconds":5,"samples":1},"item:x":{"seconds":50,"samples":1,"peak":60}}`, 5, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "h.json")
			if err := os.WriteFile(path, []byte(`{"version":1,"entries":`+tt.entries+`}`), 0o644); err != nil {
				t.Fatal(err)
			}
			s, err := Load(path)
			if err != nil {
				t.Fatal(err)
			}

			// Act
			got, ok := s.Longest("unit:")

			// Assert
			if got != tt.want || ok != tt.wantOK {
				t.Fatalf("Longest = %v, %v; want %v, %v", got, ok, tt.want, tt.wantOK)
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
			errs <- run.s.Record(map[string]Sample{run.key: once(1)})
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
	err = s.Record(map[string]Sample{"good": once(1), "bad": once(-1)})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "negative") {
		t.Fatalf("Record error = %v, want a refusal", err)
	}
	if _, statErr := os.Stat(path); !os.IsNotExist(statErr) {
		t.Fatalf("a refused record wrote the history: %v", statErr)
	}
}

func TestRecordRefusesAPeakBelowItsTypicalTime(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "h.json")
	s, err := Load(path)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	err = s.Record(map[string]Sample{"k": {Seconds: 5, Peak: 2}})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "its peak is below its typical time") {
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
	got, err := DefaultPath("ramdisk-io-default")

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
	got, err := DefaultPath("ramdisk-io-default")

	// Assert
	if err != nil || got != "/home/u/.cache/agent-repl/test-history.ramdisk-io-default.json" {
		t.Fatalf("DefaultPath = %q, %v", got, err)
	}
}

func TestDefaultPathWithoutAHomeFails(t *testing.T) {
	// Arrange
	t.Setenv("AGENT_REPL_TEST_HISTORY", "")
	t.Setenv("HOME", "")

	// Act
	_, err := DefaultPath("ramdisk-io-default")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "resolve the home directory") {
		t.Fatalf("err = %v", err)
	}
}

func TestDefaultPathRefusesConditionsThatAreNotOneName(t *testing.T) {
	cases := []struct {
		name       string
		conditions string
	}{
		{name: "no conditions", conditions: ""},
		{name: "conditions holding a separator", conditions: "a/b"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			t.Setenv("AGENT_REPL_TEST_HISTORY", "")
			t.Setenv("HOME", "/home/u")

			// Act
			_, err := DefaultPath(tc.conditions)

			// Assert
			if err == nil || !strings.Contains(err.Error(), "not one file-name component") {
				t.Fatalf("err = %v, want the conditions refused", err)
			}
		})
	}
}
