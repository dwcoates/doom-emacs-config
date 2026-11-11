package reuse

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

var keyA = strings.Repeat("a", 64)
var keyB = strings.Repeat("b", 64)

func TestAStoredRecordIsFoundUnderItsKey(t *testing.T) {
	// Arrange
	s := Store{Dir: t.TempDir()}
	rec := Record{Suite: "daemon", Key: keyA, Branch: "master", Commit: "c1", Dirty: true, Manifest: Manifest{"x": "1"}}

	// Act
	if err := s.Put(rec); err != nil {
		t.Fatal(err)
	}
	got, ok, err := s.Lookup("daemon", keyA)

	// Assert
	if err != nil || !ok {
		t.Fatalf("Lookup = %v, %v", ok, err)
	}
	if got.Commit != "c1" || !got.Dirty || got.Version != recordVersion {
		t.Fatalf("Lookup = %+v, want the stored record", got)
	}
}

func TestLookupOfAnUnrecordedKeyFindsNothing(t *testing.T) {
	// Arrange
	s := Store{Dir: t.TempDir()}
	if err := s.Put(Record{Suite: "daemon", Key: keyA}); err != nil {
		t.Fatal(err)
	}

	// Act
	_, ok, err := s.Lookup("daemon", keyB)

	// Assert
	if err != nil || ok {
		t.Fatalf("Lookup = %v, %v; want nothing", ok, err)
	}
}

func TestNewestIsTheLatestPass(t *testing.T) {
	// Arrange
	s := Store{Dir: t.TempDir()}
	t0 := time.Date(2026, 10, 14, 12, 0, 0, 0, time.UTC)
	for _, r := range []Record{{Suite: "lock", Key: keyA, PassedAt: t0}, {Suite: "lock", Key: keyB, PassedAt: t0.Add(time.Hour)}} {
		if err := s.Put(r); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	got, ok, err := s.Newest("lock")

	// Assert
	if err != nil || !ok || got.Key != keyB {
		t.Fatalf("Newest = %+v, %v, %v; want the later record", got, ok, err)
	}
}

func TestNewestOfASuiteWithNoRecordsFindsNothing(t *testing.T) {
	// Act
	_, ok, err := Store{Dir: t.TempDir()}.Newest("lock")

	// Assert
	if err != nil || ok {
		t.Fatalf("Newest = %v, %v; want nothing", ok, err)
	}
}

func TestTheStoreRefusesWhatIsNotARecord(t *testing.T) {
	tests := []struct {
		name string
		body string
		want string
	}{
		{"unreadable JSON", "{", "is not a pass record"},
		{"another version", `{"version": 99, "suite": "lock", "key": "` + keyA + `"}`, "record version 99"},
		{"another suite's record", `{"version": 1, "suite": "daemon", "key": "` + keyA + `"}`, "records daemon/"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := Store{Dir: t.TempDir()}
			if err := os.MkdirAll(filepath.Join(s.Dir, "lock"), 0o755); err != nil {
				t.Fatal(err)
			}
			if err := os.WriteFile(filepath.Join(s.Dir, "lock", keyA+".json"), []byte(tt.body), 0o644); err != nil {
				t.Fatal(err)
			}

			// Act
			_, _, err := s.Lookup("lock", keyA)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want it to mention %q", err, tt.want)
			}
		})
	}
}

func TestTheStoreRefusesABadNameOrKey(t *testing.T) {
	tests := []struct{ suite, key string }{
		{"../escape", keyA},
		{"lock", "short"},
		{"lock", "../" + keyA},
	}
	for _, tt := range tests {
		t.Run(tt.suite+" "+tt.key, func(t *testing.T) {
			// Act
			err := Store{Dir: t.TempDir()}.Put(Record{Suite: tt.suite, Key: tt.key})

			// Assert
			if err == nil {
				t.Fatal("the record was written")
			}
		})
	}
}

func TestALastOutcomeIsReadBackAndNeverReadAsAPass(t *testing.T) {
	// Arrange
	s := Store{Dir: t.TempDir()}
	failed := Record{Suite: "lock@x", Key: keyA, Failed: true, PassedAt: time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)}

	// Act
	err := s.PutLast(failed)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	got, ok, err := s.Last("lock@x")
	if err != nil || !ok || !got.Failed || got.Key != keyA {
		t.Fatalf("Last = %+v %v %v, want the failure", got, ok, err)
	}
	if _, ok, err := s.Newest("lock@x"); err != nil || ok {
		t.Fatalf("Newest found %v (%v), want a last outcome never read as a pass", ok, err)
	}
	if _, ok, err := s.Lookup("lock@x", keyA); err != nil || ok {
		t.Fatalf("Lookup found %v (%v), want a failure never reusable", ok, err)
	}
}

func TestAUnitWithNoLastOutcomeHasNone(t *testing.T) {
	// Act
	_, ok, err := Store{Dir: t.TempDir()}.Last("lock")

	// Assert
	if err != nil || ok {
		t.Fatalf("Last = %v %v, want none", ok, err)
	}
}

func TestAFailureIsNeverStoredAsAPass(t *testing.T) {
	// Act
	err := Store{Dir: t.TempDir()}.Put(Record{Suite: "lock", Key: keyA, Failed: true})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "never a pass") {
		t.Fatalf("err = %v", err)
	}
}

func TestTheLastOutcomeRefusesABadNameOrKey(t *testing.T) {
	tests := []struct{ suite, key string }{
		{"../escape", keyA},
		{"lock", "short"},
	}
	for _, tt := range tests {
		t.Run(tt.suite+" "+tt.key, func(t *testing.T) {
			// Arrange
			s := Store{Dir: t.TempDir()}

			// Act
			err := s.PutLast(Record{Suite: tt.suite, Key: tt.key})
			_, _, lastErr := s.Last(tt.suite)

			// Assert
			if err == nil {
				t.Fatal("the last outcome was written")
			}
			if tt.suite == "../escape" && lastErr == nil {
				t.Fatal("a bad name was read")
			}
		})
	}
}

func TestOnlyTheNewestPassKeepsItsManifest(t *testing.T) {
	// Arrange
	s := Store{Dir: t.TempDir()}
	t0 := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	newer := Record{Suite: "lock", Key: keyB, PassedAt: t0.Add(time.Hour), Manifest: Manifest{"b": "2"}}
	older := Record{Suite: "lock", Key: keyA, PassedAt: t0, Manifest: Manifest{"a": "1"}}

	// Act
	errNewer, errOlder := s.Put(newer), s.Put(older)

	// Assert
	if errNewer != nil || errOlder != nil {
		t.Fatal(errNewer, errOlder)
	}
	for _, k := range []string{keyA, keyB} {
		r, ok, err := s.Lookup("lock", k)
		if err != nil || !ok || r.Manifest != nil {
			t.Fatalf("Lookup(%s) = %+v %v %v, want a pass without its manifest", k, r, ok, err)
		}
	}
	got, ok, err := s.Newest("lock")
	if err != nil || !ok || got.Key != keyB || got.Manifest["b"] != "2" {
		t.Fatalf("Newest = %+v %v %v, want the later pass with its manifest", got, ok, err)
	}
}

func TestAUnitKeepsItsNewestPassesOnly(t *testing.T) {
	// Arrange
	s := Store{Dir: t.TempDir()}
	t0 := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	key := func(i int) string { return fmt.Sprintf("%064x", i) }

	// Act
	for i := 0; i < maxPasses+3; i++ {
		if err := s.Put(Record{Suite: "lock", Key: key(i), PassedAt: t0.Add(time.Duration(i) * time.Minute)}); err != nil {
			t.Fatal(err)
		}
	}

	// Assert
	for i := 0; i < maxPasses+3; i++ {
		_, ok, err := s.Lookup("lock", key(i))
		if err != nil || ok != (i >= 3) {
			t.Fatalf("pass %d kept = %v (%v), want only the newest %d", i, ok, err, maxPasses)
		}
	}
}
