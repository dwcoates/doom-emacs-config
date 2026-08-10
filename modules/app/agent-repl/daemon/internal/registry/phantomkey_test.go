package registry

import (
	"fmt"
	"strings"
	"sync"
	"testing"
)

func TestNoncanonicalRecordsIdentifiesPhantomWorkspaceKeys(t *testing.T) {
	tests := []struct {
		name string
		rec  Record
		want bool
	}{
		{name: "trailing separator is a phantom", rec: Record{SessionID: "s_1", CWD: "/w/a/"}, want: true},
		{name: "doubled separator is a phantom", rec: Record{SessionID: "s_2", CWD: "/w//a"}, want: true},
		{name: "canonical key is not a phantom", rec: Record{SessionID: "s_3", CWD: "/w/a"}, want: false},
		{name: "record with no workspace is not a phantom", rec: Record{SessionID: "s_4"}, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := noncanonicalRecords([]Record{tc.rec})

			// Assert.
			if (len(got) == 1) != tc.want {
				t.Fatalf("noncanonicalRecords(%q) = %v, want phantom=%v", tc.rec.CWD, got, tc.want)
			}
		})
	}
}

// capturingLogf collects log lines for assertions.
type capturingLogf struct {
	mu    sync.Mutex
	lines []string
}

func (c *capturingLogf) logf(format string, args ...any) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.lines = append(c.lines, fmt.Sprintf(format, args...))
}

func (c *capturingLogf) contains(sub string) bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	for _, line := range c.lines {
		if strings.Contains(line, sub) {
			return true
		}
	}
	return false
}

func TestPrepareReportsAnExistingPhantomWorkspaceRecord(t *testing.T) {
	// Arrange: a record written before ingress canonicalization existed.
	path := testPath(t)
	seed := Open(path, discardLogf)
	if err := seed.Prepare(); err != nil {
		t.Fatalf("seed prepare: %v", err)
	}
	if err := seed.Put(Record{SessionID: "s_phantom", CWD: "/w/a/", CreatedAt: "2026-07-12T00:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	if err := seed.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act: a fresh boot over the same store.
	cap := &capturingLogf{}
	r := Open(path, cap.logf)
	t.Cleanup(func() { _ = r.Close() })
	if err := r.Prepare(); err != nil {
		t.Fatalf("prepare: %v", err)
	}

	// Assert.
	if !cap.contains("phantom workspace key session_id=s_phantom") {
		t.Fatalf("boot log did not report the phantom record; lines=%v", cap.lines)
	}
}

func TestPrepareStaysSilentWhenEveryWorkspaceKeyIsCanonical(t *testing.T) {
	// Arrange.
	path := testPath(t)
	seed := Open(path, discardLogf)
	if err := seed.Prepare(); err != nil {
		t.Fatalf("seed prepare: %v", err)
	}
	if err := seed.Put(Record{SessionID: "s_clean", CWD: "/w/a", CreatedAt: "2026-07-12T00:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	if err := seed.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act.
	cap := &capturingLogf{}
	r := Open(path, cap.logf)
	t.Cleanup(func() { _ = r.Close() })
	if err := r.Prepare(); err != nil {
		t.Fatalf("prepare: %v", err)
	}

	// Assert.
	if cap.contains("phantom workspace key") {
		t.Fatalf("boot log reported a phantom for a canonical store; lines=%v", cap.lines)
	}
}

// seedRecords writes recs into a fresh store and returns its path.
func seedRecords(t *testing.T, recs ...Record) string {
	t.Helper()
	path := testPath(t)
	seed := Open(path, discardLogf)
	if err := seed.Prepare(); err != nil {
		t.Fatalf("seed prepare: %v", err)
	}
	for _, rec := range recs {
		if err := seed.Put(rec); err != nil {
			t.Fatalf("seed put %s: %v", rec.SessionID, err)
		}
	}
	if err := seed.Close(); err != nil {
		t.Fatalf("seed close: %v", err)
	}
	return path
}

// bootRegistry opens path and prepares it, returning the prepared registry.
func bootRegistry(t *testing.T, path string, logf func(string, ...any)) *Registry {
	t.Helper()
	r := Open(path, logf)
	t.Cleanup(func() { _ = r.Close() })
	if err := r.Prepare(); err != nil {
		t.Fatalf("prepare: %v", err)
	}
	return r
}

func TestPrepareRenamesAPhantomKeyWithNoCanonicalTwin(t *testing.T) {
	// Arrange.
	path := seedRecords(t, Record{SessionID: "s_phantom", CWD: "/w/a/", CreatedAt: "2026-07-12T00:00:00Z"})

	// Act.
	r := bootRegistry(t, path, discardLogf)

	// Assert.
	rec, ok := r.Get("s_phantom")
	if !ok || rec.CWD != "/w/a" || rec.Terminal {
		t.Fatalf("phantom was not renamed in place: rec=%+v ok=%v", rec, ok)
	}
}

func TestPrepareRetiresAPhantomBehindItsCanonicalTwin(t *testing.T) {
	// Arrange.
	path := seedRecords(t,
		Record{SessionID: "s_canonical", CWD: "/w/a", CreatedAt: "2026-07-12T00:00:00Z"},
		Record{SessionID: "s_phantom", CWD: "/w/a/", CreatedAt: "2026-07-11T00:00:00Z"},
	)

	// Act.
	r := bootRegistry(t, path, discardLogf)

	// Assert.
	rec, ok := r.Get("s_phantom")
	if !ok || !rec.Terminal || rec.DeathReason != phantomRetirementReason {
		t.Fatalf("phantom was not retired: rec=%+v ok=%v", rec, ok)
	}
}

func TestPrepareLeavesTheCanonicalTwinUntouched(t *testing.T) {
	// Arrange.
	canonical := Record{SessionID: "s_canonical", CWD: "/w/a", CreatedAt: "2026-07-12T00:00:00Z"}
	path := seedRecords(t, canonical,
		Record{SessionID: "s_phantom", CWD: "/w/a/", CreatedAt: "2026-07-11T00:00:00Z"},
	)

	// Act.
	r := bootRegistry(t, path, discardLogf)

	// Assert.
	rec, ok := r.Get("s_canonical")
	if !ok || rec.CWD != canonical.CWD || rec.Terminal || rec.DeathReason != "" {
		t.Fatalf("canonical record was modified: rec=%+v ok=%v", rec, ok)
	}
}

func TestPrepareExcludesARetiredPhantomFromLiveEnumeration(t *testing.T) {
	// Arrange.
	path := seedRecords(t,
		Record{SessionID: "s_canonical", CWD: "/w/a", CreatedAt: "2026-07-12T00:00:00Z"},
		Record{SessionID: "s_phantom", CWD: "/w/a/", CreatedAt: "2026-07-11T00:00:00Z"},
	)

	// Act.
	r := bootRegistry(t, path, discardLogf)

	// Assert: only the canonical record is still live.
	if got := r.liveCount(); got != 1 {
		t.Fatalf("liveCount() = %d, want 1 (the retired phantom must not enumerate as live)", got)
	}
}

func TestPrepareConsolidationIsIdempotentOnASecondBoot(t *testing.T) {
	// Arrange: a first boot that consolidates both branches.
	path := seedRecords(t,
		Record{SessionID: "s_canonical", CWD: "/w/a", CreatedAt: "2026-07-12T00:00:00Z"},
		Record{SessionID: "s_phantom", CWD: "/w/a/", CreatedAt: "2026-07-11T00:00:00Z"},
		Record{SessionID: "s_lonely", CWD: "/w/b/", CreatedAt: "2026-07-11T00:00:00Z"},
	)
	first := bootRegistry(t, path, discardLogf)
	if err := first.Close(); err != nil {
		t.Fatalf("close first: %v", err)
	}

	// Act: a second boot over the already-consolidated store.
	cap := &capturingLogf{}
	bootRegistry(t, path, cap.logf)

	// Assert.
	if !cap.contains("consolidation consolidated=0 retired=0 untouched=1") {
		t.Fatalf("second boot did work it should not have; lines=%v", cap.lines)
	}
}
