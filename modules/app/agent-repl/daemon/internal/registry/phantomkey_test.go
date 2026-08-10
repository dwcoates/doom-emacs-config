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
