package buildid

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func writeBinary(t *testing.T, dir, body string) string {
	t.Helper()
	p := filepath.Join(dir, "agent-repld")
	if err := os.WriteFile(p, []byte(body), 0o755); err != nil {
		t.Fatalf("write %s: %v", p, err)
	}
	return p
}

func fileHash(t *testing.T, p string) string {
	t.Helper()
	h, err := File(p)
	if err != nil {
		t.Fatalf("File(%s): %v", p, err)
	}
	return h
}

func TestFileCachedAnswersTheContentHashAndRecordsIt(t *testing.T) {
	// Arrange
	p := writeBinary(t, t.TempDir(), "one build")

	// Act
	got, err := FileCached(p)

	// Assert
	if err != nil || got != fileHash(t, p) {
		t.Fatalf("FileCached = %q, %v; want the content hash %q", got, err, fileHash(t, p))
	}
	raw, err := os.ReadFile(p + ".buildid")
	if err != nil || !strings.HasSuffix(strings.TrimSpace(string(raw)), got) {
		t.Fatalf("sidecar = %q, %v; want it to record %q", raw, err, got)
	}
}

func TestFileCachedUsesTheSidecarWhileTheIdentityHolds(t *testing.T) {
	// Arrange: a sidecar for this exact identity, carrying a hash the bytes
	// do not have, proves the cache is what answered.
	p := writeBinary(t, t.TempDir(), "one build")
	id, err := identityOf(p)
	if err != nil {
		t.Fatal(err)
	}
	planted := strings.Repeat("ab", 32)
	if err := os.WriteFile(p+".buildid", []byte(id+" "+planted+"\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := FileCached(p)

	// Assert
	if err != nil || got != planted {
		t.Fatalf("FileCached = %q, %v; want the sidecar's %q", got, err, planted)
	}
}

func TestFileCachedRehashesAFileRewrittenInPlace(t *testing.T) {
	// Arrange
	p := writeBinary(t, t.TempDir(), "one build")
	if _, err := FileCached(p); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(p, []byte("another build"), 0o755); err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := FileCached(p)

	// Assert
	if err != nil || got != fileHash(t, p) {
		t.Fatalf("FileCached = %q, %v; want the new content hash %q", got, err, fileHash(t, p))
	}
}

func TestFileCachedRehashesAFileReplacedByRename(t *testing.T) {
	// Arrange: a deploy installs a new binary by rename.
	dir := t.TempDir()
	p := writeBinary(t, dir, "one build")
	if _, err := FileCached(p); err != nil {
		t.Fatal(err)
	}
	staged := filepath.Join(dir, "staged")
	if err := os.WriteFile(staged, []byte("one build, rebuilt"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.Rename(staged, p); err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := FileCached(p)

	// Assert
	if err != nil || got != fileHash(t, p) {
		t.Fatalf("FileCached = %q, %v; want the replacement's hash %q", got, err, fileHash(t, p))
	}
}

func TestFileCachedRewritesAMalformedSidecar(t *testing.T) {
	// Arrange
	p := writeBinary(t, t.TempDir(), "one build")
	if err := os.WriteFile(p+".buildid", []byte("garbage\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := FileCached(p)

	// Assert
	if err != nil || got != fileHash(t, p) {
		t.Fatalf("FileCached = %q, %v; want the content hash", got, err)
	}
	raw, _ := os.ReadFile(p + ".buildid")
	if !strings.HasSuffix(strings.TrimSpace(string(raw)), got) {
		t.Fatalf("sidecar = %q, want it rewritten for %q", raw, got)
	}
}

func TestFileCachedAnswersWhenTheSidecarCannotBeWritten(t *testing.T) {
	// Arrange: a directory nothing may be created in.
	dir := t.TempDir()
	p := writeBinary(t, dir, "one build")
	if err := os.Chmod(dir, 0o555); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.Chmod(dir, 0o755) })

	// Act
	got, err := FileCached(p)

	// Assert
	if err != nil || got != fileHash(t, p) {
		t.Fatalf("FileCached = %q, %v; want the content hash with no sidecar", got, err)
	}
}

func TestFileCachedRefusesAMissingFile(t *testing.T) {
	// Act
	_, err := FileCached(filepath.Join(t.TempDir(), "absent"))

	// Assert
	if err == nil {
		t.Fatal("FileCached answered a hash for a file that does not exist")
	}
}
