package account

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"syscall"
	"testing"
)

// A rename across filesystems is the one rename failure with a fallback, and
// no portable test can arrange two filesystems. These subjects cover the
// fallback machinery itself (the classifier and the copy path it selects),
// which is what the EXDEV branch runs.

func TestIsCrossDevice(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want bool
	}{
		{
			name: "a rename's LinkError carrying EXDEV",
			err:  &os.LinkError{Op: "rename", Old: "/a", New: "/b", Err: syscall.EXDEV},
			want: true,
		},
		{
			name: "a bare EXDEV",
			err:  syscall.EXDEV,
			want: true,
		},
		{
			name: "a rename's LinkError carrying some other errno",
			err:  &os.LinkError{Op: "rename", Old: "/a", New: "/b", Err: syscall.EACCES},
			want: false,
		},
		{
			name: "an unrelated error",
			err:  errors.New("boom"),
			want: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := isCrossDevice(tc.err)

			// Assert.
			if got != tc.want {
				t.Fatalf("isCrossDevice(%v) = %v, want %v", tc.err, got, tc.want)
			}
		})
	}
}

func TestCopyFileAtomicReproducesTheContent(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	source := filepath.Join(dir, "source.jsonl")
	dest := filepath.Join(dir, "dest.jsonl")
	if err := os.WriteFile(source, []byte("{\"a\":1}\n"), 0o600); err != nil {
		t.Fatalf("writing the source = %v, want nil", err)
	}

	// Act.
	if err := copyFileAtomic(source, dest); err != nil {
		t.Fatalf("copyFileAtomic() = %v, want nil", err)
	}

	// Assert.
	got, err := os.ReadFile(dest)
	if err != nil {
		t.Fatalf("reading the destination = %v, want nil", err)
	}
	if string(got) != "{\"a\":1}\n" {
		t.Fatalf("destination = %q, want the source's bytes", got)
	}
}

func TestCopyFileAtomicLeavesNoTemporaryBehind(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	source := filepath.Join(dir, "source.jsonl")
	dest := filepath.Join(dir, "dest.jsonl")
	if err := os.WriteFile(source, []byte("x"), 0o600); err != nil {
		t.Fatalf("writing the source = %v, want nil", err)
	}

	// Act.
	if err := copyFileAtomic(source, dest); err != nil {
		t.Fatalf("copyFileAtomic() = %v, want nil", err)
	}

	// Assert.
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("reading the directory = %v, want nil", err)
	}
	for _, e := range entries {
		if len(e.Name()) > 5 && e.Name()[:5] == ".port" {
			t.Fatalf("a temporary %q survived the copy", e.Name())
		}
	}
}

func TestCopyFileAtomicSurfacesAMissingSource(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act.
	err := copyFileAtomic(filepath.Join(dir, "absent.jsonl"), filepath.Join(dir, "dest.jsonl"))

	// Assert.
	if !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("copyFileAtomic() = %v, want a not-exist failure", err)
	}
}

func TestCopyTreeReproducesEveryFileAndDirectory(t *testing.T) {
	// Arrange: a sidecar directory with a nested file.
	root := t.TempDir()
	source := filepath.Join(root, "sidecar")
	if err := os.MkdirAll(filepath.Join(source, "nested"), 0o700); err != nil {
		t.Fatalf("building the source tree = %v, want nil", err)
	}
	if err := os.WriteFile(filepath.Join(source, "nested", "blob.bin"), []byte("payload"), 0o600); err != nil {
		t.Fatalf("writing the nested file = %v, want nil", err)
	}
	dest := filepath.Join(root, "carried")

	// Act.
	if err := copyTree(source, dest); err != nil {
		t.Fatalf("copyTree() = %v, want nil", err)
	}

	// Assert.
	got, err := os.ReadFile(filepath.Join(dest, "nested", "blob.bin"))
	if err != nil {
		t.Fatalf("reading the carried file = %v, want nil", err)
	}
	if string(got) != "payload" {
		t.Fatalf("carried file = %q, want the source's bytes", got)
	}
}

func TestCopyTreeRefusesANonRegularEntry(t *testing.T) {
	// Arrange: a symlink inside a sidecar is not reproduced silently.
	root := t.TempDir()
	source := filepath.Join(root, "sidecar")
	if err := os.MkdirAll(source, 0o700); err != nil {
		t.Fatalf("building the source tree = %v, want nil", err)
	}
	if err := os.Symlink("/etc/passwd", filepath.Join(source, "link")); err != nil {
		t.Fatalf("creating the symlink = %v, want nil", err)
	}

	// Act.
	err := copyTree(source, filepath.Join(root, "carried"))

	// Assert.
	if err == nil {
		t.Fatal("copyTree() = nil error, want a refusal of the non-regular entry")
	}
}
