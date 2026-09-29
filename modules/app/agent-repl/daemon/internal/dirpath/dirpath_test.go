package dirpath

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// foldingVolume is a case-INSENSITIVE volume holding the listed directories in
// their stored case, the way the default macOS volume does: a path exists when
// it matches one under case folding, EvalSymlinks answers it in the case it was
// asked in, and the stored spelling comes back only from onDiskPath.
type foldingVolume struct {
	dirs []string
	// failing is a stored directory whose on-disk read fails.
	failing string
}

func (v foldingVolume) stored(path string) (string, bool) {
	for _, dir := range v.dirs {
		if strings.EqualFold(dir, path) {
			return dir, true
		}
	}
	return "", false
}

func (v foldingVolume) resolver() resolver {
	return resolver{
		evalSymlinks: func(path string) (string, error) {
			if _, ok := v.stored(path); !ok {
				return "", os.ErrNotExist
			}
			return path, nil
		},
		onDiskPath: func(path string) (string, error) {
			dir, ok := v.stored(path)
			if !ok {
				return "", os.ErrNotExist
			}
			if dir == v.failing {
				return "", os.ErrPermission
			}
			return dir, nil
		},
	}
}

func chesscomVolume() foldingVolume {
	return foldingVolume{dirs: []string{"/", "/Users", "/Users/me", "/Users/me/ChessCom", "/Users/me/ChessCom/iterm-1"}}
}

func TestCanonicalAnswersTheOnDiskSpelling(t *testing.T) {
	tests := []struct {
		name    string
		spelled string
		want    string
	}{
		{name: "a folded component takes its stored case", spelled: "/Users/me/chesscom/iterm-1", want: "/Users/me/ChessCom/iterm-1"},
		{name: "every folded component takes its stored case", spelled: "/users/ME/CHESSCOM/ITERM-1", want: "/Users/me/ChessCom/iterm-1"},
		{name: "the stored spelling is kept", spelled: "/Users/me/ChessCom/iterm-1", want: "/Users/me/ChessCom/iterm-1"},
		{name: "an absent leaf keeps its spelling under a stored parent", spelled: "/Users/me/chesscom/New-Tree", want: "/Users/me/ChessCom/New-Tree"},
		{name: "a trailing slash is cleaned", spelled: "/Users/me/chesscom/iterm-1/", want: "/Users/me/ChessCom/iterm-1"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r := chesscomVolume().resolver()

			// Act
			got, err := r.canonical(tc.spelled)

			// Assert
			if err != nil {
				t.Fatalf("canonical(%q): %v", tc.spelled, err)
			}
			if got != tc.want {
				t.Fatalf("canonical(%q) = %q, want %q", tc.spelled, got, tc.want)
			}
		})
	}
}

func TestCanonicalReportsAFailedOnDiskRead(t *testing.T) {
	// Arrange
	volume := chesscomVolume()
	volume.failing = "/Users/me/ChessCom/iterm-1"
	r := volume.resolver()

	// Act
	_, err := r.canonical("/Users/me/chesscom/iterm-1")

	// Assert
	if !errors.Is(err, os.ErrPermission) {
		t.Fatalf("canonical over a failing read = %v, want os.ErrPermission", err)
	}
}

func TestCanonicalRefusesAnEmptyDirectory(t *testing.T) {
	// Act
	_, err := Canonical("")

	// Assert
	if err == nil {
		t.Fatal("Canonical(\"\") succeeded, want a refusal")
	}
}

// TestCanonicalRefusesARelativeDirectory pins that a relative directory is
// never resolved against the working directory: `~/.config/doom` became
// `/Users/me/~/.config/doom` that way (2026-09-28).
func TestCanonicalRefusesARelativeDirectory(t *testing.T) {
	for _, dir := range []string{"~/.config/doom", "repo", "./repo", "../repo"} {
		t.Run(dir, func(t *testing.T) {
			// Arrange: a volume on which the working directory's guess WOULD
			// resolve, so only the refusal can pass.
			r := chesscomVolume().resolver()

			// Act.
			got, err := r.canonical(dir)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), "is not an absolute directory") {
				t.Fatalf("canonical(%q) = (%q, %v), want a refusal", dir, got, err)
			}
		})
	}
}

func TestCanonicalResolvesASymlinkOnTheRealVolume(t *testing.T) {
	// Arrange
	base := t.TempDir()
	real := filepath.Join(base, "real")
	if err := os.Mkdir(real, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	link := filepath.Join(base, "link")
	if err := os.Symlink(real, link); err != nil {
		t.Fatalf("symlink: %v", err)
	}

	// Act
	viaLink, errLink := Canonical(link)
	viaReal, errReal := Canonical(real)

	// Assert
	if errLink != nil || errReal != nil {
		t.Fatalf("Canonical: %v / %v", errLink, errReal)
	}
	if viaLink != viaReal {
		t.Fatalf("Canonical via symlink = %q, want %q", viaLink, viaReal)
	}
}

func TestCanonicalOfAnExistingDirectoryIsStable(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	first, err := Canonical(dir)
	if err != nil {
		t.Fatalf("Canonical: %v", err)
	}

	// Act
	second, err := Canonical(first)

	// Assert
	if err != nil {
		t.Fatalf("Canonical: %v", err)
	}
	if second != first {
		t.Fatalf("Canonical(Canonical(d)) = %q, want %q", second, first)
	}
}
