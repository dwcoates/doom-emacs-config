package testdir

import (
	"bufio"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

func TestHereIsThePackageDirectory(t *testing.T) {
	// Act
	got := Here(t)

	// Assert: this file sits in the directory Here names.
	if _, err := os.Stat(filepath.Join(got, "testdir_test.go")); err != nil {
		t.Fatalf("Here() = %q, which does not hold this test: %v", got, err)
	}
}

func TestModuleRootIsTheMarkedAncestor(t *testing.T) {
	// Act
	got := ModuleRoot(t)

	// Assert
	if !strings.HasSuffix(filepath.ToSlash(got), "/"+marker) {
		t.Fatalf("ModuleRoot() = %q, want a directory ending in %q", got, marker)
	}
	if _, err := os.Stat(filepath.Join(got, "testrun", "go.mod")); err != nil {
		t.Fatalf("ModuleRoot() = %q, which holds no testrun module: %v", got, err)
	}
}

func TestRepoRootIsTheModuleRootLessTheMarker(t *testing.T) {
	// Act
	got := RepoRoot(t)

	// Assert
	if want := filepath.Join(got, marker); want != ModuleRoot(t) {
		t.Fatalf("RepoRoot() = %q, want the repository holding %q", got, ModuleRoot(t))
	}
}

func TestModuleRootFrom(t *testing.T) {
	tests := []struct {
		name    string
		dir     string
		want    string
		wantErr bool
	}{
		{"the module root itself", "/r/" + marker, "/r/" + marker, false},
		{"a package below it", "/r/" + marker + "/testrun/internal/run", "/r/" + marker, false},
		{"the nearest of two", "/r/" + marker + "/x/" + marker + "/y", "/r/" + marker + "/x/" + marker, false},
		{"a tree with no module root", "/elsewhere/testrun", "", true},
		{"a partial marker", "/r/app/agent-repl", "", true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := moduleRootFrom(tt.dir)

			// Assert
			if (err != nil) != tt.wantErr || got != tt.want {
				t.Fatalf("moduleRootFrom(%q) = (%q, %v), want (%q, error %v)", tt.dir, got, err, tt.want, tt.wantErr)
			}
			if err != nil && !strings.Contains(err.Error(), tt.dir) {
				t.Fatalf("err = %v, want it to name %q", err, tt.dir)
			}
		})
	}
}

// TestNoGoTestFindsItselfByItsCompiledPath holds every Go source of the
// module to this package: runtime.Caller's file is empty of the absolute path
// under -trimpath, so a test locating itself by it breaks every run.
func TestNoGoTestFindsItselfByItsCompiledPath(t *testing.T) {
	// Arrange
	module := ModuleRoot(t)
	self := filepath.Join(Here(t), "testdir_test.go")
	var sources []string
	err := filepath.WalkDir(module, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() && (d.Name() == "node_modules" || d.Name() == ".git") {
			return filepath.SkipDir
		}
		if !d.IsDir() && strings.HasSuffix(path, ".go") && path != self {
			sources = append(sources, path)
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(sources) == 0 {
		t.Fatal("found no sources to scan: the layout this test assumes moved")
	}

	// Act
	var offenders []string
	for _, path := range sources {
		f, err := os.Open(path)
		if err != nil {
			t.Fatal(err)
		}
		sc := bufio.NewScanner(f)
		sc.Buffer(make([]byte, 1024*1024), 1024*1024)
		for n := 1; sc.Scan(); n++ {
			if strings.Contains(sc.Text(), "runtime.Caller(") {
				rel, _ := filepath.Rel(module, path)
				offenders = append(offenders, rel+":"+strconv.Itoa(n))
			}
		}
		if err := sc.Err(); err != nil {
			t.Fatalf("read %s: %v", path, err)
		}
		f.Close()
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("runtime.Caller's file is no absolute path under -trimpath; find files from the working directory (testdir.Here) instead:\n%s", strings.Join(offenders, "\n"))
	}
}
