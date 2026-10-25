package goprofile

import (
	"agentrepl/testrun/internal/testdir"
	"io/fs"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func TestParseProfileSplitsReachedBlocksFromAll(t *testing.T) {
	// Arrange
	text := "mode: atomic\nm/p/a.go:3.10,5.2 2 4\nm/p/a.go:7.1,8.3 1 0\n"

	// Act
	all, covered, err := ParseProfile(text)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(all) != 2 {
		t.Fatalf("all = %v, want both blocks", all)
	}
	want := []Block{{File: "m/p/a.go", Line0: 3, Col0: 10, Line1: 5, Col1: 2, Statements: 2}}
	if !reflect.DeepEqual(covered, want) {
		t.Fatalf("covered = %v, want %v", covered, want)
	}
	if covered[0].Key() != "m/p/a.go:3.10,5.2" {
		t.Fatalf("Key() = %q", covered[0].Key())
	}
}

func TestParseProfileRefuses(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "no mode line", text: "m/p/a.go:3.10,5.2 2 4\n", want: "starts with its mode"},
		{name: "a line with too few fields", text: "mode: set\nm/p/a.go:3.10,5.2 2\n", want: "malformed"},
		{name: "a span without a comma", text: "mode: set\nm/p/a.go:3.10 2 1\n", want: "malformed"},
		{name: "a position without a column", text: "mode: set\nm/p/a.go:3,5.2 2 1\n", want: "malformed"},
		{name: "a count that is not a number", text: "mode: set\nm/p/a.go:3.1,5.2 2 x\n", want: "malformed"},
		{name: "a statement count that is not a number", text: "mode: set\nm/p/a.go:3.1,5.2 x 1\n", want: "malformed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, _, err := ParseProfile(tt.text)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestBlockContains(t *testing.T) {
	b := Block{File: "f", Line0: 3, Col0: 10, Line1: 5, Col1: 2}
	tests := []struct {
		line, col int
		want      bool
	}{
		{3, 10, true},
		{3, 9, false},
		{4, 1, true},
		{5, 2, true},
		{5, 3, false},
		{2, 50, false},
	}
	for _, tt := range tests {
		if got := b.Contains(tt.line, tt.col); got != tt.want {
			t.Errorf("Contains(%d, %d) = %t, want %t", tt.line, tt.col, got, tt.want)
		}
	}
}

func TestHoldingIsInnermostFirstAndPerFile(t *testing.T) {
	// Arrange: a function literal's block inside the statement holding it.
	outer := Block{File: "f", Line0: 1, Col0: 1, Line1: 9, Col1: 1}
	inner := Block{File: "f", Line0: 3, Col0: 5, Line1: 4, Col1: 2}
	other := Block{File: "g", Line0: 1, Col0: 1, Line1: 9, Col1: 1}
	idx := NewIndex([]Block{outer, inner, other})

	// Act
	got := idx.Holding("f", 3, 6)

	// Assert
	if !reflect.DeepEqual(got, []Block{inner, outer}) {
		t.Fatalf("holding = %v, want inner then outer", got)
	}
	if len(idx.Holding("f", 10, 1)) != 0 {
		t.Fatal("a position past every block is held")
	}
}

// TestNoOtherFileReadsAProfileItself holds every testrun source to this
// reader: a second parser of the "mode: " format would drift from it.
func TestNoOtherFileReadsAProfileItself(t *testing.T) {
	// Arrange
	self := filepath.Join(testdir.Here(t), "goprofile_test.go")
	module := filepath.Join(testdir.ModuleRoot(t), "testrun")
	var found []string

	// Act
	err := filepath.WalkDir(module, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || filepath.Dir(path) == filepath.Dir(self) {
			return nil
		}
		b, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(b), `"mode: "`) {
			rel, _ := filepath.Rel(module, path)
			found = append(found, rel)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(found) > 0 {
		t.Fatalf("read profiles with internal/goprofile instead: %v", found)
	}
}

func TestCounterDirsFindsMetaFilesAtAnyDepth(t *testing.T) {
	// Arrange
	root := t.TempDir()
	for _, d := range []string{"a/000", "spawned/x/agent-repld", "empty"} {
		if err := os.MkdirAll(filepath.Join(root, d), 0o755); err != nil {
			t.Fatal(err)
		}
	}
	for _, f := range []string{"a/000/covmeta.1", "a/000/covcounters.1.2", "spawned/x/agent-repld/covmeta.2", "empty/notes.txt"} {
		if err := os.WriteFile(filepath.Join(root, f), nil, 0o644); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	got, err := CounterDirs(root)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	want := []string{filepath.Join(root, "a/000"), filepath.Join(root, "spawned/x/agent-repld")}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("CounterDirs = %v, want %v", got, want)
	}
}

func TestCounterDirsSurfacesAnUnreadableRoot(t *testing.T) {
	// Act
	_, err := CounterDirs(filepath.Join(t.TempDir(), "gone"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "goprofile: walk") {
		t.Fatalf("err = %v", err)
	}
}
