package reuse

import (
	"errors"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

func TestDecodeGoListReadsTheRecordStream(t *testing.T) {
	// Arrange
	out := []byte(`{"ImportPath":"fmt","Dir":"/goroot/src/fmt","Standard":true}
{"ImportPath":"m/pkg","Dir":"/r/pkg","Module":{"Path":"m","Dir":"/r","GoMod":"/r/go.mod"},"CgoFiles":["binding.go"]}`)

	// Act
	pkgs, err := DecodeGoList(out)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(pkgs) != 2 || !pkgs[0].Standard || pkgs[1].Module.GoMod != "/r/go.mod" || !slices.Equal(pkgs[1].CgoFiles, []string{"binding.go"}) {
		t.Fatalf("DecodeGoList = %+v", pkgs)
	}
}

func TestDecodeGoListRefusesBrokenOutput(t *testing.T) {
	// Act
	_, err := DecodeGoList([]byte(`{"ImportPath":`))

	// Assert
	if err == nil {
		t.Fatal("broken go list output was accepted")
	}
}

func TestAddGoPackagesPlacesThePackagesInTheCheckout(t *testing.T) {
	// Arrange
	root := writeTree(t, map[string]string{
		"mod/go.mod":                 "module m",
		"mod/go.sum":                 "",
		"mod/pkg/a.go":               "package pkg",
		"mod/pkg/testdata/case.json": "{}",
		"mod/plain/b.go":             "package plain",
		"other/go.mod":               "module o",
		"other/lib/c.go":             "package lib",
	})
	pkgs := []GoPackage{
		{ImportPath: "fmt", Dir: "/goroot/src/fmt", Standard: true},
		{ImportPath: "example.com/cached", Dir: t.TempDir(), Module: &GoModule{GoMod: "/cache/go.mod"}},
		{ImportPath: "m/pkg", Dir: filepath.Join(root, "mod", "pkg"), Module: &GoModule{GoMod: filepath.Join(root, "mod", "go.mod")}},
		{ImportPath: "m/plain", Dir: filepath.Join(root, "mod", "plain"), Module: &GoModule{GoMod: filepath.Join(root, "mod", "go.mod")}},
		{ImportPath: "o/lib", Dir: filepath.Join(root, "other", "lib"), Module: &GoModule{GoMod: filepath.Join(root, "other", "go.mod")}},
	}
	set := NewInputSet()

	// Act
	err := AddGoPackages(set, root, pkgs)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got, want := set.FlatDirs(), []string{"mod/pkg", "mod/plain", "other/lib"}; !slices.Equal(got, want) {
		t.Errorf("FlatDirs = %v, want %v", got, want)
	}
	if got, want := set.Trees(), []string{"mod/pkg/testdata"}; !slices.Equal(got, want) {
		t.Errorf("Trees = %v, want %v", got, want)
	}
	// other/ has no go.sum, so only its go.mod is placed.
	if got, want := set.Files(), []string{"mod/go.mod", "mod/go.sum", "other/go.mod"}; !slices.Equal(got, want) {
		t.Errorf("Files = %v, want %v", got, want)
	}
}

func TestAPackageInTheCheckoutWithoutAGoModIsAnError(t *testing.T) {
	// Arrange
	root := writeTree(t, map[string]string{"pkg/a.go": "package pkg"})
	pkgs := []GoPackage{{ImportPath: "m/pkg", Dir: filepath.Join(root, "pkg")}}

	// Act
	err := AddGoPackages(NewInputSet(), root, pkgs)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no go.mod") {
		t.Fatalf("err = %v, want the missing go.mod named", err)
	}
}

func TestAddPatternWalkPlacesWhatDotDotDotWalks(t *testing.T) {
	// Arrange
	root := writeTree(t, map[string]string{
		"mod/go.mod":                "module m",
		"mod/internal/a/a.go":       "package a",
		"mod/internal/a/testdata/x": "",
		"mod/.hidden/h.go":          "package h",
		"mod/_skip/s.go":            "package s",
		"mod/nested/go.mod":         "module n",
		"mod/nested/inner/i.go":     "package inner",
	})
	set := NewInputSet()

	// Act
	err := AddPatternWalk(set, root, "mod")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	// The nested module's own directory is listed (the go command looks in
	// it for its go.mod) and nothing beneath it.
	if got, want := set.Dirs(), []string{"mod", "mod/internal", "mod/internal/a", "mod/nested"}; !slices.Equal(got, want) {
		t.Fatalf("Dirs = %v, want %v", got, want)
	}
}

func TestAddGoPackagesPlacesACgoPackagesWholeTree(t *testing.T) {
	// Arrange: a cgo binding whose C includes a header from a subdirectory.
	root := writeTree(t, map[string]string{
		"mod/go.mod":                           "module m",
		"mod/grammar/src/binding.go":           "package grammar",
		"mod/grammar/src/tree_sitter/parser.h": "",
	})
	pkgs := []GoPackage{{ImportPath: "m/grammar", Dir: filepath.Join(root, "mod", "grammar", "src"),
		Module: &GoModule{GoMod: filepath.Join(root, "mod", "go.mod")}, CgoFiles: []string{"binding.go"}}}
	set := NewInputSet()

	// Act
	err := AddGoPackages(set, root, pkgs)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got, want := set.Trees(), []string{"mod/grammar/src"}; !slices.Equal(got, want) {
		t.Errorf("Trees = %v, want %v: the whole package, its headers' subdirectories included", got, want)
	}
	if got := set.FlatDirs(); len(got) != 0 {
		t.Errorf("FlatDirs = %v, want none: the tree covers the package", got)
	}
}

func TestMemoGoListAnswersEachListingOnce(t *testing.T) {
	// Arrange
	calls := map[string]int{}
	l := MemoGoList(func(dir, tags string, tests bool, patterns ...string) ([]GoPackage, error) {
		calls[dir+tags]++
		if dir == "/bad" {
			return nil, errors.New("go list exited 1")
		}
		return []GoPackage{{ImportPath: dir}}, nil
	})

	// Act
	a1, _ := l("/a", "", true, "./...")
	a2, _ := l("/a", "", true, "./...")
	tagged, _ := l("/a", "integration", true, "./...")
	_, err1 := l("/bad", "", true, "./...")
	_, err2 := l("/bad", "", true, "./...")

	// Assert
	if calls["/a"] != 1 || calls["/aintegration"] != 1 || calls["/bad"] != 1 {
		t.Fatalf("listings run %v, want each distinct one once", calls)
	}
	if a1[0].ImportPath != "/a" || a2[0].ImportPath != "/a" || tagged[0].ImportPath != "/a" {
		t.Fatalf("answers %v %v %v", a1, a2, tagged)
	}
	if err1 == nil || err2 == nil || err1.Error() != "go list exited 1" {
		t.Fatalf("a failed listing answered %v then %v, want its error both times", err1, err2)
	}
}

func TestAddGoPackagesPlacesEveryEmbeddedFile(t *testing.T) {
	// Arrange: embeds reaching into subdirectories no flat placement holds.
	root := writeTree(t, map[string]string{
		"mod/go.mod":             "module m",
		"mod/pkg/a.go":           "package pkg",
		"mod/pkg/assets/x.txt":   "x",
		"mod/pkg/assets/t/y.txt": "y",
		"mod/pkg/xt/z.txt":       "z",
	})
	pkgs := []GoPackage{{ImportPath: "m/pkg", Dir: filepath.Join(root, "mod", "pkg"), Module: &GoModule{GoMod: filepath.Join(root, "mod", "go.mod")},
		EmbedFiles: []string{"assets/x.txt"}, TestEmbedFiles: []string{"assets/t/y.txt"}, XTestEmbedFiles: []string{"xt/z.txt"}}}
	set := NewInputSet()

	// Act
	err := AddGoPackages(set, root, pkgs)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got, want := set.Files(), []string{"mod/go.mod", "mod/pkg/assets/t/y.txt", "mod/pkg/assets/x.txt", "mod/pkg/xt/z.txt"}; !slices.Equal(got, want) {
		t.Errorf("Files = %v, want %v", got, want)
	}
}
