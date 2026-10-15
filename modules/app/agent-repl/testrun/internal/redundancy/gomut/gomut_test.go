package gomut

import (
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"regexp"
	"strconv"
	"strings"
	"testing"
)

// fixture is a program each of whose mutants changes what it prints, and
// whose guard() call panics if a short circuit is ever evaluated eagerly.
const fixture = `package main

import (
	"errors"
	"fmt"
)

func less(a, b int) bool { return a < b }

func eq(a, b int) bool { return a == b }

func and(a, b bool) bool { return a && b }

func not(a bool) bool { return !a }

func add(a, b int) int { return a + b }

func rem(a, b int) int { return a % b }

func lit() bool { return true }

var calls int

func bump() { calls++ }

func call() int {
	calls = 0
	bump()
	return calls
}

func fail() (int, error) { return 0, errors.New("x") }

func guard(p *int) bool { return p != nil && *p > 0 }

// T, F and zero are constants and a variable the schema leaves alone, so
// every mutant above is reached by an input it changes.
const T, F = true, false

var zero int

func main() {
	_, err := fail()
	fmt.Println(less(1, 1), eq(1, 1), and(T, F), not(T), add(2, 3), rem(7, 4), lit(), call(), err != nil, guard(nil), guard(&zero))
}
`

func module(t *testing.T, src string) string {
	t.Helper()
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "go.mod"), []byte("module fixture\n\ngo 1.24\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "main.go"), []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	return dir
}

func build(t *testing.T, dir string, overlay string) (string, string) {
	t.Helper()
	bin := filepath.Join(t.TempDir(), "fixture")
	args := []string{"build", "-buildvcs=false", "-o", bin}
	if overlay != "" {
		args = append(args, "-overlay", overlay)
	}
	cmd := exec.Command("go", append(args, ".")...)
	cmd.Dir = dir
	out, err := cmd.CombinedOutput()
	if err != nil {
		return "", string(out)
	}
	return bin, ""
}

func overlay(t *testing.T, s *Schema) string {
	t.Helper()
	dir := t.TempDir()
	replace, err := s.Overlay(dir)
	if err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(dir, "overlay.json")
	if err := WriteOverlay(path, replace); err != nil {
		t.Fatal(err)
	}
	return path
}

func runFixture(t *testing.T, bin string, mutant int) string {
	t.Helper()
	cmd := exec.Command(bin)
	cmd.Env = append(os.Environ(), EnvMutant+"="+strconv.Itoa(mutant))
	if mutant == 0 {
		cmd.Env = append(os.Environ(), EnvMutant+"=")
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		return "CRASH: " + err.Error()
	}
	return string(out)
}

func TestSchemaEveryMutantChangesTheFixtureAndNoneActiveIsTheOriginal(t *testing.T) {
	// Arrange
	dir := module(t, fixture)
	orig, errs := build(t, dir, "")
	if errs != "" {
		t.Fatal(errs)
	}
	want := runFixture(t, orig, 0)
	s, err := Build(dir, []string{"main.go"}, 1)
	if err != nil {
		t.Fatal(err)
	}
	ov := overlay(t, s)

	// Act
	bin, errs := build(t, dir, ov)

	// Assert
	if errs != "" {
		t.Fatalf("the schema does not build:\n%s", errs)
	}
	if got := runFixture(t, bin, 0); got != want {
		t.Fatalf("with no mutant active the schema prints %q, the original %q", got, want)
	}
	for _, m := range s.Mutants {
		if got := runFixture(t, bin, m.ID); got == want {
			t.Errorf("mutant %s changes nothing the fixture prints", m.Key())
		}
	}
}

func TestSchemaLaysOutEachOperator(t *testing.T) {
	// Arrange
	dir := module(t, fixture)

	// Act
	s, err := Build(dir, []string{"main.go"}, 1)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	var ops []string
	for _, m := range s.Mutants {
		ops = append(ops, m.Op)
	}
	want := []string{
		"< -> <=", "< -> >=", "== -> !=", "&& -> ||", "drop !", "+ -> -", "% -> *",
		"true -> false", "remove call", "error -> nil",
		"&& -> ||", "!= -> ==", "> -> >=", "> -> <=",
		"remove call", "!= -> ==",
	}
	if !reflect.DeepEqual(ops, want) {
		t.Fatalf("operators = %q\nwant        %q", ops, want)
	}
}

func TestSchemaLeavesAStatementOutsideAListAlone(t *testing.T) {
	// Arrange: a call in an if's init and a for's post may not become an if.
	src := `package main

func f() {}

func main() {
	if f(); true {
	}
	for i := 0; i < 1; f() {
		i++
	}
}
`
	dir := module(t, src)

	// Act
	s, err := Build(dir, []string{"main.go"}, 1)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, m := range s.Mutants {
		if m.Op == "remove call" {
			t.Fatalf("laid out %s on a statement outside a statement list", m.Key())
		}
	}
	ov := overlay(t, s)
	if _, errs := build(t, dir, ov); errs != "" {
		t.Fatalf("the schema does not build:\n%s", errs)
	}
}

var compileError = regexp.MustCompile(`(?m)^(\S+\.go):(\d+):(\d+): `)

func TestDropRemovesTheSiteACompileErrorNamesUntilTheSchemaBuilds(t *testing.T) {
	// Arrange: a string + an identifier the rewrite cannot tell from a sum.
	src := `package main

import "fmt"

func main() {
	a, b := "x", "y"
	fmt.Println(a+b, 1+len(a))
}
`
	dir := module(t, src)
	s, err := Build(dir, []string{"main.go"}, 1)
	if err != nil {
		t.Fatal(err)
	}
	ov := overlay(t, s)
	_, errs := build(t, dir, ov)
	m := compileError.FindStringSubmatch(errs)
	if m == nil {
		t.Fatalf("the string + compiled:\n%s", errs)
	}
	line, _ := strconv.Atoi(m[2])
	col, _ := strconv.Atoi(m[3])
	path := m[1]
	if !filepath.IsAbs(path) {
		path = filepath.Join(dir, path)
	}

	// Act
	dropped, err := s.Drop(path, line, col)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(dropped) != 1 || s.Mutants[dropped[0]-1].Op != "+ -> -" || s.Mutants[dropped[0]-1].Col != 14 {
		t.Fatalf("dropped %v, want the a+b site", dropped)
	}
	ov = overlay(t, s)
	if _, errs := build(t, dir, ov); errs != "" {
		t.Fatalf("the schema still does not build:\n%s", errs)
	}
	if got := len(s.Dropped()); got != 1 {
		t.Errorf("Dropped() holds %d, want 1", got)
	}
	if got := len(s.Live()); got != len(s.Mutants)-1 {
		t.Errorf("Live() holds %d, want every mutant but the dropped one", got)
	}
}

func TestDropRefusesAPositionInNoSite(t *testing.T) {
	// Arrange
	dir := module(t, fixture)
	s, err := Build(dir, []string{"main.go"}, 1)
	if err != nil {
		t.Fatal(err)
	}

	// Act: line 1 is the package clause.
	_, err = s.Drop(filepath.Join(dir, "main.go"), 1, 1)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "in no mutation site") {
		t.Fatalf("err = %v, want the rewrite named wrong", err)
	}
}

func TestBuildRefuses(t *testing.T) {
	tests := []struct {
		name  string
		files map[string]string
		build []string
		want  string
	}{
		{
			name:  "a test file",
			files: map[string]string{"a_test.go": "package main\n"},
			build: []string{"a_test.go"},
			want:  "is a test file",
		},
		{
			name:  "a generated file",
			files: map[string]string{"gen.go": "// Code generated by x. DO NOT EDIT.\n\npackage main\n"},
			build: []string{"gen.go"},
			want:  "is generated",
		},
		{
			name:  "files of two packages",
			files: map[string]string{"a.go": "package a\n", "b.go": "package b\n"},
			build: []string{"a.go", "b.go"},
			want:  "is package b, not a",
		},
		{
			name:  "a missing file",
			build: []string{"gone.go"},
			want:  "read",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			for name, src := range tt.files {
				if err := os.WriteFile(filepath.Join(dir, name), []byte(src), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			_, err := Build(dir, tt.build, 1)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestBuildNumbersFromFirst(t *testing.T) {
	// Arrange
	dir := module(t, fixture)

	// Act
	s, err := Build(dir, []string{"main.go"}, 100)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if s.Mutants[0].ID != 100 || s.Mutants[len(s.Mutants)-1].ID != 100+len(s.Mutants)-1 {
		t.Fatalf("ids run %d..%d, want 100..%d", s.Mutants[0].ID, s.Mutants[len(s.Mutants)-1].ID, 100+len(s.Mutants)-1)
	}
}

func TestBuildRefusesAFirstIDBelowOne(t *testing.T) {
	// Arrange
	dir := module(t, fixture)

	// Act
	_, err := Build(dir, []string{"main.go"}, 0)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "start at 1") {
		t.Fatalf("err = %v, want the first id refused", err)
	}
}

func TestHoldsNamesTheOriginalAndTheOverlayPath(t *testing.T) {
	// Arrange
	dir := module(t, fixture)
	s, err := Build(dir, []string{"main.go"}, 1)
	if err != nil {
		t.Fatal(err)
	}
	scratch := t.TempDir()
	replace, err := s.Overlay(scratch)
	if err != nil {
		t.Fatal(err)
	}
	orig := filepath.Join(dir, "main.go")

	// Act / Assert
	if !s.Holds(orig) || !s.Holds(replace[orig]) {
		t.Fatalf("Holds misses %s or its overlay %s", orig, replace[orig])
	}
	if s.Holds(filepath.Join(dir, "other.go")) {
		t.Fatal("Holds claims a file outside the schema")
	}
}

func TestMutantKeyIsTheFileLineColumnAndOperator(t *testing.T) {
	// Arrange
	m := Mutant{ID: 9, File: "/x/y/sessions.go", Line: 12, Col: 3, Op: "< -> <="}

	// Act
	got := m.Key()

	// Assert
	if got != "sessions.go:12:3:< -> <=" {
		t.Fatalf("Key() = %q", got)
	}
}
