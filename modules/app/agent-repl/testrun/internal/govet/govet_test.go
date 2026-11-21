package govet

import (
	"bytes"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func TestGoVetArgs(t *testing.T) {
	tests := []struct {
		name string
		o    Options
		want []string
	}{
		{"a plain vet", Options{Packages: []string{".", "a"}}, append(append([]string{"vet", "-json"}, Flags...), ".", "./a")},
		{"a tagged vet", Options{Tags: "integration", Packages: []string{"a"}}, append(append([]string{"vet", "-json"}, Flags...), "-tags", "integration", "./a")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := GoVetArgs(tt.o)

			// Assert
			if err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("GoVetArgs = %q, %v; want %q", got, err, tt.want)
			}
		})
	}
}

func TestGoVetArgsRefusesNoPackages(t *testing.T) {
	// Act
	_, err := GoVetArgs(Options{})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "a vet of no packages") {
		t.Fatalf("GoVetArgs = %v, want a refusal", err)
	}
}

// vetOutput is `go vet -json` output over x/a (two findings, one in its
// external test package), x/b (clean, with a test variant line) and x/c
// (a type error).
const vetOutput = `# x/a
# [x/a]
{
	"x/a": {
		"printf": [
			{"posn": "/m/a/a.go:3:12", "message": "bad format"}
		]
	}
}
# x/a_test
{
	"x/a_test": {
		"tests": [
			{"posn": "/m/a/a_ext_test.go:5:1", "message": "malformed example"}
		]
	}
}
# x/b
# [x/b]
{}
# x/c
vet: c/c.go:2:12: undefined: y
`

func TestSectionsSplitsEachPackagesBlock(t *testing.T) {
	// Act
	got, err := Sections([]byte(vetOutput))

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	var pkgs []string
	for _, s := range got {
		pkgs = append(pkgs, s.Package)
	}
	if want := []string{"x/a", "x/a_test", "x/b", "x/c"}; !reflect.DeepEqual(pkgs, want) {
		t.Fatalf("sections = %v, want %v", pkgs, want)
	}
	if got[2].Body != "{}\n" {
		t.Fatalf("x/b's body = %q, want its report without the variant line", got[2].Body)
	}
}

func TestSectionsRefusesOutputBeforeAnyHeader(t *testing.T) {
	// Act
	_, err := Sections([]byte("go: cannot find main module\n"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "before any package header") {
		t.Fatalf("Sections = %v, want a refusal", err)
	}
}

func moduleDir(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "go.mod"), []byte("module x\n\ngo 1.24\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	return dir
}

func fakeVet(stderr string, exitedZero bool, err error) (GoVet, *[]string) {
	var gotArgs []string
	return func(_ string, args []string) ([]byte, bool, error) {
		gotArgs = args
		return []byte(stderr), exitedZero, err
	}, &gotArgs
}

func TestVetRecordsEachPackagesVerdict(t *testing.T) {
	// Arrange
	dir := moduleDir(t)
	results := t.TempDir()
	goVet, _ := fakeVet(vetOutput, false, nil)

	// Act
	res, err := Vet(Options{Module: dir, ResultDir: results, Packages: []string{"a", "b", "c"}}, goVet)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(res.Passed, []string{"b"}) {
		t.Fatalf("passed = %v, want [b]", res.Passed)
	}
	wantA := "/m/a/a.go:3:12: bad format (printf)\n/m/a/a_ext_test.go:5:1: malformed example (tests)\n"
	if res.Failed["a"] != wantA {
		t.Fatalf("a's findings = %q, want %q", res.Failed["a"], wantA)
	}
	if !strings.Contains(res.Failed["c"], "undefined: y") {
		t.Fatalf("c's verdict = %q, want its type error", res.Failed["c"])
	}
	var out bytes.Buffer
	if err := Check(ResultPath(results, "b"), &out); err != nil {
		t.Fatalf("Check(b) = %v, want a pass", err)
	}
	if err := Check(ResultPath(results, "a"), &out); !errors.Is(err, ErrFindings) || out.String() != wantA {
		t.Fatalf("Check(a) = %v with %q, want its findings", err, out.String())
	}
}

func TestVetFailsAPackageItNeverAnalyzedWithItsWholeOutput(t *testing.T) {
	// Arrange: a dependency outside the list did not compile, so vet never
	// reached b.
	dir := moduleDir(t)
	results := t.TempDir()
	goVet, _ := fakeVet("# x/dep\ndep/dep.go:1: bad\n", false, nil)

	// Act
	res, err := Vet(Options{Module: dir, ResultDir: results, Packages: []string{"b"}}, goVet)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(res.Failed["b"], "did not analyze") || !strings.Contains(res.Failed["b"], "dep/dep.go:1: bad") {
		t.Fatalf("b's verdict = %q, want vet's whole output", res.Failed["b"])
	}
}

func TestVetPassesAPackageOfOnlyExternalTestFiles(t *testing.T) {
	// Arrange: the module root holds only package x_test files, so vet
	// writes a block for x_test and none for x.
	dir := moduleDir(t)
	results := t.TempDir()
	goVet, _ := fakeVet("# x_test\n# [x_test]\n{}\n", true, nil)

	// Act
	res, err := Vet(Options{Module: dir, ResultDir: results, Packages: []string{"."}}, goVet)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(res.Passed, []string{"."}) {
		t.Fatalf("passed = %v, failed = %v; want . passed", res.Passed, res.Failed)
	}
}

func TestVetRefuses(t *testing.T) {
	tests := []struct {
		name       string
		stderr     string
		exitedZero bool
		runErr     error
		want       string
	}{
		{"go vet could not run", "", false, errors.New("exec: not found"), "run go vet"},
		{"output that is not go vet -json's", "flag provided but not defined\n", false, nil, "before any package header"},
		{"a non-zero exit with every package passing", "# x/b\n{}\n", false, nil, "exited non-zero"},
		{"an analyzer report that is neither findings nor an error", "# x/b\n{\"x/b\": {\"printf\": 7}}\n", true, nil, "neither findings nor an error"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			results := t.TempDir()
			goVet, _ := fakeVet(tt.stderr, tt.exitedZero, tt.runErr)

			// Act
			_, err := Vet(Options{Module: moduleDir(t), ResultDir: results, Packages: []string{"b"}}, goVet)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("Vet = %v, want %q", err, tt.want)
			}
			if _, err := os.Stat(ResultPath(results, "b")); !errors.Is(err, os.ErrNotExist) {
				t.Fatalf("a refused vet wrote a verdict: %v", err)
			}
		})
	}
}

func TestVetRecordsAnAnalyzerThatFailed(t *testing.T) {
	// Arrange
	goVet, _ := fakeVet("# x/b\n{\"x/b\": {\"printf\": {\"error\": \"boom\"}}}\n", true, nil)

	// Act
	res, err := Vet(Options{Module: moduleDir(t), ResultDir: t.TempDir(), Packages: []string{"b"}}, goVet)

	// Assert
	if err != nil || res.Failed["b"] != "x/b: printf failed: boom\n" {
		t.Fatalf("Vet = %+v, %v; want b failed by its analyzer", res, err)
	}
}

func TestCheckRefusesAVerdictNeverWritten(t *testing.T) {
	// Act
	err := Check(filepath.Join(t.TempDir(), "_.vet"), &bytes.Buffer{})

	// Assert
	if err == nil || errors.Is(err, ErrFindings) || !strings.Contains(err.Error(), "never written") {
		t.Fatalf("Check = %v, want a missing verdict", err)
	}
}

func TestParseArgsRoundTripsArgs(t *testing.T) {
	tests := []struct {
		name string
		o    Options
	}{
		{"plain", Options{Module: "/m", ResultDir: "/r", Packages: []string{".", "a"}}},
		{"tagged", Options{Module: "/m", ResultDir: "/r", Tags: "integration", Packages: []string{"a"}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParseArgs(tt.o.Args())

			// Assert
			if err != nil || !reflect.DeepEqual(got, tt.o) {
				t.Fatalf("ParseArgs(Args()) = %+v, %v; want %+v", got, err, tt.o)
			}
		})
	}
}

func TestParseArgsRefuses(t *testing.T) {
	tests := []struct {
		name string
		argv []string
		want string
	}{
		{"a flag without its value", []string{"-module"}, "-module needs a value"},
		{"an unknown argument", []string{"-x"}, `unknown argument "-x"`},
		{"no module", []string{"-resultdir", "/r", "--", "a"}, "needs -module and -resultdir"},
		{"a relative directory", []string{"-module", "m", "-resultdir", "/r", "--", "a"}, "must be absolute"},
		{"no packages", []string{"-module", "/m", "-resultdir", "/r", "--"}, "names no package"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := ParseArgs(tt.argv)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("ParseArgs(%q) = %v, want %q", tt.argv, err, tt.want)
			}
		})
	}
}

// TestVetReadsTheRealGoVetsOutput pins the output format the parse assumes
// against the installed go command, run through Exec: a clean package, one with a finding in
// its test file, one that does not type-check, and one of only external test files.
func TestVetReadsTheRealGoVetsOutput(t *testing.T) {
	// Arrange
	dir := moduleDir(t)
	files := map[string]string{
		"a/a.go":      "package a\n\nfunc A() {}\n",
		"a/a_test.go": "package a\n\nimport \"testing\"\n\nfunc TestA(t *testing.T) { t.Logf(\"%d\", \"x\") }\n",
		"b/b.go":      "package b\n\nfunc B() {}\n",
		"c/c.go":      "package c\n\nfunc C() { undefined() }\n",
		"d/d_test.go": "package d_test\n\nimport \"testing\"\n\nfunc TestD(t *testing.T) {}\n",
	}
	for name, body := range files {
		path := filepath.Join(dir, name)
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	res, err := Vet(Options{Module: dir, ResultDir: t.TempDir(), Packages: []string{"a", "b", "c", "d"}}, Exec)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(res.Passed, []string{"b", "d"}) {
		t.Fatalf("passed = %v, want [b d]; failed = %v", res.Passed, res.Failed)
	}
	if !strings.Contains(res.Failed["a"], "a_test.go:5") || !strings.Contains(res.Failed["c"], "undefined") {
		t.Fatalf("failed = %v, want a's printf finding and c's type error", res.Failed)
	}
}
