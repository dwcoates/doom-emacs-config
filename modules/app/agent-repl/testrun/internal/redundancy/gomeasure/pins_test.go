package gomeasure

import (
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func writeFiles(t *testing.T, files map[string]string) string {
	t.Helper()
	dir := t.TempDir()
	for name, src := range files {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(src), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return dir
}

const pinFixture = `package p

import "testing"

type fixture struct{}

func (f *fixture) logged() bool { return FindRecord(nil) }

func helper(t *testing.T) { (&fixture{}).logged() }

func FindRecord(any) bool { return true }

func TestMain(m *testing.M) {}

func TestDirect(t *testing.T) { FindRecord(nil) }

func TestThroughHelpers(t *testing.T) { helper(t) }

func TestPlain(t *testing.T) {}

func Testable(t *testing.T) {}

func (fixture) TestMethod(t *testing.T) {}
`

func TestTestFuncsListsOnlyTests(t *testing.T) {
	// Arrange
	dir := writeFiles(t, map[string]string{"p_test.go": pinFixture})

	// Act
	got, err := TestFuncs(dir, []string{"p_test.go"})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if want := []string{"TestDirect", "TestThroughHelpers", "TestPlain"}; !reflect.DeepEqual(got, want) {
		t.Fatalf("TestFuncs = %v, want %v", got, want)
	}
}

func TestTestFuncsRefusesAFileWithNoTests(t *testing.T) {
	// Arrange
	dir := writeFiles(t, map[string]string{"p_test.go": "package p\n"})

	// Act
	_, err := TestFuncs(dir, []string{"p_test.go"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "declare no tests") {
		t.Fatalf("err = %v, want no tests named", err)
	}
}

func TestLogPinsFollowsHelpersAndMethodsByName(t *testing.T) {
	// Arrange
	dir := writeFiles(t, map[string]string{"p_test.go": pinFixture})

	// Act
	got, err := LogPins(dir, []string{"TestDirect", "TestThroughHelpers", "TestPlain"}, []string{"FindRecord"})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	want := map[string]string{
		"TestDirect":         "asserts a log record: TestDirect -> FindRecord",
		"TestThroughHelpers": "asserts a log record: TestThroughHelpers -> helper -> logged -> FindRecord",
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("pins = %v, want %v", got, want)
	}
}

func TestLogPinsRefusesATestNoFileDeclares(t *testing.T) {
	// Arrange
	dir := writeFiles(t, map[string]string{"p_test.go": pinFixture})

	// Act
	_, err := LogPins(dir, []string{"TestGone"}, []string{"FindRecord"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "TestGone is declared in no test file") {
		t.Fatalf("err = %v, want the missing test named", err)
	}
}

func TestLogPinsRefusesAPackageWithNoTestFiles(t *testing.T) {
	// Arrange
	dir := writeFiles(t, map[string]string{"p.go": "package p\n"})

	// Act
	_, err := LogPins(dir, []string{"TestA"}, []string{"FindRecord"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "has no test files") {
		t.Fatalf("err = %v, want no test files named", err)
	}
}
