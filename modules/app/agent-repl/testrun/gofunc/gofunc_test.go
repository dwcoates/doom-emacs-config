package gofunc

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"agentrepl/testrun/internal/testdir"
)

func TestIsTestFunc(t *testing.T) {
	src := `package p

import "testing"

func TestA(t *testing.T)      {}
func Test_b(t *testing.T)     {}
func Testable(t *testing.T)   {}
func TestMain(m *testing.M)   {}
func TestNoParam()            {}
func TestTwo(a, b *testing.T) {}
func TestValue(t testing.T)   {}
func FuzzF(f *testing.F)      {}
func testStep(t *testing.T)   {}
`
	f, err := parser.ParseFile(token.NewFileSet(), "p_test.go", src, 0)
	if err != nil {
		t.Fatal(err)
	}
	want := map[string]bool{"TestA": true, "Test_b": true, "Testable": false, "TestMain": false, "TestNoParam": false, "TestTwo": false, "TestValue": false}
	for _, d := range f.Decls {
		fn, ok := d.(*ast.FuncDecl)
		if !ok {
			continue
		}
		name := fn.Name.Name
		if name == "testStep" {
			if !IsTestFunc(fn, "test", "T") || IsTestFunc(fn, "Test", "T") {
				t.Errorf("testStep is an e2e scenario step's shape (prefix test) and only that")
			}
			continue
		}
		if name == "FuzzF" {
			if !IsTestFunc(fn, "Fuzz", "F") || IsTestFunc(fn, "Test", "T") {
				t.Errorf("FuzzF is a fuzz test and only that")
			}
			continue
		}
		if got := IsTestFunc(fn, "Test", "T"); got != want[name] {
			t.Errorf("IsTestFunc(%s) = %t, want %t", name, got, want[name])
		}
	}
}

// handRolledParamCheck is the comparison IsTestFunc makes on a parameter's
// selector, which no other code should make for itself.
var handRolledParamCheck = regexp.MustCompile(`Sel\.Name == ("T"|"F"|"M"|param)`)

func TestEveryTestShapeCheckGoesThroughIsTestFunc(t *testing.T) {
	// Arrange
	module := testdir.ModuleRoot(t)
	var found []string

	// Act
	for _, tree := range []string{"testrun", "e2e"} {
		err := filepath.WalkDir(filepath.Join(module, tree), func(path string, d fs.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() && d.Name() == "node_modules" {
				return filepath.SkipDir
			}
			if d.IsDir() || !strings.HasSuffix(path, ".go") || filepath.Dir(path) == filepath.Join(module, "testrun", "gofunc") {
				return nil
			}
			body, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			if handRolledParamCheck.Match(body) {
				found = append(found, path)
			}
			return nil
		})
		if err != nil {
			t.Fatalf("walk %s: %v", tree, err)
		}
	}

	// Assert
	if len(found) > 0 {
		t.Fatalf("files checking a test function's parameter by hand: %s; use gofunc.IsTestFunc", strings.Join(found, ", "))
	}
}
