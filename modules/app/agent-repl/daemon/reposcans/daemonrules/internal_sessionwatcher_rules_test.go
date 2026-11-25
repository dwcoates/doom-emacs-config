package daemonrules

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalSessionwatcherDir is the package these rules hold, under the daemon module's root.
const internalSessionwatcherDir = "internal/sessionwatcher"

// sinkMethods answers, for every Sinks field whose type is an interface this
// package declares, the field's name mapped to its interface's method names.
func sinkMethods(t *testing.T, files []*ast.File) map[string][]string {
	t.Helper()
	interfaces := map[string][]string{}
	var fields map[string]string
	for _, file := range files {
		ast.Inspect(file, func(n ast.Node) bool {
			spec, ok := n.(*ast.TypeSpec)
			if !ok {
				return true
			}
			switch typ := spec.Type.(type) {
			case *ast.InterfaceType:
				methods := []string{}
				for _, m := range typ.Methods.List {
					for _, name := range m.Names {
						methods = append(methods, name.Name)
					}
				}
				interfaces[spec.Name.Name] = methods
			case *ast.StructType:
				if spec.Name.Name != "Sinks" {
					return true
				}
				fields = map[string]string{}
				for _, f := range typ.Fields.List {
					ident, ok := f.Type.(*ast.Ident)
					if !ok {
						continue
					}
					for _, name := range f.Names {
						fields[name.Name] = ident.Name
					}
				}
			}
			return true
		})
	}
	if fields == nil {
		t.Fatal("the package declares no Sinks struct")
	}
	out := map[string][]string{}
	for field, typ := range fields {
		if methods, ok := interfaces[typ]; ok {
			out[field] = methods
		}
	}
	return out
}

// sinkCalls answers every `<x>.sinks.<Field>.<Method>(...)` and
// `sinks.<Field>.<Method>(...)` call in files, as "Field.Method".
func sinkCalls(files []*ast.File) map[string]bool {
	out := map[string]bool{}
	for _, file := range files {
		ast.Inspect(file, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			method, ok := call.Fun.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			field, ok := method.X.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			switch holder := field.X.(type) {
			case *ast.Ident:
				if holder.Name != "sinks" {
					return true
				}
			case *ast.SelectorExpr:
				if holder.Sel.Name != "sinks" {
					return true
				}
			default:
				return true
			}
			out[field.Sel.Name+"."+method.Sel.Name] = true
			return true
		})
	}
	return out
}

// TestEverySinkMethodHasAWatcherCaller holds the watcher's fan-out to the
// sinks it declares: a method a sink asks for and the watcher never calls is a
// fact that resolver can never learn. The roster's OnActivity was such a
// method, and its `api_retrying` outlived every vendor reconnect.
func TestEverySinkMethodHasAWatcherCaller(t *testing.T) {
	sourcescan.InPackage(t, internalSessionwatcherDir)
	// Arrange.
	fset := token.NewFileSet()
	var files []*ast.File
	for _, src := range sourcescan.Production(t, internalSessionwatcherDir) {
		parsed, err := parser.ParseFile(fset, src.Name, src.Source, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", src.Name, err)
		}
		files = append(files, parsed)
	}
	sinks := sinkMethods(t, files)
	var wanted []string
	for field, methods := range sinks {
		for _, method := range methods {
			wanted = append(wanted, field+"."+method)
		}
	}
	sort.Strings(wanted)
	if len(wanted) == 0 {
		t.Fatal("found no sink methods to hold; the scan is broken")
	}

	// Act.
	called := sinkCalls(files)

	// Assert.
	for _, name := range wanted {
		t.Run(name, func(t *testing.T) {
			if !called[name] {
				t.Fatalf("the watcher never calls sinks.%s; the resolver behind it never learns the fact it asks for", name)
			}
		})
	}
}

func TestEveryViewFindsASubagentNewToTheSetThroughAgentsNewSince(t *testing.T) {
	sourcescan.InPackage(t, internalSessionwatcherDir)
	// Arrange: the views that take a run's end as final, and must draw the
	// subagent live again when the set lists it anew.
	want := []string{"internal/resolve/feed/subagent.go", "internal/resolve/footer/livework.go"}

	// Act.
	root := sourcescan.DaemonRoot(t)
	var sites []string
	sourcescan.EachDaemonSource(t, root, nil, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			if sel, ok := n.(*ast.SelectorExpr); ok && sel.Sel.Name == "AgentsNewSince" {
				rel, err := filepath.Rel(root, fset.Position(sel.Pos()).Filename)
				if err != nil {
					t.Fatalf("relative path: %v", err)
				}
				sites = append(sites, rel)
			}
			return true
		})
	})

	// Assert.
	sort.Strings(sites)
	if !reflect.DeepEqual(sites, want) {
		t.Fatalf("AgentsNewSince is called from %v, want %v", sites, want)
	}
}

// TestTheWatchersLatchesShareOneWait fails a latch wait hand-rolled beside
// awaitLatch: a select on the watcher's own close belongs in it alone.
func TestTheWatchersLatchesShareOneWait(t *testing.T) {
	sourcescan.InPackage(t, internalSessionwatcherDir)
	// Arrange.
	source, err := os.ReadFile("await.go")
	if err != nil {
		t.Fatalf("read await.go: %v", err)
	}

	// Act.
	selects := strings.Count(string(source), "case <-w.ctx.Done():")

	// Assert.
	if selects != 1 {
		t.Fatalf("await.go selects on the watcher's close %d times, want once, in awaitLatch", selects)
	}
}

// TestFreenessEdgeAssertionsGoThroughTheHarnessHelpers keeps every read of the
// lifecycle sink's freeness edges inside awaitFree, noFree, freeToldNow and
// drainFrees, so no test hand-rolls a select whose deadline or workspace check
// drifts from theirs.
func TestFreenessEdgeAssertionsGoThroughTheHarnessHelpers(t *testing.T) {
	sourcescan.InPackage(t, internalSessionwatcherDir)
	// Arrange.
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("list the package's test files: %v", err)
	}

	// Act.
	var outside []string
	for _, file := range files {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		reads := strings.Count(string(body), "<-h.rec.frees")
		if file == "livework_test.go" {
			// The four helpers' own reads; this rule's needle lives in
			// reposcans, outside the files it reads.
			reads -= 4
		}
		if reads != 0 {
			outside = append(outside, file)
		}
	}

	// Assert.
	if len(outside) != 0 {
		t.Fatalf("freeness edges read outside the harness helpers in %v", outside)
	}
}
