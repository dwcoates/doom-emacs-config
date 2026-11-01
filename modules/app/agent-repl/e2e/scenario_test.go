package e2e

import (
	"fmt"
	"go/ast"
	"go/build"
	"go/parser"
	"go/token"
	"os"
	"reflect"
	"runtime"
	"slices"
	"strings"
	"testing"
	"unicode"

	"agentrepl/testrun/gofunc"
)

// A SCENARIO IS ONE TOP-LEVEL TEST WHOSE NAMED STEPS RUN IN ORDER.
//
// The gate runs every e2e test side by side under the host's CPU cap, and a
// test that passes alone fails there waiting on a CPU inside its bound. So the
// worlds are few: scenarios run in parallel with each other, and the steps of
// one run one at a time, each over the world it builds or takes, torn down
// before the next step starts (scenarios_test.go lists them).
//
// A STEP IS A FUNCTION NAMED testXxx, and its case is named Xxx: the former
// top-level TestXxx is the step Scenario/Xxx, so the name a red run prints is
// the name the source declares, and nothing hand-spelled can drift from it.
// A step reports pass or fail on its own; a failed step ends only itself and
// the steps after it still run. The merge queue reruns a failed step by its
// path (daemon/internal/merge/failingtests.go).

// stepRunner is the part of *testing.T a scenario drives.
type stepRunner interface {
	Helper()
	Parallel()
	Run(name string, f func(*testing.T)) bool
}

// scenario runs steps as t's named cases, in order, beside the other
// scenarios.
func scenario(t *testing.T, steps ...func(*testing.T)) {
	t.Helper()
	runScenario(t, steps)
}

// runScenario is scenario over any stepRunner.
func runScenario(r stepRunner, steps []func(*testing.T)) {
	r.Helper()
	if len(steps) == 0 {
		panic("e2e: a scenario with no steps runs nothing")
	}
	r.Parallel()
	for _, step := range steps {
		r.Run(stepName(step), step)
	}
}

// stepName is the case name of STEP, its function's name without the "test"
// prefix. A step that is not a package-level function named testXxx is a bug
// in the scenario's list, and panics.
func stepName(step func(*testing.T)) string {
	full := runtime.FuncForPC(reflect.ValueOf(step).Pointer()).Name()
	name := full[strings.LastIndex(full, ".")+1:]
	rest, ok := strings.CutPrefix(name, stepPrefix)
	if !ok || rest == "" || !unicode.IsUpper(rune(rest[0])) {
		panic(fmt.Sprintf("e2e: scenario step %s is not a function named %sXxx", full, stepPrefix))
	}
	return rest
}

// stepPrefix starts every step function's name.
const stepPrefix = "test"

// scenarioUnitFile is the scenario helper's own unit tests, whose fixture
// steps no scenario lists.
const scenarioUnitFile = "scenario_unit_test.go"

// THE SCENARIO INVENTORY AS THE SOURCE DECLARES IT, read by the scans that
// keep it whole (scenario_unit_test.go) and by the scenario matrix, which
// names a step by its path.

// worldBuilders are the functions that build a world a step runs in.
var worldBuilders = []string{"NewWorld", "NewWorldWithSidecarStaleness", "SharedWorld", "NewEmacsWorld", "StartEmacs", "NewDoomWorld"}

// e2ePackage is the e2e package's source as the scans read it, under the
// default build context (so the perf-tagged files, a measurement of their
// own, are left out).
type e2ePackage struct {
	// tests are the top-level Test funcs, by name, with their files.
	tests map[string]string
	// steps are the funcs named testXxx(t *testing.T), with their files.
	steps map[string]string
	// scenarios are the steps each scenario lists, by the scenario's name.
	scenarios map[string][]string
	// refs are the package-level names each func's body names.
	refs map[string][]string
}

func parseE2EPackage(t *testing.T) e2ePackage {
	t.Helper()
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("read the e2e package: %v", err)
	}
	pkg := e2ePackage{tests: map[string]string{}, steps: map[string]string{}, scenarios: map[string][]string{}, refs: map[string][]string{}}
	fset := token.NewFileSet()
	for _, e := range entries {
		name := e.Name()
		if e.IsDir() || !strings.HasSuffix(name, ".go") {
			continue
		}
		if ok, err := build.Default.MatchFile(".", name); err != nil {
			t.Fatalf("match %s: %v", name, err)
		} else if !ok {
			continue
		}
		f, err := parser.ParseFile(fset, name, nil, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		for _, decl := range f.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			fname := fn.Name.Name
			pkg.refs[fname] = append(pkg.refs[fname], namesIn(fn.Body)...)
			if fn.Recv != nil {
				continue
			}
			switch {
			case gofunc.IsTestFunc(fn, "Test", "T"):
				pkg.tests[fname] = name
				if steps, ok := scenarioSteps(fn); ok {
					pkg.scenarios[fname] = steps
				}
			case gofunc.IsTestFunc(fn, stepPrefix, "T") && name != scenarioUnitFile:
				pkg.steps[fname] = name
			}
		}
	}
	return pkg
}

// scenarioSteps is the steps a scenario's body lists, when its body is
// exactly one call scenario(t, steps...).
func scenarioSteps(fn *ast.FuncDecl) ([]string, bool) {
	if len(fn.Body.List) != 1 {
		return nil, false
	}
	stmt, ok := fn.Body.List[0].(*ast.ExprStmt)
	if !ok {
		return nil, false
	}
	call, ok := stmt.X.(*ast.CallExpr)
	if !ok {
		return nil, false
	}
	if id, ok := call.Fun.(*ast.Ident); !ok || id.Name != "scenario" || len(call.Args) < 2 {
		return nil, false
	}
	var steps []string
	for _, arg := range call.Args[1:] {
		id, ok := arg.(*ast.Ident)
		if !ok {
			// Not a named function: listed as itself so the scan names it.
			steps = append(steps, "<not a named function>")
			continue
		}
		steps = append(steps, id.Name)
	}
	return steps, true
}

// namesIn is every identifier node names, and every selector's selected name
// (so a method call is followed by its method's name).
func namesIn(node ast.Node) []string {
	var names []string
	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.Ident:
			names = append(names, x.Name)
		case *ast.SelectorExpr:
			names = append(names, x.Sel.Name)
		}
		return true
	})
	return names
}

// reachesWorldBuilder is the world builder the func NAME reaches through the
// package-level names its body (and theirs, transitively) names, or "".
func (p e2ePackage) reachesWorldBuilder(name string) string {
	seen := map[string]bool{name: true}
	queue := []string{name}
	for len(queue) > 0 {
		next := queue[0]
		queue = queue[1:]
		for _, ref := range p.refs[next] {
			if slices.Contains(worldBuilders, ref) {
				return ref
			}
			if _, declared := p.refs[ref]; declared && !seen[ref] {
				seen[ref] = true
				queue = append(queue, ref)
			}
		}
	}
	return ""
}

// stepPath is the path a red run names step STEP by, Scenario/Xxx, when a
// scenario lists it.
func (p e2ePackage) stepPath(step string) (string, bool) {
	for scenarioName, steps := range p.scenarios {
		if slices.Contains(steps, step) {
			return scenarioName + "/" + strings.TrimPrefix(step, stepPrefix), true
		}
	}
	return "", false
}
