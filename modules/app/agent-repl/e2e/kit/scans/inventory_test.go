package scans

import (
	"agentrepl/e2e/kit/scenario"
	"agentrepl/testrun/gofunc"
	"go/ast"
	"go/build"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"
)

// THE SCENARIO INVENTORY AS THE SOURCE DECLARES IT, read by the scans that
// keep it whole (inventory_unit_test.go) and by the scenario matrix, which
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
	// docs are each package-level func's doc comment, by name.
	docs map[string]string
}

func parseE2EPackage(t *testing.T) e2ePackage {
	t.Helper()
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("read the e2e package: %v", err)
	}
	pkg := e2ePackage{tests: map[string]string{}, steps: map[string]string{}, scenarios: map[string][]string{}, refs: map[string][]string{}, docs: map[string]string{}}
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
		f, err := parser.ParseFile(fset, name, nil, parser.ParseComments)
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
			if fn.Doc != nil {
				pkg.docs[fname] = fn.Doc.Text()
			}
			switch {
			case gofunc.IsTestFunc(fn, "Test", "T"):
				pkg.tests[fname] = name
				if steps, ok := scenarioSteps(fn); ok {
					pkg.scenarios[fname] = steps
				}
			case gofunc.IsTestFunc(fn, stepPrefix, "T"):
				pkg.steps[fname] = name
			}
		}
	}
	addHarnessRefs(t, fset, pkg.refs)
	return pkg
}

// addHarnessRefs adds to refs what every function of the harness the e2e
// package builds its worlds from (kit/stack, outside its own tests) names, so
// a test reaching a world builder through a harness helper (stack.WlDriveArea
// building a World, say) is followed to it. The package spells the harness's
// functions as selectors, whose selected name namesIn records.
func addHarnessRefs(t *testing.T, fset *token.FileSet, refs map[string][]string) {
	t.Helper()
	files, err := filepath.Glob(filepath.Join("kit", "stack", "*.go"))
	if err != nil {
		t.Fatalf("glob the harness: %v", err)
	}
	if len(files) == 0 {
		t.Fatalf("no harness source under kit/stack, so no helper could be followed to a world builder")
	}
	for _, path := range files {
		if strings.HasSuffix(path, "_test.go") {
			continue
		}
		f, err := parser.ParseFile(fset, path, nil, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", path, err)
		}
		for _, decl := range f.Decls {
			if fn, ok := decl.(*ast.FuncDecl); ok && fn.Body != nil {
				refs[fn.Name.Name] = append(refs[fn.Name.Name], namesIn(fn.Body)...)
			}
		}
	}
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

// THE SEAM A TEST PROVES, as its doc comment names it (AGENTS.md, "An e2e
// test proves a seam between real systems, and nothing else"): a line of its
// own, in one of two forms.
//
//   - `Seam: <family>`: the family whose halves the scenario goldens pin
//     (testdata/scenario-goldens/*/meta.json), and whose wiring test this is
//     (the scenario matrix, scenariomatrix_test.go).
//   - `Seam: <producer> -> <consumer>, <what crosses>`: any other seam.
//
// seamClaims is the one reader of those lines: the matrix takes the families
// from it, and the seam lint (TestEveryScenarioAndStepNamesItsSeam) that
// every scenario and step has a well-formed one.

// seamClaim is one `Seam:` line of a doc comment.
type seamClaim struct {
	// line is the text after `Seam: `.
	line string
	// family is the family the line names, "" for the crossing form.
	family string
}

var (
	// seamLine is any `Seam:` line, well formed or not.
	seamLine = regexp.MustCompile(`(?m)^Seam:(.*)$`)
	// seamFamily is the family form's text.
	seamFamily = regexp.MustCompile(`^[a-z0-9-]+$`)
	// seamCrossing is the crossing form's text.
	seamCrossing = regexp.MustCompile(`^\S.*? -> \S.*?, \S.*$`)
)

// seamClaims reads DOC's `Seam:` lines: the well-formed ones as claims, and
// the text of every line in neither form as malformed.
func seamClaims(doc string) (claims []seamClaim, malformed []string) {
	for _, m := range seamLine.FindAllStringSubmatch(doc, -1) {
		text, spaced := strings.CutPrefix(m[1], " ")
		switch {
		case !spaced:
			malformed = append(malformed, m[0])
		case seamFamily.MatchString(text):
			claims = append(claims, seamClaim{line: text, family: text})
		case seamCrossing.MatchString(text):
			claims = append(claims, seamClaim{line: text})
		default:
			malformed = append(malformed, m[0])
		}
	}
	return claims, malformed
}

// stepPrefix starts every step function's name.
const stepPrefix = scenario.StepPrefix
