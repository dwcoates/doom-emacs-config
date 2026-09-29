package e2e

// The self-verifying half of `e2e/SCENARIO-MATRIX.md`.
//
// WHY THIS EXISTS. The matrix is the inventory people plan e2e work from,
// and while it was hand-maintained it was caught lying in both directions on
// the same day: twelve `!api-*` rows read `uncovered` while one test file was
// already driving every one of them, twenty session/tool rows read
// `uncovered` with strong tests already standing behind nineteen of them, and
// its own summary counts disagreed with its own table. Two agents nearly
// wrote duplicate tests off it. A coverage inventory that can be wrong is
// worse than no inventory, so every MECHANICAL fact in the document is now
// derived here and asserted against what the document says.
//
// WHAT IS DERIVED, and from what:
//
//   - The canonical scenario list comes from the mocked vendor's own source
//     (`agent-shim/claude/shim/src/fake/scenarios/*.ts`), cross-checked
//     against the registry-GENERATED prompt table in the shim's AGENTS.md.
//     Two independent readings of one registry: a scenario added, renamed or
//     deleted there fails this test until the document follows.
//   - Which test file drives which scenario comes from the TESTS, by the
//     vendor's own selection rule (`fake/registry.ts`'s `selectScenario`)
//     applied to the string literals the tests actually submit, plus the
//     bare-name arguments of the three drive helpers that prepend the `!`.
//   - The summary counts come from the table's own columns.
//
// WHAT IS NOT DERIVED, on purpose. Whether an assertion is STRONG or WEAK is
// a reading of the test body, and no script can make that judgment: the
// `Grounded?`, `Strongest assertion` and covered-vs-weak columns stay
// hand-written, and this test checks only that a row claiming ANY coverage
// has a test behind it and a row claiming none has no test behind it. It
// never upgrades `weak` to `covered` or the reverse. See the document's own
// "What this check cannot decide" section.
//
// REGENERATING. `AGENT_REPL_MATRIX_WRITE=1 go test ./e2e -run
// TestScenarioMatrixMatchesReality` rewrites the derived columns, the derived
// rows and the counts in place, carrying every hand-written cell forward
// unchanged, then re-runs the check against what it wrote.
//
// COST. File reads and one `go/parser` pass over this package. It spawns
// nothing and drives no scenario.

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"sort"
	"strconv"
	"strings"
	"testing"
)

// matrixPath is the document this test keeps honest.
const matrixPath = "SCENARIO-MATRIX.md"

// matrixWriteEnv, when set to "1", makes the test rewrite the document's
// derived parts instead of only reporting on them.
const matrixWriteEnv = "AGENT_REPL_MATRIX_WRITE"

// The three counted e2e layers, in the column order the document uses.
const (
	layerGo     = "Go e2e"
	layerWebapp = "Webapp layer"
	layerEmacs  = "Emacs e2e"
)

var matrixLayers = []string{layerGo, layerWebapp, layerEmacs}

// emDash is what an empty layer cell holds, matching the document's existing
// typography.
const emDash = "—"

// ---------------------------------------------------------------------------
// (1) The canonical scenario list, derived from the mocked vendor.
// ---------------------------------------------------------------------------

// scenarioObjectForm matches `scenario({ name: "x"` and every
// `<something>Scenario({ name: "x"` factory that takes an options object.
var scenarioObjectForm = regexp.MustCompile(`[Ss]cenario\(\s*\{\s*name:\s*"([^"]*)"`)

// scenarioFactoryForm matches `= fastModeScenario("fast-on", ...)` — the
// factories that take the name positionally.
var scenarioFactoryForm = regexp.MustCompile(`=\s*[a-z][A-Za-z0-9]*Scenario\(\s*"([^"]+)"`)

// defaultScenarioName is the fall-through prose scenario's own name in the
// registry: the empty string.
const defaultScenarioName = ""

// markerScenario is a scenario NOT selected by a `!name` prompt but by a
// marker the prompt carries, as `fake/registry.ts` checks for it.
type markerScenario struct {
	// name is the scenario's own name in the registry.
	name string
	// marker is the prompt marker that selects it, verbatim from the shim.
	marker string
	// startsWith: the marker must open the (left-trimmed) prompt; otherwise it
	// may sit anywhere in it.
	startsWith bool
}

// markerScenarios are every marker-selected scenario, in the order
// `fake/registry.ts`'s `selectScenario` tries them after its `!name` tokens:
//   - `network-resume`: the shim's OWN network-resume prompt, which opens with
//     `NETWORK_RESUME_MARKER` (engine/network-resume-prompt.ts);
//   - `fail-marker`: the daemon's merge-pipeline gate puts
//     `e2e-fail-this-turn` anywhere in an otherwise ordinary prose prompt.
var markerScenarios = []markerScenario{
	{name: "network-resume", marker: "<!--agent-repl:network-resume-->", startsWith: true},
	{name: "fail-marker", marker: "e2e-fail-this-turn"},
}

// matches answers whether PROMPT carries this scenario's marker, by the
// vendor's own rule for it.
func (m markerScenario) matches(prompt string) bool {
	if m.startsWith {
		return strings.HasPrefix(strings.TrimLeft(prompt, " \t\n\r"), m.marker)
	}
	return strings.Contains(prompt, m.marker)
}

// markerScenarioIn is the marker scenario whose marker VALUE carries anywhere
// in it (a test literal, a rendered document cell), if any.
func markerScenarioIn(value string) (markerScenario, bool) {
	for _, m := range markerScenarios {
		if strings.Contains(value, m.marker) {
			return m, true
		}
	}
	return markerScenario{}, false
}

// markerScenarioNamed is the marker scenario called NAME, if any.
func markerScenarioNamed(name string) (markerScenario, bool) {
	for _, m := range markerScenarios {
		if m.name == name {
			return m, true
		}
	}
	return markerScenario{}, false
}

// scenarioNamesFromVendorSource reads every registered scenario's name out of
// the mocked vendor's scenario modules.
func scenarioNamesFromVendorSource(t *testing.T) map[string]bool {
	t.Helper()
	dir := filepath.Join(repo.shimDir, "src", "fake", "scenarios")
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("read the mocked vendor's scenario modules at %s: %v", dir, err)
	}
	names := map[string]bool{}
	for _, entry := range entries {
		if entry.IsDir() || filepath.Ext(entry.Name()) != ".ts" {
			continue
		}
		src, err := os.ReadFile(filepath.Join(dir, entry.Name()))
		if err != nil {
			t.Fatalf("read %s: %v", entry.Name(), err)
		}
		for _, m := range scenarioObjectForm.FindAllStringSubmatch(string(src), -1) {
			names[m[1]] = true
		}
		for _, m := range scenarioFactoryForm.FindAllStringSubmatch(string(src), -1) {
			names[m[1]] = true
		}
	}
	if len(names) == 0 {
		t.Fatalf("no scenarios found under %s — the extraction patterns have gone stale", dir)
	}
	return names
}

// vendorTableHeading is the heading the registry-generated prompt table lives
// under in the shim's AGENTS.md. `scripts/scenario-table.ts` renders that
// table from `SCENARIOS` and `test/fake/registry.test.ts` asserts the
// committed copy matches the renderer in both directions, so the table is a
// second, independently-maintained projection of the same registry.
const vendorTableHeading = "## Mocked vendor: prompt → scenario table"

// scenarioNamesFromVendorTable reads the named tokens out of that table.
func scenarioNamesFromVendorTable(t *testing.T) map[string]bool {
	t.Helper()
	path := filepath.Join(repo.shimDir, "AGENTS.md")
	src, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	lines := strings.Split(string(src), "\n")
	names := map[string]bool{}
	inTable := false
	for _, line := range lines {
		if strings.HasPrefix(line, vendorTableHeading) {
			inTable = true
			continue
		}
		if !inTable {
			continue
		}
		if strings.HasPrefix(line, "## ") {
			break
		}
		if !strings.HasPrefix(line, "| `") {
			continue
		}
		prompt := line[3:]
		end := strings.Index(prompt, "`")
		if end < 0 {
			continue
		}
		prompt = prompt[:end]
		if !strings.HasPrefix(prompt, "!") {
			// The default row, whose prompt cell is prose.
			continue
		}
		names[strings.Fields(prompt)[0][1:]] = true
	}
	if len(names) == 0 {
		t.Fatalf("no rows found under %q in %s — the table's shape has changed", vendorTableHeading, path)
	}
	return names
}

// aliasBlock isolates registry.ts's ALIASES record.
var aliasBlock = regexp.MustCompile(`(?s)export const ALIASES[^{]*\{(.*?)\n\};`)

// aliasEntry matches one `"golden-name": "scenario-name",` line in it.
var aliasEntry = regexp.MustCompile(`"([^"]*)":\s*"([^"]*)"`)

// aliasesFromVendorSource reads the golden-name aliases, each of which is a
// second matchable token for an already-registered scenario.
func aliasesFromVendorSource(t *testing.T) map[string]string {
	t.Helper()
	path := filepath.Join(repo.shimDir, "src", "fake", "registry.ts")
	src, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	block := aliasBlock.FindStringSubmatch(string(src))
	if block == nil {
		t.Fatalf("no ALIASES record found in %s — the registry's shape has changed", path)
	}
	out := map[string]string{}
	for _, m := range aliasEntry.FindAllStringSubmatch(block[1], -1) {
		out[m[1]] = m[2]
	}
	if len(out) == 0 {
		t.Fatalf("the ALIASES record in %s parsed to nothing", path)
	}
	return out
}

// registry is this test's model of the mocked vendor's selection table.
type registry struct {
	// canonical is every scenario the document must carry a row for, in the
	// document's own order (sorted by token).
	canonical []string
	// tokens is every matchable `!name` token, longest first, mapped to the
	// scenario it selects — `selectScenario`'s own NAMED list.
	tokens []registryToken
}

type registryToken struct {
	token    string
	scenario string
}

// deriveRegistry builds that model and cross-checks the two derivations.
func deriveRegistry(t *testing.T) registry {
	t.Helper()
	fromSource := scenarioNamesFromVendorSource(t)
	fromTable := scenarioNamesFromVendorTable(t)

	// Every table row must have a scenario behind it in the source.
	var phantom []string
	for name := range fromTable {
		if !fromSource[name] {
			phantom = append(phantom, name)
		}
	}
	sort.Strings(phantom)
	if len(phantom) > 0 {
		t.Fatalf("the shim's generated prompt table names scenarios its own source does not declare: %v", phantom)
	}
	// And every source scenario must appear in the table, except those that
	// are not selected by a `!name` prompt at all: the prose fall-through and
	// the marker scenarios.
	want := map[string]bool{defaultScenarioName: true}
	for _, m := range markerScenarios {
		want[m.name] = true
	}
	var got, wantNames []string
	for name := range fromSource {
		if !fromTable[name] {
			got = append(got, strconv.Quote(name))
		}
	}
	for name := range want {
		wantNames = append(wantNames, strconv.Quote(name))
	}
	sort.Strings(got)
	sort.Strings(wantNames)
	if !slices.Equal(got, wantNames) {
		t.Fatalf("scenarios declared in the mocked vendor's source but absent from its generated prompt table = %v; "+
			"want exactly those no `!name` prompt selects, %v (the prose fall-through %q and every marker scenario). "+
			"Another name here means either a new unlisted scenario or a table that was not regenerated",
			got, wantNames, defaultScenarioName)
	}

	r := registry{}
	for name := range fromTable {
		r.canonical = append(r.canonical, name)
	}
	// Each marker scenario earns a row of its own: it is selected, just not
	// through a `!name`. The prose fall-through does NOT — it is selected by
	// every unrecognized prompt in the suite, so "which tests drive it" is not
	// a fact any derivation can state usefully.
	for _, m := range markerScenarios {
		r.canonical = append(r.canonical, m.name)
	}
	sort.Strings(r.canonical)

	for name := range fromTable {
		r.tokens = append(r.tokens, registryToken{token: name, scenario: name})
	}
	for alias, target := range aliasesFromVendorSource(t) {
		if !fromSource[target] {
			t.Fatalf("the registry's ALIASES points %q at unknown scenario %q", alias, target)
		}
		r.tokens = append(r.tokens, registryToken{token: alias, scenario: target})
	}
	sort.Slice(r.tokens, func(i, j int) bool {
		if len(r.tokens[i].token) != len(r.tokens[j].token) {
			return len(r.tokens[i].token) > len(r.tokens[j].token)
		}
		return r.tokens[i].token < r.tokens[j].token
	})
	return r
}

// selectScenario is `fake/registry.ts`'s own rule, reimplemented: the longest
// registered token the prompt starts with, bounded by whitespace or the end
// of the string. The second answer distinguishes a prompt that MATCHED a
// registered token from one that matched nothing — they are not the same
// thing even when both end at the prose fall-through, because `!prose-streamed`
// is a registered ALIAS of the default scenario while `!hello` is a token the
// vendor has never heard of.
func (r registry) selectScenario(prompt string) (string, bool) {
	trimmed := strings.TrimLeft(prompt, " \t\n\r")
	for _, cand := range r.tokens {
		tok := "!" + cand.token
		if !strings.HasPrefix(trimmed, tok) {
			continue
		}
		rest := trimmed[len(tok):]
		if rest == "" || strings.ContainsAny(rest[:1], " \t\n\r") {
			return cand.scenario, true
		}
	}
	for _, m := range markerScenarios {
		if m.matches(prompt) {
			return m.name, true
		}
	}
	return defaultScenarioName, false
}

// triggerShaped answers whether a `!`-prefixed string literal is even a
// candidate scenario trigger. A registered name is lower-case letters, digits
// and hyphens, so a literal like `"!%s: FeedTurnEndedErrored.Error = %T"` — a
// t.Fatalf format that happens to open with the `!` a message prints before a
// scenario name — is not one, and must not be read as a dead trigger.
var triggerShaped = regexp.MustCompile(`^![a-z][a-z0-9-]*(\s|$)`)

// ---------------------------------------------------------------------------
// (2) Which test file drives which scenario, derived from the tests.
// ---------------------------------------------------------------------------

// coverage maps scenario name -> layer -> set of file base names.
type coverage map[string]map[string]map[string]bool

func (c coverage) add(scenario, layer, file string) {
	if scenario == defaultScenarioName {
		return
	}
	byLayer, ok := c[scenario]
	if !ok {
		byLayer = map[string]map[string]bool{}
		c[scenario] = byLayer
	}
	files, ok := byLayer[layer]
	if !ok {
		files = map[string]bool{}
		byLayer[layer] = files
	}
	files[file] = true
}

func (c coverage) files(scenario, layer string) []string {
	var out []string
	for f := range c[scenario][layer] {
		out = append(out, f)
	}
	sort.Strings(out)
	return out
}

// deriveCoverage reads the three counted layers.
func deriveCoverage(t *testing.T, r registry) coverage {
	t.Helper()
	c := coverage{}
	deriveGoCoverage(t, r, c)
	deriveWebappCoverage(t, r, c)
	return c
}

// goLayerOf answers which counted layer a Go file in this package belongs to,
// or "" for a file that is not a counted e2e area file (the harness's own
// files, the perf budget files, this file).
func goLayerOf(name string) string {
	if !strings.HasSuffix(name, "_test.go") {
		return ""
	}
	if strings.HasPrefix(name, "emacs_") {
		return layerEmacs
	}
	if strings.HasSuffix(name, "_e2e_test.go") {
		return layerGo
	}
	return ""
}

// goBareDriveHelpers are the helpers whose named argument is a BARE scenario
// name that the helper itself prefixes with `!`. Every other prompt this
// suite submits is a literal string, which the `!`-literal sweep below sees
// directly, so these three are the whole of what needs dataflow.
var goBareDriveHelpers = map[string]int{"driveScenarioToCompletion": 4}

// deriveGoCoverage walks this package's AST.
func deriveGoCoverage(t *testing.T, r registry, c coverage) {
	t.Helper()
	fset := token.NewFileSet()
	pkgs, err := parser.ParseDir(fset, repo.e2eDir, func(fi os.FileInfo) bool {
		return strings.HasSuffix(fi.Name(), ".go")
	}, 0)
	if err != nil {
		t.Fatalf("parse the e2e package: %v", err)
	}
	pkg, ok := pkgs["e2e"]
	if !ok {
		t.Fatalf("no package e2e parsed out of %s", repo.e2eDir)
	}

	// (a) Every `!`-prefixed string literal, in every counted file, read by
	// the vendor's own selection rule. This is the whole of the Emacs layer's
	// mechanism and most of the Go layer's.
	var deadTriggers []string
	for path, file := range pkg.Files {
		base := filepath.Base(path)
		layer := goLayerOf(base)
		if layer == "" {
			continue
		}
		ast.Inspect(file, func(n ast.Node) bool {
			lit, ok := n.(*ast.BasicLit)
			if !ok || lit.Kind != token.STRING {
				return true
			}
			value, err := strconv.Unquote(lit.Value)
			if err != nil {
				return true
			}
			if m, ok := markerScenarioIn(value); ok {
				c.add(m.name, layer, base)
			}
			if !triggerShaped.MatchString(value) {
				return true
			}
			scenario, matched := r.selectScenario(value)
			if !matched {
				deadTriggers = append(deadTriggers, fmt.Sprintf("%s:%d: %q selects no registered scenario",
					base, fset.Position(lit.Pos()).Line, value))
				return true
			}
			c.add(scenario, layer, base)
			return true
		})
	}
	if len(deadTriggers) > 0 {
		t.Errorf("dead scenario triggers — a `!` prompt the mocked vendor does not register, so the test silently drives plain prose instead:\n  %s",
			strings.Join(deadTriggers, "\n  "))
	}

	// (b) The bare-name drive helpers, resolved through one level of
	// parameter and table-field indirection.
	sinks := goResolveSinks(pkg)
	for path, file := range pkg.Files {
		base := filepath.Base(path)
		layer := goLayerOf(base)
		if layer == "" {
			continue
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				call, ok := n.(*ast.CallExpr)
				if !ok {
					return true
				}
				name, arg, ok := goSinkArg(call, sinks)
				if !ok {
					return true
				}
				// A wrapper handing its OWN parameter through is the
				// propagation case: it is resolved at the wrapper's callers,
				// where the concrete name is, not here.
				if ident, isIdent := arg.(*ast.Ident); isIdent {
					if _, isParam := goParamNames(fn)[ident.Name]; isParam {
						return true
					}
				}
				names, resolved := goResolveStrings(arg, fn)
				if !resolved {
					t.Errorf("%s:%d: the scenario argument to %s is an expression this check cannot read (%T). "+
						"Give it a string literal, a local table field, or a range over a string slice, or teach "+
						"goResolveStrings the new shape — an unreadable argument would silently under-report coverage",
						base, fset.Position(call.Pos()).Line, name, arg)
					return true
				}
				for _, bare := range names {
					scenario, matched := r.selectScenario("!" + bare)
					if !matched {
						t.Errorf("%s:%d: %s drives %q, which the mocked vendor does not register",
							base, fset.Position(call.Pos()).Line, name, bare)
						continue
					}
					c.add(scenario, layer, base)
				}
				return true
			})
		}
	}
}

// goResolveSinks answers every (function name -> argument index) pair whose
// argument reaches a bare-name drive helper, seeded with the helpers
// themselves and closed over local wrappers that pass a parameter straight
// through.
func goResolveSinks(pkg *ast.Package) map[string]int {
	sinks := map[string]int{}
	for k, v := range goBareDriveHelpers {
		sinks[k] = v
	}
	for changed := true; changed; {
		changed = false
		for _, file := range pkg.Files {
			for _, decl := range file.Decls {
				fn, ok := decl.(*ast.FuncDecl)
				if !ok || fn.Body == nil || fn.Recv != nil {
					continue
				}
				if _, known := sinks[fn.Name.Name]; known {
					continue
				}
				params := goParamNames(fn)
				ast.Inspect(fn.Body, func(n ast.Node) bool {
					call, ok := n.(*ast.CallExpr)
					if !ok {
						return true
					}
					_, arg, ok := goSinkArg(call, sinks)
					if !ok {
						return true
					}
					ident, ok := arg.(*ast.Ident)
					if !ok {
						return true
					}
					if idx, isParam := params[ident.Name]; isParam {
						sinks[fn.Name.Name] = idx
						changed = true
					}
					return true
				})
			}
		}
	}
	return sinks
}

// goParamNames flattens a function's parameter list to name -> position.
func goParamNames(fn *ast.FuncDecl) map[string]int {
	out := map[string]int{}
	idx := 0
	for _, field := range fn.Type.Params.List {
		if len(field.Names) == 0 {
			idx++
			continue
		}
		for _, name := range field.Names {
			out[name.Name] = idx
			idx++
		}
	}
	return out
}

// goSinkArg answers the scenario-bearing argument of a call to a known sink.
func goSinkArg(call *ast.CallExpr, sinks map[string]int) (string, ast.Expr, bool) {
	ident, ok := call.Fun.(*ast.Ident)
	if !ok {
		return "", nil, false
	}
	idx, ok := sinks[ident.Name]
	if !ok || idx >= len(call.Args) {
		return "", nil, false
	}
	return ident.Name, call.Args[idx], true
}

// goResolveStrings answers the string values an argument can take, or false
// when the shape is one this check does not read. Deliberately narrow: it
// reads a literal, a same-function table field (`tc.scenario`, resolved to
// every literal that key holds in the function's composite literals), a
// same-function range variable over a string slice, and a same-function or
// package-level constant.
func goResolveStrings(arg ast.Expr, fn *ast.FuncDecl) ([]string, bool) {
	switch e := arg.(type) {
	case *ast.BasicLit:
		if e.Kind != token.STRING {
			return nil, false
		}
		v, err := strconv.Unquote(e.Value)
		if err != nil {
			return nil, false
		}
		return []string{v}, true
	case *ast.SelectorExpr:
		got := goFieldLiterals(fn, e.Sel.Name)
		return got, len(got) > 0
	case *ast.Ident:
		if got := goRangeLiterals(fn, e.Name); len(got) > 0 {
			return got, true
		}
		if got := goLocalLiterals(fn, e.Name); len(got) > 0 {
			return got, true
		}
		return nil, false
	}
	return nil, false
}

// goFieldLiterals collects every string literal a named key holds in any
// composite literal inside fn — the table of a table-driven test.
func goFieldLiterals(fn *ast.FuncDecl, key string) []string {
	var out []string
	ast.Inspect(fn.Body, func(n ast.Node) bool {
		kv, ok := n.(*ast.KeyValueExpr)
		if !ok {
			return true
		}
		ident, ok := kv.Key.(*ast.Ident)
		if !ok || ident.Name != key {
			return true
		}
		if v, ok := goStringOf(kv.Value); ok {
			out = append(out, v)
		}
		return true
	})
	return out
}

// goRangeLiterals collects the literals a `for _, name := range …` iterates.
func goRangeLiterals(fn *ast.FuncDecl, name string) []string {
	var out []string
	ast.Inspect(fn.Body, func(n ast.Node) bool {
		rng, ok := n.(*ast.RangeStmt)
		if !ok || rng.Value == nil {
			return true
		}
		ident, ok := rng.Value.(*ast.Ident)
		if !ok || ident.Name != name {
			return true
		}
		out = append(out, goSliceLiterals(fn, rng.X)...)
		return true
	})
	return out
}

// goSliceLiterals reads the string literals of a slice expression, following
// one level of local variable binding.
func goSliceLiterals(fn *ast.FuncDecl, expr ast.Expr) []string {
	switch e := expr.(type) {
	case *ast.CompositeLit:
		var out []string
		for _, elt := range e.Elts {
			if v, ok := goStringOf(elt); ok {
				out = append(out, v)
			}
		}
		return out
	case *ast.Ident:
		var out []string
		ast.Inspect(fn.Body, func(n ast.Node) bool {
			assign, ok := n.(*ast.AssignStmt)
			if !ok {
				return true
			}
			for i, lhs := range assign.Lhs {
				lid, ok := lhs.(*ast.Ident)
				if !ok || lid.Name != e.Name || i >= len(assign.Rhs) {
					continue
				}
				if cl, ok := assign.Rhs[i].(*ast.CompositeLit); ok {
					for _, elt := range cl.Elts {
						if v, ok := goStringOf(elt); ok {
							out = append(out, v)
						}
					}
				}
			}
			return true
		})
		return out
	}
	return nil
}

// goLocalLiterals reads a local `name := "literal"` or `const name = "…"`.
func goLocalLiterals(fn *ast.FuncDecl, name string) []string {
	var out []string
	ast.Inspect(fn.Body, func(n ast.Node) bool {
		switch s := n.(type) {
		case *ast.AssignStmt:
			for i, lhs := range s.Lhs {
				lid, ok := lhs.(*ast.Ident)
				if !ok || lid.Name != name || i >= len(s.Rhs) {
					continue
				}
				if v, ok := goStringOf(s.Rhs[i]); ok {
					out = append(out, v)
				}
			}
		case *ast.ValueSpec:
			for i, id := range s.Names {
				if id.Name != name || i >= len(s.Values) {
					continue
				}
				if v, ok := goStringOf(s.Values[i]); ok {
					out = append(out, v)
				}
			}
		}
		return true
	})
	return out
}

// goStringOf unquotes an expression that is exactly a string literal.
func goStringOf(expr ast.Expr) (string, bool) {
	lit, ok := expr.(*ast.BasicLit)
	if !ok || lit.Kind != token.STRING {
		return "", false
	}
	v, err := strconv.Unquote(lit.Value)
	if err != nil {
		return "", false
	}
	return v, true
}

// ---------------------------------------------------------------------------
// The webapp layer's own mechanism.
// ---------------------------------------------------------------------------

// tsBareDriveHelpers are the webapp layer's equivalents of
// driveScenarioToCompletion: they take a bare scenario name and prefix the
// `!` themselves (`webapp/test/webapp-layer/drive.ts`). Every other prompt
// the layer submits is a literal, which the `!`-literal sweep reads directly.
var tsBareDriveHelpers = []string{"driveTurn", "driveScenario", "driveScenarioRow"}

// tsLiteral is one scanned string literal: where it sits, what it holds, and
// whether it is plain (no `${}` interpolation, so its value is knowable).
type tsLiteral struct {
	start int // index of the opening quote
	end   int // index one past the closing quote
	value string
	plain bool
}

// tsSource is a scanned TypeScript file: a MASK in which every comment and
// every string literal's contents are blanked to spaces (so offsets are
// preserved and a stray apostrophe in prose cannot swallow the code after
// it), beside the literals themselves.
//
// A REAL SCAN RATHER THAN A REGEX, and this is a measurement: a regex for
// `'…'` pairs the apostrophe in a comment's "daemon's" with the next one and
// eats every literal between them. That is exactly how a first cut of this
// check reported `!hold` as driven from one webapp file when three drive it.
type tsSource struct {
	raw      string
	mask     string
	literals []tsLiteral
}

func scanTS(src string) tsSource {
	mask := []byte(src)
	blank := func(from, to int) {
		for i := from; i < to && i < len(mask); i++ {
			if mask[i] != '\n' {
				mask[i] = ' '
			}
		}
	}
	out := tsSource{}
	for i := 0; i < len(src); {
		switch {
		case strings.HasPrefix(src[i:], "//"):
			j := strings.IndexByte(src[i:], '\n')
			if j < 0 {
				j = len(src) - i
			}
			blank(i, i+j)
			i += j
		case strings.HasPrefix(src[i:], "/*"):
			j := strings.Index(src[i+2:], "*/")
			if j < 0 {
				blank(i, len(src))
				i = len(src)
				break
			}
			blank(i, i+2+j+2)
			i += 2 + j + 2
		case src[i] == '"' || src[i] == '\'':
			quote := src[i]
			j := i + 1
			var value strings.Builder
			for j < len(src) && src[j] != quote {
				if src[j] == '\\' && j+1 < len(src) {
					value.WriteByte(src[j+1])
					j += 2
					continue
				}
				if src[j] == '\n' {
					break
				}
				value.WriteByte(src[j])
				j++
			}
			if j < len(src) && src[j] == quote {
				out.literals = append(out.literals, tsLiteral{start: i, end: j + 1, value: value.String(), plain: true})
				blank(i+1, j)
				i = j + 1
				break
			}
			i++
		case src[i] == '`':
			j := i + 1
			depth := 0
			interpolated := false
			var value strings.Builder
			for j < len(src) {
				if src[j] == '\\' && j+1 < len(src) {
					value.WriteByte(src[j+1])
					j += 2
					continue
				}
				if depth == 0 && strings.HasPrefix(src[j:], "${") {
					interpolated = true
					depth++
					j += 2
					continue
				}
				if depth > 0 {
					switch src[j] {
					case '{':
						depth++
					case '}':
						depth--
					}
					j++
					continue
				}
				if src[j] == '`' {
					break
				}
				value.WriteByte(src[j])
				j++
			}
			if j < len(src) && src[j] == '`' {
				out.literals = append(out.literals, tsLiteral{start: i, end: j + 1, value: value.String(), plain: !interpolated})
				blank(i+1, j)
				i = j + 1
				break
			}
			i++
		default:
			i++
		}
	}
	out.raw = src
	out.mask = string(mask)
	return out
}

// literalAt answers the plain literal whose text is exactly the given span.
func (ts tsSource) literalAt(from, to int) (string, bool) {
	for _, lit := range ts.literals {
		if lit.start == from && lit.end == to {
			return lit.value, lit.plain
		}
	}
	return "", false
}

// tsCallSite matches an identifier followed by an open parenthesis in the
// MASKED source, so nothing inside a string or a comment can look like one.
var tsCallSite = regexp.MustCompile(`\b([A-Za-z_$][\w$]*)\s*\(`)

// tsPrefixedSubmit matches a wrapper submitting its own parameter as the
// vendor's selector: submit(app, `!${scenario}`).
var tsPrefixedSubmit = regexp.MustCompile("submit\\(\\s*[A-Za-z_$][\\w$]*\\s*,\\s*`!\\$\\{([A-Za-z_$][\\w$]*)\\}`")

// tsWrapperDecl matches a local `async function name(a, b, …)` declaration.
var tsWrapperDecl = regexp.MustCompile(`(?m)^\s*(?:export\s+)?(?:async\s+)?function\s+([A-Za-z_$][\w$]*)\s*\(([^)]*)\)`)

// deriveWebappCoverage reads the webapp layer's `.layer.test.ts` files.
func deriveWebappCoverage(t *testing.T, r registry, c coverage) {
	t.Helper()
	dir := filepath.Join(repo.repoDir, "webapp", "test", "webapp-layer")
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("read the webapp layer at %s: %v", dir, err)
	}
	seen := 0
	for _, entry := range entries {
		name := entry.Name()
		if entry.IsDir() || !strings.HasSuffix(name, ".layer.test.ts") {
			continue
		}
		seen++
		src, err := os.ReadFile(filepath.Join(dir, name))
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		ts := scanTS(string(src))

		// (a) Every `!`-prefixed literal, by the vendor's own rule.
		for _, lit := range ts.literals {
			if !lit.plain {
				continue
			}
			if m, ok := markerScenarioIn(lit.value); ok {
				c.add(m.name, layerWebapp, name)
			}
			if !triggerShaped.MatchString(lit.value) {
				continue
			}
			scenario, matched := r.selectScenario(lit.value)
			if !matched {
				t.Errorf("%s: %q selects no registered scenario — a dead trigger that silently drives plain prose", name, lit.value)
				continue
			}
			c.add(scenario, layerWebapp, name)
		}

		// (b) Bare names handed to the drive helpers.
		for _, bare := range tsBareNames(t, name, ts) {
			scenario, matched := r.selectScenario("!" + bare)
			if !matched {
				t.Errorf("%s: a drive helper is given %q, which the mocked vendor does not register", name, bare)
				continue
			}
			c.add(scenario, layerWebapp, name)
		}
	}
	if seen == 0 {
		t.Fatalf("no `.layer.test.ts` files found under %s — the webapp layer has moved", dir)
	}
}

// tsBareNames answers the bare scenario names one layer file hands to a drive
// helper, following one level of local wrapper indirection.
func tsBareNames(t *testing.T, file string, ts tsSource) []string {
	t.Helper()
	sinks := map[string]int{}
	for _, h := range tsBareDriveHelpers {
		sinks[h] = 1
	}
	// Discover local wrappers. Two shapes, both one level deep, which is all
	// the layer uses:
	//
	//   async function family(scenario, …)  { return driveTurn(app, scenario, …); }
	//   async function ask(scenario, …)     { await submit(app, `!${scenario}`); … }
	//
	// The second is the one that made `ask("perm-hold", …)`'s four scenarios
	// invisible to a first cut of this check.
	decls := tsWrapperDecl.FindAllStringSubmatchIndex(ts.mask, -1)
	for _, decl := range decls {
		fnName := ts.mask[decl[2]:decl[3]]
		params := tsParamNames(ts.mask[decl[4]:decl[5]])
		body := ts.mask[decl[1]:]
		rawBody := ts.raw[decl[1]:]
		if end := strings.Index(body, "\n}"); end >= 0 {
			body = body[:end]
			rawBody = rawBody[:end]
		}
		for _, call := range tsCallSite.FindAllStringSubmatchIndex(body, -1) {
			callee := body[call[2]:call[3]]
			idx, isSink := sinks[callee]
			if !isSink {
				continue
			}
			args := tsCallArgs(body[call[1]:])
			if idx >= len(args) {
				continue
			}
			arg := strings.TrimSpace(body[call[1]+args[idx].from : call[1]+args[idx].to])
			for i, p := range params {
				if p == arg {
					sinks[fnName] = i
				}
			}
		}
		for _, m := range tsPrefixedSubmit.FindAllStringSubmatch(rawBody, -1) {
			for i, p := range params {
				if p == m[1] {
					sinks[fnName] = i
				}
			}
		}
	}
	declared := map[int]bool{}
	for _, decl := range decls {
		declared[decl[2]] = true
	}

	var out []string
	for _, call := range tsCallSite.FindAllStringSubmatchIndex(ts.mask, -1) {
		callee := ts.mask[call[2]:call[3]]
		idx, isSink := sinks[callee]
		if !isSink || declared[call[2]] {
			continue
		}
		args := tsCallArgs(ts.mask[call[1]:])
		if idx >= len(args) {
			continue
		}
		from, to := call[1]+args[idx].from, call[1]+args[idx].to
		for from < to && (ts.mask[from] == ' ' || ts.mask[from] == '\n' || ts.mask[from] == '\t') {
			from++
		}
		for to > from && (ts.mask[to-1] == ' ' || ts.mask[to-1] == '\n' || ts.mask[to-1] == '\t') {
			to--
		}
		value, plain := ts.literalAt(from, to)
		if !plain {
			// A wrapper's own parameter pass-through was already resolved
			// above; anything else is a shape this check cannot read, and
			// silently reading nothing would under-report coverage.
			arg := strings.TrimSpace(ts.mask[from:to])
			if tsIsIdentifier(arg) {
				continue
			}
			t.Errorf("%s: the scenario argument to %s is %q, an expression this check cannot read — "+
				"give it a string literal or teach tsBareNames the new shape", file, callee, arg)
			continue
		}
		if value != "" {
			out = append(out, value)
		}
	}
	return out
}

var tsIdentifier = regexp.MustCompile(`^[A-Za-z_$][\w$]*$`)

func tsIsIdentifier(s string) bool { return tsIdentifier.MatchString(s) }

// tsParamNames splits a parameter list into bare names.
func tsParamNames(list string) []string {
	var out []string
	for _, part := range strings.Split(list, ",") {
		part = strings.TrimSpace(part)
		if i := strings.IndexAny(part, ":="); i >= 0 {
			part = strings.TrimSpace(part[:i])
		}
		out = append(out, part)
	}
	return out
}

// tsArgSpan is one argument's half-open span, relative to the text just after
// the call's opening parenthesis.
type tsArgSpan struct{ from, to int }

// tsCallArgs splits a call's argument list at top-level commas.
func tsCallArgs(text string) []tsArgSpan {
	depth := 0
	var args []tsArgSpan
	from := 0
	for i := 0; i < len(text); i++ {
		switch text[i] {
		case '(', '[', '{':
			depth++
		case ')', ']', '}':
			if text[i] == ')' && depth == 0 {
				return append(args, tsArgSpan{from: from, to: i})
			}
			depth--
		case ',':
			if depth == 0 {
				args = append(args, tsArgSpan{from: from, to: i})
				from = i + 1
			}
		}
	}
	return args
}

// ---------------------------------------------------------------------------
// (3) The document, and the check.
// ---------------------------------------------------------------------------

// matrixRow is one parsed row of table (a).
type matrixRow struct {
	scenario  string
	grounded  string
	files     map[string][]string
	assertion string
	verdict   string
}

// matrixUnreadAssertion is what a newly-derived row carries until someone
// reads the test behind it.
const matrixUnreadAssertion = "TODO — newly derived as driven; a human must read the test and state its strongest assertion."

const (
	verdictCovered   = "covered"
	verdictWeak      = "weak"
	verdictUncovered = "uncovered"
)

// matrixDoc is the parsed document: its lines, where table (a) sits, and the
// rows in it.
type matrixDoc struct {
	lines     []string
	tableFrom int // index of the first row line
	tableTo   int // index one past the last row line
	rows      []matrixRow
}

const matrixTableHeading = "## (a) Full matrix"

func parseMatrix(t *testing.T) matrixDoc {
	t.Helper()
	path := filepath.Join(repo.e2eDir, matrixPath)
	src, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	doc := matrixDoc{lines: strings.Split(string(src), "\n"), tableFrom: -1}
	inTable := false
	for i, line := range doc.lines {
		if strings.HasPrefix(line, matrixTableHeading) {
			inTable = true
			continue
		}
		if !inTable {
			continue
		}
		if strings.HasPrefix(line, "## ") {
			break
		}
		if !strings.HasPrefix(line, "| `") {
			continue
		}
		if doc.tableFrom < 0 {
			doc.tableFrom = i
		}
		doc.tableTo = i + 1
		cells := matrixCells(line)
		if len(cells) != 7 {
			t.Fatalf("%s:%d: row has %d cells, want 7: %s", matrixPath, i+1, len(cells), line)
		}
		row := matrixRow{
			scenario:  parseScenarioCell(cells[0]),
			grounded:  cells[1],
			files:     map[string][]string{},
			assertion: cells[5],
			verdict:   cells[6],
		}
		for j, layer := range matrixLayers {
			row.files[layer] = matrixFileCell(cells[2+j])
		}
		doc.rows = append(doc.rows, row)
	}
	if doc.tableFrom < 0 {
		t.Fatalf("no rows found under %q in %s", matrixTableHeading, matrixPath)
	}
	return doc
}

// parseScenarioCell reads a scenario name back out of its rendered cell,
// undoing scenarioCell.
func parseScenarioCell(cell string) string {
	if m, ok := markerScenarioIn(cell); ok {
		return m.name
	}
	return strings.TrimPrefix(strings.Trim(cell, "`"), "!")
}

// matrixCells splits one markdown table row into its cells.
func matrixCells(line string) []string {
	line = strings.TrimSpace(line)
	line = strings.TrimPrefix(line, "|")
	line = strings.TrimSuffix(line, "|")
	parts := strings.Split(line, "|")
	for i := range parts {
		parts[i] = strings.TrimSpace(parts[i])
	}
	return parts
}

// matrixFileCell reads a layer cell's comma-separated file list.
func matrixFileCell(cell string) []string {
	if cell == emDash || cell == "" {
		return nil
	}
	var out []string
	for _, part := range strings.Split(cell, ",") {
		part = strings.TrimSpace(part)
		if part != "" && part != emDash {
			out = append(out, part)
		}
	}
	sort.Strings(out)
	return out
}

func renderFileCell(files []string) string {
	if len(files) == 0 {
		return emDash
	}
	return strings.Join(files, ", ")
}

// scenarioCell renders a scenario's own name cell.
func scenarioCell(scenario string) string {
	if m, ok := markerScenarioNamed(scenario); ok {
		return "`" + m.marker + "` (marker)"
	}
	return "`!" + scenario + "`"
}

func renderRow(row matrixRow) string {
	cells := []string{
		scenarioCell(row.scenario),
		row.grounded,
		renderFileCell(row.files[layerGo]),
		renderFileCell(row.files[layerWebapp]),
		renderFileCell(row.files[layerEmacs]),
		row.assertion,
		row.verdict,
	}
	return "| " + strings.Join(cells, " | ") + " |"
}

// TestScenarioMatrixMatchesReality is the whole check.
func TestScenarioMatrixMatchesReality(t *testing.T) {
	t.Parallel()
	r := deriveRegistry(t)
	derived := deriveCoverage(t, r)
	doc := parseMatrix(t)

	if os.Getenv(matrixWriteEnv) == "1" {
		rewriteMatrix(t, r, derived, doc)
		doc = parseMatrix(t)
	}

	byScenario := map[string]matrixRow{}
	for _, row := range doc.rows {
		key := row.scenario
		if _, dup := byScenario[key]; dup {
			t.Errorf("%s: two rows for scenario %q", matrixPath, key)
		}
		byScenario[key] = row
	}

	canonical := map[string]bool{}
	for _, s := range r.canonical {
		canonical[s] = true
	}

	// (i) A scenario the registry declares but the document omits.
	for _, scenario := range r.canonical {
		if _, ok := byScenario[scenario]; !ok {
			t.Errorf("%s: the mocked vendor registers %s but the matrix has no row for it — "+
				"regenerate with %s=1", matrixPath, scenarioCell(scenario), matrixWriteEnv)
		}
	}
	// (ii) A row for a scenario the registry no longer declares.
	for scenario := range byScenario {
		if !canonical[scenario] {
			t.Errorf("%s: the matrix has a row for %q, which the mocked vendor does not register — "+
				"it was renamed or deleted; regenerate with %s=1", matrixPath, scenario, matrixWriteEnv)
		}
	}

	// (iii) Every layer cell must be exactly the files the tests themselves
	// name — a row claiming coverage no test provides, and a row omitting a
	// test that does drive it, are the same defect in two directions.
	for _, scenario := range r.canonical {
		row, ok := byScenario[scenario]
		if !ok {
			continue
		}
		for _, layer := range matrixLayers {
			want := derived.files(scenario, layer)
			got := row.files[layer]
			if strings.Join(want, ", ") == strings.Join(got, ", ") {
				continue
			}
			t.Errorf("%s: %s / %s column says %q, but the tests themselves drive it from %q",
				matrixPath, scenarioCell(scenario), layer, renderFileCell(got), renderFileCell(want))
		}
		// (iv) The verdict's MECHANICAL half. Strong-vs-weak is a reading of
		// the test body and stays hand-written; whether ANY counted layer
		// drives the scenario is not.
		driven := len(derived.files(scenario, layerGo))+len(derived.files(scenario, layerWebapp))+
			len(derived.files(scenario, layerEmacs)) > 0
		switch row.verdict {
		case verdictUncovered:
			if driven {
				t.Errorf("%s: %s reads %q, but a counted layer drives it", matrixPath, scenarioCell(scenario), verdictUncovered)
			}
		case verdictCovered, verdictWeak:
			if !driven {
				t.Errorf("%s: %s reads %q, but no counted layer drives it", matrixPath, scenarioCell(scenario), row.verdict)
			}
		default:
			t.Errorf("%s: %s has verdict %q, want one of %q/%q/%q",
				matrixPath, scenarioCell(scenario), row.verdict, verdictCovered, verdictWeak, verdictUncovered)
		}
	}

	// (v) Section (c) must say what table (a) holds.
	checkDerivedLists(t, doc)

	// (vi) The summary counts, against the table's own columns.
	checkMatrixCounts(t, doc)
}

// ---------------------------------------------------------------------------
// Section (c): the uncovered and weak lists, derived from table (a).
// ---------------------------------------------------------------------------
//
// Section (c) used to be hand-written prose grouped by priority area, and it
// lagged table (a) badly enough that the two disagreed about whether a
// scenario was covered at all. Both lists are now generated from (a)'s own
// verdict column and asserted against it, so the document cannot contradict
// itself.

const (
	derivedUncoveredBegin = "<!-- BEGIN DERIVED: uncovered -->"
	derivedUncoveredEnd   = "<!-- END DERIVED: uncovered -->"
	derivedWeakBegin      = "<!-- BEGIN DERIVED: weak -->"
	derivedWeakEnd        = "<!-- END DERIVED: weak -->"
)

// derivedBlock answers the half-open line range strictly between two markers.
func derivedBlock(t *testing.T, lines []string, begin, end string) (int, int) {
	t.Helper()
	from, to := -1, -1
	for i, line := range lines {
		switch strings.TrimSpace(line) {
		case begin:
			from = i + 1
		case end:
			to = i
		}
	}
	if from < 0 || to < from {
		t.Fatalf("%s: no %s … %s block found", matrixPath, begin, end)
	}
	return from, to
}

// renderVerdictList renders one bullet per row carrying the given verdict.
func renderVerdictList(rows []matrixRow, verdict string) []string {
	var out []string
	for _, row := range rows {
		if row.verdict != verdict {
			continue
		}
		out = append(out, "- "+scenarioCell(row.scenario))
	}
	sort.Strings(out)
	if len(out) == 0 {
		return []string{"_None._"}
	}
	return out
}

// checkDerivedLists asserts section (c) says what table (a) holds.
func checkDerivedLists(t *testing.T, doc matrixDoc) {
	t.Helper()
	for _, spec := range []struct {
		begin, end, verdict, what string
	}{
		{derivedUncoveredBegin, derivedUncoveredEnd, verdictUncovered, "uncovered"},
		{derivedWeakBegin, derivedWeakEnd, verdictWeak, "weak"},
	} {
		from, to := derivedBlock(t, doc.lines, spec.begin, spec.end)
		var got []string
		for _, line := range doc.lines[from:to] {
			if strings.TrimSpace(line) != "" {
				got = append(got, strings.TrimSpace(line))
			}
		}
		want := renderVerdictList(doc.rows, spec.verdict)
		if strings.Join(got, "\n") == strings.Join(want, "\n") {
			continue
		}
		t.Errorf("%s: section (c)'s %s list disagrees with table (a)'s own verdict column.\n  section (c): %v\n  table (a):   %v\n  regenerate with %s=1",
			matrixPath, spec.what, got, want, matrixWriteEnv)
	}
}

// countLine is one derived figure in section (b): the marker that identifies
// its line, and the number the table says it should carry.
type countLine struct {
	marker string
	want   int
}

var matrixCountMarker = regexp.MustCompile(`\*\*(\d+)\*\*`)
var matrixLayerCount = regexp.MustCompile(`^- (Go e2e \(non-emacs\)|Webapp layer|Emacs e2e): (\d+) scenarios? `)

// checkMatrixCounts recomputes section (b) from table (a).
func checkMatrixCounts(t *testing.T, doc matrixDoc) {
	t.Helper()
	verdicts := map[string]int{}
	layerScenarios := map[string]int{}
	layerFiles := map[string]map[string]bool{
		layerGo: {}, layerWebapp: {}, layerEmacs: {},
	}
	for _, row := range doc.rows {
		verdicts[row.verdict]++
		for _, layer := range matrixLayers {
			if len(row.files[layer]) == 0 {
				continue
			}
			layerScenarios[layer]++
			for _, f := range row.files[layer] {
				layerFiles[layer][f] = true
			}
		}
	}
	want := []countLine{
		{marker: "- Covered ", want: verdicts[verdictCovered]},
		{marker: "- Weak ", want: verdicts[verdictWeak]},
		{marker: "- Uncovered ", want: verdicts[verdictUncovered]},
		{marker: "- Total canonical scenarios:", want: len(doc.rows)},
	}
	for _, wantLine := range want {
		line, ok := findLine(doc.lines, wantLine.marker)
		if !ok {
			t.Errorf("%s: no summary line starting %q", matrixPath, wantLine.marker)
			continue
		}
		got, ok := firstInt(line)
		if !ok {
			t.Errorf("%s: summary line %q carries no number", matrixPath, line)
			continue
		}
		if got != wantLine.want {
			t.Errorf("%s: summary line %q says %d, but table (a) holds %d", matrixPath, wantLine.marker, got, wantLine.want)
		}
	}
	for _, line := range doc.lines {
		m := matrixLayerCount.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		layer := m[1]
		if layer == "Go e2e (non-emacs)" {
			layer = layerGo
		}
		got, _ := strconv.Atoi(m[2])
		if got != layerScenarios[layer] {
			t.Errorf("%s: the %s by-layer line says %d scenarios, but table (a)'s column holds %d",
				matrixPath, layer, got, layerScenarios[layer])
		}
	}
}

func findLine(lines []string, marker string) (string, bool) {
	for _, line := range lines {
		if strings.HasPrefix(line, marker) {
			return line, true
		}
	}
	return "", false
}

var firstIntPattern = regexp.MustCompile(`\d+`)

func firstInt(line string) (int, bool) {
	if m := matrixCountMarker.FindStringSubmatch(line); m != nil {
		n, _ := strconv.Atoi(m[1])
		return n, true
	}
	if m := firstIntPattern.FindString(line); m != "" {
		n, _ := strconv.Atoi(m)
		return n, true
	}
	return 0, false
}

// ---------------------------------------------------------------------------
// (4) Regeneration.
// ---------------------------------------------------------------------------

// rewriteMatrix rewrites table (a)'s derived columns and section (b)'s
// counts, carrying every hand-written cell forward unchanged. A scenario with
// no row yet gets one whose hand-written cells are placeholders for a human
// to fill in.
func rewriteMatrix(t *testing.T, r registry, derived coverage, doc matrixDoc) {
	t.Helper()
	existing := map[string]matrixRow{}
	for _, row := range doc.rows {
		existing[row.scenario] = row
	}
	var rendered []string
	var renderedRows []matrixRow
	verdicts := map[string]int{}
	layerScenarios := map[string]int{}
	layerFiles := map[string]map[string]bool{layerGo: {}, layerWebapp: {}, layerEmacs: {}}
	for _, scenario := range r.canonical {
		row, ok := existing[scenario]
		if !ok {
			row = matrixRow{
				grounded:  "TODO",
				assertion: "TODO — a human must read the tests and state the strongest assertion.",
			}
		}
		row.scenario = scenario
		row.files = map[string][]string{}
		driven := false
		for _, layer := range matrixLayers {
			files := derived.files(scenario, layer)
			row.files[layer] = files
			if len(files) == 0 {
				continue
			}
			driven = true
			layerScenarios[layer]++
			for _, f := range files {
				layerFiles[layer][f] = true
			}
		}
		switch {
		case !driven:
			if row.verdict != verdictUncovered {
				row.assertion = "No counted e2e layer drives this scenario."
			}
			row.verdict = verdictUncovered
		case row.verdict == verdictUncovered || row.verdict == "":
			// It IS driven, and the document said otherwise. Only a human can
			// say whether the assertion is strong, so the verdict defaults to
			// the weaker claim and the stale prose is replaced by a demand
			// rather than carried forward as a fresh lie.
			row.verdict = verdictWeak
			row.assertion = matrixUnreadAssertion
		}
		verdicts[row.verdict]++
		rendered = append(rendered, renderRow(row))
		renderedRows = append(renderedRows, row)
	}

	lines := append([]string{}, doc.lines[:doc.tableFrom]...)
	lines = append(lines, rendered...)
	lines = append(lines, doc.lines[doc.tableTo:]...)

	for i, line := range lines {
		switch {
		case strings.HasPrefix(line, "- Covered "):
			lines[i] = replaceBoldInt(line, verdicts[verdictCovered])
		case strings.HasPrefix(line, "- Weak "):
			lines[i] = replaceBoldInt(line, verdicts[verdictWeak])
		case strings.HasPrefix(line, "- Uncovered "):
			lines[i] = replaceBoldInt(line, verdicts[verdictUncovered])
		case strings.HasPrefix(line, "- Total canonical scenarios:"):
			lines[i] = fmt.Sprintf("- Total canonical scenarios: %d", len(rendered))
		case matrixLayerCount.MatchString(line):
			m := matrixLayerCount.FindStringSubmatch(line)
			layer := m[1]
			key := layer
			if key == "Go e2e (non-emacs)" {
				key = layerGo
			}
			lines[i] = fmt.Sprintf("- %s: %d scenarios referenced across %d files", layer,
				layerScenarios[key], len(layerFiles[key]))
		}
	}

	// Section (c) is regenerated from the rows just rendered.
	for _, spec := range []struct {
		begin, end, verdict string
	}{
		{derivedUncoveredBegin, derivedUncoveredEnd, verdictUncovered},
		{derivedWeakBegin, derivedWeakEnd, verdictWeak},
	} {
		from, to := derivedBlock(t, lines, spec.begin, spec.end)
		replacement := append([]string{""}, renderVerdictList(renderedRows, spec.verdict)...)
		replacement = append(replacement, "")
		next := append([]string{}, lines[:from]...)
		next = append(next, replacement...)
		next = append(next, lines[to:]...)
		lines = next
	}

	path := filepath.Join(repo.e2eDir, matrixPath)
	if err := os.WriteFile(path, []byte(strings.Join(lines, "\n")), 0o644); err != nil {
		t.Fatalf("rewrite %s: %v", matrixPath, err)
	}
	t.Logf("rewrote %s from the registry and the tests", matrixPath)
}

func replaceBoldInt(line string, n int) string {
	return matrixCountMarker.ReplaceAllString(line, fmt.Sprintf("**%d**", n))
}

// TestMarkerScenarioSelection pins the marker half of `selectScenario` to the
// vendor's own rule: the network-resume marker only when it OPENS the prompt,
// the failure marker anywhere, and the network-resume marker tried first.
func TestMarkerScenarioSelection(t *testing.T) {
	t.Parallel()
	r := registry{}
	cases := []struct {
		name    string
		prompt  string
		want    string
		matched bool
	}{
		{"network-resume opening the prompt", "<!--agent-repl:network-resume-->\nresume a1", "network-resume", true},
		{"network-resume after leading whitespace", "  \n<!--agent-repl:network-resume-->", "network-resume", true},
		{"network-resume not opening the prompt", "see <!--agent-repl:network-resume-->", defaultScenarioName, false},
		{"fail marker anywhere", "please e2e-fail-this-turn now", "fail-marker", true},
		{"network-resume tried before the fail marker", "<!--agent-repl:network-resume--> e2e-fail-this-turn", "network-resume", true},
		{"no marker", "hello", defaultScenarioName, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Act
			got, matched := r.selectScenario(tc.prompt)
			// Assert
			if got != tc.want || matched != tc.matched {
				t.Fatalf("selectScenario(%q) = (%q, %v), want (%q, %v)", tc.prompt, got, matched, tc.want, tc.matched)
			}
		})
	}
}

// TestMarkerScenarioCellRoundTrips checks every marker scenario's rendered
// document cell reads back to its own name, so a marker row is never mistaken
// for a `!name` row or for another marker's.
func TestMarkerScenarioCellRoundTrips(t *testing.T) {
	t.Parallel()
	for _, m := range markerScenarios {
		t.Run(m.name, func(t *testing.T) {
			t.Parallel()
			// Act
			cell := scenarioCell(m.name)
			// Assert
			if got := parseScenarioCell(cell); got != m.name {
				t.Fatalf("parseScenarioCell(%q) = %q, want %q", cell, got, m.name)
			}
		})
	}
}
