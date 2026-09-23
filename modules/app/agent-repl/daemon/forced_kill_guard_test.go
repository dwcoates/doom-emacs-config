package daemon_test

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strings"
	"testing"
)

// A FORCED KILL ENDS DETACHED WORK, SO ONLY THE USER MAY ASK FOR ONE. An
// interrupt ends only the synchronous turn; background agents, shells,
// monitors and workflows end by their own per-task stop, or by a forced kill
// the user explicitly asked for. Every KillTurn whose force is anything but
// the literal `false`, and every KillTurnRequest whose Force is anything but
// the literal `false`, is a forced-kill site, and each one is listed here with
// the user request that authorizes it. A new site fails this test until it is
// listed with its reason, which is where the question "did the user ask for
// this?" gets asked.
func TestForcedKillsHappenOnlyWhereTheUserAskedForThem(t *testing.T) {
	type allowance struct {
		path   string
		fn     string
		callee string
		count  int
		reason string
	}
	allowed := []allowance{
		{path: "internal/workspace/interrupt.go", fn: "(*verbs).interruptTurn", callee: "KillTurn", count: 1, reason: "force is the user's answer to the confirm challenge; an unconfirmed interrupt passes false"},
		{path: "internal/workspace/restart.go", fn: "(*verbs).forceEndTurn", callee: "KillTurn", count: 1, reason: "the user asked for the workspace to be restarted"},
		{path: "internal/workspace/sessions.go", fn: "(*shimAdapter).KillTurn", callee: "KillTurnRequest.Force", count: 1, reason: "the verbs' shim adapter forwards its caller's force unchanged"},
		{path: "internal/workspace/sender.go", fn: "(*sender).KillTurn", callee: "KillTurnRequest.Force", count: 1, reason: "the queue's sender forwards its caller's force unchanged"},
	}

	// Arrange.
	want := make(map[string]allowance, len(allowed))
	for _, item := range allowed {
		if item.reason == "" {
			t.Fatalf("allowance %s %s has no reason", item.path, item.fn)
		}
		want[forcedKillKey(item.path, item.fn, item.callee)] = item
	}

	// Act.
	sites, err := forcedKillSites(".", "cmd", "internal")
	if err != nil {
		t.Fatalf("scan production Go: %v", err)
	}

	// Assert.
	seen := make(map[string]int, len(allowed))
	var unexpected []string
	for _, site := range sites {
		key := forcedKillKey(site.path, site.fn, site.callee)
		if _, ok := want[key]; ok {
			seen[key]++
			continue
		}
		unexpected = append(unexpected, site.String())
	}
	sort.Strings(unexpected)
	if len(unexpected) > 0 {
		t.Errorf("forced kills the user never asked for; an interrupt ends only the turn, so pass force=false, or list the site with the user request that authorizes it:\n  %s", strings.Join(unexpected, "\n  "))
	}
	for key, item := range want {
		if seen[key] != item.count {
			t.Errorf("allowance %s saw %d forced kill(s), want %d; update the explicit site and reason when a forced kill moves", key, seen[key], item.count)
		}
	}
}

// TestTheForcedKillScanFindsEveryForcedShape pins the scanner itself, so the
// guard above cannot pass by no longer seeing the thing it guards.
func TestTheForcedKillScanFindsEveryForcedShape(t *testing.T) {
	tests := []struct {
		name string
		file string
		body string
		want []string
	}{
		{
			name: "a literal true is forced",
			file: "internal/x/x.go",
			body: "func f(s S) { s.KillTurn(nil, \"t\", true) }",
			want: []string{"internal/x/x.go|f|KillTurn"},
		},
		{
			name: "a variable is forced until proven otherwise",
			file: "internal/x/x.go",
			body: "func f(s S, force bool) { s.KillTurn(nil, \"t\", force) }",
			want: []string{"internal/x/x.go|f|KillTurn"},
		},
		{
			name: "a literal false is an interrupt",
			file: "internal/x/x.go",
			body: "func f(s S) { s.KillTurn(nil, \"t\", false) }",
		},
		{
			name: "a request carrying Force true is forced",
			file: "internal/x/x.go",
			body: "type T struct{}\nfunc (t *T) g(c C) { c.KillTurn(nil, &shimv1.KillTurnRequest{Force: true}) }",
			want: []string{"internal/x/x.go|(*T).g|KillTurnRequest.Force"},
		},
		{
			name: "a request carrying Force false is an interrupt",
			file: "internal/x/x.go",
			body: "func f(c C) { c.KillTurn(nil, &shimv1.KillTurnRequest{Force: false}) }",
		},
		{
			name: "a request with no Force is an interrupt",
			file: "internal/x/x.go",
			body: "func f(c C) { c.KillTurn(nil, &shimv1.KillTurnRequest{}) }",
		},
		{
			name: "a test file is not production",
			file: "internal/x/x_test.go",
			body: "func f(s S) { s.KillTurn(nil, \"t\", true) }",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			root := t.TempDir()
			path := filepath.Join(root, filepath.FromSlash(tt.file))
			for _, dir := range []string{filepath.Join(root, "cmd"), filepath.Dir(path)} {
				if err := os.MkdirAll(dir, 0o755); err != nil {
					t.Fatal(err)
				}
			}
			if err := os.WriteFile(path, []byte("package x\n\n"+tt.body+"\n"), 0o644); err != nil {
				t.Fatal(err)
			}

			// Act.
			sites, err := forcedKillSites(root, "cmd", "internal")

			// Assert.
			if err != nil {
				t.Fatalf("scan: %v", err)
			}
			var got []string
			for _, site := range sites {
				got = append(got, forcedKillKey(site.path, site.fn, site.callee))
			}
			if !slices.Equal(got, tt.want) {
				t.Fatalf("forced kill sites = %v, want %v", got, tt.want)
			}
		})
	}
}

// forcedKill is one forced-kill site: the file, the enclosing function, what
// forces (a KillTurn call or a KillTurnRequest's Force field) and its line.
type forcedKill struct {
	path   string
	fn     string
	callee string
	line   int
}

func (k forcedKill) String() string {
	return fmt.Sprintf("%s:%d %s forces %s", k.path, k.line, k.fn, k.callee)
}

func forcedKillKey(path, fn, callee string) string { return path + "|" + fn + "|" + callee }

// forcedKillSites walks the production Go under each root of base and answers
// every forced-kill site, in walk order.
func forcedKillSites(base string, roots ...string) ([]forcedKill, error) {
	files := token.NewFileSet()
	var sites []forcedKill
	for _, root := range roots {
		err := filepath.WalkDir(filepath.Join(base, root), func(path string, entry fs.DirEntry, walkErr error) error {
			if walkErr != nil {
				return walkErr
			}
			if entry.IsDir() || filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") {
				return nil
			}
			parsed, err := parser.ParseFile(files, path, nil, 0)
			if err != nil {
				return err
			}
			rel, err := filepath.Rel(base, path)
			if err != nil {
				return err
			}
			rel = filepath.ToSlash(rel)
			for _, declaration := range parsed.Decls {
				function, ok := declaration.(*ast.FuncDecl)
				if !ok || function.Body == nil {
					continue
				}
				name := funcName(function)
				ast.Inspect(function.Body, func(node ast.Node) bool {
					if callee, ok := forcedKillIn(node); ok {
						sites = append(sites, forcedKill{path: rel, fn: name, callee: callee, line: files.Position(node.Pos()).Line})
					}
					return true
				})
			}
			return nil
		})
		if err != nil {
			return nil, err
		}
	}
	return sites, nil
}

// forcedKillIn answers what forces a kill at node, if anything does.
func forcedKillIn(node ast.Node) (string, bool) {
	switch n := node.(type) {
	case *ast.CallExpr:
		selector, ok := n.Fun.(*ast.SelectorExpr)
		if !ok || selector.Sel.Name != "KillTurn" || len(n.Args) != 3 {
			return "", false
		}
		return "KillTurn", !isFalse(n.Args[2])
	case *ast.CompositeLit:
		if !namesKillTurnRequest(n.Type) {
			return "", false
		}
		for _, element := range n.Elts {
			field, ok := element.(*ast.KeyValueExpr)
			if !ok {
				continue
			}
			if key, ok := field.Key.(*ast.Ident); ok && key.Name == "Force" && !isFalse(field.Value) {
				return "KillTurnRequest.Force", true
			}
		}
	}
	return "", false
}

func namesKillTurnRequest(expr ast.Expr) bool {
	switch t := expr.(type) {
	case *ast.SelectorExpr:
		return t.Sel.Name == "KillTurnRequest"
	case *ast.Ident:
		return t.Name == "KillTurnRequest"
	}
	return false
}

func isFalse(expr ast.Expr) bool {
	ident, ok := expr.(*ast.Ident)
	return ok && ident.Name == "false"
}

// funcName names a function the way the allowances do: `f` for a function,
// `(*T).m` or `(T).m` for a method.
func funcName(function *ast.FuncDecl) string {
	if function.Recv == nil || len(function.Recv.List) == 0 {
		return function.Name.Name
	}
	receiver := function.Recv.List[0].Type
	pointer := ""
	if star, ok := receiver.(*ast.StarExpr); ok {
		pointer, receiver = "*", star.X
	}
	if index, ok := receiver.(*ast.IndexExpr); ok {
		receiver = index.X
	}
	name := "?"
	if ident, ok := receiver.(*ast.Ident); ok {
		name = ident.Name
	}
	return "(" + pointer + name + ")." + function.Name.Name
}
