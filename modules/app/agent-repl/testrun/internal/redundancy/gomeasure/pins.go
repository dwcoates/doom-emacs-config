package gomeasure

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// TestFuncs are the top-level tests the named test files of dir declare.
func TestFuncs(dir string, files []string) ([]string, error) {
	var out []string
	for _, name := range files {
		af, err := parser.ParseFile(token.NewFileSet(), filepath.Join(dir, name), nil, 0)
		if err != nil {
			return nil, fmt.Errorf("gomeasure: parse %s: %w", name, err)
		}
		for _, d := range af.Decls {
			fd, ok := d.(*ast.FuncDecl)
			if ok && fd.Recv == nil && isTestFunc(fd) {
				out = append(out, fd.Name.Name)
			}
		}
	}
	if len(out) == 0 {
		return nil, fmt.Errorf("gomeasure: %s declare no tests", strings.Join(files, ", "))
	}
	return out, nil
}

func isTestFunc(fd *ast.FuncDecl) bool {
	n := fd.Name.Name
	if !strings.HasPrefix(n, "Test") || n == "TestMain" {
		return false
	}
	if len(n) > 4 && strings.ToLower(n[4:5]) == n[4:5] && n[4] != '_' {
		return false // Testable is not a test
	}
	p := fd.Type.Params
	return p != nil && len(p.List) == 1 && len(p.List[0].Names) <= 1
}

// LogPins finds the tests that assert a canonical log record: a test whose
// body, or any function of the package's test files it reaches by name,
// calls one of calls (a function or method name, e.g. FindRecord). Reaching
// is by name across every test file, so a name two helpers share pins both
// ways: the analysis over-pins rather than misses one.
func LogPins(dir string, tests []string, calls []string) (map[string]string, error) {
	matches, err := filepath.Glob(filepath.Join(dir, "*_test.go"))
	if err != nil {
		return nil, fmt.Errorf("gomeasure: list %s's test files: %w", dir, err)
	}
	if len(matches) == 0 {
		return nil, fmt.Errorf("gomeasure: %s has no test files", dir)
	}
	sort.Strings(matches)
	bodies := map[string][]ast.Node{}
	for _, path := range matches {
		src, err := os.ReadFile(path)
		if err != nil {
			return nil, fmt.Errorf("gomeasure: read %s: %w", path, err)
		}
		af, err := parser.ParseFile(token.NewFileSet(), path, src, 0)
		if err != nil {
			return nil, fmt.Errorf("gomeasure: parse %s: %w", path, err)
		}
		for _, d := range af.Decls {
			if fd, ok := d.(*ast.FuncDecl); ok && fd.Body != nil {
				bodies[fd.Name.Name] = append(bodies[fd.Name.Name], fd.Body)
			}
		}
	}
	target := map[string]bool{}
	for _, c := range calls {
		target[c] = true
	}
	// direct[f] is the call that pins f itself; callees[f] the names it calls.
	direct := map[string]string{}
	callees := map[string][]string{}
	for name, nodes := range bodies {
		for _, n := range nodes {
			ast.Inspect(n, func(c ast.Node) bool {
				call, ok := c.(*ast.CallExpr)
				if !ok {
					return true
				}
				var callee string
				switch fn := call.Fun.(type) {
				case *ast.Ident:
					callee = fn.Name
				case *ast.SelectorExpr:
					callee = fn.Sel.Name
				}
				if target[callee] && direct[name] == "" {
					direct[name] = callee
				}
				if _, ok := bodies[callee]; ok {
					callees[name] = append(callees[name], callee)
				}
				return true
			})
		}
	}
	pins := map[string]string{}
	for _, test := range tests {
		if _, ok := bodies[test]; !ok {
			return nil, fmt.Errorf("gomeasure: test %s is declared in no test file of %s", test, dir)
		}
		if why := reach(test, direct, callees); why != "" {
			pins[test] = why
		}
	}
	return pins, nil
}

// reach is the path by which f reaches a pinning call, or "".
func reach(f string, direct map[string]string, callees map[string][]string) string {
	seen := map[string]bool{f: true}
	type step struct {
		name string
		path []string
	}
	queue := []step{{f, []string{f}}}
	for len(queue) > 0 {
		s := queue[0]
		queue = queue[1:]
		if c := direct[s.name]; c != "" {
			return "asserts a log record: " + strings.Join(append(s.path, c), " -> ")
		}
		for _, next := range callees[s.name] {
			if !seen[next] {
				seen[next] = true
				queue = append(queue, step{next, append(append([]string{}, s.path...), next)})
			}
		}
	}
	return ""
}
