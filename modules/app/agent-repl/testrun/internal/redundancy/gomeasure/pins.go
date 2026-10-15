package gomeasure

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"agentrepl/testrun/internal/gotest"
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
			if ok && fd.Recv == nil && gotest.IsTestFunc(fd, "Test", "T") {
				out = append(out, fd.Name.Name)
			}
		}
	}
	if len(out) == 0 {
		return nil, fmt.Errorf("gomeasure: %s declare no tests", strings.Join(files, ", "))
	}
	return out, nil
}

// Pin clauses (owner ruling via the parent workspace, 2026-10-15). A test
// stays whatever it covers when it verifies what the canonical logging
// helper received, which no mutant can see (no operator changes a record's
// level or fields):
//
//	(a) it awaits or asserts a record at WARN or ERROR level;
//	(b) it asserts a record's message or context fields, at any level;
//	(uncertain) the analysis cannot tell which a call does: pinned, failing
//	    closed.
//
// A bare await or existence check used as a barrier is no pin: whether the
// record is emitted at all is what the mutants on the log call itself test.
type clause int

const (
	barrier clause = iota
	uncertain
	fields
	level
)

func (c clause) String() string {
	return map[clause]string{barrier: "barrier", uncertain: "(uncertain)", fields: "(b) asserts a record's fields", level: "(a) a WARN or ERROR record"}[c]
}

var (
	// levelNames are the identifiers and literals that name WARN or ERROR.
	levelNames = map[string]bool{"LevelWarn": true, "LevelError": true, "warn": true, "warning": true, "error": true, "WARN": true, "WARNING": true, "ERROR": true}
	// quietLevels are the levels a check may name without pinning.
	quietLevels = map[string]bool{"LevelInfo": true, "LevelDebug": true, "info": true, "debug": true, "INFO": true, "DEBUG": true}
	// recordFields are a record's fields whose assertion is clause (b).
	recordFields = map[string]bool{"Message": true, "Msg": true, "Context": true, "Fields": true}
	// reportingCalls only print their arguments into a failure.
	reportingCalls = map[string]bool{"Fatalf": true, "Errorf": true, "Logf": true, "Fatal": true, "Error": true, "Log": true, "Sprintf": true, "Printf": true}
)

func calleeName(call *ast.CallExpr) string {
	switch fn := call.Fun.(type) {
	case *ast.Ident:
		return fn.Name
	case *ast.SelectorExpr:
		return fn.Sel.Name
	}
	return ""
}

func nameOf(e ast.Expr) string {
	switch e := e.(type) {
	case *ast.Ident:
		return e.Name
	case *ast.SelectorExpr:
		return e.Sel.Name
	case *ast.BasicLit:
		if v, err := strconv.Unquote(e.Value); err == nil {
			return v
		}
	}
	return ""
}

// classifyArgs reads what a log assertion's arguments check: a WARN/ERROR
// level, a record's fields, a level only known at run time, or nothing.
func classifyArgs(call *ast.CallExpr) clause {
	got := barrier
	raise := func(c clause) {
		if c > got {
			got = c
		}
	}
	for i, arg := range call.Args {
		ast.Inspect(arg, func(n ast.Node) bool {
			switch n := n.(type) {
			case *ast.Ident:
				// Only a Level constant: the word error also names a type.
				if strings.HasPrefix(n.Name, "Level") && levelNames[n.Name] {
					raise(level)
				}
			case *ast.BasicLit:
				if levelNames[nameOf(n)] {
					raise(level)
				}
			case *ast.SelectorExpr:
				if levelNames[n.Sel.Name] {
					raise(level)
				}
				if recordFields[n.Sel.Name] {
					raise(fields)
				}
			case *ast.KeyValueExpr:
				key := nameOf(n.Key)
				switch {
				case recordFields[key]:
					raise(fields)
				case key == "Level" && !levelNames[nameOf(n.Value)] && !quietLevels[nameOf(n.Value)]:
					raise(uncertain)
				}
			case *ast.BinaryExpr:
				// r.Level == x, with x neither a loud nor a quiet level.
				for _, side := range [][2]ast.Expr{{n.X, n.Y}, {n.Y, n.X}} {
					if sel, ok := side[0].(*ast.SelectorExpr); ok && sel.Sel.Name == "Level" {
						v := nameOf(side[1])
						if !levelNames[v] && !quietLevels[v] {
							raise(uncertain)
						}
					}
				}
			}
			return true
		})
		// An await's predicate that is not written out where it is called
		// may check anything.
		if strings.Contains(calleeName(call), "Record") && strings.HasPrefix(calleeName(call), "Await") && i == len(call.Args)-1 {
			if _, ok := arg.(*ast.FuncLit); !ok {
				raise(uncertain)
			}
		}
	}
	return got
}

// classifyUses reads what a function does with the variables a log call's
// result is bound to.
func classifyUses(body ast.Node, names []string, finders map[string]bool) clause {
	want := map[string]bool{}
	for _, n := range names {
		if n != "_" {
			want[n] = true
		}
	}
	if len(want) == 0 {
		return barrier
	}
	got := barrier
	raise := func(c clause) {
		if c > got {
			got = c
		}
	}
	var stack []ast.Node
	ast.Inspect(body, func(n ast.Node) bool {
		if n == nil {
			stack = stack[:len(stack)-1]
			return true
		}
		if id, ok := n.(*ast.Ident); ok && want[id.Name] && len(stack) > 0 {
			switch p := stack[len(stack)-1].(type) {
			case *ast.SelectorExpr:
				switch {
				case recordFields[p.Sel.Name]:
					raise(fields)
				case p.Sel.Name == "Level":
					raise(uncertain)
				}
			case *ast.CallExpr:
				if name := calleeName(p); !reportingCalls[name] && !finders[name] && p.Fun != id {
					raise(uncertain)
				}
			case *ast.IndexExpr, *ast.RangeStmt:
				raise(uncertain)
			}
		}
		stack = append(stack, n)
		return true
	})
	return got
}

// classifyBody is the strongest clause of fn's own log assertions, with the
// call that earned it.
func classifyBody(body ast.Node, target map[string]bool) (clause, string) {
	best, by := barrier, ""
	raise := func(c clause, name string) {
		if c > best {
			best, by = c, name
		}
	}
	finders := map[string]bool{"FindRecord": true}
	var stack []ast.Node
	ast.Inspect(body, func(n ast.Node) bool {
		if n == nil {
			stack = stack[:len(stack)-1]
			return true
		}
		if call, ok := n.(*ast.CallExpr); ok && target[calleeName(call)] {
			name := calleeName(call)
			var parent ast.Node
			if len(stack) > 0 {
				parent = stack[len(stack)-1]
			}
			c := classifyArgs(call)
			// Records() handed straight to FindRecord is FindRecord's.
			if name == "Records" {
				c = uncertain
				if pc, ok := parent.(*ast.CallExpr); ok && finders[calleeName(pc)] {
					c = barrier
				}
			}
			switch p := parent.(type) {
			case *ast.AssignStmt:
				var names []string
				for _, l := range p.Lhs {
					if id, ok := l.(*ast.Ident); ok {
						names = append(names, id.Name)
					}
				}
				// FindRecord's bool says only that the record exists.
				if name == "FindRecord" && len(names) == 2 {
					names = names[:1]
				}
				if u := classifyUses(body, names, finders); u > c {
					c = u
				}
			case *ast.ValueSpec:
				var names []string
				for _, id := range p.Names {
					names = append(names, id.Name)
				}
				if u := classifyUses(body, names, finders); u > c {
					c = u
				}
			case *ast.ExprStmt, *ast.CallExpr:
			default:
				// Used inline (compared, indexed, returned): it may be
				// checked in any way.
				if name != "FindRecord" {
					c = max(c, uncertain)
				}
			}
			raise(c, name)
		}
		stack = append(stack, n)
		return true
	})
	return best, by
}

// LogPins finds the tests that verify what the canonical logging helper
// received: a test whose body, or any function of the package's test files
// it reaches by name, makes a log assertion of clause (a), (b) or
// (uncertain) with one of calls (a function or method name, e.g.
// FindRecord). Reaching is by name across every test file, so a name two
// helpers share pins both ways, and a helper's assertion counts at its
// strongest whatever arguments a test passes it: the analysis fails closed.
// Each pin says its clause and the path to the call that earned it.
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
	type verdict struct {
		c  clause
		by string
	}
	direct := map[string]verdict{}
	callees := map[string][]string{}
	for name, nodes := range bodies {
		for _, n := range nodes {
			if c, by := classifyBody(n, target); c > direct[name].c {
				direct[name] = verdict{c, by}
			}
			ast.Inspect(n, func(c ast.Node) bool {
				if call, ok := c.(*ast.CallExpr); ok {
					if _, ok := bodies[calleeName(call)]; ok {
						callees[name] = append(callees[name], calleeName(call))
					}
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
		// The strongest clause any reachable function earns, with its path.
		best, bestPath := barrier, ""
		seen := map[string]bool{test: true}
		type step struct {
			name string
			path []string
		}
		queue := []step{{test, []string{test}}}
		for len(queue) > 0 {
			s := queue[0]
			queue = queue[1:]
			if v := direct[s.name]; v.c > best {
				best, bestPath = v.c, strings.Join(append(append([]string{}, s.path...), v.by), " -> ")
			}
			for _, next := range callees[s.name] {
				if !seen[next] {
					seen[next] = true
					queue = append(queue, step{next, append(append([]string{}, s.path...), next)})
				}
			}
		}
		if best > barrier {
			pins[test] = best.String() + ": " + bestPath
		}
	}
	return pins, nil
}
