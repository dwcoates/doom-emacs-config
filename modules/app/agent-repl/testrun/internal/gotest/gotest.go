// Package gotest holds what testrun knows about the go test runner's own
// conventions, shared by every part of testrun that reads or drives one: which
// functions are tests, how a -test.run pattern selects them, and how a -v
// result line reads.
package gotest

import (
	"go/ast"
	"regexp"
	"slices"
	"strings"
)

// IsTestFunc reports a func PrefixXxx(x *testing.<param>), the shape `go test`
// runs (TestMain excluded).
func IsTestFunc(fn *ast.FuncDecl, prefix, param string) bool {
	name := fn.Name.Name
	if name == "TestMain" || !strings.HasPrefix(name, prefix) {
		return false
	}
	if rest := name[len(prefix):]; rest != "" && rest[0] >= 'a' && rest[0] <= 'z' {
		return false
	}
	params := fn.Type.Params.List
	if len(params) != 1 || len(params[0].Names) > 1 {
		return false
	}
	star, ok := params[0].Type.(*ast.StarExpr)
	if !ok {
		return false
	}
	sel, ok := star.X.(*ast.SelectorExpr)
	return ok && sel.Sel.Name == param
}

// TopPattern matches exactly the named top-level tests (and their subtests).
func TopPattern(tests []string) string {
	return "^" + LiteralAlternation(tests) + "$"
}

// LiteralAlternation is a regexp group matching any one of items literally:
// the one spelling of "these exact names" every runner's name filter builds
// on (TopPattern here, the vitest -t pattern in suites).
func LiteralAlternation(items []string) string {
	q := make([]string, len(items))
	for i, it := range items {
		q[i] = regexp.QuoteMeta(it)
	}
	return "(" + strings.Join(q, "|") + ")"
}

// PathPattern is the -test.run value that runs exactly one test path: a
// top-level test and the subtests beneath it, each segment matched exactly
// (`-test.run` splits on "/" and matches each level separately).
func PathPattern(path string) string {
	segments := strings.Split(path, "/")
	q := make([]string, len(segments))
	for i, segment := range segments {
		q[i] = "^" + regexp.QuoteMeta(segment) + "$"
	}
	return strings.Join(q, "/")
}

// PathsPatterns are the -test.run values that together run exactly the test
// paths NAMES, each once: every named top-level test in one pattern; the
// paths beneath one top-level test in one pattern when each is a direct
// subtest of it; and every deeper path in a pattern of its own, since
// `-test.run` matches each level separately and one pattern cannot hold two
// depths beneath one test. A name nested under another named one runs with
// that one, not again.
func PathsPatterns(names []string) []string {
	var kept []string
	for _, n := range names {
		covered := slices.ContainsFunc(names, func(o string) bool { return strings.HasPrefix(n, o+"/") })
		if !covered && !slices.Contains(kept, n) {
			kept = append(kept, n)
		}
	}
	var tops, order []string
	beneath := map[string][]string{}
	for _, n := range kept {
		top, rest, nested := strings.Cut(n, "/")
		if !nested {
			tops = append(tops, n)
			continue
		}
		if _, seen := beneath[top]; !seen {
			order = append(order, top)
		}
		beneath[top] = append(beneath[top], rest)
	}
	var patterns []string
	if len(tops) > 0 {
		patterns = append(patterns, TopPattern(tops))
	}
	for _, top := range order {
		rests := beneath[top]
		if !slices.ContainsFunc(rests, func(r string) bool { return strings.Contains(r, "/") }) {
			patterns = append(patterns, TopPattern([]string{top})+"/"+TopPattern(rests))
			continue
		}
		for _, rest := range rests {
			patterns = append(patterns, PathPattern(top+"/"+rest))
		}
	}
	return patterns
}

// ResultLine is a -v result line; its groups are the outcome (PASS, FAIL or
// SKIP), the test's name and its seconds.
var ResultLine = regexp.MustCompile(`^\s*--- (PASS|FAIL|SKIP): (\S+) \(([0-9.]+)s\)$`)
