// Package gotest holds what testrun knows about the go test runner's own
// conventions, shared by every part of testrun that reads or drives one: which
// functions are tests, how a -test.run pattern selects them, and how a -v
// result line reads.
package gotest

import (
	"go/ast"
	"regexp"
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

// ResultLine is a -v result line; its groups are the outcome (PASS, FAIL or
// SKIP), the test's name and its seconds.
var ResultLine = regexp.MustCompile(`^\s*--- (PASS|FAIL|SKIP): (\S+) \(([0-9.]+)s\)$`)
