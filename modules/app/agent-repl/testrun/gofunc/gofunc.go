// Package gofunc is which Go functions the go test runner runs, the one
// reading of that shape for testrun and for the e2e suite's own source scans
// (e2e/scenario_test.go), which cannot import testrun's internal packages.
package gofunc

import (
	"go/ast"
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
