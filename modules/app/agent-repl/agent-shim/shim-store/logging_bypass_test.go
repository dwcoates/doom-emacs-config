package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

type loggingBypass struct {
	path     string
	function string
	call     string
}

// A COMMAND-LINE TOOL'S STDOUT IS ITS ANSWER, NOT A DIAGNOSTIC. The rule this
// test enforces is about the SERVICE: under launchd, stderr is an append-only
// file the process neither owns nor can roll, so a second copy of an
// already-rotated log accumulates there forever. `cmd/shapes` is a one-shot
// report a human runs in a terminal and reads; routing its rows through the
// rotating log would put the answer somewhere the caller cannot see. The
// sanction is per FUNCTION, so anything else added to that file is still caught.
var sanctionedLoggingWrites = map[string]map[string]bool{
	"main.go":                     {"reportFatal": true},
	"internal/logging/logging.go": {"writeFull": true},
	"cmd/shapes/main.go":          {"main": true, "render": true},
}

func TestProductionHasNoLoggingBypasses(t *testing.T) {
	// Arrange.
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller could not locate the lint test")
	}
	root := filepath.Dir(thisFile)

	// Act.
	var bypasses []loggingBypass
	err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if entry.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		found, err := loggingBypassesInFile(path, filepath.ToSlash(rel))
		if err != nil {
			return err
		}
		bypasses = append(bypasses, found...)
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walk production Go: %v", err)
	}
	if len(bypasses) != 0 {
		t.Fatalf("direct diagnostic output bypasses internal/logging: %+v", bypasses)
	}
}

func loggingBypassesInFile(path, rel string) ([]loggingBypass, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, nil, 0)
	if err != nil {
		return nil, fmt.Errorf("parse %s: %w", rel, err)
	}
	imports := map[string]string{}
	for _, spec := range file.Imports {
		name := strings.Trim(spec.Path.Value, `"`)
		alias := filepath.Base(name)
		if spec.Name != nil {
			alias = spec.Name.Name
		}
		imports[alias] = name
	}
	var bypasses []loggingBypass
	for _, declaration := range file.Decls {
		fn, ok := declaration.(*ast.FuncDecl)
		if !ok || fn.Body == nil {
			continue
		}
		ast.Inspect(fn.Body, func(node ast.Node) bool {
			call, ok := node.(*ast.CallExpr)
			if !ok {
				return true
			}
			name, forbidden := forbiddenDiagnosticCall(call, imports)
			if forbidden && !sanctionedLoggingWrites[rel][fn.Name.Name] {
				bypasses = append(bypasses, loggingBypass{path: rel, function: fn.Name.Name, call: name})
			}
			return true
		})
	}
	return bypasses, nil
}

func forbiddenDiagnosticCall(call *ast.CallExpr, imports map[string]string) (string, bool) {
	if ident, ok := call.Fun.(*ast.Ident); ok && (ident.Name == "print" || ident.Name == "println") {
		return ident.Name, true
	}
	selector, ok := call.Fun.(*ast.SelectorExpr)
	if !ok {
		return "", false
	}
	if receiver, ok := selector.X.(*ast.Ident); ok {
		qualified := receiver.Name + "." + selector.Sel.Name
		switch imports[receiver.Name] {
		case "fmt":
			return qualified, strings.HasPrefix(selector.Sel.Name, "Print") || strings.HasPrefix(selector.Sel.Name, "Fprint")
		case "log":
			return qualified, true
		case "log/slog":
			return qualified, true
		case "io":
			return qualified, selector.Sel.Name == "WriteString"
		}
		if receiver.Name == "stderr" && selector.Sel.Name == "Write" {
			return qualified, true
		}
	}
	if nested, ok := selector.X.(*ast.SelectorExpr); ok {
		if receiver, ok := nested.X.(*ast.Ident); ok && imports[receiver.Name] == "os" && nested.Sel.Name == "Stderr" && selector.Sel.Name == "Write" {
			return receiver.Name + ".Stderr.Write", true
		}
	}
	return "", false
}

func TestLoggingBypassDetectorRecognizesForbiddenCalls(t *testing.T) {
	tests := []struct {
		name   string
		source string
		want   bool
	}{
		{name: "formatted terminal write", source: `package p; import "fmt"; func f() { fmt.Fprintf(nil, "bad") }`, want: true},
		{name: "standard logger", source: `package p; import "log"; func f() { log.Printf("bad") }`, want: true},
		{name: "structured standard logger", source: `package p; import "log/slog"; func f() { slog.Info("bad") }`, want: true},
		{name: "ordinary formatting", source: `package p; import "fmt"; func f() { _ = fmt.Sprintf("ok") }`, want: false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			fset := token.NewFileSet()
			file, err := parser.ParseFile(fset, "fixture.go", tt.source, 0)
			if err != nil {
				t.Fatalf("parse fixture: %v", err)
			}
			imports := map[string]string{}
			for _, spec := range file.Imports {
				name := strings.Trim(spec.Path.Value, `"`)
				imports[filepath.Base(name)] = name
			}
			var got bool

			// Act.
			ast.Inspect(file, func(node ast.Node) bool {
				if call, ok := node.(*ast.CallExpr); ok {
					_, forbidden := forbiddenDiagnosticCall(call, imports)
					got = got || forbidden
				}
				return true
			})

			// Assert.
			if got != tt.want {
				t.Fatalf("forbidden call = %t, want %t", got, tt.want)
			}
		})
	}
}
