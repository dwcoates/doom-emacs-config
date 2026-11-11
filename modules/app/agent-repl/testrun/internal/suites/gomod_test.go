package suites

import (
	"go/ast"
	"go/parser"
	"go/token"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"agentrepl/testrun/internal/gobuild"
	"agentrepl/testrun/roster"
)

func TestPkgOfMakesThePackageAsItsBuildDoes(t *testing.T) {
	tests := []struct {
		name string
		tags string
	}{
		{name: "the ordinary pass", tags: ""},
		{name: "a tagged pass", tags: "integration"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			l := Layout{Module: "/m", Work: "/w", Self: "/testrun"}
			s := roster.Suite{Name: "daemon",
				TestParallel: map[string]roster.Parallel{"internal/x": {Tests: 4, Slots: 2}},
				Programs:     map[string][]string{"internal/x": {"fakeshim"}}}
			b := moduleBuild(l, s.Name, "/m/daemon", tt.tags, false)

			// Act
			p := b.pkgOf(l, s, "internal/x")

			// Assert
			want := goPkg{Suite: "daemon", Module: "/m/daemon", Rel: "internal/x", Tags: tt.tags, Parallel: roster.Parallel{Tests: 4, Slots: 2},
				Bin: gobuild.BinPath(b.Options.BinDir, "internal/x"), Self: "/testrun", After: b.ID,
				Programs: []string{"fakeshim"}, ProgramsDir: l.ProgramsDir()}
			if p.Suite != want.Suite || p.Module != want.Module || p.Rel != want.Rel || p.Tags != want.Tags ||
				p.Parallel != want.Parallel || p.Bin != want.Bin || p.Self != want.Self || p.After != want.After ||
				!slices.Equal(p.Programs, want.Programs) || p.ProgramsDir != want.ProgramsDir {
				t.Fatalf("pkgOf = %+v, want %+v", p, want)
			}
		})
	}
}

// TestEveryPlannerMakesItsPackagesThroughPkgOf pins that the Go planners
// build a package's units only through GoBuild.pkgOf, so a hand-built goPkg
// cannot drift from the binary its build makes.
func TestEveryPlannerMakesItsPackagesThroughPkgOf(t *testing.T) {
	// Arrange
	files, err := filepath.Glob("*.go")
	if err != nil || len(files) == 0 {
		t.Fatalf("list the package's files: %v (%d files)", err, len(files))
	}
	var offenders []string

	// Act
	for _, path := range files {
		if strings.HasSuffix(path, "_test.go") {
			continue
		}
		f, err := parser.ParseFile(token.NewFileSet(), path, nil, 0)
		if err != nil {
			t.Fatal(err)
		}
		for _, decl := range f.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Name.Name == "pkgOf" {
				continue
			}
			ast.Inspect(fn, func(n ast.Node) bool {
				if lit, ok := n.(*ast.CompositeLit); ok {
					if id, ok := lit.Type.(*ast.Ident); ok && id.Name == "goPkg" {
						offenders = append(offenders, path+" "+fn.Name.Name)
					}
				}
				return true
			})
		}
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("goPkg built by hand in %v: make it with GoBuild.pkgOf", offenders)
	}
}
