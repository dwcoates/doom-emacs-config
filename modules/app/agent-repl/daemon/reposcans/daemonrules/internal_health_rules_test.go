package daemonrules

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"testing"

	"agent-repld/internal/health"
	"agent-repld/reposcans/internal/sourcescan"
)

// internalHealthDir is the package these rules hold, under the daemon module's root.
const internalHealthDir = "internal/health"

// stringConsts answers every string constant of the health package whose
// name and declared type satisfy keep, name to value.
func stringConsts(t *testing.T, keep func(name, typ string) bool) map[string]string {
	t.Helper()
	out := map[string]string{}
	for _, d := range sourcescan.StringDecls(t, internalHealthDir) {
		if d.Const && len(d.Values) == 1 && keep(d.Name, d.Type) {
			out[d.Name] = d.Values[0]
		}
	}
	return out
}

// kindConsts answers every fault kind the vocabulary spells: each `Kind*`
// string constant of the health package.
func kindConsts(t *testing.T) map[string]string {
	t.Helper()
	return stringConsts(t, func(name, _ string) bool {
		return strings.HasPrefix(name, "Kind")
	})
}

// lifetimeTableKinds answers every kind health's lifetime table
// (faultLifetimes, in lifetime.go) has a row for, read off the table's map
// literal: each key is a Kind constant, resolved through KINDS (kindConsts),
// or a string literal. Go cannot list an unexported map from outside its
// package, so the rule reads the table as it is written.
func lifetimeTableKinds(t *testing.T, kinds map[string]string) []string {
	t.Helper()
	path := filepath.Join(sourcescan.DaemonRoot(t), internalHealthDir, "lifetime.go")
	file, err := parser.ParseFile(token.NewFileSet(), path, nil, 0)
	if err != nil {
		t.Fatalf("parse %s: %v", path, err)
	}
	var out []string
	found := false
	for _, decl := range file.Decls {
		gen, ok := decl.(*ast.GenDecl)
		if !ok || gen.Tok != token.VAR {
			continue
		}
		for _, raw := range gen.Specs {
			spec := raw.(*ast.ValueSpec)
			if len(spec.Names) != 1 || spec.Names[0].Name != "faultLifetimes" || len(spec.Values) != 1 {
				continue
			}
			table, ok := spec.Values[0].(*ast.CompositeLit)
			if !ok {
				t.Fatalf("faultLifetimes is not a map literal")
			}
			found = true
			for _, elt := range table.Elts {
				kv := elt.(*ast.KeyValueExpr)
				switch key := kv.Key.(type) {
				case *ast.Ident:
					value, ok := kinds[key.Name]
					if !ok {
						t.Fatalf("faultLifetimes keys %s, which no Kind constant declares", key.Name)
					}
					out = append(out, value)
				case *ast.BasicLit:
					value, err := strconv.Unquote(key.Value)
					if err != nil {
						t.Fatalf("unquote %s: %v", key.Value, err)
					}
					out = append(out, value)
				default:
					t.Fatalf("faultLifetimes has a key that is neither a constant nor a literal: %T", kv.Key)
				}
			}
		}
	}
	if !found || len(out) == 0 {
		t.Fatalf("no faultLifetimes table in %s", path)
	}
	sort.Strings(out)
	return out
}

func TestEveryFaultKindDeclaresALifetime(t *testing.T) {
	// Arrange
	kinds := kindConsts(t)
	names := make([]string, 0, len(kinds))
	for name := range kinds {
		names = append(names, name)
	}
	sort.Strings(names)

	for _, name := range names {
		t.Run(name, func(t *testing.T) {
			// Act
			_, ok := health.FaultLifetime(kinds[name])

			// Assert
			if !ok {
				t.Fatalf("%s (%q) declares no lifetime in health/lifetime.go; a kind with no defined end stands until the next restart", name, kinds[name])
			}
		})
	}
}

func TestEveryDeclaredLifetimeNamesAKindOfTheVocabulary(t *testing.T) {
	// Arrange
	kinds := kindConsts(t)
	known := map[string]bool{}
	for _, value := range kinds {
		known[value] = true
	}

	for _, kind := range lifetimeTableKinds(t, kinds) {
		t.Run(kind, func(t *testing.T) {
			// Act
			ok := known[kind]

			// Assert
			if !ok {
				t.Fatalf("the lifetime table declares %q, which no Kind constant spells", kind)
			}
		})
	}
}

func TestEveryDeclaredEdgeIsARecoveryEdgeConstant(t *testing.T) {
	// Arrange
	edges := map[string]bool{}
	for _, value := range edgeConsts(t) {
		edges[value] = true
	}

	for _, kind := range lifetimeTableKinds(t, kindConsts(t)) {
		t.Run(kind, func(t *testing.T) {
			lifetime, _ := health.FaultLifetime(kind)
			for _, edge := range lifetime.Edges {
				// Act
				ok := edges[string(edge)]

				// Assert
				if !ok {
					t.Fatalf("%q declares the edge %q, which no Edge constant names", kind, edge)
				}
			}
		})
	}
}

func TestEveryRecoveryEdgeClosesSomeKind(t *testing.T) {
	// Arrange
	declared := map[health.Edge]bool{health.EdgeRecorded: true}
	for _, kind := range lifetimeTableKinds(t, kindConsts(t)) {
		lifetime, _ := health.FaultLifetime(kind)
		for _, edge := range lifetime.Edges {
			declared[edge] = true
		}
	}

	for name, value := range edgeConsts(t) {
		t.Run(name, func(t *testing.T) {
			// Act
			ok := declared[health.Edge(value)]

			// Assert
			if !ok {
				t.Fatalf("the recovery edge %s closes no kind; it is dead", name)
			}
		})
	}
}

// edgeConsts answers every recovery edge: each constant typed Edge.
func edgeConsts(t *testing.T) map[string]string {
	t.Helper()
	return stringConsts(t, func(_, typ string) bool {
		return typ == "Edge"
	})
}

// productionSources answers the daemon's production Go sources outside this
// package's lifetime table, keyed by path: every place an edge can be fired
// from. The integration suite and test files fire nothing in production.
func productionSources(t *testing.T) map[string]string {
	t.Helper()
	out := map[string]string{}
	err := filepath.WalkDir(sourcescan.DaemonRoot(t), func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if d.Name() == "integration" || d.Name() == "testdata" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		if filepath.Clean(path) == filepath.Join(sourcescan.DaemonRoot(t), "internal", "health", "lifetime.go") {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		out[path] = string(body)
		return nil
	})
	if err != nil {
		t.Fatalf("walk the daemon sources: %v", err)
	}
	return out
}

func TestEveryRecoveryEdgeHasACaller(t *testing.T) {
	sourcescan.InPackage(t, internalHealthDir)
	// Arrange
	edges := edgeConsts(t)
	sources := productionSources(t)
	names := make([]string, 0, len(edges))
	for name := range edges {
		names = append(names, name)
	}
	sort.Strings(names)

	for _, name := range names {
		t.Run(name, func(t *testing.T) {
			// Act
			qualified := regexp.MustCompile(`\bhealth\.` + name + `\b`)
			bare := regexp.MustCompile(`\b` + name + `\b`)
			called := false
			for path, body := range sources {
				inPackage := filepath.Dir(filepath.Clean(path)) == filepath.Join(sourcescan.DaemonRoot(t), "internal", "health")
				if (inPackage && bare.MatchString(body)) || qualified.MatchString(body) {
					called = true
					break
				}
			}

			// Assert
			if !called {
				t.Fatalf("the recovery edge %s (%q) has no caller; every fault that declares it would stand for good", name, edges[name])
			}
		})
	}
}
