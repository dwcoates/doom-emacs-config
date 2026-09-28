package health

import (
	"go/ast"
	"go/parser"
	"go/token"
	"sort"
	"strconv"
	"strings"
	"testing"

	"claude-repld/internal/sourcescan"
)

// stringConsts parses this package's production sources and answers every
// string constant whose name satisfies keep, name to value.
func stringConsts(t *testing.T, keep func(name string, spec *ast.ValueSpec) bool) map[string]string {
	t.Helper()
	out := map[string]string{}
	fset := token.NewFileSet()
	for _, file := range sourcescan.Production(t) {
		parsed, err := parser.ParseFile(fset, file.Name, file.Source, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", file.Name, err)
		}
		for _, decl := range parsed.Decls {
			gen, ok := decl.(*ast.GenDecl)
			if !ok || gen.Tok != token.CONST {
				continue
			}
			for _, raw := range gen.Specs {
				spec := raw.(*ast.ValueSpec)
				for i, name := range spec.Names {
					if !keep(name.Name, spec) || i >= len(spec.Values) {
						continue
					}
					lit, ok := spec.Values[i].(*ast.BasicLit)
					if !ok || lit.Kind != token.STRING {
						continue
					}
					value, err := strconv.Unquote(lit.Value)
					if err != nil {
						t.Fatalf("unquote %s: %v", name.Name, err)
					}
					out[name.Name] = value
				}
			}
		}
	}
	return out
}

// kindConsts answers every fault kind the vocabulary spells: each `Kind*`
// string constant of this package.
func kindConsts(t *testing.T) map[string]string {
	t.Helper()
	return stringConsts(t, func(name string, _ *ast.ValueSpec) bool {
		return strings.HasPrefix(name, "Kind")
	})
}

// edgeConsts answers every recovery edge: each constant typed Edge.
func edgeConsts(t *testing.T) map[string]string {
	t.Helper()
	return stringConsts(t, func(_ string, spec *ast.ValueSpec) bool {
		typ, ok := spec.Type.(*ast.Ident)
		return ok && typ.Name == "Edge"
	})
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
			_, ok := FaultLifetime(kinds[name])

			// Assert
			if !ok {
				t.Fatalf("%s (%q) declares no lifetime in health/lifetime.go; a kind with no defined end stands until the next restart", name, kinds[name])
			}
		})
	}
}

func TestEveryDeclaredLifetimeNamesAKindOfTheVocabulary(t *testing.T) {
	// Arrange
	known := map[string]bool{}
	for _, value := range kindConsts(t) {
		known[value] = true
	}

	for kind := range faultLifetimes {
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

func TestEveryLifetimeIsEitherMomentaryOrStandingOnAnEdge(t *testing.T) {
	for kind, lifetime := range faultLifetimes {
		t.Run(kind, func(t *testing.T) {
			// Arrange, Act
			momentaryWithEdges := lifetime.Momentary && len(lifetime.Edges) > 0
			standingWithout := !lifetime.Momentary && len(lifetime.Edges) == 0

			// Assert
			if momentaryWithEdges {
				t.Fatalf("%q is momentary and names edges %v; a momentary kind closes as it is recorded", kind, lifetime.Edges)
			}
			if standingWithout {
				t.Fatalf("%q stands with no recovery edge; nothing would ever close it", kind)
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

	for kind, lifetime := range faultLifetimes {
		t.Run(kind, func(t *testing.T) {
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
	declared := map[Edge]bool{EdgeRecorded: true}
	for _, lifetime := range faultLifetimes {
		for _, edge := range lifetime.Edges {
			declared[edge] = true
		}
	}

	for name, value := range edgeConsts(t) {
		t.Run(name, func(t *testing.T) {
			// Act
			ok := declared[Edge(value)]

			// Assert
			if !ok {
				t.Fatalf("the recovery edge %s closes no kind; it is dead", name)
			}
		})
	}
}

func TestClosesOnAnswersTheMomentaryEdgeForAMomentaryKind(t *testing.T) {
	// Arrange, Act
	got := ClosesOn(KindBounceDisposition, EdgeRecorded)

	// Assert
	if !got {
		t.Fatalf("ClosesOn(%q, recorded) = false, want true", KindBounceDisposition)
	}
}

func TestClosesOnAnswersNothingForAnUndeclaredKind(t *testing.T) {
	// Arrange, Act
	got := ClosesOn("no_such_kind", EdgeHealthyAttach)

	// Assert
	if got {
		t.Fatalf("ClosesOn(undeclared kind) = true, want false")
	}
}
