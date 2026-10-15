package server

import (
	"go/ast"
	"go/parser"
	"go/token"
	"strings"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/sourcescan"
)

func TestPerWorkspaceMintsAnEntryOnFirstUse(t *testing.T) {
	// Arrange.
	s := &server{}
	m := map[ids.WorkspaceID]*int{}

	// Act.
	got := perWorkspace(s, m, "w1")

	// Assert.
	if got == nil || m["w1"] != got {
		t.Fatalf("perWorkspace = %v, map = %v, want a minted entry stored under w1", got, m)
	}
}

func TestPerWorkspaceAnswersTheSameEntryAgain(t *testing.T) {
	// Arrange.
	s := &server{}
	m := map[ids.WorkspaceID]*int{}
	first := perWorkspace(s, m, "w1")

	// Act.
	second := perWorkspace(s, m, "w1")

	// Assert.
	if first != second {
		t.Fatal("a second lookup minted a new entry, want the first one")
	}
}

func TestPerWorkspaceKeepsWorkspacesApart(t *testing.T) {
	// Arrange.
	s := &server{}
	m := map[ids.WorkspaceID]*int{}
	first := perWorkspace(s, m, "w1")

	// Act.
	other := perWorkspace(s, m, "w2")

	// Assert.
	if first == other || len(m) != 2 {
		t.Fatalf("w1 and w2 share an entry (map %v), want one each", m)
	}
}

// TestNoAccessorHandRollsPerWorkspace pins that every server method answering
// a *publish.Topic or *publish.Events for a workspace is a bare call of
// perWorkspace, so no accessor reimplements the lock-and-mint by hand.
func TestNoAccessorHandRollsPerWorkspace(t *testing.T) {
	// Arrange.
	fset := token.NewFileSet()
	checked := 0

	// Act and Assert.
	for _, f := range sourcescan.Production(t) {
		name, src := f.Name, f.Source
		file, err := parser.ParseFile(fset, name, src, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Recv == nil || fn.Type.Results == nil || len(fn.Type.Results.List) != 1 {
				continue
			}
			result := string(src[fset.Position(fn.Type.Results.Pos()).Offset:fset.Position(fn.Type.Results.End()).Offset])
			params := fn.Type.Params.List
			if !strings.HasPrefix(result, "*publish.") || len(params) != 1 ||
				string(src[fset.Position(params[0].Type.Pos()).Offset:fset.Position(params[0].Type.End()).Offset]) != "ids.WorkspaceID" {
				continue
			}
			checked++
			body := string(src[fset.Position(fn.Body.Lbrace).Offset+1 : fset.Position(fn.Body.Rbrace).Offset])
			if !strings.HasPrefix(strings.TrimSpace(body), "return perWorkspace(s, s.") || len(fn.Body.List) != 1 {
				t.Errorf("%s: %s hand-rolls a per-workspace accessor; call perWorkspace", name, fn.Name.Name)
			}
		}
	}
	if checked == 0 {
		t.Fatal("the scan found no per-workspace accessor, so it checks nothing")
	}
}
