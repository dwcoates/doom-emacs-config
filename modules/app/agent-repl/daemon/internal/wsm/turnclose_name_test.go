package wsm

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"strings"
	"testing"
)

func TestTurnCloseStringNamesEveryDeclaredClose(t *testing.T) {
	tests := []struct {
		how  TurnClose
		want string
	}{
		{how: CloseCompleted, want: "completed"},
		{how: CloseFailed, want: "failed"},
		{how: CloseKilled, want: "killed"},
		{how: CloseOrphaned, want: "orphaned"},
		{how: CloseAgentDied, want: "agent_died"},
		{how: CloseFolded, want: "folded"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act
			got := tt.how.String()
			// Assert
			if got != tt.want {
				t.Fatalf("String() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestTurnCloseStringNamesAnUndeclaredCloseByItsNumber(t *testing.T) {
	// Act
	got := TurnClose(42).String()
	// Assert
	if got != "close(42)" {
		t.Fatalf("String() = %q, want close(42)", got)
	}
}

// TestTurnCloseIsNamedOnlyByItsString fails a production switch outside this
// package that names turn closes itself: every close is named by
// TurnClose.String, so a second table cannot drift from the first.
func TestTurnCloseIsNamedOnlyByItsString(t *testing.T) {
	// Act
	offenders := handRolledNames(t, "CloseAgentDied")

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("turn closes named by a hand-rolled switch instead of TurnClose.String in %v", offenders)
	}
}

// TestClassificationArmIsNamedOnlyByItsString is the same guard for the
// verdict arms and ClassificationArm.String.
func TestClassificationArmIsNamedOnlyByItsString(t *testing.T) {
	// Act
	offenders := handRolledNames(t, "ArmUninterruptibleTurn")

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("classification arms named by a hand-rolled switch instead of ClassificationArm.String in %v", offenders)
	}
}

// handRolledNames scans every production source of the daemon outside this
// package for a switch case on the constant SELECTOR whose body returns a
// string literal: a second naming table for this package's type.
func handRolledNames(t *testing.T, selector string) []string {
	t.Helper()
	var offenders []string
	err := filepath.WalkDir(filepath.Join("..", ".."), func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if d.Name() == "testdata" || d.Name() == "node_modules" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		if strings.HasSuffix(filepath.ToSlash(filepath.Dir(path)), "internal/wsm") {
			return nil
		}
		file, err := parser.ParseFile(token.NewFileSet(), path, nil, 0)
		if err != nil {
			return err
		}
		ast.Inspect(file, func(n ast.Node) bool {
			clause, ok := n.(*ast.CaseClause)
			if !ok {
				return true
			}
			for _, expr := range clause.List {
				sel, ok := expr.(*ast.SelectorExpr)
				if !ok || sel.Sel.Name != selector {
					continue
				}
				for _, stmt := range clause.Body {
					if ret, ok := stmt.(*ast.ReturnStmt); ok && len(ret.Results) == 1 {
						if lit, ok := ret.Results[0].(*ast.BasicLit); ok && lit.Kind == token.STRING {
							offenders = append(offenders, path)
						}
					}
				}
			}
			return true
		})
		return nil
	})
	if err != nil {
		t.Fatalf("scan the daemon's sources: %v", err)
	}
	return offenders
}
