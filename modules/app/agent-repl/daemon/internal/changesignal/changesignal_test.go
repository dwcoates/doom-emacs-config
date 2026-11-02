package changesignal

import (
	"go/ast"
	"go/token"
	"testing"

	"agent-repld/internal/sourcescan"
)

func isClosed(ch <-chan struct{}) bool {
	select {
	case <-ch:
		return true
	default:
		return false
	}
}

func TestSignal(t *testing.T) {
	cases := []struct {
		name string
		act  func(s *Signal) (taken <-chan struct{}, after <-chan struct{})
		// wantTaken and wantAfter are whether each channel is closed.
		wantTaken, wantAfter bool
	}{
		{name: "a taken channel stays open until a change", act: func(s *Signal) (<-chan struct{}, <-chan struct{}) {
			return s.Changed(), s.Changed()
		}},
		{name: "a change closes the taken channel and stands a fresh one", wantTaken: true, act: func(s *Signal) (<-chan struct{}, <-chan struct{}) {
			taken := s.Changed()
			s.Notify()
			return taken, s.Changed()
		}},
		{name: "a change nobody took is not seen by a later taker", act: func(s *Signal) (<-chan struct{}, <-chan struct{}) {
			s.Notify()
			taken := s.Changed()
			return taken, s.Changed()
		}},
		{name: "every waiter that took the channel is woken", wantTaken: true, wantAfter: true, act: func(s *Signal) (<-chan struct{}, <-chan struct{}) {
			first, second := s.Changed(), s.Changed()
			s.Notify()
			return first, second
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var s Signal

			// Act
			taken, after := tc.act(&s)

			// Assert
			if isClosed(taken) != tc.wantTaken || isClosed(after) != tc.wantAfter {
				t.Fatalf("closed = (%v, %v), want (%v, %v)", isClosed(taken), isClosed(after), tc.wantTaken, tc.wantAfter)
			}
		})
	}
}

// No daemon source hand-rolls the broadcast: a close of a channel followed by
// a fresh make into the same name is a Signal.
func TestNoDaemonSourceHandRollsAChangeBroadcast(t *testing.T) {
	// Arrange
	root := sourcescan.DaemonRoot(t)

	// Act
	var handRolled []string
	sourcescan.EachDaemonSource(t, root, []string{"internal/changesignal"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			block, ok := n.(*ast.BlockStmt)
			if !ok {
				return true
			}
			for i := 0; i+1 < len(block.List); i++ {
				if name, ok := closedName(block.List[i]); ok && remadeName(block.List[i+1]) == name {
					handRolled = append(handRolled, fset.Position(block.List[i].Pos()).String())
				}
			}
			return true
		})
	})

	// Assert
	if len(handRolled) != 0 {
		t.Fatalf("hand-rolled change broadcasts at %v; use changesignal.Signal", handRolled)
	}
}

// closedName answers X for a `close(X)` statement.
func closedName(stmt ast.Stmt) (string, bool) {
	expr, ok := stmt.(*ast.ExprStmt)
	if !ok {
		return "", false
	}
	call, ok := expr.X.(*ast.CallExpr)
	if !ok || len(call.Args) != 1 {
		return "", false
	}
	if fn, ok := call.Fun.(*ast.Ident); !ok || fn.Name != "close" {
		return "", false
	}
	return exprName(call.Args[0]), true
}

// remadeName answers X for an `X = make(chan ...)` statement.
func remadeName(stmt ast.Stmt) string {
	assign, ok := stmt.(*ast.AssignStmt)
	if !ok || len(assign.Lhs) != 1 || len(assign.Rhs) != 1 {
		return ""
	}
	call, ok := assign.Rhs[0].(*ast.CallExpr)
	if !ok || len(call.Args) == 0 {
		return ""
	}
	if fn, ok := call.Fun.(*ast.Ident); !ok || fn.Name != "make" {
		return ""
	}
	if _, ok := call.Args[0].(*ast.ChanType); !ok {
		return ""
	}
	return exprName(assign.Lhs[0])
}

func exprName(e ast.Expr) string {
	switch v := e.(type) {
	case *ast.Ident:
		return v.Name
	case *ast.SelectorExpr:
		return exprName(v.X) + "." + v.Sel.Name
	}
	return ""
}
