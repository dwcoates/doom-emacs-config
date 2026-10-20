package server

import (
	"context"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"path/filepath"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// receivePush reads STREAM until PICK answers a non-nil value for a push, and
// answers that value. PICK answering nil skips the push. The test fails,
// naming WHAT it waited for, when the stream ends first. Every receive*
// reader in this package's tests is a PICK over this one loop.
func receivePush[R, V any](
	t testing.TB,
	stream *connect.ServerStreamForClient[R],
	what string,
	pick func(*R) *V,
) *V {
	t.Helper()
	for stream.Receive() {
		if v := pick(stream.Msg()); v != nil {
			return v
		}
	}
	t.Fatalf("the stream ended before %s arrived: %v", what, stream.Err())
	return nil
}

// fatalRecorder is a testing.TB whose Fatalf records the message and
// returns, so a test can observe receivePush's failure without failing.
type fatalRecorder struct {
	testing.TB
	fatals []string
}

func (r *fatalRecorder) Helper() {}

func (r *fatalRecorder) Fatalf(format string, args ...any) {
	r.fatals = append(r.fatals, fmt.Sprintf(format, args...))
}

func TestReceivePushAnswersTheFirstPushPickAccepts(t *testing.T) {
	// Arrange
	h := newHarness(t)
	stream := dialHostStream(t, h, t.Context())
	h.Server.NotificationClicked(testWorkspaceID)
	h.Server.(*server).hostEventStream(testWorkspaceID).Emit(&agentreplv1.WatchHostWorkspaceResponse{
		Push: &agentreplv1.WatchHostWorkspaceResponse_FinalResponseClicked{
			FinalResponseClicked: &agentreplv1.HostWorkspaceFinalResponseClicked{},
		},
	})

	// Act — the host state and the notification click are skipped.
	got := receivePush(t, stream, "a final-response click",
		func(r *agentreplv1.WatchHostWorkspaceResponse) *agentreplv1.HostWorkspaceFinalResponseClicked {
			return r.GetFinalResponseClicked()
		})

	// Assert
	if got == nil {
		t.Fatalf("receivePush answered nil, want the final-response click")
	}
}

func TestReceivePushFailsNamingWhatItWaitedFor(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	stream := dialHostStream(t, h, ctx)
	cancel()
	rec := &fatalRecorder{TB: t}

	// Act
	got := receivePush(rec, stream, "a never-sent push",
		func(*agentreplv1.WatchHostWorkspaceResponse) *agentreplv1.HostWorkspaceFinalResponseClicked {
			return nil
		})

	// Assert
	if got != nil || len(rec.fatals) != 1 || !strings.Contains(rec.fatals[0], "before a never-sent push arrived") {
		t.Fatalf("answer = %v, fatals = %q; want nil and one failure naming the push", got, rec.fatals)
	}
}

// TestEveryStreamReaderIsAPickOverReceivePush pins that no receive* test
// reader loops over a stream by hand: each one hands receivePush its pick, so
// the skip-until-found loop and its failure message exist once.
func TestEveryStreamReaderIsAPickOverReceivePush(t *testing.T) {
	// Arrange
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	fset := token.NewFileSet()
	var handRolled []string

	// Act
	for _, name := range files {
		file, err := parser.ParseFile(fset, name, nil, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil || !strings.HasPrefix(fn.Name.Name, "receive") || fn.Name.Name == "receivePush" {
				continue
			}
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				if loop, ok := n.(*ast.ForStmt); ok && callsReceive(loop.Cond) {
					handRolled = append(handRolled, fmt.Sprintf("%s:%s", name, fn.Name.Name))
				}
				return true
			})
		}
	}

	// Assert
	if len(handRolled) != 0 {
		t.Fatalf("readers looping over a stream by hand: %v; hand receivePush a pick instead", handRolled)
	}
}

// callsReceive reports whether EXPR is a `<x>.Receive()` call.
func callsReceive(expr ast.Expr) bool {
	call, ok := expr.(*ast.CallExpr)
	if !ok {
		return false
	}
	sel, ok := call.Fun.(*ast.SelectorExpr)
	return ok && sel.Sel.Name == "Receive"
}
