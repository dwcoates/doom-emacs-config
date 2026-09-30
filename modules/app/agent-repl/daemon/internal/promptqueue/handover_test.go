package promptqueue

import (
	"context"
	"errors"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

func TestHandOverNamesTheMainAgentTheShimNamed(t *testing.T) {
	tests := []struct {
		name  string
		agent string
		want  []string
	}{
		{name: "an agent named", agent: "main-1", want: []string{"main-1"}},
		{name: "no agent named", agent: "", want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			w := &fakeWatcher{}
			success := &shimv1.StartTurnSuccess{Prompt: &conversationv1.AgentPrompt{
				Id: &conversationv1.TurnId{Value: "t1"}, Agent: &conversationv1.AgentId{Value: tt.agent},
			}}

			// Act
			handOver(theWorkspace, success, w)

			// Assert
			if got := w.mainAgents; len(got) != len(tt.want) || (len(got) == 1 && got[0] != tt.want[0]) {
				t.Fatalf("main agents = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestHandOverGivesTheWatcherTheAcceptedTurn(t *testing.T) {
	// Arrange
	w := &fakeWatcher{}
	success := &shimv1.StartTurnSuccess{Prompt: &conversationv1.AgentPrompt{Id: &conversationv1.TurnId{Value: "t1"}}}

	// Act
	handOver(theWorkspace, success, w)

	// Assert
	if len(w.opened) != 1 || w.opened[0].GetId().GetValue() != "t1" {
		t.Fatalf("opened = %v, want t1 handed over", w.opened)
	}
}

func TestRetireDeliveredTombstonesTheHoldAndClearsTheHead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	head := ids.TurnID("t1")
	h.q.state(theWorkspace).head = &head

	// Act
	err := h.q.retireDelivered(context.Background(), theWorkspace, "t1", dlog.NewTestLogger())

	// Assert
	held, _, _ := h.db.HeldPromptByTurn(context.Background(), "t1")
	if err != nil || held.Tombstone == nil || held.Tombstone.Kind != tombstoneDelivered || h.q.state(theWorkspace).head != nil {
		t.Fatalf("err = %v, hold = %+v, head = %v; want it retired as delivered and the head cleared", err, held, h.q.state(theWorkspace).head)
	}
}

func TestRetireDeliveredSurfacesAFailedTombstoneAndLogsIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.tombstoneErr = errors.New("disk full")

	// Act
	err := h.q.retireDelivered(context.Background(), theWorkspace, "t1", h.log.Global())

	// Assert
	if err == nil || !hasRecord(h, dlog.LevelError, opDeliver, "the hold was delivered but not retired") {
		t.Fatalf("err = %v, records = %+v; want the failure surfaced and logged at error", err, h.log.Records())
	}
}

// TestTurnsAreHandedOverAndRetiredThroughOneHelper fails a queue source that
// hands a turn to the watcher, or tombstones a delivered hold, by hand
// instead of through handOver and retireDelivered.
func TestTurnsAreHandedOverAndRetiredThroughOneHelper(t *testing.T) {
	// Arrange
	owners := map[string]string{"OnTurnOpened": "handOver", "tombstoneDelivered": "retireDelivered"}
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("read the package: %v", err)
	}
	var offenders []string

	// Act
	for _, e := range entries {
		name := e.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		file, err := parser.ParseFile(token.NewFileSet(), name, nil, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				var used string
				switch x := n.(type) {
				case *ast.SelectorExpr:
					used = x.Sel.Name
				case *ast.CompositeLit:
					for _, elt := range x.Elts {
						if kv, ok := elt.(*ast.KeyValueExpr); ok {
							if id, ok := kv.Value.(*ast.Ident); ok && id.Name == "tombstoneDelivered" {
								used = "tombstoneDelivered"
							}
						}
					}
				}
				if owner, ok := owners[used]; ok && fn.Name.Name != owner {
					offenders = append(offenders, name+":"+fn.Name.Name+" uses "+used)
				}
				return true
			})
		}
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled hand-over or retirement: %v", offenders)
	}
}
