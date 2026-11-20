package merge

import (
	"go/ast"
	"go/parser"
	"go/token"
	"slices"
	"testing"

	"agent-repld/internal/sourcescan"
)

// The owner's ruling (2026-09-28): a parked merge does NOT block its
// repository's queue. It yields its slot; the merges behind it proceed; once
// resumed it waits for the slot and makes its merge afresh on the new tip.

// TestAKickToARunningPumpIsRemembered pins the lost-wakeup fix: a merge
// enqueued (or a parked run asking for its slot back) in the instant the pump
// is finishing is still admitted, because the pump reads the mark before it
// goes idle.
func TestAKickToARunningPumpIsRemembered(t *testing.T) {
	// Arrange: an asynchronous orchestrator whose pump is running.
	h := newHarness(t)
	h.o.async = true
	h.o.mu.Lock()
	h.o.pumping[h.repoKey()] = true
	h.o.mu.Unlock()

	// Act.
	h.o.kick(h.repoKey())

	// Assert.
	h.o.mu.Lock()
	defer h.o.mu.Unlock()
	if !h.o.kicked[h.repoKey()] {
		t.Fatal("a kick to a running pump was dropped")
	}
}

func TestHeadExitOf(t *testing.T) {
	tests := []struct {
		name string
		out  outcome
		want string
	}{
		{name: "a landing", out: outcome{landed: "abc123", target: "master"}, want: HeadExitLanded},
		{name: "nothing to merge", out: outcome{alreadyOn: "master"}, want: HeadExitAlreadyOn},
		{name: "a failure", out: outcome{failed: "the suite failed"}, want: HeadExitFailed},
		{name: "a dequeue", out: outcome{abandoned: CauseUserDequeue}, want: "dequeued"},
		{name: "an evict", out: outcome{abandoned: CauseUserDrop}, want: "evicted"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := headExitOf(tc.out)

			// Assert
			if got != tc.want {
				t.Fatalf("headExitOf(%+v) = %q, want %q", tc.out, got, tc.want)
			}
		})
	}
}

func TestVacatingTheHeadRecordsItsStayOnceByItsEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	r := liveRun(t, h, theWorkspace)
	h.o.mu.Lock()
	h.o.takeHeadLocked(r)
	r.endExit = HeadExitLanded
	h.o.mu.Unlock()

	// Act
	h.o.mu.Lock()
	h.o.vacateHeadLocked(r)
	h.o.vacateHeadLocked(r)
	head := h.o.running[r.repo]
	h.o.mu.Unlock()

	// Assert
	exits, took := h.meter.headStays()
	if !slices.Equal(exits, []string{HeadExitLanded}) {
		t.Fatalf("head stays = %v, want one landed stay", exits)
	}
	if took[0] <= 0 {
		t.Fatalf("stay took %v, want the time from taking the head", took[0])
	}
	if head != nil {
		t.Fatalf("head = %v after the vacate, want none", head.ws)
	}
}

func TestVacatingARunThatDoesNotHoldTheHeadRecordsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	head := liveRun(t, h, theWorkspace)
	other := liveRun(t, h, "ws-b")
	h.o.mu.Lock()
	h.o.takeHeadLocked(head)
	h.o.mu.Unlock()

	// Act
	h.o.mu.Lock()
	h.o.vacateHeadLocked(other)
	still := h.o.running[head.repo]
	h.o.mu.Unlock()

	// Assert
	if exits, _ := h.meter.headStays(); len(exits) != 0 {
		t.Fatalf("head stays = %v, want none for a run off the head", exits)
	}
	if still != head {
		t.Fatal("the vacate of another run took the head away")
	}
}

func TestASuspendedHeadLeavesByItsSuspension(t *testing.T) {
	tests := []struct {
		name string
		why  suspension
		want string
	}{
		{name: "its tests failed", why: suspendAsideForTests, want: HeadExitNeededFixes},
		{name: "its rebase conflicted", why: suspendAsideForConflicts, want: HeadExitNeededConflictResolution},
		{name: "its workspace's transfer", why: suspendTransfer, want: HeadExitTransferred},
		{name: "its queue's pause", why: suspendPause, want: HeadExitPaused},
		{name: "its workspace's restart", why: suspendRestart, want: HeadExitRestarted},
		{name: "the handover", why: suspendHandover, want: HeadExitHandover},
		{name: "the daemon's exit", why: suspendExit, want: HeadExitDaemonExit},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			r := liveRun(t, h, theWorkspace)
			h.o.mu.Lock()
			h.o.takeHeadLocked(r)
			r.suspendLocked(tc.why)
			h.o.mu.Unlock()

			// Act
			h.o.mu.Lock()
			h.o.vacateHeadLocked(r)
			h.o.mu.Unlock()

			// Assert
			if exits, _ := h.meter.headStays(); !slices.Equal(exits, []string{tc.want}) {
				t.Fatalf("head stays = %v, want one %s stay", exits, tc.want)
			}
		})
	}
}

func TestARunLeavingTheHeadWithNoEndFailsHard(t *testing.T) {
	// Arrange
	h := newHarness(t)
	r := liveRun(t, h, theWorkspace)
	h.o.mu.Lock()
	h.o.takeHeadLocked(r)
	h.o.mu.Unlock()
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("a run left the head with no suspension and no teardown, and nothing failed")
		}
		h.o.mu.Unlock()
	}()

	// Act
	h.o.mu.Lock()
	h.o.vacateHeadLocked(r)
}

func TestEveryHeadChangeGoesThroughTheHeadHelpers(t *testing.T) {
	// Arrange
	fset := token.NewFileSet()
	owners := map[string]bool{"takeHeadLocked": true, "vacateHeadLocked": true}
	for _, source := range sourcescan.Production(t) {
		parsed, err := parser.ParseFile(fset, source.Name, source.Source, 0)
		if err != nil {
			t.Fatalf("parsing %s: %v", source.Name, err)
		}

		// Act
		var enclosing string
		ast.Inspect(parsed, func(n ast.Node) bool {
			if decl, ok := n.(*ast.FuncDecl); ok {
				enclosing = decl.Name.Name
			}
			// Assert
			switch node := n.(type) {
			case *ast.AssignStmt:
				for _, lhs := range node.Lhs {
					if index, ok := lhs.(*ast.IndexExpr); ok && namesRunning(index.X) && !owners[enclosing] {
						t.Errorf("%s: %s sets the queue's head itself, not through takeHeadLocked", fset.Position(node.Pos()), enclosing)
					}
				}
			case *ast.CallExpr:
				if fn, ok := node.Fun.(*ast.Ident); ok && fn.Name == "delete" && len(node.Args) > 0 && namesRunning(node.Args[0]) && !owners[enclosing] {
					t.Errorf("%s: %s clears the queue's head itself, not through vacateHeadLocked", fset.Position(node.Pos()), enclosing)
				}
			}
			return true
		})
	}
}

// namesRunning reports whether EXPR is an orchestrator's head map,
// `<x>.running`.
func namesRunning(expr ast.Expr) bool {
	sel, ok := expr.(*ast.SelectorExpr)
	return ok && sel.Sel.Name == "running"
}
