package feed

import (
	"context"
	"errors"
	"go/ast"
	"go/token"
	"slices"
	"strings"
	"sync"
	"testing"

	"agent-repld/internal/sourcescan"
)

func TestQueuedStoreWorkRunsOffTheLockOnceItIsReleased(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	var ranHeld, ran bool
	h.resolver.mu.Lock()
	h.resolver.afterUnlock(func(storeHold) {
		ran = true
		ranHeld = h.resolver.mu.Held()
	})

	// Act.
	notYet := ran
	h.resolver.mu.Unlock()

	// Assert.
	if notYet {
		t.Fatal("the queued step ran while the lock was still held")
	}
	if !ran || ranHeld {
		t.Fatalf("ran = %v under the lock = %v, want it run off the lock before Unlock returned", ran, ranHeld)
	}
}

func TestQueuedStoreWorkRunsInTheOrderTheHoldsReleased(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	var order []string

	// Act: two holds, each queuing two steps.
	for _, hold := range []string{"first", "second"} {
		h.resolver.mu.Lock()
		h.resolver.afterUnlock(func(storeHold) { order = append(order, hold+"/a") })
		h.resolver.afterUnlock(func(storeHold) { order = append(order, hold+"/b") })
		h.resolver.mu.Unlock()
	}

	// Assert.
	if want := []string{"first/a", "first/b", "second/a", "second/b"}; !slices.Equal(order, want) {
		t.Fatalf("order = %v, want %v", order, want)
	}
}

// TestAReleaseWaitsForItsBatchWhileAnotherGoroutineDrains pins that a caller
// returns only once its own work ran, even when the queue is drained by
// another goroutine: no write is left behind for a shutdown.
func TestAReleaseWaitsForItsBatchWhileAnotherGoroutineDrains(t *testing.T) {
	// Arrange: the first hold's step blocks until the test lets it go.
	h := newHarness(t)
	inFirst, letFirstGo := make(chan struct{}), make(chan struct{})
	var secondRan bool
	var mu sync.Mutex
	firstDone := make(chan struct{})
	go func() {
		defer close(firstDone)
		h.resolver.mu.Lock()
		h.resolver.afterUnlock(func(storeHold) {
			close(inFirst)
			<-letFirstGo
		})
		h.resolver.mu.Unlock()
	}()
	<-inFirst

	// Act: the second hold releases while the first goroutine drains.
	secondReturned := make(chan bool, 1)
	go func() {
		h.resolver.mu.Lock()
		h.resolver.afterUnlock(func(storeHold) {
			mu.Lock()
			secondRan = true
			mu.Unlock()
		})
		h.resolver.mu.Unlock()
		mu.Lock()
		defer mu.Unlock()
		secondReturned <- secondRan
	}()
	close(letFirstGo)

	// Assert.
	if ran := <-secondReturned; !ran {
		t.Fatal("the second release returned before its own step ran")
	}
	<-firstDone
}

func TestASettleRunsUnderTheLockAndItsWorkJoinsTheDrain(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	var order []string
	var settledHeld bool

	// Act.
	h.resolver.mu.Lock()
	h.resolver.afterUnlock(func(hold storeHold) {
		order = append(order, "step")
		hold.settle(func() {
			settledHeld = h.resolver.mu.Held()
			h.resolver.afterUnlock(func(storeHold) { order = append(order, "queued by the settle") })
		})
		order = append(order, "after the settle")
	})
	h.resolver.mu.Unlock()

	// Assert.
	if !settledHeld {
		t.Fatal("the settle ran off the lock")
	}
	if want := []string{"step", "after the settle", "queued by the settle"}; !slices.Equal(order, want) {
		t.Fatalf("order = %v, want %v", order, want)
	}
}

func TestStoreWorkQueuedOutsideAHoldPanics(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("queuing store work outside a hold did not panic")
		}
	}()

	// Act.
	h.resolver.afterUnlock(func(storeHold) {})
}

func TestAWriteOutcomeIsKnownOnceTheHoldIsReleased(t *testing.T) {
	cause := errors.New("disk full")
	tests := []struct {
		name         string
		err          error
		wantFailed   bool
		wantRecorded bool
	}{
		{name: "a write that succeeds is recorded", wantRecorded: true},
		{name: "a write that fails is told and answered", err: cause, wantFailed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			var failed, recorded bool
			h.resolver.mu.Lock()
			outcome := h.resolver.writeAfterUnlock(func(context.Context) error { return tt.err },
				func(err error) { failed = errors.Is(err, cause) }, func() { recorded = true })

			// Act.
			h.resolver.mu.Unlock()

			// Assert.
			if !errors.Is(outcome.err, tt.err) || failed != tt.wantFailed || recorded != tt.wantRecorded {
				t.Fatalf("outcome = %v, failed = %v, recorded = %v; want %v, %v, %v", outcome.err, failed, recorded, tt.err, tt.wantFailed, tt.wantRecorded)
			}
		})
	}
}

// TestEveryEntryPointPrimesItsWorkspace pins prime's one rule: every
// exported method of the resolver whose first argument names a workspace
// primes it as its first statement, so nothing it reaches under the lock
// reads the store (prime.go). The exceptions name why.
func TestEveryEntryPointPrimesItsWorkspace(t *testing.T) {
	// Arrange.
	exempt := map[string]string{
		"MarkMergeWorkspaceRemoved": "it never wakes a workspace whose feed is not held",
		"SetWorkspaceName":          "a push of a name, which reads nothing",
	}
	var unprimed []string

	// Act.
	sourcescan.EachSourceIn(t, ".", false, func(fset *token.FileSet, file *ast.File) {
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Recv == nil || !fn.Name.IsExported() || !receiverIsResolver(fn) || exempt[fn.Name.Name] != "" {
				continue
			}
			ws, ok := firstWorkspaceParam(fn)
			if !ok {
				continue
			}
			if !primesFirst(fn, ws) {
				unprimed = append(unprimed, fset.Position(fn.Pos()).String()+" "+fn.Name.Name)
			}
		}
	})

	// Assert.
	if len(unprimed) > 0 {
		t.Fatalf("entry points that do not prime their workspace first: %v", unprimed)
	}
}

func receiverIsResolver(fn *ast.FuncDecl) bool {
	star, ok := fn.Recv.List[0].Type.(*ast.StarExpr)
	if !ok {
		return false
	}
	id, ok := star.X.(*ast.Ident)
	return ok && id.Name == "resolver"
}

// firstWorkspaceParam answers the name of FN's first ids.WorkspaceID
// parameter when it is FN's first one past a context.
func firstWorkspaceParam(fn *ast.FuncDecl) (string, bool) {
	for _, field := range fn.Type.Params.List {
		if sourcescan.IsSelector(field.Type, "context", "Context") {
			continue
		}
		if !sourcescan.IsSelector(field.Type, "ids", "WorkspaceID") || len(field.Names) == 0 {
			return "", false
		}
		return field.Names[0].Name, true
	}
	return "", false
}

// primesFirst reports whether FN's first statement is r.prime(WS, ...).
func primesFirst(fn *ast.FuncDecl, ws string) bool {
	if fn.Body == nil || len(fn.Body.List) == 0 {
		return false
	}
	stmt, ok := fn.Body.List[0].(*ast.ExprStmt)
	if !ok {
		return false
	}
	call, ok := stmt.X.(*ast.CallExpr)
	if !ok || len(call.Args) == 0 {
		return false
	}
	sel, ok := call.Fun.(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "prime" {
		return false
	}
	arg, ok := call.Args[0].(*ast.Ident)
	return ok && strings.EqualFold(arg.Name, ws)
}
