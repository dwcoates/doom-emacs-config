package main

import (
	"go/ast"
	"go/parser"
	"go/token"
	"strconv"
	"testing"
)

// BOOT ORDER IS THE PRODUCT HERE, so it is asserted on the boot's SOURCE ORDER
// rather than on a wall clock. A timing test would measure this machine; these
// tests measure the sequencing that made ~2s of every bounce dead time.
//
// bootCallOffset returns the source offset of the first call matching want in
// main.go, where want is matched against the callee's name (or, for
// phases.Mark, against the phase literal it marks).
func bootCallOffset(t *testing.T, want string) int {
	t.Helper()
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "main.go", nil, 0)
	if err != nil {
		t.Fatalf("parse main.go: %v", err)
	}
	found := -1
	ast.Inspect(file, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok || found >= 0 {
			return found < 0
		}
		if name := calleeName(call) + phaseSuffix(call); name == want {
			found = fset.Position(call.Pos()).Offset
		}
		return found < 0
	})
	if found < 0 {
		t.Fatalf("main.go contains no call matching %q; the boot sequencing test is measuring something that no longer exists", want)
	}
	return found
}

// calleeName is the bare function or method name being called.
func calleeName(call *ast.CallExpr) string {
	switch fn := call.Fun.(type) {
	case *ast.Ident:
		return fn.Name
	case *ast.SelectorExpr:
		return fn.Sel.Name
	}
	return ""
}

// phaseSuffix distinguishes phases.Mark("a") from phases.Mark("b"), which are
// otherwise the same call.
func phaseSuffix(call *ast.CallExpr) string {
	if calleeName(call) != "Mark" || len(call.Args) != 1 {
		return ""
	}
	lit, ok := call.Args[0].(*ast.BasicLit)
	if !ok || lit.Kind != token.STRING {
		return ""
	}
	phase, err := strconv.Unquote(lit.Value)
	if err != nil {
		return ""
	}
	return "(" + phase + ")"
}

// THE LISTENER OPENS FIRST. A reconnecting host must find an open socket long
// before the daemon's dependencies finish opening; while this bind sat at the
// end of boot every dial was refused and paid a client-side backoff.
func TestFrontendListenerBindsBeforeTheSSMOpenPhaseCompletes(t *testing.T) {
	// Arrange + Act.
	bind := bootCallOffset(t, "ListenFrontendUDS")
	ssmOpen := bootCallOffset(t, "Mark(ssm-open)")

	// Assert.
	if bind >= ssmOpen {
		t.Fatalf("frontend UDS bind is at offset %d, after the ssm-open phase mark at %d; the host cannot connect until the bind happens", bind, ssmOpen)
	}
}

// EXCLUSIVITY STILL PRECEDES EVERY UNIX BIND. Moving the frontend bind earlier
// must not move it in front of the TCP claim: that inversion is what let a
// duplicate daemon unlink a live daemon's socket and sustain a ~15 minute
// outage (bootclaim.go).
func TestBootClaimPrecedesTheFrontendSocketBind(t *testing.T) {
	// Arrange + Act.
	claim := bootCallOffset(t, "claimBootExclusivity")
	bind := bootCallOffset(t, "ListenFrontendUDS")

	// Assert.
	if claim >= bind {
		t.Fatalf("claimBootExclusivity is at offset %d, not before the frontend socket bind at %d", claim, bind)
	}
}

// The same exclusivity ordering for the shim socket, whose bind also unlinks.
func TestBootClaimPrecedesTheShimSocketBind(t *testing.T) {
	// Arrange + Act.
	claim := bootCallOffset(t, "claimBootExclusivity")
	bind := bootCallOffset(t, "ListenShim")

	// Assert.
	if claim >= bind {
		t.Fatalf("claimBootExclusivity is at offset %d, not before the shim socket bind at %d", claim, bind)
	}
}

// The frontend socket is bound before it is accepted on, and the accept phase
// is the one that reports serving. Binding without ever serving would leave a
// host connected to a socket nobody answers.
func TestFrontendListenerBindPrecedesItsServe(t *testing.T) {
	// Arrange + Act.
	bind := bootCallOffset(t, "Mark(frontend-listener-bind)")
	serve := bootCallOffset(t, "Mark(frontend-listener-serve)")

	// Assert.
	if bind >= serve {
		t.Fatalf("frontend-listener-bind is at offset %d, not before frontend-listener-serve at %d", bind, serve)
	}
}

// The rebase-worktree sweep is deferred behind the same host-connect hold: it
// is a ~1.3s $TMPDIR walk that used to run inside the frontend transport's own
// construction, directly in front of the host's accept.
func TestRebaseWorktreeSweepIsHeldOnTheHostConnectSignal(t *testing.T) {
	// Arrange + Act.
	sweep := bootCallOffset(t, "SweepOrphanRebaseWorktrees")
	serve := bootCallOffset(t, "Mark(frontend-listener-serve)")

	// Assert.
	if sweep <= serve {
		t.Fatalf("the rebase-worktree sweep is at offset %d, not after the frontend listener starts accepting at %d", sweep, serve)
	}
}

// The deferred geometry backfill is gated: its hold is constructed from the
// transport's host-connect signal, so the subprocess storm cannot precede the
// host's connect snapshot.
func TestGeometryBackfillIsHeldOnTheHostConnectSignal(t *testing.T) {
	// Arrange + Act.
	hold := bootCallOffset(t, "newBackfillHold")
	run := bootCallOffset(t, "HostConnectSnapshotServed")

	// Assert.
	if hold < 0 || run < 0 {
		t.Fatal("the geometry backfill is no longer held on the host-connect signal")
	}
}
