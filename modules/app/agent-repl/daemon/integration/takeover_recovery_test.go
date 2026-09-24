//go:build integration

package integration

import (
	"testing"

	"claude-repld/integration/harness"
)

// TestASuccessorHandedNothingRecoversTheShimNobodyServes is the live deploy of
// 2026-09-24 15:07, end to end against real daemon processes.
//
// A joining successor boots on a READ-ONLY state handle and used to promote it
// only inside a per-workspace adoption. Handed nothing, it took over still
// unable to write: its orphan recovery adopted the running shims and then
// every serving claim was refused, `wsm: handle is read-only`.
//
// The arrangement: a session runs on an incumbent; a successor joins it; the
// incumbent dies without writing a manifest (SIGKILL: the shim, in its own
// process group, survives holding its lock). The successor's takeover must
// recover that shim and write its claim.
func TestASuccessorHandedNothingRecoversTheShimNobodyServes(t *testing.T) {
	t.Parallel()
	// Arrange: a live session, and a successor joined to its daemon.
	f := newOpened(t, harness.Opts{})
	f.shim.ExpectStartSession()
	f.shim.ExpectWatchSession()
	watchesBefore := f.shim.Count(harness.RPCWatchSession)
	successor := harness.StartDaemon(t, harness.Opts{
		StateDir: f.d.StateDir,
		Joining:  f.d.Addr,
		ExtraEnv: []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir},
	})

	// Act: the incumbent dies having handed nothing over.
	f.d.Kill()

	// Assert: the successor adopts the shim at its takeover and its claim is
	// written. The recovery's INFO follows the claim; a refused claim is an
	// ERROR the warning sweep reports.
	successor.AwaitLogRecord(successor.RunLogPath(), "the takeover's recovery of the unserved shim", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.rollout.orphans" && r.Message == "adopted a shim a gone daemon left running"
	})
	ftAwaitTrue(t, successor.Ctx(), func() bool {
		return f.shim.Count(harness.RPCWatchSession) > watchesBefore
	}, "the successor's WatchSession on the recovered shim")
}
