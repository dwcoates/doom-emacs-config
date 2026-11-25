package daemonrules

import (
	"testing"
	"time"

	"agentrepl/logging/logsite/scan"

	"agent-repld/reposcans/internal/sourcescan"
)

// daemonLogCalls are the shapes of every dlog call: the four levels, each
// (operation, template, context), and logsite.Msg for a message a helper
// forwards.
var daemonLogCalls = scan.Calls(3, 1, 2, "Debug", "Info", "Warn", "Error")

// TestEveryDaemonLogSiteIsSound scans the daemon's source for every log call
// and fails on each site logging-contract.md "Log sites" refuses: a template
// that does not parse (a literal brace not doubled), a placeholder the call's
// own context literal does not hold, two calls of one function writing one
// template, and two sites sharing a key. Every fault is listed, not just the
// first.
func TestEveryDaemonLogSiteIsSound(t *testing.T) {
	// Arrange.
	sourcescan.InPackage(t, daemonRootDir)
	spec := scan.Spec{Runtime: "daemon", Module: "agent-repld", Root: ".", Calls: daemonLogCalls}

	// Act and Assert.
	started := time.Now()
	sites := scan.Require(t, spec)
	t.Logf("scanned %d log sites in %s", len(sites), time.Since(started))
}
