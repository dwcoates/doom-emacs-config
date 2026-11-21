package daemon_test

import (
	"strings"
	"testing"
	"time"

	"agentrepl/logging/logsite/scan"
)

// daemonLogCalls are the shapes of every dlog call: the four levels, each
// (operation, template, context), and logsite.Msg for a message a helper
// forwards.
var daemonLogCalls = []scan.Call{
	{Selector: "Debug", Args: 3, Template: 1, Context: 2},
	{Selector: "Info", Args: 3, Template: 1, Context: 2},
	{Selector: "Warn", Args: 3, Template: 1, Context: 2},
	{Selector: "Error", Args: 3, Template: 1, Context: 2},
	scan.MsgCall,
}

// TestEveryDaemonLogSiteIsSound scans the daemon's source for every log call
// and fails on each site logging-contract.md "Log sites" refuses: a template
// that does not parse (a literal brace not doubled), a placeholder the call's
// own context literal does not hold, two calls of one function writing one
// template, and two sites sharing a key. Every fault is listed, not just the
// first.
func TestEveryDaemonLogSiteIsSound(t *testing.T) {
	// Arrange.
	spec := scan.Spec{Runtime: "daemon", Module: "agent-repld", Root: ".", Calls: daemonLogCalls}

	// Act.
	started := time.Now()
	sites, err := scan.Scan(spec)
	faults := scan.Check(sites)
	t.Logf("scanned %d log sites in %s", len(sites), time.Since(started))

	// Assert.
	if err != nil {
		t.Fatalf("scan the daemon's source: %v", err)
	}
	if len(sites) == 0 {
		t.Fatal("the scan found no log site at all: it read none of the daemon's source")
	}
	if len(faults) > 0 {
		lines := make([]string, len(faults))
		for i, f := range faults {
			lines[i] = f.Error()
		}
		t.Fatalf("%d unsound log sites:\n%s", len(faults), strings.Join(lines, "\n"))
	}
}
