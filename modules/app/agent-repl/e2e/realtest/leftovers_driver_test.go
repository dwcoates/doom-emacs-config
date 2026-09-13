//go:build realtest

package realtest

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"testing"
)

// THE SWEEP'S LEFTOVER CHECK, driven from bin/realtest.sh.
//
// It is a `go test` rather than a second program because everything it needs
// already lives in this package — the snapshot read of the owner's registry
// (state.go), the command-file ingress (leftovers.go) — and a second spelling
// of either in bash is the drift the shared implementation exists to prevent.
//
// IT IS NOT A REALTEST AND MUST NEVER BE NAMED LIKE ONE. bin/test-realtest.sh
// asserts that every `func TestRealtest*` in this package has a row in the
// runner's world table, and docs/REALTEST-PLAN.md is the contract for which
// realtests exist. This drives no editor, presses no key and measures nothing;
// it is a maintenance verb the sweep runs at its edges.
//
// The sweep calls it three times:
//
//	mode=report, prefix=~/.claude-emacs/realtest   BEFORE anything runs. Any
//	    row here belongs to a PREVIOUS sweep and the run declines rather than
//	    adding its own rows on top of a mess nobody noticed.
//	mode=clean,  prefix=<this run's directory>     at the END of the sweep,
//	    however the sweep ended — passed, failed or panicked. Every row this
//	    run created is closed and forgotten through the daemon, and a row that
//	    survives fails the sweep with the ids named.
//	mode=clean,  prefix=~/.claude-emacs/realtest   for `--clean-leftovers`,
//	    the operator's own way to clear what an older sweep left.

// leftoverFindingName is the ONE name the finding is reported under, so a
// reader of a sweep's output, of a manifest and of the changelog is reading
// about the same thing each time.
const leftoverFindingName = "REALTEST LEFTOVER WORKSPACES"

func TestCleanRealtestLeftovers(t *testing.T) {
	mode := os.Getenv(leftoverModeEnv)
	if mode == "" {
		t.Skipf("the leftover check runs only from bin/realtest.sh, which sets %s to report or clean",
			leftoverModeEnv)
	}
	prefix := os.Getenv(leftoverPrefixEnv)
	if prefix == "" {
		t.Fatalf("%s=%s was set without %s; the check refuses to guess which directory's rows it is about, "+
			"because guessing wide would forget the owner's own workspaces", leftoverModeEnv, mode, leftoverPrefixEnv)
	}
	home, err := os.UserHomeDir()
	if err != nil {
		t.Fatalf("resolve the owner's home directory: %v", err)
	}
	stateDir := filepath.Join(home, ".claude-emacs")
	dbPath := StateDBPath(stateDir)
	ctx := context.Background()

	switch mode {
	case "report":
		rows, err := LeftoverWorkspaces(ctx, dbPath, prefix)
		if err != nil {
			t.Fatalf("read the registry for rows under %s: %v", prefix, err)
		}
		if len(rows) == 0 {
			t.Logf("no workspace row in %s names a directory under %s", dbPath, prefix)
			return
		}
		t.Errorf("%s\n%s\nEach of these is a registry row a previous sweep created and never removed. "+
			"While one stands, the owner's editor reports it — `cannot host a durable log sink "+
			"(registered-dir=... [MISSING])` — every time the workspace is touched.\nRemedy: %s",
			leftoverFindingName, DescribeLeftovers(prefix, rows), LeftoverRemedy)
	case "clean":
		remaining, err := CleanLeftovers(ctx, dbPath, stateDir, prefix, func(line string) { t.Log(line) })
		if err != nil {
			t.Fatalf("clean the rows under %s: %v", prefix, err)
		}
		if len(remaining) == 0 {
			t.Logf("no workspace row in %s names a directory under %s", dbPath, prefix)
			return
		}
		t.Errorf("%s\n%s\nThese were asked to close and forget through the daemon's command-file ingress "+
			"and are still in the registry. Either no daemon is sweeping %s, or it refused; the daemon's own "+
			"log (%s) carries the refusal.\nRemedy once a daemon is serving: %s",
			leftoverFindingName, DescribeLeftovers(prefix, remaining),
			filepath.Join(stateDir, "output"),
			filepath.Join(stateDir, "logs", "daemon.run.log"), LeftoverRemedy)
	default:
		t.Fatalf("%s=%q is not a mode this check knows; it is `report` or `clean`", leftoverModeEnv, mode)
	}
}

// leftoverAssertionMessage is the same finding, phrased for the act realtests'
// own end-of-test assertion. It lives here rather than in the test that uses
// it so both spellings of the verdict cannot drift apart.
func leftoverAssertionMessage(prefix string, rows []LeftoverRow) string {
	return fmt.Sprintf("%s\n%s\nThis realtest created these and did not leave the registry as it found it. "+
		"Every one of them will produce `cannot host a durable log sink (registered-dir=... [MISSING])` in the "+
		"owner's editor once the run directory is deleted.\nRemedy: %s",
		leftoverFindingName, DescribeLeftovers(prefix, rows), LeftoverRemedy)
}
