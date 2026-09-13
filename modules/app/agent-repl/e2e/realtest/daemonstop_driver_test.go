//go:build realtest

package realtest

import (
	"context"
	"os"
	"path/filepath"
	"testing"
)

// THE SWEEP'S ORDERLY DAEMON STOP, driven from bin/realtest.sh.
//
// It is a `go test` for the same reason the leftover check is one: everything
// it needs — the generated Connect client, the daemon's published address — is
// already on this module's path, and a hand-rolled Connect frame in bash would
// be a second implementation of the wire contract.
//
// IT IS NOT A REALTEST AND MUST NEVER BE NAMED LIKE ONE. bin/test-realtest.sh
// asserts that every `func TestRealtest*` in this package has a row in the
// runner's world table. This drives no editor, presses no key and measures
// nothing; it is the sweep's way of stopping a daemon without leaving its
// sessions behind.
func TestOrderlyDaemonStop(t *testing.T) {
	if os.Getenv(daemonStopEnv) != "1" {
		t.Skipf("the orderly daemon stop runs only from bin/realtest.sh, which sets %s=1; "+
			"nothing else may fire an immediate shutdown at the owner's running daemon", daemonStopEnv)
	}
	home, err := os.UserHomeDir()
	if err != nil {
		t.Fatalf("resolve the owner's home directory: %v", err)
	}
	stateDir := filepath.Join(home, ".claude-emacs")
	if err := StopDaemonOrderly(context.Background(), stateDir, func(line string) { t.Log(line) }); err != nil {
		// THE FALLBACK IS THE CALLER'S, so this says what failed and stops.
		// bin/realtest.sh reads the non-zero status as "the daemon did not
		// answer its own door", states so, and only then signals.
		t.Fatalf("the daemon did not accept an orderly stop: %v\n"+
			"bin/realtest.sh falls back to SIGTERM from here, and a daemon stopped that way stands no "+
			"session down: its shims survive it and the next daemon reports each one as an unaccounted-for "+
			"bounce (daemon.rollout.reconcile).", err)
	}
}
