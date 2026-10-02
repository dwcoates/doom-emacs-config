// socketflag_test.go — SUBJECT: which of the two ways to name the store's
// socket wins.
//
// $AGENT_REPL_STORE_SOCKET exists so a harness can point every participant at a
// private store without editing anybody's command line, and it supplies the
// --socket flag's DEFAULT. An explicit flag therefore beats it — which is the
// half that is easy to break silently, because a store that read the variable
// last would serve a path nothing on the command line ever named while a test
// or a supervisor waited on the one it asked for.
package integration

import (
	"net"
	"os"
	"testing"
)

// TestAnExplicitSocketFlagBeatsTheEnvironment.
func TestAnExplicitSocketFlagBeatsTheEnvironment(t *testing.T) {
	// Arrange: the environment names a path the store must never bind.
	decoy := shortSocketPath(t)

	// Act: startStore waits for the FLAG's socket to accept, so the wait itself
	// is half the assertion.
	store := startStore(t, storeOptions{envSocketPath: decoy})

	// Assert: the flag's socket serves...
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t, shim.agentEntry("w-flag", "u-flag",
		frameLine(agentID("main"), responseFrame("main", "act-1", "served on the flag's socket"))))
	page := openSession(ctx, t, store.client(), "main", nil)
	assertTexts(t, "the book on the flag's socket", pageTexts(page.GetPage()), []string{"served on the flag's socket"})

	// ...and the environment's path was never bound at all.
	if _, err := os.Stat(decoy); err == nil {
		t.Errorf("the store bound the environment's socket %q even though --socket named another", decoy)
	}
	store.assertNoErrorRecords()
}

// TestTheEnvironmentNamesTheSocketWhenNoFlagDoes is the other half: with no
// --socket on the command line the variable IS the address, which is what makes
// it usable as a harness-wide default.
func TestTheEnvironmentNamesTheSocketWhenNoFlagDoes(t *testing.T) {
	// Arrange + Act: no --socket flag, so only the environment names the path.
	store := startStore(t, storeOptions{noSocketFlag: true})

	// Assert: the store accepts on the environment's path and serves there.
	conn, err := net.Dial("unix", store.socket)
	if err != nil {
		t.Fatalf("the environment's socket %q does not accept: %v\nstderr:\n%s", store.socket, err, store.stderrText())
	}
	if err := conn.Close(); err != nil {
		t.Fatalf("closing the probe: %v", err)
	}
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t, shim.agentEntry("w-env", "u-env",
		frameLine(agentID("main"), responseFrame("main", "act-1", "served on the environment's socket"))))
	page := openSession(ctx, t, store.client(), "main", nil)
	assertTexts(t, "the book on the environment's socket", pageTexts(page.GetPage()), []string{"served on the environment's socket"})
	store.assertNoErrorRecords()
}
