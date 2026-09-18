package harness

import (
	"os"
	"strconv"
	"strings"
	"sync"
	"testing"
)

// DefaultDaemonSlots is how many top-level tests may hold a LIVE DAEMON at
// once, no matter what `-parallel` or `-p` the run was invoked with.
//
// WHY THE HARNESS OWNS THIS AND NOT THE MAKEFILE. Every test in this suite
// owns a real claude-repld process plus a fake shim, and every wait it makes
// is bounded by DefaultTimeout — a failure bound sized at ~1.8x the slowest
// ordinary test measured at eight concurrent daemons. `make integration`
// passes `-parallel 8` for exactly that reason, but a flag is not a
// guarantee: `go test -tags integration ./...` runs packages at `-p
// $(nproc)`, each package at `-parallel $(nproc)`, so on a 16-core host the
// same tests ran against up to 256 concurrent daemon-plus-shim pairs. The
// whole suite then failed the way an oversubscribed suite fails — ordinary
// daemon steps past DefaultTimeout, fake-shim control sockets answering
// `broken pipe` — and which tests drew the short straw varied run to run.
// That is not a bound that was too tight; it is load nothing bounded.
//
// So the cap lives with the thing that creates the load. StartDaemon admits
// at most this many top-level tests at a time and the rest wait their turn,
// which makes the measured basis for DefaultTimeout true under EVERY
// invocation instead of only the Makefile's.
const DefaultDaemonSlots = 8

// DaemonSlotsEnv overrides DefaultDaemonSlots. A bigger host can raise it,
// and the Makefile's measured basis (AGENTS.md) applies: past eight the suite
// stops getting faster while the per-test load keeps climbing.
const DaemonSlotsEnv = "AGENT_REPL_ITEST_DAEMON_SLOTS"

var daemonSlots = make(chan struct{}, daemonSlotCount())

func daemonSlotCount() int {
	if raw := strings.TrimSpace(os.Getenv(DaemonSlotsEnv)); raw != "" {
		n, err := strconv.Atoi(raw)
		if err != nil || n < 1 {
			// REFUSED, never ignored: a run that believes it bounded the load
			// and did not is the failure this whole file exists to prevent.
			panic("harness: " + DaemonSlotsEnv + "=" + raw + ": want a positive integer")
		}
		return n
	}
	return DefaultDaemonSlots
}

// held is the live daemon count per TOP-LEVEL test, so one slot covers a test
// however many daemons it starts.
//
// THE SLOT IS PER TOP-LEVEL TEST, NOT PER DAEMON, AND THAT IS WHAT MAKES IT
// DEADLOCK-FREE. A handover test starts an incumbent and then a joining
// successor; a per-daemon slot would have each such test holding one while
// waiting for another, and with every slot held by such a test nobody could
// ever proceed. A test that already holds the suite's admission cannot block
// on it again.
var (
	heldMu sync.Mutex
	held   = map[string]int{}
)

// acquireDaemonSlot admits t's top-level test to the suite's live-daemon cap
// and registers the release. It is reentrant within one top-level test,
// including from its subtests.
func acquireDaemonSlot(t *testing.T) {
	t.Helper()
	root, _, _ := strings.Cut(t.Name(), "/")

	heldMu.Lock()
	first := held[root] == 0
	held[root]++
	heldMu.Unlock()

	if first {
		daemonSlots <- struct{}{}
	}

	t.Cleanup(func() {
		heldMu.Lock()
		held[root]--
		last := held[root] == 0
		if last {
			delete(held, root)
		}
		heldMu.Unlock()
		if last {
			<-daemonSlots
		}
	})
}
