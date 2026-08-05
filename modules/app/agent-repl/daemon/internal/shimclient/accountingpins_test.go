package shimclient

import (
	"errors"
	"testing"
)

// fakeOpenClaims answers the durable open-claim question.
type fakeOpenClaims struct {
	ids []string
	err error
}

func (f *fakeOpenClaims) ActiveTurnIDs(string, string) ([]string, error) { return f.ids, f.err }

// pinClient is a Client wired only for the pin bookkeeping under test.
func pinClient(t *testing.T, claims OpenTurnClaims) *Client {
	t.Helper()
	return &Client{cfg: Config{SessionID: "s1", Workspace: "/ws", OpenTurnClaims: claims},
		logf: func(string, ...any) {}}
}

// THE UNPINNED-END REGRESSION. The pin set keeps a turn's start and end atomic:
// the durable cursor may only advance while nothing is pinned. Discarding the
// set on reconnect declared "nothing is in flight", which let the cursor move
// past a start whose end had not arrived — the end then replayed alone and was
// rejected as naming an unpinned accounting turn.
func TestPinsAreRebuiltFromTheDurableLedger(t *testing.T) {
	// Arrange — the ledger still holds one open claim.
	c := pinClient(t, &fakeOpenClaims{ids: []string{"turn-open"}})

	// Act.
	pinned := c.reconstructPinnedTurns()

	// Assert.
	if _, ok := pinned["turn-open"]; !ok {
		t.Fatalf("pins = %v, want the open claim rebuilt so the cursor stays held behind it", pinned)
	}
}

// A ledger read failure yields an EMPTY set, never a guess: inventing pins from
// a failed read would hold the cursor behind turns that may not exist.
func TestAFailedLedgerReadRebuildsNoPins(t *testing.T) {
	// Arrange.
	c := pinClient(t, &fakeOpenClaims{err: errors.New("db gone")})

	// Act / Assert.
	if got := c.reconstructPinnedTurns(); len(got) != 0 {
		t.Fatalf("pins = %v, want none after a failed read", got)
	}
}

// With no durable authority wired the behavior is the pre-existing one, so the
// change cannot alter a deployment that never supplied it.
func TestNoDurableAuthorityRebuildsNoPins(t *testing.T) {
	// Arrange.
	c := pinClient(t, nil)

	// Act / Assert.
	if got := c.reconstructPinnedTurns(); len(got) != 0 {
		t.Fatalf("pins = %v, want none without an OpenTurnClaims", got)
	}
}

// THE FROZEN-CURSOR REGRESSION, the mirror of the first. Only a stream
// TurnEnded released a pin, so a turn the daemon closed itself kept its pin
// forever and the cursor could never advance past it again.
func TestASynthesizedCloseReleasesItsPin(t *testing.T) {
	// Arrange.
	c := pinClient(t, nil)
	c.pinnedAccountingTurns = map[string]struct{}{"turn-a": {}, "turn-b": {}}

	// Act.
	c.UnpinAccountingTurn("turn-a")

	// Assert.
	if _, ok := c.pinnedAccountingTurns["turn-a"]; ok {
		t.Fatal("the synthesized close left its pin standing; the cursor would freeze here forever")
	}
	if _, ok := c.pinnedAccountingTurns["turn-b"]; !ok {
		t.Fatal("an unrelated turn's pin was released; only the closed turn frees its hold")
	}
}

// A close naming a turn this client never pinned is a no-op, not a fault.
func TestUnpinningAnUnknownTurnIsANoOp(t *testing.T) {
	// Arrange.
	c := pinClient(t, nil)
	c.pinnedAccountingTurns = map[string]struct{}{"turn-a": {}}

	// Act.
	c.UnpinAccountingTurn("never-pinned")

	// Assert.
	if len(c.pinnedAccountingTurns) != 1 {
		t.Fatalf("pins = %v, want the known pin untouched", c.pinnedAccountingTurns)
	}
}
