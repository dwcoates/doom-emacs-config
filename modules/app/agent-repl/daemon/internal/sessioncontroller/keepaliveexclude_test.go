package sessioncontroller

import (
	"errors"
	"fmt"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// excludeConsumer is a bare translation consumer holding a window ledger and
// nothing else. The exclusion is a pure function of the delta and the ledger,
// so nothing else is wired.
func excludeConsumer(t *testing.T, windows KeepAliveWindowLedger, logf func(string, ...any)) *consumer {
	t.Helper()
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil,
		newFakeClearCompactStore(), emptyTurnAccountingStore{}, logf, nil, nil, nil, nil, nil)
	c.keepAliveWindows = windows
	return c
}

// excludeItem is one conversation item carrying a request id and an instant —
// the two facts the exclusion can decide on.
func excludeItem(uuid, requestID string, tsMs int64) *frontendv1.ConversationItem {
	return &frontendv1.ConversationItem{Uuid: uuid, RequestId: requestID, TsMs: tsMs}
}

func excludeDelta(items ...*frontendv1.ConversationItem) *frontendv1.ConversationDelta {
	return &frontendv1.ConversationDelta{Workspace: "ws", SessionId: "s1", Items: items, ThroughSeq: 7}
}

func itemUUIDs(cd *frontendv1.ConversationDelta) []string {
	var out []string
	for _, item := range cd.GetItems() {
		out = append(out, item.GetUuid())
	}
	return out
}

// IDENTITY DECIDES, AND IT DECIDES UNDER A WARPED CLOCK. The item carries the
// ping's request id, which IS the window row's key, so the exclusion is a key
// lookup — and no disagreement between the daemon's clock and the vendor's can
// make that lookup answer differently. This is the defect the identity path
// exists for: with the interval as the only evidence, a daemon clock running
// ahead of the vendor's placed every one of the ping's own items OUTSIDE its
// own window and rendered them as the user's prompt.
func TestKeepAliveExclusionWithholdsByRequestIDUnderAWarpedClock(t *testing.T) {
	// Arrange — the ledger holds the ping's window, and its interval covers
	// NOTHING (the clocks disagree, so no timestamp comparison can hit).
	windows := newFakeKeepAliveWindows()
	if err := windows.Open(KeepAliveWindowRecord{TurnID: "ka_1", Workspace: "ws", StartedAtMs: 5_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}
	windows.coverAll = false
	c := excludeConsumer(t, windows, func(string, ...any) {})
	cd := excludeDelta(excludeItem("u1", "ka_1", 1_000))

	// Act.
	withheld := c.withholdKeepAlive(cd)

	// Assert.
	if withheld != 1 || len(cd.GetItems()) != 0 {
		t.Fatalf("withheld=%d remaining=%v, want the ping's own item withheld on its request id alone",
			withheld, itemUUIDs(cd))
	}
}

// THE FILE PLANE STILL FALLS BACK TO THE INTERVAL. A transcript record carries
// no request id at all, so the instant is the only evidence it has, and taking
// the identity path away from it would leave the ping's own transcript records
// rendered as the user's.
func TestKeepAliveExclusionWithholdsAnIDLessItemByItsInterval(t *testing.T) {
	// Arrange — no window row names anything this item carries; only the
	// interval covers it.
	windows := newFakeKeepAliveWindows()
	windows.coverAll = true
	c := excludeConsumer(t, windows, func(string, ...any) {})
	cd := excludeDelta(excludeItem("u1", "", 1_000))

	// Act.
	withheld := c.withholdKeepAlive(cd)

	// Assert.
	if withheld != 1 || len(cd.GetItems()) != 0 {
		t.Fatalf("withheld=%d remaining=%v, want the id-less item withheld on the interval",
			withheld, itemUUIDs(cd))
	}
}

// AN ID THAT NAMES NO WINDOW IS THE USER'S TURN, and it is SHOWN even when the
// interval claims it. That id names a real turn; the interval is two clocks
// agreeing. Preferring the interval here is how a skewed or stale window
// swallows the leading edge of the user's next real prompt.
func TestKeepAliveExclusionShowsAnUnknownRequestIDInsideACoveredInterval(t *testing.T) {
	// Arrange — the interval covers everything, and the item names a turn the
	// ledger has never heard of.
	windows := newFakeKeepAliveWindows()
	windows.coverAll = true
	c := excludeConsumer(t, windows, func(string, ...any) {})
	cd := excludeDelta(excludeItem("u1", "req_user", 1_000))

	// Act.
	withheld := c.withholdKeepAlive(cd)

	// Assert.
	if withheld != 0 || len(cd.GetItems()) != 1 {
		t.Fatalf("withheld=%d remaining=%v, want the user's own turn shown",
			withheld, itemUUIDs(cd))
	}
}

// AN IDENTITY READ FAILURE SHOWS THE ITEM AND SAYS SO, exactly as the interval
// read failure already did. Hiding real conversation because a bookkeeping
// table was unreadable is the worse failure of the two.
func TestKeepAliveExclusionShowsAnItemWhoseIdentityReadFailed(t *testing.T) {
	// Arrange.
	windows := newFakeKeepAliveWindows()
	windows.hasTurnErr = errors.New("state store is unavailable")
	var logged []string
	c := excludeConsumer(t, windows, func(f string, a ...any) {
		logged = append(logged, fmt.Sprintf(f, a...))
	})
	cd := excludeDelta(excludeItem("u1", "ka_1", 1_000))

	// Act.
	withheld := c.withholdKeepAlive(cd)

	// Assert.
	if withheld != 0 || len(cd.GetItems()) != 1 {
		t.Fatalf("withheld=%d remaining=%v, want the item shown after an unreadable ledger",
			withheld, itemUUIDs(cd))
	}
	if !strings.Contains(strings.Join(logged, "\n"), "keep-alive exclusion READ FAILED") {
		t.Fatalf("the unreadable ledger was never reported; logs = %v", logged)
	}
}
