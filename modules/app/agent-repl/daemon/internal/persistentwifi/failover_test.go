package persistentwifi

import (
	"context"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/usersetup"
)

func TestTickJoinsTheHotspotWhenWifiIsLostWithTheModeOn(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(true, hotspot)})
	h.r.on(argJoin, answer{})

	// Act.
	h.c.tick(context.Background())

	// Assert.
	if h.r.count(argJoin) != 1 {
		t.Fatalf("joins = %d, want 1", h.r.count(argJoin))
	}
	got, _ := h.c.Topic().Latest()
	if wifiArm(got) != "joined" || got.GetJoined().GetNetworkName() != hotspot {
		t.Fatalf("published (%s, %q), want the hotspot joined at once", wifiArm(got), got.GetJoined().GetNetworkName())
	}
	var joined bool
	for _, r := range h.records("info", opFailover) {
		if r.Context["hotspot"] == hotspot && r.Context["wifi"] == "joined" {
			joined = true
		}
	}
	if !joined {
		t.Fatalf("failover INFO records = %+v, want one of the hotspot joined", h.records("info", opFailover))
	}
}

func TestTickLeavesTheNetworkAloneWhenTheFailoverHasNothingToDo(t *testing.T) {
	cases := []struct {
		name  string
		pmset string
		link  string
	}{
		{name: "the mode is off", pmset: pmsetOff, link: summary(false, "")},
		{name: "another network is joined", pmset: pmsetOn, link: summary(true, "Home")},
		{name: "a network with a withheld name is joined", pmset: pmsetOn, link: summary(true, redactedName)},
		{name: "the mode is unreadable", pmset: "no such line", link: summary(false, "")},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t).standing(tc.pmset, tc.link)

			// Act.
			h.c.tick(context.Background())

			// Assert.
			if h.r.count(argJoin) != 0 {
				t.Fatal("a join ran with nothing for the failover to do")
			}
			if n := len(h.records("", opFailover)); n != 0 {
				t.Fatalf("recorded %d failover records, want none", n)
			}
		})
	}
}

func TestTickRetriesAFailedJoinOnEveryBeatAndWarnsOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})

	// Act.
	h.c.tick(context.Background())
	h.c.tick(context.Background())

	// Assert.
	if h.r.count(argJoin) != 2 {
		t.Fatalf("joins = %d, want one per beat", h.r.count(argJoin))
	}
	warns := h.records("warn", opFailover)
	if len(warns) != 1 {
		t.Fatalf("failover WARN records = %+v, want one", warns)
	}
	if warns[0].Context["cause"] != "Could not find network Test Hotspot." || warns[0].Context["hotspot"] != hotspot ||
		warns[0].Context["outcome"] != "failed" {
		t.Fatalf("WARN context = %+v, want the hotspot, the failed outcome and the tool's sentence", warns[0].Context)
	}
	if len(h.records("debug", opFailover)) != 1 {
		t.Fatalf("failover DEBUG records = %+v, want the repeat at DEBUG", h.records("debug", opFailover))
	}
	got, _ := h.c.Topic().Latest()
	if wifiArm(got) != "not_joined" {
		t.Fatalf("wifi = %s, want not_joined after a join that did not take", wifiArm(got))
	}
}

func TestTickWarnsAgainForADifferentCause(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argJoin, answer{out: "first"}, answer{out: "second"})

	// Act.
	h.c.tick(context.Background())
	h.c.tick(context.Background())

	// Assert.
	if n := len(h.records("warn", opFailover)); n != 2 {
		t.Fatalf("failover WARN records = %d, want one per distinct cause", n)
	}
}

func TestTickRecordsTheFailoverStandingDown(t *testing.T) {
	// Arrange: a join that did not take, then a network joined by other means.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(true, "Home")})
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	h.c.tick(context.Background())

	// Act.
	h.c.tick(context.Background())

	// Assert.
	var stoodDown bool
	for _, r := range h.records("info", opFailover) {
		if r.Context["was"] == "Could not find network Test Hotspot." && r.Context["wifi"] == "joined" {
			stoodDown = true
		}
	}
	if !stoodDown {
		t.Fatalf("failover INFO records = %+v, want the stand-down naming the cause it ended", h.records("info", opFailover))
	}
	if h.c.failoverCause != "" {
		t.Fatalf("failoverCause = %q, want it cleared", h.c.failoverCause)
	}
}

func TestTickWithNoHotspotConfiguredRecordsTheFaultOnce(t *testing.T) {
	// Arrange.
	h := newHarnessFor(t, "").standing(pmsetOn, summary(false, ""))

	// Act.
	h.c.tick(context.Background())
	h.c.tick(context.Background())

	// Assert.
	if h.r.count(argJoin) != 0 || h.r.count(argPreferred) != 0 {
		t.Fatal("a join was attempted with no hotspot configured")
	}
	errs := h.records("error", opFailover)
	if len(errs) != 1 {
		t.Fatalf("failover ERROR records = %+v, want one", errs)
	}
	cause, _ := errs[0].Context["cause"].(string)
	if errs[0].Context["env"] != EnvHotspot || !strings.Contains(cause, usersetup.Doc) {
		t.Fatalf("ERROR context = %+v, want the variable and the user guide", errs[0].Context)
	}
}

func TestTickWithNoWifiInterfaceWarnsOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.r.on(argPmsetRead, answer{out: pmsetOn})
	h.r.on(argPorts, answer{out: noWifi})

	// Act.
	h.c.tick(context.Background())
	h.c.tick(context.Background())

	// Assert.
	warns := h.records("warn", opFailover)
	if len(warns) != 1 || warns[0].Context["outcome"] != "no_wifi_interface" {
		t.Fatalf("failover WARN records = %+v, want one naming no_wifi_interface", warns)
	}
}

func TestTickAbandonedByItsCallerJoinsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	h.c.tick(ctx)

	// Assert.
	if h.r.count(argJoin) != 0 {
		t.Fatal("an abandoned beat joined the hotspot")
	}
}

func TestRunJoinsTheHotspotOnTheBeatThatSeesWifiLost(t *testing.T) {
	// Arrange: the boot read sees the hotspot joined, the beat sees it lost.
	h := newHarness(t).standing(pmsetOn, summary(true, hotspot))
	h.r.on(argSummary, answer{out: summary(true, hotspot)}, answer{out: summary(false, "")}, answer{out: summary(true, hotspot)})
	h.r.on(argJoin, answer{})
	h.c.Refresh(context.Background())
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- h.c.Run(ctx) }()

	// Act.
	(<-h.clock.afters) <- time.Unix(0, 0)
	<-h.clock.afters // the second wait proves the beat finished
	cancel()

	// Assert.
	if err := <-done; err != nil {
		t.Fatalf("Run() = %v, want nil on cancellation", err)
	}
	if h.r.count(argJoin) != 1 {
		t.Fatalf("joins = %d, want the beat's one", h.r.count(argJoin))
	}
	got, _ := h.c.Topic().Latest()
	if wifiArm(got) != "joined" {
		t.Fatalf("wifi after the beat = %s, want joined", wifiArm(got))
	}
}

func TestRefreshNeverJoinsTheHotspot(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))

	// Act.
	h.c.Refresh(context.Background())

	// Assert.
	if h.r.count(argJoin) != 0 {
		t.Fatal("Refresh joined the hotspot; the boot must never wait on a join")
	}
}
