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

func TestTickRetriesAFailedJoinFiveSecondsLaterAndWarnsOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})

	// Act.
	h.c.tick(context.Background())
	h.clock.set(5 * time.Second)
	h.c.tick(context.Background())

	// Assert.
	if h.r.count(argJoin) != 2 {
		t.Fatalf("joins = %d, want one per beat five seconds apart", h.r.count(argJoin))
	}
	warns := h.records("warn", opFailover)
	if len(warns) != 1 {
		t.Fatalf("failover WARN records = %+v, want one", warns)
	}
	if warns[0].Context["cause"] != "Could not find network Test Hotspot." || warns[0].Context["hotspot"] != hotspot ||
		warns[0].Context["outcome"] != "failed" || warns[0].Context["retry_every"] != "5s" {
		t.Fatalf("WARN context = %+v, want the hotspot, the failed outcome, the tool's sentence and the 5s retry", warns[0].Context)
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
	h.clock.set(5 * time.Second)
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

func TestFailoverRetryEveryBacksOffWithTheTimeLost(t *testing.T) {
	cases := []struct {
		name  string
		lost  time.Duration
		every time.Duration
		ok    bool
	}{
		{name: "at the loss", lost: 0, every: 5 * time.Second, ok: true},
		{name: "just short of five minutes", lost: 5*time.Minute - time.Second, every: 5 * time.Second, ok: true},
		{name: "at five minutes", lost: 5 * time.Minute, every: time.Minute, ok: true},
		{name: "just short of thirty minutes", lost: 30*time.Minute - time.Second, every: time.Minute, ok: true},
		{name: "at thirty minutes", lost: 30 * time.Minute, every: time.Hour, ok: true},
		{name: "just short of three hours", lost: 3*time.Hour - time.Second, every: time.Hour, ok: true},
		{name: "at three hours", lost: 3 * time.Hour, ok: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			every, ok := failoverRetryEvery(tc.lost)

			// Assert.
			if every != tc.every || ok != tc.ok {
				t.Fatalf("failoverRetryEvery(%s) = (%s, %t), want (%s, %t)", tc.lost, every, ok, tc.every, tc.ok)
			}
		})
	}
}

func TestFailoverScheduleFailureAfterTheScheduleIsSpentGivesUpOnTheNextBeat(t *testing.T) {
	// Arrange: a loss whose last join started inside the three hours and
	// ended after them.
	var s failoverSchedule
	at := time.Unix(0, 0)
	s.due(at)
	s.failed(at.Add(2 * time.Hour))

	// Act.
	s.failed(at.Add(3*time.Hour + time.Second))
	got := s.due(at.Add(3*time.Hour + 5*time.Second))

	// Assert.
	if got != failoverExhausted {
		t.Fatalf("due = %d, want failoverExhausted at once rather than another hour's wait", got)
	}
}

func TestTickWaitsForTheRetryInterval(t *testing.T) {
	cases := []struct {
		name   string
		failAt time.Duration
		beatAt time.Duration
		joins  int
	}{
		{name: "the first tier's beat before five seconds", failAt: 0, beatAt: 4 * time.Second, joins: 1},
		{name: "the first tier's beat at five seconds", failAt: 0, beatAt: 5 * time.Second, joins: 2},
		{name: "the second tier's beat before a minute", failAt: 5 * time.Minute, beatAt: 5*time.Minute + 55*time.Second, joins: 2},
		{name: "the second tier's beat at a minute", failAt: 5 * time.Minute, beatAt: 6 * time.Minute, joins: 3},
		{name: "the third tier's beat before an hour", failAt: 30 * time.Minute, beatAt: 89 * time.Minute, joins: 2},
		{name: "the third tier's beat at an hour", failAt: 30 * time.Minute, beatAt: 90 * time.Minute, joins: 3},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: Wi-Fi lost at the epoch, a failed join then, and one
			// more failed join at failAt when it is past the loss.
			h := newHarness(t).standing(pmsetOn, summary(false, ""))
			h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
			h.c.tick(context.Background())
			if tc.failAt > 0 {
				h.clock.set(tc.failAt)
				h.c.tick(context.Background())
			}

			// Act.
			h.clock.set(tc.beatAt)
			h.c.tick(context.Background())

			// Assert.
			if n := h.r.count(argJoin); n != tc.joins {
				t.Fatalf("joins = %d, want %d", n, tc.joins)
			}
		})
	}
}

func TestTickRecordsEachBackOffOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	h.c.tick(context.Background())
	h.clock.set(5 * time.Second)
	h.c.tick(context.Background())

	// Act.
	h.clock.set(5 * time.Minute)
	h.c.tick(context.Background())
	h.clock.set(6 * time.Minute)
	h.c.tick(context.Background())
	h.clock.set(30 * time.Minute)
	h.c.tick(context.Background())

	// Assert.
	var backOffs []string
	for _, r := range h.records("info", opFailover) {
		if every, ok := r.Context["retry_every"].(string); ok {
			backOffs = append(backOffs, every)
		}
	}
	if len(backOffs) != 2 || backOffs[0] != "1m0s" || backOffs[1] != "1h0m0s" {
		t.Fatalf("back-off INFO intervals = %v, want [1m0s 1h0m0s]", backOffs)
	}
}

func TestTickGivesUpAfterThreeHoursAndRecordsItOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	h.c.tick(context.Background())
	h.clock.set(2*time.Hour + 30*time.Minute)
	h.c.tick(context.Background())

	// Act.
	h.clock.set(3*time.Hour + 30*time.Minute)
	h.c.tick(context.Background())
	h.clock.set(5 * time.Hour)
	h.c.tick(context.Background())

	// Assert.
	if n := h.r.count(argJoin); n != 2 {
		t.Fatalf("joins = %d, want none once three hours have passed", n)
	}
	var gaveUp []map[string]any
	for _, r := range h.records("warn", opFailover) {
		if _, ok := r.Context["lost_for"]; ok {
			gaveUp = append(gaveUp, r.Context)
		}
	}
	if len(gaveUp) != 1 {
		t.Fatalf("give-up WARN records = %+v, want one", gaveUp)
	}
	if gaveUp[0]["lost_for"] != "3h30m0s" || gaveUp[0]["attempts"] != 2 ||
		gaveUp[0]["cause"] != "Could not find network Test Hotspot." {
		t.Fatalf("give-up WARN context = %+v, want the time lost, the attempts and the last cause", gaveUp[0])
	}
}

func TestTickAfterAGiveUpRestartsTheScheduleOnceANetworkWasJoined(t *testing.T) {
	// Arrange: a failover that gave up, then a network joined, then lost.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(false, "")},
		answer{out: summary(true, "Home")}, answer{out: summary(false, "")})
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	h.c.tick(context.Background())
	h.clock.set(3 * time.Hour)
	h.c.tick(context.Background())
	h.clock.set(3*time.Hour + 5*time.Second)
	h.c.tick(context.Background())

	// Act.
	h.clock.set(3*time.Hour + 10*time.Second)
	h.c.tick(context.Background())

	// Assert.
	if n := h.r.count(argJoin); n != 2 {
		t.Fatalf("joins = %d, want the loss's first and the new loss's at once", n)
	}
	var stoodDown bool
	for _, r := range h.records("info", opFailover) {
		if r.Context["gave_up"] == true && r.Context["wifi"] == "joined" {
			stoodDown = true
		}
	}
	if !stoodDown {
		t.Fatalf("failover INFO records = %+v, want the stand-down naming the give-up", h.records("info", opFailover))
	}
}

func TestTickAfterAJoinTookRestartsTheScheduleOnTheNextLoss(t *testing.T) {
	// Arrange: a failed join, then one that took, then Wi-Fi lost again.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(false, "")},
		answer{out: summary(true, hotspot)}, answer{out: summary(false, "")})
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."}, answer{}, answer{})
	h.c.tick(context.Background())
	h.clock.set(10 * time.Minute)
	h.c.tick(context.Background())

	// Act.
	h.clock.set(10*time.Minute + 5*time.Second)
	h.c.tick(context.Background())

	// Assert.
	if n := h.r.count(argJoin); n != 3 {
		t.Fatalf("joins = %d, want the new loss joined at once, not a minute later", n)
	}
	var attempts []any
	for _, r := range h.records("info", opFailover) {
		if a, ok := r.Context["attempt"]; ok {
			attempts = append(attempts, a)
		}
	}
	if len(attempts) != 3 || attempts[2] != 1 {
		t.Fatalf("join INFO attempts = %v, want the new loss's join counted as its first", attempts)
	}
}
