package persistentwifi

import (
	"context"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/usersetup"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

func TestTickJoinsTheHotspotWhenNoKnownNetworkComesBackWithinTheSettle(t *testing.T) {
	// Arrange: Wi-Fi lost, the radio cycled, nothing joined after the settle.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(false, "")},
		answer{out: summary(false, "")}, answer{out: summary(true, hotspot)})
	h.r.on(argJoin, answer{})
	h.c.tick(context.Background())

	// Act.
	h.tickAt(knownNetworkSettle)

	// Assert.
	if h.r.count(argJoin) != 1 || h.r.count(argRadioOff) != 1 {
		t.Fatalf("joins = %d and radio cycles = %d, want one of each", h.r.count(argJoin), h.r.count(argRadioOff))
	}
	got, _ := h.c.Topic().Latest()
	if wifiArm(got) != "joined" || got.GetJoined().GetNetworkName() != hotspot {
		t.Fatalf("published (%s, %q), want the hotspot joined", wifiArm(got), got.GetJoined().GetNetworkName())
	}
	joining := h.failoverRecords("info", msgJoiningHotspot)
	if len(joining) != 1 || joining[0]["known_network"] != knownSettled || joining[0]["attempt"] != 1 {
		t.Fatalf("joining records = %+v, want one naming the settle that passed", joining)
	}
	joined := h.failoverRecords("info", "the hotspot was joined after Wi-Fi was lost")
	if len(joined) != 1 || joined[0]["hotspot"] != hotspot || joined[0]["wifi"] != "joined" {
		t.Fatalf("joined records = %+v, want one of the hotspot joined", joined)
	}
}

func TestTickCyclesTheRadioBeforeTheHotspotWhenWifiIsLost(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))

	// Act.
	h.c.tick(context.Background())

	// Assert.
	var steps []string
	for _, c := range h.r.ran() {
		if c == argRadioOff || c == argRadioOn || c == argJoin {
			steps = append(steps, c)
		}
	}
	if strings.Join(steps, "|") != argRadioOff+"|"+argRadioOn {
		t.Fatalf("steps = %v, want the radio powered off then on and no join yet", steps)
	}
	cycling := h.failoverRecords("info", msgCyclingRadio)
	if len(cycling) != 1 || cycling[0]["device"] != "en0" || cycling[0]["attempt"] != 1 || cycling[0]["lost_for"] != "0s" {
		t.Fatalf("cycling records = %+v, want one naming the device, the attempt and the time lost", cycling)
	}
}

func TestTickRejoinsAKnownNetworkTheRadioCycleBringsBack(t *testing.T) {
	// Arrange: the read after the radio cycle finds a network whose name macOS
	// withholds.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(true, redactedName)})

	// Act.
	h.c.tick(context.Background())

	// Assert.
	if h.r.count(argJoin) != 0 {
		t.Fatal("the hotspot was joined though a known network came back")
	}
	rejoined := h.failoverRecords("info", "a network macOS knows was joined after the Wi-Fi radio was cycled")
	if len(rejoined) != 1 || rejoined[0]["attempts"] != 1 || rejoined[0]["network_name"] != "" {
		t.Fatalf("rejoined records = %+v, want one after the first attempt", rejoined)
	}
	if _, waiting := h.c.failoverRetryIn(); waiting || !h.c.failover.lost.IsZero() {
		t.Fatalf("failover = %+v, want it ended with nothing waiting", h.c.failover)
	}
}

func TestTickRecordsTheSettleAfterARadioCycleThatJoinedNothing(t *testing.T) {
	cases := []struct {
		name      string
		radio     []answer
		wantPower string
		wantCause string
	}{
		{name: "the radio came back on", radio: []answer{{out: radioPowered}}, wantPower: "on"},
		{name: "the radio stayed off", radio: []answer{{out: radioPowered}, {out: radioUnpowered}}, wantPower: "off"},
		{name: "the radio's power cannot be read", radio: []answer{{out: radioPowered}, {out: "denied", code: 1}},
			wantPower: "unreadable", wantCause: "/t/networksetup exited 1: denied"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the first power read is the loss's record, the second the
			// settle's.
			h := newHarness(t).standing(pmsetOn, summary(false, ""))
			h.r.on(argRadioPower, tc.radio...)

			// Act.
			h.c.tick(context.Background())

			// Assert.
			settles := h.failoverRecords("info", msgSettling)
			if len(settles) != 1 {
				t.Fatalf("settle records = %+v, want one", settles)
			}
			got := settles[0]
			if got["radio_power"] != tc.wantPower || got["settle"] != "20s" || got["wifi"] != "not_joined" ||
				(tc.wantCause != "" && got["radio_power_cause"] != tc.wantCause) {
				t.Fatalf("settle record = %+v, want radio %s (%q), the 20s settle and Wi-Fi not joined", got, tc.wantPower, tc.wantCause)
			}
		})
	}
}

func TestTickJoinsNoHotspotWhileTheKnownNetworkSettles(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.c.tick(context.Background())

	// Act.
	h.tickAt(knownNetworkSettle - time.Second)

	// Assert.
	if h.r.count(argJoin) != 0 || h.r.count(argRadioOff) != 1 {
		t.Fatalf("joins = %d and radio cycles = %d, want neither again before the settle passes", h.r.count(argJoin), h.r.count(argRadioOff))
	}
	if wait, ok := h.c.failoverRetryIn(); !ok || wait != time.Second {
		t.Fatalf("failoverRetryIn() = (%s, %t), want the settle's last second waiting", wait, ok)
	}
}

func TestTickRecordsAKnownNetworkMacOSJoinedDuringTheSettle(t *testing.T) {
	// Arrange: the radio cycled and nothing joined at once; a later read finds
	// a network.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(false, "")}, answer{out: summary(true, "")})
	h.c.tick(context.Background())

	// Act.
	h.tickAt(9 * time.Second)

	// Assert.
	if h.r.count(argJoin) != 0 {
		t.Fatal("the hotspot was joined though macOS joined a known network")
	}
	down := h.failoverRecords("info", msgStoodDown)
	if len(down) != 1 || down[0]["awaiting_known_network"] != true || down[0]["attempts"] != 1 || down[0]["wifi"] != "joined" {
		t.Fatalf("stand-down records = %+v, want one naming the settle it ended", down)
	}
}

func TestTickJoinsTheHotspotAtOnceWhenTheRadioCannotBeCycled(t *testing.T) {
	cases := []struct {
		name      string
		off, on   answer
		wantCause string
	}{
		{name: "the radio refuses to power off", off: answer{out: "denied", code: 1}, on: answer{},
			wantCause: "/t/networksetup exited 1: denied"},
		{name: "the radio refuses to power on", off: answer{}, on: answer{out: "denied", code: 1},
			wantCause: "/t/networksetup exited 1: denied"},
		{name: "both refusals are named", off: answer{out: "no", code: 1}, on: answer{out: "denied", code: 1},
			wantCause: "/t/networksetup exited 1: no; /t/networksetup exited 1: denied"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t).standing(pmsetOn, summary(false, ""))
			h.r.on(argRadioOff, tc.off)
			h.r.on(argRadioOn, tc.on)
			h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})

			// Act: two attempts, five seconds apart.
			h.c.tick(context.Background())
			h.tickAt(5 * time.Second)

			// Assert.
			if h.r.count(argJoin) != 2 {
				t.Fatalf("joins = %d, want one per attempt with no settle", h.r.count(argJoin))
			}
			warns := h.failoverRecords("warn", msgRadioRefused)
			if len(warns) != 1 || warns[0]["cause"] != tc.wantCause || warns[0]["device"] != "en0" {
				t.Fatalf("radio WARN records = %+v, want one naming %q", warns, tc.wantCause)
			}
			joining := h.failoverRecords("info", msgJoiningHotspot)
			if len(joining) != 2 || joining[0]["known_network"] != knownRadioFailed {
				t.Fatalf("joining records = %+v, want each naming the radio cycle that failed", joining)
			}
		})
	}
}

func TestTickWithNoWifiInterfaceCyclesNoRadio(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.r.on(argPmsetRead, answer{out: pmsetOn})
	h.r.on(argPorts, answer{out: noWifi})

	// Act.
	h.c.tick(context.Background())

	// Assert.
	if h.r.count(argRadioOff) != 0 {
		t.Fatal("a radio was cycled on a machine with no Wi-Fi interface")
	}
	joining := h.failoverRecords("info", msgJoiningHotspot)
	if len(joining) != 1 || joining[0]["known_network"] != knownNoInterface {
		t.Fatalf("joining records = %+v, want one naming no Wi-Fi interface", joining)
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

func TestTickRetriesAFailedAttemptFiveSecondsLaterAndWarnsOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})

	// Act: two attempts, each a radio cycle and the hotspot after the settle.
	h.c.tick(context.Background())
	h.tickAt(knownNetworkSettle)
	h.tickAt(knownNetworkSettle + 5*time.Second)
	h.tickAt(2*knownNetworkSettle + 5*time.Second)

	// Assert.
	if h.r.count(argJoin) != 2 || h.r.count(argRadioOff) != 2 {
		t.Fatalf("joins = %d and radio cycles = %d, want two attempts five seconds apart", h.r.count(argJoin), h.r.count(argRadioOff))
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
	h.tickAt(knownNetworkSettle)
	h.tickAt(knownNetworkSettle + 5*time.Second)
	h.tickAt(2*knownNetworkSettle + 5*time.Second)

	// Assert.
	if n := len(h.records("warn", opFailover)); n != 2 {
		t.Fatalf("failover WARN records = %d, want one per distinct cause", n)
	}
}

func TestTickRecordsTheFailoverStandingDown(t *testing.T) {
	// Arrange: an attempt that did not take, then a network joined by other means.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(false, "")},
		answer{out: summary(false, "")}, answer{out: summary(true, "Home")})
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	h.c.tick(context.Background())
	h.tickAt(knownNetworkSettle)

	// Act.
	h.c.tick(context.Background())

	// Assert.
	down := h.failoverRecords("info", msgStoodDown)
	if len(down) != 1 || down[0]["was"] != "Could not find network Test Hotspot." || down[0]["wifi"] != "joined" ||
		down[0]["network_name"] != "Home" || down[0]["awaiting_known_network"] != false {
		t.Fatalf("stand-down records = %+v, want one naming the cause it ended and the network joined", down)
	}
	if h.c.failoverCause != "" || h.c.knownCause != "" {
		t.Fatalf("causes = (%q, %q), want both cleared", h.c.failoverCause, h.c.knownCause)
	}
}

func TestTickWithNoHotspotConfiguredRecordsTheFaultOnce(t *testing.T) {
	// Arrange.
	h := newHarnessFor(t, "").standing(pmsetOn, summary(false, ""))

	// Act: two attempts.
	h.c.tick(context.Background())
	h.tickAt(knownNetworkSettle)
	h.tickAt(knownNetworkSettle + 5*time.Second)
	h.tickAt(2*knownNetworkSettle + 5*time.Second)

	// Assert.
	if h.r.count(argJoin) != 0 || h.r.count(argPreferred) != 0 {
		t.Fatal("a join was attempted with no hotspot configured")
	}
	errs := h.records("error", opFailover)
	if len(errs) != 1 {
		t.Fatalf("failover ERROR records = %+v, want one", errs)
	}
	cause, _ := errs[0].Context["cause"].(string)
	if errs[0].Context["env"] != EnvHotspot || !strings.Contains(cause, usersetup.Doc) || errs[0].Context["known_network"] != knownSettled {
		t.Fatalf("ERROR context = %+v, want the variable, the user guide and the settle that passed", errs[0].Context)
	}
}

func TestTickWithNoHotspotConfiguredStillCyclesTheRadioOnTheSchedule(t *testing.T) {
	// Arrange.
	h := newHarnessFor(t, "").standing(pmsetOn, summary(false, ""))
	h.c.tick(context.Background())
	h.tickAt(knownNetworkSettle)

	// Act.
	h.tickAt(knownNetworkSettle + 5*time.Second)

	// Assert.
	if n := h.r.count(argRadioOff); n != 2 {
		t.Fatalf("radio cycles = %d, want the known networks tried again five seconds after the attempt", n)
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

func TestRunJoinsTheHotspotOnceTheSettleAfterTheNetworkChangeThatLosesWifiPasses(t *testing.T) {
	// Arrange: Refresh and Run's opening read see the hotspot joined.
	h := newHarness(t).standing(pmsetOn, summary(true, hotspot))
	h.r.on(argSummary, answer{out: summary(true, hotspot)}, answer{out: summary(true, hotspot)},
		answer{out: summary(false, "")}, answer{out: summary(false, "")}, answer{out: summary(false, "")},
		answer{out: summary(true, hotspot)})
	h.r.on(argJoin, answer{})
	stop := h.runController(t)
	defer stop()

	// Act: Wi-Fi is lost, the radio cycles, and the settle passes.
	h.network.fire()
	lost := <-h.published
	settle := <-h.clock.afters
	h.clock.set(knownNetworkSettle)
	settle <- time.Unix(20, 0)
	rejoined := <-h.published

	// Assert.
	if wifiArm(lost) != "not_joined" || wifiArm(rejoined) != "joined" {
		t.Fatalf("standings = (%s, %s), want (not_joined, joined)", wifiArm(lost), wifiArm(rejoined))
	}
	if h.r.count(argJoin) != 1 || h.r.count(argRadioOff) != 1 {
		t.Fatalf("joins = %d and radio cycles = %d, want one of each", h.r.count(argJoin), h.r.count(argRadioOff))
	}
}

func TestRunStandsDownWhenMacOSRejoinsAKnownNetworkDuringTheSettle(t *testing.T) {
	// Arrange: Refresh and Run's opening read see a network joined.
	h := newHarness(t).standing(pmsetOn, summary(true, ""))
	h.r.on(argSummary, answer{out: summary(true, "")}, answer{out: summary(true, "")},
		answer{out: summary(false, "")}, answer{out: summary(false, "")}, answer{out: summary(true, "")})
	stop := h.runController(t)
	defer stop()
	h.network.fire()
	<-h.published
	<-h.clock.afters
	<-h.network.taken

	// Act: the network change of macOS's own auto-join.
	h.network.fire()
	rejoined := <-h.published

	// Assert.
	if wifiArm(rejoined) != "joined" || h.r.count(argJoin) != 0 {
		t.Fatalf("wifi = %s with %d joins, want joined with no hotspot join", wifiArm(rejoined), h.r.count(argJoin))
	}
}

func TestRunRetriesAFailedJoinWhenItsRetryIsDue(t *testing.T) {
	// Arrange: Run's opening read finds Wi-Fi lost and its join fails.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(false, "")},
		answer{out: summary(false, "")}, answer{out: summary(true, hotspot)})
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."}, answer{})
	stop := h.runController(t)
	defer stop()
	retry := <-h.clock.afters

	// Act: the retry falls due.
	h.clock.set(5 * time.Second)
	retry <- time.Unix(5, 0)
	got := <-h.published

	// Assert.
	if wifiArm(got) != "joined" {
		t.Fatalf("wifi after the retry = %s, want joined", wifiArm(got))
	}
}

func TestFailoverRetryInAnswersWhatIsWaiting(t *testing.T) {
	lost := &agentreplv1.PersistentWifiState{
		Mode: &agentreplv1.PersistentWifiState_On{On: &agentreplv1.PersistentWifiModeOn{}},
		Wifi: &agentreplv1.PersistentWifiState_NotJoined{NotJoined: &agentreplv1.PersistentWifiNotJoined{}},
	}
	joined := &agentreplv1.PersistentWifiState{
		Mode: &agentreplv1.PersistentWifiState_On{On: &agentreplv1.PersistentWifiModeOn{}},
		Wifi: &agentreplv1.PersistentWifiState_Joined{Joined: &agentreplv1.PersistentWifiJoined{}},
	}
	waiting := failoverSchedule{lost: time.Unix(0, 0), next: time.Unix(5, 0), every: 5 * time.Second, attempts: 1}
	cases := []struct {
		name     string
		last     *agentreplv1.PersistentWifiState
		schedule failoverSchedule
		wantWait time.Duration
		wantOK   bool
	}{
		{name: "a failed join waits for its next try", last: lost, schedule: waiting, wantWait: 5 * time.Second, wantOK: true},
		{name: "nothing was lost", last: lost, schedule: failoverSchedule{}, wantOK: false},
		{name: "a network is joined again", last: joined, schedule: waiting, wantOK: false},
		{name: "nothing was read yet", last: nil, schedule: waiting, wantOK: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.c.last, h.c.failover = tc.last, tc.schedule

			// Act.
			wait, ok := h.c.failoverRetryIn()

			// Assert.
			if wait != tc.wantWait || ok != tc.wantOK {
				t.Fatalf("failoverRetryIn() = (%s, %t), want (%s, %t)", wait, ok, tc.wantWait, tc.wantOK)
			}
		})
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
	}{
		{name: "at the loss", lost: 0, every: 5 * time.Second},
		{name: "just short of five minutes", lost: 5*time.Minute - time.Second, every: 5 * time.Second},
		{name: "at five minutes", lost: 5 * time.Minute, every: time.Minute},
		{name: "at thirty minutes", lost: 30 * time.Minute, every: time.Minute},
		{name: "at three hours", lost: 3 * time.Hour, every: time.Minute},
		{name: "a day on", lost: 24 * time.Hour, every: time.Minute},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			every := failoverRetryEvery(tc.lost)

			// Assert.
			if every != tc.every {
				t.Fatalf("failoverRetryEvery(%s) = %s, want %s", tc.lost, every, tc.every)
			}
		})
	}
}

func TestTickWaitsForTheRetryInterval(t *testing.T) {
	cases := []struct {
		name     string
		failAt   time.Duration
		beatAt   time.Duration
		attempts int
	}{
		{name: "the first tier's beat before five seconds", failAt: knownNetworkSettle, beatAt: knownNetworkSettle + 4*time.Second, attempts: 1},
		{name: "the first tier's beat at five seconds", failAt: knownNetworkSettle, beatAt: knownNetworkSettle + 5*time.Second, attempts: 2},
		{name: "the second tier's beat before a minute", failAt: 5 * time.Minute, beatAt: 5*time.Minute + 55*time.Second, attempts: 2},
		{name: "the second tier's beat at a minute", failAt: 5 * time.Minute, beatAt: 6 * time.Minute, attempts: 3},
		{name: "a beat before a minute three hours on", failAt: 3 * time.Hour, beatAt: 3*time.Hour + 59*time.Second, attempts: 2},
		{name: "a beat at a minute three hours on", failAt: 3 * time.Hour, beatAt: 3*time.Hour + time.Minute, attempts: 3},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: Wi-Fi lost at the epoch and an attempt that failed after
			// the settle, then one more attempt failing at failAt.
			h := newHarness(t).standing(pmsetOn, summary(false, ""))
			h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
			h.failAttemptAt(0)
			if tc.failAt > knownNetworkSettle {
				h.failAttemptAt(tc.failAt - knownNetworkSettle)
			}

			// Act.
			h.tickAt(tc.beatAt)

			// Assert: an attempt starts with its radio cycle.
			if n := h.r.count(argRadioOff); n != tc.attempts {
				t.Fatalf("attempts = %d, want %d", n, tc.attempts)
			}
		})
	}
}

func TestTickRecordsEachBackOffOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	h.failAttemptAt(0)
	h.failAttemptAt(knownNetworkSettle + 5*time.Second)

	// Act.
	h.failAttemptAt(5*time.Minute - knownNetworkSettle)
	h.failAttemptAt(6 * time.Minute)
	h.failAttemptAt(30*time.Minute - knownNetworkSettle)
	h.failAttemptAt(3 * time.Hour)

	// Assert.
	var backOffs []string
	for _, r := range h.failoverRecords("info", msgBackOff) {
		backOffs = append(backOffs, r["retry_every"].(string))
	}
	if len(backOffs) != 1 || backOffs[0] != "1m0s" {
		t.Fatalf("back-off INFO intervals = %v, want [1m0s]: no tier after the minute", backOffs)
	}
}

func TestTickKeepsTryingEveryMinuteForAsLongAsWifiStaysLost(t *testing.T) {
	// Arrange: attempts failing past five minutes, then hours later.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	h.failAttemptAt(0)
	h.failAttemptAt(5*time.Minute - knownNetworkSettle)
	h.failAttemptAt(10 * time.Hour)

	// Act.
	h.tickAt(10*time.Hour + knownNetworkSettle + time.Minute - time.Second)
	waitingBefore := h.r.count(argRadioOff)
	h.tickAt(10*time.Hour + knownNetworkSettle + time.Minute)

	// Assert.
	if waitingBefore != 3 || h.r.count(argRadioOff) != 4 {
		t.Fatalf("attempts = (%d before the minute, %d at it), want (3, 4): one a minute ten hours on", waitingBefore, h.r.count(argRadioOff))
	}
	if n := len(h.records("warn", opFailover)); n != 1 {
		t.Fatalf("failover WARN records = %d, want the one hotspot cause and no give-up", n)
	}
}

func TestTickAfterAJoinTookRestartsTheScheduleOnTheNextLoss(t *testing.T) {
	// Arrange: a failed attempt, then one whose hotspot join took, then Wi-Fi
	// lost again.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(false, "")},
		answer{out: summary(false, "")}, answer{out: summary(false, "")}, answer{out: summary(false, "")},
		answer{out: summary(false, "")}, answer{out: summary(true, hotspot)}, answer{out: summary(false, "")})
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."}, answer{})
	h.failAttemptAt(0)
	h.failAttemptAt(10 * time.Minute)

	// Act.
	h.tickAt(10*time.Minute + knownNetworkSettle + 5*time.Second)

	// Assert.
	var attempts []any
	for _, r := range h.failoverRecords("info", msgCyclingRadio) {
		attempts = append(attempts, r["attempt"])
	}
	if len(attempts) != 3 || attempts[2] != 1 {
		t.Fatalf("radio cycle attempts = %v, want the new loss's cycle at once, counted as its first", attempts)
	}
}

// The failover's record messages a test picks records by.
const (
	msgCyclingRadio   = "Wi-Fi was lost with persistent wifi on; cycling the Wi-Fi radio so macOS rejoins a network it knows"
	msgSettling       = "the Wi-Fi radio was cycled and no network is joined yet; the hotspot is joined unless macOS joins a network it knows within the settle"
	msgRadioRefused   = "the Wi-Fi radio could not be cycled to rejoin a network macOS knows; the hotspot is joined at once"
	msgJoiningHotspot = "no network macOS knows came back; joining the hotspot"
	msgStoodDown      = "the Wi-Fi failover stood down: the mode is off or a network is joined"
	msgBackOff        = "the Wi-Fi failover backs off: it is now tried less often"
)

// failoverRecords answers the contexts of the failover's records at level
// carrying msg.
func (h *harness) failoverRecords(level, msg string) []map[string]any {
	var out []map[string]any
	for _, r := range h.records(level, opFailover) {
		if r.Message == msg {
			out = append(out, r.Context)
		}
	}
	return out
}

// lossRecords answers the failover's records of a loss (recordLossLocked).
func (h *harness) lossRecords() []map[string]any {
	return h.failoverRecords("info", "Wi-Fi was lost with persistent wifi on")
}

// tickAt moves the clock to d past the epoch and runs one beat.
func (h *harness) tickAt(d time.Duration) {
	h.clock.set(d)
	h.c.tick(context.Background())
}

// failAttemptAt runs one failover attempt starting at d: the beat that cycles
// the radio, and the beat after the settle that joins the hotspot, which the
// test scripts to fail.
func (h *harness) failAttemptAt(d time.Duration) {
	h.tickAt(d)
	h.tickAt(d + knownNetworkSettle)
}

func TestTickRecordsWhatTheMachineStoodAtWhenWifiWasLost(t *testing.T) {
	cases := []struct {
		name      string
		ports     string
		radio     answer
		wantPower string
		wantCause string
	}{
		{name: "a powered radio", ports: ports, radio: answer{out: radioPowered}, wantPower: "on"},
		{name: "a radio macOS powered down", ports: ports, radio: answer{out: radioUnpowered}, wantPower: "off"},
		{name: "a radio power that cannot be read names its cause", ports: ports, radio: answer{out: "denied", code: 1},
			wantPower: "unreadable", wantCause: "/t/networksetup exited 1: denied"},
		{name: "a machine with no Wi-Fi interface has no radio to ask", ports: noWifi, wantPower: "no_wifi_interface"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the lid closed into the quiet state, its display asleep, in
			// Low Power Mode.
			h := newHarness(t).standing(pmsetIn(true, energyModeLowPower), summary(false, ""))
			h.r.on(argPorts, answer{out: tc.ports})
			h.r.on(argRadioPower, tc.radio)
			h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
			h.c.lid, h.c.quiet, h.c.displayAsleep = lidFact{kind: lidClosed}, true, true
			h.r.on(argPowerSource, answer{out: onBattery})

			// Act.
			h.c.tick(context.Background())

			// Assert.
			losses := h.lossRecords()
			if len(losses) != 1 {
				t.Fatalf("loss records = %+v, want one", losses)
			}
			got := losses[0]
			if got["radio_power"] != tc.wantPower || (tc.wantCause != "") != (got["radio_power_cause"] != nil) ||
				(tc.wantCause != "" && got["radio_power_cause"] != tc.wantCause) {
				t.Fatalf("loss record radio = (%v, %v), want (%s, %q)", got["radio_power"], got["radio_power_cause"], tc.wantPower, tc.wantCause)
			}
			if got["lid"] != "closed" || got["quiet"] != true || got["display_asleep"] != true ||
				got["energy_mode"] != energyModeLowPower || got["link_active"] != false {
				t.Fatalf("loss record = %+v, want the closed lid, the quiet state, the asleep display, Low Power Mode and the inactive link", got)
			}
		})
	}
}

func TestTickRecordsTheLinkSummaryOfTheLoss(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})

	// Act.
	h.c.tick(context.Background())

	// Assert.
	losses := h.lossRecords()
	if len(losses) != 1 || losses[0]["link_summary"] != strings.TrimSpace(summary(false, "")) || losses[0]["device"] != "en0" {
		t.Fatalf("loss records = %+v, want ipconfig's summary of en0", losses)
	}
}

func TestTickRecordsALossOncePerLoss(t *testing.T) {
	// Arrange: lost, still lost a retry later, joined, then lost again.
	h := newHarness(t).standing(pmsetOn, summary(false, ""))
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(false, "")}, answer{out: summary(false, "")},
		answer{out: summary(true, "Home")}, answer{out: summary(false, "")})
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})

	// Act.
	h.c.tick(context.Background())
	h.clock.set(5 * time.Second)
	h.c.tick(context.Background())
	h.clock.set(10 * time.Second)
	h.c.tick(context.Background())
	h.clock.set(15 * time.Second)
	h.c.tick(context.Background())

	// Assert.
	if n := len(h.lossRecords()); n != 2 {
		t.Fatalf("loss records = %d, want one per loss", n)
	}
}
