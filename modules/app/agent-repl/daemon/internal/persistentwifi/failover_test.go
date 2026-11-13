package persistentwifi

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// argJoinNamed is one `networksetup -setairportnetwork en0 <name>` call.
func argJoinNamed(name string) string { return "/t/networksetup -setairportnetwork en0 " + name }

// lostClosed is a harness whose lid is closed into the quiet state with the
// mode on and Wi-Fi joined to nothing: the one standing the failover acts on.
func lostClosed(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t).standing(pmsetOn, summary(false, "")).withLid(lidClosed)
	h.c.quiet = true
	return h
}

// joinCalls answers every network join and radio power call, in order.
func (h *harness) joinCalls() []string {
	var out []string
	for _, c := range h.r.ran() {
		if strings.Contains(c, "-setairportnetwork") || strings.Contains(c, "-setairportpower") {
			out = append(out, c)
		}
	}
	return out
}

func TestTheFailoverNeverActsWithTheLidOpen(t *testing.T) {
	// Arrange: Wi-Fi lost with the mode on, the lid open.
	h := newHarness(t).standing(pmsetOn, summary(false, "")).withLid(lidOpen)

	// Act: a beat at the loss and one long past the grace.
	h.tickAt(0)
	h.tickAt(10 * failoverLossGrace)

	// Assert.
	if calls := h.joinCalls(); len(calls) != 0 {
		t.Fatalf("join and radio calls = %v, want none with the lid open", calls)
	}
	if n := len(h.lossRecords()); n != 0 {
		t.Fatalf("loss records = %d, want none: the failover does not watch an open lid", n)
	}
}

func TestALossThatEndsWithinTheGraceDoesNothing(t *testing.T) {
	// Arrange: lost at the first beat, joined again by macOS ten seconds on.
	h := lostClosed(t)
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(true, "Home")})

	// Act.
	h.tickAt(0)
	h.tickAt(failoverLossGrace / 2)

	// Assert.
	if calls := h.joinCalls(); len(calls) != 0 {
		t.Fatalf("join and radio calls = %v, want none for a loss macOS ended itself", calls)
	}
	ended := h.failoverRecords("debug", "a Wi-Fi loss ended before the failover's grace, so nothing was done")
	if len(ended) != 1 || ended[0]["grace"] != failoverLossGrace.String() {
		t.Fatalf("debug records = %+v, want the short loss recorded once", ended)
	}
}

func TestTheFailoverWaitsOutItsGrace(t *testing.T) {
	cases := []struct {
		name     string
		at       time.Duration
		wantJoin bool
	}{
		{name: "just short of the grace", at: failoverLossGrace - time.Second, wantJoin: false},
		{name: "at the grace", at: failoverLossGrace, wantJoin: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: lost at the first beat.
			h := lostClosed(t)
			h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
			h.tickAt(0)

			// Act.
			h.tickAt(tc.at)

			// Assert.
			if got := h.r.count(argJoin) == 1; got != tc.wantJoin {
				t.Fatalf("hotspot joined = %t, want %t (calls %v)", got, tc.wantJoin, h.joinCalls())
			}
		})
	}
}

func TestSavedNetworksAreTriedInOrderAndStopAtTheFirstJoin(t *testing.T) {
	// Arrange: two saved networks before the hotspot; the first is out of
	// range, the second joins.
	h := lostClosed(t)
	h.r.on(argPreferred, answer{out: preferred("Home", "Office", hotspot)})
	h.r.on(argJoinNamed("Home"), answer{out: "Could not find network Home."})
	h.r.on(argJoinNamed("Office"), answer{})
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(false, "")}, answer{out: summary(true, "Office")})
	h.tickAt(0)

	// Act.
	h.tickAt(failoverLossGrace)

	// Assert.
	want := []string{argJoinNamed("Home"), argJoinNamed("Office")}
	if calls := h.joinCalls(); !slices.Equal(calls, want) {
		t.Fatalf("join calls = %v, want %v and no hotspot", calls, want)
	}
	rejoined := h.failoverRecords("info", "a saved Wi-Fi network was rejoined after Wi-Fi was lost")
	if len(rejoined) != 1 || rejoined[0]["index"] != 1 || rejoined[0]["saved"] != 2 {
		t.Fatalf("rejoined records = %+v, want the second of two saved networks", rejoined)
	}
	for _, level := range []string{"debug", "info", "warn", "error"} {
		for _, r := range h.records(level, opFailover) {
			for _, v := range r.Context {
				if s, ok := v.(string); ok && (strings.Contains(s, "Home") || strings.Contains(s, "Office")) {
					t.Fatalf("failover record %q names a saved network: %+v", r.Message, r.Context)
				}
			}
		}
	}
}

func TestTheHotspotIsTriedAfterTheSavedNetworksAndOnlyOnce(t *testing.T) {
	cases := []struct {
		name  string
		saved string
		want  []string
	}{
		{name: "a saved network out of range, then the hotspot", saved: preferred("Home", hotspot),
			want: []string{argJoinNamed("Home"), argJoin}},
		{name: "no saved network goes straight to the hotspot", saved: preferred(),
			want: []string{argJoin}},
		{name: "a hotspot saved first is still tried once, last", saved: preferred(hotspot, "Home"),
			want: []string{argJoinNamed("Home"), argJoin}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := lostClosed(t)
			h.r.on(argPreferred, answer{out: tc.saved})
			h.r.on(argJoinNamed("Home"), answer{out: "Could not find network Home."})
			h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
			h.tickAt(0)

			// Act.
			h.tickAt(failoverLossGrace)

			// Assert.
			if calls := h.joinCalls(); !slices.Equal(calls, tc.want) {
				t.Fatalf("join calls = %v, want %v", calls, tc.want)
			}
		})
	}
}

func TestAJoinedHotspotEndsTheFailover(t *testing.T) {
	// Arrange: no saved network but the hotspot, which joins.
	h := lostClosed(t)
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(false, "")}, answer{out: summary(true, hotspot)})
	h.r.on(argJoin, answer{})
	h.tickAt(0)

	// Act.
	h.tickAt(failoverLossGrace)

	// Assert.
	joined := h.failoverRecords("info", "the hotspot was joined after Wi-Fi was lost")
	if len(joined) != 1 || joined[0]["hotspot"] != hotspot || joined[0]["wifi"] != "joined" {
		t.Fatalf("joined records = %+v, want the hotspot joined", joined)
	}
	if h.c.failover.attempts != 0 || !h.c.failover.lost.IsZero() {
		t.Fatalf("failover = %+v, want it cleared", h.c.failover)
	}
}

func TestUnreadableSavedNetworksAreAnErrorOnceAndTheHotspotIsStillTried(t *testing.T) {
	// Arrange: the saved list cannot be read on either attempt.
	h := lostClosed(t)
	h.r.on(argPreferred, answer{out: "denied", code: 1})
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	h.tickAt(0)

	// Act: two attempts.
	h.tickAt(failoverLossGrace)
	h.tickAt(failoverLossGrace + 5*time.Second)

	// Assert.
	errs := h.failoverRecords("error", "the saved Wi-Fi networks could not be read to rejoin one; the hotspot is tried")
	if len(errs) != 1 || errs[0]["cause"] != "/t/networksetup exited 1: denied" {
		t.Fatalf("error records = %+v, want one naming the cause", errs)
	}
	if n := h.r.count(argJoin); n != 2 {
		t.Fatalf("hotspot joins = %d, want one per attempt", n)
	}
}

func TestTheFailoverNeverPowerCyclesTheRadio(t *testing.T) {
	// Arrange: nothing ever joins.
	h := lostClosed(t)
	h.r.on(argPreferred, answer{out: preferred("Home", hotspot)})
	h.r.on(argJoinNamed("Home"), answer{out: "Could not find network Home."})
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})

	// Act: the loss, then attempts over ten minutes.
	h.tickAt(0)
	for d := failoverLossGrace; d < 10*time.Minute; d += 5 * time.Second {
		h.tickAt(d)
	}

	// Assert.
	if h.r.count(argRadioOff) != 0 || h.r.count(argRadioOn) != 0 {
		t.Fatalf("radio power calls = %d off, %d on, want none", h.r.count(argRadioOff), h.r.count(argRadioOn))
	}
	if h.r.count(argJoin) == 0 {
		t.Fatal("no attempt was made")
	}
}

func TestTheLidOpeningStandsTheFailoverDown(t *testing.T) {
	// Arrange: one failed attempt with the lid closed.
	h := lostClosed(t)
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	h.tickAt(0)
	h.tickAt(failoverLossGrace)

	// Act: the lid opens, Wi-Fi still lost.
	h.c.quiet = false
	h.tickAt(failoverLossGrace + time.Minute)

	// Assert.
	if n := h.r.count(argJoin); n != 1 {
		t.Fatalf("hotspot joins = %d, want only the attempt made before the lid opened", n)
	}
	down := h.failoverRecords("info", "the Wi-Fi failover stood down: a network is joined, the lid opened or the mode is off")
	if len(down) != 1 || down[0]["quiet"] != false || down[0]["attempts"] != 1 {
		t.Fatalf("stand-down records = %+v, want one at the lid's opening", down)
	}
}

func TestAFailedAttemptIsRetriedOnTheScheduleAndWarnsOnce(t *testing.T) {
	// Arrange.
	h := lostClosed(t)
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	h.tickAt(0)
	h.tickAt(failoverLossGrace)

	// Act: a beat before the retry is due, then the beat it falls due.
	h.tickAt(failoverLossGrace + 4*time.Second)
	before := h.r.count(argJoin)
	h.tickAt(failoverLossGrace + 5*time.Second)

	// Assert.
	if before != 1 || h.r.count(argJoin) != 2 {
		t.Fatalf("joins = %d before the retry and %d after, want 1 and 2", before, h.r.count(argJoin))
	}
	warns := h.failoverRecords("warn", "the hotspot could not be joined after Wi-Fi was lost; the failover is tried again on the retry schedule")
	if len(warns) != 1 || warns[0]["cause"] != "Could not find network Test Hotspot." {
		t.Fatalf("warnings = %+v, want one for the standing cause", warns)
	}
}

func TestWithNoHotspotConfiguredTheFaultIsAnErrorOnce(t *testing.T) {
	// Arrange: no saved network, no hotspot.
	h := newHarnessFor(t, "").standing(pmsetOn, summary(false, "")).withLid(lidClosed)
	h.c.quiet = true
	h.r.on(argPreferred, answer{out: preferred()})
	h.tickAt(0)

	// Act: two attempts.
	h.tickAt(failoverLossGrace)
	h.tickAt(failoverLossGrace + 5*time.Second)

	// Assert.
	errs := h.failoverRecords("error", "Wi-Fi was lost with the lid closed, no saved network could be rejoined, and no hotspot is configured to join")
	if len(errs) != 1 || errs[0]["env"] != EnvHotspot {
		t.Fatalf("error records = %+v, want one naming the setting", errs)
	}
}

func TestTickAbandonedByItsCallerJoinsNothing(t *testing.T) {
	// Arrange: lost, the grace passed.
	h := lostClosed(t)
	h.tickAt(0)
	h.clock.set(failoverLossGrace)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	h.c.tick(ctx)

	// Assert.
	if calls := h.joinCalls(); len(calls) != 0 {
		t.Fatalf("an abandoned beat made calls %v", calls)
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
		quiet    bool
		schedule failoverSchedule
		wantWait time.Duration
		wantOK   bool
	}{
		{name: "a failed attempt waits for its next try", last: lost, quiet: true, schedule: waiting, wantWait: 5 * time.Second, wantOK: true},
		{name: "nothing was lost", last: lost, quiet: true, schedule: failoverSchedule{}, wantOK: false},
		{name: "a network is joined again", last: joined, quiet: true, schedule: waiting, wantOK: false},
		{name: "the lid is open", last: lost, quiet: false, schedule: waiting, wantOK: false},
		{name: "nothing was read yet", last: nil, quiet: true, schedule: waiting, wantOK: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.c.last, h.c.quiet, h.c.failover = tc.last, tc.quiet, tc.schedule

			// Act.
			wait, ok := h.c.failoverRetryIn()

			// Assert.
			if wait != tc.wantWait || ok != tc.wantOK {
				t.Fatalf("failoverRetryIn() = (%s, %t), want (%s, %t)", wait, ok, tc.wantWait, tc.wantOK)
			}
		})
	}
}

func TestRefreshNeverJoinsANetwork(t *testing.T) {
	// Arrange.
	h := lostClosed(t)

	// Act.
	h.c.Refresh(context.Background())

	// Assert.
	if calls := h.joinCalls(); len(calls) != 0 {
		t.Fatalf("Refresh made calls %v; the boot must never wait on a join", calls)
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
			h.c.lid, h.c.quiet, h.c.displayAsleep = lidFact{kind: lidClosed}, true, true

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
	h := lostClosed(t)

	// Act.
	h.c.tick(context.Background())

	// Assert.
	losses := h.lossRecords()
	if len(losses) != 1 || losses[0]["link_summary"] != strings.TrimSpace(summary(false, "")) || losses[0]["device"] != "en0" {
		t.Fatalf("loss records = %+v, want ipconfig's summary of en0", losses)
	}
}

func TestTickRecordsALossOncePerLoss(t *testing.T) {
	// Arrange: lost, still lost, joined, then lost again.
	h := lostClosed(t)
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(false, "")},
		answer{out: summary(true, "Home")}, answer{out: summary(false, "")})

	// Act.
	h.tickAt(0)
	h.tickAt(5 * time.Second)
	h.tickAt(10 * time.Second)
	h.tickAt(15 * time.Second)

	// Assert.
	if n := len(h.lossRecords()); n != 2 {
		t.Fatalf("loss records = %d, want one per loss", n)
	}
}

func TestAFailoverJoinRunsWithTheControllerLockFree(t *testing.T) {
	// Arrange: one saved network out of range, then the hotspot; each join
	// looks at the lock as it runs.
	h := lostClosed(t)
	h.r.on(argPreferred, answer{out: preferred("Home", hotspot)})
	h.r.on(argJoinNamed("Home"), answer{out: "Could not find network Home."})
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	var held []string
	for _, argv := range []string{argJoinNamed("Home"), argJoin} {
		h.r.whileRunning(argv, func(context.Context) {
			if !h.c.mu.TryLock() {
				held = append(held, argv)
				return
			}
			h.c.mu.Unlock()
		})
	}
	h.tickAt(0)

	// Act
	h.tickAt(failoverLossGrace)

	// Assert
	if len(held) != 0 {
		t.Fatalf("joins run with the controller lock held: %v", held)
	}
	if h.r.count(argJoinNamed("Home")) != 1 || h.r.count(argJoin) != 1 {
		t.Fatalf("join calls = %v, want the saved network then the hotspot", h.joinCalls())
	}
}

func TestALidOpeningMidAttemptEndsTheAttemptBeforeItsNextJoin(t *testing.T) {
	// Arrange: two saved networks before the hotspot; the lid opens while
	// the first is being joined.
	h := lostClosed(t)
	h.r.on(argPreferred, answer{out: preferred("Home", "Office", hotspot)})
	h.r.on(argJoinNamed("Home"), answer{out: "Could not find network Home."})
	h.r.on(argDisplayWake, answer{})
	h.r.whileRunning(argJoinNamed("Home"), func(context.Context) { h.hear(LidOpen) })
	h.tickAt(0)

	// Act
	h.tickAt(failoverLossGrace)

	// Assert
	if calls := h.joinCalls(); !slices.Equal(calls, []string{argJoinNamed("Home")}) {
		t.Fatalf("join calls = %v, want only the join in flight when the lid opened", calls)
	}
	if h.c.quiet {
		t.Fatal("the quiet state still stands after the lid opened mid-attempt")
	}
	abandoned := h.failoverRecords("info", "a Wi-Fi failover attempt was abandoned: the quiet state ended, the mode changed or the daemon stood down")
	if len(abandoned) != 1 || abandoned[0]["attempt"] != 1 || abandoned[0]["quiet"] != false {
		t.Fatalf("abandoned records = %+v, want one naming the attempt", abandoned)
	}
}

func TestALidOpeningCancelsTheJoinInFlight(t *testing.T) {
	// Arrange: the lid opens while the first saved network is being joined.
	h := lostClosed(t)
	h.r.on(argPreferred, answer{out: preferred("Home", hotspot)})
	h.r.on(argJoinNamed("Home"), answer{out: "Could not find network Home."})
	h.r.on(argDisplayWake, answer{})
	var joinErr error
	h.r.whileRunning(argJoinNamed("Home"), func(ctx context.Context) {
		h.hear(LidOpen)
		joinErr = ctx.Err()
	})
	h.tickAt(0)

	// Act
	h.tickAt(failoverLossGrace)

	// Assert
	if !errors.Is(joinErr, context.Canceled) {
		t.Fatalf("the join's context after the lid opened = %v, want it cancelled", joinErr)
	}
	h.c.mu.Lock()
	defer h.c.mu.Unlock()
	if h.c.failoverCancel != nil {
		t.Fatal("the attempt left its cancel behind once it ended")
	}
}

func TestEachFailoverAttemptRecordsItsStart(t *testing.T) {
	// Arrange
	h := lostClosed(t)
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	h.tickAt(0)

	// Act
	h.tickAt(failoverLossGrace)

	// Assert
	started := h.failoverRecords("info", "a Wi-Fi failover attempt starts")
	if len(started) != 1 || started[0]["attempt"] != 1 || started[0]["lost_for"] != failoverLossGrace.String() {
		t.Fatalf("start records = %+v, want the first attempt at the grace", started)
	}
}
