package persistentwifi

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"slices"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
)

// argJoinNamed is one `networksetup -setairportnetwork en0 <name>` call.
func argJoinNamed(name string) string { return "/t/networksetup -setairportnetwork en0 " + name }

// lostClosed is a harness whose lid is closed into the quiet state with the
// mode on and Wi-Fi joined to nothing: the one standing the failover acts on.
func lostClosed(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t).standing(pmsetOn, summary(false, "")).withLid(lidClosed)
	h.c.quiet = true
	// The hotspot is out of reach unless a test says otherwise.
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	return h
}

// argScan is the prefix of every Wi-Fi scan's argv; its answer file follows.
const argScan = "/t/open -W -n -g -a /t/AgentReplWifi.app --args scan --out "

// argAuthorize is the prefix of the helper's authorize launch; its answer
// file follows.
const argAuthorize = "/t/open -n -g -a /t/AgentReplWifi.app --args authorize --out "

// launchesAuthorize scripts the helper's authorize launch to succeed.
func (h *harness) launchesAuthorize() *harness {
	h.r.onPrefix(argAuthorize, func([]string) answer { return answer{} })
	return h
}

// sight scripts every scan to answer SCANS in turn, repeating the last: the
// helper's answer written to the file the scan names.
func (h *harness) sight(scans ...wifiScan) *harness {
	t := h.r.t
	i := 0
	h.r.onPrefix(argScan, func(argv []string) answer {
		scan := scans[min(i, len(scans)-1)]
		i++
		raw, err := json.Marshal(scan)
		if err != nil {
			t.Fatalf("marshal a scan: %v", err)
		}
		if err := os.WriteFile(argv[len(argv)-1], raw, 0o600); err != nil {
			t.Fatalf("write a scan's answer: %v", err)
		}
		return answer{}
	})
	return h
}

// inSight is an authorized scan seeing NAMES, joined to CURRENT.
func inSight(current string, names ...string) wifiScan {
	scan := wifiScan{Authorization: "authorized", Current: current}
	for i, n := range names {
		scan.Visible = append(scan.Visible, visibleNetwork{SSID: n, RSSI: -40 - i})
	}
	return scan
}

// scanCount answers how many scans ran.
func (h *harness) scanCount() int {
	n := 0
	for _, c := range h.r.ran() {
		if strings.HasPrefix(c, argScan) {
			n++
		}
	}
	return n
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

	// Act: a beat at the loss and one long after.
	h.tickAt(0)
	h.tickAt(10 * time.Minute)

	// Assert.
	if calls := h.joinCalls(); len(calls) != 0 || h.scanCount() != 0 {
		t.Fatalf("join and radio calls = %v and %d scans, want none with the lid open", calls, h.scanCount())
	}
	if n := len(h.lossRecords()); n != 0 {
		t.Fatalf("loss records = %d, want none: the failover does not watch an open lid", n)
	}
}

func TestTheFirstAttemptIsAtTheBeatThatFindsTheLoss(t *testing.T) {
	// Arrange: lost with the lid closed.
	h := lostClosed(t)

	// Act.
	h.tickAt(0)

	// Assert: no grace (owner ruling, 2026-11-14).
	if n := h.r.count(argJoin); n != 1 {
		t.Fatalf("hotspot joins at the loss = %d, want the first attempt at once", n)
	}
}

func TestOnlyKnownNetworksInSightAreTriedInOrderAndStopAtTheFirstJoin(t *testing.T) {
	// Arrange: three saved networks before the hotspot. Home is out of sight;
	// Office is in sight but will not join; Cafe joins; Stranger is in sight
	// but unknown.
	h := lostClosed(t)
	h.r.on(argPreferred, answer{out: preferred("Home", "Office", "Cafe", hotspot)})
	h.sight(inSight("", "Stranger", "Cafe", "Office"))
	h.r.on(argJoinNamed("Office"), answer{out: "Could not find network Office."})
	h.r.on(argJoinNamed("Cafe"), answer{})
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(true, "")})

	// Act.
	h.tickAt(0)

	// Assert.
	want := []string{argJoinNamed("Office"), argJoinNamed("Cafe")}
	if calls := h.joinCalls(); !slices.Equal(calls, want) {
		t.Fatalf("join calls = %v, want %v: only known networks in sight, in preference order, and no hotspot", calls, want)
	}
	joined := h.failoverRecords("info", "a known Wi-Fi network in sight was joined after Wi-Fi was lost")
	if len(joined) != 1 || joined[0]["index"] != 1 || joined[0]["visible_known"] != 2 || joined[0]["saved"] != 3 {
		t.Fatalf("joined records = %+v, want the second of two known networks in sight", joined)
	}
	tries := h.failoverRecords("info", "joined a Wi-Fi network, or tried to")
	if len(tries) != 2 || tries[0]["joined"] != false || tries[1]["joined"] != true || tries[1]["took_ms"] == nil {
		t.Fatalf("join records = %+v, want each join recorded with its outcome and duration", tries)
	}
}

// TestEveryJoinAttemptIsRecordedWithTheNetworkAndItsError pins the owner's
// 2026-11-18 ruling: a join record naming neither the network nor its error
// said nothing of two visible networks' repeated failures.
func TestEveryJoinAttemptIsRecordedWithTheNetworkAndItsError(t *testing.T) {
	// Arrange: Office will not join; Cafe joins.
	h := lostClosed(t)
	h.r.on(argPreferred, answer{out: preferred("Office", "Cafe", hotspot)})
	h.sight(inSight("", "Cafe", "Office"))
	h.r.on(argJoinNamed("Office"), answer{out: "Could not find network Office.", code: 1})
	h.r.on(argJoinNamed("Cafe"), answer{})
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(true, "")})

	// Act.
	h.tickAt(0)

	// Assert.
	tries := h.failoverRecords("info", "joined a Wi-Fi network, or tried to")
	if len(tries) != 2 || tries[0]["network"] != "Office" || tries[1]["network"] != "Cafe" {
		t.Fatalf("join records = %+v, want each naming its network", tries)
	}
	if cause, _ := tries[0]["error"].(string); !strings.Contains(cause, "Could not find network Office.") {
		t.Fatalf("failed join record = %+v, want its error", tries[0])
	}
	if _, has := tries[1]["error"]; has {
		t.Fatalf("joined record = %+v, want no error", tries[1])
	}
}

func TestTheHotspotIsTriedAfterTheKnownNetworksInSightAndOnlyOnce(t *testing.T) {
	cases := []struct {
		name   string
		saved  string
		inView []string
		want   []string
	}{
		{name: "a known network in sight that will not join, then the hotspot", saved: preferred("Home", hotspot),
			inView: []string{"Home"}, want: []string{argJoinNamed("Home"), argJoin}},
		{name: "a known network out of sight is never tried", saved: preferred("Home", hotspot),
			inView: []string{"Stranger"}, want: []string{argJoin}},
		{name: "no saved network goes straight to the hotspot", saved: preferred(),
			want: []string{argJoin}},
		{name: "a hotspot saved first is still tried once, last", saved: preferred(hotspot, "Home"),
			inView: []string{"Home", hotspot}, want: []string{argJoinNamed("Home"), argJoin}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := lostClosed(t)
			h.r.on(argPreferred, answer{out: tc.saved})
			h.sight(inSight("", tc.inView...))
			h.r.on(argJoinNamed("Home"), answer{out: "Could not find network Home."})

			// Act.
			h.tickAt(0)

			// Assert.
			if calls := h.joinCalls(); !slices.Equal(calls, tc.want) {
				t.Fatalf("join calls = %v, want %v", calls, tc.want)
			}
		})
	}
}

func TestAScanThatNamesNoNetworkIsAnErrorOnceAndTheHotspotIsStillTried(t *testing.T) {
	cases := []struct {
		name      string
		scan      wifiScan
		wantCause string
	}{
		{name: "the helper is not allowed Location Services", scan: wifiScan{Authorization: "not_determined"},
			wantCause: unauthorizedScan("not_determined").Error()},
		{name: "the helper's scan failed", scan: wifiScan{Authorization: "authorized", Error: "the scan failed: busy"},
			wantCause: "the Wi-Fi helper's the scan failed: busy"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a saved network, and scans that name nothing on both attempts.
			h := lostClosed(t)
			h.r.on(argPreferred, answer{out: preferred("Home", hotspot)})
			h.sight(tc.scan).launchesAuthorize()

			// Act: two attempts.
			h.tickAt(0)
			h.tickAt(5 * time.Second)

			// Assert.
			errs := h.failoverRecords("error", "the visible Wi-Fi networks could not be read; only the hotspot can be joined")
			if len(errs) != 1 || errs[0]["cause"] != tc.wantCause {
				t.Fatalf("error records = %+v, want one naming %q", errs, tc.wantCause)
			}
			if h.r.count(argJoinNamed("Home")) != 0 || h.r.count(argJoin) != 2 {
				t.Fatalf("join calls = %v, want the hotspot once per attempt and no saved network", h.joinCalls())
			}
		})
	}
}

// TestAJoinedHotspotEndsTheFailoverAndIsKept pins the owner's 2026-11-18
// ruling: with the lid closed the hotspot is never left for a known network,
// so nothing is scheduled once it is joined.
func TestAJoinedHotspotEndsTheFailoverAndIsKept(t *testing.T) {
	// Arrange: no saved network but the hotspot, which joins.
	h := lostClosed(t)
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(true, hotspot)})
	h.r.on(argJoin, answer{})

	// Act.
	h.tickAt(0)

	// Assert.
	joined := h.failoverRecords("info", "the hotspot was joined after Wi-Fi was lost")
	if len(joined) != 1 || joined[0]["hotspot"] != hotspot || joined[0]["wifi"] != "joined" {
		t.Fatalf("joined records = %+v, want the hotspot joined", joined)
	}
	if h.c.failover.attempts != 0 || !h.c.failover.lost.IsZero() {
		t.Fatalf("failover = %+v, want it cleared", h.c.failover)
	}
	if in, waiting := h.c.failoverRetryIn(); waiting {
		t.Fatalf("failoverRetryIn = (%v, true), want nothing scheduled on the hotspot", in)
	}
}

func TestUnreadableSavedNetworksAreAnErrorOnceAndTheHotspotIsStillTried(t *testing.T) {
	// Arrange: the saved list cannot be read on either attempt.
	h := lostClosed(t)
	h.r.on(argPreferred, answer{out: "denied", code: 1})
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})

	// Act: two attempts.
	h.tickAt(0)
	h.tickAt(5 * time.Second)

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
	h.sight(inSight("", "Home"))

	// Act: the loss, then attempts over ten minutes.
	for d := time.Duration(0); d < 10*time.Minute; d += 5 * time.Second {
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

	// Act: the lid opens, Wi-Fi still lost.
	h.c.quiet = false
	h.tickAt(time.Minute)

	// Assert.
	if n := h.r.count(argJoin); n != 1 {
		t.Fatalf("hotspot joins = %d, want only the attempt made before the lid opened", n)
	}
	down := h.failoverRecords("info", "the Wi-Fi failover stood down: a network is joined, the lid opened or the mode is off")
	if len(down) != 1 || down[0]["quiet"] != false || down[0]["attempts"] != 1 {
		t.Fatalf("stand-down records = %+v, want one at the lid's opening", down)
	}
}

func TestAFailedAttemptIsRetriedOnTheScheduleAndRecordedOnceAsTransient(t *testing.T) {
	// Arrange.
	h := lostClosed(t)
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	h.tickAt(0)

	// Act: a beat before the retry is due, then the beat it falls due.
	h.tickAt(4 * time.Second)
	before := h.r.count(argJoin)
	h.tickAt(5 * time.Second)

	// Assert.
	if before != 1 || h.r.count(argJoin) != 2 {
		t.Fatalf("joins = %d before the retry and %d after, want 1 and 2", before, h.r.count(argJoin))
	}
	infos := h.failoverRecords("info", "the hotspot could not be joined after Wi-Fi was lost; the failover is tried again on the retry schedule")
	if len(infos) != 1 || infos[0]["cause"] != "Could not find network Test Hotspot." || infos[0][dlog.TransientKey] != true {
		t.Fatalf("records = %+v, want one transient INFO for the standing cause", infos)
	}
	if warns := h.failoverRecords("warn", "the hotspot could not be joined after Wi-Fi was lost; the failover is tried again on the retry schedule"); len(warns) != 0 {
		t.Fatalf("warnings = %+v, want none", warns)
	}
}

func TestWithNoHotspotConfiguredTheFaultIsAnErrorOnce(t *testing.T) {
	// Arrange: no saved network, no hotspot.
	h := newHarnessFor(t, "").standing(pmsetOn, summary(false, "")).withLid(lidClosed)
	h.c.quiet = true
	h.r.on(argPreferred, answer{out: preferred()})

	// Act: two attempts.
	h.tickAt(0)
	h.tickAt(5 * time.Second)

	// Assert.
	errs := h.failoverRecords("error", "Wi-Fi was lost with the lid closed, no saved network could be rejoined, and no hotspot is configured to join")
	if len(errs) != 1 || errs[0]["env"] != EnvHotspot {
		t.Fatalf("error records = %+v, want one naming the setting", errs)
	}
}

func TestTickAbandonedByItsCallerJoinsNothing(t *testing.T) {
	// Arrange: lost with the lid closed.
	h := lostClosed(t)
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
	h.sight(inSight("", "Home"))
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

	// Act
	h.tickAt(0)

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
	h.sight(inSight("", "Home", "Office"))
	h.r.on(argDisplayWake, answer{})
	h.r.whileRunning(argJoinNamed("Home"), func(context.Context) { h.hear(LidOpen) })

	// Act
	h.tickAt(0)

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
	h.sight(inSight("", "Home"))
	h.r.on(argDisplayWake, answer{})
	var joinErr error
	h.r.whileRunning(argJoinNamed("Home"), func(ctx context.Context) {
		h.hear(LidOpen)
		joinErr = ctx.Err()
	})

	// Act
	h.tickAt(0)

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

	// Act
	h.tickAt(0)

	// Assert
	started := h.failoverRecords("info", "a Wi-Fi failover attempt starts")
	if len(started) != 1 || started[0]["attempt"] != 1 || started[0]["lost_for"] != "0s" {
		t.Fatalf("start records = %+v, want the first attempt at the loss", started)
	}
}

// surveyRecords answers the failover's attempt surveys (surveyAttempt).
func (h *harness) surveyRecords() []map[string]any {
	return h.failoverRecords("info", "a Wi-Fi failover attempt's survey: the hotspot's sighting, Bluetooth, the lid, the display and the power")
}

// channel is a scan's channel number.
func channel(n int) *int { return &n }

func TestTheKnownStepTurnedOffGoesStraightToTheHotspot(t *testing.T) {
	// Arrange: a saved network in sight that would join, the step off.
	h := lostClosed(t)
	h.c.cfg.KnownJoinOff = true
	h.r.on(argPreferred, answer{out: preferred("Home", hotspot)})
	h.r.on(argJoinNamed("Home"), answer{})
	h.sight(inSight("", "Home"))

	// Act: two attempts.
	h.tickAt(0)
	h.tickAt(5 * time.Second)

	// Assert.
	if calls := h.joinCalls(); !slices.Equal(calls, []string{argJoin, argJoin}) {
		t.Fatalf("join calls = %v, want only the hotspot, once per attempt", calls)
	}
	off := h.failoverRecords("info", "the Wi-Fi failover's known-network step is turned off by the setting; each attempt joins only the hotspot")
	if len(off) != 1 || off[0]["env"] != EnvJoinKnown || off[0]["attempt"] != 1 {
		t.Fatalf("turned-off records = %+v, want one per loss naming the setting", off)
	}
}

func TestTheKnownStepTurnedOffIsRecordedOncePerLoss(t *testing.T) {
	// Arrange: lost, joined, then lost again, the step off.
	h := lostClosed(t)
	h.c.cfg.KnownJoinOff = true
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(false, "")},
		answer{out: summary(true, "Home")}, answer{out: summary(false, "")})

	// Act.
	h.tickAt(0)
	h.tickAt(5 * time.Second)
	h.tickAt(10 * time.Second)
	h.tickAt(15 * time.Second)

	// Assert.
	off := h.failoverRecords("info", "the Wi-Fi failover's known-network step is turned off by the setting; each attempt joins only the hotspot")
	if len(off) != 2 {
		t.Fatalf("turned-off records = %d, want one per loss", len(off))
	}
}

func TestEachAttemptsSurveyRecordsTheHotspotsSighting(t *testing.T) {
	cases := []struct {
		name string
		scan wifiScan
		want map[string]any
		gone []string
	}{
		{name: "a hotspot in sight under its curly apostrophe, with its strength and channel",
			scan: wifiScan{Authorization: "authorized", Visible: []visibleNetwork{
				{SSID: "Cafe", RSSI: -50}, {SSID: "Test’s Phone", RSSI: -61, Channel: channel(149)}}},
			want: map[string]any{"hotspot_sighting": "visible", "hotspot_ssid": "Test’s Phone", "hotspot_rssi": -61,
				"hotspot_channel": 149, "scan": "read", "visible": 2}},
		{name: "a hotspot out of sight",
			scan: inSight("", "Cafe"),
			want: map[string]any{"hotspot_sighting": "not_visible", "scan": "read", "visible": 1},
			gone: []string{"hotspot_rssi", "hotspot_channel"}},
		{name: "a scan that failed leaves the sighting unknown, naming why",
			scan: wifiScan{Authorization: "authorized", Error: "the scan failed: busy"},
			want: map[string]any{"hotspot_sighting": "unscanned", "scan": "failed",
				"scan_cause": "the Wi-Fi helper's the scan failed: busy"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the hotspot configured with a straight apostrophe.
			h := newHarnessFor(t, "Test's Phone").standing(pmsetIn(true, energyModeLowPower), summary(false, "")).withLid(lidClosed)
			h.c.quiet, h.c.displayAsleep = true, true
			h.r.on(argPreferred, answer{out: preferred()})
			h.r.on("/t/networksetup -setairportnetwork en0 Test's Phone", answer{out: "Could not find network Test's Phone."})
			h.sight(tc.scan)

			// Act: two attempts with the same cause.
			h.tickAt(0)
			h.tickAt(5 * time.Second)

			// Assert: one survey per attempt, never de-duplicated.
			surveys := h.surveyRecords()
			if len(surveys) != 2 {
				t.Fatalf("survey records = %+v, want one per attempt", surveys)
			}
			for i, got := range surveys {
				for k, v := range tc.want {
					if got[k] != v {
						t.Fatalf("survey %d[%s] = %v, want %v (record %+v)", i, k, got[k], v, got)
					}
				}
				for _, k := range tc.gone {
					if _, has := got[k]; has {
						t.Fatalf("survey %d carries %s: %+v", i, k, got)
					}
				}
				if got["attempt"] != i+1 || got["hotspot"] != "Test's Phone" || got["lid"] != "closed" || got["quiet"] != true ||
					got["display_asleep"] != true || got["power_source"] != "battery" || got["energy_mode"] != energyModeLowPower {
					t.Fatalf("survey %d = %+v, want the attempt, the hotspot, the lid, the display and the power", i, got)
				}
			}
		})
	}
}

func TestASurveyWithNoHotspotConfiguredSaysSo(t *testing.T) {
	// Act.
	got := hotspotSighting("", &wifiScan{})

	// Assert.
	if got["hotspot_sighting"] != "no_hotspot_configured" {
		t.Fatalf("hotspotSighting = %+v, want the hotspot named unconfigured", got)
	}
}

func TestEachAttemptsSurveyRecordsBluetooth(t *testing.T) {
	cases := []struct {
		name      string
		answer    answer
		hang      bool
		wantPower string
		wantCause string
	}{
		{name: "powered", answer: answer{out: bluetoothOn}, wantPower: "on"},
		{name: "powered down", answer: answer{out: bluetoothOff}, wantPower: "off"},
		{name: "a read that failed is unreadable with its cause", answer: answer{out: "denied", code: 1},
			wantPower: "unreadable", wantCause: "/t/system_profiler exited 1: denied"},
		{name: "an answer naming no state is unreadable with what it named",
			answer:    answer{out: `{"SPBluetoothDataType":[{"controller_properties":{"controller_state":"attrib_odd"}}]}`},
			wantPower: "unreadable", wantCause: `system_profiler named no Bluetooth power state: controller_state "attrib_odd"`},
		{name: "a read that outlives its bound is cut and unreadable", hang: true,
			wantPower: "unreadable", wantCause: "the Bluetooth read did not answer within 1ms (expected 640ms): context deadline exceeded"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := lostClosed(t)
			if tc.hang {
				h.c.bluetoothBound = time.Millisecond
				h.r.hangPrefix(argBluetooth)
			} else {
				h.r.on(argBluetooth, tc.answer)
			}

			// Act.
			h.tickAt(0)

			// Assert: the attempt still went on to the hotspot.
			surveys := h.surveyRecords()
			if len(surveys) != 1 || surveys[0]["bluetooth_power"] != tc.wantPower {
				t.Fatalf("survey records = %+v, want Bluetooth %s", surveys, tc.wantPower)
			}
			if cause, has := surveys[0]["bluetooth_power_cause"]; (tc.wantCause != "") != has || (has && cause != tc.wantCause) {
				t.Fatalf("survey Bluetooth cause = %v, want %q", cause, tc.wantCause)
			}
			if h.r.count(argJoin) != 1 {
				t.Fatalf("join calls = %v, want the hotspot tried after the survey", h.joinCalls())
			}
		})
	}
}

func TestEveryHotspotJoinRecordsItsExactOutcome(t *testing.T) {
	// Arrange: the hotspot will not join on the first attempt, then joins.
	h := lostClosed(t)
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."}, answer{})
	h.r.on(argSummary, answer{out: summary(false, "")}, answer{out: summary(false, "")},
		answer{out: summary(false, "")}, answer{out: summary(true, hotspot)})

	// Act.
	h.tickAt(0)
	h.tickAt(5 * time.Second)

	// Assert.
	tries := h.failoverRecords("info", "joined the hotspot, or tried to")
	if len(tries) != 2 {
		t.Fatalf("hotspot join records = %+v, want one per attempt", tries)
	}
	if tries[0]["outcome"] != "failed" || tries[0]["joined"] != false || tries[0]["error"] != "Could not find network Test Hotspot." ||
		tries[0]["network"] != hotspot || tries[0]["attempt"] != 1 || tries[0]["took_bound"] != false {
		t.Fatalf("failed join record = %+v, want the failure and its detail", tries[0])
	}
	if tries[1]["outcome"] != "joined" || tries[1]["joined"] != true || tries[1]["attempt"] != 2 {
		t.Fatalf("joined record = %+v, want the join", tries[1])
	}
	if _, has := tries[1]["error"]; has {
		t.Fatalf("joined record = %+v, want no error", tries[1])
	}
}

func TestEveryHotspotJoinIsRecordedWhileItsCauseIsRecordedOnce(t *testing.T) {
	// Arrange: the hotspot out of reach on three attempts.
	h := lostClosed(t)

	// Act.
	h.tickAt(0)
	h.tickAt(5 * time.Second)
	h.tickAt(10 * time.Second)

	// Assert.
	if n := len(h.failoverRecords("info", "joined the hotspot, or tried to")); n != 3 {
		t.Fatalf("hotspot join records = %d, want one per attempt", n)
	}
	if n := len(h.failoverRecords("info", "the hotspot could not be joined after Wi-Fi was lost; the failover is tried again on the retry schedule")); n != 1 {
		t.Fatalf("cause records = %d, want the standing cause once", n)
	}
}

func TestHotspotJoinRecordNamesAnOutcomeWithoutAJoin(t *testing.T) {
	// Arrange.
	outcome := &agentreplv1.UpdatePersistentWifiModeHotspot{Outcome: &agentreplv1.UpdatePersistentWifiModeHotspot_NoWifiInterface{
		NoWifiInterface: &agentreplv1.UpdatePersistentWifiModeHotspotNoWifiInterface{}}}

	// Act.
	got := hotspotJoinRecord("", 3, hotspot, outcome, 0, false)

	// Assert.
	if got["outcome"] != "no_wifi_interface" || got["joined"] != false || got["error"] != "no_wifi_interface" || got["attempt"] != 3 {
		t.Fatalf("hotspotJoinRecord = %+v, want the arm named as the outcome and the error", got)
	}
}

func TestJoinRecord(t *testing.T) {
	cases := []struct {
		name  string
		cause string
		want  map[string]any
	}{
		{name: "a join", want: map[string]any{"device": "en0", "network": "Home", "took_ms": int64(1500), "took_bound": false, "joined": true}},
		{name: "a failed join names its error", cause: "Could not find network Home.",
			want: map[string]any{"device": "en0", "network": "Home", "took_ms": int64(1500), "took_bound": false, "joined": false,
				"error": "Could not find network Home."}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := joinRecord("en0", "Home", 1500*time.Millisecond, false, tc.cause)

			// Assert.
			if len(got) != len(tc.want) {
				t.Fatalf("joinRecord = %+v, want %+v", got, tc.want)
			}
			for k, v := range tc.want {
				if got[k] != v {
					t.Fatalf("joinRecord[%s] = %v, want %v", k, got[k], v)
				}
			}
		})
	}
}

// TestBothJoinRecordsShareJoinRecordsShape pins that the saved-network and
// hotspot join records are both built on joinRecord: a hand-rolled record
// missing one of its keys fails here.
func TestBothJoinRecordsShareJoinRecordsShape(t *testing.T) {
	// Arrange: a saved network in sight that will not join, then the hotspot.
	h := lostClosed(t)
	h.r.on(argPreferred, answer{out: preferred("Home", hotspot)})
	h.r.on(argJoinNamed("Home"), answer{out: "Could not find network Home."})
	h.sight(inSight("", "Home"))

	// Act.
	h.tickAt(0)

	// Assert.
	known := h.failoverRecords("info", "joined a Wi-Fi network, or tried to")
	spot := h.failoverRecords("info", "joined the hotspot, or tried to")
	if len(known) != 1 || len(spot) != 1 {
		t.Fatalf("join records = %+v and %+v, want one of each", known, spot)
	}
	for k := range joinRecord("en0", "x", 0, false, "cause") {
		if _, has := known[0][k]; !has {
			t.Fatalf("saved-network join record %+v lacks %s", known[0], k)
		}
		if _, has := spot[0][k]; !has {
			t.Fatalf("hotspot join record %+v lacks %s", spot[0], k)
		}
	}
}
