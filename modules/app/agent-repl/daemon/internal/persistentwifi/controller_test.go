package persistentwifi

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/usersetup"
)

// harness is one controller over a fake runner.
type harness struct {
	c       *Controller
	r       *fakeRunner
	log     *dlog.TestLogger
	clock   *fakeClock
	changes []*agentreplv1.PersistentWifiState
	// published is told of every standing OnChange is handed, so a test
	// driving Run knows a read finished.
	published chan *agentreplv1.PersistentWifiState
	network   *fakeNetwork
	power     *fakePower
	exists  map[string]bool
	dims    *fakeLidDims
	lid     *fakeLidWatcher
}

// withLid stands the lid as heard, without a read: the state an Update then
// finds.
func (h *harness) withLid(kind lidKind) *harness {
	h.c.lid = lidFact{kind: kind}
	return h
}

func newHarness(t *testing.T) *harness {
	t.Helper()
	return newHarnessFor(t, hotspot)
}

// newHarnessFor is newHarness configured to join the named hotspot. The
// interface's saved networks list the fake hotspot unless a test says
// otherwise.
func newHarnessFor(t *testing.T, configured string) *harness {
	t.Helper()
	h := &harness{
		r: newFakeRunner(t), log: dlog.NewTestLogger(), clock: newFakeClock(),
		exists: map[string]bool{tools.WifiUtil: true, tools.Brightness: true},
		dims:   &fakeLidDims{}, lid: newFakeLidWatcher(),
		published: make(chan *agentreplv1.PersistentWifiState, 256),
		network:   newFakeNetwork(), power: newFakePower(),
	}
	c, err := New(Deps{
		Config: Config{Tools: tools, Hotspot: configured},
		Runner: h.r, Clock: h.clock, Log: h.log, Lid: h.lid, LidDims: h.dims,
		NetworkChanged: h.network.changed, PowerChanges: h.power.source,
		Exists: func(p string) bool { return h.exists[p] },
		OnChange: func(s *agentreplv1.PersistentWifiState) {
			h.changes = append(h.changes, s)
			h.published <- s
		},
	})
	if err != nil {
		t.Fatalf("New() error = %v", err)
	}
	h.c = c
	h.r.on(argPreferred, answer{out: preferred(hotspot)})
	return h
}

// standing scripts one read: the mode and the link.
func (h *harness) standing(pmset, link string) *harness {
	h.r.on(argPmsetRead, answer{out: pmset})
	h.r.on(argPorts, answer{out: ports})
	h.r.on(argSummary, answer{out: link})
	return h
}

// records answers the captured records at level for operation.
func (h *harness) records(level, operation string) []dlog.Record {
	return dlog.FilterRecords(h.log.Records(), dlog.RecordQuery{Level: level, Operation: operation})
}

func update(action string) *agentreplv1.UpdatePersistentWifiModeRequest {
	switch action {
	case "on":
		return &agentreplv1.UpdatePersistentWifiModeRequest{Action: &agentreplv1.UpdatePersistentWifiModeRequest_On{On: &agentreplv1.UpdatePersistentWifiModeOn{}}}
	case "off":
		return &agentreplv1.UpdatePersistentWifiModeRequest{Action: &agentreplv1.UpdatePersistentWifiModeRequest_Off{Off: &agentreplv1.UpdatePersistentWifiModeOff{}}}
	default:
		return &agentreplv1.UpdatePersistentWifiModeRequest{Action: &agentreplv1.UpdatePersistentWifiModeRequest_Toggle{Toggle: &agentreplv1.UpdatePersistentWifiModeToggle{}}}
	}
}

func TestNewRefusesAnIncompleteDeps(t *testing.T) {
	full := func() Deps {
		return Deps{Config: Config{Tools: tools, Hotspot: hotspot}, Runner: newFakeRunner(t), Clock: newFakeClock(), Log: dlog.NewTestLogger(),
			Lid: newFakeLidWatcher(), LidDims: &fakeLidDims{},
			NetworkChanged: newFakeNetwork().changed, PowerChanges: newFakePower().source}
	}
	cases := []struct {
		name   string
		break_ func(*Deps)
	}{
		{name: "no runner", break_: func(d *Deps) { d.Runner = nil }},
		{name: "no clock", break_: func(d *Deps) { d.Clock = nil }},
		{name: "no logger", break_: func(d *Deps) { d.Log = nil }},
		{name: "no lid watcher", break_: func(d *Deps) { d.Lid = nil }},
		{name: "no store for the pre-close brightness", break_: func(d *Deps) { d.LidDims = nil }},
		{name: "no network change signal", break_: func(d *Deps) { d.NetworkChanged = nil }},
		{name: "no power change signal", break_: func(d *Deps) { d.PowerChanges = nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			d := full()
			tc.break_(&d)

			// Act.
			_, err := New(d)

			// Assert.
			if err == nil {
				t.Fatal("New() = nil error, want a refusal")
			}
		})
	}
}

func TestRefreshPublishesTheStanding(t *testing.T) {
	cases := []struct {
		name               string
		pmset, ports, link string
		wantWifi, wantMode string
		wantName           string
	}{
		{name: "joined and on", pmset: pmsetOn, ports: ports, link: summary(true, "Home"), wantWifi: "joined", wantMode: "on", wantName: "Home"},
		{name: "a withheld name is joined with none", pmset: pmsetOff, ports: ports, link: summary(true, redactedName), wantWifi: "joined", wantMode: "off"},
		{name: "an inactive link is not joined", pmset: pmsetOn, ports: ports, link: summary(false, ""), wantWifi: "not_joined", wantMode: "on"},
		{name: "no Wi-Fi interface is not joined", pmset: pmsetOff, ports: noWifi, wantWifi: "not_joined", wantMode: "off"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.r.on(argPmsetRead, answer{out: tc.pmset})
			h.r.on(argPorts, answer{out: tc.ports})
			h.r.on(argSummary, answer{out: tc.link})

			// Act.
			h.c.Refresh(context.Background())

			// Assert.
			got, ok := h.c.Topic().Latest()
			if !ok {
				t.Fatal("Topic().Latest() published nothing")
			}
			if wifiArm(got) != tc.wantWifi || modeArm(got) != tc.wantMode || got.GetJoined().GetNetworkName() != tc.wantName {
				t.Fatalf("published (%s, %s, %q), want (%s, %s, %q)", wifiArm(got), modeArm(got),
					got.GetJoined().GetNetworkName(), tc.wantWifi, tc.wantMode, tc.wantName)
			}
			if len(h.changes) != 1 {
				t.Fatalf("OnChange called %d times, want 1", len(h.changes))
			}
		})
	}
}

func TestRefreshRecordsHowLongTheReadTook(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(true, "Home"))

	// Act.
	h.c.Refresh(context.Background())

	// Assert.
	changed := h.records("info", opProbe)
	if len(changed) != 1 {
		t.Fatalf("recorded %d probe records, want the one change", len(changed))
	}
	if _, ok := changed[0].Context["took_ms"]; !ok {
		t.Fatalf("the change record %+v carries no took_ms; the boot reads it before serving, so its cost must be said", changed[0].Context)
	}
}

func TestRefreshHandsAnUnchangedStandingToNobody(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(true, "Home"))
	h.c.Refresh(context.Background())

	// Act.
	h.c.Refresh(context.Background())

	// Assert.
	if len(h.changes) != 1 {
		t.Fatalf("OnChange called %d times, want 1 (the second read changed nothing)", len(h.changes))
	}
}

func TestRefreshLeavesAnUnreadableModeUnassignedAndRecordsItOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(true, "Home"))
	h.r.on(argPmsetRead, answer{out: "pmset: denied", code: 1})

	// Act.
	h.c.Refresh(context.Background())
	h.c.Refresh(context.Background())

	// Assert.
	got, _ := h.c.Topic().Latest()
	if modeArm(got) != "unknown" || wifiArm(got) != "joined" {
		t.Fatalf("published (%s, %s), want (joined, unknown)", wifiArm(got), modeArm(got))
	}
	errs := h.records("error", opProbe)
	if len(errs) != 1 || errs[0].Context["fact"] != "mode" {
		t.Fatalf("probe ERROR records = %+v, want one for the mode", errs)
	}
}

func TestRefreshLeavesAnUnspawnableLinkReadUnassigned(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.r.on(argPmsetRead, answer{out: pmsetOff})
	h.r.on(argPorts, answer{err: errors.New("no such file")})

	// Act.
	h.c.Refresh(context.Background())

	// Assert.
	got, _ := h.c.Topic().Latest()
	if wifiArm(got) != "unknown" {
		t.Fatalf("wifi = %s, want unknown", wifiArm(got))
	}
	errs := h.records("error", opProbe)
	if len(errs) != 1 || errs[0].Context["fact"] != "wifi" {
		t.Fatalf("probe ERROR records = %+v, want one for wifi", errs)
	}
}

func TestRefreshRecordsARecoveredFactAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(true, "Home"))
	h.r.on(argPmsetRead, answer{code: 1}, answer{out: pmsetOn})
	h.c.Refresh(context.Background())

	// Act.
	h.c.Refresh(context.Background())

	// Assert.
	var recovered bool
	for _, r := range h.records("info", opProbe) {
		if r.Context["fact"] == "mode" {
			recovered = true
		}
	}
	if !recovered {
		t.Fatal("no INFO record of the mode becoming readable again")
	}
}

// runController starts Run after Refresh and waits until its watches are
// taken; the returned stop cancels Run and answers its error.
func (h *harness) runController(t *testing.T) func() error {
	t.Helper()
	h.c.Refresh(context.Background())
	drain(h.published)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- h.c.Run(ctx) }()
	<-h.network.taken
	return func() error {
		cancel()
		return <-done
	}
}

// drain empties a buffered channel of what it holds now.
func drain[T any](ch chan T) {
	for {
		select {
		case <-ch:
		default:
			return
		}
	}
}

func TestRunReadsOnceAfterTakingItsWatches(t *testing.T) {
	// Arrange: the mode turned on between Refresh and Run.
	h := newHarness(t).standing(pmsetOff, summary(true, "Home"))
	h.r.on(argPmsetRead, answer{out: pmsetOff}, answer{out: pmsetOn})

	// Act.
	stop := h.runController(t)
	got := <-h.published

	// Assert.
	if modeArm(got) != "on" {
		t.Fatalf("mode after Run's opening read = %s, want on", modeArm(got))
	}
	if err := stop(); err != nil {
		t.Fatalf("Run() = %v, want nil on cancellation", err)
	}
}

func TestRunRereadsOnANetworkChange(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOff, summary(true, "Home"))
	h.r.on(argSummary, answer{out: summary(true, "Home")}, answer{out: summary(true, "Home")}, answer{out: summary(true, "Office")})
	stop := h.runController(t)
	defer stop()

	// Act.
	h.network.fire()
	got := <-h.published

	// Assert.
	if got.GetJoined().GetNetworkName() != "Office" {
		t.Fatalf("network after the change = %q, want Office", got.GetJoined().GetNetworkName())
	}
}

func TestRunRereadsOnAPowerChange(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOff, summary(true, "Home"))
	h.r.on(argPmsetRead, answer{out: pmsetOff}, answer{out: pmsetOff}, answer{out: pmsetOn})
	stop := h.runController(t)
	defer stop()

	// Act.
	h.power.changes <- nil
	got := <-h.published

	// Assert.
	if modeArm(got) != "on" {
		t.Fatalf("mode after the power change = %s, want on", modeArm(got))
	}
}

func TestRunRecordsAFailedPowerWatchAtErrorAndKeepsHearingTheNetwork(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOff, summary(true, "Home"))
	h.r.on(argSummary, answer{out: summary(true, "Home")}, answer{out: summary(true, "Home")}, answer{out: summary(true, "Office")})
	stop := h.runController(t)
	defer stop()

	// Act: the watch fails, then the network still changes.
	h.power.changes <- errors.New("the preference directory was removed")
	h.network.fire()
	<-h.published

	// Assert.
	errs := h.records("error", opProbe)
	if len(errs) != 1 || errs[0].Context["cause"] != "the preference directory was removed" {
		t.Fatalf("ERROR records = %+v, want the failed power watch with its cause", errs)
	}
}

func TestUpdateOnWithTheLidClosedJoinsTheHotspotDisablesSleepAndDims(t *testing.T) {
	// Arrange.
	h := newHarness(t).withLid(lidClosed)
	h.r.on(argBrightnessRead, answer{out: "0.81\n"})
	h.r.on(argLidDim, answer{})
	h.r.on(argPmsetRead, answer{out: pmsetOff}, answer{out: pmsetOn})
	h.r.on(argPorts, answer{out: ports})
	h.r.on(argSummary, answer{out: summary(true, "Home")}, answer{out: summary(true, hotspot)})
	h.r.on(argJoin, answer{})
	scriptPower(h.r, "1")

	// Act.
	resp := h.c.Update(context.Background(), update("on"))

	// Assert.
	s := resp.GetSuccess()
	if s == nil {
		t.Fatalf("Update() = %v, want success", resp)
	}
	if s.GetHotspot().GetJoined().GetNetworkName() != hotspot || s.GetDisplay().GetDimmed() == nil {
		t.Fatalf("outcomes = (%v, %v), want (joined, dimmed)", s.GetHotspot(), s.GetDisplay())
	}
	if modeArm(s.GetState()) != "on" || s.GetState().GetJoined().GetNetworkName() != hotspot {
		t.Fatalf("state = %v, want on and joined to the hotspot", s.GetState())
	}
	calls := h.r.ran()
	join, power, energy, dim := slices.Index(calls, argJoin), slices.Index(calls, argPower("disablesleep", "1")),
		slices.Index(calls, argEnergy(true)), slices.Index(calls, argLidDim)
	if !(join < power && power < energy && energy < dim) {
		t.Fatalf("step order = %v, want hotspot, power, energy mode, display", calls)
	}
}

func TestUpdateSetsTheEnergyModeForTheDirection(t *testing.T) {
	cases := []struct {
		name   string
		action string
		pmset  string
		want   func(*agentreplv1.UpdatePersistentWifiModeEnergyMode) bool
		arm    string
	}{
		{name: "on is low power", action: "on", pmset: pmsetOff, arm: "low_power",
			want: func(e *agentreplv1.UpdatePersistentWifiModeEnergyMode) bool { return e.GetLowPower() != nil }},
		{name: "off is high power", action: "off", pmset: pmsetOn, arm: "high_power",
			want: func(e *agentreplv1.UpdatePersistentWifiModeEnergyMode) bool { return e.GetHighPower() != nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			on := tc.action == "on"
			h := newHarness(t).standing(tc.pmset, summary(true, "Home"))
			h.r.on(argJoin, answer{})
			value := "0"
			if on {
				value = "1"
			}
			scriptPower(h.r, value)

			// Act.
			resp := h.c.Update(context.Background(), update(tc.action))

			// Assert.
			if resp.GetSuccess() == nil || !tc.want(resp.GetSuccess().GetEnergyMode()) {
				t.Fatalf("Update() = %v, want energy mode %s", resp, tc.arm)
			}
			if h.r.count(argEnergy(on)) != 1 {
				t.Fatalf("calls = %v, want one %q", h.r.ran(), argEnergy(on))
			}
			infos := h.records("info", opUpdate)
			if len(infos) != 1 || infos[0].Context["energy_mode_outcome"] != tc.arm {
				t.Fatalf("update INFO records = %+v, want one naming energy mode %s", infos, tc.arm)
			}
			if warns := h.records("warn", opUpdate); len(warns) != 0 {
				t.Fatalf("update WARN records = %+v, want none", warns)
			}
		})
	}
}

func TestUpdateReportsAFailedEnergyModeWithoutFailingTheRequest(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOff, summary(true, hotspot))
	h.r.on(argPmsetRead, answer{out: pmsetOff}, answer{out: pmsetOn})
	scriptPower(h.r, "1")
	h.r.on(argEnergy(true), answer{out: "sudo: a password is required", code: 1})

	// Act.
	resp := h.c.Update(context.Background(), update("on"))

	// Assert.
	s := resp.GetSuccess()
	if s == nil {
		t.Fatalf("Update() = %v, want success", resp)
	}
	failed := s.GetEnergyMode().GetFailed()
	if failed == nil || !strings.HasPrefix(failed.GetDetail(), "pmset -a powermode 1: ") {
		t.Fatalf("energy mode = %v, want failed naming the invocation", s.GetEnergyMode())
	}
	if s.GetDisplay().GetUnchanged() == nil || modeArm(s.GetState()) != "on" {
		t.Fatalf("display = %v, mode = %s, want the display step still run (unchanged) and on", s.GetDisplay(), modeArm(s.GetState()))
	}
	warns := h.records("warn", opUpdate)
	if len(warns) != 1 || warns[0].Context["cause"] != failed.GetDetail() || warns[0].Context["on"] != true {
		t.Fatalf("update WARN records = %+v, want one carrying the energy mode cause", warns)
	}
}

func TestUpdateOnSkipsAJoinWhenAlreadyOnTheHotspot(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOff, summary(true, hotspot))
	scriptPower(h.r, "1")

	// Act.
	resp := h.c.Update(context.Background(), update("on"))

	// Assert.
	if resp.GetSuccess().GetHotspot().GetAlreadyJoined() == nil {
		t.Fatalf("hotspot = %v, want already_joined", resp.GetSuccess().GetHotspot())
	}
	if h.r.count(argJoin) != 0 {
		t.Fatal("a join ran for a hotspot already joined")
	}
}

func TestUpdateOnRunsThePowerStepWhenTheJoinFails(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOff, summary(true, "Home"))
	h.r.on(argJoin, answer{out: "Could not find network Test Hotspot."})
	scriptPower(h.r, "1")

	// Act.
	resp := h.c.Update(context.Background(), update("on"))

	// Assert.
	failed := resp.GetSuccess().GetHotspot().GetFailed()
	if failed == nil || failed.GetDetail() != "Could not find network Test Hotspot." {
		t.Fatalf("hotspot = %v, want failed with the tool's sentence", resp.GetSuccess().GetHotspot())
	}
	if h.r.count(argPower("disablesleep", "1")) != 1 {
		t.Fatal("the power step did not run after a failed join")
	}
	if len(h.records("warn", opUpdate)) != 1 {
		t.Fatalf("WARN records = %v, want one for the hotspot", h.records("warn", opUpdate))
	}
}

func TestUpdateOnWithNoWifiInterfaceSkipsTheHotspot(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.r.on(argPmsetRead, answer{out: pmsetOff})
	h.r.on(argPorts, answer{out: noWifi})
	scriptPower(h.r, "1")

	// Act.
	resp := h.c.Update(context.Background(), update("on"))

	// Assert.
	if resp.GetSuccess().GetHotspot().GetNoWifiInterface() == nil {
		t.Fatalf("hotspot = %v, want no_wifi_interface", resp.GetSuccess().GetHotspot())
	}
}

func TestUpdateOffLeavesTheHotspot(t *testing.T) {
	cases := []struct {
		name        string
		wifiUtil    bool
		links       []answer
		disconnect  bool
		wantRestart bool
		wantLeft    bool
	}{
		{
			name: "the disconnect takes", wifiUtil: true, disconnect: true,
			links:    []answer{{out: summary(true, hotspot)}, {out: summary(false, "")}},
			wantLeft: true,
		},
		{
			name: "the disconnect is refused and the radio restart takes", wifiUtil: true, disconnect: true,
			links:       []answer{{out: summary(true, hotspot)}, {out: summary(true, hotspot)}, {out: summary(false, "")}},
			wantRestart: true, wantLeft: true,
		},
		{
			name: "no wifi-util goes straight to the radio restart", wifiUtil: false,
			links:       []answer{{out: summary(true, hotspot)}, {out: summary(false, "")}},
			wantRestart: true, wantLeft: true,
		},
		{
			name: "still joined after both is a failed outcome", wifiUtil: true, disconnect: true,
			links:       []answer{{out: summary(true, hotspot)}},
			wantRestart: true, wantLeft: false,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.exists[tools.WifiUtil] = tc.wifiUtil
			h.r.on(argPmsetRead, answer{out: pmsetOn})
			h.r.on(argPorts, answer{out: ports})
			h.r.on(argSummary, tc.links...)
			h.r.on(argDisconnect, answer{})
			h.r.on(argRadioOff, answer{})
			h.r.on(argRadioOn, answer{})
			scriptPower(h.r, "0")

			// Act.
			resp := h.c.Update(context.Background(), update("off"))

			// Assert.
			hs := resp.GetSuccess().GetHotspot()
			if (hs.GetLeft() != nil) != tc.wantLeft || (hs.GetFailed() != nil) == tc.wantLeft {
				t.Fatalf("hotspot = %v, want left=%v", hs, tc.wantLeft)
			}
			if (h.r.count(argRadioOff) == 1) != tc.wantRestart {
				t.Fatalf("radio restarted = %v, want %v (calls %v)", h.r.count(argRadioOff) == 1, tc.wantRestart, h.r.ran())
			}
			if (h.r.count(argDisconnect) == 1) != tc.disconnect {
				t.Fatalf("disconnect ran = %v, want %v", h.r.count(argDisconnect) == 1, tc.disconnect)
			}
			if resp.GetSuccess().GetDisplay().GetUnchanged() == nil {
				t.Fatalf("display = %v, want unchanged with no lid dim in force", resp.GetSuccess().GetDisplay())
			}
		})
	}
}

// TestUpdateWithNoHotspotConfiguredFailsTheHotspotStepOnly pins that a host
// that named no hotspot runs no network tool, answers the hotspot step as
// failed with a detail naming the variable and the user guide, records it at
// ERROR, and still changes the mode.
func TestUpdateWithNoHotspotConfiguredFailsTheHotspotStepOnly(t *testing.T) {
	cases := []struct {
		name   string
		action string
		pmset  string
		power  string
	}{
		{name: "turning on", action: "on", pmset: pmsetOff, power: "1"},
		{name: "turning off", action: "off", pmset: pmsetOn, power: "0"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarnessFor(t, "")
			h.standing(tc.pmset, summary(true, "Home"))
			scriptPower(h.r, tc.power)

			// Act.
			resp := h.c.Update(context.Background(), update(tc.action))

			// Assert.
			failed := resp.GetSuccess().GetHotspot().GetFailed()
			if failed == nil || !strings.Contains(failed.GetDetail(), EnvHotspot) || !strings.Contains(failed.GetDetail(), usersetup.Doc) {
				t.Fatalf("hotspot = %v, want failed naming %s and the user guide", resp.GetSuccess().GetHotspot(), EnvHotspot)
			}
			for _, ran := range h.r.ran() {
				if strings.Contains(ran, "networksetup -setairport") || strings.Contains(ran, "wifi-util") {
					t.Fatalf("ran %q with no hotspot configured", ran)
				}
			}
			if len(h.records(dlog.LevelError, opUpdate)) != 1 {
				t.Fatalf("records = %+v, want one ERROR under %s", h.log.Records(), opUpdate)
			}
		})
	}
}

func TestUpdateOffLeavesNothingItCannotIdentify(t *testing.T) {
	cases := []struct {
		name string
		link string
		want func(*agentreplv1.UpdatePersistentWifiModeHotspot) bool
	}{
		{name: "a withheld name is network_unreadable", link: summary(true, redactedName),
			want: func(h *agentreplv1.UpdatePersistentWifiModeHotspot) bool { return h.GetNetworkUnreadable() != nil }},
		{name: "another network is not_on_hotspot", link: summary(true, "Home"),
			want: func(h *agentreplv1.UpdatePersistentWifiModeHotspot) bool { return h.GetNotOnHotspot() != nil }},
		{name: "no network is not_on_hotspot", link: summary(false, ""),
			want: func(h *agentreplv1.UpdatePersistentWifiModeHotspot) bool { return h.GetNotOnHotspot() != nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t).standing(pmsetOn, tc.link)
			scriptPower(h.r, "0")

			// Act.
			resp := h.c.Update(context.Background(), update("off"))

			// Assert.
			if !tc.want(resp.GetSuccess().GetHotspot()) {
				t.Fatalf("hotspot = %v", resp.GetSuccess().GetHotspot())
			}
			if h.r.count(argDisconnect)+h.r.count(argRadioOff) != 0 {
				t.Fatalf("a leave step ran: %v", h.r.ran())
			}
		})
	}
}

func TestUpdateAnswersARefusedPowerStepAsAnErrorAndSkipsTheDisplay(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOff, summary(true, "Home"))
	h.r.on(argJoin, answer{})
	h.r.on(argPower("disablesleep", "1"), answer{out: "sudo: a password is required", code: 1})

	// Act.
	resp := h.c.Update(context.Background(), update("on"))

	// Assert.
	refused := resp.GetError().GetPowerSettingsRefused()
	if refused == nil {
		t.Fatalf("Update() = %v, want power_settings_refused", resp)
	}
	if h.r.count(argBrightnessRead)+h.r.count(argEnergy(true))+h.r.count(argPower("networkoversleep", "1")) != 0 {
		t.Fatalf("a step ran after the refusal: %v", h.r.ran())
	}
	errs := h.records("error", opUpdate)
	if len(errs) != 1 || errs[0].Context["cause"] == nil {
		t.Fatalf("update ERROR records = %+v, want one carrying the cause", errs)
	}
}

func TestUpdateToggleTurnsTheReadModeOver(t *testing.T) {
	cases := []struct {
		name      string
		pmset     string
		wantValue string
	}{
		{name: "on turns off", pmset: pmsetOn, wantValue: "0"},
		{name: "off turns on", pmset: pmsetOff, wantValue: "1"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t).standing(tc.pmset, summary(true, "Home"))
			h.r.on(argJoin, answer{})
			scriptPower(h.r, tc.wantValue)

			// Act.
			resp := h.c.Update(context.Background(), update("toggle"))

			// Assert.
			if resp.GetSuccess() == nil || h.r.count(argPower("disablesleep", tc.wantValue)) != 1 {
				t.Fatalf("Update() = %v with calls %v, want disablesleep %s", resp, h.r.ran(), tc.wantValue)
			}
		})
	}
}

func TestUpdateToggleRefusesAnUnreadableModeAndChangesNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOn, summary(true, "Home"))
	h.r.on(argPmsetRead, answer{out: "garbage"})

	// Act.
	resp := h.c.Update(context.Background(), update("toggle"))

	// Assert.
	if resp.GetError().GetModeUnreadable() == nil {
		t.Fatalf("Update() = %v, want mode_unreadable", resp)
	}
	for _, c := range h.r.ran() {
		if c != argPmsetRead && c != argPorts && c != argSummary {
			t.Fatalf("a change step ran: %q", c)
		}
	}
	errs := h.records("error", opUpdate)
	if len(errs) != 1 || errs[0].Context["action"] != "toggle" {
		t.Fatalf("update ERROR records = %+v, want one for the toggle", errs)
	}
}

func TestUpdateReportsTheDisplayStepWithoutFailingTheRequest(t *testing.T) {
	cases := []struct {
		name    string
		present bool
		read    answer
		want    func(*agentreplv1.UpdatePersistentWifiModeDisplay) bool
	}{
		{name: "an absent tool is tool_missing", present: false,
			want: func(d *agentreplv1.UpdatePersistentWifiModeDisplay) bool {
				return d.GetToolMissing().GetToolPath() == tools.Brightness
			}},
		{name: "a failing read is failed", present: true, read: answer{out: "no display", code: 1},
			want: func(d *agentreplv1.UpdatePersistentWifiModeDisplay) bool {
				return strings.Contains(d.GetFailed().GetDetail(), "could not be read")
			}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t).standing(pmsetOff, summary(true, hotspot)).withLid(lidClosed)
			h.exists[tools.Brightness] = tc.present
			scriptPower(h.r, "1")
			h.r.on(argBrightnessRead, tc.read)

			// Act.
			resp := h.c.Update(context.Background(), update("on"))

			// Assert.
			if resp.GetSuccess() == nil || !tc.want(resp.GetSuccess().GetDisplay()) {
				t.Fatalf("Update() = %v", resp)
			}
			if errs := h.records(dlog.LevelError, opDisplay); len(errs) != 1 || errs[0].Context["trigger"] != "update" {
				t.Fatalf("display ERROR records = %+v, want one for the update", errs)
			}
			if h.r.count(argLidDim) != 0 || len(h.dims.puts) != 0 {
				t.Fatalf("calls = %v, puts = %v, want no dim and no kept level", h.r.ran(), h.dims.puts)
			}
		})
	}
}

func TestRefreshAbandonedByItsCallerRecordsNoCause(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.r.on(argPmsetRead, answer{err: context.Canceled})
	h.r.on(argPorts, answer{err: context.Canceled})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	h.c.Refresh(ctx)

	// Assert.
	if errs := h.records("error", opProbe); len(errs) != 0 {
		t.Fatalf("probe ERROR records = %+v, want none for an abandoned read", errs)
	}
}

func TestRefreshAbandonedByItsCallerPublishesNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.r.on(argPmsetRead, answer{err: context.Canceled})
	h.r.on(argPorts, answer{err: context.Canceled})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	h.c.Refresh(ctx)

	// Assert.
	if _, ok := h.c.Topic().Latest(); ok {
		t.Fatalf("an abandoned read published a standing")
	}
}

func TestRefreshAbandonedByItsCallerIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.r.on(argPmsetRead, answer{err: context.Canceled})
	h.r.on(argPorts, answer{err: context.Canceled})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	h.c.Refresh(ctx)

	// Assert.
	if infos := h.records("info", opProbe); len(infos) != 1 || infos[0].Context["cause"] != context.Canceled.Error() {
		t.Fatalf("probe INFO records = %+v, want one naming the cancellation", infos)
	}
}

// ---- the hotspot's name is resolved against the saved networks ----

// Two spellings of one hotspot: the ASCII apostrophe a person types, and the
// right single quotation mark an iPhone names itself with.
const (
	straightHotspot = "Someone's iPhone"
	curlyHotspot    = "Someone\u2019s iPhone"
)

func TestUpdateOnJoinsTheSavedSpellingOfTheHotspot(t *testing.T) {
	// Arrange: configured straight, saved curly, on another network now.
	h := newHarnessFor(t, straightHotspot).standing(pmsetOff, summary(true, "Home"))
	h.r.on(argPreferred, answer{out: preferred("Home", curlyHotspot)})
	h.r.on("/t/networksetup -setairportnetwork en0 "+curlyHotspot, answer{})
	scriptPower(h.r, "1")

	// Act.
	resp := h.c.Update(context.Background(), update("on"))

	// Assert: the join named the saved SSID exactly.
	if got := resp.GetSuccess().GetHotspot().GetJoined().GetNetworkName(); got != curlyHotspot {
		t.Fatalf("hotspot = %v, want joined to the saved %q", resp.GetSuccess().GetHotspot(), curlyHotspot)
	}
}

func TestUpdateOnSeesTheOtherSpellingAsAlreadyJoined(t *testing.T) {
	// Arrange: configured straight, joined to the curly one.
	h := newHarnessFor(t, straightHotspot).standing(pmsetOff, summary(true, curlyHotspot))
	scriptPower(h.r, "1")

	// Act.
	resp := h.c.Update(context.Background(), update("on"))

	// Assert.
	if resp.GetSuccess().GetHotspot().GetAlreadyJoined() == nil {
		t.Fatalf("hotspot = %v, want already_joined", resp.GetSuccess().GetHotspot())
	}
}

func TestUpdateOnTriesTheConfiguredNameWhenNoSavedNetworkMatches(t *testing.T) {
	// Arrange: nothing saved matches; the join of the configured name fails.
	h := newHarnessFor(t, straightHotspot).standing(pmsetOff, summary(true, "Home"))
	h.r.on(argPreferred, answer{out: preferred("Home")})
	h.r.on("/t/networksetup -setairportnetwork en0 "+straightHotspot, answer{out: "Could not find network Someone's iPhone."})
	scriptPower(h.r, "1")

	// Act.
	resp := h.c.Update(context.Background(), update("on"))

	// Assert: the detail says no saved network matched.
	failed := resp.GetSuccess().GetHotspot().GetFailed()
	if failed == nil || !strings.Contains(failed.GetDetail(), "no saved network matches") {
		t.Fatalf("hotspot = %v, want failed naming the missing saved network", resp.GetSuccess().GetHotspot())
	}
}

func TestUpdateOffLeavesAHotspotJoinedUnderTheOtherSpelling(t *testing.T) {
	// Arrange: configured straight, joined curly; the disconnect takes.
	h := newHarnessFor(t, straightHotspot)
	h.r.on(argPmsetRead, answer{out: pmsetOn})
	h.r.on(argPorts, answer{out: ports})
	h.r.on(argSummary, answer{out: summary(true, curlyHotspot)}, answer{out: summary(false, "")})
	h.r.on(argDisconnect, answer{})
	scriptPower(h.r, "0")

	// Act.
	resp := h.c.Update(context.Background(), update("off"))

	// Assert.
	if resp.GetSuccess().GetHotspot().GetLeft() == nil {
		t.Fatalf("hotspot = %v, want left", resp.GetSuccess().GetHotspot())
	}
}

func TestStillOnHotspotReadsTheOtherSpellingAsStillJoined(t *testing.T) {
	// Arrange: the disconnect is refused, the link still names the curly
	// spelling, and the radio restart then takes.
	h := newHarnessFor(t, straightHotspot)
	h.r.on(argPmsetRead, answer{out: pmsetOn})
	h.r.on(argPorts, answer{out: ports})
	h.r.on(argSummary, answer{out: summary(true, curlyHotspot)}, answer{out: summary(true, curlyHotspot)}, answer{out: summary(false, "")})
	h.r.on(argDisconnect, answer{})
	h.r.on(argRadioOff, answer{})
	h.r.on(argRadioOn, answer{})
	scriptPower(h.r, "0")

	// Act.
	h.c.Update(context.Background(), update("off"))

	// Assert: still joined was seen, so the radio was restarted.
	if h.r.count(argRadioOff) != 1 {
		t.Fatalf("calls = %v, want a radio restart after the refused disconnect", h.r.ran())
	}
}
