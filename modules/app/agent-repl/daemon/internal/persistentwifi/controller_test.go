package persistentwifi

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
)

// harness is one controller over a fake runner.
type harness struct {
	c       *Controller
	r       *fakeRunner
	log     *dlog.TestLogger
	clock   *fakeClock
	changes []*agentreplv1.PersistentWifiState
	exists  map[string]bool
}

func newHarness(t *testing.T) *harness {
	t.Helper()
	h := &harness{
		r: newFakeRunner(t), log: dlog.NewTestLogger(), clock: newFakeClock(),
		exists: map[string]bool{tools.WifiUtil: true, tools.Brightness: true},
	}
	c, err := New(Deps{
		Config: Config{Tools: tools, Hotspot: hotspot},
		Runner: h.r, Clock: h.clock, Log: h.log,
		Exists:   func(p string) bool { return h.exists[p] },
		OnChange: func(s *agentreplv1.PersistentWifiState) { h.changes = append(h.changes, s) },
	})
	if err != nil {
		t.Fatalf("New() error = %v", err)
	}
	h.c = c
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
	var out []dlog.Record
	for _, r := range h.log.Records() {
		if r.Level == level && r.Operation == operation {
			out = append(out, r)
		}
	}
	return out
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
		return Deps{Config: Config{Tools: tools, Hotspot: hotspot}, Runner: newFakeRunner(t), Clock: newFakeClock(), Log: dlog.NewTestLogger()}
	}
	cases := []struct {
		name   string
		break_ func(*Deps)
	}{
		{name: "no runner", break_: func(d *Deps) { d.Runner = nil }},
		{name: "no clock", break_: func(d *Deps) { d.Clock = nil }},
		{name: "no logger", break_: func(d *Deps) { d.Log = nil }},
		{name: "a negative cadence", break_: func(d *Deps) { d.Every = -time.Second }},
		{name: "no hotspot", break_: func(d *Deps) { d.Config.Hotspot = "" }},
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

func TestRunRereadsOnEveryTickAndEndsWithItsContext(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOff, summary(false, ""))
	h.r.on(argPmsetRead, answer{out: pmsetOff}, answer{out: pmsetOn})
	h.c.Refresh(context.Background())
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- h.c.Run(ctx) }()

	// Act.
	(<-h.clock.afters) <- time.Unix(0, 0)
	<-h.clock.afters // the second wait proves the tick's read finished
	cancel()

	// Assert.
	if err := <-done; err != nil {
		t.Fatalf("Run() = %v, want nil on cancellation", err)
	}
	got, _ := h.c.Topic().Latest()
	if modeArm(got) != "on" {
		t.Fatalf("mode after the tick = %s, want on", modeArm(got))
	}
}

func TestUpdateOnJoinsTheHotspotDisablesSleepAndDims(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.r.on(argPmsetRead, answer{out: pmsetOff}, answer{out: pmsetOn})
	h.r.on(argPorts, answer{out: ports})
	h.r.on(argSummary, answer{out: summary(true, "Home")}, answer{out: summary(true, hotspot)})
	h.r.on(argJoin, answer{})
	scriptPower(h.r, "1")
	h.r.on(argDim, answer{})

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
	join, power, dim := slices.Index(calls, argJoin), slices.Index(calls, argPower("disablesleep", "1")), slices.Index(calls, argDim)
	if !(join < power && power < dim) {
		t.Fatalf("step order = %v, want hotspot, power, display", calls)
	}
}

func TestUpdateOnSkipsAJoinWhenAlreadyOnTheHotspot(t *testing.T) {
	// Arrange.
	h := newHarness(t).standing(pmsetOff, summary(true, hotspot))
	scriptPower(h.r, "1")
	h.r.on(argDim, answer{})

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
	h.r.on(argDim, answer{})

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
	h.r.on(argDim, answer{})

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
			h.r.on(argRestore, answer{})

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
			if resp.GetSuccess().GetDisplay().GetRestored() == nil {
				t.Fatalf("display = %v, want restored", resp.GetSuccess().GetDisplay())
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
			h.r.on(argRestore, answer{})

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
	if h.r.count(argDim)+h.r.count(argPower("networkoversleep", "1")) != 0 {
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
			h.r.on(argDim, answer{})
			h.r.on(argRestore, answer{})

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
		answer  answer
		want    func(*agentreplv1.UpdatePersistentWifiModeDisplay) bool
	}{
		{name: "an absent tool is tool_missing", present: false,
			want: func(d *agentreplv1.UpdatePersistentWifiModeDisplay) bool {
				return d.GetToolMissing().GetToolPath() == tools.Brightness
			}},
		{name: "a failing tool is failed", present: true, answer: answer{out: "no display", code: 1},
			want: func(d *agentreplv1.UpdatePersistentWifiModeDisplay) bool { return d.GetFailed() != nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t).standing(pmsetOff, summary(true, hotspot))
			h.exists[tools.Brightness] = tc.present
			scriptPower(h.r, "1")
			h.r.on(argDim, tc.answer)

			// Act.
			resp := h.c.Update(context.Background(), update("on"))

			// Assert.
			if resp.GetSuccess() == nil || !tc.want(resp.GetSuccess().GetDisplay()) {
				t.Fatalf("Update() = %v", resp)
			}
			if len(h.records("warn", opUpdate)) != 1 {
				t.Fatalf("WARN records = %v, want one for the display", h.records("warn", opUpdate))
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
