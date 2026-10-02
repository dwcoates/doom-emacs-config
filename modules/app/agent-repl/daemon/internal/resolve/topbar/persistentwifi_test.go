package topbar

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/ids"
)

func wifiState(wifi, mode string, name *string) *agentreplv1.PersistentWifiState {
	s := &agentreplv1.PersistentWifiState{}
	switch wifi {
	case "joined":
		s.Wifi = &agentreplv1.PersistentWifiState_Joined{Joined: &agentreplv1.PersistentWifiJoined{NetworkName: name}}
	case "not_joined":
		s.Wifi = &agentreplv1.PersistentWifiState_NotJoined{NotJoined: &agentreplv1.PersistentWifiNotJoined{}}
	}
	switch mode {
	case "on":
		s.Mode = &agentreplv1.PersistentWifiState_On{On: &agentreplv1.PersistentWifiModeOn{}}
	case "off":
		s.Mode = &agentreplv1.PersistentWifiState_Off{Off: &agentreplv1.PersistentWifiModeOff{}}
	}
	return s
}

func TestPersistentWifiChipProjectsEachFact(t *testing.T) {
	home := "Home"
	cases := []struct {
		name              string
		state             *agentreplv1.PersistentWifiState
		joined, notJoined bool
		on, off           bool
		wantTooltip       string
	}{
		{name: "joined with a name, on", state: wifiState("joined", "on", &home), joined: true, on: true,
			wantTooltip: "Closing laptop lid disables agents"},
		{name: "joined with a withheld name, off", state: wifiState("joined", "off", nil), joined: true, off: true,
			wantTooltip: "Closing laptop lid disables agents"},
		{name: "not joined, on", state: wifiState("not_joined", "on", nil), notJoined: true, on: true,
			wantTooltip: "Closing laptop lid disables agents"},
		{name: "both unread", state: wifiState("", "", nil),
			wantTooltip: "Closing laptop lid disables agents. Wi-Fi and persistent wifi mode could not be read."},
		{name: "only the mode unread", state: wifiState("joined", "", &home), joined: true,
			wantTooltip: "Closing laptop lid disables agents. Persistent wifi mode could not be read."},
		{name: "a standing nobody read", state: nil,
			wantTooltip: "Closing laptop lid disables agents. Wi-Fi and persistent wifi mode could not be read."},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			chip := persistentWifiChip(tc.state)

			// Assert.
			if (chip.GetJoined() != nil) != tc.joined || (chip.GetNotJoined() != nil) != tc.notJoined ||
				(chip.GetOn() != nil) != tc.on || (chip.GetOff() != nil) != tc.off {
				t.Fatalf("chip = %v", chip)
			}
			if got := chip.GetTooltip().GetText(); got != tc.wantTooltip {
				t.Fatalf("tooltip = %q, want %q", got, tc.wantTooltip)
			}
		})
	}
}

func TestThePersistentWifiStandingStandsOnEveryStrip(t *testing.T) {
	tests := []struct {
		name  string
		setup func(t *testing.T, h *harness, set func())
	}{
		{"strips that stood before it was set", func(t *testing.T, h *harness, set func()) {
			h.ready(t)
			readyOther(t, h)
			set()
		}},
		{"a strip made after it was set", func(t *testing.T, h *harness, set func()) {
			h.ready(t)
			set()
			readyOther(t, h)
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			tc.setup(t, h, func() { h.r.SetPersistentWifi(wifiState("joined", "on", nil)) })

			// Assert.
			for _, ws := range []ids.WorkspaceID{testWS, otherWS} {
				view, ok := h.r.Topic(ws).Latest()
				if !ok {
					t.Fatalf("no topbar for %s", ws)
				}
				if view.GetPersistentWifi().GetOn() == nil || view.GetPersistentWifi().GetJoined() == nil {
					t.Fatalf("%s chip = %v, want joined and on", ws, view.GetPersistentWifi())
				}
			}
		})
	}
}
