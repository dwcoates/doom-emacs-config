package topbar

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/ids"
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

func TestPersistentWifiChipProjectsTheLid(t *testing.T) {
	cases := []struct {
		name        string
		lid         func(*agentreplv1.PersistentWifiState)
		arm         string
		wantTooltip string
	}{
		{name: "an unheard lid states nothing", arm: "",
			wantTooltip: "Closing laptop lid disables agents"},
		{name: "an open lid", arm: "open",
			lid: func(s *agentreplv1.PersistentWifiState) {
				s.Lid = &agentreplv1.PersistentWifiState_LidOpen{LidOpen: &agentreplv1.PersistentWifiLidOpen{}}
			},
			wantTooltip: "Closing laptop lid disables agents. Lid open."},
		{name: "a closed lid", arm: "closed",
			lid: func(s *agentreplv1.PersistentWifiState) {
				s.Lid = &agentreplv1.PersistentWifiState_LidClosed{LidClosed: &agentreplv1.PersistentWifiLidClosed{}}
			},
			wantTooltip: "Closing laptop lid disables agents. Lid closed."},
		{name: "no lid", arm: "none",
			lid: func(s *agentreplv1.PersistentWifiState) {
				s.Lid = &agentreplv1.PersistentWifiState_NoLid{NoLid: &agentreplv1.PersistentWifiNoLid{}}
			},
			wantTooltip: "Closing laptop lid disables agents. No lid."},
		{name: "an unwatched lid names its cause", arm: "unwatched",
			lid: func(s *agentreplv1.PersistentWifiState) {
				s.Lid = &agentreplv1.PersistentWifiState_LidUnwatched{LidUnwatched: &agentreplv1.PersistentWifiLidUnwatched{Detail: "IOPMrootDomain was not found"}}
			},
			wantTooltip: "Closing laptop lid disables agents. The lid is not being watched, so a closed lid does not quiet the machine: IOPMrootDomain was not found."},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			state := wifiState("joined", "on", nil)
			if tc.lid != nil {
				tc.lid(state)
			}

			// Act.
			chip := persistentWifiChip(state)

			// Assert.
			arm := ""
			switch {
			case chip.GetLidOpen() != nil:
				arm = "open"
			case chip.GetLidClosed() != nil:
				arm = "closed"
			case chip.GetNoLid() != nil:
				arm = "none"
			case chip.GetLidUnwatched() != nil:
				arm = "unwatched"
			}
			if arm != tc.arm {
				t.Fatalf("lid arm = %q, want %q", arm, tc.arm)
			}
			if got := chip.GetTooltip().GetText(); got != tc.wantTooltip {
				t.Fatalf("tooltip = %q, want %q", got, tc.wantTooltip)
			}
		})
	}
}

func TestPersistentWifiChipStatesTheLidAfterAnUnreadFact(t *testing.T) {
	// Arrange.
	state := wifiState("", "on", nil)
	state.Lid = &agentreplv1.PersistentWifiState_LidOpen{LidOpen: &agentreplv1.PersistentWifiLidOpen{}}

	// Act.
	chip := persistentWifiChip(state)

	// Assert.
	if got, want := chip.GetTooltip().GetText(), "Closing laptop lid disables agents. Wi-Fi could not be read. Lid open."; got != want {
		t.Fatalf("tooltip = %q, want %q", got, want)
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
