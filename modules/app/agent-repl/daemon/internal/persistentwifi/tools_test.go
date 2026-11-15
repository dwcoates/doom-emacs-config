package persistentwifi

import (
	"reflect"
	"testing"

	"agent-repld/internal/appfreeze"
)

func TestConfigFromEnv(t *testing.T) {
	cases := []struct {
		name    string
		env     map[string]string
		want    Config
		wantErr bool
	}{
		{
			name:    "a HOME that is not absolute is refused: the Wi-Fi helper could not be found",
			env:     map[string]string{"HOME": "home/owner"},
			wantErr: true,
		},
		{
			name:    "no HOME is refused",
			env:     map[string]string{},
			wantErr: true,
		},
		{
			name: "unset is the production layout and no hotspot",
			env:  map[string]string{"HOME": "/home/owner"},
			want: Config{Tools: homeTools(), FreezeApps: appfreeze.DefaultApps},
		},
		{
			name: "the freeze list names the applications",
			env:  map[string]string{"HOME": "/home/owner", appfreeze.EnvApps: "Slack, Discord"},
			want: Config{Tools: homeTools(), FreezeApps: []string{"Slack", "Discord"}},
		},
		{
			name:    "a freeze list naming a terminal is refused",
			env:     map[string]string{"HOME": "/home/owner", appfreeze.EnvApps: "Slack,Terminal"},
			wantErr: true,
		},
		{
			name: "a fake host freezes nothing unless told to",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open", EnvPowerSource: "ac"},
			want: Config{Tools: ToolsIn("/fake/bin"), FakeLid: "open", FakePowerSource: "ac"},
		},
		{
			name: "a fake host freezes what it is told to",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open", EnvPowerSource: "ac", appfreeze.EnvApps: "Slack"},
			want: Config{Tools: ToolsIn("/fake/bin"), FakeLid: "open", FakePowerSource: "ac", FreezeApps: []string{"Slack"}},
		},
		{
			name: "a tools dir resolves every tool there by its base name, with its fake lid",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "/fake/lid", EnvPowerSource: "ac"},
			want: Config{Tools: Tools{
				Sudo: "/fake/bin/sudo", Pmset: "/fake/bin/pmset", Ipconfig: "/fake/bin/ipconfig",
				Networksetup: "/fake/bin/networksetup", WifiUtil: "/fake/bin/wifi-util",
				Caffeinate: "/fake/bin/caffeinate", Brightness: "/fake/bin/mac-brightness", Ps: "/fake/bin/ps",
				Open: "/fake/bin/open", WifiApp: "/fake/bin/AgentReplWifi.app",
			}, FakeLid: "/fake/lid", FakePowerSource: "ac"},
		},
		{
			name:    "a tools dir without a fake lid is refused",
			env:     map[string]string{EnvToolsDir: "/fake/bin", EnvPowerSource: "ac"},
			wantErr: true,
		},
		{
			name:    "a fake lid without a tools dir is refused",
			env:     map[string]string{EnvLid: "/fake/lid"},
			wantErr: true,
		},
		{
			name:    "a tools dir and a fake lid without a fake power source are refused",
			env:     map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open"},
			wantErr: true,
		},
		{
			name:    "a fake power source without a tools dir is refused",
			env:     map[string]string{"HOME": "/home/owner", EnvPowerSource: "battery"},
			wantErr: true,
		},
		{
			name: "a power source state is a fixed power source",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open", EnvPowerSource: "battery"},
			want: Config{Tools: ToolsIn("/fake/bin"), FakeLid: "open", FakePowerSource: "battery"},
		},
		{
			name: "an absolute power source socket is a socket power source",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open", EnvPowerSource: "/fake/power"},
			want: Config{Tools: ToolsIn("/fake/bin"), FakeLid: "open", FakePowerSource: "/fake/power"},
		},
		{
			name:    "a relative power source socket is refused",
			env:     map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open", EnvPowerSource: "fake/power"},
			wantErr: true,
		},
		{
			name: "a lid state is a fixed lid",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open", EnvPowerSource: "ac"},
			want: Config{Tools: ToolsIn("/fake/bin"), FakeLid: "open", FakePowerSource: "ac"},
		},
		{
			name:    "a relative lid socket is refused",
			env:     map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "fake/lid", EnvPowerSource: "ac"},
			wantErr: true,
		},
		{
			name: "a hotspot override names the hotspot",
			env:  map[string]string{"HOME": "/home/owner", EnvHotspot: "Other Phone"},
			want: Config{Tools: homeTools(), Hotspot: "Other Phone", FreezeApps: appfreeze.DefaultApps},
		},
		{
			name:    "a relative tools dir is refused",
			env:     map[string]string{EnvToolsDir: "fake/bin", EnvLid: "/fake/lid", EnvPowerSource: "ac"},
			wantErr: true,
		},
		{
			name:    "a blank hotspot is refused",
			env:     map[string]string{"HOME": "/home/owner", EnvHotspot: "   "},
			wantErr: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			getenv := func(k string) string { return tc.env[k] }

			// Act.
			got, err := ConfigFromEnv(getenv)

			// Assert.
			if tc.wantErr {
				if err == nil {
					t.Fatalf("ConfigFromEnv() = %+v, want a refusal", got)
				}
				return
			}
			if err != nil {
				t.Fatalf("ConfigFromEnv() error = %v", err)
			}
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("ConfigFromEnv() = %+v, want %+v", got, tc.want)
			}
		})
	}
}

func TestSameNetworkFoldsTheApostropheSpellings(t *testing.T) {
	cases := []struct {
		a, b string
		want bool
	}{
		{"Someone's iPhone", "Someone’s iPhone", true},
		{"Someone‘s iPhone", "Someone’s iPhone", true},
		{"Someone's iPhone", "Someones iPhone", false},
		{"", "", false},
	}
	for _, tc := range cases {
		t.Run(tc.a+"|"+tc.b, func(t *testing.T) {
			// Act, Assert.
			if got := sameNetwork(tc.a, tc.b); got != tc.want {
				t.Fatalf("sameNetwork(%q, %q) = %v, want %v", tc.a, tc.b, got, tc.want)
			}
		})
	}
}

func TestPreferredNetworksParsesTheIndentedSSIDs(t *testing.T) {
	// Act.
	got := preferredNetworks("Preferred networks on en0:\n\tHome\n\tSomeone’s iPhone\n")

	// Assert.
	if len(got) != 2 || got[0] != "Home" || got[1] != "Someone’s iPhone" {
		t.Fatalf("preferredNetworks = %q, want the two saved SSIDs", got)
	}
}

// homeTools is the production layout for HOME /home/owner.
func homeTools() Tools {
	t := DefaultTools()
	t.WifiApp = WifiAppIn("/home/owner")
	return t
}
