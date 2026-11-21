package persistentwifi

import (
	"reflect"
	"runtime"
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
			want: Config{Platform: "darwin", Tools: homeTools(), FreezeApps: appfreeze.DefaultApps},
		},
		{
			name: "the freeze list names the applications",
			env:  map[string]string{"HOME": "/home/owner", appfreeze.EnvApps: "Slack, Discord"},
			want: Config{Platform: "darwin", Tools: homeTools(), FreezeApps: []string{"Slack", "Discord"}},
		},
		{
			name:    "a freeze list naming a terminal is refused",
			env:     map[string]string{"HOME": "/home/owner", appfreeze.EnvApps: "Slack,Terminal"},
			wantErr: true,
		},
		{
			name: "a fake host freezes nothing unless told to",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open", EnvPowerSource: "ac"},
			want: Config{Platform: "darwin", Tools: ToolsIn("/fake/bin"), FakeLid: "open", FakePowerSource: "ac"},
		},
		{
			name: "a fake host freezes what it is told to",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open", EnvPowerSource: "ac", appfreeze.EnvApps: "Slack"},
			want: Config{Platform: "darwin", Tools: ToolsIn("/fake/bin"), FakeLid: "open", FakePowerSource: "ac", FreezeApps: []string{"Slack"}},
		},
		{
			name: "a tools dir resolves every tool there by its base name, with its fake lid",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "/fake/lid", EnvPowerSource: "ac"},
			want: Config{Platform: "darwin", Tools: Tools{
				Sudo: "/fake/bin/sudo", Pmset: "/fake/bin/pmset", Ipconfig: "/fake/bin/ipconfig",
				Networksetup: "/fake/bin/networksetup", WifiUtil: "/fake/bin/wifi-util",
				Caffeinate: "/fake/bin/caffeinate", Brightness: "/fake/bin/mac-brightness", Ps: "/fake/bin/ps",
				Pkill: "/fake/bin/pkill", Open: "/fake/bin/open", SystemProfiler: "/fake/bin/system_profiler",
				WifiApp: "/fake/bin/AgentReplWifi.app",
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
			want: Config{Platform: "darwin", Tools: ToolsIn("/fake/bin"), FakeLid: "open", FakePowerSource: "battery"},
		},
		{
			name: "an absolute power source socket is a socket power source",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open", EnvPowerSource: "/fake/power"},
			want: Config{Platform: "darwin", Tools: ToolsIn("/fake/bin"), FakeLid: "open", FakePowerSource: "/fake/power"},
		},
		{
			name:    "a relative power source socket is refused",
			env:     map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open", EnvPowerSource: "fake/power"},
			wantErr: true,
		},
		{
			name: "a lid state is a fixed lid",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open", EnvPowerSource: "ac"},
			want: Config{Platform: "darwin", Tools: ToolsIn("/fake/bin"), FakeLid: "open", FakePowerSource: "ac"},
		},
		{
			name:    "a relative lid socket is refused",
			env:     map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "fake/lid", EnvPowerSource: "ac"},
			wantErr: true,
		},
		{
			name: "a hotspot override names the hotspot",
			env:  map[string]string{"HOME": "/home/owner", EnvHotspot: "Other Phone"},
			want: Config{Platform: "darwin", Tools: homeTools(), Hotspot: "Other Phone", FreezeApps: appfreeze.DefaultApps},
		},
		{
			name: "the known-network step set true is on",
			env:  map[string]string{"HOME": "/home/owner", EnvJoinKnown: "true"},
			want: Config{Platform: "darwin", Tools: homeTools(), FreezeApps: appfreeze.DefaultApps},
		},
		{
			name: "the known-network step set false is off",
			env:  map[string]string{"HOME": "/home/owner", EnvJoinKnown: "false"},
			want: Config{Platform: "darwin", Tools: homeTools(), FreezeApps: appfreeze.DefaultApps, KnownJoinOff: true},
		},
		{
			name:    "the known-network step set to neither true nor false is refused",
			env:     map[string]string{"HOME": "/home/owner", EnvJoinKnown: "no"},
			wantErr: true,
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
			got, err := configFromEnv("darwin", getenv)

			// Assert.
			if tc.wantErr {
				if err == nil {
					t.Fatalf("configFromEnv() = %+v, want a refusal", got)
				}
				return
			}
			if err != nil {
				t.Fatalf("configFromEnv() error = %v", err)
			}
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("configFromEnv() = %+v, want %+v", got, tc.want)
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

func TestConfigFromEnvNamesThisPlatform(t *testing.T) {
	// Act
	got, err := ConfigFromEnv(func(k string) string {
		if k == "HOME" {
			return "/home/u"
		}
		return ""
	})

	// Assert
	if err != nil || got.Platform != runtime.GOOS {
		t.Fatalf("ConfigFromEnv() = (%+v, %v), want the platform %s", got, err, runtime.GOOS)
	}
}

func TestConfigFromEnvOnLinux(t *testing.T) {
	tests := []struct {
		name string
		env  map[string]string
		want Config
	}{
		{name: "the freedesktop tools", env: map[string]string{appfreeze.EnvApps: "none"},
			want: Config{Platform: "linux", Linux: DefaultLinuxTools()}},
		{name: "fake tools from one directory", env: map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open", EnvPowerSource: "ac"},
			want: Config{Platform: "linux", Linux: LinuxToolsIn("/fake/bin"), FakeLid: "open", FakePowerSource: "ac"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := configFromEnv("linux", func(k string) string { return tt.env[k] })

			// Assert
			if err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("configFromEnv(linux) = (%+v, %v), want %+v", got, err, tt.want)
			}
		})
	}
}

func TestKnownJoinOff(t *testing.T) {
	cases := []struct {
		name    string
		value   string
		wantOff bool
		wantErr string
	}{
		{name: "unset leaves the step on", value: ""},
		{name: "true leaves the step on", value: "true"},
		{name: "false turns the step off", value: "false", wantOff: true},
		{name: "anything else is refused, naming the variable and the value", value: "False",
			wantErr: `persistentwifi: AGENT_REPL_PERSISTENT_WIFI_JOIN_KNOWN="False" is neither true nor false`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			off, err := knownJoinOff(tc.value)

			// Assert.
			if tc.wantErr != "" {
				if err == nil || err.Error() != tc.wantErr {
					t.Fatalf("knownJoinOff(%q) error = %v, want %q", tc.value, err, tc.wantErr)
				}
				return
			}
			if err != nil || off != tc.wantOff {
				t.Fatalf("knownJoinOff(%q) = (%t, %v), want (%t, nil)", tc.value, off, err, tc.wantOff)
			}
		})
	}
}
