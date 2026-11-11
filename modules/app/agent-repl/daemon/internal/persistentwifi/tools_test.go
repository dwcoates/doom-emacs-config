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
			name: "unset is the production layout and no hotspot",
			env:  map[string]string{},
			want: Config{Tools: DefaultTools(), FreezeApps: appfreeze.DefaultApps},
		},
		{
			name: "the freeze list names the applications",
			env:  map[string]string{appfreeze.EnvApps: "Slack, Discord"},
			want: Config{Tools: DefaultTools(), FreezeApps: []string{"Slack", "Discord"}},
		},
		{
			name:    "a freeze list naming a terminal is refused",
			env:     map[string]string{appfreeze.EnvApps: "Slack,Terminal"},
			wantErr: true,
		},
		{
			name: "a fake host freezes nothing unless told to",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open"},
			want: Config{Tools: ToolsIn("/fake/bin"), FakeLid: "open"},
		},
		{
			name: "a fake host freezes what it is told to",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open", appfreeze.EnvApps: "Slack"},
			want: Config{Tools: ToolsIn("/fake/bin"), FakeLid: "open", FreezeApps: []string{"Slack"}},
		},
		{
			name: "a tools dir resolves every tool there by its base name, with its fake lid",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "/fake/lid"},
			want: Config{Tools: Tools{
				Sudo: "/fake/bin/sudo", Pmset: "/fake/bin/pmset", Ipconfig: "/fake/bin/ipconfig",
				Networksetup: "/fake/bin/networksetup", WifiUtil: "/fake/bin/wifi-util",
				Caffeinate: "/fake/bin/caffeinate", Brightness: "/fake/bin/mac-brightness", Ps: "/fake/bin/ps",
			}, FakeLid: "/fake/lid"},
		},
		{
			name:    "a tools dir without a fake lid is refused",
			env:     map[string]string{EnvToolsDir: "/fake/bin"},
			wantErr: true,
		},
		{
			name:    "a fake lid without a tools dir is refused",
			env:     map[string]string{EnvLid: "/fake/lid"},
			wantErr: true,
		},
		{
			name: "a lid state is a fixed lid",
			env:  map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "open"},
			want: Config{Tools: ToolsIn("/fake/bin"), FakeLid: "open"},
		},
		{
			name:    "a relative lid socket is refused",
			env:     map[string]string{EnvToolsDir: "/fake/bin", EnvLid: "fake/lid"},
			wantErr: true,
		},
		{
			name: "a hotspot override names the hotspot",
			env:  map[string]string{EnvHotspot: "Other Phone"},
			want: Config{Tools: DefaultTools(), Hotspot: "Other Phone", FreezeApps: appfreeze.DefaultApps},
		},
		{
			name:    "a relative tools dir is refused",
			env:     map[string]string{EnvToolsDir: "fake/bin", EnvLid: "/fake/lid"},
			wantErr: true,
		},
		{
			name:    "a blank hotspot is refused",
			env:     map[string]string{EnvHotspot: "   "},
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
