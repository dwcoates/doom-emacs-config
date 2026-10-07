package persistentwifi

import (
	"testing"
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
			want: Config{Tools: DefaultTools()},
		},
		{
			name: "a tools dir resolves every tool there by its base name",
			env:  map[string]string{EnvToolsDir: "/fake/bin"},
			want: Config{Tools: Tools{
				Sudo: "/fake/bin/sudo", Pmset: "/fake/bin/pmset", Ipconfig: "/fake/bin/ipconfig",
				Networksetup: "/fake/bin/networksetup", WifiUtil: "/fake/bin/wifi-util",
				Brightness: "/fake/bin/mac-brightness",
			}},
		},
		{
			name: "a hotspot override names the hotspot",
			env:  map[string]string{EnvHotspot: "Other Phone"},
			want: Config{Tools: DefaultTools(), Hotspot: "Other Phone"},
		},
		{
			name:    "a relative tools dir is refused",
			env:     map[string]string{EnvToolsDir: "fake/bin"},
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
			if got != tc.want {
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
