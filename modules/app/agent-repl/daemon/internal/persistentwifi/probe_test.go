package persistentwifi

import "testing"

func TestParseMode(t *testing.T) {
	cases := []struct {
		name       string
		out        string
		on, wantOK bool
	}{
		{name: "sleep disabled is on", out: pmsetOn, on: true, wantOK: true},
		{name: "sleep enabled is off", out: pmsetOff, on: false, wantOK: true},
		{name: "no SleepDisabled line is unreadable", out: "Currently in use:\n standby 1\n", wantOK: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			on, ok := parseMode(tc.out)

			// Assert.
			if on != tc.on || ok != tc.wantOK {
				t.Fatalf("parseMode() = (%v, %v), want (%v, %v)", on, ok, tc.on, tc.wantOK)
			}
		})
	}
}

func TestWifiInterface(t *testing.T) {
	cases := []struct {
		name string
		out  string
		want string
	}{
		{name: "the device after the Wi-Fi port", out: ports, want: "en0"},
		{name: "no Wi-Fi port is no device", out: noWifi, want: ""},
		{name: "a Wi-Fi port with no device line is no device", out: "Hardware Port: Wi-Fi\n\nHardware Port: Ethernet\nDevice: en5\n", want: ""},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := wifiInterface(tc.out)

			// Assert.
			if got != tc.want {
				t.Fatalf("wifiInterface() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestParseSummary(t *testing.T) {
	cases := []struct {
		name string
		out  string
		want link
	}{
		{name: "an active link with a name", out: summary(true, "Home"), want: link{joined: true, name: "Home"}},
		{name: "a withheld name is joined without one", out: summary(true, redactedName), want: link{joined: true}},
		{name: "an inactive link is not joined and carries no name", out: summary(false, "Home"), want: link{}},
		{name: "a nested SSID key is not the network's name", out: summary(true, ""), want: link{joined: true}},
		{name: "no link status is not joined", out: "<dictionary> {\n  SSID : Home\n}\n", want: link{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := parseSummary(tc.out)

			// Assert.
			if got != tc.want {
				t.Fatalf("parseSummary() = %+v, want %+v", got, tc.want)
			}
		})
	}
}
