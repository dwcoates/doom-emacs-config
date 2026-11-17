package hostpower

import "testing"

func TestParseEnergyMode(t *testing.T) {
	// pmset is `pmset -g`'s output naming the energy mode MODE, or none when
	// MODE is empty.
	pmset := func(mode string) string {
		out := "System-wide power settings:\n Sleep On Power Button 1\nCurrently in use:\n sleep                0\n"
		if mode != "" {
			out += " powermode            " + mode + "\n"
		}
		return out + " hibernatemode        3\n"
	}
	cases := []struct{ name, out, want string }{
		{name: "Low Power Mode", out: pmset(EnergyModeLowPower), want: "1"},
		{name: "High Power Mode", out: pmset(EnergyModeHighPower), want: "2"},
		{name: "automatic", out: pmset("0"), want: "0"},
		{name: "a machine with no energy mode names none", out: pmset(""), want: ""},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := ParseEnergyMode(tc.out)

			// Assert
			if got != tc.want {
				t.Fatalf("ParseEnergyMode = %q, want %q", got, tc.want)
			}
		})
	}
}
