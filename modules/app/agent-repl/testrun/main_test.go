package main

import (
	"testing"

	"agentrepl/testrun/internal/ramdisk"
)

func TestRunConditions(t *testing.T) {
	cases := []struct {
		name   string
		prefix []string
		want   string
	}{
		{name: "a run on the RAM disk names its units' I/O tier", prefix: ramdisk.UnitPrefix(), want: "ramdisk-io-" + ramdisk.UnitIOTier},
		{name: "a run on the disk names the tier it inherited", prefix: nil, want: "disk-io-inherited"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := runConditions(tc.prefix)

			// Assert
			if got != tc.want {
				t.Fatalf("runConditions = %q, want %q", got, tc.want)
			}
		})
	}
}
