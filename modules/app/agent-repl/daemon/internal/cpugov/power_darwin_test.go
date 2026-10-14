//go:build darwin

package cpugov

import "testing"

func TestTheHostReadsItsPowerSource(t *testing.T) {
	// Act.
	got, err := (&Host{}).Power()

	// Assert.
	if err != nil || (got != PowerAC && got != PowerBattery) {
		t.Fatalf("Power = %v, %v; want AC or battery", got, err)
	}
}
