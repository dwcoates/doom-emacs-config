package persistentwifi

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

func TestOutcomeArmNamesTheSetArm(t *testing.T) {
	cases := []struct {
		name string
		got  string
		want string
	}{
		{"a hotspot outcome", hotspotArm(&agentreplv1.UpdatePersistentWifiModeHotspot{
			Outcome: &agentreplv1.UpdatePersistentWifiModeHotspot_NotOnHotspot{NotOnHotspot: &agentreplv1.UpdatePersistentWifiModeHotspotNotOnHotspot{}},
		}), "not_on_hotspot"},
		{"a display outcome", displayArm(&agentreplv1.UpdatePersistentWifiModeDisplay{
			Outcome: &agentreplv1.UpdatePersistentWifiModeDisplay_Dimmed{Dimmed: &agentreplv1.UpdatePersistentWifiModeDisplayDimmed{}},
		}), "dimmed"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Assert.
			if tc.got != tc.want {
				t.Fatalf("arm = %q, want %q", tc.got, tc.want)
			}
		})
	}
}

func TestOutcomeArmPanicsOnAnUnsetOutcome(t *testing.T) {
	// Assert.
	defer func() {
		if recover() == nil {
			t.Fatal("outcomeArm() did not panic on an unset outcome")
		}
	}()

	// Act.
	outcomeArm(&agentreplv1.UpdatePersistentWifiModeDisplay{})
}

func TestOutcomeArmPanicsOnAMessageWithNoOutcome(t *testing.T) {
	// Assert.
	defer func() {
		if recover() == nil {
			t.Fatal("outcomeArm() did not panic on a message with no outcome oneof")
		}
	}()

	// Act.
	outcomeArm(&agentreplv1.PersistentWifiState{})
}
