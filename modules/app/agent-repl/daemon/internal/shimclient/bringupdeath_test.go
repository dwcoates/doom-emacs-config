package shimclient

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestABringUpDeathIsAStandDownOnlyWhenTheSweepKilledIt(t *testing.T) {
	tests := []struct {
		name string
		attr *KillAttribution
		want bool
	}{
		{name: "the supervisor's stand-down sweep", attr: &KillAttribution{Actor: ActorStandDown}, want: true},
		{name: "another kill this daemon asked for", attr: &KillAttribution{Actor: "shimclient.bringup"}, want: false},
		{name: "a death nobody asked for", attr: nil, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			err := &BringUpDeathError{Exit: ExitInfo{PID: 7, Code: -1, Signal: "killed", Attribution: tt.attr}}

			// Act
			got := errors.Is(err, ErrStandingDown)

			// Assert
			if got != tt.want {
				t.Fatalf("errors.Is(death, ErrStandingDown) = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestOnlyNetworkFaults(t *testing.T) {
	network := &conversationv1.SessionFault{Kind: &conversationv1.SessionFault_NetworkUnreachable{NetworkUnreachable: &conversationv1.SessionFaultNetworkUnreachable{}}}
	store := &conversationv1.SessionFault{Kind: &conversationv1.SessionFault_StoreUnreachable{StoreUnreachable: &conversationv1.SessionFaultStoreUnreachable{}}}
	tests := []struct {
		name   string
		faults []*conversationv1.SessionFault
		want   bool
	}{
		{name: "no faults", want: false},
		{name: "a network fault alone", faults: []*conversationv1.SessionFault{network}, want: true},
		{name: "a network fault beside a store fault", faults: []*conversationv1.SessionFault{network, store}, want: false},
		{name: "a store fault alone", faults: []*conversationv1.SessionFault{store}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := onlyNetworkFaults(tt.faults)

			// Assert
			if got != tt.want {
				t.Fatalf("onlyNetworkFaults = %v, want %v", got, tt.want)
			}
		})
	}
}
