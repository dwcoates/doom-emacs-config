package main

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/viewquiet"
)

type recordingTopbar struct {
	took []*agentreplv1.PersistentWifiState
}

func (r *recordingTopbar) SetPersistentWifi(s *agentreplv1.PersistentWifiState) {
	r.took = append(r.took, s)
}

func TestWifiStandingReceiverMovesTheViewGateWithTheQuietStateAndHandsTheChipEveryStanding(t *testing.T) {
	quiet := &agentreplv1.PersistentWifiState{Quiet: &agentreplv1.PersistentWifiQuiet{}}
	open := &agentreplv1.PersistentWifiState{}
	unreachable := &agentreplv1.PersistentWifiState{ServiceUnreachable: &agentreplv1.PersistentWifiServiceUnreachable{Detail: "down"}}
	tests := []struct {
		name      string
		standings []*agentreplv1.PersistentWifiState
		wantQuiet bool
	}{
		{"a quiet standing stops the views", []*agentreplv1.PersistentWifiState{quiet}, true},
		{"a standing without the quiet state lifts them", []*agentreplv1.PersistentWifiState{quiet, open}, false},
		{"an unreachable service lifts them, since nothing holds the machine quiet", []*agentreplv1.PersistentWifiState{quiet, unreachable}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			gate := viewquiet.New(nil)
			topbar := &recordingTopbar{}
			receive := wifiStandingReceiver(gate, topbar)

			// Act
			for _, s := range tt.standings {
				receive(s)
			}

			// Assert
			if gate.Quiet() != tt.wantQuiet {
				t.Fatalf("gate quiet = %v, want %v", gate.Quiet(), tt.wantQuiet)
			}
			if len(topbar.took) != len(tt.standings) || topbar.took[len(topbar.took)-1] != tt.standings[len(tt.standings)-1] {
				t.Fatalf("the chip took %v, want every standing in order", topbar.took)
			}
		})
	}
}
