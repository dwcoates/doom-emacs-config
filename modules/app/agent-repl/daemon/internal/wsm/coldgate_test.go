package wsm

import (
	"context"
	"testing"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func gateRecord(vendor string, tokens uint64) ColdGateRecord {
	return ColdGateRecord{
		VendorSessionID: vendor,
		Cold:            &conversationv1.SessionCold{ContextTokens: tokens, LastRequestAtMs: 1800},
		ConfigDir:       "/accounts/work",
	}
}

func TestColdGateRecord(t *testing.T) {
	tests := []struct {
		name      string
		set       []ColdGateRecord
		clear     bool
		want      ColdGateRecord
		wantFound bool
	}{
		{name: "a workspace with no gate recorded has none"},
		{name: "a recorded gate is read back whole", set: []ColdGateRecord{gateRecord("vendor-1", 120_000)}, want: gateRecord("vendor-1", 120_000), wantFound: true},
		{name: "a later gate replaces it", set: []ColdGateRecord{gateRecord("vendor-1", 120_000), gateRecord("vendor-2", 90_000)}, want: gateRecord("vendor-2", 90_000), wantFound: true},
		{name: "a cleared gate is gone", set: []ColdGateRecord{gateRecord("vendor-1", 120_000)}, clear: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s).ID
			for _, rec := range tt.set {
				if err := s.SetColdGate(context.Background(), ws, rec); err != nil {
					t.Fatalf("SetColdGate: %v", err)
				}
			}
			if tt.clear {
				if cleared, err := s.ClearColdGate(context.Background(), ws); err != nil || !cleared {
					t.Fatalf("ClearColdGate = %v, %v; want the gate cleared", cleared, err)
				}
			}

			// Act
			got, found, err := s.ColdGate(context.Background(), ws)

			// Assert
			if err != nil {
				t.Fatalf("ColdGate: %v", err)
			}
			if found != tt.wantFound || got.VendorSessionID != tt.want.VendorSessionID || got.ConfigDir != tt.want.ConfigDir || !proto.Equal(got.Cold, tt.want.Cold) {
				t.Fatalf("ColdGate = %+v, %v; want %+v, %v", got, found, tt.want, tt.wantFound)
			}
		})
	}
}

func TestClearColdGateOfNoneAnswersNothingCleared(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID

	// Act
	cleared, err := s.ClearColdGate(context.Background(), ws)

	// Assert
	if err != nil || cleared {
		t.Fatalf("ClearColdGate = %v, %v; want nothing cleared", cleared, err)
	}
}

func TestSetColdGateRefusesAGateNamingNoConversation(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID

	// Act
	err := s.SetColdGate(context.Background(), ws, ColdGateRecord{Cold: &conversationv1.SessionCold{}})

	// Assert
	if err == nil {
		t.Fatal("SetColdGate with no conversation succeeded, want a refusal")
	}
}
