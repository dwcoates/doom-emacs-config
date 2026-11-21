package shimfacts

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestServedEntry(t *testing.T) {
	frame := &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_AgentFrame{AgentFrame: &conversationv1.AgentFrame{}}}
	tests := []struct {
		name      string
		turn      string
		placeMs   int64
		wantTurn  string
		wantPlace int64
	}{
		{name: "a stamped and placed entry carries both", turn: "t1", placeMs: 42, wantTurn: "t1", wantPlace: 42},
		{name: "an unstamped entry carries no turn", placeMs: 42, wantPlace: 42},
		{name: "an unplaced entry carries no place", turn: "t1", wantTurn: "t1"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			at := ServedEntry("p-1", tc.turn, tc.placeMs, frame)

			// Assert
			if at.GetAt().GetValue() != "p-1" || at.GetEntry() != frame {
				t.Fatalf("entry = %+v, want the frame at p-1", at)
			}
			if (at.GetTurn() == nil) != (tc.wantTurn == "") || at.GetTurn().GetValue() != tc.wantTurn {
				t.Fatalf("turn = %v, want %q", at.GetTurn(), tc.wantTurn)
			}
			if (at.GetPlace() == nil) != (tc.wantPlace == 0) || at.GetRecordedPlace().GetAtMs() != tc.wantPlace {
				t.Fatalf("place = %v, want %d", at.GetPlace(), tc.wantPlace)
			}
		})
	}
}

func TestStartedSessionStatesTheFakesDefaultFacts(t *testing.T) {
	// Act
	s := StartedSession("v-1", "sha-1", "opus")

	// Assert
	if s.GetVendorSessionId() != "v-1" || s.GetRuntime().GetShimBuildSha() != "sha-1" || s.GetRuntime().GetSdkVersion() != SDKVersion {
		t.Fatalf("started = %+v, want v-1 on sha-1 at %s", s, SDKVersion)
	}
	if s.GetEffectiveModel().GetName() != "opus" || s.GetPermissionMode().GetAuto() == nil {
		t.Fatalf("started = %+v, want opus in auto mode", s)
	}
	if len(s.GetModelCatalog()) != len(DefaultCatalog()) || s.GetLiveWork() != nil {
		t.Fatalf("started = %+v, want the default catalog and no live work", s)
	}
}
