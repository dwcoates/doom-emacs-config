package shimfacts

import (
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"google.golang.org/protobuf/proto"
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

func TestOpeningPage(t *testing.T) {
	tests := []struct {
		name      string
		req       *shimv1.WatchAgentRequest
		wantFloor bool
		wantErr   string
	}{
		{name: "a tail-only open gets nothing", req: &shimv1.WatchAgentRequest{Opening: &shimv1.WatchAgentRequest_TailOnly{TailOnly: &shimv1.WatchAgentTailOnly{}}}},
		{name: "an open naming neither gets the empty floor", req: &shimv1.WatchAgentRequest{}, wantFloor: true},
		{name: "a catch-up of a fresh book is refused", req: &shimv1.WatchAgentRequest{
			Target:  &conversationv1.AgentId{Value: "a1"},
			Opening: &shimv1.WatchAgentRequest_KnownThrough{KnownThrough: &conversationv1.HistoryPointer{Value: "p-3"}},
		}, wantErr: "no catch-up for a1 known through p-3"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			page, err := OpeningPage(tc.req)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) || page != nil {
					t.Fatalf("OpeningPage = %v, %v; want the refusal %q", page, err, tc.wantErr)
				}
				return
			}
			want := TailOnlyPage()
			if tc.wantFloor {
				want = EmptyFloorPage()
			}
			if err != nil || !proto.Equal(page, want) {
				t.Fatalf("OpeningPage = %v, %v; want %v", page, err, want)
			}
		})
	}
}
