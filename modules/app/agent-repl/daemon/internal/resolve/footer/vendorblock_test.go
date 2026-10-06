package footer

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
)

// servesRecorder collects the vendor-serves edges the resolver tells.
type servesRecorder struct {
	edges []string
}

func (s *servesRecorder) listen(ws ids.WorkspaceID, was string) {
	s.edges = append(s.edges, string(ws)+":"+was)
}

// rejectedRateLimit is a rate-limit verdict that refuses the session.
func rejectedRateLimit() *conversationv1.SessionUpdate {
	update := rateLimitStatus(fiveHourWindow(), 100, 5*time.Hour)
	update.GetRateLimitStatus().Status = &conversationv1.SessionRateLimitStatus_Rejected{
		Rejected: &conversationv1.SessionRateLimitRejected{},
	}
	return update
}

// usageLimited stands a usage-limit block.
func usageLimited(h *harness) { h.r.OnSessionUpdate(testWS, rejectedRateLimit()) }

// authBlocked stands an authentication block from the turn's own terminal.
func authBlocked(h *harness) {
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, authFailure())
	h.r.SetTurn(testWS, nil)
}

// apiRetrying stands the vendor's retry of the running turn's call.
func apiRetrying(h *harness) {
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnApiError(testWS, mainAgent, &conversationv1.ApiRequestFailed{Message: "overloaded"})
}

func TestVendorBlockNamesTheStandingMidSessionBlock(t *testing.T) {
	tests := []struct {
		name  string
		stand func(h *harness)
		want  string
	}{
		{name: "a rejected rate-limit verdict", stand: usageLimited, want: VendorBlockUsageLimit},
		{name: "an authentication refusal", stand: authBlocked, want: VendorBlockAuth},
		{name: "a retry holding the running turn", stand: apiRetrying, want: VendorBlockAPIRetrying},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			tc.stand(h)

			// Act
			got, ok := h.r.VendorBlock(testWS)

			// Assert
			if !ok || got != tc.want {
				t.Fatalf("VendorBlock = (%q, %v), want (%q, true)", got, ok, tc.want)
			}
		})
	}
}

func TestVendorBlockIsFalseWithNoBlockStanding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	got, ok := h.r.VendorBlock(testWS)

	// Assert
	if ok {
		t.Fatalf("VendorBlock = (%q, true) with nothing standing", got)
	}
}

func TestVendorBlockIsFalseForAnUnseenWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, ok := h.r.VendorBlock(ids.WorkspaceID("never-seen"))

	// Assert
	if ok {
		t.Fatalf("VendorBlock reports a block for a workspace the footer never saw")
	}
}

func TestTheVendorServesEdgeIsToldOnEveryFactThatLiftsTheBlock(t *testing.T) {
	tests := []struct {
		name  string
		stand func(h *harness)
		lift  func(h *harness)
		want  string
	}{
		{
			name:  "an allowed rate-limit verdict lifts a usage limit",
			stand: usageLimited,
			lift: func(h *harness) {
				h.r.OnSessionUpdate(testWS, rateLimitStatus(fiveHourWindow(), 10, 5*time.Hour))
			},
			want: string(testWS) + ":" + VendorBlockUsageLimit,
		},
		{
			name:  "a session start lifts an authentication block",
			stand: authBlocked,
			lift: func(h *harness) {
				h.r.OnSessionStarted(testWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-2"})
			},
			want: string(testWS) + ":" + VendorBlockAuth,
		},
		{
			name:  "a new turn opening lifts a usage limit",
			stand: usageLimited,
			lift:  func(h *harness) { h.r.SetTurn(testWS, &TurnStarted{At: instant}) },
			want:  string(testWS) + ":" + VendorBlockUsageLimit,
		},
		{
			name:  "the retried call answered lifts an api retry",
			stand: apiRetrying,
			lift:  func(h *harness) { h.r.OnActivity(testWS, mainAgent, thinkingActivity("th-1")) },
			want:  string(testWS) + ":" + VendorBlockAPIRetrying,
		},
		{
			name:  "the retried turn ending lifts an api retry",
			stand: apiRetrying,
			lift:  func(h *harness) { h.r.SetTurn(testWS, nil) },
			want:  string(testWS) + ":" + VendorBlockAPIRetrying,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			rec := &servesRecorder{}
			h := newHarness(t, WithVendorServes(rec.listen))
			connected(h)
			tc.stand(h)

			// Act
			tc.lift(h)

			// Assert
			if len(rec.edges) != 1 || rec.edges[0] != tc.want {
				t.Fatalf("vendor-serves edges = %v, want [%s]", rec.edges, tc.want)
			}
		})
	}
}

func TestTheVendorServesEdgeIsNotToldWhileTheBlockStands(t *testing.T) {
	tests := []struct {
		name  string
		after func(h *harness)
	}{
		{name: "the shim link dies", after: func(h *harness) { h.r.OnLink(testWS, shimclient.LinkDead) }},
		{name: "the network becomes unreachable", after: func(h *harness) {
			h.r.OpenFault(testWS, faultOf(t, "net", health.KindNetworkUnreachable, false))
		}},
		{name: "a second rejected verdict arrives", after: func(h *harness) { h.r.OnSessionUpdate(testWS, rejectedRateLimit()) }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			rec := &servesRecorder{}
			h := newHarness(t, WithVendorServes(rec.listen))
			connected(h)
			usageLimited(h)

			// Act
			tc.after(h)

			// Assert
			if len(rec.edges) != 0 {
				t.Fatalf("vendor-serves edges = %v, want none while the usage limit stands", rec.edges)
			}
		})
	}
}

func TestTheVendorServesEdgeIsNotToldWhenNoBlockStood(t *testing.T) {
	// Arrange
	rec := &servesRecorder{}
	h := newHarness(t, WithVendorServes(rec.listen))
	connected(h)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Assert
	if len(rec.edges) != 0 {
		t.Fatalf("vendor-serves edges = %v, want none: no block stood", rec.edges)
	}
}

func TestTheVendorServesEdgeIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	usageLimited(h)

	// Act
	h.r.OnSessionStarted(testWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-2"})

	// Assert
	if got := infoRecords(h, "daemon.footer.vendor_serves"); got != 1 {
		t.Fatalf("daemon.footer.vendor_serves INFO records = %d, want 1", got)
	}
}

// infoRecords counts the INFO records of one operation.
func infoRecords(h *harness, operation string) int {
	n := 0
	for _, rec := range h.log.Records() {
		if rec.Level == "info" && rec.Operation == operation {
			n++
		}
	}
	return n
}
