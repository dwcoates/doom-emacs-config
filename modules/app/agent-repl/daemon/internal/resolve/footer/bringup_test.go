package footer

import (
	"testing"

	"agent-repld/internal/health"
	"agent-repld/internal/shimclient"
)

func TestABringUpOnARouteNeverSeenIsStarting(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, true)

	// Act
	h.r.SetBringingUp(testWS, true)

	// Assert
	if got := h.view(t).GetStrip().GetStatus().GetStarting(); got == nil {
		t.Fatalf("status = %v, want starting", h.view(t).GetStrip().GetStatus())
	}
}

func TestABringUpOnASeenRouteDefersToTheLink(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetBringingUp(testWS, true)

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle: a connected route is proven", got)
	}
}

func TestATurnAcceptedOnARouteNeverSeenHasNoClock(t *testing.T) {
	// Arrange: a cold submit held while the session comes up.
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, true)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Assert: no clock, so no stop control for a turn no session holds.
	if h.view(t).GetStrip().GetClock().TurnStartedAtMs != nil {
		t.Fatal("the clock runs for a turn accepted on a route never seen")
	}
}

func TestTheClockStartsOnceTheRouteIsSeen(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, true)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Act
	h.r.OnLink(testWS, shimclient.LinkConnected)

	// Assert
	if h.view(t).GetStrip().GetClock().TurnStartedAtMs == nil {
		t.Fatal("the clock did not start once the route was seen")
	}
}

// A ROUTE NEVER SEEN UNDER A STANDING AGENT-REPL FAULT is the fault: with no
// link state yet, the fault is the only evidence about the route, so a
// bring-up under it never draws `starting`.
func TestABringUpUnderAStandingAgentReplFaultDrawsTheFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, true)
	h.r.OpenFault(testWS, faultOf(t, "fault-1", health.KindShimStartFailed, false))

	// Act
	h.r.SetBringingUp(testWS, true)

	// Assert
	status := h.view(t).GetStrip().GetStatus()
	if status.GetAgentReplFault().GetStartFailed() == nil {
		t.Fatalf("status = %v, want agent_repl_fault · start_failed over a route never seen", status)
	}
}

// THE STARTING CELL: a previous bring-up's failure first, then the shared
// salient lines, then unpinned.
func TestTheStartingActivityCell(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    string
	}{
		{
			name:    "a bare boot draws unpinned",
			arrange: func(*harness) {},
			want:    "unpinned",
		},
		{
			name: "a previous bring-up's failure stands salient",
			arrange: func(h *harness) {
				h.r.SetStartFailed(testWS, &StartFailed{Detail: "the shim exited"})
			},
			want: "start_failed",
		},
		{
			name: "a standing agent-repl fault stands salient",
			arrange: func(h *harness) {
				h.r.OpenFault(testWS, faultOf(t, "fault-1", health.KindShimDied, false))
			},
			want: "fault",
		},
		{
			name: "a push notification stands salient",
			arrange: func(h *harness) {
				h.r.OnActivity(testWS, mainAgent, notificationFrame("the agent needs you"))
			},
			want: "notification",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(h)

			// Act
			h.r.OnLink(testWS, shimclient.LinkDialing)

			// Assert
			activity := h.view(t).GetStrip().GetStatus().GetStarting().GetActivity()
			got := "unset"
			switch {
			case activity.GetUnpinned() != nil:
				got = "unpinned"
			case activity.GetSalient().GetStartFailed() != nil:
				got = "start_failed"
			case activity.GetSalient().GetFault() != nil:
				got = "fault"
			case activity.GetSalient().GetNotification() != nil:
				got = "notification"
			}
			if got != tt.want {
				t.Fatalf("starting activity = %s (%v), want %s", got, activity, tt.want)
			}
		})
	}
}
