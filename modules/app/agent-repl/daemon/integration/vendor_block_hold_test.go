//go:build integration

package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// rateLimitVerdict is a five-hour rate-limit status with the given verdict.
func rateLimitVerdict(rejected bool) *conversationv1.SessionUpdate {
	status := &conversationv1.SessionRateLimitStatus{
		RateLimitType: &conversationv1.SessionRateLimitType{
			Window: &conversationv1.SessionRateLimitType_FiveHour{FiveHour: &conversationv1.SessionRateLimitWindowFiveHour{}},
		},
		Status: &conversationv1.SessionRateLimitStatus_Allowed{Allowed: &conversationv1.SessionRateLimitAllowed{}},
	}
	if rejected {
		status.Status = &conversationv1.SessionRateLimitStatus_Rejected{Rejected: &conversationv1.SessionRateLimitRejected{}}
	}
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_RateLimitStatus{RateLimitStatus: status}}
}

// TestAPromptDuringAUsageLimitIsHeldAfterReconnectAndDeliveredWhenTheVendorServes
// is the owner's ruling (2026-10-06) end to end through a real daemon and the
// fake shim: a prompt sent while a mid-session usage limit stands is held
// after reconnect with no verdict and never reaches the shim; the vendor's
// next allowed verdict releases it and it is delivered.
func TestAPromptDuringAUsageLimitIsHeldAfterReconnectAndDeliveredWhenTheVendorServes(t *testing.T) {
	t.Parallel()
	// Arrange: an opened workspace whose vendor refuses the session.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	holds := f.d.WatchHolds(f.ws)
	f.shim.PushSessionUpdate(rateLimitVerdict(true))
	awaitFooter(t, f, footer, "the footer paints vendor_fault · usage_limit", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetVendorFault().GetUsageLimit() != nil
	})

	// Act: a prompt is sent under the block.
	resp := f.submit("after the limit", "k-vendor-block", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	turn := resp.GetSuccess().GetTurn().GetTurn()

	// Assert: held after reconnect, unclassified, and not sent.
	tray := awaitView(t, f, holds, "the after-reconnect hold", func(tray *frontendv1.DaemonHoldTray) bool {
		return promptHeldEntry(tray, turn).GetReconnect() != nil
	})
	if entry := promptHeldEntry(tray, turn); entry.GetDaemonHeld() == nil {
		t.Fatalf("held entry = %v, want the daemon_held verdict: nothing classified it", entry)
	}
	expectRPCCount(t, f.shim, harness.RPCStartTurn, 0, harness.ProbeWindow)

	// Act: the vendor serves again.
	f.shim.PushSessionUpdate(rateLimitVerdict(false))

	// Assert: the held prompt is delivered.
	started := f.shim.ExpectStartTurn()
	if started.GetTurn().GetValue() != turn.GetValue() {
		t.Fatalf("StartTurn turn = %q, want the held %q", started.GetTurn().GetValue(), turn.GetValue())
	}
}
