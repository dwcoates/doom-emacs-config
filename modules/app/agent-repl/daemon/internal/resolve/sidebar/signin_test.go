package sidebar_test

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/resolve/footer"
)

// authFailure is the vendor rejecting the credential.
func authFailure() *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
		Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}}}}}
}

func TestASignInAnswersOnlyAnAccountFailure(t *testing.T) {
	cases := []struct {
		name    string
		failure *conversationv1.AgentFailure
		want    string
	}{
		{name: "a rejected credential draws ready", failure: authFailure(), want: "ready"},
		{name: "billing stands", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
			Kind: &conversationv1.ApiRequestFailed_BillingError{BillingError: &conversationv1.ApiBillingError{}}}}}, want: "vendor_blocked"},
		{name: "a dead query stands", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}}}, want: "turn_died"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t))
			failedTurn(r, tc.failure)

			// Act.
			r.OnSignedIn(theWS)

			// Assert.
			if got := statusName(onlyRow(t, r)); got != tc.want {
				t.Fatalf("status = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestTheNextTurnAfterASignInIsDrawnAsUsual(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	failedTurn(r, authFailure())
	r.OnSignedIn(theWS)

	// Act.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert.
	if got := statusName(onlyRow(t, r)); got == "ready" || got == "turn_failed" || got == "vendor_blocked" {
		t.Fatalf("status = %q, want the new turn's own arm", got)
	}
}

func TestASignInIsRecordedOnTheRoster(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	failedTurn(r, authFailure())

	// Act.
	r.OnSignedIn(theWS)

	// Assert.
	for _, rec := range r.surfaces.Records() {
		if rec.Operation == "daemon.sidebar.signed_in" && rec.Level == "info" {
			return
		}
	}
	t.Fatalf("no INFO daemon.sidebar.signed_in record in %+v", r.surfaces.Records())
}
