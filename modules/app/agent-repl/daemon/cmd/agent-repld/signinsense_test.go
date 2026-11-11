package main

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/sessionwatcher"
)

// terminalFooter records every terminal handed to the footer.
type terminalFooter struct {
	sessionwatcher.FooterSink
	terminals int
}

func (f *terminalFooter) OnAgentTerminal(ids.WorkspaceID, *conversationv1.AgentId, *ids.TurnID, *conversationv1.AgentSuccess, *conversationv1.AgentFailure) {
	f.terminals++
}

// requestRecorder records every workspace a sign-in was requested for.
type requestRecorder struct{ asked []ids.WorkspaceID }

func (r *requestRecorder) RequestSignIn(ws ids.WorkspaceID) { r.asked = append(r.asked, ws) }

// apiFailure is a turn that ended on the API's refusal of KIND.
func apiFailure(kind *conversationv1.ApiRequestFailed) *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: kind}}
}

func TestATurnRejectedForTheCredentialAsksForTheLogin(t *testing.T) {
	tests := []struct {
		name    string
		failure *conversationv1.AgentFailure
		want    int
	}{
		{"a rejected credential", apiFailure(&conversationv1.ApiRequestFailed{
			Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}}}), 1},
		{"an organization the credential may not use", apiFailure(&conversationv1.ApiRequestFailed{
			Kind: &conversationv1.ApiRequestFailed_OauthOrgNotAllowed{OauthOrgNotAllowed: &conversationv1.ApiOauthOrgNotAllowed{}}}), 1},
		{"an overloaded API", apiFailure(&conversationv1.ApiRequestFailed{
			Kind: &conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{}}}), 0},
		{"a turn that succeeded", nil, 0},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			footer, requests := &terminalFooter{}, &requestRecorder{}
			sink := signInSensingFooter{FooterSink: footer, topbar: requests}

			// Act
			sink.OnAgentTerminal("w1", &conversationv1.AgentId{Value: "a"}, nil, nil, tc.failure)

			// Assert
			if footer.terminals != 1 || len(requests.asked) != tc.want {
				t.Fatalf("footer terminals %d, login requests %v, want 1 and %d", footer.terminals, requests.asked, tc.want)
			}
		})
	}
}
