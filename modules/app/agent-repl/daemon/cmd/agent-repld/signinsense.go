package main

import (
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/ladder"
	"agent-repld/internal/sessionwatcher"
)

// signInRequester is the topbar's half of a needed sign-in: every strip
// spending as the workspace's account asks the page to open the login.
type signInRequester interface {
	RequestSignIn(ws ids.WorkspaceID)
}

// signInSensingFooter is the session watcher's footer sink, which also asks
// for the login when an agent's turn ends on a failure a sign-in cures
// (ladder.FailureCuredBySignIn): the vendor rejected the account's
// credential, so the login opens by itself instead of waiting for the reader
// to find a control. It hangs off the footer's terminal edge because every
// agent terminal, live or first read from a catch-up page, reaches the footer
// through that one call (sessionwatcher endAgentRunLocked).
type signInSensingFooter struct {
	sessionwatcher.FooterSink
	topbar signInRequester
}

// OnAgentTerminal hands the terminal to the footer, then asks for the login
// when a sign-in cures its failure.
func (f signInSensingFooter) OnAgentTerminal(ws ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) {
	f.FooterSink.OnAgentTerminal(ws, agent, turn, success, failure)
	if ladder.FailureCuredBySignIn(failure) {
		f.topbar.RequestSignIn(ws)
	}
}
