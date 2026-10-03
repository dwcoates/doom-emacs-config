package sidebar_test

import (
	"fmt"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/ladder"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/vocab"
	"claude-repld/internal/wsm"
)

// ONE STATUS, EVERY SURFACE (owner ruling, 2026-09-28). The footer strip, the
// roster row and the tab bar that paints the roster row's arm must ALWAYS make
// the same coarse claim about a workspace. This suite is what makes that
// structural: it walks the fact combinations both resolvers observe — merge
// state x link state x turn state x parked x read — through the REAL footer
// and roster resolvers, fed the same facts, and requires the footer's status
// and the roster's arm to project onto one ladder claim (resolve/ladder).

// repoVocabDir is the checked-in vocabulary, which both resolvers assert
// their arms against at construction.
const repoVocabDir = "../../../../proto/vocab"

// surfaces is one workspace's two status resolvers, fed identically.
type surfaces struct {
	footer  footer.Resolver
	roster  sidebar.Resolver
	logs    []*dlog.TestSurfaces
	turn    ids.TurnID
	session bool
}

// newSurfaces builds both resolvers over the real vocabulary, registers the
// workspace on both, and puts the footer's two client hops up: the roster
// observes no client stream, so a hop down is a footer-only fact outside this
// suite's walk.
func newSurfaces(t *testing.T, session *wsm.Session) *surfaces {
	t.Helper()
	colors, err := vocab.LoadRenderColors(repoVocabDir)
	if err != nil {
		t.Fatalf("LoadRenderColors: %v", err)
	}
	footerLogs, rosterLogs := dlog.NewTestSurfaces(), dlog.NewTestSurfaces()
	// The dwell never elapses inside a subtest, so a momentary status stands
	// for the whole assertion rather than racing a timer.
	f, err := footer.New(colors, footerLogs, footer.WithMomentaryDwell(time.Hour))
	if err != nil {
		t.Fatalf("footer.New: %v", err)
	}
	if err := f.SetWorkspaceDir(theWS, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}
	f.SetParticipants(theWS, true, true)
	r, err := sidebar.New(colors, rosterLogs)
	if err != nil {
		t.Fatalf("sidebar.New: %v", err)
	}
	reg := registry(workspace(string(theWS), "one"))
	if session != nil {
		reg.Sessions = []wsm.Session{*session}
	}
	r.SetRegistry(reg)
	return &surfaces{footer: f, roster: r, logs: []*dlog.TestSurfaces{footerLogs, rosterLogs}, turn: ids.TurnID("turn-1")}
}

// link feeds one link state to both.
func (s *surfaces) link(state shimclient.LinkState) {
	s.footer.OnLink(theWS, state)
	s.roster.OnLink(theWS, state)
}

// startTurn accepts a turn on both.
func (s *surfaces) startTurn() {
	turn := &footer.TurnStarted{At: epoch, Act: footer.ActPrompt}
	s.footer.SetTurn(theWS, turn)
	s.roster.SetTurn(theWS, turn)
}

// endTurn delivers the main agent's terminal to both, and the daemon's own
// close to the roster, which is the one surface told how the turn closed.
func (s *surfaces) endTurn(success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure, how wsm.TurnClose) {
	turn := s.turn
	s.footer.OnAgentTerminal(theWS, agent("main"), &turn, success, failure)
	s.roster.OnAgentTerminal(theWS, agent("main"), &turn, success, failure)
	s.roster.SetTurnEnded(theWS, how)
}

// linkCase is one link state, with whether a session record exists for it.
type linkCase struct {
	name    string
	session bool
	apply   func(s *surfaces)
}

var linkCases = []linkCase{
	// A workspace with no session record and no link seen: the roster's
	// `none`, and a footer with nothing to report. A session record with no
	// link seen yet is left out ON PURPOSE: the roster reads that window as
	// `init` from the durable record, which the footer is not fed — one of
	// the one-sided facts resolve/ladder's package comment lists.
	{name: "no session", session: false, apply: func(*surfaces) {}},
	{name: "dialing", session: true, apply: func(s *surfaces) { s.link(shimclient.LinkDialing) }},
	{name: "connected", session: true, apply: func(s *surfaces) { s.link(shimclient.LinkConnected) }},
	{name: "redialing", session: true, apply: func(s *surfaces) {
		s.link(shimclient.LinkConnected)
		s.link(shimclient.LinkRedialing)
	}},
	{name: "dead after serving", session: true, apply: func(s *surfaces) {
		s.link(shimclient.LinkConnected)
		s.link(shimclient.LinkDead)
	}},
	{name: "dead before serving", session: true, apply: func(s *surfaces) { s.link(shimclient.LinkDead) }},
	{name: "degraded", session: true, apply: func(s *surfaces) {
		s.link(shimclient.LinkConnected)
		s.footer.OnSessionUpdate(theWS, degradedUpdate())
		s.roster.OnSessionUpdate(theWS, degradedUpdate())
	}},
	// THE FAULT DOMAINS (owner ruling, 2026-10-02), each fed to both
	// surfaces from its one source: the vendor-start run's fault to the footer
	// with its state to the roster, and the network fault through the one
	// fault door to both.
	{name: "a vendor start being retried", session: true, apply: func(s *surfaces) {
		s.link(shimclient.LinkConnected)
		s.footer.OpenFault(theWS, domainFault("v-1", health.KindVendorStartRetrying))
		s.roster.SetVendorStart(theWS, sidebar.VendorStartRetrying)
	}},
	{name: "a vendor start that stopped, over the link its stop killed", session: true, apply: func(s *surfaces) {
		s.link(shimclient.LinkConnected)
		s.footer.OpenFault(theWS, domainFault("v-1", health.KindVendorStartFailed))
		s.roster.SetVendorStart(theWS, sidebar.VendorStartStopped)
		s.link(shimclient.LinkDead)
	}},
	{name: "the network unreachable", session: true, apply: func(s *surfaces) {
		s.link(shimclient.LinkConnected)
		s.footer.OpenFault(theWS, domainFault("n-1", health.KindNetworkUnreachable))
		s.roster.NetworkFaultOpened(theWS, "n-1")
	}},
	{name: "the network unreachable while a vendor start is retried", session: true, apply: func(s *surfaces) {
		s.link(shimclient.LinkConnected)
		s.footer.OpenFault(theWS, domainFault("v-1", health.KindVendorStartRetrying))
		s.roster.SetVendorStart(theWS, sidebar.VendorStartRetrying)
		s.footer.OpenFault(theWS, domainFault("n-1", health.KindNetworkUnreachable))
		s.roster.NetworkFaultOpened(theWS, "n-1")
	}},
	// A shim taken back after a failed handover that never re-reported its
	// session state: the rollout states it to both surfaces at once.
	{name: "state unreported after a take-back", session: true, apply: func(s *surfaces) {
		s.link(shimclient.LinkConnected)
		s.footer.SetStateUnreported(theWS, true)
		s.roster.SetStateUnreported(theWS, true)
	}},
}

// domainFault is a standing fault of kind as the health partition hands it to
// the footer.
func domainFault(id, kind string) footer.Fault {
	cell, _ := health.FaultFooterCell(kind, false)
	return footer.Fault{ID: id, Kind: kind, Status: string(cell.Status), SubStatus: cell.SubStatus, Detail: "detail", At: epoch}
}

// turnCase is one turn state.
type turnCase struct {
	name  string
	apply func(s *surfaces)
}

var turnCases = []turnCase{
	{name: "no turn", apply: func(*surfaces) {}},
	{name: "submitting", apply: func(s *surfaces) { s.startTurn() }},
	{name: "thinking", apply: func(s *surfaces) {
		s.startTurn()
		act := &conversationv1.AgentActivity{
			ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
			Item: &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{
				Result: &conversationv1.AgentThinking_Start{Start: &conversationv1.AgentThinkingStart{}}}},
		}
		s.footer.OnActivity(theWS, agent("main"), act)
		s.roster.OnActivity(theWS, agent("main"), act)
	}},
	{name: "permission", apply: func(s *surfaces) {
		s.startTurn()
		s.footer.OnPermission(theWS, agent("main"), permissionAsk("p-1"))
		s.roster.OnPermission(theWS, agent("main"), permissionAsk("p-1"))
	}},
	{name: "completed", apply: func(s *surfaces) {
		s.startTurn()
		s.endTurn(&conversationv1.AgentSuccess{
			Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
		}, nil, wsm.CloseCompleted)
	}},
	{name: "interrupted", apply: func(s *surfaces) {
		s.startTurn()
		s.endTurn(&conversationv1.AgentSuccess{
			Outcome: &conversationv1.AgentSuccess_Interrupted{Interrupted: &conversationv1.AgentInterrupted{
				Cause: &conversationv1.AgentInterrupted_ByUser{ByUser: &conversationv1.AgentInterruptedByUser{}}}},
		}, nil, wsm.CloseKilled)
	}},
	{name: "vendor refused", apply: func(s *surfaces) {
		s.startTurn()
		s.endTurn(nil, &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{
					AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}}}},
		}, wsm.CloseFailed)
	}},
	{name: "vendor refused, then the session restarted", apply: func(s *surfaces) {
		s.startTurn()
		s.endTurn(nil, &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}},
		}, wsm.CloseFailed)
		started := &conversationv1.SessionStarted{VendorSessionId: "vendor-2"}
		s.footer.OnSessionStarted(theWS, started)
		s.roster.OnSessionStarted(theWS, started)
	}},
	{name: "the turn's own failure", apply: func(s *surfaces) {
		s.startTurn()
		s.endTurn(nil, &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_ExecutionError{ExecutionError: &conversationv1.AgentExecutionError{}},
		}, wsm.CloseFailed)
	}},
	{name: "an expected stop", apply: func(s *surfaces) {
		s.startTurn()
		s.endTurn(nil, &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_StopHookPrevented{StopHookPrevented: &conversationv1.AgentStoppedByStopHook{}},
		}, wsm.CloseFailed)
	}},
	{name: "the query died mid-turn", apply: func(s *surfaces) {
		s.startTurn()
		died := &conversationv1.SessionUpdate{
			Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}},
		}
		s.footer.OnSessionUpdate(theWS, died)
		s.roster.OnSessionUpdate(theWS, died)
		s.roster.SetTurnEnded(theWS, wsm.CloseFailed)
	}},
	{name: "the allowance rejected", apply: func(s *surfaces) {
		s.footer.OnSessionUpdate(theWS, rejectedRateLimit())
		s.roster.OnSessionUpdate(theWS, rejectedRateLimit())
	}},
	{name: "the vendor retrying the turn's call", apply: func(s *surfaces) {
		s.startTurn()
		s.apiError()
	}},
	{name: "the retried call answered", apply: func(s *surfaces) {
		s.startTurn()
		s.apiError()
		act := &conversationv1.AgentActivity{Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{}}}
		s.footer.OnActivity(theWS, agent("main"), act)
		s.roster.OnActivity(theWS, agent("main"), act)
	}},
	{name: "a prompt sent during the retry opening its own turn", apply: func(s *surfaces) {
		s.startTurn()
		s.apiError()
		s.startTurn()
	}},
}

// apiError reports the vendor retrying the main agent's call to both surfaces.
func (s *surfaces) apiError() {
	failed := &conversationv1.ApiRequestFailed{Message: "Can't reach the API server (ENOTFOUND)"}
	s.footer.OnApiError(theWS, agent("main"), failed)
	s.roster.OnApiError(theWS, agent("main"), failed)
}

// mergeStates is every state the merge orchestrator states, "none" included,
// each with the step or area the orchestrator states beside it.
var mergeStates = map[string]footer.MergeFacts{
	"none":    {State: "none"},
	"queued":  {State: "queued", Step: footer.StepEnqueued, QueuePlace: 1, QueueWaiting: 1},
	"merging": {State: "merging", Step: footer.StepTesting},
	"failed":  {State: "failed", FailedArea: footer.FailedConflicts},
	"merged":  {State: "merged"},
}

// footerClaim reads the footer's last view's claim.
func footerClaim(t *testing.T, f footer.Resolver) (ladder.Claim, *frontendv1.FooterStatus) {
	t.Helper()
	view, ok := f.Topic(theWS).Latest()
	if !ok {
		t.Fatal("the footer published nothing")
	}
	status := view.GetStrip().GetStatus()
	claim, placed := ladder.FooterClaim(status)
	if !placed {
		t.Fatalf("the footer's status %v is on no ladder rung", status)
	}
	return claim, status
}

// rosterClaim reads the roster row's claim.
func rosterClaim(t *testing.T, r sidebar.Resolver) (ladder.Claim, string) {
	t.Helper()
	arm := statusName(onlyRow(t, r))
	claim, placed := ladder.RosterArmClaim(arm)
	if !placed {
		t.Fatalf("the roster's arm %q is on no ladder rung", arm)
	}
	return claim, arm
}

func TestTheFooterAndTheRosterAlwaysMakeTheSameCoarseClaim(t *testing.T) {
	for merge, mergeFacts := range mergeStates {
		for _, lc := range linkCases {
			for _, tc := range turnCases {
				for _, parked := range []bool{false, true} {
					for _, read := range []bool{false, true} {
						name := fmt.Sprintf("merge=%s/link=%s/turn=%s/parked=%v/read=%v", merge, lc.name, tc.name, parked, read)
						t.Run(name, func(t *testing.T) {
							// Arrange: a parked session is a hibernated
							// session record the roster reads, and the
							// footer's own park, which a later link edge
							// would lift, so it is stated last.
							var session *wsm.Session
							if lc.session || parked {
								session = &wsm.Session{Workspace: theWS}
							}
							if parked {
								session.Terminal = &wsm.SessionTerminal{Kind: "hibernated", At: epoch}
							}
							s := newSurfaces(t, session)
							lc.apply(s)
							tc.apply(s)
							facts := mergeFacts
							s.footer.SetMerge(theWS, facts)
							s.roster.SetMerge(theWS, facts)
							if parked {
								s.footer.SetParked(theWS, true)
							}

							// Act
							if read {
								s.roster.SetViewed(theWS)
							}
							fClaim, fStatus := footerClaim(t, s.footer)
							rClaim, rArm := rosterClaim(t, s.roster)

							// Assert
							if fClaim != rClaim {
								t.Fatalf("the footer claims %q (%v) where the roster claims %q (%s)",
									fClaim, fStatus.GetStatus(), rClaim, rArm)
							}
							for _, logs := range s.logs {
								for _, op := range []string{"daemon.footer.status_claim", "daemon.sidebar.status_claim"} {
									if hasError(logs.Records(), op) {
										t.Fatalf("a resolver recorded %s: it drew an arm off its own claim", op)
									}
								}
							}
						})
					}
				}
			}
		}
	}
}
