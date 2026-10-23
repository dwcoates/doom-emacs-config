package sidebar_test

import (
	"fmt"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	kinds "agent-repld/internal/detachedkindtest"
	"agent-repld/internal/dlog"
	"agent-repld/internal/health"
	"agent-repld/internal/ids"
	"agent-repld/internal/protoarm"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/ladder"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/vocab"
	"agent-repld/internal/wsm"
)

// ONE STATUS, EVERY SURFACE (owner ruling, 2026-09-28). The footer strip, the
// roster row and the tab bar that paints the roster row's arm must ALWAYS make
// the same coarse claim about a workspace. This suite drives the facts both
// resolvers observe (merge state, link state, turn state, parked, read)
// through the REAL footer and roster resolvers, fed the same facts, and
// requires the footer's status and the roster's arm to project onto one
// ladder claim (resolve/ladder). Its rows are CHOSEN, never enumerated: one
// per distinct outcome the facts reach (walkRows).

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
	var r sidebar.Resolver
	f, err := footer.New(colors, footerLogs, footer.WithMomentaryDwell(time.Hour),
		footer.WithStatusChanged(func(ws ids.WorkspaceID) { r.FooterStatusChanged(ws) }))
	if err != nil {
		t.Fatalf("footer.New: %v", err)
	}
	r, err = sidebar.New(colors, rosterLogs, f)
	if err != nil {
		t.Fatalf("sidebar.New: %v", err)
	}
	if err := f.SetWorkspaceDir(theWS, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}
	f.SetParticipants(theWS, true, true)
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

// startTurn accepts a prompt's turn on both.
func (s *surfaces) startTurn() {
	s.startAct(footer.ActPrompt)
}

// endTurn delivers the main agent's terminal to both, and the daemon's own
// close to both, which is what raises a failed turn's fault on each.
func (s *surfaces) endTurn(success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure, how wsm.TurnClose) {
	turn := s.turn
	s.footer.OnAgentTerminal(theWS, agent("main"), &turn, success, failure)
	s.roster.OnAgentTerminal(theWS, agent("main"), &turn, success, failure)
	s.closeTurn(how)
}

// closeTurn delivers the close the prompt queue's door recorded to both.
func (s *surfaces) closeTurn(how wsm.TurnClose) {
	s.footer.SetTurnEnded(theWS, how)
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
	// `none`, and a footer with nothing to report.
	{name: "no session", session: false, apply: func(*surfaces) {}},
	// A session record with no link seen, and the bring-up that makes it the
	// window a route is being established in.
	{name: "session record", session: true, apply: func(*surfaces) {}},
	{name: "bringing up", session: true, apply: func(s *surfaces) {
		s.footer.SetBringingUp(theWS, true)
		s.roster.SetBringingUp(theWS, true)
	}},
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
		s.closeTurn(wsm.CloseFailed)
	}},
	{name: "the agent process died with the turn", apply: func(s *surfaces) {
		s.startTurn()
		s.closeTurn(wsm.CloseAgentDied)
	}},
	{name: "a turn closed as orphaned", apply: func(s *surfaces) {
		s.startTurn()
		s.closeTurn(wsm.CloseOrphaned)
	}},
	{name: "a turn the vendor ended, then the next turn", apply: func(s *surfaces) {
		s.startTurn()
		s.endTurn(nil, &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}},
		}, wsm.CloseFailed)
		s.startTurn()
	}},
	{name: "the allowance rejected", apply: func(s *surfaces) {
		s.footer.OnSessionUpdate(theWS, rejectedRateLimit())
		s.roster.OnSessionUpdate(theWS, rejectedRateLimit())
	}},
	{name: "the weekly allowance rejected, then the session allowance allowed", apply: func(s *surfaces) {
		for _, update := range []*conversationv1.SessionUpdate{
			windowedRateLimit(sevenDayWindow(), rejectedVerdict()), windowedRateLimit(fiveHourWindow(), allowedVerdict()),
		} {
			s.footer.OnSessionUpdate(theWS, update)
			s.roster.OnSessionUpdate(theWS, update)
		}
	}},
	{name: "the weekly allowance rejected, then lifted by its own verdict", apply: func(s *surfaces) {
		for _, update := range []*conversationv1.SessionUpdate{
			windowedRateLimit(sevenDayWindow(), rejectedVerdict()), windowedRateLimit(sevenDayWindow(), allowedVerdict()),
		} {
			s.footer.OnSessionUpdate(theWS, update)
			s.roster.OnSessionUpdate(theWS, update)
		}
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

// requireOneClaim fails unless the footer's status and the roster's arm
// project onto one ladder claim and neither resolver recorded drawing off its
// own claim, and answers the roster's arm.
func requireOneClaim(t *testing.T, s *surfaces) string {
	t.Helper()
	fClaim, fStatus := footerClaim(t, s.footer)
	rClaim, rArm := rosterClaim(t, s.roster)
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
	return rArm
}

// walkRow is one chosen combination of the facts both resolvers observe,
// each fact named by its case above.
type walkRow struct {
	merge, link, turn string
	parked, read      bool
}

// walkRows holds one row per distinct (footer status, roster arm) outcome
// the merge, link and turn cases reach, each labeled with that outcome. The
// full product of the facts (5040 rows) reached exactly these 44 outcomes
// (docs/investigations/2026-10-14-test-cross-product-audit-psv.md, section 5):
// the roster derives every arm above idle from the footer's status
// (statusArm, projectArm), and the two share one ladder walk, so the claim
// depends on the outcome a row reaches, not on how many other rows reach it.
// A fact case added above that reaches a NEW outcome gets a row here. The
// arms these facts never reach are TestTheFooterAndTheRosterAgreeWhereTheWalkNeverReaches's.
var walkRows = []walkRow{
	// footer idle/ready (unpinned/enduring), roster none
	{merge: "none", link: "no session", turn: "no turn", parked: false, read: false},
	// footer idle/ready (unpinned/enduring), roster ready
	{merge: "none", link: "session record", turn: "no turn", parked: false, read: false},
	// footer starting (unpinned/enduring), roster init
	{merge: "none", link: "no session", turn: "submitting", parked: false, read: false},
	// footer working/submitting (unpinned/enduring), roster submitting
	{merge: "none", link: "connected", turn: "submitting", parked: false, read: false},
	// footer working/thinking (unpinned/enduring), roster thinking
	{merge: "none", link: "connected", turn: "thinking", parked: false, read: false},
	// footer permission (salient/at), roster permission
	{merge: "none", link: "connected", turn: "permission", parked: false, read: false},
	// footer idle/done (unpinned/enduring), roster none
	{merge: "none", link: "no session", turn: "completed", parked: false, read: false},
	// footer idle/done (unpinned/enduring), roster done
	{merge: "none", link: "session record", turn: "completed", parked: false, read: false},
	// footer interrupted/by_user (unpinned/enduring), roster none
	{merge: "none", link: "no session", turn: "interrupted", parked: false, read: false},
	// footer interrupted/by_user (unpinned/enduring), roster interrupted
	{merge: "none", link: "session record", turn: "interrupted", parked: false, read: false},
	// footer vendor_fault/auth (salient/at), roster vendor_blocked
	{merge: "none", link: "no session", turn: "vendor refused", parked: false, read: false},
	// footer vendor_fault/vendor_error (salient/at), roster vendor_blocked
	{merge: "none", link: "no session", turn: "the turn's own failure", parked: false, read: false},
	// footer idle/done (salient/at), roster none
	{merge: "none", link: "no session", turn: "the query died mid-turn", parked: false, read: false},
	// footer idle/done (salient/at), roster turn_failed
	{merge: "none", link: "session record", turn: "the query died mid-turn", parked: false, read: false},
	// footer idle/done (unpinned/enduring), roster turn_failed
	{merge: "none", link: "session record", turn: "the agent process died with the turn", parked: false, read: false},
	// footer vendor_fault/usage_limit (unpinned/enduring), roster vendor_blocked
	{merge: "none", link: "no session", turn: "the allowance rejected", parked: false, read: false},
	// footer vendor_fault/api_retrying (salient/at), roster api_retrying
	{merge: "none", link: "connected", turn: "the vendor retrying the turn's call", parked: false, read: false},
	// footer starting (unpinned/transient), roster init
	{merge: "none", link: "no session", turn: "the retried call answered", parked: false, read: false},
	// footer working/thinking (unpinned/transient), roster thinking
	{merge: "none", link: "connected", turn: "the retried call answered", parked: false, read: false},
	// footer agent_repl_fault/turn_died (salient/at), roster turn_died
	{merge: "none", link: "connected", turn: "the query died mid-turn", parked: false, read: false},
	// footer agent_repl_fault/severed (unpinned/enduring), roster severed
	{merge: "none", link: "redialing", turn: "no turn", parked: false, read: false},
	// footer agent_repl_fault/severed (salient/at), roster severed
	{merge: "none", link: "redialing", turn: "the query died mid-turn", parked: false, read: false},
	// footer agent_repl_fault/severed (unpinned/transient), roster severed
	{merge: "none", link: "redialing", turn: "the retried call answered", parked: false, read: false},
	// footer agent_repl_fault/dead (unpinned/enduring), roster dead
	{merge: "none", link: "dead after serving", turn: "no turn", parked: false, read: false},
	// footer agent_repl_fault/dead (salient/at), roster dead
	{merge: "none", link: "dead after serving", turn: "the query died mid-turn", parked: false, read: false},
	// footer agent_repl_fault/dead (unpinned/transient), roster dead
	{merge: "none", link: "dead after serving", turn: "the retried call answered", parked: false, read: false},
	// footer agent_repl_fault/start_failed (unpinned/enduring), roster start_failed
	{merge: "none", link: "dead before serving", turn: "no turn", parked: false, read: false},
	// footer agent_repl_fault/start_failed (salient/at), roster start_failed
	{merge: "none", link: "dead before serving", turn: "the query died mid-turn", parked: false, read: false},
	// footer agent_repl_fault/start_failed (unpinned/transient), roster start_failed
	{merge: "none", link: "dead before serving", turn: "the retried call answered", parked: false, read: false},
	// footer degraded/observation (unpinned/enduring), roster degraded
	{merge: "none", link: "degraded", turn: "no turn", parked: false, read: false},
	// footer degraded/observation (unpinned/transient), roster degraded
	{merge: "none", link: "degraded", turn: "the retried call answered", parked: false, read: false},
	// footer vendor_fault/vendor_retry (salient/at), roster vendor_fault
	{merge: "none", link: "a vendor start being retried", turn: "no turn", parked: false, read: false},
	// footer vendor_fault/vendor_failed (salient/at), roster vendor_fault
	{merge: "none", link: "a vendor start that stopped, over the link its stop killed", turn: "no turn", parked: false, read: false},
	// footer network_fault/offline (salient/at), roster network_fault
	{merge: "none", link: "the network unreachable", turn: "no turn", parked: false, read: false},
	// footer degraded/state_unreported (unpinned/enduring), roster degraded
	{merge: "none", link: "state unreported after a take-back", turn: "no turn", parked: false, read: false},
	// footer degraded/state_unreported (unpinned/transient), roster degraded
	{merge: "none", link: "state unreported after a take-back", turn: "the retried call answered", parked: false, read: false},
	// footer merging/enqueued (unpinned/enduring), roster merge_queued
	{merge: "queued", link: "no session", turn: "no turn", parked: false, read: false},
	// footer merging/enqueued (unpinned/transient), roster merge_queued
	{merge: "queued", link: "no session", turn: "the retried call answered", parked: false, read: false},
	// footer merging/testing (unpinned/enduring), roster merging
	{merge: "merging", link: "no session", turn: "no turn", parked: false, read: false},
	// footer merging/testing (unpinned/transient), roster merging
	{merge: "merging", link: "no session", turn: "the retried call answered", parked: false, read: false},
	// footer merge_failed/conflicts (unpinned/enduring), roster merge_failed
	{merge: "failed", link: "no session", turn: "no turn", parked: false, read: false},
	// footer merge_failed/conflicts (unpinned/transient), roster merge_failed
	{merge: "failed", link: "connected", turn: "the retried call answered", parked: false, read: false},
	// footer merged (unpinned/enduring), roster merged
	{merge: "merged", link: "no session", turn: "no turn", parked: false, read: false},
	// footer merged (unpinned/transient), roster merged
	{merge: "merged", link: "connected", turn: "the retried call answered", parked: false, read: false},
}

func TestTheFooterAndTheRosterAlwaysMakeTheSameCoarseClaim(t *testing.T) {
	for _, row := range walkRows {
		name := fmt.Sprintf("merge=%s/link=%s/turn=%s/parked=%v/read=%v", row.merge, row.link, row.turn, row.parked, row.read)
		t.Run(name, func(t *testing.T) {
			// Arrange: a parked session is a hibernated session record the
			// roster reads, and the footer's own park, which a later link edge
			// would lift, so it is stated last.
			mergeFacts, ok := mergeStates[row.merge]
			if !ok {
				t.Fatalf("walk row names merge state %q, which mergeStates does not hold", row.merge)
			}
			lc := caseNamed(t, "link", linkCases, func(c linkCase) string { return c.name }, row.link)
			tc := caseNamed(t, "turn", turnCases, func(c turnCase) string { return c.name }, row.turn)
			var session *wsm.Session
			if lc.session || row.parked {
				session = &wsm.Session{Workspace: theWS}
			}
			if row.parked {
				session.Terminal = &wsm.SessionTerminal{Kind: "hibernated", At: epoch}
			}
			s := newSurfaces(t, session)
			lc.apply(s)
			tc.apply(s)
			s.footer.SetMerge(theWS, mergeFacts)
			s.roster.SetMerge(theWS, mergeFacts)
			if row.parked {
				s.footer.SetParked(theWS, true)
			}

			// Act
			if row.read {
				s.roster.SetViewed(theWS)
			}

			// Assert
			requireOneClaim(t, s)
		})
	}
}

// caseNamed answers the case of CASES whose nameOf is NAME; a walk row
// naming none is a broken table and fails, naming KIND.
func caseNamed[C any](t testing.TB, kind string, cases []C, nameOf func(C) string, name string) C {
	t.Helper()
	for _, c := range cases {
		if nameOf(c) == name {
			return c
		}
	}
	t.Fatalf("walk row names %s case %q, which no case holds", kind, name)
	var none C
	return none
}

// TestTheFooterAndTheRosterAgreeWhereTheWalkNeverReaches covers the roster
// arms the fact walk above never draws: its link, turn and merge facts reach
// 44 distinct outcomes and none of these
// (docs/investigations/2026-10-14-test-cross-product-audit-psv.md, section 5).
// Each row is the fact that draws its arm, on a session whose link is up, and
// each must make one claim on both surfaces and land on its own arm.
func TestTheFooterAndTheRosterAgreeWhereTheWalkNeverReaches(t *testing.T) {
	tests := []struct {
		name       string
		apply      func(t *testing.T, s *surfaces)
		wantFooter string
		want       string
	}{
		{name: "an interrupt registered mid-turn", wantFooter: "waiting/interrupting", want: "waiting", apply: func(t *testing.T, s *surfaces) {
			s.ackedTurn()
			s.footer.SetInterrupting(theWS, true)
		}},
		{name: "an open question", wantFooter: "question", want: "question", apply: func(t *testing.T, s *surfaces) {
			s.ackedTurn()
			s.footer.OnQuestion(theWS, agent("main"), &conversationv1.AgentQuestion{
				Id: &conversationv1.AgentQuestionId{Value: "q1"},
				Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{
					Batch: &conversationv1.AgentQuestionBatch{Questions: []*conversationv1.AgentQuestionAsked{{
						Question: &conversationv1.AgentQuestionText{Text: "which one?"},
					}}},
				}},
			})
		}},
		{name: "a standing cold gate", wantFooter: "waiting/cold_gate", want: "cold_gate", apply: func(t *testing.T, s *surfaces) {
			s.ackedTurn()
			s.footer.SetColdGate(theWS, footer.ColdGate{Standing: true, Cost: footer.FiguredLine{Lead: "cold"}})
		}},
		{name: "a refused close", wantFooter: "closing/blocked", want: "closing", apply: func(t *testing.T, s *surfaces) {
			s.footer.SetClosing(theWS, &footer.CloseBlocked{Reason: "held_prompts", Detail: "a prompt is held"})
		}},
		{name: "a daemon-scoped fault", wantFooter: "agent_repl_fault/daemon_impaired", want: "daemon_impaired", apply: func(t *testing.T, s *surfaces) {
			s.footer.OpenFault("", footerFault(t, "fault-1", health.KindWsmReadOnly, true))
		}},
		{name: "a clear running", wantFooter: "working/clearing", want: "clearing", apply: func(t *testing.T, s *surfaces) {
			s.startAct(footer.ActClear)
		}},
		{name: "a compaction running", wantFooter: "working/compacting", want: "compacting", apply: func(t *testing.T, s *surfaces) {
			s.startAct(footer.ActCompact)
		}},
		{name: "detached work outliving a read turn end", wantFooter: "background", want: "idle_async", apply: func(t *testing.T, s *surfaces) {
			s.startTurn()
			// A subagent spawned detached, announced as the shim announces it.
			work := kinds.CreatedSubagent(agent("main"), "work-1", "sub-1", "general-purpose", epoch.UnixMilli())
			s.footer.OnDetachedWork(theWS, agent("main"), work)
			s.roster.OnDetachedWork(theWS, agent("main"), work)
			s.closeTurn(wsm.CloseCompleted)
			s.roster.SetViewed(theWS)
		}},
		{name: "a self-scheduled wakeup where the footer reads idle", wantFooter: "waiting/wakeup", want: "ready", apply: func(t *testing.T, s *surfaces) {
			wakeup := &conversationv1.AgentActivity{
				ActivityId: &conversationv1.AgentActivityId{Value: "wake-1"},
				Item: &conversationv1.AgentActivity_ScheduleWakeup{ScheduleWakeup: &conversationv1.AgentScheduleWakeup{
					Result: &conversationv1.AgentScheduleWakeup_Success{Success: &conversationv1.AgentScheduleWakeupSuccess{
						Outcome: &conversationv1.AgentScheduleWakeupSuccess_Scheduled{
							Scheduled: &conversationv1.AgentScheduleWakeupScheduled{WakeAtMs: epoch.Add(5 * time.Minute).UnixMilli()},
						},
					}},
				}},
			}
			s.footer.OnActivity(theWS, agent("main"), wakeup)
			s.roster.OnActivity(theWS, agent("main"), wakeup)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := newSurfaces(t, &wsm.Session{Workspace: theWS})
			s.link(shimclient.LinkConnected)

			// Act
			tt.apply(t, s)

			// Assert
			got := requireOneClaim(t, s)
			if _, status := footerClaim(t, s.footer); footerShape(status) != tt.wantFooter {
				t.Fatalf("the footer drew %s, want %s", footerShape(status), tt.wantFooter)
			}
			if got != tt.want {
				t.Fatalf("the roster drew %q, want %q", got, tt.want)
			}
		})
	}
}

// footerShape names a footer status by its arm and, where the arm states one,
// its substatus: "waiting/wakeup", "question".
func footerShape(status *frontendv1.FooterStatus) string {
	arm, ok := protoarm.Set(status.ProtoReflect(), "status")
	if !ok {
		return "unset"
	}
	name := string(arm.Name())
	armMessage := status.ProtoReflect().Get(arm).Message()
	if armMessage.Descriptor().Oneofs().ByName("substatus") == nil {
		return name
	}
	if sub, ok := protoarm.Name(armMessage, "substatus"); ok {
		return name + "/" + string(sub)
	}
	return name
}

// startAct accepts a turn of the given act on both.
func (s *surfaces) startAct(act footer.SessionAct) {
	turn := &footer.TurnStarted{At: epoch, Act: act}
	s.footer.SetTurn(theWS, turn)
	s.roster.SetTurn(theWS, turn)
}

// ackedTurn accepts a turn on both and acknowledges it on both, as the shim
// taking it does.
func (s *surfaces) ackedTurn() {
	s.startTurn()
	s.footer.AckTurn(theWS)
	s.roster.AckTurn(theWS)
}

// fatalRecorder is a testing.TB that records a Fatalf and stops the caller
// there, as the real one does, by panicking with itself.
type fatalRecorder struct {
	testing.TB
	fatal string
}

func (f *fatalRecorder) Helper() {}

func (f *fatalRecorder) Fatalf(format string, args ...any) {
	f.fatal = fmt.Sprintf(format, args...)
	panic(f)
}

func TestCaseNamedAnswersTheCaseItNames(t *testing.T) {
	// Act
	got := caseNamed(t, "turn", turnCases, func(c turnCase) string { return c.name }, "thinking")

	// Assert
	if got.name != "thinking" {
		t.Fatalf("caseNamed answered %q, want thinking", got.name)
	}
}

func TestCaseNamedFailsNamingTheKindAndTheMissingName(t *testing.T) {
	// Arrange
	rec := &fatalRecorder{TB: t}
	defer func() {
		// Assert
		if recover() != rec {
			t.Fatal("caseNamed returned for a name no case holds, want a fatal")
		}
		if !strings.Contains(rec.fatal, "link") || !strings.Contains(rec.fatal, `"no such link"`) {
			t.Fatalf("fatal = %q, want it to name the kind and the missing name", rec.fatal)
		}
	}()

	// Act
	caseNamed(rec, "link", linkCases, func(c linkCase) string { return c.name }, "no such link")
}
