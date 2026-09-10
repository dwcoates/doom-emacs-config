package sidebar_test

import (
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// theWS is the workspace every status test drives facts into.
const theWS = ids.WorkspaceID("w1")

// sidebarResolver is the resolver plus the surfaces its records land in, so a
// test that asserts a record and one that asserts a row read the same handle.
type sidebarResolver struct {
	sidebar.Resolver
	surfaces *dlog.TestSurfaces
}

// arrange installs a one-workspace registry and answers the resolver.
func arrange(t *testing.T, ws ...wsm.Workspace) sidebarResolver {
	t.Helper()
	r, surfaces := newResolver(t)
	if len(ws) == 0 {
		ws = []wsm.Workspace{workspace(string(theWS), "one")}
	}
	r.SetRegistry(registry(ws...))
	return sidebarResolver{Resolver: r, surfaces: surfaces}
}

// live brings the workspace to a proven, idle session: the route serves and
// the session has announced itself, which is where `ready` sits.
func live(t *testing.T, r sidebarResolver) sidebarResolver {
	t.Helper()
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})
	return r
}

func TestRowIsNoneBeforeAnySession(t *testing.T) {
	// Arrange, Act.
	r := arrange(t)

	// Assert: an assertion that the resolver looked, not an absent oneof.
	if got := statusName(onlyRow(t, r)); got != "none" {
		t.Fatalf("status = %q, want none", got)
	}
}

func TestRowIsInitWhileTheRouteIsBeingEstablished(t *testing.T) {
	// Arrange.
	r := arrange(t)

	// Act.
	r.OnLink(theWS, shimclient.LinkDialing)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "init" {
		t.Fatalf("status = %q, want init", got)
	}
}

func TestRowIsInitWhileTheSessionHasNotAnnouncedItself(t *testing.T) {
	// Arrange.
	r := arrange(t)

	// Act: the route serves but no SessionStarted has arrived.
	r.OnLink(theWS, shimclient.LinkConnected)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "init" {
		t.Fatalf("status = %q, want init", got)
	}
}

func TestRowIsReadyOnceTheSessionIsProvenAndIdle(t *testing.T) {
	// Arrange, Act.
	r := live(t, arrange(t))

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "ready" {
		t.Fatalf("status = %q, want ready", got)
	}
}

func TestRowIsSeveredWhileTheLinkRedials(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.OnLink(theWS, shimclient.LinkRedialing)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "severed" {
		t.Fatalf("status = %q, want severed", got)
	}
}

func TestRowIsDeadWhenAConnectedShimIsGone(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.OnLink(theWS, shimclient.LinkDead)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "dead" {
		t.Fatalf("status = %q, want dead", got)
	}
}

func TestRowIsStartFailedWhenTheShimNeverConnected(t *testing.T) {
	// Arrange.
	r := arrange(t)

	// Act: dead without ever having connected is a startup that failed.
	r.OnLink(theWS, shimclient.LinkDialing)
	r.OnLink(theWS, shimclient.LinkDead)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "start_failed" {
		t.Fatalf("status = %q, want start_failed", got)
	}
}

func TestRowIsDegradedWithAnOpenDegradedWindow(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.OnSessionUpdate(theWS, degradedUpdate())

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "degraded" {
		t.Fatalf("status = %q, want degraded", got)
	}
}

func TestRowIsSubmittingBeforeTheFirstActivity(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "submitting" {
		t.Fatalf("status = %q, want submitting", got)
	}
}

func TestRowIsThinkingOnceTheTurnProducesActivity(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Act.
	r.OnActivity(theWS, agent("a1"), &conversationv1.AgentActivity{})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "thinking" {
		t.Fatalf("status = %q, want thinking", got)
	}
}

func TestRowIsClearingWhileAClearRuns(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActClear})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "clearing" {
		t.Fatalf("status = %q, want clearing", got)
	}
}

func TestRowIsCompactingWhileACompactionRuns(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActCompact})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "compacting" {
		t.Fatalf("status = %q, want compacting", got)
	}
}

func TestRowIsCompactingOnAVendorInitiatedCompaction(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act: no accepted turn of ours announces this one.
	r.OnSessionUpdate(theWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Compacting{
			Compacting: &conversationv1.SessionCompacting{}}})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "compacting" {
		t.Fatalf("status = %q, want compacting", got)
	}
}

func TestRowIsPermissionWhileAGateIsOpen(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.OnPermission(theWS, agent("a1"), permissionAsk("p1"))

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "permission" {
		t.Fatalf("status = %q, want permission", got)
	}
}

func TestRowLeavesPermissionWhenTheGateIsDecided(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.OnPermission(theWS, agent("a1"), permissionAsk("p1"))

	// Act: opened or closed, a decided gate is no longer waiting on the user.
	r.OnPermission(theWS, agent("a1"), permissionDecided("p1"))

	// Assert.
	if got := statusName(onlyRow(t, r)); got == "permission" {
		t.Fatal("the row stayed on permission after the gate was decided")
	}
}

func TestRowStaysOnPermissionUntilEveryGateIsDecided(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.OnPermission(theWS, agent("a1"), permissionAsk("p1"))
	r.OnPermission(theWS, agent("a1"), permissionAsk("p2"))

	// Act.
	r.OnPermission(theWS, agent("a1"), permissionDecided("p1"))

	// Assert: two gated calls must both be answered.
	if got := statusName(onlyRow(t, r)); got != "permission" {
		t.Fatalf("status = %q, want permission while a second gate stands", got)
	}
}

func TestRowIsIdleAsyncWithDetachedWorkAndNoTurn(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.OnDetachedWork(theWS, agent("a1"), detachedWork("work-1"))

	// Act.
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "idle_async" {
		t.Fatalf("status = %q, want idle_async", got)
	}
}

func TestRowIsDoneWhenTheTurnFinished(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Act.
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "done" {
		t.Fatalf("status = %q, want done", got)
	}
}

func TestRowIsInterruptedWhenTheUserStoppedTheTurn(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Act.
	r.SetTurnEnded(theWS, wsm.CloseKilled)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "interrupted" {
		t.Fatalf("status = %q, want interrupted", got)
	}
}

func TestRowIsVendorBlockedWhenTheQueryDied(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.OnSessionUpdate(theWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{
			QueryDied: &conversationv1.SessionQueryDied{}}})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "vendor_blocked" {
		t.Fatalf("status = %q, want vendor_blocked", got)
	}
}

func TestRowIsVendorBlockedWhenTheVendorRejectedTheAllowance(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.OnSessionUpdate(theWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_RateLimitStatus{
			RateLimitStatus: &conversationv1.SessionRateLimitStatus{
				Status: &conversationv1.SessionRateLimitStatus_Rejected{
					Rejected: &conversationv1.SessionRateLimitRejected{}}}}})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "vendor_blocked" {
		t.Fatalf("status = %q, want vendor_blocked", got)
	}
}

func TestRowIsVendorBlockedOnAnAccountLevelFailure(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.OnAgentTerminal(theWS, agent("a1"), nil, nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_BlockingLimit{
			BlockingLimit: &conversationv1.AgentStoppedAtBlockingLimit{}}})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "vendor_blocked" {
		t.Fatalf("status = %q, want vendor_blocked", got)
	}
}

func TestRowIsNotVendorBlockedOnAnOrdinaryRunFailure(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act: a run that failed is not a session that cannot proceed.
	r.OnAgentTerminal(theWS, agent("a1"), nil, nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ModelError{
			ModelError: &conversationv1.AgentModelError{}}})

	// Assert.
	if got := statusName(onlyRow(t, r)); got == "vendor_blocked" {
		t.Fatal("an ordinary run failure blocked the row on the vendor")
	}
}

// The four run terminals whose failure.proto evidence messages state that they
// resolve the workspace PURPLE — landing 8.
func TestRowIsVendorBlockedOnEachPurpleRunTerminal(t *testing.T) {
	tests := []struct {
		name    string
		failure *conversationv1.AgentFailure
	}{
		{"max_turns", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{
			MaxTurns: &conversationv1.AgentMaxTurnsReached{}}}},
		{"budget_exhausted", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BudgetExhausted{
			BudgetExhausted: &conversationv1.AgentBudgetExhausted{}}}},
		{"execution_error", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ExecutionError{
			ExecutionError: &conversationv1.AgentExecutionError{}}}},
		{"structured_output_retry_exhausted", &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_StructuredOutputRetryExhausted{
				StructuredOutputRetryExhausted: &conversationv1.AgentStructuredOutputRetriesExhausted{}}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t))

			// Act.
			r.OnAgentTerminal(theWS, agent("a1"), nil, nil, tc.failure)

			// Assert.
			if got := statusName(onlyRow(t, r)); got != "vendor_blocked" {
				t.Fatalf("status = %q, want vendor_blocked", got)
			}
		})
	}
}

// A Stop hook is the user's own configuration, not the vendor refusing.
func TestRowIsNotVendorBlockedOnAStopHookPrevention(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.OnAgentTerminal(theWS, agent("a1"), nil, nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_StopHookPrevented{
			StopHookPrevented: &conversationv1.AgentStoppedByStopHook{}}})

	// Assert.
	if got := statusName(onlyRow(t, r)); got == "vendor_blocked" {
		t.Fatal("a Stop hook blocked the row on the vendor")
	}
}

func TestRowIsInactiveWhenClosedWithNoLiveSession(t *testing.T) {
	// Arrange.
	closed := workspace(string(theWS), "one")
	closed.Closed = true

	// Act.
	r := arrange(t, closed)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "inactive" {
		t.Fatalf("status = %q, want inactive", got)
	}
}

func TestRowIsNotInactiveWhenClosedWithASessionStillLive(t *testing.T) {
	// Arrange.
	closed := workspace(string(theWS), "one")
	closed.Closed = true

	// Act: closed is orthogonal to the lifecycle — a live session still has one.
	r := live(t, arrange(t, closed))

	// Assert.
	if got := statusName(onlyRow(t, r)); got == "inactive" {
		t.Fatal("a closed workspace with a live session read inactive")
	}
}

func TestRosterResolvesEveryMergeArm(t *testing.T) {
	tests := []struct {
		name  string
		state string
		want  string
	}{
		{name: "enqueuing", state: "enqueuing", want: "merge_enqueuing"},
		{name: "queued", state: "queued", want: "merge_queued"},
		{name: "merging", state: "merging", want: "merging"},
		{name: "conflict", state: "conflict", want: "merge_conflict"},
		// A parked merge holds its lease awaiting the user; the roster has no
		// parked arm and spells it as the conflict awaiting resolution.
		{name: "parked", state: "parked", want: "merge_conflict"},
		{name: "failed", state: "failed", want: "merge_failed"},
		{name: "merged", state: "merged", want: "merged"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t))

			// Act.
			r.SetMerge(theWS, footer.MergeFacts{State: tc.state})

			// Assert.
			if got := statusName(onlyRow(t, r)); got != tc.want {
				t.Fatalf("status = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestMergeStateNoneLeavesTheSessionLifecycleStanding(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.SetMerge(theWS, footer.MergeFacts{State: "none"})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "ready" {
		t.Fatalf("status = %q, want the session's own arm", got)
	}
}

func TestInactiveDominatesTheMergePipeline(t *testing.T) {
	// Arrange.
	closed := workspace(string(theWS), "one")
	closed.Closed = true
	r := arrange(t, closed)

	// Act.
	r.SetMerge(theWS, footer.MergeFacts{State: "merging"})

	// Assert: a perspective-less workspace is inactive whatever else holds.
	if got := statusName(onlyRow(t, r)); got != "inactive" {
		t.Fatalf("status = %q, want inactive", got)
	}
}

func TestTheMergePipelineDominatesTheLink(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.OnLink(theWS, shimclient.LinkRedialing)

	// Act.
	r.SetMerge(theWS, footer.MergeFacts{State: "merging"})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "merging" {
		t.Fatalf("status = %q, want merging", got)
	}
}

func TestTheLinkDominatesTheSessionLifecycle(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.OnActivity(theWS, agent("a1"), &conversationv1.AgentActivity{})

	// Act: the route is what a session state is reported OVER.
	r.OnLink(theWS, shimclient.LinkRedialing)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "severed" {
		t.Fatalf("status = %q, want severed", got)
	}
}

func TestVendorBlockedDominatesAnOpenPermission(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.OnPermission(theWS, agent("a1"), permissionAsk("p1"))

	// Act.
	r.OnSessionUpdate(theWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{
			QueryDied: &conversationv1.SessionQueryDied{}}})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "vendor_blocked" {
		t.Fatalf("status = %q, want vendor_blocked", got)
	}
}

func TestAnOpenPermissionDominatesTheRunningTurn(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.OnActivity(theWS, agent("a1"), &conversationv1.AgentActivity{})

	// Act.
	r.OnPermission(theWS, agent("a1"), permissionAsk("p1"))

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "permission" {
		t.Fatalf("status = %q, want permission", got)
	}
}

func TestIdleAsyncRetiresOnAnEmptyLiveWorkSet(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.OnDetachedWork(theWS, agent("a1"), detachedWork("work-1"))
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Act: the watcher reaped the last item's watch.
	r.OnLiveWorkChanged(theWS, sidebar.LiveWorkSet{})

	// Assert: the row does not wait for the next turn to stop saying idle_async.
	if got := statusName(onlyRow(t, r)); got != "done" {
		t.Fatalf("status = %q, want done once the live-work set emptied", got)
	}
}

func TestIdleAsyncStandsWhileTheLiveWorkSetIsNotEmpty(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Act: the watcher states a live item the roster never saw announced.
	r.OnLiveWorkChanged(theWS, sidebar.LiveWorkSet{
		Shells: []*conversationv1.DetachedWorkId{{Value: "work-1"}}})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "idle_async" {
		t.Fatalf("status = %q, want idle_async", got)
	}
}

func TestDetachedWorkDominatesTheTurnsTerminal(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.OnDetachedWork(theWS, agent("a1"), detachedWork("work-1"))

	// Act: work happening NOW outranks how the last turn ended.
	r.SetTurnEnded(theWS, wsm.CloseKilled)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "idle_async" {
		t.Fatalf("status = %q, want idle_async", got)
	}
}

func TestARunningTurnDominatesDetachedWork(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.OnDetachedWork(theWS, agent("a1"), detachedWork("work-1"))

	// Act.
	r.OnActivity(theWS, agent("a1"), &conversationv1.AgentActivity{})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "thinking" {
		t.Fatalf("status = %q, want thinking", got)
	}
}

func TestANewTurnRetiresThePreviousTurnsDetachedWork(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.OnDetachedWork(theWS, agent("a1"), detachedWork("work-1"))
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Act: the items announced by the turn before belong to that turn's account.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "done" {
		t.Fatalf("status = %q, want done", got)
	}
}

func TestANewTurnRetiresAStandingVendorBlock(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.OnSessionUpdate(theWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{
			QueryDied: &conversationv1.SessionQueryDied{}}})

	// Act.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "submitting" {
		t.Fatalf("status = %q, want submitting", got)
	}
}

func TestEveryRowCarriesAStatusArm(t *testing.T) {
	// Arrange, Act: an unset oneof is a contract breach, not a default dot.
	r := arrange(t)

	// Assert.
	if got := statusName(onlyRow(t, r)); got == "" {
		t.Fatal("a row shipped an unset status oneof")
	}
}

// degradedUpdate is a diagnostics push carrying one OPEN degraded window.
func degradedUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{
			Diagnostics: &conversationv1.SessionDiagnostics{
				DegradedWindows: []*conversationv1.SessionDegradedWindow{{
					Extent: &conversationv1.SessionDegradedWindow_Open{
						Open: &conversationv1.SessionDegradedOpen{}},
				}},
			}}}
}

func TestRowTakesDetachedWorkRestoredWithNoAnnouncingAgent(t *testing.T) {
	// Arrange: live work restored from SessionStarted.live_work has no
	// announcing agent, so the sink receives a nil one.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Act.
	r.OnDetachedWork(theWS, nil, detachedWork("work-restored"))

	// Assert: such an item is never dropped.
	if got := statusName(onlyRow(t, r)); got != "idle_async" {
		t.Fatalf("status = %q, want idle_async — restored live work was dropped", got)
	}
}

func TestRowTakesAPermissionWithNoAnnouncingAgent(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.OnPermission(theWS, nil, permissionAsk("p1"))

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "permission" {
		t.Fatalf("status = %q, want permission — an agentless ask was dropped", got)
	}
}

// TestRowIsThinkingOnceTheShimTakesTheTurn pins the ack edge: `submitting`
// names the window before the shim answers StartTurn, and a turn that then
// produces no activity at all would otherwise sit in that window for its whole
// life.
func TestRowIsThinkingOnceTheShimTakesTheTurn(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Act.
	r.AckTurn(theWS)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "thinking" {
		t.Fatalf("status = %q, want thinking", got)
	}
}

// TestAParkedSessionKeepsAnIdleArm covers the hibernation's presentation: the
// idle sweep stood the shim down deliberately, so the row must not report the
// route it took down as a fault. The frontend cannot be allowed to tell a
// parked workspace from an idle one.
func TestAParkedSessionKeepsAnIdleArm(t *testing.T) {
	// Arrange: a proven, idle session whose shim the sweep then stood down.
	r := live(t, arrange(t))
	r.OnLink(theWS, shimclient.LinkRedialing)

	// Act: the park lands on the durable record.
	r.SetRegistry(sidebar.Registry{
		Workspaces:   []wsm.Workspace{workspace(string(theWS), "one")},
		Repositories: []wsm.Repository{repo},
		Sessions: []wsm.Session{{
			Workspace: theWS,
			Terminal:  &wsm.SessionTerminal{Kind: "hibernated"},
		}},
	})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "ready" {
		t.Fatalf("the parked row's status = %q, want an idle arm", got)
	}
}

// ---------------------------------------------------------------------------
// The arm's ORDER, not merely its value.
//
// THE DEFECT, MEASURED. On a cold workspace the roster published
// `ready` ~340ms AFTER SubmitPrompt had answered with a turn id, and the whole
// walk Emacs observed was none -> ready -> init -> submitting -> thinking.
// Both halves of that contradict sidebar.proto: `ready` is "live, PROVEN
// USABLE, and idle", and a workspace whose session record exists while no link
// state has been seen has proven nothing and is not idle. `ready` also cannot
// precede the `init` it is supposed to follow.
// ---------------------------------------------------------------------------

// TestRowIsInitWhileTheSessionRecordExistsAndNoLinkHasBeenSeen is the
// resolver-level shape of the leading `ready`: the durable session row lands
// before any link state does, and that window is `init`, never `ready`.
func TestRowIsInitWhileTheSessionRecordExistsAndNoLinkHasBeenSeen(t *testing.T) {
	// Arrange: a registry that already carries the session, with no OnLink yet.
	r := arrange(t)

	// Act.
	r.SetRegistry(sidebar.Registry{
		Workspaces:   []wsm.Workspace{workspace(string(theWS), "one")},
		Repositories: []wsm.Repository{repo},
		Sessions:     []wsm.Session{{Workspace: theWS}},
	})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "init" {
		t.Fatalf("status = %q with a session record and no observed link, want init: nothing has proven the route usable yet", got)
	}
}

// TestTheRowNeverReadsReadyBeforeInit walks the cold-start sequence the editor
// saw and asserts the ORDER of what was published, not merely the final value.
func TestTheRowNeverReadsReadyBeforeInit(t *testing.T) {
	// Arrange: watch from before the first fact, so the walk is the subject.
	r := arrange(t)
	watch := watchStatusWalk(t, r)

	// Act: the cold start, in the order the daemon produces it.
	r.SetRegistry(sidebar.Registry{
		Workspaces:   []wsm.Workspace{workspace(string(theWS), "one")},
		Repositories: []wsm.Repository{repo},
		Sessions:     []wsm.Session{{Workspace: theWS}},
	})
	r.OnLink(theWS, shimclient.LinkDialing)
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})

	// Assert.
	walk := watch.awaitArm(t, "ready")
	sawInit := false
	for _, arm := range walk {
		switch arm {
		case "init":
			sawInit = true
		case "ready":
			if !sawInit {
				t.Fatalf("the roster published ready before any init: %v", walk)
			}
		}
	}
	if !sawInit {
		t.Fatalf("the cold start published no init at all: %v", walk)
	}
}

// TestTheRowNeverReadsReadyOnceATurnIsAccepted is the accept side of the same
// contract: `ready` says idle, and a workspace whose turn the daemon has
// accepted is not idle until that turn ends.
func TestTheRowNeverReadsReadyOnceATurnIsAccepted(t *testing.T) {
	// Arrange: a proven, idle session, watched from the accept onward.
	r := live(t, arrange(t))
	watch := watchStatusWalk(t, r)

	// Act: accept, then ack, then the activity that moves it to thinking —
	// the turn's whole life short of its end.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.AckTurn(theWS)

	// Assert: `ready` was the row's value when the watch opened, so the walk
	// is read from the accept onward.
	walk := watch.awaitArm(t, "thinking")
	for _, arm := range walk[1:] {
		if arm == "ready" {
			t.Fatalf("the roster published ready while an accepted turn was in flight: %v", walk)
		}
	}
	if walk[0] != "ready" {
		t.Fatalf("the walk opened at %q, want the pre-accept ready this test reads past: %v", walk[0], walk)
	}
}

// statusWalk records every status arm one workspace's row has been published
// with, in order.
type statusWalk struct {
	mu   sync.Mutex
	seen []string
	// woke announces each append, so awaitArm blocks on an EVENT rather than
	// polling or sleeping.
	woke chan struct{}
}

// watchStatusWalk subscribes to the roster and records the workspace's status
// arm from every roster published from now on.
func watchStatusWalk(t *testing.T, r sidebar.Resolver) *statusWalk {
	t.Helper()
	rosters := subscribe(t, r)
	w := &statusWalk{woke: make(chan struct{}, 1)}
	// The goroutine ends when the subscription's channel closes, which
	// subscribe's own cleanup guarantees.
	go func() {
		for roster := range rosters {
			sections := roster.GetRepository().GetSections()
			if len(sections) == 0 || len(sections[0].GetRows().GetRows()) == 0 {
				continue
			}
			w.mu.Lock()
			w.seen = append(w.seen, statusName(sections[0].GetRows().GetRows()[0]))
			w.mu.Unlock()
			select {
			case w.woke <- struct{}{}:
			default:
			}
		}
	}()
	return w
}

// walkBound is how long awaitArm waits for an arm that the acts before it have
// already produced. The resolver publishes SYNCHRONOUSLY on every input and
// the topic's pump is one goroutine hop away, so the arm is there
// microseconds later; this is a failure bound, not a wait, and it exists only
// so a defect reports itself instead of hanging the suite.
const walkBound = 2 * time.Second

// awaitArm blocks until arm has been published and answers the whole walk up
// to and including it.
func (w *statusWalk) awaitArm(t *testing.T, arm string) []string {
	t.Helper()
	deadline := time.After(walkBound)
	for {
		w.mu.Lock()
		for i, got := range w.seen {
			if got == arm {
				out := append([]string(nil), w.seen[:i+1]...)
				w.mu.Unlock()
				return out
			}
		}
		seen := append([]string(nil), w.seen...)
		w.mu.Unlock()
		select {
		case <-w.woke:
		case <-deadline:
			t.Fatalf("the roster never published %q within %s; the walk was %v", arm, walkBound, seen)
			return nil
		}
	}
}
