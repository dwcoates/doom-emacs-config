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
	"claude-repld/internal/vocab"
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

func TestRowIsReadyOnceTheLinkConnectsEvenBeforeSessionStarted(t *testing.T) {
	// Arrange.
	r := arrange(t)

	// Act: the route is connected but no SessionStarted has arrived yet.
	r.OnLink(theWS, shimclient.LinkConnected)

	// Assert: a CONNECTED route is proven and is not a link fault, so the row
	// is not `init` (a BLUE, link-fault color) — it falls through to the
	// session lifecycle, which for an idle session is `ready`. `init` on a
	// connected route was the stuck-blue a resumed, idle session showed: a
	// reconnect replays the link but never the one-shot SessionStarted, so the
	// old `!s.started` guard held the row blue forever.
	if got := statusName(onlyRow(t, r)); got != "ready" {
		t.Fatalf("status = %q, want ready: a connected route is proven, not a link fault", got)
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
	// Arrange: the turn completed with detached work still running.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.OnDetachedWork(theWS, agent("a1"), detachedWork("work-1"))
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Act: the user reads the result, so it no longer holds the row on done.
	r.SetViewed(theWS)

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

func TestAQueryDeathDoesNotBlockTheRow(t *testing.T) {
	// Arrange: a dead query is a FAILED TURN, not a block (owner ruling,
	// 2026-09-28); the watcher closes the turn it cut as failed.
	r := live(t, arrange(t))

	// Act.
	r.OnSessionUpdate(theWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{
			QueryDied: &conversationv1.SessionQueryDied{}}})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "ready" {
		t.Fatalf("status = %q, want ready", got)
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

// rejectedRateLimit is the vendor's rejected allowance verdict.
func rejectedRateLimit() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_RateLimitStatus{
			RateLimitStatus: &conversationv1.SessionRateLimitStatus{
				Status: &conversationv1.SessionRateLimitStatus_Rejected{
					Rejected: &conversationv1.SessionRateLimitRejected{}}}}}
}

// failedTurn runs one turn on a live row that the main agent's terminal ends
// with failure, closed as the daemon closes it: failed.
func failedTurn(r sidebarResolver, failure *conversationv1.AgentFailure) {
	turn := ids.TurnID("turn-1")
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.OnAgentTerminal(theWS, agent("main"), &turn, nil, failure)
	r.SetTurnEnded(theWS, wsm.CloseFailed)
}

// TestEveryAgentFailureArmTakesItsClassifiedArmAndColor walks every
// AgentFailure arm (owner ruling, 2026-09-28): vendor_blocked ONLY for the
// vendor or the account, the turn's own `turn_failed` for every other failure
// (any future arm included), and a green `done` for the two expected stops.
// The colors are the real vocabulary's, on the shared assignment and on the
// tab bar, which paints the same arm.
func TestEveryAgentFailureArmTakesItsClassifiedArmAndColor(t *testing.T) {
	colors, err := vocab.LoadRenderColors("../../../../proto/vocab")
	if err != nil {
		t.Fatalf("LoadRenderColors: %v", err)
	}
	cases := []struct {
		name    string
		failure *conversationv1.AgentFailure
		arm     string
		color   string
	}{
		{name: "api_request_failed", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
			Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}}}}}, arm: "vendor_blocked", color: "turquoise"},
		{name: "blocking_limit", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BlockingLimit{BlockingLimit: &conversationv1.AgentStoppedAtBlockingLimit{}}}, arm: "vendor_blocked", color: "turquoise"},
		{name: "rapid_refill_breaker", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_RapidRefillBreaker{RapidRefillBreaker: &conversationv1.AgentStoppedByRapidRefillBreaker{}}}, arm: "vendor_blocked", color: "turquoise"},
		{name: "model_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}}}, arm: "turn_failed", color: "turquoise"},
		{name: "api_request_failed: overloaded", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
			Kind: &conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{}}}}}, arm: "turn_failed", color: "turquoise"},
		{name: "api_request_failed: billing", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
			Kind: &conversationv1.ApiRequestFailed_BillingError{BillingError: &conversationv1.ApiBillingError{}}}}}, arm: "vendor_blocked", color: "turquoise"},
		{name: "prompt_too_long", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_PromptTooLong{PromptTooLong: &conversationv1.AgentPromptTooLong{}}}, arm: "turn_failed", color: "turquoise"},
		{name: "image_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ImageError{ImageError: &conversationv1.AgentImageRejected{}}}, arm: "turn_failed", color: "turquoise"},
		{name: "malformed_tool_use_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MalformedToolUseExhausted{MalformedToolUseExhausted: &conversationv1.AgentMalformedToolUseExhausted{}}}, arm: "turn_failed", color: "turquoise"},
		{name: "stop_hook_prevented", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StopHookPrevented{StopHookPrevented: &conversationv1.AgentStoppedByStopHook{}}}, arm: "done", color: "green"},
		{name: "hook_stopped", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_HookStopped{HookStopped: &conversationv1.AgentStoppedByHook{}}}, arm: "turn_failed", color: "turquoise"},
		{name: "tool_deferred", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ToolDeferred{ToolDeferred: &conversationv1.AgentToolDeferred{}}}, arm: "done", color: "green"},
		{name: "tool_deferred_unavailable", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ToolDeferredUnavailable{ToolDeferredUnavailable: &conversationv1.AgentToolDeferredUnavailable{}}}, arm: "turn_failed", color: "turquoise"},
		{name: "max_turns", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}}}, arm: "turn_failed", color: "turquoise"},
		{name: "budget_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BudgetExhausted{BudgetExhausted: &conversationv1.AgentBudgetExhausted{}}}, arm: "turn_failed", color: "turquoise"},
		{name: "structured_output_retry_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StructuredOutputRetryExhausted{StructuredOutputRetryExhausted: &conversationv1.AgentStructuredOutputRetriesExhausted{}}}, arm: "turn_failed", color: "turquoise"},
		{name: "turn_setup_failed", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_TurnSetupFailed{TurnSetupFailed: &conversationv1.AgentTurnSetupFailed{}}}, arm: "turn_failed", color: "turquoise"},
		{name: "execution_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ExecutionError{ExecutionError: &conversationv1.AgentExecutionError{}}}, arm: "turn_failed", color: "turquoise"},
		{name: "continuation_prevented", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ContinuationPrevented{ContinuationPrevented: &conversationv1.AgentContinuationPrevented{}}}, arm: "turn_failed", color: "turquoise"},
		{name: "lost", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_Lost{Lost: &conversationv1.DetachedLost{}}}, arm: "turn_failed", color: "turquoise"},
		{name: "query_died", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}}}, arm: "turn_failed", color: "turquoise"},
		{name: "an arm this build does not know", failure: &conversationv1.AgentFailure{}, arm: "turn_failed", color: "turquoise"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t))

			// Act.
			failedTurn(r, tc.failure)

			// Assert.
			got := statusName(onlyRow(t, r))
			if got != tc.arm {
				t.Fatalf("status = %q, want %q", got, tc.arm)
			}
			for _, surface := range []string{"webapp", "emacs_tab_bar"} {
				color, err := colors.RosterStatusColor(surface, got)
				if err != nil {
					t.Fatalf("RosterStatusColor(%s, %s): %v", surface, got, err)
				}
				if color != tc.color {
					t.Fatalf("%s paints %q %q, want %q", surface, got, color, tc.color)
				}
			}
		})
	}
}

func TestASubagentFailureDoesNotBlockTheRow(t *testing.T) {
	// Arrange: only the turn's own terminal speaks for the workspace, which is
	// all the footer has ever read.
	r := live(t, arrange(t))

	// Act.
	r.OnAgentTerminal(theWS, agent("sub-1"), nil, nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_BlockingLimit{
			BlockingLimit: &conversationv1.AgentStoppedAtBlockingLimit{}}})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "ready" {
		t.Fatalf("status = %q, want ready", got)
	}
}

func TestViewingAVendorBlockedRowReadsTheFailedTurnsResult(t *testing.T) {
	// Arrange: a vendor failure blocks the row over the failed turn's end.
	r := live(t, arrange(t))
	failedTurn(r, &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BlockingLimit{
		BlockingLimit: &conversationv1.AgentStoppedAtBlockingLimit{}}})
	if got := statusName(onlyRow(t, r)); got != "vendor_blocked" {
		t.Fatalf("status = %q, want vendor_blocked — the arrangement missed the arm", got)
	}

	// Act: the user views the blocked row, and the block then lifts.
	r.SetViewed(theWS)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-2"})

	// Assert: the failed turn's end is READ, so it is drawn PARTIAL.
	row := onlyRow(t, r)
	if statusName(row) != "turn_failed" || row.GetViewed() == nil {
		t.Fatalf("status = %q viewed = %v, want a READ turn_failed", statusName(row), row.GetViewed())
	}
}

func TestAnUnviewedVendorBlockedRowLeavesTheResultUnread(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	failedTurn(r, &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BlockingLimit{
		BlockingLimit: &conversationv1.AgentStoppedAtBlockingLimit{}}})

	// Act: the block lifts with no view.
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-2"})

	// Assert.
	row := onlyRow(t, r)
	if statusName(row) != "turn_failed" || row.GetViewed() != nil {
		t.Fatalf("status = %q viewed = %v, want an UNREAD turn_failed", statusName(row), row.GetViewed())
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

func TestTheLinkDominatesATerminalMerge(t *testing.T) {
	// Arrange: a failed merge is over, so a broken route outranks it (the one
	// ladder, resolve/ladder).
	r := live(t, arrange(t))
	r.SetMerge(theWS, footer.MergeFacts{State: "failed"})

	// Act.
	r.OnLink(theWS, shimclient.LinkRedialing)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "severed" {
		t.Fatalf("status = %q, want severed", got)
	}
}

func TestAParkedSessionStillShowsItsMerge(t *testing.T) {
	// Arrange: parking skips only the link rung, which is all its ruling
	// spoke about; a failed merge on a parked workspace is still a failed
	// merge.
	r := arrange(t)
	r.SetRegistry(sidebar.Registry{
		Workspaces:   []wsm.Workspace{workspace(string(theWS), "one")},
		Repositories: []wsm.Repository{repo},
		Sessions: []wsm.Session{{
			Workspace: theWS,
			Terminal:  &wsm.SessionTerminal{Kind: "hibernated"},
		}},
	})

	// Act.
	r.SetMerge(theWS, footer.MergeFacts{State: "failed"})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "merge_failed" {
		t.Fatalf("status = %q, want merge_failed", got)
	}
}

func TestATurnAcceptedBeforeAnyLinkAwaitsTheBringUp(t *testing.T) {
	// Arrange: no session record and no link yet, so the row reads `none`.
	r := arrange(t)

	// Act: the daemon accepts a prompt before the session is spawned.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert: the route is coming up, which the footer draws as
	// `disconnected · starting` from the same two facts (ladder.AwaitingBringUp).
	if got := statusName(onlyRow(t, r)); got != "init" {
		t.Fatalf("status = %q, want init", got)
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
	r.OnSessionUpdate(theWS, rejectedRateLimit())

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
	r.SetViewed(theWS) // the result is read, so it does not hold the row on done

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

	r.SetTurnEnded(theWS, wsm.CloseKilled)

	// Act: once the interruption is read, work happening NOW outranks how the
	// last turn ended.
	r.SetViewed(theWS)

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

// TestANewTurnDominatesTheAuthoritativeLiveWorkSet is the owner's ruling on the
// PRODUCTION path: `thinking`/`submitting` is the highest-priority live state
// and, when a turn is in flight, must be the ONLY thing the roster row reflects
// — it overrides detached/async work. The existing dominance test drives the
// async fact through OnDetachedWork (the announcement set `s.detached`, which
// `startTurn` RESETS), so it never exercises the item the watcher's
// AUTHORITATIVE OnLiveWorkChanged set carries — the set `startTurn` deliberately
// preserves because a detached item outlives the turn that spawned it. This
// pins that the surviving authoritative item does NOT keep the row at
// `idle_async` once the user sends the next prompt: a turn in flight wins, and
// the published row says so.
func TestANewTurnDominatesTheAuthoritativeLiveWorkSet(t *testing.T) {
	// Arrange: a prior turn spawned a detached item the WATCHER states
	// authoritatively; the turn then ended, so the row rests at idle_async.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.OnLiveWorkChanged(theWS, sidebar.LiveWorkSet{
		Shells: []*conversationv1.DetachedWorkId{{Value: "work-1"}}})
	r.SetTurnEnded(theWS, wsm.CloseCompleted)
	r.SetViewed(theWS) // the result is read, so the row rests at idle_async
	if got := statusName(onlyRow(t, r)); got != "idle_async" {
		t.Fatalf("arrange status = %q, want idle_async before the new turn", got)
	}

	// Act: the user sends a new prompt while the detached item still runs. The
	// watcher has NOT retired the item, so asyncLive() is still true.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert: the in-flight turn overrides the still-live authoritative async
	// item, and the published row reflects it.
	if got := statusName(onlyRow(t, r)); got != "submitting" {
		t.Fatalf("status = %q, want submitting: a turn in flight overrides live async work", got)
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
	r.SetViewed(theWS) // the result is read, so it does not hold the row on done

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

// ---- The UNREAD RESULT: a turn end holds the row until the user reads it ---
//
// A turn that completes or is interrupted leaves a result the user has not
// read. While it is unread the row shows the turn-end arm (done or
// interrupted) even over live detached work; once the editor reports the row
// viewed it yields to idle_async, drawn FULL; and when that work ends the row
// comes back on its turn-end arm in the READ state, PARTIAL — never a fresh,
// full turn-end claiming a result nobody has read.

// liveShell is a watcher-stated live-work set carrying one detached shell.
func liveShell() sidebar.LiveWorkSet {
	return sidebar.LiveWorkSet{Shells: []*conversationv1.DetachedWorkId{{Value: "work-1"}}}
}

// endedWithAsync runs one turn to how while detached work the watcher states
// is still live, which is where every unread-result case starts.
func endedWithAsync(t *testing.T, how sidebar.TurnClose) sidebarResolver {
	t.Helper()
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.OnLiveWorkChanged(theWS, liveShell())
	r.SetTurnEnded(theWS, how)
	return r
}

func TestAnUnreadTurnEndHoldsTheRowOverDetachedWork(t *testing.T) {
	cases := []struct {
		name       string
		how        sidebar.TurnClose
		act        func(r sidebarResolver)
		wantStatus string
		wantViewed bool
	}{
		{
			name:       "completed with async live is done, full",
			how:        wsm.CloseCompleted,
			act:        func(sidebarResolver) {},
			wantStatus: "done",
		},
		{
			name:       "completed, viewed while async live is idle_async, full",
			how:        wsm.CloseCompleted,
			act:        func(r sidebarResolver) { r.SetViewed(theWS) },
			wantStatus: "idle_async",
		},
		{
			name: "completed, async ends after read is done, partial",
			how:  wsm.CloseCompleted,
			act: func(r sidebarResolver) {
				r.SetViewed(theWS)
				r.OnLiveWorkChanged(theWS, sidebar.LiveWorkSet{})
			},
			wantStatus: "done",
			wantViewed: true,
		},
		{
			name:       "completed, async ends while unread is done, full",
			how:        wsm.CloseCompleted,
			act:        func(r sidebarResolver) { r.OnLiveWorkChanged(theWS, sidebar.LiveWorkSet{}) },
			wantStatus: "done",
		},
		{
			name: "completed, a new prompt clears the unread result",
			how:  wsm.CloseCompleted,
			act: func(r sidebarResolver) {
				// The prompt is accepted and then retired unrun, so the row
				// falls back past the turn to what the facts now say.
				r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
				r.SetTurn(theWS, nil)
			},
			wantStatus: "idle_async",
		},
		{
			name:       "interrupted with async live is interrupted, full",
			how:        wsm.CloseKilled,
			act:        func(sidebarResolver) {},
			wantStatus: "interrupted",
		},
		{
			name:       "interrupted, viewed while async live is idle_async, full",
			how:        wsm.CloseKilled,
			act:        func(r sidebarResolver) { r.SetViewed(theWS) },
			wantStatus: "idle_async",
		},
		{
			name: "interrupted, async ends after read is interrupted, partial",
			how:  wsm.CloseKilled,
			act: func(r sidebarResolver) {
				r.SetViewed(theWS)
				r.OnLiveWorkChanged(theWS, sidebar.LiveWorkSet{})
			},
			wantStatus: "interrupted",
			wantViewed: true,
		},
		{
			name:       "interrupted, async ends while unread is interrupted, full",
			how:        wsm.CloseKilled,
			act:        func(r sidebarResolver) { r.OnLiveWorkChanged(theWS, sidebar.LiveWorkSet{}) },
			wantStatus: "interrupted",
		},
		{
			name: "interrupted, a new prompt clears the unread result",
			how:  wsm.CloseKilled,
			act: func(r sidebarResolver) {
				r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
				r.SetTurn(theWS, nil)
			},
			wantStatus: "idle_async",
		},
		{
			name:       "failed with async live is turn_failed, full",
			how:        wsm.CloseFailed,
			act:        func(sidebarResolver) {},
			wantStatus: "turn_failed",
		},
		{
			name:       "orphaned with async live is turn_failed, full",
			how:        wsm.CloseOrphaned,
			act:        func(sidebarResolver) {},
			wantStatus: "turn_failed",
		},
		{
			name:       "agent died with async live is turn_failed, full",
			how:        wsm.CloseAgentDied,
			act:        func(sidebarResolver) {},
			wantStatus: "turn_failed",
		},
		{
			name:       "failed, viewed while async live is idle_async, full",
			how:        wsm.CloseFailed,
			act:        func(r sidebarResolver) { r.SetViewed(theWS) },
			wantStatus: "idle_async",
		},
		{
			name: "failed, async ends after read is turn_failed, partial",
			how:  wsm.CloseFailed,
			act: func(r sidebarResolver) {
				r.SetViewed(theWS)
				r.OnLiveWorkChanged(theWS, sidebar.LiveWorkSet{})
			},
			wantStatus: "turn_failed",
			wantViewed: true,
		},
		{
			name:       "failed, async ends while unread is turn_failed, full",
			how:        wsm.CloseFailed,
			act:        func(r sidebarResolver) { r.OnLiveWorkChanged(theWS, sidebar.LiveWorkSet{}) },
			wantStatus: "turn_failed",
		},
		{
			name: "failed, a new prompt clears the unread result",
			how:  wsm.CloseFailed,
			act: func(r sidebarResolver) {
				r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
				r.SetTurn(theWS, nil)
			},
			wantStatus: "idle_async",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := endedWithAsync(t, tc.how)

			// Act.
			tc.act(r)

			// Assert.
			row := onlyRow(t, r)
			if got := statusName(row); got != tc.wantStatus {
				t.Fatalf("status = %q, want %q", got, tc.wantStatus)
			}
			if got := row.GetViewed() != nil; got != tc.wantViewed {
				t.Fatalf("viewed = %v, want %v", got, tc.wantViewed)
			}
		})
	}
}

func TestATurnEndWithNoAsyncShowsItsTurnEndArm(t *testing.T) {
	cases := []struct {
		name       string
		how        sidebar.TurnClose
		viewed     bool
		wantStatus string
	}{
		{name: "completed, unread", how: wsm.CloseCompleted, wantStatus: "done"},
		{name: "completed, viewed", how: wsm.CloseCompleted, viewed: true, wantStatus: "done"},
		{name: "interrupted, unread", how: wsm.CloseKilled, wantStatus: "interrupted"},
		{name: "interrupted, viewed", how: wsm.CloseKilled, viewed: true, wantStatus: "interrupted"},
		{name: "failed, unread", how: wsm.CloseFailed, wantStatus: "turn_failed"},
		{name: "failed, viewed", how: wsm.CloseFailed, viewed: true, wantStatus: "turn_failed"},
		{name: "orphaned, unread", how: wsm.CloseOrphaned, wantStatus: "turn_failed"},
		{name: "orphaned, viewed", how: wsm.CloseOrphaned, viewed: true, wantStatus: "turn_failed"},
		{name: "agent died, unread", how: wsm.CloseAgentDied, wantStatus: "turn_failed"},
		{name: "agent died, viewed", how: wsm.CloseAgentDied, viewed: true, wantStatus: "turn_failed"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t))
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
			r.SetTurnEnded(theWS, tc.how)

			// Act.
			if tc.viewed {
				r.SetViewed(theWS)
			}

			// Assert: the arm is the turn end's; the report only draws it PARTIAL.
			row := onlyRow(t, r)
			if got := statusName(row); got != tc.wantStatus {
				t.Fatalf("status = %q, want %q", got, tc.wantStatus)
			}
			if got := row.GetViewed() != nil; got != tc.viewed {
				t.Fatalf("viewed = %v, want %v", got, tc.viewed)
			}
		})
	}
}

func TestTheUnreadResultTransitionsAreRecorded(t *testing.T) {
	cases := []struct {
		name      string
		how       sidebar.TurnClose
		act       func(r sidebarResolver)
		operation string
	}{
		{name: "a turn end sets unread", how: wsm.CloseCompleted, act: func(sidebarResolver) {}, operation: "daemon.sidebar.result_unread"},
		{name: "an unread result outranks async", how: wsm.CloseCompleted, act: func(sidebarResolver) {}, operation: "daemon.sidebar.unread_outranks_async"},
		{name: "a viewed report reads it", how: wsm.CloseCompleted, act: func(r sidebarResolver) { r.SetViewed(theWS) }, operation: "daemon.sidebar.result_read"},
		{name: "a new prompt clears it", how: wsm.CloseCompleted, act: func(r sidebarResolver) {
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
		}, operation: "daemon.sidebar.result_unread_cleared"},
		{name: "a failed turn end sets unread", how: wsm.CloseFailed, act: func(sidebarResolver) {}, operation: "daemon.sidebar.result_unread"},
		{name: "an unread failed result outranks async", how: wsm.CloseFailed, act: func(sidebarResolver) {}, operation: "daemon.sidebar.unread_outranks_async"},
		{name: "a viewed report reads a failed result", how: wsm.CloseFailed, act: func(r sidebarResolver) { r.SetViewed(theWS) }, operation: "daemon.sidebar.result_read"},
		{name: "a new prompt clears a failed result", how: wsm.CloseFailed, act: func(r sidebarResolver) {
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
		}, operation: "daemon.sidebar.result_unread_cleared"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := endedWithAsync(t, tc.how)

			// Act.
			tc.act(r)

			// Assert.
			for _, rec := range r.surfaces.Records() {
				if rec.Operation == tc.operation {
					return
				}
			}
			t.Fatalf("no %s record", tc.operation)
		})
	}
}

func TestAnUnknownTurnCloseIsRecordedAndLeavesNoUnreadResult(t *testing.T) {
	// Arrange: a close no build of the roster has an arm for.
	unknown := sidebar.TurnClose(99)

	// Act.
	r := endedWithAsync(t, unknown)

	// Assert: the breach is recorded loudly, and no unread result holds the
	// row over the live detached work.
	if !hasError(r.surfaces.Records(), "daemon.sidebar.set_turn_ended") {
		t.Fatal("no daemon.sidebar.set_turn_ended error record for an unknown close")
	}
	if got := statusName(onlyRow(t, r)); got != "idle_async" {
		t.Fatalf("status = %q, want idle_async", got)
	}
}

func TestARunningTurnFoundAtAttachDrawsThinking(t *testing.T) {
	tests := []struct {
		name      string
		startedAt *time.Time
	}{
		{name: "with its row's start", startedAt: at(0)},
		{name: "with no row", startedAt: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a daemon that took a busy workspace over.
			r := live(t, arrange(t))

			// Act.
			r.OnTurnRunningAtAttach(theWS, "turn-1", tt.startedAt)

			// Assert.
			if got := statusName(onlyRow(t, r)); got != "thinking" {
				t.Fatalf("status = %q, want thinking: the adopted turn is running", got)
			}
		})
	}
}

func TestARunningTurnFoundAtAttachLeavesThisDaemonsOwnTurn(t *testing.T) {
	// Arrange: the queue already stood a clear of this daemon's.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActClear})

	// Act.
	r.OnTurnRunningAtAttach(theWS, "turn-1", at(0))

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "clearing" {
		t.Fatalf("status = %q, want the standing clear untouched", got)
	}
}

func TestRosterResolvesEveryMergeArmWithNothingParked(t *testing.T) {
	tests := []struct {
		name  string
		facts footer.MergeFacts
		want  string
	}{
		{name: "queued", facts: footer.MergeFacts{State: "queued", Step: footer.StepEnqueued, QueuePlace: 1, QueueWaiting: 2}, want: "merge_queued"},
		{name: "merging", facts: footer.MergeFacts{State: "merging", Step: footer.StepRebasing, Total: 1}, want: "merging"},
		{name: "failed", facts: footer.MergeFacts{State: "failed", FailedArea: footer.FailedConflicts}, want: "merge_failed"},
		{name: "merged", facts: footer.MergeFacts{State: "merged"}, want: "merged"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t))

			// Act.
			r.SetMerge(theWS, tc.facts)

			// Assert.
			if got := statusName(onlyRow(t, r)); got != tc.want {
				t.Fatalf("status = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestAMergeInFlightDominatesTheLink(t *testing.T) {
	// Arrange: the daemon owns the merge, so it is knowable whatever the
	// route is doing.
	r := live(t, arrange(t))
	r.OnLink(theWS, shimclient.LinkRedialing)

	// Act.
	r.SetMerge(theWS, footer.MergeFacts{State: "merging", Step: footer.StepTesting})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "merging" {
		t.Fatalf("status = %q, want merging", got)
	}
}

// ---- the API-retry block ------------------------------------------------

// apiRetry is the vendor's report that it is retrying a failed call.
func apiRetry() *conversationv1.ApiRequestFailed {
	return &conversationv1.ApiRequestFailed{Message: "Can't reach the API server (ENOTFOUND)"}
}

// prose is a frame of the agent's prose, which answers a retried call.
func prose() *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{}}}
}

func TestRowIsApiRetryingWhileTheVendorRetriesTheTurnsCall(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch})

	// Act.
	r.OnApiError(theWS, agent("main"), apiRetry())

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "api_retrying" {
		t.Fatalf("status = %q, want api_retrying", got)
	}
}

func TestARetryWithNoTurnInFlightDoesNotBlockTheRow(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.OnApiError(theWS, agent("main"), apiRetry())

	// Assert.
	if got := statusName(onlyRow(t, r)); got == "api_retrying" {
		t.Fatalf("status = api_retrying with no turn in flight")
	}
}

func TestTheRetriedAgentsAnswerEndsApiRetrying(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch})
	r.OnApiError(theWS, agent("main"), apiRetry())

	// Act.
	r.OnActivity(theWS, agent("main"), prose())

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "thinking" {
		t.Fatalf("status = %q, want thinking once the retried call is answered", got)
	}
}

func TestAnotherAgentsAnswerDoesNotEndApiRetrying(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch})
	r.OnApiError(theWS, agent("main"), apiRetry())

	// Act.
	r.OnActivity(theWS, agent("sub-1"), prose())

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "api_retrying" {
		t.Fatalf("status = %q, want api_retrying while the main agent's call is still retried", got)
	}
}

func TestAToolFrameWithoutUsageDoesNotEndApiRetrying(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch})
	r.OnApiError(theWS, agent("main"), apiRetry())

	// Act.
	r.OnActivity(theWS, agent("main"), &conversationv1.AgentActivity{Item: &conversationv1.AgentActivity_Read{}})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "api_retrying" {
		t.Fatalf("status = %q, want api_retrying until the call is answered", got)
	}
}

func TestANewTurnEndsApiRetrying(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch})
	r.OnApiError(theWS, agent("main"), apiRetry())

	// Act.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "submitting" {
		t.Fatalf("status = %q, want submitting for the prompt that opened the new turn", got)
	}
}

func TestTheTurnsEndEndsApiRetrying(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch})
	r.OnApiError(theWS, agent("main"), apiRetry())

	// Act.
	r.SetTurnEnded(theWS, wsm.CloseFailed)

	// Assert.
	if got := statusName(onlyRow(t, r)); got == "api_retrying" {
		t.Fatalf("status = api_retrying after the turn ended")
	}
}

func TestAVendorBlockOutranksApiRetrying(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch})
	r.OnApiError(theWS, agent("main"), apiRetry())

	// Act.
	r.OnSessionUpdate(theWS, rejectedRateLimit())

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "vendor_blocked" {
		t.Fatalf("status = %q, want vendor_blocked over the retry", got)
	}
}

// A VENDOR THAT WILL NOT START IS A VENDOR FAULT (owner ruling, 2026-10-02):
// a run being retried and a stopped one both draw `vendor_fault`, over a
// connected link and over the dead one a stopped start leaves behind.
func TestTheVendorStartRunDrawsTheVendorFault(t *testing.T) {
	tests := []struct {
		name  string
		state sidebar.VendorStart
		link  shimclient.LinkState
	}{
		{"retrying over a connected link", sidebar.VendorStartRetrying, shimclient.LinkConnected},
		{"stopped over a connected link", sidebar.VendorStartStopped, shimclient.LinkConnected},
		{"stopped over the dead link its stop left", sidebar.VendorStartStopped, shimclient.LinkDead},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t))
			r.OnLink(theWS, tt.link)

			// Act.
			r.SetVendorStart(theWS, tt.state)

			// Assert.
			if got := statusName(onlyRow(t, r)); got != "vendor_fault" {
				t.Fatalf("status = %q, want vendor_fault", got)
			}
		})
	}
}

func TestARedialingLinkOutranksARetriedVendorStart(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetVendorStart(theWS, sidebar.VendorStartRetrying)

	// Act.
	r.OnLink(theWS, shimclient.LinkRedialing)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "severed" {
		t.Fatalf("status = %q, want severed: agent-repl's own fault outranks the vendor's", got)
	}
}

func TestANetworkFaultDrawsTheNetworkArm(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.NetworkFaultOpened(theWS, "net-1")

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "network_fault" {
		t.Fatalf("status = %q, want network_fault", got)
	}
}

func TestANetworkFaultOutranksTheVendorStartRun(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetVendorStart(theWS, sidebar.VendorStartRetrying)

	// Act.
	r.NetworkFaultOpened(theWS, "net-1")

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "network_fault" {
		t.Fatalf("status = %q, want network_fault over the vendor fault", got)
	}
}

func TestADeadLinkOutranksANetworkFault(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.NetworkFaultOpened(theWS, "net-1")

	// Act.
	r.OnLink(theWS, shimclient.LinkDead)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "dead" {
		t.Fatalf("status = %q, want dead: agent-repl's own fault outranks the network's", got)
	}
}

func TestClosingTheNetworkFaultRestoresTheRow(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.NetworkFaultOpened(theWS, "net-1")

	// Act.
	r.FaultClosed(theWS, "net-1")

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "ready" {
		t.Fatalf("status = %q, want ready once the network is back", got)
	}
}

func TestClosingAFaultTheRosterNeverHeldPublishesNothing(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	before, _ := r.Topic().Latest()

	// Act.
	r.FaultClosed(theWS, "never-opened")

	// Assert.
	if after, _ := r.Topic().Latest(); after != before {
		t.Fatal("closing a fault the roster never held republished the roster")
	}
}

func TestAVendorStartRunThatEndsLeavesTheLinkToDraw(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetVendorStart(theWS, sidebar.VendorStartRetrying)

	// Act.
	r.SetVendorStart(theWS, sidebar.VendorStartNone)

	// Assert.
	if got := statusName(onlyRow(t, r)); got == "vendor_fault" {
		t.Fatalf("status = %q, want the connected link's own arm once the run ended", got)
	}
}

func TestARedialingLinkOutranksTheVendorStartRun(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetVendorStart(theWS, sidebar.VendorStartStopped)

	// Act.
	r.OnLink(theWS, shimclient.LinkRedialing)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "severed" {
		t.Fatalf("status = %q, want severed", got)
	}
}
