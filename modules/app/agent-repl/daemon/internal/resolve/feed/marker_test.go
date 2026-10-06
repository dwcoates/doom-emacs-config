package feed

import (
	"strings"
	"testing"

	"google.golang.org/protobuf/reflect/protoreflect"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/resolve/turnfault"
	"claude-repld/internal/wsm"
)

// THE OUTCOME MARKER (owner ruling, 2026-10-06): each ending's marker family
// follows the fault the workspace status raises for it, a neutral marker
// carries no expansion, and a fault's expansion holds only sourced fields.

// markerOf is a turn's ending marker, from whichever outcome arm carries it.
func (h *harness) markerOf(turn string) *frontendv1.FeedOutcomeMarker {
	h.t.Helper()
	ended := h.terminalRow(turn)
	if m := ended.GetErrored().GetMarker(); m != nil {
		return m
	}
	return ended.GetInterrupted().GetMarker()
}

// markerFamily names a marker's family arm.
func markerFamily(m *frontendv1.FeedOutcomeMarker) string {
	switch m.GetFamily().(type) {
	case *frontendv1.FeedOutcomeMarker_Neutral:
		return "neutral"
	case *frontendv1.FeedOutcomeMarker_VendorFault:
		return "vendor_fault"
	case *frontendv1.FeedOutcomeMarker_AgentReplFault:
		return "agent_repl_fault"
	}
	return "unset"
}

func apiFailure(failed *conversationv1.ApiRequestFailed) *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: failed}}
}

func TestEveryFailureArmDrawsTheMarkerFamilyItsFaultTakes(t *testing.T) {
	oneof := (&conversationv1.AgentFailure{}).ProtoReflect().Descriptor().Oneofs().ByName("failure")
	fields := oneof.Fields()
	want := map[string]string{
		"query_died":          "agent_repl_fault",
		"stop_hook_prevented": "neutral",
		"tool_deferred":       "neutral",
	}
	for i := 0; i < fields.Len(); i++ {
		field := fields.Get(i)
		t.Run(string(field.Name()), func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.deliverPrompt("turn-1", "hello")
			failure := &conversationv1.AgentFailure{}
			m := failure.ProtoReflect()
			m.Set(field, m.NewField(field))
			expected, special := want[string(field.Name())]
			if !special {
				expected = "vendor_fault"
			}

			// Act
			h.terminal("turn-1", nil, failure)

			// Assert
			if got := markerFamily(h.markerOf("turn-1")); got != expected {
				t.Fatalf("family = %s, want %s", got, expected)
			}
		})
	}
}

func TestAVendorMarkerIsLabelledVendorErrorWithItsCauseAsDetail(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	failure := apiFailure(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_RateLimited{RateLimited: &conversationv1.ApiRateLimited{}}})

	// Act
	h.terminal("turn-1", nil, failure)

	// Assert
	m := h.markerOf("turn-1")
	if m.GetLabel().GetText() != "vendor error" || m.GetDetail().GetText() != "rate limited" {
		t.Fatalf("marker = %q · %q, want vendor error · rate limited", m.GetLabel().GetText(), m.GetDetail().GetText())
	}
}

func TestAStopHookMarkerIsNeutralAndCarriesNoExpansion(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StopHookPrevented{StopHookPrevented: &conversationv1.AgentStoppedByStopHook{}}})

	// Assert
	m := h.markerOf("turn-1")
	if m.GetNeutral() == nil || m.GetLabel().GetText() != "Stop hook ended the run" || m.Detail != nil {
		t.Fatalf("marker = %+v, want the neutral \"Stop hook ended the run\"", m)
	}
}

func TestAnInterruptDrawsTheNeutralMarkerUnlessAnInterjectionSupersededIt(t *testing.T) {
	cases := []struct {
		name       string
		byUser     *conversationv1.AgentInterruptedByUser
		wantMarker bool
	}{
		{name: "a direct stop", byUser: &conversationv1.AgentInterruptedByUser{Command: &conversationv1.AgentInterruptedByUser_Direct{Direct: &conversationv1.AgentInterruptedByUserDirect{}}}, wantMarker: true},
		{name: "a stop that stated no command", byUser: &conversationv1.AgentInterruptedByUser{}, wantMarker: true},
		{name: "an interjection", byUser: &conversationv1.AgentInterruptedByUser{Command: &conversationv1.AgentInterruptedByUser_Interjection{Interjection: &conversationv1.AgentInterruptedByUserInterjection{}}}, wantMarker: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.deliverPrompt("turn-1", "hello")

			// Act
			h.terminal("turn-1", &conversationv1.AgentSuccess{Outcome: &conversationv1.AgentSuccess_Interrupted{Interrupted: &conversationv1.AgentInterrupted{
				Cause: &conversationv1.AgentInterrupted_ByUser{ByUser: tc.byUser}}}}, nil)

			// Assert
			m := h.terminalRow("turn-1").GetInterrupted().GetMarker()
			if (m != nil) != tc.wantMarker {
				t.Fatalf("marker = %+v, want present = %v", m, tc.wantMarker)
			}
			if tc.wantMarker && (m.GetNeutral() == nil || m.GetLabel().GetText() != "interrupted") {
				t.Fatalf("marker = %+v, want the neutral \"interrupted\"", m)
			}
		})
	}
}

func TestAVendorExpansionCarriesTheTimeTypeAndMessage(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	failure := apiFailure(&conversationv1.ApiRequestFailed{Message: "Overloaded", Kind: &conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{}}})

	// Act
	h.terminal("turn-1", nil, failure)

	// Assert
	x := h.markerOf("turn-1").GetVendorFault().GetExpansion()
	if x.GetTime().GetAtMs() != h.terminalRow("turn-1").GetEndedAtMs() {
		t.Fatalf("time = %d, want the ending's instant", x.GetTime().GetAtMs())
	}
	if x.GetErrorType().GetText() != "overloaded" || x.GetMessage().GetText() != "Overloaded" {
		t.Fatalf("error = %q: %q, want overloaded: Overloaded", x.GetErrorType().GetText(), x.GetMessage().GetText())
	}
}

func TestAVendorExpansionWithNoMessageCarriesNone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}}})

	// Assert
	if x := h.markerOf("turn-1").GetVendorFault().GetExpansion(); x.Message != nil {
		t.Fatalf("message = %+v, want none: the vendor recorded none", x.Message)
	}
}

func TestAVendorExpansionCountsTheRetriesTheVendorAnnounced(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	turn := &conversationv1.TurnId{Value: "turn-1"}
	for _, attempt := range []uint32{1, 2, 3} {
		h.resolver.OnApiError(testWorkspace, mainAgent(), &conversationv1.ApiRequestFailed{
			Message: "overloaded", Retry: &conversationv1.ApiRetry{Attempt: attempt, MaxRetries: 10},
		}, turn, nil)
	}

	// Act
	h.terminal("turn-1", nil, apiFailure(&conversationv1.ApiRequestFailed{Message: "gave up", Kind: &conversationv1.ApiRequestFailed_Internal{Internal: &conversationv1.ApiInternal{}}}))

	// Assert
	if got := h.markerOf("turn-1").GetVendorFault().GetExpansion().GetRetries().GetCount(); got != 3 {
		t.Fatalf("retries = %d, want 3", got)
	}
}

func TestAVendorExpansionWithNoRetryScheduleCountsNone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.terminal("turn-1", nil, apiFailure(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Internal{Internal: &conversationv1.ApiInternal{}}}))

	// Assert
	if x := h.markerOf("turn-1").GetVendorFault().GetExpansion(); x.Retries != nil {
		t.Fatalf("retries = %+v, want none", x.Retries)
	}
}

func TestAVendorExpansionCountsDownTheVendorsStatedWait(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	wait := int64(42_000)

	// Act
	h.terminal("turn-1", nil, apiFailure(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_RateLimited{RateLimited: &conversationv1.ApiRateLimited{RetryAfterMs: &wait}}}))

	// Assert
	ended := h.terminalRow("turn-1")
	if got := h.markerOf("turn-1").GetVendorFault().GetExpansion().GetRetryAt().GetAtMs(); got != ended.GetEndedAtMs()+wait {
		t.Fatalf("retry_at = %d, want the ending plus the wait", got)
	}
}

func TestAVendorExpansionNamesTheModelAndAccountSeenLive(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.OnSessionStarted(testWorkspace, &conversationv1.SessionStarted{EffectiveModel: &conversationv1.AgentModel{Name: "claude-opus-5"}})
	h.resolver.SetAccount(testWorkspace, "me@example.com")
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}}})

	// Assert
	x := h.markerOf("turn-1").GetVendorFault().GetExpansion()
	if x.GetModel().GetName() != "claude-opus-5" || x.GetAccount().GetEmail() != "me@example.com" {
		t.Fatalf("model, account = %q, %q, want the session's", x.GetModel().GetName(), x.GetAccount().GetEmail())
	}
}

func TestAVendorExpansionNamesTheModelAChangeInstalled(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.OnSessionStarted(testWorkspace, &conversationv1.SessionStarted{EffectiveModel: &conversationv1.AgentModel{Name: "a"}})
	h.resolver.OnSessionUpdate(testWorkspace, &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_ModelChanged{
		ModelChanged: &conversationv1.SessionModelChanged{EffectiveModel: &conversationv1.AgentModel{Name: "b"}}}})
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}}})

	// Assert
	if got := h.markerOf("turn-1").GetVendorFault().GetExpansion().GetModel().GetName(); got != "b" {
		t.Fatalf("model = %q, want the changed model", got)
	}
}

func TestAVendorExpansionNamesNoModelOrAccountTheDaemonNeverSaw(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}}})

	// Assert
	x := h.markerOf("turn-1").GetVendorFault().GetExpansion()
	if x.Model != nil || x.Account != nil {
		t.Fatalf("model, account = %+v, %+v, want neither", x.Model, x.Account)
	}
}

func TestALoggedOutAccountIsNotNamed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.SetAccount(testWorkspace, "")
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}}})

	// Assert
	if x := h.markerOf("turn-1").GetVendorFault().GetExpansion(); x.Account != nil {
		t.Fatalf("account = %+v, want none for a logged-out root", x.Account)
	}
}

func TestAnAccountCauseOffersTheSignInAction(t *testing.T) {
	cases := []struct {
		name string
		kind *conversationv1.ApiRequestFailed
		want bool
	}{
		{name: "a rejected credential", kind: &conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}}}, want: true},
		{name: "an organization not allowed", kind: &conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_OauthOrgNotAllowed{OauthOrgNotAllowed: &conversationv1.ApiOauthOrgNotAllowed{}}}, want: true},
		{name: "billing has no sign-in cure", kind: &conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_BillingError{BillingError: &conversationv1.ApiBillingError{}}}, want: false},
		{name: "an overload", kind: &conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{}}}, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.deliverPrompt("turn-1", "hello")

			// Act
			h.terminal("turn-1", nil, apiFailure(tc.kind))

			// Assert
			if got := h.markerOf("turn-1").GetVendorFault().GetExpansion().SignIn != nil; got != tc.want {
				t.Fatalf("sign_in = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestATurnThatProducedNothingOffersToResendItsPrompt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}}})

	// Assert
	said := h.markerOf("turn-1").GetVendorFault().GetExpansion().GetResend().GetSaid()
	if got := said.GetContent().GetBlocks()[0].GetText().GetText(); got != "hello" {
		t.Fatalf("resend said = %q, want the prompt as said", got)
	}
}

func TestATurnThatProducedSomethingOffersNoResend(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.liveResponse("unit-1", "partial answer", "turn-1")

	// Act
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}}})

	// Assert
	if x := h.markerOf("turn-1").GetVendorFault().GetExpansion(); x.Resend != nil {
		t.Fatalf("resend = %+v, want none: the turn produced a response", x.Resend)
	}
}

func TestAQueryDeathExpandsToWhatDiedAndWhatItThrew(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.queryDied(&conversationv1.SessionQueryDied{Cause: &conversationv1.SessionQueryDied_IteratorFailure{
		IteratorFailure: &conversationv1.SessionQueryIteratorFailure{Cause: "socket hang up"}}})

	// Assert
	m := h.markerOf("turn-1")
	if m.GetLabel().GetText() != "agent-repl" || m.GetDetail().GetText() != "query died" {
		t.Fatalf("marker = %q · %q, want agent-repl · query died", m.GetLabel().GetText(), m.GetDetail().GetText())
	}
	query := m.GetAgentReplFault().GetExpansion().GetWhatDied().GetQuery()
	if query.GetThrown().GetText() != "socket hang up" || query.GetLine().GetText() == "" {
		t.Fatalf("what died = %+v, want the query with what it threw", query)
	}
}

func TestAnAgentProcessDeathExpandsToTheProcess(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.closeTurn("turn-1", wsm.CloseAgentDied)

	// Assert
	m := h.markerOf("turn-1")
	want, _ := turnfault.OfClose(wsm.CloseAgentDied)
	process := m.GetAgentReplFault().GetExpansion().GetWhatDied().GetProcess()
	if m.GetDetail().GetText() != "process died" || process.GetLine().GetText() != want.Sentence {
		t.Fatalf("marker = %+v, want agent-repl · process died expanding to the process", m)
	}
}

func TestAnOrphanedCloseDrawsAVendorMarkerByItsStopWord(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.closeTurn("turn-1", wsm.CloseOrphaned)

	// Assert
	if got := h.markerOf("turn-1").GetVendorFault().GetExpansion().GetErrorType().GetText(); got != "closed:orphaned" {
		t.Fatalf("error type = %q, want closed:orphaned", got)
	}
}

func TestTheSessionsNextStartSaysTheRestartWorked(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.queryDied(&conversationv1.SessionQueryDied{})
	h.nowMs += 5_000

	// Act
	h.resolver.OnSessionStarted(testWorkspace, &conversationv1.SessionStarted{VendorSessionId: "vendor-2"})

	// Assert
	restarted := h.markerOf("turn-1").GetAgentReplFault().GetExpansion().GetRestarted()
	if restarted.GetAtMs() != h.nowMs {
		t.Fatalf("restarted = %+v, want the start's instant", restarted)
	}
}

func TestNoRestartIsClaimedBeforeTheSessionStartsAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.queryDied(&conversationv1.SessionQueryDied{})

	// Assert
	if x := h.markerOf("turn-1").GetAgentReplFault().GetExpansion(); x.Restarted != nil {
		t.Fatalf("restarted = %+v, want none until a start is seen", x.Restarted)
	}
}

func TestAVendorFaultIsNeverToldARestart(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}}})
	before := h.terminalRow("turn-1")

	// Act
	h.resolver.OnSessionStarted(testWorkspace, &conversationv1.SessionStarted{VendorSessionId: "vendor-2"})

	// Assert
	if after := h.terminalRow("turn-1"); after.String() != before.String() {
		t.Fatalf("a vendor fault's ending changed on a session start")
	}
}

func TestTheUsersDenialDrawsTheNeutralMarker(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ask("unit-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to run a command"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Act
	h.ask("unit-1", "unit-1", &conversationv1.AgentPermissionSuccess{
		Decision: &conversationv1.AgentPermissionSuccess_Denied{Denied: &conversationv1.AgentPermissionDenied{
			By: &conversationv1.AgentPermissionDenied_User{User: &conversationv1.AgentPermissionDeniedByUser{}},
		}},
	})

	// Assert
	m := h.permissionCard().GetAnswered().GetDeniedByUser().GetMarker()
	if m.GetNeutral() == nil || m.GetLabel().GetText() != "permission denied by you" || m.Detail != nil {
		t.Fatalf("marker = %+v, want the neutral \"permission denied by you\"", m)
	}
}

func TestABrokenPlanDrawsTheNeutralMarkerWithItsReason(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.planFrame("unit-1", &conversationv1.AgentPlanModeFailure{
		Error: &conversationv1.AgentToolFailure{
			Content: &conversationv1.ToolResultContent{Blocks: []*conversationv1.ToolResultContentBlock{{
				Block: &conversationv1.ToolResultContentBlock_Text{Text: &conversationv1.TextBlock{Text: "plan mode is unavailable"}},
			}}},
		},
	})

	// Assert
	m := h.planBubble().GetFailed().GetMarker()
	if m.GetNeutral() == nil || m.GetLabel().GetText() != "plan failed" || m.GetDetail().GetText() != "plan mode is unavailable" {
		t.Fatalf("marker = %+v, want the neutral plan-failed marker with its reason", m)
	}
}

// A FAILED MERGE NEVER DRAWS A MARKER (owner ruling, 2026-10-06): its merge
// bubble is its whole account, so no merge message can carry one.
func TestNoMergeMessageCanCarryAnOutcomeMarker(t *testing.T) {
	// Arrange
	file := (&frontendv1.FeedOutcomeMarker{}).ProtoReflect().Descriptor().ParentFile()
	marker := (&frontendv1.FeedOutcomeMarker{}).ProtoReflect().Descriptor().FullName()
	var reaches func(md protoreflect.MessageDescriptor, seen map[protoreflect.FullName]bool) bool
	reaches = func(md protoreflect.MessageDescriptor, seen map[protoreflect.FullName]bool) bool {
		if md.FullName() == marker {
			return true
		}
		if seen[md.FullName()] {
			return false
		}
		seen[md.FullName()] = true
		fields := md.Fields()
		for i := 0; i < fields.Len(); i++ {
			if sub := fields.Get(i).Message(); sub != nil && reaches(sub, seen) {
				return true
			}
		}
		return false
	}
	messages := file.Messages()
	checked := 0
	for i := 0; i < messages.Len(); i++ {
		md := messages.Get(i)
		if !strings.HasPrefix(string(md.Name()), "FeedMerge") {
			continue
		}
		checked++

		// Act
		got := reaches(md, map[protoreflect.FullName]bool{})

		// Assert
		if got {
			t.Fatalf("%s can carry a FeedOutcomeMarker; a failed merge draws none", md.FullName())
		}
	}
	if checked == 0 {
		t.Fatal("no FeedMerge message was checked")
	}
}
