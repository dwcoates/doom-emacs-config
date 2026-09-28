package topbar

import (
	"slices"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/types/known/structpb"

	"claude-repld/internal/ids"
)

// unmodeledStart is an unmodeled tool call as issued.
func unmodeledStart(unit, tool string, args map[string]any) *conversationv1.AgentActivity {
	var arguments *structpb.Struct
	if args != nil {
		built, err := structpb.NewStruct(args)
		if err != nil {
			panic(err)
		}
		arguments = built
	}
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Unmodeled{
			Unmodeled: &conversationv1.AgentUnmodeled{
				Result: &conversationv1.AgentUnmodeled_Start{
					Start: &conversationv1.AgentUnmodeledStart{
						ToolName:  tool,
						Arguments: arguments,
						StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: instant.UnixMilli()},
					},
				},
			},
		},
	}
}

// responseSettled is a settled response unit, optionally carrying its usage.
func responseSettled(unit string, u *conversationv1.TokenUsage) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Usage:      u,
		Item: &conversationv1.AgentActivity_Response{
			Response: &conversationv1.AgentResponse{
				Result: &conversationv1.AgentResponse_Success{
					Success: &conversationv1.AgentResponseSuccess{},
				},
			},
		},
	}
}

// usage builds one canonical token record.
func usage(read, written, unwritten, output, thinking uint64) *conversationv1.TokenUsage {
	return &conversationv1.TokenUsage{
		InputHits:            &conversationv1.TokenCacheHits{Read: read},
		InputMisses:          &conversationv1.TokenCacheMisses{Written: written, Unwritten: unwritten},
		OutputTokens:         output,
		OutputThinkingTokens: thinking,
	}
}

// diagnostics is one health push.
func diagnostics(faults []*conversationv1.SessionFault, windows []*conversationv1.SessionDegradedWindow) *conversationv1.SessionUpdate {
	report := &conversationv1.SessionDiagnostics{DegradedWindows: windows}
	if len(faults) == 0 {
		report.Health = &conversationv1.SessionDiagnostics_Healthy{
			Healthy: &conversationv1.SessionHealthy{},
		}
	} else {
		report.Health = &conversationv1.SessionDiagnostics_Unhealthy{
			Unhealthy: &conversationv1.SessionUnhealthy{Faults: faults},
		}
	}
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{Diagnostics: report},
	}
}

// warnings is the last published warning list.
func warnings(t *testing.T, h *harness) []*frontendv1.TopbarWarning {
	t.Helper()
	return h.view(t).GetWarnings().GetWarnings()
}

func TestNothingWrongDrawsAnEmptyList(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.ready(t)

	// Assert
	if got := warnings(t, h); len(got) != 0 {
		t.Fatalf("warnings = %+v, want an empty list saying nothing is wrong", got)
	}
}

func TestAnUnmodeledToolRaisesOneWarning(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, unmodeledStart("u1", "mcp__github__list_prs", nil))

	// Assert
	got := warnings(t, h)
	if len(got) != 1 || got[0].GetUnmodeledTool() == nil {
		t.Fatalf("warnings = %+v, want one unmodeled-tool warning", got)
	}
	if name := got[0].GetUnmodeledTool().GetToolName().GetText(); name != "mcp__github__list_prs" {
		t.Fatalf("tool name = %q, want the producer's own name", name)
	}
}

func TestTheSameUnmodeledToolWarnsOnlyOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, unmodeledStart("u1", "mcp__github__list_prs", nil))
	h.r.OnActivity(testWS, nil, unmodeledStart("u2", "mcp__github__list_prs", nil))

	// Assert
	if got := warnings(t, h); len(got) != 1 {
		t.Fatalf("warnings = %d, want one per DISTINCT name", len(got))
	}
}

func TestTwoDistinctUnmodeledToolsWarnSeparately(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, unmodeledStart("u1", "tool_a", nil))
	h.r.OnActivity(testWS, nil, unmodeledStart("u2", "tool_b", nil))

	// Assert
	if got := warnings(t, h); len(got) != 2 {
		t.Fatalf("warnings = %d, want one per distinct name", len(got))
	}
}

func TestUnmodeledArgumentsAreAbbreviatedNotDumped(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, unmodeledStart("u1", "tool_a", map[string]any{
		"repo":    "anthropics/claude-code",
		"limit":   25.0,
		"filters": []any{"open", "mine"},
		"nested":  map[string]any{"a": 1.0, "b": 2.0},
	}))

	// Assert
	lines := warnings(t, h)[0].GetUnmodeledTool().GetArgumentLines()
	if len(lines) != 4 {
		t.Fatalf("lines = %+v, want one legible line per argument", lines)
	}
	wants := []string{
		"filters: 2 items",
		"limit: 25",
		"nested: {2 fields}",
		"repo: anthropics/claude-code",
	}
	for i, want := range wants {
		if lines[i].GetText() != want {
			t.Fatalf("line %d = %q, want %q", i, lines[i].GetText(), want)
		}
	}
}

func TestAnArgumentlessCallCarriesItsNameAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, unmodeledStart("u1", "tool_a", nil))

	// Assert
	if lines := warnings(t, h)[0].GetUnmodeledTool().GetArgumentLines(); len(lines) != 0 {
		t.Fatalf("lines = %+v, want none: the name alone carries the overlay", lines)
	}
}

func TestALiveDetachedUnmodeledItemWarns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetDetachedUnmodeled(testWS, []DetachedUnmodeled{
		{ToolName: "mcp__runner__watch", StartedAt: instant},
	})

	// Assert
	got := warnings(t, h)
	detail := got[0].GetDetachedUnmodeled()
	if detail == nil {
		t.Fatalf("warnings = %+v, want the detached-unmodeled arm", got)
	}
	if detail.GetStartedAtMs() != instant.UnixMilli() {
		t.Fatalf("started_at = %d, want the overlay's clock instant", detail.GetStartedAtMs())
	}
}

func TestEachLiveDetachedUnmodeledItemWarnsSeparately(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetDetachedUnmodeled(testWS, []DetachedUnmodeled{
		{ToolName: "a", StartedAt: instant},
		{ToolName: "b", StartedAt: instant.Add(time.Second)},
	})

	// Assert
	if got := warnings(t, h); len(got) != 2 {
		t.Fatalf("warnings = %d, want one per live item", len(got))
	}
}

func TestADetachedUnmodeledItemLeavingRetractsItsWarning(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	h.r.SetDetachedUnmodeled(testWS, []DetachedUnmodeled{{ToolName: "a", StartedAt: instant}})

	// Act
	h.r.SetDetachedUnmodeled(testWS, nil)

	// Assert
	if got := warnings(t, h); len(got) != 0 {
		t.Fatalf("warnings = %+v, want the item's warning retracted", got)
	}
}

func TestAShimFaultWarnsWithItsKindRespelled(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnSessionUpdate(testWS, diagnostics([]*conversationv1.SessionFault{{
		Kind: &conversationv1.SessionFault_StoreUnreachable{
			StoreUnreachable: &conversationv1.SessionFaultStoreUnreachable{},
		},
	}}, nil))

	// Assert
	got := warnings(t, h)
	detail := got[0].GetSessionFault()
	if detail == nil {
		t.Fatalf("warnings = %+v, want the session-fault arm", got)
	}
	if detail.GetComponent().GetText() != "store client" {
		t.Fatalf("component = %q, want the kind's own component", detail.GetComponent().GetText())
	}
	if !contains(got[0].GetLine().GetText(), "store cannot be reached") {
		t.Fatalf("line = %q, want the kind's sentence", got[0].GetLine().GetText())
	}
}

func TestTheShimsOwnComponentWinsWhereItStatedOne(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnSessionUpdate(testWS, diagnostics([]*conversationv1.SessionFault{{
		Component: "store client (batch writer)",
		Detail:    "the socket refused three times",
		Kind: &conversationv1.SessionFault_StoreUnreachable{
			StoreUnreachable: &conversationv1.SessionFaultStoreUnreachable{},
		},
	}}, nil))

	// Assert
	detail := warnings(t, h)[0].GetSessionFault()
	if detail.GetComponent().GetText() != "store client (batch writer)" {
		t.Fatalf("component = %q, want the shim's own spelling", detail.GetComponent().GetText())
	}
	if detail.GetDetail().GetText() != "the socket refused three times" {
		t.Fatalf("detail = %q, want the shim's own account", detail.GetDetail().GetText())
	}
}

func TestAnUnclassifiedFaultStillDrawsASentence(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnSessionUpdate(testWS, diagnostics([]*conversationv1.SessionFault{{}}, nil))

	// Assert
	got := warnings(t, h)
	if len(got) != 1 || got[0].GetLine().GetText() == "" {
		t.Fatalf("warnings = %+v, want a fault drawn with a sentence rather than a blank", got)
	}
}

func TestEveryFaultKindHasItsOwnSentence(t *testing.T) {
	// Arrange
	kinds := []*conversationv1.SessionFault{
		{Kind: &conversationv1.SessionFault_StoreUnreachable{StoreUnreachable: &conversationv1.SessionFaultStoreUnreachable{}}},
		{Kind: &conversationv1.SessionFault_ConverterDefect{ConverterDefect: &conversationv1.SessionFaultConverterDefect{}}},
		{Kind: &conversationv1.SessionFault_LogSinkPoisoned{LogSinkPoisoned: &conversationv1.SessionFaultLogSinkPoisoned{}}},
		{Kind: &conversationv1.SessionFault_KeepaliveFailed{KeepaliveFailed: &conversationv1.SessionFaultKeepaliveFailed{}}},
		{Kind: &conversationv1.SessionFault_VendorQueryFailed{VendorQueryFailed: &conversationv1.SessionFaultVendorQueryFailed{}}},
		{},
	}
	seen := map[string]bool{}

	// Act
	for _, fault := range kinds {
		component, line := faultKindWords(fault)
		seen[component+"|"+line] = true
	}

	// Assert
	if len(seen) != len(kinds) {
		t.Fatalf("respellings = %+v, want one per kind including the unclassified one", seen)
	}
}

func TestAHealthyDiagnosticsRetractsEveryFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	h.r.OnSessionUpdate(testWS, diagnostics([]*conversationv1.SessionFault{{
		Kind: &conversationv1.SessionFault_ConverterDefect{
			ConverterDefect: &conversationv1.SessionFaultConverterDefect{},
		},
	}}, nil))

	// Act
	h.r.OnSessionUpdate(testWS, diagnostics(nil, nil))

	// Assert
	if got := warnings(t, h); len(got) != 0 {
		t.Fatalf("warnings = %+v, want every fault retracted by the healthy verdict", got)
	}
}

func TestAnOpenDegradedWindowWarnsAsOpen(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnSessionUpdate(testWS, diagnostics(nil, []*conversationv1.SessionDegradedWindow{{
		Component: "converter",
		Reason:    "the queue backed up",
		BeganAtMs: instant.UnixMilli(),
		Extent:    &conversationv1.SessionDegradedWindow_Open{Open: &conversationv1.SessionDegradedOpen{}},
	}}))

	// Assert
	detail := warnings(t, h)[0].GetDegradedWindow()
	if detail.GetOpen() == nil {
		t.Fatalf("extent = %+v, want the open arm — the loudest state", detail.GetExtent())
	}
	if detail.GetReason().GetText() != "the queue backed up" {
		t.Fatalf("reason = %q, want the shim's stated reason", detail.GetReason().GetText())
	}
}

func TestAClosedDegradedWindowCarriesWhatItCost(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnSessionUpdate(testWS, diagnostics(nil, []*conversationv1.SessionDegradedWindow{{
		Component: "converter",
		BeganAtMs: instant.UnixMilli(),
		Extent: &conversationv1.SessionDegradedWindow_Closed{
			Closed: &conversationv1.SessionDegradedClosed{
				EndedAtMs: instant.Add(time.Minute).UnixMilli(), DroppedCount: 12,
			},
		},
	}}))

	// Assert
	closed := warnings(t, h)[0].GetDegradedWindow().GetClosed()
	if closed.GetDroppedCount() != 12 {
		t.Fatalf("dropped = %d, want the window's cost", closed.GetDroppedCount())
	}
	if !contains(warnings(t, h)[0].GetLine().GetText(), "12 observations") {
		t.Fatalf("line = %q, want the dropped count in the sentence", warnings(t, h)[0].GetLine().GetText())
	}
}

func TestAClosedWindowIsKeptAsEvidence(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	h.r.OnSessionUpdate(testWS, diagnostics(nil, []*conversationv1.SessionDegradedWindow{{
		Component: "converter",
		BeganAtMs: instant.UnixMilli(),
		Extent: &conversationv1.SessionDegradedWindow_Closed{
			Closed: &conversationv1.SessionDegradedClosed{DroppedCount: 3},
		},
	}}))

	// Act: a later healthy push carrying no windows at all.
	h.r.OnSessionUpdate(testWS, diagnostics(nil, nil))

	// Assert
	if got := warnings(t, h); len(got) != 1 {
		t.Fatalf("warnings = %+v, want the closed window kept: it is evidence of a hole", got)
	}
}

func TestAReObservedFaultKeepsItsPlaceInTheList(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	fault := []*conversationv1.SessionFault{{
		Kind: &conversationv1.SessionFault_ConverterDefect{
			ConverterDefect: &conversationv1.SessionFaultConverterDefect{},
		},
	}}
	h.r.OnSessionUpdate(testWS, diagnostics(fault, nil))
	h.r.OnActivity(testWS, nil, unmodeledStart("u1", "tool_a", nil))
	before := warnings(t, h)[0].GetUnmodeledTool()

	// Act: the same fault is reported again.
	h.r.OnSessionUpdate(testWS, diagnostics(fault, nil))

	// Assert
	if before == nil || warnings(t, h)[0].GetUnmodeledTool() == nil {
		t.Fatalf("warnings = %+v, want the newer unmodeled warning still first", warnings(t, h))
	}
}

func TestTheDropdownIsCapped(t *testing.T) {
	// Arrange
	h := newHarness(t, WithWarningCap(2))
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, unmodeledStart("u1", "tool_a", nil))
	h.r.OnActivity(testWS, nil, unmodeledStart("u2", "tool_b", nil))
	h.r.OnActivity(testWS, nil, unmodeledStart("u3", "tool_c", nil))

	// Assert
	got := warnings(t, h)
	if len(got) != 2 {
		t.Fatalf("warnings = %d, want the cap enforced", len(got))
	}
	if !contains(got[0].GetLine().GetText(), "tool_c") {
		t.Fatalf("first = %q, want the newest first", got[0].GetLine().GetText())
	}
}

func TestAResponseWithNoUsageRaisesTheAccountingWarning(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, responseSettled("r1", nil))

	// Assert
	got := warnings(t, h)
	detail := got[0].GetAccounting()
	if detail == nil {
		t.Fatalf("warnings = %+v, want the accounting arm", got)
	}
	if detail.GetLines()[0].GetText() != "1 response missing usage" {
		t.Fatalf("evidence = %q, want the count", detail.GetLines()[0].GetText())
	}
}

func TestTheAccountingWarningRetractsWhenItReconciles(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	h.r.OnActivity(testWS, nil, responseSettled("r1", nil))

	// Act
	h.r.OnActivity(testWS, nil, responseSettled("r1", usage(0, 100, 0, 0, 0)))

	// Assert
	if got := warnings(t, h); len(got) != 0 {
		t.Fatalf("warnings = %+v, want the accounting warning retracted once it reconciles", got)
	}
}

func TestAContradictoryUsageRaisesTheAccountingWarning(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	h.r.OnActivity(testWS, nil, responseSettled("r1", usage(0, 100, 0, 0, 0)))

	// Act
	h.r.OnActivity(testWS, nil, responseSettled("r1", usage(0, 900, 0, 0, 0)))

	// Assert
	detail := warnings(t, h)[0].GetAccounting()
	if detail == nil || len(detail.GetLines()) == 0 {
		t.Fatalf("warnings = %+v, want the contradiction named", warnings(t, h))
	}
}

func TestThinkingAboveOutputRaisesTheAccountingWarning(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, responseSettled("r1", usage(0, 10, 0, 5, 9)))

	// Assert
	if warnings(t, h)[0].GetAccounting() == nil {
		t.Fatalf("thinking tokens above output tokens must not reconcile")
	}
}

func TestEveryWarningLineIsNonEmpty(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, unmodeledStart("u1", "tool_a", nil))
	h.r.SetDetachedUnmodeled(testWS, []DetachedUnmodeled{{ToolName: "b", StartedAt: instant}})
	h.r.OnSessionUpdate(testWS, diagnostics([]*conversationv1.SessionFault{{}},
		[]*conversationv1.SessionDegradedWindow{{Component: "converter", BeganAtMs: 1,
			Extent: &conversationv1.SessionDegradedWindow_Open{Open: &conversationv1.SessionDegradedOpen{}}}}))
	h.r.OnActivity(testWS, nil, responseSettled("r1", nil))

	// Assert
	got := warnings(t, h)
	if len(got) != 5 {
		t.Fatalf("warnings = %d, want one of each kind", len(got))
	}
	for _, warning := range got {
		if warning.GetLine().GetText() == "" {
			t.Fatalf("warning = %+v, drew an empty line", warning)
		}
		if warning.GetDetail() == nil {
			t.Fatalf("warning = %+v, carries no overlay", warning)
		}
	}
}

// thinkingUnit is the reasoning unit an API response opens with — the unit
// that CARRIES the response's usage, per AgentActivity.usage's rule.
func thinkingUnit(unit string, u *conversationv1.TokenUsage) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Usage:      u,
		Item:       &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{}},
	}
}

// THE ORDINARY PROSE TURN RECONCILES. Its API response opens with a thinking
// block, so the usage rides the THINKING unit and the response unit carries
// none — which the contract spells as "not the carrying unit", never "free".
// Reconciling by set cardinality across those two key spaces warned about a
// perfectly healthy session.
func TestAResponseWhoseUsageRodeItsThinkingUnitReconciles(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act: ONE API response — a thinking block that carries the usage, then the
	// two prose blocks it yielded, neither of which carries any.
	h.r.OnActivity(testWS, nil, thinkingUnit("t1", usage(0, 100, 0, 40, 10)))
	h.r.OnActivity(testWS, nil, responseSettled("r1", nil))
	h.r.OnActivity(testWS, nil, responseSettled("r2", nil))

	// Assert
	if got := warnings(t, h); len(got) != 0 {
		t.Fatalf("warnings = %+v, want none: the response's usage rode its thinking unit", got)
	}
}

// A RESPONSE WHOSE OWN API RESPONSE CARRIED NO USAGE STILL WARNS, and a later
// response reporting its own usage does not account for it: the bill for the
// first one really is missing, and the warning must not be retracted by
// somebody else's figure.
func TestAResponseWithNoUsageOfItsOwnStillWarnsAfterALaterOneReportsIts(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	h.r.OnActivity(testWS, nil, responseSettled("r1", nil))

	// Act: a later, fully accounted API response.
	h.r.OnActivity(testWS, nil, thinkingUnit("t2", usage(0, 100, 0, 40, 10)))
	h.r.OnActivity(testWS, nil, responseSettled("r2", nil))

	// Assert
	got := warnings(t, h)
	detail := got[0].GetAccounting()
	if detail == nil {
		t.Fatalf("warnings = %+v, want the accounting arm", got)
	}
	if detail.GetLines()[0].GetText() != "1 response missing usage" {
		t.Fatalf("evidence = %q, want exactly the unaccounted response counted",
			detail.GetLines()[0].GetText())
	}
}

func TestADaemonRaisedWarningIsDrawnAsItsLineAlone(t *testing.T) {
	for _, tc := range []struct {
		name      string
		raises    [][2]string
		wantLines []string
	}{
		{
			name:      "one raised condition draws one line with no overlay",
			raises:    [][2]string{{"detached:w1", "detached shell w1 could not be placed"}},
			wantLines: []string{"detached shell w1 could not be placed"},
		},
		{
			name: "raising the same key again restates the line rather than adding one",
			raises: [][2]string{
				{"detached:w1", "first sentence"},
				{"detached:w1", "second sentence"},
			},
			wantLines: []string{"second sentence"},
		},
		{
			name: "two keys draw two lines, newest first",
			raises: [][2]string{
				{"detached:w1", "older"},
				{"detached:w2", "newer"},
			},
			wantLines: []string{"newer", "older"},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.ready(t)

			// Act
			for _, raise := range tc.raises {
				h.r.RaiseWarning(testWS, raise[0], raise[1])
			}

			// Assert
			got := warnings(t, h)
			if len(got) != len(tc.wantLines) {
				t.Fatalf("warnings = %+v, want %d", got, len(tc.wantLines))
			}
			for i, want := range tc.wantLines {
				if line := got[i].GetLine().GetText(); line != want {
					t.Errorf("warning %d line = %q, want %q", i, line, want)
				}
				if got[i].GetDetail() != nil {
					t.Errorf("warning %d carries an overlay %T, want the line alone", i, got[i].GetDetail())
				}
			}
		})
	}
}

// otherWS is a second workspace, for the conditions that stand on every strip.
const otherWS = ids.WorkspaceID("ws-2")

// readyOther binds and readies otherWS as the harness readies testWS.
func readyOther(t *testing.T, h *harness) {
	t.Helper()
	if err := h.r.SetWorkspaceDir(otherWS, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}
	h.r.SetNaming(otherWS, Naming{Title: "other", Branch: "main", DefaultBranch: "main", ConfigDir: "/Users/dev/.claude"})
	h.r.SetAccount(otherWS, testAccount("dev@example.com"))
}

// lines answers a workspace's published warning lines, newest first.
func lines(t *testing.T, h *harness, ws ids.WorkspaceID) []string {
	t.Helper()
	view, ok := h.r.Topic(ws).Latest()
	if !ok {
		t.Fatalf("no topbar published for %s", ws)
	}
	var out []string
	for _, w := range view.GetWarnings().GetWarnings() {
		out = append(out, w.GetLine().GetText())
	}
	return out
}

func TestADaemonWarningStandsOnEveryStrip(t *testing.T) {
	tests := []struct {
		name  string
		setup func(t *testing.T, h *harness, raise func())
	}{
		{"strips that stood before it was raised", func(t *testing.T, h *harness, raise func()) {
			h.ready(t)
			readyOther(t, h)
			raise()
		}},
		{"a strip made after it was raised", func(t *testing.T, h *harness, raise func()) {
			h.ready(t)
			raise()
			readyOther(t, h)
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			tc.setup(t, h, func() { h.r.RaiseDaemonWarning("fault-1", "deploy failed: build webapp: tsc") })

			// Assert
			for _, ws := range []ids.WorkspaceID{testWS, otherWS} {
				if got := lines(t, h, ws); !slices.Contains(got, "deploy failed: build webapp: tsc") {
					t.Fatalf("%s warning lines = %q, want the daemon's line among them", ws, got)
				}
			}
		})
	}
}

func TestADaemonWarningIsItsLineAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.RaiseDaemonWarning("fault-1", "deploy failed: build webapp: tsc")

	// Assert
	got := warnings(t, h)
	if len(got) != 1 || got[0].GetDetail() != nil {
		t.Fatalf("warnings = %+v, want one line with no overlay", got)
	}
}

func TestRaisingADaemonWarningAgainRestatesItsLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	h.r.RaiseDaemonWarning("fault-1", "first")

	// Act
	h.r.RaiseDaemonWarning("fault-1", "second")

	// Assert
	if got := lines(t, h, testWS); len(got) != 1 || got[0] != "second" {
		t.Fatalf("warning lines = %q, want the one restated line", got)
	}
}

func TestARetractedDaemonWarningLeavesEveryStrip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	readyOther(t, h)
	h.r.RaiseDaemonWarning("fault-1", "deploy failed: build webapp: tsc")

	// Act
	h.r.RetractDaemonWarning("fault-1")

	// Assert
	for _, ws := range []ids.WorkspaceID{testWS, otherWS} {
		if got := lines(t, h, ws); slices.Contains(got, "deploy failed: build webapp: tsc") {
			t.Fatalf("%s warning lines = %q, want the daemon's line gone", ws, got)
		}
	}
}

func TestARetractedDaemonWarningIsNotDrawnOnALaterStrip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	h.r.RaiseDaemonWarning("fault-1", "deploy failed: build webapp: tsc")
	h.r.RetractDaemonWarning("fault-1")

	// Act
	readyOther(t, h)

	// Assert
	if got := lines(t, h, otherWS); slices.Contains(got, "deploy failed: build webapp: tsc") {
		t.Fatalf("warning lines = %q, want the retracted line absent", got)
	}
}

func TestADaemonWarningIsRecordedOnTheRunLog(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.RaiseDaemonWarning("fault-1", "deploy failed")

	// Assert
	for _, r := range h.log.Records() {
		if r.Operation == "daemon.topbar.raise_daemon_warning" && r.Level == "info" && r.Context["workspaces"] == 1 {
			return
		}
	}
	t.Fatalf("records = %+v, want the raise at INFO naming the one strip", h.log.Records())
}
