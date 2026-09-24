package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE SKILL CARD: exactly TWO frames — the invocation and the document. No
// temporal window folds subsequent work under it.

// skillCard finds the skill card on the root feed.
func (h *harness) skillCard() *frontendv1.FeedSkill {
	h.t.Helper()
	for _, row := range h.rows(rootFeed()) {
		if skill := row.GetActivity().GetSkill(); skill != nil {
			return skill
		}
	}
	h.t.Fatal("no skill card on the root feed")
	return nil
}

// skillFrame sends one skill frame.
func (h *harness) skillFrame(unit string, result any) {
	h.t.Helper()
	skill := &conversationv1.AgentSkillUse{}
	switch r := result.(type) {
	case *conversationv1.AgentSkillUseStart:
		skill.Result = &conversationv1.AgentSkillUse_Start{Start: r}
	case *conversationv1.AgentSkillUseSuccess:
		skill.Result = &conversationv1.AgentSkillUse_Success{Success: r}
	case *conversationv1.AgentSkillUseFailure:
		skill.Result = &conversationv1.AgentSkillUse_Failure{Failure: r}
	}
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item:       &conversationv1.AgentActivity_SkillUse{SkillUse: skill},
	})
}

func TestTheInvocationLineIsTheLineAUserWouldHaveTyped(t *testing.T) {
	tests := []struct {
		name    string
		skill   string
		args    string
		hasArgs bool
		want    string
	}{
		{name: "a bare invocation", skill: "graphify", want: "/graphify"},
		{
			name:  "an invocation with arguments",
			skill: "workspace", args: "merge DWC/fix-flaky", hasArgs: true,
			want: "/workspace merge DWC/fix-flaky",
		},
		{
			name:  "a name the vendor already spelled with a slash is not doubled",
			skill: "/graphify",
			want:  "/graphify",
		},
		{
			name: "an empty argument is not an argument", skill: "graphify", args: "", hasArgs: true,
			want: "/graphify",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			start := &conversationv1.AgentSkillUseStart{
				Skill:     &conversationv1.AgentSkillName{Name: tc.skill},
				StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
			}
			if tc.hasArgs {
				args := tc.args
				start.Args = &args
			}

			// Act.
			h.skillFrame("unit-1", start)

			// Assert.
			got := h.skillCard().GetInvocation().GetText()
			if got != tc.want {
				t.Fatalf("invocation = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestAnInvokedSkillIsRunningUntilItsDocumentLands(t *testing.T) {
	// Arrange, Act: the unit settles on the DOCUMENT, not on the tool's own
	// acknowledgement.
	h := newHarness(t)
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseStart{
		Skill:     &conversationv1.AgentSkillName{Name: "graphify"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert.
	if h.skillCard().GetRunning() == nil {
		t.Fatalf("outcome = %T, want running", h.skillCard().GetOutcome())
	}
}

func TestALoadedSkillCarriesItsDocumentAndItsAllowances(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseStart{
		Skill:     &conversationv1.AgentSkillName{Name: "graphify"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Act.
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseSuccess{
		Skill:    &conversationv1.AgentSkillName{Name: "graphify"},
		Document: &conversationv1.AgentSkillDocument{Markdown: "# graphify\n"},
		AllowedTools: &conversationv1.AgentSkillAllowedTools{
			ToolNames: []string{"Bash", "Write"},
		},
	})

	// Assert: the consent line states what invoking it PERMITS.
	loaded := h.skillCard().GetLoaded()
	if loaded.GetDocument().GetMarkdown() != "# graphify\n" {
		t.Fatalf("document = %q", loaded.GetDocument().GetMarkdown())
	}
	if loaded.GetAllowances().GetText() != "allows: Bash, Write" {
		t.Fatalf("allowances = %q", loaded.GetAllowances().GetText())
	}
}

func TestASkillThatDeclaredNoAllowancesDrawsNoLine(t *testing.T) {
	// Arrange, Act: declaring none is distinct from declaring an empty set, and
	// absence draws no line rather than an empty one.
	h := newHarness(t)
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseSuccess{
		Skill:    &conversationv1.AgentSkillName{Name: "graphify"},
		Document: &conversationv1.AgentSkillDocument{Markdown: "# graphify\n"},
	})

	// Assert.
	if h.skillCard().GetLoaded().GetAllowances() != nil {
		t.Fatalf("allowances = %+v, want unset", h.skillCard().GetLoaded().GetAllowances())
	}
}

func TestAnEmptyAllowanceSetAlsoDrawsNoLine(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseSuccess{
		Skill:        &conversationv1.AgentSkillName{Name: "graphify"},
		Document:     &conversationv1.AgentSkillDocument{Markdown: "# graphify\n"},
		AllowedTools: &conversationv1.AgentSkillAllowedTools{},
	})

	// Assert.
	if h.skillCard().GetLoaded().GetAllowances() != nil {
		t.Fatalf("allowances = %+v, want unset", h.skillCard().GetLoaded().GetAllowances())
	}
}

func TestExactlyTwoFramesMakeTheCardAndNothingFoldsUnderIt(t *testing.T) {
	// Arrange: an invocation and its document.
	h := newHarness(t)
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseStart{
		Skill:     &conversationv1.AgentSkillName{Name: "graphify"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseSuccess{
		Skill:    &conversationv1.AgentSkillName{Name: "graphify"},
		Document: &conversationv1.AgentSkillDocument{Markdown: "# graphify\n"},
	})

	// Act: ordinary work afterwards.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseSuccessActivity("unit-2", "working on it"), nil, noAddress())

	// Assert: the later row is its OWN top-level row — nothing delimits a
	// skill's scope at the source, so no window folds it under the card.
	rows := h.rows(rootFeed())
	if len(rows) != 2 {
		t.Fatalf("rows = %d, want the card and the response", len(rows))
	}
	for _, row := range rows {
		if row.GetParent() != nil {
			t.Fatalf("row %q was nested under the skill card", row.GetId().GetValue())
		}
	}
}

func TestAFailedSkillDrawsTheProducersAccount(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseFailure{
		// A settled frame restates what its call named (the contract).
		Skill: &conversationv1.AgentSkillName{Name: "absent-skill"},
		Error: &conversationv1.AgentToolFailure{
			Content: &conversationv1.ToolResultContent{Blocks: []*conversationv1.ToolResultContentBlock{{
				Block: &conversationv1.ToolResultContentBlock_Text{
					Text: &conversationv1.TextBlock{Text: "no such skill"},
				},
			}}},
		},
	})

	// Assert.
	if got := h.skillCard().GetFailed().GetText(); got != "no such skill" {
		t.Fatalf("reason = %q", got)
	}
}

func TestAFailedSkillWithNoAccountStillStatesSomething(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseFailure{
		Skill: &conversationv1.AgentSkillName{Name: "absent-skill"},
	})

	// Assert: never an empty card.
	if got := h.skillCard().GetFailed().GetText(); got == "" {
		t.Fatal("a failed skill drew an empty reason")
	}
}

func TestASkillTheGateRefusedSaysNothingWasLoaded(t *testing.T) {
	// Arrange: the gate refuses the skill's call before it is announced.
	h := newHarness(t)
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to load a skill"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseStart{
		Skill:     &conversationv1.AgentSkillName{Name: "graphify"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Act.
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionSuccess{
		Decision: &conversationv1.AgentPermissionSuccess_Denied{Denied: &conversationv1.AgentPermissionDenied{
			By: &conversationv1.AgentPermissionDenied_Policy{
				Policy: &conversationv1.AgentPermissionDeniedByPolicy{Message: "no"},
			},
		}},
	})

	// Assert: the badge says it; consent is the permission card's story.
	if h.skillCard().GetDenied() == nil {
		t.Fatalf("outcome = %T, want denied", h.skillCard().GetOutcome())
	}
}

func TestAStartAfterTheDocumentLandedKeepsTheCardLoaded(t *testing.T) {
	// Arrange: the stream plane's success lands before the file plane's start
	// for the same unit.
	h := newHarness(t)
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseSuccess{
		Skill:    &conversationv1.AgentSkillName{Name: "graphify"},
		Document: &conversationv1.AgentSkillDocument{Markdown: "# graphify\n"},
	})

	// Act.
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseStart{
		Skill:     &conversationv1.AgentSkillName{Name: "graphify"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert.
	if got := h.skillCard().GetLoaded().GetDocument().GetMarkdown(); got != "# graphify\n" {
		t.Fatalf("outcome = %T (document %q), want the loaded card to stand", h.skillCard().GetOutcome(), got)
	}
}

func TestAStartAfterTheDocumentLandedStillStatesItsInvocation(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseSuccess{
		Skill:    &conversationv1.AgentSkillName{Name: "graphify"},
		Document: &conversationv1.AgentSkillDocument{Markdown: "# graphify\n"},
	})
	args := "--deep"

	// Act.
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseStart{
		Skill:     &conversationv1.AgentSkillName{Name: "graphify"},
		Args:      &args,
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert.
	if got := h.skillCard().GetInvocation().GetText(); got != "/graphify --deep" {
		t.Fatalf("invocation = %q, want the start's full line", got)
	}
}

func TestAStartAfterAFailureKeepsTheCardFailed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseFailure{Skill: &conversationv1.AgentSkillName{Name: "graphify"}})

	// Act.
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseStart{
		Skill:     &conversationv1.AgentSkillName{Name: "graphify"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert.
	if h.skillCard().GetFailed() == nil {
		t.Fatalf("outcome = %T, want the failed card to stand", h.skillCard().GetOutcome())
	}
}

// A REPLAYED SKILL FAILURE STANDS ALONE: the store keeps the settle alone once
// it has upserted over the start, so the failure names the skill itself.
func TestAReplayedSkillFailureDrawsTheSkillItRestated(t *testing.T) {
	// Arrange, Act: the settle alone, as a replay serves it.
	h := newHarness(t)
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseFailure{
		Skill: &conversationv1.AgentSkillName{Name: "absent-skill"},
	})

	// Assert.
	if got := h.skillCard().GetInvocation().GetText(); got != "/absent-skill" {
		t.Fatalf("invocation = %q, want the restated skill", got)
	}
}

func TestAReplayedSkillFailureRestatingNothingDrawsNoCard(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseFailure{})

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want 0: an unrestated failure must not draw an empty card", len(rows))
	}
}

func TestASkillFailureKeepsTheHeldStartsArguments(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	args := "--deep"
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseStart{
		Skill: &conversationv1.AgentSkillName{Name: "graphify"},
		Args:  &args,
	})

	// Act.
	h.skillFrame("unit-1", &conversationv1.AgentSkillUseFailure{
		Skill: &conversationv1.AgentSkillName{Name: "graphify"},
	})

	// Assert: the start carried the arguments, which no settle restates.
	if got := h.skillCard().GetInvocation().GetText(); got != "/graphify --deep" {
		t.Fatalf("invocation = %q, want the held start's full line", got)
	}
}
