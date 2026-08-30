package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE CONSENT CARD. A DENIAL IS AN ANSWER, and the standing's content never
// reaches a client.

// ask sends one permission frame.
func (h *harness) ask(askID, gated string, result any) {
	h.t.Helper()
	p := &conversationv1.AgentPermission{
		Id:        &conversationv1.AgentPermissionId{Value: askID},
		GatedCall: &conversationv1.AgentActivityId{Value: gated},
	}
	switch r := result.(type) {
	case *conversationv1.AgentPermissionStart:
		p.Result = &conversationv1.AgentPermission_Start{Start: r}
	case *conversationv1.AgentPermissionSuccess:
		p.Result = &conversationv1.AgentPermission_Success{Success: r}
	case *conversationv1.AgentPermissionFailure:
		p.Result = &conversationv1.AgentPermission_Failure{Failure: r}
	}
	h.resolver.OnPermission(testWorkspace, mainAgent(), p, noAddress())
}

// permissionCard finds the consent card on the root feed.
func (h *harness) permissionCard() *frontendv1.FeedPermission {
	h.t.Helper()
	for _, row := range h.rows(rootFeed()) {
		if card := row.GetPermission(); card != nil {
			return card
		}
	}
	h.t.Fatal("no permission card on the root feed")
	return nil
}

func TestTheConsentCardDrawsTheVendorsResolvedSentence(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	subtitle := "Claude will have read access to files in ~/Downloads"
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt: &conversationv1.AgentPermissionPrompt{
			Title:       "Claude wants to read foo.txt",
			DisplayName: "Read file",
			Description: &subtitle,
		},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert: resolved end to end — no consumer composes it from the tool.
	card := h.permissionCard()
	if card.GetHeadline().GetText() != "Claude wants to read foo.txt" {
		t.Fatalf("headline = %q", card.GetHeadline().GetText())
	}
	if card.GetSubtitle().GetText() != subtitle {
		t.Fatalf("subtitle = %q", card.GetSubtitle().GetText())
	}
	if card.GetOpen() == nil {
		t.Fatalf("state = %T, want open", card.GetState())
	}
}

func TestASubtitleTheVendorDidNotGiveIsUnset(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to read foo.txt"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert.
	if h.permissionCard().GetSubtitle() != nil {
		t.Fatalf("subtitle = %+v, want unset", h.permissionCard().GetSubtitle())
	}
}

func TestTheTriggerNoteCarriesEveryFactTheVendorReported(t *testing.T) {
	// Arrange: three INDEPENDENT facts on one ask; dropping any hides part of
	// the reason.
	h := newHarness(t)
	rule := "Bash(git push:*)"
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt: &conversationv1.AgentPermissionPrompt{Title: "Claude wants to run a command"},
		Trigger: &conversationv1.AgentPermissionTrigger{
			BlockedPath: &conversationv1.AgentPermissionBlockedPath{Path: "/etc/hosts"},
			AskRule: &conversationv1.AgentPermissionAskRule{
				Source: "project settings", ToolName: "Bash", RuleContent: &rule,
			},
			Note: &conversationv1.AgentPermissionTriggerNote{Text: "the vendor's own sentence"},
		},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert: all three, and the ask rule worded as USER-CONFIGURED so a host
	// cannot read it as auto-approvable.
	note := h.permissionCard().GetTrigger().GetText()
	if !contains(note, "/etc/hosts") {
		t.Fatalf("trigger = %q, want the blocked path", note)
	}
	if !contains(note, "you configured a rule that always asks for Bash") {
		t.Fatalf("trigger = %q, want the ask rule worded as the user's own", note)
	}
	if !contains(note, "the vendor's own sentence") {
		t.Fatalf("trigger = %q, want the unclassified note", note)
	}
}

func TestATriggerTheVendorDidNotExplainIsUnset(t *testing.T) {
	// Arrange, Act: simply the default for this tool.
	h := newHarness(t)
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to read foo.txt"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert.
	if h.permissionCard().GetTrigger() != nil {
		t.Fatalf("trigger = %+v, want unset", h.permissionCard().GetTrigger())
	}
}

func TestTheArgumentPreviewReusesTheGatedCallsOwnComposedLine(t *testing.T) {
	// Arrange: the gated call already drew its card.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
			Command:   &conversationv1.AgentBashCommand{Line: "git push origin main"},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))

	// Act.
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to run a command"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert: ONE composition, drawn in two places — never a second phrasing
	// that could disagree with the card.
	lines := h.permissionCard().GetArguments().GetLines()
	if len(lines) != 1 || lines[0] != "git push origin main" {
		t.Fatalf("arguments = %v, want the gated call's own line", lines)
	}
}

func TestAnAskWithNoDrawnGatedCallHasNoArgumentLines(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.ask("ask-1", "unit-unknown", &conversationv1.AgentPermissionStart{
		Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants something"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert.
	if got := h.permissionCard().GetArguments().GetLines(); len(got) != 0 {
		t.Fatalf("arguments = %v, want none", got)
	}
}

func TestAnAskWithNoStandingOfferedDrawsNoAlwaysAllow(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to read foo.txt"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert: presence is what makes the button drawable, so absence must be
	// absence.
	if h.permissionCard().GetStandingOffered() != nil {
		t.Fatal("standing_offered is set on an ask the vendor offered no standing for")
	}
}

func TestTheDecisionUpsertsTheSameRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to read foo.txt"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Act.
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionSuccess{
		Decision: &conversationv1.AgentPermissionSuccess_Allowed{
			Allowed: &conversationv1.AgentPermissionAllowed{
				Scope: &conversationv1.AgentPermissionAllowed_Once{
					Once: &conversationv1.AgentPermissionAllowedOnce{},
				},
			},
		},
	})

	// Assert: one row, now answered, and the headline is still the ask's.
	if rows := h.rows(rootFeed()); len(rows) != 1 {
		t.Fatalf("rows = %d, want the one card upserted", len(rows))
	}
	card := h.permissionCard()
	if card.GetAnswered().GetAllowedOnce() == nil {
		t.Fatalf("answer = %T, want allowed_once", card.GetAnswered().GetAnswer())
	}
	if card.GetHeadline().GetText() != "Claude wants to read foo.txt" {
		t.Fatal("the decision lost the ask's headline")
	}
}

func TestEveryVerdictHasItsOwnTreatment(t *testing.T) {
	tests := []struct {
		name     string
		decision any
		want     string
	}{
		{
			name: "allowed once",
			decision: &conversationv1.AgentPermissionAllowed{
				Scope: &conversationv1.AgentPermissionAllowed_Once{Once: &conversationv1.AgentPermissionAllowedOnce{}},
			},
			want: "allowed_once",
		},
		{
			name: "allowed with standing",
			decision: &conversationv1.AgentPermissionAllowed{
				Scope: &conversationv1.AgentPermissionAllowed_Standing{
					Standing: &conversationv1.AgentPermissionAllowedStanding{
						Standing: &conversationv1.AgentPermissionStanding{},
					},
				},
			},
			want: "allowed_standing",
		},
		{
			name: "the user said no",
			decision: &conversationv1.AgentPermissionDenied{
				By: &conversationv1.AgentPermissionDenied_User{
					User: &conversationv1.AgentPermissionDeniedByUser{Message: "not that one"},
				},
			},
			want: "denied_by_user",
		},
		{
			name: "policy said no with nobody asked",
			decision: &conversationv1.AgentPermissionDenied{
				By: &conversationv1.AgentPermissionDenied_Policy{
					Policy: &conversationv1.AgentPermissionDeniedByPolicy{Message: "no"},
				},
			},
			want: "denied_by_policy",
		},
		{
			name: "nobody refused: the decider was unreachable",
			decision: &conversationv1.AgentPermissionDenied{
				By: &conversationv1.AgentPermissionDenied_Undecidable{
					Undecidable: &conversationv1.AgentPermissionDeniedForWantOfDecider{},
				},
			},
			want: "denied_by_policy",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
				Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to read foo.txt"},
				StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
			})
			success := &conversationv1.AgentPermissionSuccess{}
			switch d := tc.decision.(type) {
			case *conversationv1.AgentPermissionAllowed:
				success.Decision = &conversationv1.AgentPermissionSuccess_Allowed{Allowed: d}
			case *conversationv1.AgentPermissionDenied:
				success.Decision = &conversationv1.AgentPermissionSuccess_Denied{Denied: d}
			}

			// Act.
			h.ask("ask-1", "unit-1", success)

			// Assert.
			got := verdictWord(h.permissionCard().GetAnswered())
			if got != tc.want {
				t.Fatalf("verdict = %q, want %q", got, tc.want)
			}
		})
	}
}

// verdictWord names an answered card's verdict arm.
func verdictWord(answered *frontendv1.FeedPermissionAnswered) string {
	switch answered.GetAnswer().(type) {
	case *frontendv1.FeedPermissionAnswered_AllowedOnce:
		return "allowed_once"
	case *frontendv1.FeedPermissionAnswered_AllowedStanding:
		return "allowed_standing"
	case *frontendv1.FeedPermissionAnswered_DeniedByUser:
		return "denied_by_user"
	case *frontendv1.FeedPermissionAnswered_DeniedByPolicy:
		return "denied_by_policy"
	}
	return "unset"
}

func TestAnUndecidableDenialNeverReadsAsTheUsersActOrAsARule(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to read foo.txt"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	detail := "the classifier's model was unavailable"

	// Act.
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionSuccess{
		Decision: &conversationv1.AgentPermissionSuccess_Denied{Denied: &conversationv1.AgentPermissionDenied{
			By: &conversationv1.AgentPermissionDenied_Undecidable{
				Undecidable: &conversationv1.AgentPermissionDeniedForWantOfDecider{Detail: &detail},
			},
		}},
	})

	// Assert: it is the only denial retrying may resolve, and it says so.
	text := h.permissionCard().GetAnswered().GetDeniedByPolicy().GetText()
	if !contains(text, "denied for want of a decider") || !contains(text, detail) {
		t.Fatalf("text = %q, want the want-of-a-decider wording", text)
	}
}

func TestAPolicyDenialCarriesTheDecidersOwnReason(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to run a command"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	reason := "a deny rule matched"

	// Act.
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionSuccess{
		Decision: &conversationv1.AgentPermissionSuccess_Denied{Denied: &conversationv1.AgentPermissionDenied{
			By: &conversationv1.AgentPermissionDenied_Policy{
				Policy: &conversationv1.AgentPermissionDeniedByPolicy{Reason: &reason, Message: "no"},
			},
		}},
	})

	// Assert.
	if got := h.permissionCard().GetAnswered().GetDeniedByPolicy().GetText(); got != reason {
		t.Fatalf("text = %q, want the decider's reason", got)
	}
}

func TestADeniedGateMarksTheGatedCallAsNeverHavingRun(t *testing.T) {
	// Arrange: a gated call drawn as running.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
			Command:   &conversationv1.AgentBashCommand{Line: "rm -rf /"},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to run a command"},
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

	// Assert: the card says denied rather than sitting running forever.
	var denied bool
	for _, row := range h.rows(rootFeed()) {
		if card := row.GetActivity().GetSimpleToolCall(); card != nil && card.GetDenied() != nil {
			denied = true
		}
	}
	if !denied {
		t.Fatal("the gated call's card is still running after the gate refused it")
	}
	if !h.hasRecord("debug", "daemon.feed.tool_call_denied") {
		t.Fatalf("records = %+v, want the denial recorded", h.records())
	}
}

func TestAnAskThatCouldNotBePutToTheUserIsAbandoned(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to read foo.txt"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Act.
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionFailure{})

	// Assert.
	if h.permissionCard().GetAbandoned() == nil {
		t.Fatalf("state = %T, want abandoned", h.permissionCard().GetState())
	}
}

func TestAPermissionFrameWithNoAskIdentityIsRefusedLoudly(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.OnPermission(testWorkspace, mainAgent(), &conversationv1.AgentPermission{
		Result: &conversationv1.AgentPermission_Start{Start: &conversationv1.AgentPermissionStart{}},
	}, noAddress())

	// Assert.
	if !h.hasRecord("error", "daemon.feed.permission_without_identity") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.permission_without_identity", h.records())
	}
}
