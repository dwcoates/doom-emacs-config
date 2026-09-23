package convert

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// TestSkillAllowancesRideTheAcknowledgement covers where the declared allowances
// actually live: the skill's ACKNOWLEDGEMENT (`toolUseResult.allowedTools`) and
// nowhere else. The call's input names only the skill and its args, and the
// document record that settles the unit states no allowances at all — so the set
// has to be retained across the acknowledgement onto the settling frame.
func TestSkillAllowancesRideTheAcknowledgement(t *testing.T) {
	tests := []struct {
		name          string
		toolUseResult string
		wantDeclared  bool
		wantNames     []string
	}{
		{
			name:          "declared on the acknowledgement",
			toolUseResult: `{"commandName":"graphify","success":true,"allowedTools":["Bash(run.sh:*)","Read(/tmp/x.json)"]}`,
			wantDeclared:  true,
			wantNames:     []string{"Bash(run.sh:*)", "Read(/tmp/x.json)"},
		},
		{
			name:          "an empty declared set is not the same as none",
			toolUseResult: `{"commandName":"graphify","success":true,"allowedTools":[]}`,
			wantDeclared:  true,
			wantNames:     nil,
		},
		{
			name:          "no allowances declared at all stays unset",
			toolUseResult: `{"commandName":"graphify","success":true}`,
			wantDeclared:  false,
			wantNames:     nil,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := newTestConverter(t)
			call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_skill", "Skill", `{"skill":"graphify"}`))
			ack := toolResultLine("u1", "toolu_skill", ts2, `[{"type":"text","text":"graphify"}]`, tc.toolUseResult)
			doc := `{"type":"user","uuid":"u2","isSidechain":false,"isMeta":true,"sourceToolUseID":"toolu_skill",` +
				`"timestamp":"` + ts2 + `","message":{"role":"user","content":[{"type":"text","text":"# body"}]}}`

			// Act.
			entries := convertLines(t, c, call, ack, doc)

			// Assert.
			var settled *conversationv1.AgentSkillUseSuccess
			for _, e := range entries {
				if s := activityOf(e).GetSkillUse().GetSuccess(); s != nil {
					settled = s
				}
			}
			if settled == nil {
				t.Fatalf("no settled skill unit was produced")
			}
			if declared := settled.AllowedTools != nil; declared != tc.wantDeclared {
				t.Fatalf("allowed_tools declared = %v, want %v", declared, tc.wantDeclared)
			}
			got := settled.GetAllowedTools().GetToolNames()
			if len(got) != len(tc.wantNames) {
				t.Fatalf("tool_names = %v, want %v", got, tc.wantNames)
			}
			for i, want := range tc.wantNames {
				if got[i] != want {
					t.Fatalf("tool_names[%d] = %q, want %q", i, got[i], want)
				}
			}
		})
	}
}

// TestASkillThatDidNotResolveSettlesAsAFailure covers the arm a vendor-stated
// error lands on. No document ever arrives for a skill that did not resolve, so
// the error result IS the unit's terminal — and a plane that emitted only the
// START would leave the unit drawn as running forever, overwriting the other
// plane's failed card on every replay.
func TestASkillThatDidNotResolveSettlesAsAFailure(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_skill", "Skill", `{"skill":"absent-skill"}`))
	failed := toolResultLineWithError("u1", "toolu_skill", ts2,
		`[{"type":"text","text":"Error: no such skill: absent-skill"}]`,
		`{"commandName":"absent-skill","success":false}`, true)

	// Act.
	entries := convertLines(t, c, call, failed)

	// Assert.
	var settled *conversationv1.AgentSkillUseFailure
	for _, e := range entries {
		if f := activityOf(e).GetSkillUse().GetFailure(); f != nil {
			settled = f
		}
	}
	if settled == nil {
		t.Fatalf("no failed skill unit was produced")
	}
}

// TestAFailedSkillCarriesTheProducersAccount covers what the failure arm says:
// the producer's own error text, so the card can draw the reason verbatim
// instead of a fabricated one.
func TestAFailedSkillCarriesTheProducersAccount(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_skill", "Skill", `{"skill":"absent-skill"}`))
	failed := toolResultLineWithError("u1", "toolu_skill", ts2,
		`[{"type":"text","text":"Error: no such skill: absent-skill"}]`,
		`{"commandName":"absent-skill","success":false}`, true)

	// Act.
	entries := convertLines(t, c, call, failed)

	// Assert.
	var text string
	for _, e := range entries {
		if f := activityOf(e).GetSkillUse().GetFailure(); f != nil {
			for _, b := range f.GetError().GetContent().GetBlocks() {
				text += b.GetText().GetText()
			}
		}
	}
	if want := "Error: no such skill: absent-skill"; text != want {
		t.Fatalf("failure content = %q, want %q", text, want)
	}
}

// TestAFailedSkillRestatesTheSkillItInvoked covers the settled frame standing
// alone: the start it upserts over is gone once it lands, so the failure names
// the skill itself, exactly as the success does.
func TestAFailedSkillRestatesTheSkillItInvoked(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_skill", "Skill", `{"skill":"absent-skill"}`))
	failed := toolResultLineWithError("u1", "toolu_skill", ts2,
		`[{"type":"text","text":"Error: no such skill: absent-skill"}]`,
		`{"commandName":"absent-skill","success":false}`, true)

	// Act.
	entries := convertLines(t, c, call, failed)

	// Assert.
	var name string
	for _, e := range entries {
		if f := activityOf(e).GetSkillUse().GetFailure(); f != nil {
			name = f.GetSkill().GetName()
		}
	}
	if name != "absent-skill" {
		t.Fatalf("restated skill = %q, want %q", name, "absent-skill")
	}
}
