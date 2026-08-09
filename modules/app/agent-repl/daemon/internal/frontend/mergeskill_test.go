package frontend

import (
	"testing"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/types/known/structpb"
)

// skillCall builds one Skill tool_use block exactly as the transcript writes
// it: {"skill": "<name>", "args": "<verb and flags>"}.
func skillCall(t *testing.T, toolName, skill, args string) *datav1.ToolUseBlock {
	t.Helper()
	input, err := structpb.NewStruct(map[string]any{"skill": skill, "args": args})
	if err != nil {
		t.Fatalf("structpb.NewStruct: %v", err)
	}
	return &datav1.ToolUseBlock{Id: "toolu_merge", Name: toolName, Input: input}
}

func TestMergeSkillCallClassifiesTheInvocation(t *testing.T) {
	tests := []struct {
		name      string
		toolName  string
		skill     string
		args      string
		wantOK    bool
		wantLabel string
	}{
		{
			name:      "the merge verb of the workspace skill",
			toolName:  "Skill",
			skill:     "create-or-update-workspace",
			args:      "merge",
			wantOK:    true,
			wantLabel: "/create-or-update-workspace merge",
		},
		{
			name:      "the merge verb with its own arguments",
			toolName:  "Skill",
			skill:     "create-or-update-workspace",
			args:      "merge feat/thing --keep",
			wantOK:    true,
			wantLabel: "/create-or-update-workspace merge feat/thing --keep",
		},
		{
			name:     "another verb of the same skill",
			toolName: "Skill",
			skill:    "create-or-update-workspace",
			args:     "create feat/thing",
		},
		{
			name:     "a verb the merge verb is a prefix of",
			toolName: "Skill",
			skill:    "create-or-update-workspace",
			args:     "merge-status",
		},
		{
			name:     "the merge verb of another skill",
			toolName: "Skill",
			skill:    "create-or-update-pr",
			args:     "merge",
		},
		{
			name:     "a plugin-qualified skill of the same trailing name",
			toolName: "Skill",
			skill:    "vendor:create-or-update-workspace",
			args:     "merge",
		},
		{
			name:     "the merge verb named by a tool that is not Skill",
			toolName: "Task",
			skill:    "create-or-update-workspace",
			args:     "merge",
		},
		{
			name:     "the skill invoked with no verb at all",
			toolName: "Skill",
			skill:    "create-or-update-workspace",
			args:     "",
		},
		{
			name:     "the verb mentioned past the head of the arguments",
			toolName: "Skill",
			skill:    "create-or-update-workspace",
			args:     "status merge",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			use := skillCall(t, tc.toolName, tc.skill, tc.args)

			// Act
			label, ok := MergeSkillCall(use)

			// Assert
			if ok != tc.wantOK {
				t.Fatalf("MergeSkillCall(%s skill=%q args=%q) ok = %v, want %v: the rule requires BOTH the skill and the bare merge verb",
					tc.toolName, tc.skill, tc.args, ok, tc.wantOK)
			}
			if label != tc.wantLabel {
				t.Errorf("label = %q, want %q", label, tc.wantLabel)
			}
		})
	}
}

func TestMergeSkillCallIgnoresACallWithNoInput(t *testing.T) {
	// Arrange
	use := &datav1.ToolUseBlock{Id: "toolu_bare", Name: "Skill"}

	// Act
	_, ok := MergeSkillCall(use)

	// Assert
	if ok {
		t.Fatal("a Skill call carrying no input names no skill, so it can never be the merge invocation")
	}
}

// AMENDED: MergeSkillCallInItem is gone, because a per-item pass that answered
// only "is this the merge call" would have to be run beside a second pass
// answering "is this some other skill", and the two could disagree about one
// call. SkillToolCallsInItem + SkillCall is the ONE pass, and each invocation
// carries its own merge verdict.

// classifyItem is the pass the consumer makes: every Skill call in the item,
// each one classified once.
func classifyItem(item *frontendv1.ConversationItem) []SkillInvocation {
	var out []SkillInvocation
	for _, use := range SkillToolCallsInItem(item) {
		if inv, ok := SkillCall(use); ok {
			out = append(out, inv)
		}
	}
	return out
}

func TestClassifyingAnItemFindsTheMergeInvocationAmongAnItemsCalls(t *testing.T) {
	// Arrange
	item := &frontendv1.ConversationItem{Item: &frontendv1.ConversationItem_Agent{
		Agent: &frontendv1.AgentEmission{Emission: &frontendv1.AgentEmission_Response{
			Response: &frontendv1.AgentResponse{Body: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_ToolUse{ToolUse: skillCall(t, "Skill", "create-or-update-pr", "merge")}},
				{Block: &datav1.ContentBlock_ToolUse{ToolUse: skillCall(t, "Skill", "create-or-update-workspace", "merge now")}},
			}}},
		}},
	}}

	// Act
	got := classifyItem(item)

	// Assert
	var merge *SkillInvocation
	for i := range got {
		if got[i].IsMerge {
			merge = &got[i]
		}
	}
	if merge == nil {
		t.Fatalf("the item made the merge invocation and the classifier reported %d invocation(s), none of them the merge", len(got))
	}
	if merge.ToolUseID != "toolu_merge" {
		t.Errorf("tool_use_id = %q, want the merge call's own id", merge.ToolUseID)
	}
	if merge.Label != "/create-or-update-workspace merge now" {
		t.Errorf("label = %q, want the invocation verbatim", merge.Label)
	}
}

func TestClassifyingAnItemReportsEveryInvocationInOrder(t *testing.T) {
	// Arrange: a near-miss skill and the merge, both real invocations.
	item := &frontendv1.ConversationItem{Item: &frontendv1.ConversationItem_Agent{
		Agent: &frontendv1.AgentEmission{Emission: &frontendv1.AgentEmission_Response{
			Response: &frontendv1.AgentResponse{Body: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_ToolUse{ToolUse: skillCall(t, "Skill", "create-or-update-pr", "merge")}},
				{Block: &datav1.ContentBlock_ToolUse{ToolUse: skillCall(t, "Skill", "create-or-update-workspace", "merge now")}},
			}}},
		}},
	}}

	// Act
	got := classifyItem(item)

	// Assert: every Skill call is an invocation of something; only one of them
	// is the merge.
	if len(got) != 2 {
		t.Fatalf("classified %d invocations, want both of the item's Skill calls", len(got))
	}
	if got[0].SkillName != "create-or-update-pr" {
		t.Errorf("the first invocation is %q, want the item's first Skill call", got[0].SkillName)
	}
}

func TestClassifyingAnItemReportsNothingForAnOrdinaryItem(t *testing.T) {
	// Arrange
	item := &frontendv1.ConversationItem{Item: &frontendv1.ConversationItem_UserMessage{
		UserMessage: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentString{ContentString: "merge the workspace please"}},
	}}

	// Act
	got := classifyItem(item)

	// Assert
	if len(got) != 0 {
		t.Fatal("a user asking in prose for a merge is not a Skill invocation and must open no bubble")
	}
}

// --- the skill reading ------------------------------------------------------

func TestSkillCallClassifiesANonMergeInvocationAsASkill(t *testing.T) {
	// Arrange
	use := skillCall(t, "Skill", "demo", "run it")

	// Act
	inv, ok := SkillCall(use)

	// Assert
	if !ok {
		t.Fatal("every Skill call that names a skill is an invocation")
	}
	if inv.IsMerge {
		t.Fatal("a skill that is not /create-or-update-workspace merge is never the merge run")
	}
}

func TestSkillCallCarriesTheSkillNameVerbatim(t *testing.T) {
	// Arrange, Act
	inv, _ := SkillCall(skillCall(t, "Skill", "demo", "run it"))

	// Assert
	if inv.SkillName != "demo" {
		t.Fatalf("skill name = %q, want the input's own value", inv.SkillName)
	}
}

func TestSkillCallCarriesTheArgsVerbatim(t *testing.T) {
	// Arrange: the args carry padding a label would tidy away.
	inv, _ := SkillCall(skillCall(t, "Skill", "demo", "  run it  "))

	// Assert
	if inv.Args != "  run it  " {
		t.Fatalf("args = %q, want the input's own value untouched: the contract says verbatim", inv.Args)
	}
}

func TestSkillCallLabelsAnInvocationAsTheAgentWroteIt(t *testing.T) {
	// Arrange, Act
	inv, _ := SkillCall(skillCall(t, "Skill", "demo", "run it"))

	// Assert
	if inv.Label != "/demo run it" {
		t.Fatalf("label = %q, want the invocation as written", inv.Label)
	}
}

func TestSkillCallLabelsAnArglessInvocationWithoutATrailingSpace(t *testing.T) {
	// Arrange, Act
	inv, _ := SkillCall(skillCall(t, "Skill", "demo", ""))

	// Assert
	if inv.Label != "/demo" {
		t.Fatalf("label = %q, want %q: a skill invoked with no arguments has no arguments to render", inv.Label, "/demo")
	}
}

func TestSkillCallIgnoresANonSkillTool(t *testing.T) {
	// Arrange, Act
	_, ok := SkillCall(skillCall(t, "Task", "demo", "run it"))

	// Assert
	if ok {
		t.Fatal("a tool that is not Skill invokes no skill, whatever its input happens to carry")
	}
}

func TestSkillCallRefusesACallThatNamesNoSkill(t *testing.T) {
	// Arrange
	use := &datav1.ToolUseBlock{Id: "toolu_bare", Name: "Skill"}

	// Act
	_, ok := SkillCall(use)

	// Assert
	if ok {
		t.Fatal("a Skill call naming no skill has nothing to label a bubble with and must not be classified as an invocation")
	}
}

func TestItemBelongsToCallMatchesTheCallsOwnResult(t *testing.T) {
	tests := []struct {
		name string
		item *frontendv1.ConversationItem
		want bool
	}{
		{
			name: "the user record handing the result back",
			item: &frontendv1.ConversationItem{Item: &frontendv1.ConversationItem_UserMessage{
				UserMessage: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentBlocks{
					ContentBlocks: &datav1.ApiContentBlocks{Blocks: []*datav1.ContentBlock{
						{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{ToolUseId: "toolu_merge"}}},
					}},
				}},
			}},
			want: true,
		},
		{
			name: "the skill body addressed to the card",
			item: &frontendv1.ConversationItem{Item: &frontendv1.ConversationItem_Agent{
				Agent: &frontendv1.AgentEmission{Emission: &frontendv1.AgentEmission_SkillBody{
					SkillBody: &frontendv1.SkillBodyItem{ToolUseId: "toolu_merge"},
				}},
			}},
			want: true,
		},
		{
			name: "a result belonging to a different call",
			item: &frontendv1.ConversationItem{Item: &frontendv1.ConversationItem_UserMessage{
				UserMessage: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentBlocks{
					ContentBlocks: &datav1.ApiContentBlocks{Blocks: []*datav1.ContentBlock{
						{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{ToolUseId: "toolu_other"}}},
					}},
				}},
			}},
			want: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act
			got := ItemBelongsToCall(tc.item, "toolu_merge")

			// Assert
			if got != tc.want {
				t.Fatalf("ItemBelongsToCall = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestItemBelongsToCallMatchesNothingWithoutACall(t *testing.T) {
	// Arrange
	item := &frontendv1.ConversationItem{Item: &frontendv1.ConversationItem_UserMessage{
		UserMessage: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentBlocks{
			ContentBlocks: &datav1.ApiContentBlocks{Blocks: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{}}},
			}},
		}},
	}}

	// Act, Assert
	if ItemBelongsToCall(item, "") {
		t.Fatal("with no origin call named, no item is that call's result")
	}
}
