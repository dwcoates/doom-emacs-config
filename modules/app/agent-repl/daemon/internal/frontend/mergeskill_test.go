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

func TestMergeSkillCallInItemFindsTheCallAmongAnItemsCalls(t *testing.T) {
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
	toolUseID, label, ok := MergeSkillCallInItem(item)

	// Assert
	if !ok {
		t.Fatal("the item made the merge invocation and the classifier missed it")
	}
	if toolUseID != "toolu_merge" {
		t.Errorf("tool_use_id = %q, want the merge call's own id", toolUseID)
	}
	if label != "/create-or-update-workspace merge now" {
		t.Errorf("label = %q, want the invocation verbatim", label)
	}
}

func TestMergeSkillCallInItemReportsNothingForAnOrdinaryItem(t *testing.T) {
	// Arrange
	item := &frontendv1.ConversationItem{Item: &frontendv1.ConversationItem_UserMessage{
		UserMessage: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentString{ContentString: "merge the workspace please"}},
	}}

	// Act
	_, _, ok := MergeSkillCallInItem(item)

	// Assert
	if ok {
		t.Fatal("a user asking in prose for a merge is not a Skill invocation and must open no bubble")
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
