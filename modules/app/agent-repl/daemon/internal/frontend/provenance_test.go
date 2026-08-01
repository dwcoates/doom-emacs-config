package frontend

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

func TestStampConversationSource(t *testing.T) {
	// Arrange.
	tests := []struct {
		name  string
		delta *frontendv1.ConversationDelta
		src   frontendv1.ConversationSource
		want  frontendv1.ConversationSource
	}{
		{
			name: "merge revises the curator's user verdict",
			delta: &frontendv1.ConversationDelta{Workspace: "ws", Items: []*frontendv1.ConversationItem{
				{Uuid: "a", Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER},
				{Uuid: "b", Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER},
			}},
			src:  frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE,
			want: frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE,
		},
		{
			name: "user is stamped on an unstamped item",
			delta: &frontendv1.ConversationDelta{Workspace: "ws", Items: []*frontendv1.ConversationItem{
				{Uuid: "a"},
			}},
			src:  frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
			want: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		},
		{
			name:  "an item-less delta is stamped without complaint",
			delta: &frontendv1.ConversationDelta{Workspace: "ws"},
			src:   frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE,
			want:  frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			err := StampConversationSource(tt.delta, tt.src)

			// Assert.
			if err != nil {
				t.Fatalf("StampConversationSource: %v", err)
			}
			for _, item := range tt.delta.GetItems() {
				if item.GetSource() != tt.want {
					t.Fatalf("item %q source = %v, want %v", item.GetUuid(), item.GetSource(), tt.want)
				}
			}
		})
	}
}

func TestStampConversationSourceRefusals(t *testing.T) {
	// Arrange.
	tests := []struct {
		name  string
		delta *frontendv1.ConversationDelta
		src   frontendv1.ConversationSource
		want  string
	}{
		{
			name:  "nil delta",
			delta: nil,
			src:   frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE,
			want:  "nil delta",
		},
		{
			name:  "unspecified source",
			delta: &frontendv1.ConversationDelta{Workspace: "ws", Items: []*frontendv1.ConversationItem{{Uuid: "a"}}},
			src:   frontendv1.ConversationSource_CONVERSATION_SOURCE_UNSPECIFIED,
			want:  "UNSPECIFIED",
		},
		{
			name:  "nil item inside the delta",
			delta: &frontendv1.ConversationDelta{Workspace: "ws", Items: []*frontendv1.ConversationItem{nil}},
			src:   frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE,
			want:  "nil item",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			err := StampConversationSource(tt.delta, tt.src)

			// Assert.
			if err == nil {
				t.Fatal("StampConversationSource = nil, want an error")
			}
			if !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("error = %q, want it to mention %q", err, tt.want)
			}
		})
	}
}

func TestStampItemConversationSource(t *testing.T) {
	// Arrange.
	item := &frontendv1.ConversationItem{Uuid: "a"}

	// Act.
	err := StampItemConversationSource(item, frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE)

	// Assert.
	if err != nil {
		t.Fatalf("StampItemConversationSource: %v", err)
	}
	if item.GetSource() != frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE {
		t.Fatalf("source = %v, want MERGE", item.GetSource())
	}
}

func TestStampItemConversationSourceRefusals(t *testing.T) {
	// Arrange.
	tests := []struct {
		name string
		item *frontendv1.ConversationItem
		src  frontendv1.ConversationSource
		want string
	}{
		{
			name: "nil item",
			item: nil,
			src:  frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE,
			want: "nil item",
		},
		{
			name: "unspecified source",
			item: &frontendv1.ConversationItem{Uuid: "a"},
			src:  frontendv1.ConversationSource_CONVERSATION_SOURCE_UNSPECIFIED,
			want: "UNSPECIFIED",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			err := StampItemConversationSource(tt.item, tt.src)

			// Assert.
			if err == nil {
				t.Fatal("StampItemConversationSource = nil, want an error")
			}
			if !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("error = %q, want it to mention %q", err, tt.want)
			}
		})
	}
}
