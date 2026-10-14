package promptorigin

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestOnlyAMergeQueueNoticeIsUnseen(t *testing.T) {
	for value := range conversationv1.PromptOrigin_name {
		origin := conversationv1.PromptOrigin(value)
		t.Run(origin.String(), func(t *testing.T) {
			// Act.
			got := Unseen(origin)

			// Assert.
			if want := origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_QUEUE_NOTICE; got != want {
				t.Fatalf("Unseen(%s) = %v, want %v", origin, got, want)
			}
		})
	}
}

func TestUnseenNamedReadsTheStoredSpelling(t *testing.T) {
	tests := []struct {
		name string
		want bool
	}{
		{name: "PROMPT_ORIGIN_MERGE_QUEUE_NOTICE", want: true},
		{name: "PROMPT_ORIGIN_MERGE_HANDBACK", want: false},
		{name: "NO_SUCH_ORIGIN", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := UnseenNamed(tt.name)

			// Assert.
			if got != tt.want {
				t.Fatalf("UnseenNamed(%q) = %v, want %v", tt.name, got, tt.want)
			}
		})
	}
}
