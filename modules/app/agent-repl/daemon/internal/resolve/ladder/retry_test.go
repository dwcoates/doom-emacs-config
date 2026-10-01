package ladder

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestRetryAnswered(t *testing.T) {
	cases := []struct {
		name string
		act  *conversationv1.AgentActivity
		want bool
	}{
		{name: "reasoning answers the retry", act: &conversationv1.AgentActivity{Item: &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{}}}, want: true},
		{name: "prose answers the retry", act: &conversationv1.AgentActivity{Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{}}}, want: true},
		{name: "a tool frame carrying usage answers the retry", act: &conversationv1.AgentActivity{Item: &conversationv1.AgentActivity_Read{}, Usage: &conversationv1.TokenUsage{}}, want: true},
		{name: "a tool frame without usage does not answer the retry", act: &conversationv1.AgentActivity{Item: &conversationv1.AgentActivity_Read{}}, want: false},
		{name: "a nil frame does not answer the retry", act: nil, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := RetryAnswered(tc.act)

			// Assert
			if got != tc.want {
				t.Fatalf("RetryAnswered = %v, want %v", got, tc.want)
			}
		})
	}
}
