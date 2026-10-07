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

func TestRetryAnsweredBy(t *testing.T) {
	prose := &conversationv1.AgentActivity{Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{}}}
	tool := &conversationv1.AgentActivity{Item: &conversationv1.AgentActivity_Read{}}
	cases := []struct {
		name    string
		retried string
		agent   string
		act     *conversationv1.AgentActivity
		want    bool
	}{
		{name: "the retried agent's answer ends its retry", retried: "main", agent: "main", act: prose, want: true},
		{name: "another agent's answer does not end the retry", retried: "main", agent: "sub-1", act: prose, want: false},
		{name: "the retried agent's frame that does not answer does not end the retry", retried: "main", agent: "main", act: tool, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := RetryAnsweredBy(tc.retried, tc.agent, tc.act)

			// Assert
			if got != tc.want {
				t.Fatalf("RetryAnsweredBy(%q, %q) = %v, want %v", tc.retried, tc.agent, got, tc.want)
			}
		})
	}
}
