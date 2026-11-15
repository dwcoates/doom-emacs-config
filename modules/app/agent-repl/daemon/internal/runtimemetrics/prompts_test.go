package runtimemetrics

import (
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestPromptSubmissionsAreServedByOrigin(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.CountPromptSubmitted(conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT)
	s.CountPromptSubmitted(conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT)
	s.CountPromptSubmitted(conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_prompts_submitted_total{origin="PROMPT_ORIGIN_AGENT_SENT"} 2`,
		`agentrepl_prompts_submitted_total{origin="PROMPT_ORIGIN_USER_SENT"} 1`,
	)
}

// TestEveryDeclaredOriginIsServedFromZero pins that each declared origin's
// series stands at 0 before its first count, so increase_pure reads the
// first one, and that the unspecified origin, which the queue never takes,
// has no series.
func TestEveryDeclaredOriginIsServedFromZero(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	body := scrape(t, s)

	// Assert
	for value, name := range conversationv1.PromptOrigin_name {
		series := `agentrepl_prompts_submitted_total{origin="` + name + `"} 0`
		if value == int32(conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED) {
			if strings.Contains(body, `origin="`+name+`"`) {
				t.Fatalf("the unspecified origin has a series:\n%s", body)
			}
			continue
		}
		mustContain(t, body, series)
	}
}

func TestCountingAnOriginTheQueueNeverTakesPanics(t *testing.T) {
	tests := []struct {
		name   string
		origin conversationv1.PromptOrigin
	}{
		{name: "unspecified", origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED},
		{name: "undeclared", origin: conversationv1.PromptOrigin(9999)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := openSurface(t)
			defer func() {
				// Assert
				if recover() == nil {
					t.Fatalf("counting origin %d must panic", int32(tt.origin))
				}
			}()

			// Act
			s.CountPromptSubmitted(tt.origin)
		})
	}
}

func TestTheOffSurfaceCountsNoPromptSubmission(t *testing.T) {
	// Arrange
	var s *Surface

	// Act and assert: the off surface takes the count and records nothing.
	s.CountPromptSubmitted(conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT)
}
