package runtimemetrics

import (
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"github.com/prometheus/client_golang/prometheus"
)

// PromptsSubmittedName counts every submission the prompt queue took
// (delivered or held), by its conversation.v1.PromptOrigin spelling. A
// submission the queue refused is not counted.
const PromptsSubmittedName = "agentrepl_prompts_submitted_total"

// promptTelemetry holds the prompt queue's submission counter. Every declared
// origin's child is resolved once here, so a count on the submit path is one
// atomic add on a map nothing writes after construction: no lock, no I/O.
type promptTelemetry struct {
	submitted *prometheus.CounterVec
	byOrigin  map[conversationv1.PromptOrigin]prometheus.Counter
}

func newPromptTelemetry() promptTelemetry {
	submitted := prometheus.NewCounterVec(prometheus.CounterOpts{
		Name: PromptsSubmittedName, Help: "Submissions the prompt queue took, delivered or held, by origin.",
	}, []string{"origin"})
	byOrigin := make(map[conversationv1.PromptOrigin]prometheus.Counter, len(conversationv1.PromptOrigin_name))
	for value, name := range conversationv1.PromptOrigin_name {
		origin := conversationv1.PromptOrigin(value)
		// The queue refuses a submission with no origin, so none is ever
		// counted under it.
		if origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
			continue
		}
		byOrigin[origin] = submitted.WithLabelValues(name)
	}
	return promptTelemetry{submitted: submitted, byOrigin: byOrigin}
}

// CountPromptSubmitted implements promptqueue.SubmissionTelemetry. The off
// surface records nothing.
//
// The queue takes only a submission whose origin conversation.v1.PromptOrigin
// declares and is not unspecified, so an origin with no resolved child is a
// broken invariant and panics.
func (s *Surface) CountPromptSubmitted(origin conversationv1.PromptOrigin) {
	if s == nil {
		return
	}
	counter, ok := s.prompts.byOrigin[origin]
	if !ok {
		panic(fmt.Sprintf("runtimemetrics: a submission was counted under origin %d, which the prompt queue never takes", int32(origin)))
	}
	counter.Inc()
}
