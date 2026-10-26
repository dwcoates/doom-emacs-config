package headless

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
)

// CauseInvalidAnswer is an Ask's failure cause when the model answered but no
// answer passed the caller's validation. Every other cause is a run's own.
const CauseInvalidAnswer = "invalid_answer"

// Ask is one validated headless question: Request asked until Validate
// accepts an answer, at most Attempts times.
type Ask struct {
	// Request is asked unchanged on every attempt.
	Request Request
	// Attempts bounds the calls made. At least one.
	Attempts int
	// Validate answers the model's raw text as the value the caller keeps,
	// or why the text is not one.
	Validate func(answer string) (string, error)
}

// AskFailure is an Ask that produced no valid answer.
type AskFailure struct {
	// Cause is one of this package's Cause* tokens.
	Cause string
	// Detail is the failure's own account.
	Detail string
	// Attempts is how many calls were made.
	Attempts uint32
	// Answer is the last answer the model gave, empty when it never answered.
	Answer string
}

func (f *AskFailure) Error() string {
	return fmt.Sprintf("the headless question failed after %d attempts (%s): %s", f.Attempts, f.Cause, f.Detail)
}

// AskValidated asks ASK's question until an answer validates. Each attempt
// that fails records an Info under OP; the caller owns the failure's one
// Error record. A guard refusal is never retried: it is a standing fact
// about this process, so a second call would be refused identically.
func AskValidated(ctx context.Context, r Runner, ask Ask, log dlog.Logger, op string) (string, *AskFailure) {
	if ask.Attempts < 1 {
		panic(fmt.Sprintf("headless.AskValidated: %d attempts", ask.Attempts))
	}
	var last AskFailure
	for attempt := 1; attempt <= ask.Attempts; attempt++ {
		resp, err := r.Run(ctx, ask.Request)
		if err != nil {
			last = AskFailure{Cause: CauseOf(err), Detail: err.Error(), Attempts: uint32(attempt)}
			log.Info(op, "the headless question did not answer", dlog.Context{
				"site": ask.Request.Site, "cause": last.Cause, "detail": last.Detail, "attempt": attempt,
			})
			if last.Cause == CauseGuardRefused {
				break
			}
			continue
		}
		value, err := ask.Validate(resp.Text)
		if err != nil {
			last = AskFailure{Cause: CauseInvalidAnswer, Detail: err.Error(), Attempts: uint32(attempt), Answer: resp.Text}
			log.Info(op, "the headless answer did not validate", dlog.Context{
				"site": ask.Request.Site, "answer": resp.Text, "reason": err.Error(), "attempt": attempt,
			})
			continue
		}
		log.Info(op, "the headless question answered", dlog.Context{
			"site": ask.Request.Site, "value": value, "attempt": attempt, "duration_ms": resp.Duration.Milliseconds(),
		})
		return value, nil
	}
	return "", &last
}
