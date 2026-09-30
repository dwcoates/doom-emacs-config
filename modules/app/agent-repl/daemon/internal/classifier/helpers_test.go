package classifier

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/envc"
	"claude-repld/internal/headless"
	"claude-repld/internal/prompts"
)

// routingBrief is the loaded brief every vendor-judge subject is served, so no
// test reads the prompts directory.
var routingBrief = prompts.Prompt{
	Name:         BriefRouting,
	Body:         "answer {{token_interrupt}}, {{token_after_tool_call}} or {{token_hold}} about {{running_turn}} / {{new_message}}",
	Placeholders: []string{"token_interrupt", "token_after_tool_call", "token_hold", "running_turn", "new_message"},
}

// permissiveGuard is a guard that permits the vendor call, which is what every
// subject exercising the run path needs.
func permissiveGuard(t *testing.T) envc.VendorGuard {
	t.Helper()
	t.Setenv(envc.EnvForbidVendorCalls, "")
	return envc.NewVendorGuard(envc.Load())
}

// forbiddingGuard is the guard every test process actually runs under.
func forbiddingGuard(t *testing.T) envc.VendorGuard {
	t.Helper()
	t.Setenv(envc.EnvForbidVendorCalls, "1")
	return envc.NewVendorGuard(envc.Load())
}

// answering builds a judge whose vendor run returns out and err verbatim, and
// records the question it was handed.
func answering(t *testing.T, guard envc.VendorGuard, out string, runErr error) (*vendorJudge, *string) {
	t.Helper()
	var asked string
	j := newVendorJudge(guard, headless.New(guard, "fake-claude"), "unused")
	j.load = func(string, string) (prompts.Prompt, error) { return routingBrief, nil }
	j.splice = func(_ prompts.Prompt, values map[string]string) (string, error) {
		return values["token_interrupt"] + "|" + values["token_after_tool_call"] + "|" + values["token_hold"] + "|" +
			values["running_turn"] + "|" + values["new_message"], nil
	}
	j.run = func(_ context.Context, question string) (string, error) {
		asked = question
		return out, runErr
	}
	return j, &asked
}

// errLoad is the loader failure a brief-absent subject injects.
var errLoad = errors.New("the brief is absent")
