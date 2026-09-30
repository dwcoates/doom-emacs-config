package classifier

import (
	"context"
	"fmt"
	"strings"
	"time"

	"claude-repld/internal/envc"
	"claude-repld/internal/headless"
	"claude-repld/internal/prompts"
)

// BriefRouting is the routing question's brief, read from the prompts
// directory AT USE TIME so an edit takes effect without a daemon bounce.
const BriefRouting = "queue-routing-classifier"

// The three answer tokens the brief instructs the model to reply with, EXACTLY
// one of them and nothing else. They are spelled here because the brief's
// {{token_interrupt}} / {{token_after_tool_call}} / {{token_hold}}
// placeholders are spliced from these: the question and the answer parser can
// never drift apart.
const (
	// TokenInterrupt is the interrupt answer.
	TokenInterrupt = "ROUTE_INTERRUPT"
	// TokenAfterToolCall is the join-after-the-current-tool-call answer.
	TokenAfterToolCall = "ROUTE_AFTER_TOOL_CALL"
	// TokenHold is the wait-for-turn-end answer.
	TokenHold = "ROUTE_HOLD"
)

// The evidence a vendor verdict carries.
const (
	vendorInterruptReason     = "the routing classifier answered interrupt"
	vendorAfterToolCallReason = "the routing classifier answered after this tool call"
	vendorHoldReason          = "the routing classifier answered hold"
)

// VendorSite is the guard site name the classifier's run asks under. It is the
// literal AGENTS.md names for AGENT_REPL_FORBID_VENDOR_CALLS.
const VendorSite = "classifier"

// RunTimeout bounds one classification. The prompt queue holds an incoming
// message while this runs, so an unanswered call must fail rather than hold
// forever.
const RunTimeout = 15 * time.Second

// loaderFunc reads one brief at use time. It is a seam so the judge's tests
// never touch a prompts directory.
type loaderFunc func(dir, name string) (prompts.Prompt, error)

// splicerFunc substitutes the routing brief's placeholders. It is a seam for
// the same reason loaderFunc is.
type splicerFunc func(brief prompts.Prompt, values map[string]string) (string, error)

// runnerFunc invokes the vendor binary with the composed question on stdin and
// answers with its stdout. It is a seam so the judge's tests never exec.
type runnerFunc func(ctx context.Context, question string) (string, error)

// vendorJudge is the daemon's own headless vendor run. The exec itself is
// internal/headless's, shared with the workspace naming call, so the guard,
// the binary resolution and the stdin discipline are stated once.
type vendorJudge struct {
	guard      envc.VendorGuard
	headless   headless.Runner
	promptsDir string
	load       loaderFunc
	splice     splicerFunc
	run        runnerFunc
}

// newVendorJudge builds the production judge.
func newVendorJudge(guard envc.VendorGuard, runner headless.Runner, promptsDir string) *vendorJudge {
	j := &vendorJudge{guard: guard, headless: runner, promptsDir: promptsDir, load: prompts.Load, splice: spliceBrief}
	j.run = j.runHeadless
	return j
}

func (j *vendorJudge) Judge(ctx context.Context, running, incoming string) (Verdict, error) {
	if ExplicitInterrupt(incoming) {
		return Verdict{Route: RouteInterrupt, Reason: ExplicitInterruptReason, FastPath: true}, nil
	}
	if err := j.guard.Check(VendorSite); err != nil {
		return Verdict{}, fmt.Errorf("classify the incoming prompt: %w", err)
	}
	if j.headless == nil || j.headless.Bin() == "" {
		return Verdict{}, fmt.Errorf("classify the incoming prompt: no vendor binary is configured")
	}

	brief, err := j.load(j.promptsDir, BriefRouting)
	if err != nil {
		return Verdict{}, fmt.Errorf("read the %s brief: %w", BriefRouting, err)
	}
	question, err := j.splice(brief, map[string]string{
		"token_interrupt":       TokenInterrupt,
		"token_after_tool_call": TokenAfterToolCall,
		"token_hold":            TokenHold,
		"running_turn":          running,
		"new_message":           incoming,
	})
	if err != nil {
		return Verdict{}, fmt.Errorf("splice the %s brief: %w", BriefRouting, err)
	}

	out, err := j.run(ctx, question)
	if err != nil {
		return Verdict{}, fmt.Errorf("run the routing classifier: %w", err)
	}
	switch strings.TrimSpace(out) {
	case TokenInterrupt:
		return Verdict{Route: RouteInterrupt, Reason: vendorInterruptReason}, nil
	case TokenAfterToolCall:
		return Verdict{Route: RouteAfterToolCall, Reason: vendorAfterToolCallReason}, nil
	case TokenHold:
		return Verdict{Route: RouteQueue, Reason: vendorHoldReason}, nil
	default:
		return Verdict{}, fmt.Errorf("the routing classifier answered %q, which is none of %s, %s or %s",
			strings.TrimSpace(out), TokenInterrupt, TokenAfterToolCall, TokenHold)
	}
}

// spliceBrief is the production splicer: the brief's own substitution.
func spliceBrief(brief prompts.Prompt, values map[string]string) (string, error) {
	return brief.Splice(values)
}

// runHeadless is the production runner: the shared headless facility, asked
// for plain text under the classifier's own guard site.
func (j *vendorJudge) runHeadless(ctx context.Context, question string) (string, error) {
	resp, err := j.headless.Run(ctx, headless.Request{
		Site:    VendorSite,
		Format:  headless.FormatText,
		Prompt:  question,
		Timeout: RunTimeout,
	})
	if err != nil {
		return "", err
	}
	return resp.Text, nil
}
