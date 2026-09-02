package classifier

import (
	"bytes"
	"context"
	"fmt"
	"os/exec"
	"strings"

	"claude-repld/internal/envc"
	"claude-repld/internal/prompts"
)

// BriefRouting is the routing question's brief, read from the prompts
// directory AT USE TIME so an edit takes effect without a daemon bounce.
const BriefRouting = "queue-routing-classifier"

// The two answer tokens the brief instructs the model to reply with, EXACTLY
// one of them and nothing else. They are spelled here because the brief's
// {{token_jump}} / {{token_hold}} placeholders are spliced from these: the
// question and the answer parser can never drift apart.
const (
	// TokenJump is the interject answer.
	TokenJump = "ROUTE_INTERJECT"
	// TokenHold is the wait-for-turn-end answer.
	TokenHold = "ROUTE_HOLD"
)

// The evidence a vendor verdict carries.
const (
	vendorInterjectReason = "the routing classifier answered interject"
	vendorHoldReason      = "the routing classifier answered hold"
)

// VendorSite is the guard site name the classifier's run asks under. It is the
// literal AGENTS.md names for AGENT_REPL_FORBID_VENDOR_CALLS.
const VendorSite = "classifier"

// loaderFunc reads one brief at use time. It is a seam so the judge's tests
// never touch a prompts directory.
type loaderFunc func(dir, name string) (prompts.Prompt, error)

// splicerFunc substitutes the routing brief's placeholders. It is a seam for
// the same reason loaderFunc is.
type splicerFunc func(brief prompts.Prompt, values map[string]string) (string, error)

// runnerFunc invokes the vendor binary with the composed question on stdin and
// answers with its stdout. It is a seam so the judge's tests never exec.
type runnerFunc func(ctx context.Context, bin, question string) (string, error)

// vendorJudge is the daemon's own headless vendor run.
type vendorJudge struct {
	guard      envc.VendorGuard
	bin        string
	promptsDir string
	load       loaderFunc
	splice     splicerFunc
	run        runnerFunc
}

// newVendorJudge builds the production judge.
func newVendorJudge(guard envc.VendorGuard, bin, promptsDir string) *vendorJudge {
	return &vendorJudge{guard: guard, bin: bin, promptsDir: promptsDir, load: prompts.Load, splice: spliceBrief, run: runVendor}
}

func (j *vendorJudge) Judge(ctx context.Context, running, incoming string) (Verdict, error) {
	if ExplicitInterrupt(incoming) {
		return Verdict{Interject: true, Reason: ExplicitInterruptReason, FastPath: true}, nil
	}
	if err := j.guard.Check(VendorSite); err != nil {
		return Verdict{}, fmt.Errorf("classify the incoming prompt: %w", err)
	}
	if j.bin == "" {
		return Verdict{}, fmt.Errorf("classify the incoming prompt: no vendor binary is configured")
	}

	brief, err := j.load(j.promptsDir, BriefRouting)
	if err != nil {
		return Verdict{}, fmt.Errorf("read the %s brief: %w", BriefRouting, err)
	}
	question, err := j.splice(brief, map[string]string{
		"token_jump":   TokenJump,
		"token_hold":   TokenHold,
		"running_turn": running,
		"new_message":  incoming,
	})
	if err != nil {
		return Verdict{}, fmt.Errorf("splice the %s brief: %w", BriefRouting, err)
	}

	out, err := j.run(ctx, j.bin, question)
	if err != nil {
		return Verdict{}, fmt.Errorf("run the routing classifier: %w", err)
	}
	switch strings.TrimSpace(out) {
	case TokenJump:
		return Verdict{Interject: true, Reason: vendorInterjectReason}, nil
	case TokenHold:
		return Verdict{Interject: false, Reason: vendorHoldReason}, nil
	default:
		return Verdict{}, fmt.Errorf("the routing classifier answered %q, which is neither %s nor %s",
			strings.TrimSpace(out), TokenJump, TokenHold)
	}
}

// spliceBrief is the production splicer: the brief's own substitution.
func spliceBrief(brief prompts.Prompt, values map[string]string) (string, error) {
	return brief.Splice(values)
}

// runVendor is the one exec site: a headless print run with the composed
// question on stdin, so the question never rides an argv a process listing
// would show.
func runVendor(ctx context.Context, bin, question string) (string, error) {
	cmd := exec.CommandContext(ctx, bin, "-p", "--output-format", "text")
	cmd.Stdin = strings.NewReader(question)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("%s -p: %w (stderr: %s)", bin, err, strings.TrimSpace(stderr.String()))
	}
	return stdout.String(), nil
}
