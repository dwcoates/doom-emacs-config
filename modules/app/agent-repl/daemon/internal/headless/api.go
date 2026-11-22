// Package headless is the daemon's OWN one-shot vendor run: `claude -p` with
// the composed question on stdin and the answer read off stdout.
//
// It is the single exec site for every in-process model question the daemon
// asks — the interjection classifier's routing verdict and the workspace
// naming call — so the vendor guard, the binary resolution, the stdin
// discipline and the deadline are stated ONCE rather than copied per caller.
// A per-session agent turn is NOT this: that is the shim's business and runs a
// whole SDK session.
//
// The prompt rides STDIN, never an argv, so a process listing never shows the
// user's words.
package headless

import (
	"context"
	"errors"
	"os"
	"time"

	"agent-repld/internal/envc"
	conversationv1 "agentrepl/proto/conversation/v1"
)

// EnvClaudeBin names the vendor binary every headless run execs. AGENTS.md
// pins it as a test knob: a test points it at a fake script, and an explicit
// path is what makes the spawn legal under AGENT_REPL_FORBID_VENDOR_CALLS.
// It is deliberately the SAME variable the login pty reads, because a machine
// has one vendor CLI and not one per call site.
const EnvClaudeBin = "AGENT_REPL_CLAUDE_BIN"

// DefaultBin is the vendor binary a headless run execs when nothing names one.
const DefaultBin = "claude"

// ModelHaiku is the small, fast model the daemon asks its own questions of.
// It is the spelling the CLI's `--model` takes.
const ModelHaiku = "haiku"

// ShortAnswerTimeout bounds ONE attempt of a short-answer question to
// ModelHaiku: a handful of tokens back, such as a workspace's name or a
// question's abbreviation. Every such call shares it, because what bounds them
// is the CLI's start-up and the round trip, not the answer's length.
//
// It is just over twice the slowest healthy call measured: over the week to
// 2026-10-27 the naming call answered 128 times with a 9.7s maximum, and the
// abbreviation call 10 times with a 10.2s maximum. The 15s it was before
// timed both naming attempts out on 2026-10-27 while the call ran slow.
const ShortAnswerTimeout = 21 * time.Second

// ModelSonnet is the stronger model the daemon asks for prose a person reads
// closely: the news digest's condensing.
const ModelSonnet = "sonnet"

// The causes a failed run reports. They are a CLOSED set of tokens, because
// each one rides a refusal arm to a client that renders it.
const (
	// CauseGuardRefused is AGENT_REPL_FORBID_VENDOR_CALLS refusing the site.
	CauseGuardRefused = "guard_refused"
	// CauseNoBinary is a client built with no vendor binary at all.
	CauseNoBinary = "no_binary"
	// CauseTimeout is a run that did not answer within its budget.
	CauseTimeout = "timeout"
	// CauseExitStatus is a run the CLI failed: a non-zero exit, or a binary
	// that would not start.
	CauseExitStatus = "exit_status"
	// CauseUnreadableEnvelope is a JSON run whose envelope did not parse, or
	// which reported its own error.
	CauseUnreadableEnvelope = "unreadable_envelope"
)

// Request is one headless question.
type Request struct {
	// Site is the vendor-guard site name this run asks under. Required: a run
	// with no site cannot be refused by name, and an unnameable vendor call is
	// exactly what the guard exists to prevent.
	Site string
	// Model is the `--model` the run asks for. Empty omits the flag and takes
	// the CLI's own default.
	Model string
	// ConfigDir is the account root the run bills, exported as
	// CLAUDE_CONFIG_DIR. Empty leaves the environment's own.
	ConfigDir string
	// Prompt is the composed question. It rides stdin.
	Prompt string
	// Timeout bounds the run. Zero leaves the bound to ctx alone.
	Timeout time.Duration
	// Kind is what the run may do: KindInference or KindToolCalls. Required:
	// every headless call states which kind it is, and one that states none
	// is a caller bug, so Run panics.
	Kind Kind
	// Tools are the built-in CLI tools a KindToolCalls run may use
	// ("WebSearch"): exactly these are made available (`--tools`) and
	// pre-approved (`--allowedTools`), since a headless run has no one to
	// ask. A KindToolCalls run names at least one, and a KindInference run
	// names none; either broken is a caller bug, and Run panics.
	Tools []string
}

// Kind is what a headless run may do.
//
// EVERY HEADLESS CALL IS PURE INFERENCE UNLESS IT HAS A VERY GOOD REASON NOT
// TO BE (owner ruling, 2026-11-19): a daemon question is a short
// classification or a short piece of writing, and a run that keeps the CLI's
// own tool set may spend its bound on tool calls. Which kind a new call is,
// is the user's to say when it is added.
type Kind int

const (
	// KindUnspecified is a request that stated no kind; Run panics on it.
	KindUnspecified Kind = iota
	// KindInference is bare inference: no tools at all (`--tools ""`) and an
	// empty system prompt (`--system-prompt ""`), so the model sees only
	// Prompt.
	KindInference
	// KindToolCalls may call exactly the Request's Tools.
	KindToolCalls
)

// Response is one headless answer.
type Response struct {
	// Text is the model's answer: the result envelope's `result`.
	Text string
	// Model is the model the request asked for, echoed so a caller logging the
	// answer does not have to hold the constant twice.
	Model string
	// Duration is how long the run took, wall clock.
	Duration time.Duration
}

// Error is a failed run, carrying the closed-set cause its caller surfaces.
type Error struct {
	// Cause is one of the Cause* tokens.
	Cause string
	// Detail is the failure's own account, for the log and the sentence.
	Detail string
}

func (e *Error) Error() string { return e.Cause + ": " + e.Detail }

// CauseOf reads a failed run's closed-set cause. A failure that is not this
// package's own *Error is reported as CauseExitStatus rather than becoming an
// empty cause, so a caller's `cause` field always names something. Every
// caller that surfaces a headless failure's cause reads it here, so no two
// spell the fallback differently.
func CauseOf(err error) string {
	var hErr *Error
	if errors.As(err, &hErr) {
		return hErr.Cause
	}
	return CauseExitStatus
}

// ResolveBin answers the vendor binary a headless run execs: the configured
// one, else $AGENT_REPL_CLAUDE_BIN, else DefaultBin. It never answers empty,
// which is the hole `buildJudge` had — it built the classifier with an empty
// binary, and every classification refused before it reached the model.
func ResolveBin(configured string) string {
	if configured != "" {
		return configured
	}
	if fromEnv := os.Getenv(EnvClaudeBin); fromEnv != "" {
		return fromEnv
	}
	return DefaultBin
}

// BinSource names where ResolveBin's answer came from, for the boot record.
func BinSource(configured string) string {
	switch {
	case configured != "":
		return "configured"
	case os.Getenv(EnvClaudeBin) != "":
		return EnvClaudeBin
	default:
		return "default"
	}
}

// Runner is the narrow surface a caller depends on, so a component that asks a
// headless question is testable against a scripted answer with no exec.
type Runner interface {
	// Run asks one headless question.
	Run(ctx context.Context, req Request) (Response, error)
	// Bin reports the vendor binary this runner execs.
	Bin() string
}

// New builds the production client. bin empty resolves through ResolveBin.
func New(guard envc.VendorGuard, bin string, spend SpendSink) *Client {
	if spend == nil {
		panic("headless: a client needs the spend sink every run's vendor calls are counted into")
	}
	return &Client{guard: guard, bin: ResolveBin(bin), exec: execCLI, now: time.Now, spend: spend}
}

// SpendSink is told the vendor calls one run made, billed to configDir, as
// spend entries (conversation.v1.VendorSpend) the metrics surface folds
// (runtimemetrics.Surface.ObserveVendorSpend).
type SpendSink func(configDir string, entries []*conversationv1.VendorSpend)
