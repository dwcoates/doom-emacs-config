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

	"claude-repld/internal/envc"
)

// EnvClaudeBin names the vendor binary every headless run execs. AGENTS.md
// pins it as a test knob: a test points it at a fake script, and an explicit
// path is what makes the spawn legal under AGENT_REPL_FORBID_VENDOR_CALLS.
// It is deliberately the SAME variable the login pty reads, because a machine
// has one vendor CLI and not one per call site.
const EnvClaudeBin = "AGENT_REPL_CLAUDE_BIN"

// DefaultBin is the vendor binary a headless run execs when nothing names one.
const DefaultBin = "claude"

// The output formats a run may ask the CLI for.
const (
	// FormatText is the bare answer on stdout, with no envelope.
	FormatText = "text"
	// FormatJSON is the CLI's result envelope, whose `result` string is the
	// model's own answer. The envelope is parsed here so no caller learns the
	// CLI's shape twice.
	FormatJSON = "json"
)

// ModelHaiku is the small, fast model the daemon asks its own questions of.
// It is the spelling the CLI's `--model` takes.
const ModelHaiku = "haiku"

// ModelSonnet is the stronger model the daemon asks for prose a person reads
// closely: the desktop banner's summary of a turn's final answer.
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
	// Format is FormatText or FormatJSON. Empty means FormatText.
	Format string
	// ConfigDir is the account root the run bills, exported as
	// CLAUDE_CONFIG_DIR. Empty leaves the environment's own.
	ConfigDir string
	// Prompt is the composed question. It rides stdin.
	Prompt string
	// Timeout bounds the run. Zero leaves the bound to ctx alone.
	Timeout time.Duration
}

// Response is one headless answer.
type Response struct {
	// Text is the model's answer: stdout for FormatText, the envelope's
	// `result` for FormatJSON.
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
func New(guard envc.VendorGuard, bin string) *Client {
	return &Client{guard: guard, bin: ResolveBin(bin), exec: execCLI, now: time.Now}
}
