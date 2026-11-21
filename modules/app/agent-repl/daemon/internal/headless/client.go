package headless

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"agent-repld/internal/childproc"
	"agent-repld/internal/envc"
	conversationv1 "agentrepl/proto/conversation/v1"
)

// execFunc is the one exec site, as a seam so the client's own tests never
// spawn anything. A failed run answers its error AND whatever it wrote to
// stdout before failing, because a JSON run states its own failure there.
type execFunc func(ctx context.Context, bin string, args []string, env []string, stdin string) (string, error)

// Client is the daemon's headless vendor runner: one guard, one binary, one
// exec site.
type Client struct {
	guard envc.VendorGuard
	bin   string
	exec  execFunc
	now   func() time.Time
	// spend is told every vendor call a run made (SpendSink).
	spend SpendSink
}

// Bin reports the vendor binary this client execs.
func (c *Client) Bin() string { return c.bin }

// Run asks one headless question.
//
// The order is deliberate: the guard first, so a forbidden site never composes
// argv at all; then the binary; then the bounded spawn. Every failure answers
// an *Error carrying a closed-set cause, because the causes ride a refusal arm
// to a client that renders them.
func (c *Client) Run(ctx context.Context, req Request) (Response, error) {
	if req.Site == "" {
		return Response{}, fmt.Errorf("headless: a run must name its vendor-guard site")
	}
	switch {
	case req.Kind == KindInference && len(req.Tools) > 0:
		panic(fmt.Sprintf("headless: an inference run for site %q named tools %v", req.Site, req.Tools))
	case req.Kind == KindToolCalls && len(req.Tools) == 0:
		panic(fmt.Sprintf("headless: a tool-calling run for site %q named no tools", req.Site))
	case req.Kind != KindInference && req.Kind != KindToolCalls:
		panic(fmt.Sprintf("headless: the run for site %q states no kind (%d)", req.Site, req.Kind))
	}
	// THE GUARD AND THE BINARY ARE ONE RULE, and it is login's, stated once
	// here for every headless call: the guard refuses only the DEFAULT
	// `claude` on PATH, because an explicitly named binary is by definition
	// not a call to the real CLI. That is what lets a test point
	// AGENT_REPL_CLAUDE_BIN at a scripted stand-in and still exercise the
	// whole call path, and what makes a run with no binary named refuse.
	if c.bin == DefaultBin {
		if err := c.guard.Check(req.Site); err != nil {
			return Response{}, &Error{Cause: CauseGuardRefused, Detail: err.Error()}
		}
	}
	if c.bin == "" {
		return Response{}, &Error{Cause: CauseNoBinary, Detail: "no vendor binary is configured"}
	}

	args := []string{"-p", "--output-format", streamFormat, "--verbose"}
	if req.Model != "" {
		args = append(args, "--model", req.Model)
	}
	switch req.Kind {
	case KindToolCalls:
		tools := strings.Join(req.Tools, ",")
		args = append(args, "--tools", tools, "--allowedTools", tools)
	case KindInference:
		args = append(args, "--tools", "", "--system-prompt", "")
	}
	args = append(args, pinnedArgs...)

	env := os.Environ()
	if req.ConfigDir != "" {
		env = append(env, "CLAUDE_CONFIG_DIR="+req.ConfigDir)
	}

	runCtx := ctx
	if req.Timeout > 0 {
		var cancel context.CancelFunc
		runCtx, cancel = context.WithTimeout(ctx, req.Timeout)
		defer cancel()
	}

	started := c.now()
	stdout, err := c.exec(runCtx, c.bin, args, env, req.Prompt)
	elapsed := c.now().Sub(started)
	// EVERY CALL THE RUN MADE IS COUNTED, whether or not it answered: a run
	// that failed after its calls still spent them.
	result, calls, streamErr := readStream(stdout)
	if len(calls) > 0 {
		c.spend(effectiveConfigDir(req.ConfigDir), spendOf(calls, conversationv1.VendorCallCause_VENDOR_CALL_CAUSE_HEADLESS_QUESTION))
	}
	if err != nil {
		// A DEADLINE IS ITS OWN CAUSE. exec reports a killed child as a plain
		// exit failure, so the context is what distinguishes "the model took
		// too long" from "the CLI fell over", and the two need different
		// remedies.
		if errors.Is(runCtx.Err(), context.DeadlineExceeded) {
			return Response{}, &Error{Cause: CauseTimeout, Detail: err.Error()}
		}
		return Response{}, &Error{Cause: CauseExitStatus, Detail: err.Error() + exitAccount(result, stdout)}
	}
	text, err := resultOf(result, streamErr)
	if err != nil {
		return Response{}, &Error{Cause: CauseUnreadableEnvelope, Detail: err.Error()}
	}
	return Response{Text: text, Model: req.Model, Duration: elapsed}, nil
}

// effectiveConfigDir is the account root a run bills: the one it named, else
// the environment's own CLAUDE_CONFIG_DIR, else the CLI's default under the
// home directory.
func effectiveConfigDir(named string) string {
	if named != "" {
		return named
	}
	if env := os.Getenv("CLAUDE_CONFIG_DIR"); env != "" {
		return env
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return filepath.Join(home, ".claude")
}

// pinnedSettings is the settings every headless run carries on `--settings`,
// so the user's INTERACTIVE configuration under CLAUDE_CONFIG_DIR can never
// change what a headless call does. MEASURED 2026-09-29: a user settings file
// with `alwaysThinkingEnabled: true` made the three-word naming call think for
// 313-516 output tokens and 5-6.7s end to end, with a tail past the 15s naming
// bound; pinned off it answers in ~19 tokens and ~2.4s.
type pinnedSettings struct {
	AlwaysThinkingEnabled bool `json:"alwaysThinkingEnabled"`
}

// pinnedArgs is THE ONE definition of the flags every headless run carries,
// whatever its Request says:
//
//   - `--settings` with thinking off (pinnedSettings), because a daemon
//     question is a short classification, not a reasoning task;
//   - `--strict-mcp-config` with no `--mcp-config`, so no MCP server (the
//     user's claude.ai connectors included) is started for a one-shot answer;
//   - `--safe-mode`, which the CLI documents as disabling "CLAUDE.md, skills,
//     installed plugins, hooks, MCP servers, custom commands and agents" while
//     "Auth, model selection, built-in tools and plugins, and permissions work
//     normally" -- so the per-account OAuth the call bills through survives.
//     `--bare` is NOT usable: it refuses OAuth.
var pinnedArgs = mustPinnedArgs()

// mustPinnedArgs composes pinnedArgs. The settings are a fixed Go value, so a
// marshal failure is a broken build, and it fails the process at start rather
// than letting a headless call run on the user's settings.
func mustPinnedArgs() []string {
	settings, err := json.Marshal(pinnedSettings{AlwaysThinkingEnabled: false})
	if err != nil {
		panic(fmt.Sprintf("headless: the pinned settings did not marshal: %v", err))
	}
	return []string{"--settings", string(settings), "--strict-mcp-config", "--safe-mode"}
}

// envelope is the slice of the CLI's result envelope this package reads:
// the stream's last line. The model's own answer is `result`; `is_error` is
// the CLI reporting its own failure inside a zero exit.
type envelope struct {
	Result  string `json:"result"`
	IsError bool   `json:"is_error"`
	Subtype string `json:"subtype"`
}

// resultOf reads the model's answer out of a run's result envelope. A stream
// that broke, one that carried no result, or a result reporting its own
// error is a failure, never an empty answer passed off as the model's.
func resultOf(result *envelope, streamErr error) (string, error) {
	switch {
	case streamErr != nil:
		return "", fmt.Errorf("the stream did not parse: %w", streamErr)
	case result == nil:
		return "", errors.New("the stream carried no result")
	case result.IsError:
		return "", fmt.Errorf("the cli reported an error %s", result.account())
	}
	return result.Result, nil
}

// account words an error envelope's own account of its failure: its subtype
// and its result. Every site that reports an envelope's error words it here.
func (env envelope) account() string {
	return fmt.Sprintf("(%s): %s", env.Subtype, strings.TrimSpace(env.Result))
}

// exitAccount is what a run that exited non-zero said on stdout, worded to
// follow the exit's own error, or "" when it said nothing.
//
// GROUNDED (2026-10-19). A naming call under an account whose OAuth session
// had expired exited 1 with an EMPTY stderr: the CLI states its failure in
// its result envelope, and the exit discarded it, so the log read "exit
// status 1 (stderr: )" and named no cause at all. An error envelope is read
// for its own account; anything else on stdout is carried as written.
func exitAccount(result *envelope, stdout string) string {
	if result != nil && result.IsError {
		return "; the cli said " + result.account()
	}
	said := strings.TrimSpace(stdout)
	if said == "" {
		return ""
	}
	return "; the cli said: " + said
}

// execCLI is the production exec: a headless print run with the composed
// question on stdin, so the question never rides an argv a process listing
// would show.
func execCLI(ctx context.Context, bin string, args, env []string, stdin string) (string, error) {
	cmd := childproc.CommandContext(ctx, childproc.Stable, bin, args...) //nolint:gosec // daemon-resolved binary, never client input
	cmd.Stdin = strings.NewReader(stdin)
	cmd.Env = env
	stdout, stderr, runErr, err := childproc.Run(cmd, nil)
	if err != nil {
		return "", fmt.Errorf("%s %s: %w", bin, strings.Join(args, " "), errors.Join(runErr, err))
	}
	if runErr != nil {
		return stdout, fmt.Errorf("%s %s: %w (stderr: %s)", bin, strings.Join(args, " "), runErr, strings.TrimSpace(stderr))
	}
	return stdout, nil
}
