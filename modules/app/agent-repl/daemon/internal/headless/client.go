package headless

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"

	"claude-repld/internal/envc"
)

// execFunc is the one exec site, as a seam so the client's own tests never
// spawn anything.
type execFunc func(ctx context.Context, bin string, args []string, env []string, stdin string) (string, error)

// Client is the daemon's headless vendor runner: one guard, one binary, one
// exec site.
type Client struct {
	guard envc.VendorGuard
	bin   string
	exec  execFunc
	now   func() time.Time
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

	format := req.Format
	if format == "" {
		format = FormatText
	}
	args := []string{"-p", "--output-format", format}
	if req.Model != "" {
		args = append(args, "--model", req.Model)
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
	if err != nil {
		// A DEADLINE IS ITS OWN CAUSE. exec reports a killed child as a plain
		// exit failure, so the context is what distinguishes "the model took
		// too long" from "the CLI fell over", and the two need different
		// remedies.
		if errors.Is(runCtx.Err(), context.DeadlineExceeded) {
			return Response{}, &Error{Cause: CauseTimeout, Detail: err.Error()}
		}
		return Response{}, &Error{Cause: CauseExitStatus, Detail: err.Error()}
	}

	text := stdout
	if format == FormatJSON {
		text, err = resultOf(stdout)
		if err != nil {
			return Response{}, &Error{Cause: CauseUnreadableEnvelope, Detail: err.Error()}
		}
	}
	return Response{Text: text, Model: req.Model, Duration: elapsed}, nil
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

// envelope is the slice of the CLI's `--output-format json` answer this
// package reads. The model's own answer is `result`; `is_error` is the CLI
// reporting its own failure inside a zero exit.
type envelope struct {
	Result  string `json:"result"`
	IsError bool   `json:"is_error"`
	Subtype string `json:"subtype"`
}

// resultOf reads the model's answer out of a JSON envelope. An envelope that
// does not parse, or that reports its own error, is a failure and never an
// empty answer passed off as the model's.
func resultOf(stdout string) (string, error) {
	var env envelope
	if err := json.Unmarshal([]byte(stdout), &env); err != nil {
		return "", fmt.Errorf("the json envelope did not parse: %w", err)
	}
	if env.IsError {
		return "", fmt.Errorf("the cli reported an error (%s): %s", env.Subtype, strings.TrimSpace(env.Result))
	}
	return env.Result, nil
}

// execCLI is the production exec: a headless print run with the composed
// question on stdin, so the question never rides an argv a process listing
// would show.
func execCLI(ctx context.Context, bin string, args, env []string, stdin string) (string, error) {
	cmd := exec.CommandContext(ctx, bin, args...) //nolint:gosec // daemon-resolved binary, never client input
	cmd.Stdin = strings.NewReader(stdin)
	cmd.Env = env
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("%s %s: %w (stderr: %s)", bin, strings.Join(args, " "), err, strings.TrimSpace(stderr.String()))
	}
	return stdout.String(), nil
}
