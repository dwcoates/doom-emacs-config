package reload

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os/exec"
	"strings"

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitexec"
)

// This package NEVER mutates a repository. Every git invocation here is a
// read-only probe (rev-parse, log, diff) whose only job is to answer what the
// merge that just landed touched, and whether it landed in the checkout this
// daemon itself runs from.
//
// Every one of them goes through gitexec.Command, the daemon's single
// repository-selection boundary, so `-C dir` is the ONLY thing that picks the
// repository. For this package the stakes of an inherited GIT_DIR are higher
// than a wrong answer: a leaked binding would make EVERY merge target resolve
// to the leaking repository's identity, so an unrelated repository's merge
// could be mistaken for a self-merge and bounce the whole live stack.

// gitCapture runs a read-only probe and returns trimmed stdout. A non-zero exit
// is an error: every caller here is asking a question whose unanswered form
// must abort the reload decision rather than be guessed at.
func gitCapture(ctx context.Context, dir string, args ...string) (string, error) {
	cmd := gitexec.Command(ctx, dir, args...)
	var out, errb bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errb
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("reload: git %v in %s: %w (stderr: %s)", args, dir, err, dlog.Clamp(errb.String(), 400))
	}
	return strings.TrimSpace(out.String()), nil
}

// gitSucceeds runs a probe whose ANSWER is its exit status (merge-base
// --is-ancestor). A non-zero exit is the "no" answer, not a failure; only a
// failure to run git at all is an error.
func gitSucceeds(ctx context.Context, dir string, args ...string) (bool, error) {
	cmd := gitexec.Command(ctx, dir, args...)
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	err := cmd.Run()
	if err == nil {
		return true, nil
	}
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) {
		return false, nil
	}
	return false, fmt.Errorf("reload: git %v in %s: %w (output: %s)", args, dir, err, dlog.Clamp(out.String(), 400))
}
