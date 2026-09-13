package headless

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/envc"
)

// permissiveGuard permits the vendor call, which is what every subject
// exercising the run path needs.
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

// fakeVendor writes an executable stand-in for the vendor binary and answers
// its path. NOTHING here ever runs the real `claude`: the exec site's own
// tests drive a scripted executable, the same way the git client's tests drive
// a scripted `git`.
func fakeVendor(t *testing.T, script string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "fake-claude")
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"+script+"\n"), 0o755); err != nil {
		t.Fatalf("write the fake vendor %q: %v", path, err)
	}
	return path
}

// scripted builds a client whose exec seam answers out and err verbatim,
// recording the argv, env and stdin it was handed.
type recorded struct {
	Bin   string
	Args  []string
	Env   []string
	Stdin string
}

func scripted(t *testing.T, guard envc.VendorGuard, bin, out string, runErr error) (*Client, *recorded) {
	t.Helper()
	seen := &recorded{}
	c := New(guard, bin)
	c.exec = func(_ context.Context, execBin string, args, env []string, stdin string) (string, error) {
		seen.Bin, seen.Args, seen.Env, seen.Stdin = execBin, args, env, stdin
		return out, runErr
	}
	return c, seen
}

func TestRunRefusesARequestWithNoSite(t *testing.T) {
	// Arrange.
	c, _ := scripted(t, permissiveGuard(t), "fake-claude", "ok", nil)

	// Act.
	_, err := c.Run(context.Background(), Request{Prompt: "q"})

	// Assert.
	if err == nil {
		t.Fatal("Run() = nil error, want a refusal: an unnameable vendor call cannot be guarded")
	}
}

func TestRunRefusesWhenTheGuardForbidsTheSite(t *testing.T) {
	// Arrange: the DEFAULT binary, which is the only spelling the guard
	// refuses — an explicit path is by definition not the real CLI.
	c, seen := scripted(t, forbiddingGuard(t), DefaultBin, "ok", nil)

	// Act.
	_, err := c.Run(context.Background(), Request{Site: "workspace_naming", Prompt: "q"})

	// Assert.
	var hErr *Error
	if !errors.As(err, &hErr) || hErr.Cause != CauseGuardRefused {
		t.Fatalf("err = %v, want the %s cause", err, CauseGuardRefused)
	}
	if seen.Bin != "" {
		t.Fatalf("the exec site ran %q, want a forbidden site never to compose argv", seen.Bin)
	}
}

// TestRunPermitsAnExplicitBinaryUnderTheGuard pins login's rule, which every
// headless call now shares: a named stand-in is not the real CLI, so a
// forbidding guard does not refuse it and a test can drive the whole path.
func TestRunPermitsAnExplicitBinaryUnderTheGuard(t *testing.T) {
	// Arrange.
	c, seen := scripted(t, forbiddingGuard(t), "/tmp/fake-claude", "ok", nil)

	// Act.
	if _, err := c.Run(context.Background(), Request{Site: "workspace_naming", Prompt: "q"}); err != nil {
		t.Fatalf("Run() error = %v, want nil", err)
	}

	// Assert.
	if seen.Bin != "/tmp/fake-claude" {
		t.Fatalf("the exec site ran %q, want the explicitly named stand-in", seen.Bin)
	}
}

func TestRunRefusesWithNoBinaryConfigured(t *testing.T) {
	// Arrange: a client whose binary was emptied after resolution.
	c, _ := scripted(t, permissiveGuard(t), "fake-claude", "ok", nil)
	c.bin = ""

	// Act.
	_, err := c.Run(context.Background(), Request{Site: "workspace_naming", Prompt: "q"})

	// Assert.
	var hErr *Error
	if !errors.As(err, &hErr) || hErr.Cause != CauseNoBinary {
		t.Fatalf("err = %v, want the %s cause", err, CauseNoBinary)
	}
}

func TestRunAnswersWithStdoutForATextRun(t *testing.T) {
	// Arrange.
	c, _ := scripted(t, permissiveGuard(t), "fake-claude", "ROUTE_HOLD\n", nil)

	// Act.
	got, err := c.Run(context.Background(), Request{Site: "classifier", Format: FormatText, Prompt: "q"})

	// Assert.
	if err != nil {
		t.Fatalf("Run() error = %v, want nil", err)
	}
	if got.Text != "ROUTE_HOLD\n" {
		t.Fatalf("Text = %q, want the binary's stdout verbatim", got.Text)
	}
}

func TestRunReadsTheResultOutOfAJSONEnvelope(t *testing.T) {
	// Arrange.
	c, _ := scripted(t, permissiveGuard(t), "fake-claude", `{"type":"result","result":"flaky-login-test"}`, nil)

	// Act.
	got, err := c.Run(context.Background(), Request{Site: "workspace_naming", Format: FormatJSON, Prompt: "q"})

	// Assert.
	if err != nil {
		t.Fatalf("Run() error = %v, want nil", err)
	}
	if got.Text != "flaky-login-test" {
		t.Fatalf("Text = %q, want the envelope's result", got.Text)
	}
}

func TestRunRefusesAnUnparseableEnvelope(t *testing.T) {
	// Arrange.
	c, _ := scripted(t, permissiveGuard(t), "fake-claude", "not json at all", nil)

	// Act.
	_, err := c.Run(context.Background(), Request{Site: "workspace_naming", Format: FormatJSON, Prompt: "q"})

	// Assert.
	var hErr *Error
	if !errors.As(err, &hErr) || hErr.Cause != CauseUnreadableEnvelope {
		t.Fatalf("err = %v, want the %s cause", err, CauseUnreadableEnvelope)
	}
}

func TestRunRefusesAnEnvelopeThatReportsItsOwnError(t *testing.T) {
	// Arrange.
	c, _ := scripted(t, permissiveGuard(t), "fake-claude",
		`{"type":"result","subtype":"error_during_execution","is_error":true,"result":"credit balance too low"}`, nil)

	// Act.
	_, err := c.Run(context.Background(), Request{Site: "workspace_naming", Format: FormatJSON, Prompt: "q"})

	// Assert.
	var hErr *Error
	if !errors.As(err, &hErr) || hErr.Cause != CauseUnreadableEnvelope {
		t.Fatalf("err = %v, want the %s cause", err, CauseUnreadableEnvelope)
	}
	if !strings.Contains(hErr.Detail, "credit balance too low") {
		t.Fatalf("detail = %q, want the cli's own account carried", hErr.Detail)
	}
}

func TestRunReportsAnExitFailureAsAnExitStatus(t *testing.T) {
	// Arrange.
	c, _ := scripted(t, permissiveGuard(t), "fake-claude", "", errors.New("exit status 3"))

	// Act.
	_, err := c.Run(context.Background(), Request{Site: "classifier", Prompt: "q"})

	// Assert.
	var hErr *Error
	if !errors.As(err, &hErr) || hErr.Cause != CauseExitStatus {
		t.Fatalf("err = %v, want the %s cause", err, CauseExitStatus)
	}
}

// TestRunReportsAnExpiredDeadlineAsATimeout pins that the two failures a
// caller must tell apart — the model took too long, and the cli fell over —
// carry different causes, because they have different remedies.
func TestRunReportsAnExpiredDeadlineAsATimeout(t *testing.T) {
	// Arrange: an exec seam that outlives the request's own deadline.
	c, _ := scripted(t, permissiveGuard(t), "fake-claude", "", nil)
	c.exec = func(ctx context.Context, _ string, _, _ []string, _ string) (string, error) {
		<-ctx.Done()
		return "", errors.New("signal: killed")
	}

	// Act.
	_, err := c.Run(context.Background(), Request{Site: "workspace_naming", Prompt: "q", Timeout: time.Millisecond})

	// Assert.
	var hErr *Error
	if !errors.As(err, &hErr) || hErr.Cause != CauseTimeout {
		t.Fatalf("err = %v, want the %s cause", err, CauseTimeout)
	}
}

func TestRunPutsTheModelOnTheArgv(t *testing.T) {
	// Arrange.
	c, seen := scripted(t, permissiveGuard(t), "fake-claude", `{"result":"x"}`, nil)

	// Act.
	if _, err := c.Run(context.Background(), Request{
		Site: "workspace_naming", Model: ModelHaiku, Format: FormatJSON, Prompt: "q",
	}); err != nil {
		t.Fatalf("Run() error = %v, want nil", err)
	}

	// Assert.
	if got, want := strings.Join(seen.Args, " "), "-p --output-format json --model "+ModelHaiku; got != want {
		t.Fatalf("argv = %q, want %q", got, want)
	}
}

func TestRunOmitsTheModelFlagWhenNoModelIsAsked(t *testing.T) {
	// Arrange.
	c, seen := scripted(t, permissiveGuard(t), "fake-claude", "ok", nil)

	// Act.
	if _, err := c.Run(context.Background(), Request{Site: "classifier", Prompt: "q"}); err != nil {
		t.Fatalf("Run() error = %v, want nil", err)
	}

	// Assert.
	if got, want := strings.Join(seen.Args, " "), "-p --output-format text"; got != want {
		t.Fatalf("argv = %q, want %q", got, want)
	}
}

// TestRunPutsThePromptOnStdin pins WHY the exec site exists in this shape: the
// composed question never rides an argv a process listing shows.
func TestRunPutsThePromptOnStdin(t *testing.T) {
	// Arrange.
	c, seen := scripted(t, permissiveGuard(t), "fake-claude", "ok", nil)

	// Act.
	if _, err := c.Run(context.Background(), Request{Site: "classifier", Prompt: "the composed question"}); err != nil {
		t.Fatalf("Run() error = %v, want nil", err)
	}

	// Assert.
	if seen.Stdin != "the composed question" {
		t.Fatalf("stdin = %q, want the composed question", seen.Stdin)
	}
	for _, arg := range seen.Args {
		if strings.Contains(arg, "the composed question") {
			t.Fatalf("argv = %v, want the prompt never to ride an argv", seen.Args)
		}
	}
}

func TestRunExportsTheConfigDir(t *testing.T) {
	// Arrange.
	c, seen := scripted(t, permissiveGuard(t), "fake-claude", "ok", nil)

	// Act.
	if _, err := c.Run(context.Background(), Request{
		Site: "workspace_naming", Prompt: "q", ConfigDir: "/roots/multi",
	}); err != nil {
		t.Fatalf("Run() error = %v, want nil", err)
	}

	// Assert.
	found := false
	for _, entry := range seen.Env {
		if entry == "CLAUDE_CONFIG_DIR=/roots/multi" {
			found = true
		}
	}
	if !found {
		t.Fatal("env carries no CLAUDE_CONFIG_DIR, want the naming call to bill the workspace's account")
	}
}

// TestExecCLIAnswersWithStdout pins the production exec against a scripted
// executable — never the real `claude`.
func TestExecCLIAnswersWithStdout(t *testing.T) {
	// Arrange.
	bin := fakeVendor(t, "printf 'flaky-login-test'")

	// Act.
	got, err := execCLI(context.Background(), bin, []string{"-p"}, os.Environ(), "q")

	// Assert.
	if err != nil {
		t.Fatalf("execCLI() error = %v, want nil", err)
	}
	if got != "flaky-login-test" {
		t.Fatalf("execCLI() = %q, want the binary's stdout", got)
	}
}

func TestExecCLISurfacesAnAbsentBinary(t *testing.T) {
	// Arrange.
	bin := filepath.Join(t.TempDir(), "not-installed")

	// Act.
	_, err := execCLI(context.Background(), bin, []string{"-p"}, os.Environ(), "q")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), bin) {
		t.Fatalf("err = %v, want a refusal naming the absent binary %q", err, bin)
	}
}

func TestExecCLISurfacesANonZeroExitWithItsStderr(t *testing.T) {
	// Arrange.
	bin := fakeVendor(t, "echo 'credit balance is too low' >&2\nexit 3")

	// Act.
	_, err := execCLI(context.Background(), bin, []string{"-p"}, os.Environ(), "q")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "credit balance is too low") {
		t.Fatalf("err = %v, want the binary's stderr carried", err)
	}
}
