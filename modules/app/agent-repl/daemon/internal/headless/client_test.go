package headless

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/envc"
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

func TestEnvelopeAccountWordsItsSubtypeAndTrimmedResult(t *testing.T) {
	// Arrange.
	env := envelope{Subtype: "error_during_execution", Result: "  credit balance too low\n", IsError: true}

	// Act.
	got := env.account()

	// Assert.
	if want := "(error_during_execution): credit balance too low"; got != want {
		t.Fatalf("account() = %q, want %q", got, want)
	}
}

// TestEveryEnvelopeErrorIsWordedByAccount pins that the two sites reporting an
// error envelope share one wording: a hand-rolled site would drift from it.
func TestEveryEnvelopeErrorIsWordedByAccount(t *testing.T) {
	// Arrange.
	env := envelope{Subtype: "error_max_turns", Result: "ran out", IsError: true}
	raw := `{"subtype":"error_max_turns","result":"ran out","is_error":true}`

	// Act.
	_, parsed := resultOf(raw)
	exited := exitAccount(FormatJSON, raw)

	// Assert.
	if parsed == nil || !strings.HasSuffix(parsed.Error(), env.account()) {
		t.Fatalf("resultOf error = %v, want it to end with %q", parsed, env.account())
	}
	if !strings.HasSuffix(exited, env.account()) {
		t.Fatalf("exitAccount = %q, want it to end with %q", exited, env.account())
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

func TestRunCarriesWhatAFailedRunSaidOnStdout(t *testing.T) {
	tests := []struct {
		name   string
		format string
		stdout string
		want   string
	}{
		{
			name:   "a json run's error envelope is read for its own account",
			format: FormatJSON,
			stdout: `{"type":"result","subtype":"error_during_execution","is_error":true,"result":"Failed to authenticate: OAuth session expired"}` + "\n",
			want:   "exit status 1; the cli said (error_during_execution): Failed to authenticate: OAuth session expired",
		},
		{
			name:   "a json run whose stdout is no envelope is carried as written",
			format: FormatJSON,
			stdout: "  not an envelope\n",
			want:   "exit status 1; the cli said: not an envelope",
		},
		{
			name:   "a json envelope reporting no error is carried as written",
			format: FormatJSON,
			stdout: `{"is_error":false,"result":"half"}`,
			want:   `exit status 1; the cli said: {"is_error":false,"result":"half"}`,
		},
		{
			name:   "a text run's stdout is carried as written",
			format: FormatText,
			stdout: "Invalid API key\n",
			want:   "exit status 1; the cli said: Invalid API key",
		},
		{
			name:   "a run that said nothing adds nothing",
			format: FormatJSON,
			stdout: " \n",
			want:   "exit status 1",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			c, _ := scripted(t, permissiveGuard(t), "fake-claude", tt.stdout, errors.New("exit status 1"))

			// Act.
			_, err := c.Run(context.Background(), Request{Site: "workspace_naming", Format: tt.format, Prompt: "q"})

			// Assert.
			var hErr *Error
			if !errors.As(err, &hErr) || hErr.Cause != CauseExitStatus {
				t.Fatalf("err = %v, want the %s cause", err, CauseExitStatus)
			}
			if hErr.Detail != tt.want {
				t.Fatalf("detail = %q, want %q", hErr.Detail, tt.want)
			}
		})
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

// wantPinned is the pinned tail spelled out literally, so a change to the one
// definition in client.go is a visible change here too.
const wantPinned = `--settings {"alwaysThinkingEnabled":false} --strict-mcp-config --safe-mode`

func TestRunComposesTheExactArgv(t *testing.T) {
	tests := []struct {
		name string
		req  Request
		out  string
		want string
	}{
		{
			name: "a json run with a model",
			req:  Request{Site: "workspace_naming", Model: ModelHaiku, Format: FormatJSON, Prompt: "q"},
			out:  `{"result":"x"}`,
			want: "-p --output-format json --model " + ModelHaiku + " " + wantPinned,
		},
		{
			name: "a text run with no model omits the model flag",
			req:  Request{Site: "classifier", Prompt: "q"},
			out:  "ok",
			want: "-p --output-format text " + wantPinned,
		},
		{
			name: "a run with tools makes exactly those available and approved",
			req:  Request{Site: "news_digest_ipo", Model: ModelSonnet, Format: FormatJSON, Prompt: "q", Tools: []string{"WebSearch", "WebFetch"}},
			out:  `{"result":"x"}`,
			want: "-p --output-format json --model " + ModelSonnet + " --tools WebSearch,WebFetch --allowedTools WebSearch,WebFetch " + wantPinned,
		},
		{
			name: "a plain run has no tools and an empty system prompt",
			req:  Request{Site: "turn_summary", Model: ModelHaiku, Prompt: "q", Plain: true},
			out:  "ok",
			want: "-p --output-format text --model " + ModelHaiku + " --tools  --system-prompt  " + wantPinned,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			c, seen := scripted(t, permissiveGuard(t), "fake-claude", tt.out, nil)

			// Act.
			if _, err := c.Run(context.Background(), tt.req); err != nil {
				t.Fatalf("Run() error = %v, want nil", err)
			}

			// Assert.
			if got := strings.Join(seen.Args, " "); got != tt.want {
				t.Fatalf("argv = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestRunPinsItsSettingsWhateverTheRequestSays pins that no Request field can
// drop or alter the call-owned settings: the user's interactive config never
// reaches a headless call's behavior.
func TestRunPinsItsSettingsWhateverTheRequestSays(t *testing.T) {
	tests := []struct {
		name string
		req  Request
	}{
		{name: "the bare minimum", req: Request{Site: "classifier", Prompt: "q"}},
		{name: "a model and a text format", req: Request{Site: "classifier", Model: "sonnet", Format: FormatText, Prompt: "q"}},
		{name: "a config dir", req: Request{Site: "workspace_naming", ConfigDir: "/roots/multi", Prompt: "q"}},
		{name: "a timeout", req: Request{Site: "title_synth", Timeout: time.Minute, Prompt: "q"}},
		{name: "tools", req: Request{Site: "news_digest_ipo", Tools: []string{"WebSearch"}, Prompt: "q"}},
		{name: "plain", req: Request{Site: "turn_summary", Plain: true, Prompt: "q"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			c, seen := scripted(t, permissiveGuard(t), "fake-claude", "ok", nil)

			// Act.
			if _, err := c.Run(context.Background(), tt.req); err != nil {
				t.Fatalf("Run() error = %v, want nil", err)
			}

			// Assert.
			if got := strings.Join(seen.Args, " "); !strings.HasSuffix(got, " "+wantPinned) {
				t.Fatalf("argv = %q, want it to end with the pinned %q", got, wantPinned)
			}
		})
	}
}

// TestRunPanicsOnAPlainRunNamingTools pins the invariant that bare inference
// and a tool set are exclusive: a caller asking for both is a bug.
func TestRunPanicsOnAPlainRunNamingTools(t *testing.T) {
	// Arrange.
	c, seen := scripted(t, permissiveGuard(t), "fake-claude", "ok", nil)
	req := Request{Site: "turn_summary", Plain: true, Tools: []string{"WebSearch"}, Prompt: "q"}

	// Act.
	var recovered any
	func() {
		defer func() { recovered = recover() }()
		_, _ = c.Run(context.Background(), req)
	}()

	// Assert.
	if recovered == nil {
		t.Fatal("Run() did not panic on a Plain run naming tools")
	}
	if seen.Args != nil {
		t.Fatalf("argv = %q, want no spawn", seen.Args)
	}
}

// TestPinnedSettingsTurnThinkingOff decodes the `--settings` value rather than
// matching its spelling, so the assertion is on what the CLI will read.
func TestPinnedSettingsTurnThinkingOff(t *testing.T) {
	// Arrange.
	var raw string
	for i, arg := range pinnedArgs {
		if arg == "--settings" && i+1 < len(pinnedArgs) {
			raw = pinnedArgs[i+1]
		}
	}
	if raw == "" {
		t.Fatalf("pinnedArgs = %v, want a --settings value", pinnedArgs)
	}

	// Act.
	var got map[string]any
	err := json.Unmarshal([]byte(raw), &got)

	// Assert.
	if err != nil {
		t.Fatalf("the pinned settings %q did not decode: %v", raw, err)
	}
	if thinking, ok := got["alwaysThinkingEnabled"].(bool); !ok || thinking {
		t.Fatalf("alwaysThinkingEnabled = %v, want an explicit false", got["alwaysThinkingEnabled"])
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

func TestExecCLIAnswersAFailedRunsStdout(t *testing.T) {
	// Arrange: the CLI states a json run's failure on stdout, then exits.
	bin := fakeVendor(t, "printf '{\"is_error\":true}'\nexit 1")

	// Act.
	got, err := execCLI(context.Background(), bin, []string{"-p"}, os.Environ(), "q")

	// Assert.
	if err == nil {
		t.Fatal("execCLI() error = nil, want the non-zero exit")
	}
	if got != `{"is_error":true}` {
		t.Fatalf("execCLI() stdout = %q, want what the failed run wrote", got)
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

func TestExecCLISurfacesAnOutputThatCannotBeCaptured(t *testing.T) {
	// Arrange: no file for the run's output can be made.
	bin := fakeVendor(t, "printf 'never'")
	t.Setenv("TMPDIR", filepath.Join(t.TempDir(), "absent"))

	// Act.
	_, err := execCLI(context.Background(), bin, []string{"-p"}, os.Environ(), "q")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), bin) || !strings.Contains(err.Error(), "make the child's stdout file") {
		t.Fatalf("err = %v, want a refusal naming %q and the stdout file", err, bin)
	}
}
