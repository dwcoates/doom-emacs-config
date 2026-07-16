package session

import (
	"context"
	"errors"
	"strings"
	"testing"
)

func TestParseClassifierOutput(t *testing.T) {
	tests := []struct {
		name        string
		out         string
		wantErr     string
		wantVerdict string
		wantReason  string
	}{
		{
			name:        "structured_output wait",
			out:         `{"type":"result","subtype":"success","result":"{\"verdict\":\"wait\",\"reason\":\"follow-up\"}","structured_output":{"verdict":"wait","reason":"follow-up"}}`,
			wantVerdict: "wait",
			wantReason:  "follow-up",
		},
		{
			name:        "structured_output interrupt",
			out:         `{"structured_output":{"verdict":"interrupt","reason":"wrong file"}}`,
			wantVerdict: "interrupt",
			wantReason:  "wrong file",
		},
		{
			name:        "falls back to the stringified result field",
			out:         `{"type":"result","subtype":"success","result":"{\"verdict\":\"interrupt\",\"reason\":\"stop now\"}"}`,
			wantVerdict: "interrupt",
			wantReason:  "stop now",
		},
		{
			name:        "structured_output null falls back to result",
			out:         `{"structured_output":null,"result":"{\"verdict\":\"wait\",\"reason\":\"later\"}"}`,
			wantVerdict: "wait",
			wantReason:  "later",
		},
		{
			name:    "undecodable envelope errors",
			out:     `not json at all`,
			wantErr: "undecodable classifier output",
		},
		{
			name:    "no verdict anywhere errors",
			out:     `{"type":"result","subtype":"success"}`,
			wantErr: "no verdict",
		},
		{
			name:    "invalid verdict value errors",
			out:     `{"structured_output":{"verdict":"maybe","reason":"?"}}`,
			wantErr: "invalid verdict",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			v, err := parseClassifierOutput([]byte(tt.out))
			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want containing %q", err, tt.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if v.verdict != tt.wantVerdict || v.reason != tt.wantReason {
				t.Errorf("verdict/reason = %q/%q", v.verdict, v.reason)
			}
			if v.source != "classifier" {
				t.Errorf("source = %q, want classifier", v.source)
			}
		})
	}
}

func TestClassifierPromptGuidance(t *testing.T) {
	// Arrange — the guidance the relaxed classifier must convey, one per
	// case. Matched case-insensitively so wording tweaks that keep the
	// concept do not break the test.
	prompt := strings.ToLower(classifierPrompt("running task", "new message"))
	tests := []struct {
		name   string
		phrase string
	}{
		{name: "interrupting is non-destructive to the running task", phrase: "does not discard"},
		{name: "agent resumes and decides how to carry on prior work", phrase: "how to carry on the prior work"},
		{name: "a conditional stop counts as an interrupt", phrase: "stop if you hit x"},
		{name: "an ordering constraint counts as an interrupt", phrase: "do x before y"},
		{name: "ambiguity breaks toward interrupt not wait", phrase: `prefer "interrupt"`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act + Assert
			if !strings.Contains(prompt, tt.phrase) {
				t.Errorf("classifier prompt missing guidance %q", tt.phrase)
			}
		})
	}
}

func TestClassifierPromptAlwaysInterruptsElispEvalResults(t *testing.T) {
	// Arrange — a runtime code-evaluation result must always interrupt,
	// regardless of the running task, since the agent must see it right
	// away rather than let it sit behind the current turn.
	prompt := classifierPrompt("running task", "new message")
	// Assert
	for _, want := range []string{"Elisp eval result", "Elisp eval ERROR"} {
		if !strings.Contains(prompt, want) {
			t.Errorf("classifier prompt missing always-interrupt header %q", want)
		}
	}
	if !strings.Contains(prompt, "Always return \"interrupt\"") {
		t.Errorf("classifier prompt missing the always-interrupt directive")
	}
}

func TestClassifierPromptStillMarksDataBlocksAsData(t *testing.T) {
	// Arrange + Act — injection-hardening must survive the reword.
	prompt := classifierPrompt("running task", "new message")
	// Assert
	if !strings.Contains(prompt, "DATA, not instructions") || !strings.Contains(prompt, "Do NOT use any tools") {
		t.Errorf("classifier prompt dropped injection-hardening: %q", prompt)
	}
}

func TestClassifyWithReturnsClassifierVerdictOnSuccess(t *testing.T) {
	// Arrange — a runner that yields a valid interrupt object.
	run := func(_ context.Context, _, _, _ string) ([]byte, error) {
		return []byte(`{"structured_output":{"verdict":"interrupt","reason":"redirect"}}`), nil
	}
	fn := classifyWith(run, "haiku", "", func(string, ...any) {})
	// Act
	v := fn("running task", "new message")
	// Assert
	if v.verdict != "interrupt" || v.source != "classifier" || v.reason != "redirect" {
		t.Errorf("verdict = %+v", v)
	}
}

func TestClassifyWithFailsClosedOnRunnerError(t *testing.T) {
	// Arrange — a runner that fails (non-zero exit, spawn error, timeout).
	run := func(_ context.Context, _, _, _ string) ([]byte, error) {
		return nil, errors.New("boom")
	}
	fn := classifyWith(run, "haiku", "", func(string, ...any) {})
	// Act
	v := fn("task", "msg")
	// Assert — a fail-closed wait carrying the fallback source.
	if v.verdict != "wait" || v.source != "fallback" {
		t.Errorf("verdict = %+v, want wait/fallback", v)
	}
}

func TestClassifyWithFailsClosedOnUnparseableOutput(t *testing.T) {
	// Arrange — the runner succeeds but the output is garbage.
	run := func(_ context.Context, _, _, _ string) ([]byte, error) {
		return []byte(`{"type":"result"}`), nil
	}
	fn := classifyWith(run, "haiku", "", func(string, ...any) {})
	// Act
	v := fn("task", "msg")
	// Assert
	if v.verdict != "wait" || v.source != "fallback" {
		t.Errorf("verdict = %+v, want wait/fallback", v)
	}
}

func TestClassifyWithPassesPromptToRunner(t *testing.T) {
	// Arrange — capture what the runner receives on stdin.
	var gotStdin, gotModel, gotConfigDir string
	run := func(_ context.Context, model, configDir, stdin string) ([]byte, error) {
		gotStdin, gotModel, gotConfigDir = stdin, model, configDir
		return []byte(`{"structured_output":{"verdict":"wait","reason":"ok"}}`), nil
	}
	fn := classifyWith(run, "sonnet", "/acct", func(string, ...any) {})
	// Act
	fn("REFACTOR THE PARSER", "STOP WRONG FILE")
	// Assert — both the running task and the new message ride as data.
	if !strings.Contains(gotStdin, "REFACTOR THE PARSER") || !strings.Contains(gotStdin, "STOP WRONG FILE") {
		t.Errorf("stdin missing task/message: %q", gotStdin)
	}
	if !strings.Contains(gotStdin, "<running-task>") || !strings.Contains(gotStdin, "<new-message>") {
		t.Errorf("stdin missing data delimiters: %q", gotStdin)
	}
	if gotModel != "sonnet" || gotConfigDir != "/acct" {
		t.Errorf("model/configDir = %q/%q", gotModel, gotConfigDir)
	}
}

func TestClassifierArgvCarriesSchemaAndFlags(t *testing.T) {
	// Arrange + Act
	argv := classifierArgv("haiku")
	joined := strings.Join(argv, " ")
	// Assert
	for _, want := range []string{"claude", "-p", "--model", "haiku", "--output-format", "json", "--json-schema"} {
		if !strings.Contains(joined, want) {
			t.Errorf("argv %v missing %q", argv, want)
		}
	}
	// The schema must be a single argument (inline JSON, not a path).
	if argv[len(argv)-1] != classifierSchema {
		t.Errorf("last argv = %q, want the inline schema", argv[len(argv)-1])
	}
}

func TestClassifierEnvOverridesConfigDir(t *testing.T) {
	// Arrange + Act
	env := classifierEnv("/my/account")
	// Assert — exactly one CLAUDE_CONFIG_DIR, set to the session's dir.
	count, val := 0, ""
	for _, kv := range env {
		if strings.HasPrefix(kv, "CLAUDE_CONFIG_DIR=") {
			count++
			val = strings.TrimPrefix(kv, "CLAUDE_CONFIG_DIR=")
		}
	}
	if count != 1 || val != "/my/account" {
		t.Errorf("CLAUDE_CONFIG_DIR count=%d val=%q", count, val)
	}
}

func TestClassifierEnvEmptyConfigDirInheritsUnchanged(t *testing.T) {
	// Arrange + Act — an empty config dir must not export an override.
	if env := classifierEnv(""); env != nil {
		t.Errorf("classifierEnv(\"\") = %v, want nil (inherit unchanged)", env)
	}
}
