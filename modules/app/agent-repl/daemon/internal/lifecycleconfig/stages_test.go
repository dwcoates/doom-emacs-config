package lifecycleconfig

import (
	"errors"
	"path/filepath"
	"testing"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"
)

// builtins is the defaults' checkout in every test.
var builtins = Builtins{CheckoutRoot: "/agent-repl"}

// mustParse is a config document parsed, failing the test when it is refused.
func mustParse(t *testing.T, doc string) *repoconfigv1.LifecycleConfig {
	t.Helper()
	cfg, err := Parse("lifecycle.yaml", []byte(doc))
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	return cfg
}

// hookShape is a hook as a test compares it: a prompt's text, a script's
// path, or "builtin".
func hookShape(h *repoconfigv1.Hook) string {
	switch {
	case h.GetPrompt() != nil:
		return "prompt:" + h.GetPrompt().GetText()
	case h.GetScript() != nil:
		return "script:" + h.GetScript().GetPath()
	case h.GetBuiltin() != nil:
		return "builtin"
	}
	return "none"
}

func shapes(hooks []*repoconfigv1.Hook) []string {
	out := []string{}
	for _, h := range hooks {
		out = append(out, hookShape(h))
	}
	return out
}

func TestHooksRunsEachKindsDefaultForAnUnsetList(t *testing.T) {
	postDeploy := "script:" + filepath.Join("/agent-repl", DefaultPostDeployScript)
	tests := []struct {
		name  string
		stage Stage
		kind  Originator
		want  []string
	}{
		{name: "emoji-ack creation posts that work started", stage: StageCreation, kind: OriginatorEmojiAck, want: []string{"prompt:" + DefaultCreationPrompt}},
		{name: "emoji-ack finish announces the pull request", stage: StageFinish, kind: OriginatorEmojiAck, want: []string{"prompt:" + DefaultFinishPrompt}},
		{name: "emoji-ack post_deploy reacts merged", stage: StagePostDeploy, kind: OriginatorEmojiAck, want: []string{postDeploy}},
		{name: "emoji-ack pre_deploy has no default", stage: StagePreDeploy, kind: OriginatorEmojiAck, want: []string{}},
		{name: "user_or_workspace creation has no default", stage: StageCreation, kind: OriginatorUserOrWorkspace, want: []string{}},
		{name: "user_or_workspace finish has no default", stage: StageFinish, kind: OriginatorUserOrWorkspace, want: []string{}},
		{name: "user_or_workspace post_deploy has no default", stage: StagePostDeploy, kind: OriginatorUserOrWorkspace, want: []string{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a config that sets no stage list at all.
			cfg := mustParse(t, "pre_deploy: {}\n")

			// Act
			got := shapes(builtins.Hooks(cfg, tt.stage, tt.kind, true))

			// Assert
			if len(got) != len(tt.want) || (len(got) > 0 && got[0] != tt.want[0]) {
				t.Fatalf("Hooks = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestHooksWithNoConfigRunsTheDefaults(t *testing.T) {
	// Act
	emoji := shapes(builtins.Hooks(nil, StageCreation, OriginatorEmojiAck, false))
	other := shapes(builtins.Hooks(nil, StageCreation, OriginatorUserOrWorkspace, false))

	// Assert
	if len(emoji) != 1 || emoji[0] != "prompt:"+DefaultCreationPrompt || len(other) != 0 {
		t.Fatalf("Hooks(nil) = %v / %v, want the emoji-ack default alone", emoji, other)
	}
}

func TestHooksRunsOnlyTheWorkspacesOwnKind(t *testing.T) {
	// Arrange
	cfg := mustParse(t, `
pre_deploy: {}
finish:
  emoji_ack:
    hooks:
      - hook: {prompt: {text: open the pull request and say so}}
        runs_for: {any_target: {}}
  user_or_workspace:
    hooks:
      - hook: {prompt: {text: open the pull request}}
        runs_for: {any_target: {}}
`)

	// Act
	emoji := shapes(builtins.Hooks(cfg, StageFinish, OriginatorEmojiAck, true))
	other := shapes(builtins.Hooks(cfg, StageFinish, OriginatorUserOrWorkspace, true))

	// Assert
	if len(emoji) != 1 || emoji[0] != "prompt:open the pull request and say so" {
		t.Errorf("emoji-ack finish = %v, want its own list alone", emoji)
	}
	if len(other) != 1 || other[0] != "prompt:open the pull request" {
		t.Errorf("user_or_workspace finish = %v, want its own list alone", other)
	}
}

func TestHooksAnEmptyListOverridesTheDefault(t *testing.T) {
	// Arrange
	cfg := mustParse(t, "pre_deploy: {}\npost_deploy:\n  emoji_ack:\n    hooks: []\n")

	// Act
	got := builtins.Hooks(cfg, StagePostDeploy, OriginatorEmojiAck, true)

	// Assert
	if len(got) != 0 {
		t.Fatalf("Hooks = %v, want none: an empty list replaces the default", shapes(got))
	}
}

func TestHooksSplicesTheDefaultWhereBuiltinStands(t *testing.T) {
	// Arrange: the repository's own hook, then the default.
	cfg := mustParse(t, `
pre_deploy: {}
finish:
  emoji_ack:
    hooks:
      - hook: {prompt: {text: open the pull request}}
        runs_for: {any_target: {}}
      - hook: {builtin: {}}
        runs_for: {any_target: {}}
`)

	// Act
	got := shapes(builtins.Hooks(cfg, StageFinish, OriginatorEmojiAck, true))

	// Assert
	if len(got) != 2 || got[0] != "prompt:open the pull request" || got[1] != "prompt:"+DefaultFinishPrompt {
		t.Fatalf("Hooks = %v, want the repository's prompt then the default", got)
	}
}

func TestHooksSkipsAnEntryThatDoesNotRunForTheTarget(t *testing.T) {
	// Arrange
	cfg := mustParse(t, `
pre_deploy: {}
creation:
  user_or_workspace:
    hooks:
      - hook: {script: {path: bin/setup.sh, timeout_seconds: 5}}
        runs_for: {main: {}}
      - hook: {prompt: {text: read the notes}}
        runs_for: {any_target: {}}
`)
	tests := []struct {
		name   string
		toMain bool
		want   int
	}{
		{name: "landing on main runs both", toMain: true, want: 2},
		{name: "landing into a parent skips the main-only one", toMain: false, want: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := builtins.Hooks(cfg, StageCreation, OriginatorUserOrWorkspace, tt.toMain)

			// Assert
			if len(got) != tt.want {
				t.Fatalf("Hooks = %v, want %d hooks", shapes(got), tt.want)
			}
		})
	}
}

func TestHooksPanicsForTheDeployStage(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Fatal("Hooks of the deploy stage did not panic")
		}
	}()
	builtins.Hooks(nil, StageDeploy, OriginatorEmojiAck, true)
}

func TestThePostDeployDefaultNeedsACheckout(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Fatal("the post-deploy default with no checkout did not panic")
		}
	}()
	Builtins{}.Hooks(nil, StagePostDeploy, OriginatorEmojiAck, true)
}

func TestDeployHookIsTheKindsOwn(t *testing.T) {
	// Arrange
	cfg := mustParse(t, `
pre_deploy: {}
deploy:
  emoji_ack:
    hook: {prompt: {text: "rebase, enqueue, wait"}}
    runs_for: {main: {}}
`)
	tests := []struct {
		name   string
		kind   Originator
		toMain bool
		want   string
	}{
		{name: "emoji-ack on main", kind: OriginatorEmojiAck, toMain: true, want: "prompt:rebase, enqueue, wait"},
		{name: "emoji-ack into a parent", kind: OriginatorEmojiAck, toMain: false, want: "none"},
		{name: "user_or_workspace, unset", kind: OriginatorUserOrWorkspace, toMain: true, want: "none"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := hookShape(DeployHook(cfg, tt.kind, tt.toMain))

			// Assert
			if got != tt.want {
				t.Fatalf("DeployHook = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestReleaseScript(t *testing.T) {
	tests := []struct {
		name string
		doc  string
		want string
	}{
		{name: "the user_or_workspace script", doc: "pre_deploy: {}\ndeploy:\n  user_or_workspace:\n    hook: {script: {path: a.sh, timeout_seconds: 5}}\n    runs_for: {main: {}}\n  emoji_ack:\n    hook: {script: {path: b.sh, timeout_seconds: 5}}\n    runs_for: {main: {}}\n", want: "a.sh"},
		{name: "else the emoji_ack script", doc: "pre_deploy: {}\ndeploy:\n  user_or_workspace:\n    hook: {prompt: {text: go}}\n    runs_for: {main: {}}\n  emoji_ack:\n    hook: {script: {path: b.sh, timeout_seconds: 5}}\n    runs_for: {main: {}}\n", want: "b.sh"},
		{name: "none when neither is a script", doc: "pre_deploy: {}\ndeploy:\n  user_or_workspace:\n    hook: {prompt: {text: go}}\n    runs_for: {main: {}}\n", want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := ReleaseScript(mustParse(t, tt.doc)).GetPath()

			// Assert
			if got != tt.want {
				t.Fatalf("ReleaseScript = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestOriginatorSpellingsRoundTrip(t *testing.T) {
	for _, kind := range Originators {
		t.Run(kind.String(), func(t *testing.T) {
			// Act
			got, err := ParseOriginator(kind.String())

			// Assert
			if err != nil || got != kind {
				t.Fatalf("ParseOriginator(%q) = %v, %v", kind.String(), got, err)
			}
		})
	}
}

func TestParseOriginatorRefusesAnUnknownSpelling(t *testing.T) {
	// Act
	_, err := ParseOriginator("emoji")

	// Assert
	if err == nil {
		t.Fatal("ParseOriginator(\"emoji\") succeeded, want a refusal")
	}
}

func TestTheZeroOriginatorIsNoKind(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Fatal("the zero originator spelled itself")
		}
	}()
	_ = Originator(0).String()
}

func TestStageKeys(t *testing.T) {
	want := []string{"creation", "finish", "pre_deploy", "deploy", "post_deploy"}
	for i, stage := range Stages {
		if stage.Key() != want[i] {
			t.Errorf("stage %d Key = %q, want %q", i, stage.Key(), want[i])
		}
	}
}

func TestEveryDefaultRunsForAnEmojiAckWorkspaceWithAThread(t *testing.T) {
	// Every default prompt names the thread, which only a workspace with a
	// Slack origin can resolve: an emoji-ack workspace always has one.
	for _, text := range []string{DefaultCreationPrompt, DefaultFinishPrompt} {
		if _, err := ExpandPrompt(text, ""); !errors.Is(err, ErrNoSlackThread) {
			t.Errorf("ExpandPrompt(%q, \"\") = %v, want ErrNoSlackThread: the default names the thread", text, err)
		}
	}
}
