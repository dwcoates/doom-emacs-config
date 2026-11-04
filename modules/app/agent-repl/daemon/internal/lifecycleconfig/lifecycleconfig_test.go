package lifecycleconfig

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"google.golang.org/protobuf/proto"
)

// agentRepl is agent-repl's own config as its repository commits it.
const agentRepl = `
pre_deploy:
  local_merge:
    tests:
      agent_repl_suites:
        script_path: modules/app/agent-repl/bin/test-all.sh
    version_tag: {}
deploy:
  user_or_workspace:
    hook:
      script:
        path: modules/app/agent-repl/bin/deploy-landing.sh
        timeout_seconds: 60
    runs_for:
      main: {}
`

func TestParseAcceptsAgentReplsConfig(t *testing.T) {
	// Act
	cfg, err := Parse("lifecycle.yaml", []byte(agentRepl))

	// Assert
	if err != nil {
		t.Fatalf("Parse = %v, want agent-repl's config accepted", err)
	}
	lm := cfg.GetPreDeploy().GetLocalMerge()
	if got := lm.GetTests().GetAgentReplSuites().GetScriptPath(); got != "modules/app/agent-repl/bin/test-all.sh" {
		t.Errorf("tests script = %q", got)
	}
	if lm.GetVersionTag() == nil {
		t.Error("version_tag unset, want the repository versioned")
	}
	if got := cfg.GetDeploy().GetUserOrWorkspace().GetHook().GetScript().GetTimeoutSeconds(); got != 60 {
		t.Errorf("deploy timeout = %d, want 60", got)
	}
	if cfg.PostDeploy != nil {
		t.Errorf("post_deploy = %v, want unset", cfg.GetPostDeploy())
	}
}

func TestParseAcceptsHookOnlyConfigs(t *testing.T) {
	// Arrange: a repository with no local merge, a prompt deploy and a script
	// post-deploy hook.
	doc := `
pre_deploy: {}
deploy:
  user_or_workspace:
    hook:
      prompt:
        text: open the pull request
    runs_for:
      any_target: {}
post_deploy:
  user_or_workspace:
    hooks:
      - hook:
          script:
            path: scripts/pull.sh
            timeout_seconds: 30
        runs_for:
          main: {}
`
	// Act
	cfg, err := Parse("lifecycle.yaml", []byte(doc))

	// Assert
	if err != nil {
		t.Fatalf("Parse = %v", err)
	}
	if cfg.GetPreDeploy().GetLocalMerge() != nil {
		t.Error("local_merge set, want none")
	}
	if cfg.GetDeploy().GetUserOrWorkspace().GetHook().GetPrompt().GetText() != "open the pull request" {
		t.Errorf("deploy prompt = %v", cfg.GetDeploy())
	}
	post := cfg.GetPostDeploy().GetUserOrWorkspace().GetHooks()
	if len(post) != 1 || post[0].GetHook().GetScript().GetPath() != "scripts/pull.sh" {
		t.Errorf("post_deploy = %v", cfg.GetPostDeploy())
	}
}

func TestParseAcceptsQueueDrainedScriptsInOrder(t *testing.T) {
	// Arrange
	doc := `
pre_deploy: {}
queue_drained:
  - path: bin/push-release.sh
    timeout_seconds: 120
  - path: bin/announce.sh
    timeout_seconds: 5
`
	// Act
	cfg, err := Parse("lifecycle.yaml", []byte(doc))

	// Assert
	if err != nil {
		t.Fatalf("Parse = %v", err)
	}
	got := cfg.GetQueueDrained()
	if len(got) != 2 || got[0].GetPath() != "bin/push-release.sh" || got[0].GetTimeoutSeconds() != 120 || got[1].GetPath() != "bin/announce.sh" {
		t.Fatalf("queue_drained = %v, want push-release.sh then announce.sh", got)
	}
}

func TestParseRefusesUnusableConfigs(t *testing.T) {
	tests := []struct {
		name string
		doc  string
		want string
	}{
		{name: "an empty file", doc: "", want: "empty"},
		{name: "YAML that does not parse", doc: "pre_deploy: [", want: "yaml"},
		{name: "an unknown key", doc: "pre_deploy: {}\nmerge_before: x\n", want: "merge_before"},
		{name: "no pre_deploy", doc: "post_deploy: {}\n", want: "pre_deploy is required"},
		{name: "a local merge with no test kind", doc: "pre_deploy:\n  local_merge:\n    tests: {}\n", want: "names no test kind"},
		{name: "a test kind with no script", doc: "pre_deploy:\n  local_merge:\n    tests:\n      agent_repl_suites: {}\n", want: "script_path is required"},
		{name: "a hook entry with no hook", doc: "pre_deploy: {}\ndeploy:\n  emoji_ack:\n    runs_for:\n      main: {}\n", want: "deploy.emoji_ack.hook is required"},
		{name: "a hook entry with no scope", doc: "pre_deploy: {}\ndeploy:\n  user_or_workspace:\n    hook:\n      prompt:\n        text: go\n", want: "deploy.user_or_workspace.runs_for is required"},
		{name: "a script with no path", doc: "pre_deploy:\n  user_or_workspace:\n    hooks:\n      - hook:\n          script:\n            timeout_seconds: 5\n        runs_for:\n          main: {}\n", want: "pre_deploy.user_or_workspace.hooks[0].hook.script.path is required"},
		{name: "an absolute script path", doc: "pre_deploy:\n  emoji_ack:\n    hooks:\n      - hook:\n          script:\n            path: /bin/x\n            timeout_seconds: 5\n        runs_for:\n          main: {}\n", want: "must be relative"},
		{name: "a script with no timeout", doc: "pre_deploy: {}\npost_deploy:\n  user_or_workspace:\n    hooks:\n      - hook:\n          script:\n            path: x.sh\n        runs_for:\n          main: {}\n", want: "post_deploy.user_or_workspace.hooks[0].hook.script.timeout_seconds is required"},
		{name: "a blank prompt", doc: "pre_deploy: {}\nfinish:\n  user_or_workspace:\n    hooks:\n      - hook:\n          prompt:\n            text: \"  \"\n        runs_for:\n          any_target: {}\n", want: "never blank"},
		{name: "a creation hook with no scope", doc: "pre_deploy: {}\ncreation:\n  emoji_ack:\n    hooks:\n      - hook:\n          prompt:\n            text: go\n", want: "creation.emoji_ack.hooks[0].runs_for is required"},
		{name: "an unknown placeholder", doc: "pre_deploy: {}\ncreation:\n  emoji_ack:\n    hooks:\n      - hook:\n          prompt:\n            text: \"reply in {{slack_threads}}\"\n        runs_for:\n          any_target: {}\n", want: "{{slack_threads}}"},
		{name: "a builtin in a user_or_workspace list", doc: "pre_deploy: {}\nfinish:\n  user_or_workspace:\n    hooks:\n      - hook:\n          builtin: {}\n        runs_for:\n          any_target: {}\n", want: "finish.user_or_workspace.hooks[0].hook.builtin stands where there is no built-in default"},
		{name: "a builtin in a stage with no default", doc: "pre_deploy:\n  emoji_ack:\n    hooks:\n      - hook:\n          builtin: {}\n        runs_for:\n          any_target: {}\n", want: "pre_deploy.emoji_ack.hooks[0].hook.builtin stands where there is no built-in default"},
		{name: "a builtin as a deploy hook", doc: "pre_deploy: {}\ndeploy:\n  emoji_ack:\n    hook:\n      builtin: {}\n    runs_for:\n      main: {}\n", want: "deploy.emoji_ack.hook.builtin stands where there is no built-in default"},
		{name: "the retired one-list post_deploy", doc: "pre_deploy: {}\npost_deploy:\n  - hook:\n      prompt:\n        text: go\n    runs_for:\n      main: {}\n", want: "unexpected token ["},
		{name: "concurrent naming no stage", doc: "pre_deploy: {}\nconcurrent: {}\n", want: "concurrent names no stage"},
		{name: "concurrent with a local merge", doc: "pre_deploy:\n  local_merge:\n    tests:\n      agent_repl_suites:\n        script_path: t.sh\nconcurrent:\n  through_pre_deploy: {}\n", want: "cannot be set with pre_deploy.local_merge"},
		{name: "a queue_drained script with no path", doc: "pre_deploy: {}\nqueue_drained:\n  - timeout_seconds: 5\n", want: "queue_drained[0].path is required"},
		{name: "an absolute queue_drained path", doc: "pre_deploy: {}\nqueue_drained:\n  - path: /bin/x\n    timeout_seconds: 5\n", want: "queue_drained[0].path \"/bin/x\" must be relative"},
		{name: "a queue_drained script with no timeout", doc: "pre_deploy: {}\nqueue_drained:\n  - path: x.sh\n", want: "queue_drained[0].timeout_seconds is required"},
		{name: "a queue_drained prompt", doc: "pre_deploy: {}\nqueue_drained:\n  - prompt:\n      text: release\n", want: "prompt"},
		{name: "concurrent through a stage the schema does not name", doc: "pre_deploy: {}\nconcurrent:\n  through_post_deploy: {}\n", want: "through_post_deploy"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := Parse("lifecycle.yaml", []byte(tt.doc))

			// Assert
			var invalid *InvalidError
			if !errors.As(err, &invalid) {
				t.Fatalf("Parse = %v, want an InvalidError", err)
			}
			if invalid.Path != "lifecycle.yaml" || !strings.Contains(strings.ToLower(invalid.Detail), strings.ToLower(tt.want)) {
				t.Errorf("InvalidError = %+v, want its detail to name %q", invalid, tt.want)
			}
		})
	}
}

func TestOnDiskLoadsTheRepositorysFile(t *testing.T) {
	// Arrange
	root := t.TempDir()
	if err := os.MkdirAll(filepath.Join(root, ".agent-repl"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(Path(root), []byte(agentRepl), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	cfg, err := OnDisk{}.Load(root)

	// Assert
	if err != nil || cfg.GetPreDeploy().GetLocalMerge() == nil {
		t.Fatalf("Load = %v, %v; want the committed config", cfg, err)
	}
}

func TestOnDiskReportsAMissingFile(t *testing.T) {
	// Arrange
	root := t.TempDir()

	// Act
	_, err := OnDisk{}.Load(root)

	// Assert
	var missing *MissingError
	if !errors.As(err, &missing) || missing.Path != Path(root) {
		t.Fatalf("Load = %v, want a MissingError naming %s", err, Path(root))
	}
}

func TestRunsFor(t *testing.T) {
	main := &repoconfigv1.HookEntry{RunsFor: &repoconfigv1.HookRunsFor{Scope: &repoconfigv1.HookRunsFor_Main{Main: &repoconfigv1.HookRunsForMain{}}}}
	any := &repoconfigv1.HookEntry{RunsFor: &repoconfigv1.HookRunsFor{Scope: &repoconfigv1.HookRunsFor_AnyTarget{AnyTarget: &repoconfigv1.HookRunsForAnyTarget{}}}}
	tests := []struct {
		name   string
		entry  *repoconfigv1.HookEntry
		toMain bool
		want   bool
	}{
		{name: "main, landing on main", entry: main, toMain: true, want: true},
		{name: "main, landing in a parent", entry: main, toMain: false, want: false},
		{name: "any target, landing on main", entry: any, toMain: true, want: true},
		{name: "any target, landing in a parent", entry: any, toMain: false, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := RunsFor(tt.entry, tt.toMain); got != tt.want {
				t.Errorf("RunsFor = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestRunsForPanicsOnAnUncheckedEntry(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Fatal("RunsFor on an entry with no scope did not panic")
		}
	}()
	RunsFor(&repoconfigv1.HookEntry{}, true)
}

// repoRoot is the repository this package is checked out in: agent-repl's own.
func repoRoot(t *testing.T) string {
	t.Helper()
	root, err := filepath.Abs(filepath.Join("..", "..", "..", "..", "..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	return root
}

func TestParseAcceptsConcurrentStages(t *testing.T) {
	// Arrange: explanation-engine's shape, a pull request in the deploy stage
	// run concurrently and the main checkout's update one at a time.
	doc := `
pre_deploy: {}
concurrent:
  through_deploy: {}
deploy:
  user_or_workspace:
    hook:
      prompt:
        text: open the pull request and wait for it to merge
    runs_for:
      main: {}
post_deploy:
  user_or_workspace:
    hooks:
      - hook:
          script:
            path: .agent-repl/hooks/update-master-from-origin.sh
            timeout_seconds: 120
        runs_for:
          main: {}
`
	// Act
	cfg, err := Parse("lifecycle.yaml", []byte(doc))

	// Assert
	if err != nil {
		t.Fatalf("Parse = %v", err)
	}
	if cfg.GetConcurrent().GetThroughDeploy() == nil {
		t.Fatalf("concurrent = %v, want through_deploy", cfg.GetConcurrent())
	}
}

func TestConcurrent(t *testing.T) {
	through := func(arm string) *repoconfigv1.LifecycleConfig {
		cfg := &repoconfigv1.LifecycleConfig{PreDeploy: &repoconfigv1.DeployPreDeployStage{}}
		switch arm {
		case "pre_deploy":
			cfg.Concurrent = &repoconfigv1.DeployConcurrentStages{Through: &repoconfigv1.DeployConcurrentStages_ThroughPreDeploy{ThroughPreDeploy: &repoconfigv1.DeployConcurrentThroughPreDeploy{}}}
		case "deploy":
			cfg.Concurrent = &repoconfigv1.DeployConcurrentStages{Through: &repoconfigv1.DeployConcurrentStages_ThroughDeploy{ThroughDeploy: &repoconfigv1.DeployConcurrentThroughDeploy{}}}
		}
		return cfg
	}
	tests := []struct {
		name    string
		through string
		want    [3]bool
		any     bool
	}{
		{name: "unset: every stage one at a time", through: "", want: [3]bool{false, false, false}, any: false},
		{name: "through pre_deploy", through: "pre_deploy", want: [3]bool{true, false, false}, any: true},
		{name: "through deploy", through: "deploy", want: [3]bool{true, true, false}, any: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			cfg := through(tt.through)

			// Act
			got := [3]bool{Concurrent(cfg, StagePreDeploy), Concurrent(cfg, StageDeploy), Concurrent(cfg, StagePostDeploy)}

			// Assert
			if got != tt.want || AnyConcurrent(cfg) != tt.any {
				t.Fatalf("concurrent = %v (any %v), want %v (any %v)", got, AnyConcurrent(cfg), tt.want, tt.any)
			}
		})
	}
}

// TestAgentReplsOwnConfigIsTodaysPipeline pins agent-repl's committed
// `.agent-repl/lifecycle.yaml` to exactly the steps the queue ran for its own
// repository before deploys were configured: rebase, agent-repl's suites, the
// merge commit and the version tag, then the runtime's redeploy on landings
// into master, for both kinds of workspace; once the queue fully depletes,
// the release push. An emoji-ack workspace's finish announces nothing (no
// pull request is opened) and its thread is told the work landed.
func TestAgentReplsOwnConfigIsTodaysPipeline(t *testing.T) {
	// Arrange
	root := repoRoot(t)

	// Act
	cfg, err := OnDisk{}.Load(root)

	// Assert
	if err != nil {
		t.Fatalf("Load(%s): %v", root, err)
	}
	lm := cfg.GetPreDeploy().GetLocalMerge()
	script := lm.GetTests().GetAgentReplSuites().GetScriptPath()
	if script != "modules/app/agent-repl/bin/test-all.sh" || lm.GetVersionTag() == nil {
		t.Fatalf("local merge = %v, want agent-repl's suites and a version tag", lm)
	}
	if _, err := os.Stat(filepath.Join(root, script)); err != nil {
		t.Fatalf("the test script %s: %v", script, err)
	}
	if cfg.Concurrent != nil {
		t.Fatalf("concurrent = %v, want unset: every stage of agent-repl's deploy runs one at a time", cfg.GetConcurrent())
	}
	builtins := Builtins{CheckoutRoot: "/agent-repl"}
	for _, kind := range Originators {
		if hooks := builtins.Hooks(cfg, StagePreDeploy, kind, true); len(hooks) != 0 {
			t.Fatalf("%s pre_deploy hooks = %v, want none", kind, hooks)
		}
		if hooks := builtins.Hooks(cfg, StageCreation, OriginatorUserOrWorkspace, true); len(hooks) != 0 {
			t.Fatalf("user_or_workspace creation hooks = %v, want none", hooks)
		}
	}
	if hooks := builtins.Hooks(cfg, StageFinish, OriginatorEmojiAck, true); len(hooks) != 0 {
		t.Fatalf("emoji_ack finish hooks = %v, want none: agent-repl opens no pull request to announce", hooks)
	}
	post := builtins.Hooks(cfg, StagePostDeploy, OriginatorEmojiAck, true)
	if len(post) != 1 || !strings.Contains(post[0].GetPrompt().GetText(), SlackThreadPlaceholder) {
		t.Fatalf("emoji_ack post_deploy hooks = %v, want one prompt telling the thread the work landed", post)
	}
	if hooks := builtins.Hooks(cfg, StagePostDeploy, OriginatorUserOrWorkspace, true); len(hooks) != 0 {
		t.Fatalf("user_or_workspace post_deploy hooks = %v, want none", hooks)
	}
	deploy := cfg.GetDeploy().GetUserOrWorkspace()
	if deploy.GetRunsFor().GetMain() == nil {
		t.Fatalf("deploy runs for %v, want main alone: only a landing in the main checkout redeploys", deploy.GetRunsFor())
	}
	if !proto.Equal(cfg.GetDeploy().GetEmojiAck(), deploy) {
		t.Fatalf("emoji_ack deploy = %v, want the same redeploy as user_or_workspace's %v", cfg.GetDeploy().GetEmojiAck(), deploy)
	}
	hook := deploy.GetHook().GetScript()
	info, err := os.Stat(filepath.Join(root, hook.GetPath()))
	if err != nil || info.Mode()&0o111 == 0 {
		t.Fatalf("the deploy script %s = %v, %v; want an executable script", hook.GetPath(), info, err)
	}
	drained := cfg.GetQueueDrained()
	if len(drained) != 1 || drained[0].GetPath() != "modules/app/agent-repl/bin/push-release.sh" || drained[0].GetTimeoutSeconds() != 120 {
		t.Fatalf("queue_drained = %v, want push-release.sh alone with a 120s timeout", drained)
	}
	info, err = os.Stat(filepath.Join(root, drained[0].GetPath()))
	if err != nil || !info.Mode().IsRegular() || info.Mode()&0o111 == 0 {
		t.Fatalf("the queue-drained script %s = %v, %v; want an executable file", drained[0].GetPath(), info, err)
	}
}

func TestPromptHookAndScriptHookAreCheckedHooks(t *testing.T) {
	tests := []struct {
		name  string
		hook  *repoconfigv1.Hook
		check func(*repoconfigv1.Hook) bool
	}{
		{name: "a prompt", hook: PromptHook("open the pull request"),
			check: func(h *repoconfigv1.Hook) bool { return h.GetPrompt().GetText() == "open the pull request" }},
		{name: "a script", hook: ScriptHook("bin/deploy.sh", 60),
			check: func(h *repoconfigv1.Hook) bool {
				return h.GetScript().GetPath() == "bin/deploy.sh" && h.GetScript().GetTimeoutSeconds() == 60
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Assert
			if !tt.check(tt.hook) {
				t.Fatalf("hook = %v", tt.hook)
			}
			if err := CheckHook("hook", tt.hook); err != nil {
				t.Fatalf("CheckHook = %v, want the built hook accepted", err)
			}
		})
	}
}

// TestNoProductionCodeHandBuildsAHook pins that every production site builds a
// hook through PromptHook or ScriptHook, so one spelling of a hook stands.
func TestNoProductionCodeHandBuildsAHook(t *testing.T) {
	// Arrange
	daemon, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	var byHand []string

	// Act
	err = filepath.WalkDir(daemon, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") ||
			strings.HasSuffix(path, ".pb.go") || filepath.Dir(path) == filepath.Join(daemon, "internal", "lifecycleconfig") {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(raw), "repoconfigv1.Hook_Prompt{") || strings.Contains(string(raw), "repoconfigv1.Hook_Script{") {
			byHand = append(byHand, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(byHand) != 0 {
		t.Fatalf("hooks built by hand in %v, want PromptHook and ScriptHook", byHand)
	}
}

func TestEveryScriptIsCheckedByCheckScript(t *testing.T) {
	// Arrange: a stage's script hook and a queue_drained script are held to
	// one set of rules, so the timeout rule is spelled once.
	raw, err := os.ReadFile("lifecycleconfig.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	spelled := strings.Count(string(raw), "GetTimeoutSeconds() < 1")

	// Assert
	if spelled != 1 {
		t.Fatalf("the script timeout rule is spelled %d times in lifecycleconfig.go, want once, in checkScript", spelled)
	}
}

func TestPathIsRelBeneathTheRoot(t *testing.T) {
	// Act
	got := Path("/repo")

	// Assert
	if got != filepath.Join("/repo", Rel) {
		t.Fatalf("Path = %q, want %q beneath the root", got, Rel)
	}
}

func TestTestScript(t *testing.T) {
	cases := []struct {
		name string
		yaml string
		want string
	}{
		{name: "a config with a test step names its script", yaml: agentRepl, want: "modules/app/agent-repl/bin/test-all.sh"},
		{name: "a config with no local merge names none", yaml: "pre_deploy: {}\n", want: ""},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			cfg, err := Parse("lifecycle.yaml", []byte(tc.yaml))
			if err != nil {
				t.Fatal(err)
			}

			// Act
			got := TestScript(cfg)

			// Assert
			if got != tc.want {
				t.Fatalf("TestScript = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestNoProductionCodeReadsTheTestScriptByHand(t *testing.T) {
	// Arrange
	daemon, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	var byHand []string

	// Act
	err = filepath.WalkDir(daemon, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") ||
			strings.HasSuffix(path, ".pb.go") || filepath.Dir(path) == filepath.Join(daemon, "internal", "lifecycleconfig") {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(raw), "GetAgentReplSuites()") {
			byHand = append(byHand, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(byHand) != 0 {
		t.Fatalf("the test script read by hand in %v, want lifecycleconfig.TestScript", byHand)
	}
}

func TestExpandPrompt(t *testing.T) {
	tests := []struct {
		name, text, thread, want string
		err                      error
	}{
		{name: "no placeholder", text: "open the pull request", thread: "", want: "open the pull request"},
		{name: "the thread replaced", text: "reply in {{slack_thread}} and {{slack_thread}}", thread: "https://w.slack.com/archives/C1/p1", want: "reply in https://w.slack.com/archives/C1/p1 and https://w.slack.com/archives/C1/p1"},
		{name: "a thread the workspace has none of", text: "reply in {{slack_thread}}", thread: "", err: ErrNoSlackThread},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ExpandPrompt(tt.text, tt.thread)

			// Assert
			if !errors.Is(err, tt.err) || got != tt.want {
				t.Fatalf("ExpandPrompt = %q, %v; want %q, %v", got, err, tt.want, tt.err)
			}
		})
	}
}

func TestScriptArgv(t *testing.T) {
	tests := []struct {
		name, path, want string
	}{
		{name: "a config's script, beneath the root", path: "bin/x.sh", want: "/repo/bin/x.sh"},
		{name: "a built-in default's script, absolute", path: "/agent-repl/bin/hooks/y.sh", want: "/agent-repl/bin/hooks/y.sh"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := ScriptArgv("/repo", &repoconfigv1.HookScript{Path: tt.path})

			// Assert
			if len(got) != 2 || got[0] != "bash" || got[1] != tt.want {
				t.Fatalf("ScriptArgv = %v, want [bash %s]", got, tt.want)
			}
		})
	}
}

func TestCheckHookRefusesABuiltinInARequest(t *testing.T) {
	// Act
	err := CheckHook("standard.deploy_hooks.pre_deploy[0]", &repoconfigv1.Hook{Hook: &repoconfigv1.Hook_Builtin{Builtin: &repoconfigv1.HookBuiltIn{}}})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no built-in default") {
		t.Fatalf("CheckHook = %v, want the builtin refused", err)
	}
}

// TestNoProductionCodeRunsAScriptHookByItsOwnPath pins that every runner of a
// script hook builds its command with ScriptArgv, so a built-in default's
// absolute path and a config's relative one resolve one way.
func TestNoProductionCodeRunsAScriptHookByItsOwnPath(t *testing.T) {
	// Arrange
	daemon, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	var byHand []string

	// Act
	err = filepath.WalkDir(daemon, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || strings.HasSuffix(path, ".pb.go") {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(raw), "filepath.Join(repoDir, script.GetPath())") || strings.Contains(string(raw), "script.GetPath())}") {
			byHand = append(byHand, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(byHand) != 0 {
		t.Fatalf("a script hook's command built by hand in %v, want ScriptArgv", byHand)
	}
}
