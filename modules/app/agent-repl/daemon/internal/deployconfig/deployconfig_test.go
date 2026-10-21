package deployconfig

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"
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
  hook:
    script:
      path: modules/app/agent-repl/bin/deploy-landing.sh
      timeout_seconds: 60
  runs_for:
    main: {}
`

func TestParseAcceptsAgentReplsConfig(t *testing.T) {
	// Act
	cfg, err := Parse("deploy.yaml", []byte(agentRepl))

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
	if got := cfg.GetDeploy().GetHook().GetScript().GetTimeoutSeconds(); got != 60 {
		t.Errorf("deploy timeout = %d, want 60", got)
	}
	if len(cfg.GetPostDeploy()) != 0 {
		t.Errorf("post_deploy = %v, want none", cfg.GetPostDeploy())
	}
}

func TestParseAcceptsHookOnlyConfigs(t *testing.T) {
	// Arrange: a repository with no local merge, a prompt deploy and a script
	// post-deploy hook.
	doc := `
pre_deploy: {}
deploy:
  hook:
    prompt:
      text: open the pull request
  runs_for:
    any_target: {}
post_deploy:
  - hook:
      script:
        path: scripts/pull.sh
        timeout_seconds: 30
    runs_for:
      main: {}
`
	// Act
	cfg, err := Parse("deploy.yaml", []byte(doc))

	// Assert
	if err != nil {
		t.Fatalf("Parse = %v", err)
	}
	if cfg.GetPreDeploy().GetLocalMerge() != nil {
		t.Error("local_merge set, want none")
	}
	if cfg.GetDeploy().GetHook().GetPrompt().GetText() != "open the pull request" {
		t.Errorf("deploy prompt = %v", cfg.GetDeploy())
	}
	if len(cfg.GetPostDeploy()) != 1 || cfg.GetPostDeploy()[0].GetHook().GetScript().GetPath() != "scripts/pull.sh" {
		t.Errorf("post_deploy = %v", cfg.GetPostDeploy())
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
		{name: "no pre_deploy", doc: "post_deploy: []\n", want: "pre_deploy is required"},
		{name: "a local merge with no test kind", doc: "pre_deploy:\n  local_merge:\n    tests: {}\n", want: "names no test kind"},
		{name: "a test kind with no script", doc: "pre_deploy:\n  local_merge:\n    tests:\n      agent_repl_suites: {}\n", want: "script_path is required"},
		{name: "a hook entry with no hook", doc: "pre_deploy: {}\ndeploy:\n  runs_for:\n    main: {}\n", want: "deploy.hook is required"},
		{name: "a hook entry with no scope", doc: "pre_deploy: {}\ndeploy:\n  hook:\n    prompt:\n      text: go\n", want: "deploy.runs_for is required"},
		{name: "a script with no path", doc: "pre_deploy:\n  hooks:\n    - hook:\n        script:\n          timeout_seconds: 5\n      runs_for:\n        main: {}\n", want: "pre_deploy.hooks[0].hook.script.path is required"},
		{name: "an absolute script path", doc: "pre_deploy:\n  hooks:\n    - hook:\n        script:\n          path: /bin/x\n          timeout_seconds: 5\n      runs_for:\n        main: {}\n", want: "must be relative"},
		{name: "a script with no timeout", doc: "pre_deploy: {}\npost_deploy:\n  - hook:\n      script:\n        path: x.sh\n    runs_for:\n      main: {}\n", want: "post_deploy[0].hook.script.timeout_seconds is required"},
		{name: "a blank prompt", doc: "pre_deploy: {}\npost_deploy:\n  - hook:\n      prompt:\n        text: \"  \"\n    runs_for:\n      any_target: {}\n", want: "never blank"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := Parse("deploy.yaml", []byte(tt.doc))

			// Assert
			var invalid *InvalidError
			if !errors.As(err, &invalid) {
				t.Fatalf("Parse = %v, want an InvalidError", err)
			}
			if invalid.Path != "deploy.yaml" || !strings.Contains(strings.ToLower(invalid.Detail), strings.ToLower(tt.want)) {
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
	main := &repoconfigv1.DeployHookEntry{RunsFor: &repoconfigv1.DeployHookRunsFor{Scope: &repoconfigv1.DeployHookRunsFor_Main{Main: &repoconfigv1.DeployHookRunsForMain{}}}}
	any := &repoconfigv1.DeployHookEntry{RunsFor: &repoconfigv1.DeployHookRunsFor{Scope: &repoconfigv1.DeployHookRunsFor_AnyTarget{AnyTarget: &repoconfigv1.DeployHookRunsForAnyTarget{}}}}
	tests := []struct {
		name   string
		entry  *repoconfigv1.DeployHookEntry
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
	RunsFor(&repoconfigv1.DeployHookEntry{}, true)
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

// TestAgentReplsOwnConfigIsTodaysPipeline pins agent-repl's committed
// `.agent-repl/deploy.yaml` to exactly the steps the queue ran for its own
// repository before deploys were configured: rebase, agent-repl's suites, the
// merge commit and the version tag, then the runtime's redeploy on landings
// into master, and no hook besides.
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
	if len(cfg.GetPreDeploy().GetHooks()) != 0 || len(cfg.GetPostDeploy()) != 0 {
		t.Fatalf("hooks = %v / %v, want none: agent-repl's merges ran no configured prompts", cfg.GetPreDeploy().GetHooks(), cfg.GetPostDeploy())
	}
	deploy := cfg.GetDeploy()
	if deploy.GetRunsFor().GetMain() == nil {
		t.Fatalf("deploy runs for %v, want main alone: only a landing in the main checkout redeploys", deploy.GetRunsFor())
	}
	hook := deploy.GetHook().GetScript()
	info, err := os.Stat(filepath.Join(root, hook.GetPath()))
	if err != nil || info.Mode()&0o111 == 0 {
		t.Fatalf("the deploy script %s = %v, %v; want an executable script", hook.GetPath(), info, err)
	}
}
