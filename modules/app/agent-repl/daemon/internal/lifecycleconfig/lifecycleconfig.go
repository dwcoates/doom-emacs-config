// Package lifecycleconfig reads a repository's `.agent-repl/lifecycle.yaml`:
// what happens at each point of one of its workspaces' lives
// (repoconfig.v1.LifecycleConfig), and which hooks each stage runs for each
// kind of workspace, the built-in defaults included (stages.go).
//
// THE FILE IS READ AT EVERY USE, never cached, so an edit takes effect on the
// next create or deploy. It is YAML converted to JSON and decoded with
// protojson with unknown fields refused, then checked for every field the
// schema calls REQUIRED: a file that decodes but leaves one unset is refused
// with the field named, never run with a default.
package lifecycleconfig

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"go.yaml.in/yaml/v3"
	"google.golang.org/protobuf/encoding/protojson"
)

// Rel is the config's path relative to a repository's main checkout root, as
// an author writes it.
const Rel = ".agent-repl/lifecycle.yaml"

// Path is repoRoot's config file.
func Path(repoRoot string) string {
	return filepath.Join(repoRoot, filepath.FromSlash(Rel))
}

// MissingError is a repository with no lifecycle config.
type MissingError struct {
	// Path is the file looked for.
	Path string
}

func (e *MissingError) Error() string {
	return fmt.Sprintf("lifecycleconfig: %s does not exist; the repository states no deploy", e.Path)
}

// InvalidError is a lifecycle config that cannot be used.
type InvalidError struct {
	// Path is the file.
	Path string
	// Detail is why it was refused.
	Detail string
}

func (e *InvalidError) Error() string {
	return fmt.Sprintf("lifecycleconfig: %s is not a usable lifecycle config: %s", e.Path, e.Detail)
}

// Files reads a repository's lifecycle config. It is an interface because the
// queue and the create are unit-tested against configs that never touch a
// disk; OnDisk is the production value.
type Files interface {
	// Load answers repoRoot's lifecycle config, a *MissingError when the file
	// is absent, an *InvalidError when it is unusable, or the read's own
	// error.
	Load(repoRoot string) (*repoconfigv1.LifecycleConfig, error)
}

// OnDisk is the production Files.
type OnDisk struct{}

// Load implements Files.
func (OnDisk) Load(repoRoot string) (*repoconfigv1.LifecycleConfig, error) {
	path := Path(repoRoot)
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, &MissingError{Path: path}
	}
	if err != nil {
		return nil, &InvalidError{Path: path, Detail: err.Error()}
	}
	return Parse(path, data)
}

// Parse decodes and checks one config's bytes; path names it in refusals.
func Parse(path string, data []byte) (*repoconfigv1.LifecycleConfig, error) {
	var doc any
	if err := yaml.Unmarshal(data, &doc); err != nil {
		return nil, &InvalidError{Path: path, Detail: err.Error()}
	}
	if doc == nil {
		return nil, &InvalidError{Path: path, Detail: "the file is empty"}
	}
	raw, err := json.Marshal(doc)
	if err != nil {
		return nil, &InvalidError{Path: path, Detail: err.Error()}
	}
	cfg := &repoconfigv1.LifecycleConfig{}
	if err := (protojson.UnmarshalOptions{DiscardUnknown: false}).Unmarshal(raw, cfg); err != nil {
		return nil, &InvalidError{Path: path, Detail: err.Error()}
	}
	if err := check(cfg); err != nil {
		return nil, &InvalidError{Path: path, Detail: err.Error()}
	}
	return cfg, nil
}

// check refuses a decoded config that leaves a REQUIRED field unset, or
// places a hook where it cannot run.
func check(cfg *repoconfigv1.LifecycleConfig) error {
	if cfg.GetPreDeploy() == nil {
		return errors.New("pre_deploy is required (an empty `pre_deploy: {}` states a deploy with no first stage)")
	}
	for _, stage := range Stages {
		if stage == StageDeploy {
			continue
		}
		for _, kind := range Originators {
			list := listOf(cfg, stage, kind)
			if list == nil {
				continue
			}
			for i, entry := range list.GetHooks() {
				at := fmt.Sprintf("%s.%s.hooks[%d]", stage.Key(), kind, i)
				if err := checkEntry(at, entry, stage.hasDefault(kind)); err != nil {
					return err
				}
			}
		}
	}
	for _, kind := range Originators {
		if entry := deployEntryOf(cfg, kind); entry != nil {
			if err := checkEntry(fmt.Sprintf("deploy.%s", kind), entry, false); err != nil {
				return err
			}
		}
	}
	if lm := cfg.GetPreDeploy().GetLocalMerge(); lm != nil {
		switch kind := lm.GetTests().GetKind().(type) {
		case *repoconfigv1.DeployLocalMergeTests_AgentReplSuites:
			if strings.TrimSpace(kind.AgentReplSuites.GetScriptPath()) == "" {
				return errors.New("pre_deploy.local_merge.tests.agent_repl_suites.script_path is required")
			}
		default:
			return errors.New("pre_deploy.local_merge.tests names no test kind")
		}
	}
	for i, script := range cfg.GetQueueDrained() {
		if err := checkScript(fmt.Sprintf("queue_drained[%d]", i), script); err != nil {
			return err
		}
	}
	if cfg.Concurrent != nil {
		if cfg.GetConcurrent().GetThrough() == nil {
			return errors.New("concurrent names no stage: `through_pre_deploy: {}` or `through_deploy: {}`")
		}
		if cfg.GetPreDeploy().GetLocalMerge() != nil {
			return errors.New("concurrent cannot be set with pre_deploy.local_merge: a local merge lands on the target, which one deploy at a time may move")
		}
	}
	return nil
}

// Concurrent reports whether deploy STAGE runs concurrently under CFG:
// deploys run it side by side ahead of their turn, rather than one at a time
// in queue order. Only a leading run of the deploy stages can be
// (LifecycleConfig.concurrent); creation and finish are never queued at all.
func Concurrent(cfg *repoconfigv1.LifecycleConfig, stage Stage) bool {
	switch cfg.GetConcurrent().GetThrough().(type) {
	case *repoconfigv1.DeployConcurrentStages_ThroughPreDeploy:
		return stage == StagePreDeploy
	case *repoconfigv1.DeployConcurrentStages_ThroughDeploy:
		return stage == StagePreDeploy || stage == StageDeploy
	}
	return false
}

// AnyConcurrent reports whether CFG runs any deploy stage concurrently.
func AnyConcurrent(cfg *repoconfigv1.LifecycleConfig) bool {
	return Concurrent(cfg, StagePreDeploy)
}

// checkEntry refuses a hook entry with no hook or no scope. BUILTIN says
// whether the entry's list may hold a `builtin` hook.
func checkEntry(at string, entry *repoconfigv1.HookEntry, builtin bool) error {
	if err := checkHook(at+".hook", entry.GetHook(), builtin); err != nil {
		return err
	}
	if entry.GetRunsFor().GetScope() == nil {
		return fmt.Errorf("%s.runs_for is required: `main: {}` or `any_target: {}`", at)
	}
	return nil
}

// CheckHook refuses a hook a REQUEST supplies that is neither a usable script
// nor a usable prompt: a request's own hooks are held to the file's rules, and
// a request names no stage, so it may not name a stage's built-in default.
func CheckHook(at string, hook *repoconfigv1.Hook) error {
	return checkHook(at, hook, false)
}

// checkHook refuses a hook that is not usable where it stands; BUILTIN says
// whether a `builtin` hook may stand there.
func checkHook(at string, hook *repoconfigv1.Hook, builtin bool) error {
	switch h := hook.GetHook().(type) {
	case *repoconfigv1.Hook_Script:
		return checkScript(at+".script", h.Script)
	case *repoconfigv1.Hook_Prompt:
		if strings.TrimSpace(h.Prompt.GetText()) == "" {
			return fmt.Errorf("%s.prompt.text is required and never blank", at)
		}
		for _, token := range placeholderPattern.FindAllString(h.Prompt.GetText(), -1) {
			if token != SlackThreadPlaceholder {
				return fmt.Errorf("%s.prompt.text carries the placeholder %s; the one placeholder a prompt may carry is %s", at, token, SlackThreadPlaceholder)
			}
		}
	case *repoconfigv1.Hook_Builtin:
		if !builtin {
			return fmt.Errorf("%s.builtin stands where there is no built-in default: only an emoji_ack list of creation, finish or post_deploy has one", at)
		}
	default:
		return fmt.Errorf("%s is required: `script: {...}`, `prompt: {...}` or `builtin: {}`", at)
	}
	return nil
}

// checkScript refuses a script with no relative path or no timeout of at
// least a second, wherever the config gives one: a stage's script hook and a
// queue_drained script are held to the same rules.
func checkScript(at string, script *repoconfigv1.HookScript) error {
	if strings.TrimSpace(script.GetPath()) == "" {
		return fmt.Errorf("%s.path is required", at)
	}
	if filepath.IsAbs(script.GetPath()) {
		return fmt.Errorf("%s.path %q must be relative to the repository's main checkout root", at, script.GetPath())
	}
	if script.GetTimeoutSeconds() < 1 {
		return fmt.Errorf("%s.timeout_seconds is required and at least 1", at)
	}
	return nil
}

// PromptHook is a prompt hook of TEXT, the one spelling every builder of a
// hook uses.
func PromptHook(text string) *repoconfigv1.Hook {
	return &repoconfigv1.Hook{Hook: &repoconfigv1.Hook_Prompt{Prompt: &repoconfigv1.HookPrompt{Text: text}}}
}

// ScriptHook is a script hook of PATH bounded at TIMEOUT seconds.
func ScriptHook(path string, timeoutSeconds uint32) *repoconfigv1.Hook {
	return &repoconfigv1.Hook{Hook: &repoconfigv1.Hook_Script{Script: &repoconfigv1.HookScript{Path: path, TimeoutSeconds: timeoutSeconds}}}
}

// ScriptArgv is the command a script hook runs as from the repository's main
// checkout root REPOROOT. A config's script is relative to that root; a
// built-in default's is absolute, in agent-repl's own checkout (stages.go),
// which no config can name because checkScript refuses an absolute path.
// EVERY RUNNER OF A SCRIPT HOOK BUILDS ITS COMMAND HERE, so the two kinds of
// path resolve one way.
func ScriptArgv(repoRoot string, script *repoconfigv1.HookScript) []string {
	path := script.GetPath()
	if !filepath.IsAbs(path) {
		path = filepath.Join(repoRoot, path)
	}
	return []string{"bash", path}
}

// TestScript is the config's agent_repl_suites test runner, relative to a
// checkout's root: the script the merge queue gates on and the daemon runs an
// agent's selected tests with. "" is a config that names none.
func TestScript(cfg *repoconfigv1.LifecycleConfig) string {
	return cfg.GetPreDeploy().GetLocalMerge().GetTests().GetAgentReplSuites().GetScriptPath()
}

// RunsFor reports whether a hook entry runs for a workspace or deploy whose
// target is (toMain) or is not the repository's main branch in its main
// worktree.
func RunsFor(entry *repoconfigv1.HookEntry, toMain bool) bool {
	switch entry.GetRunsFor().GetScope().(type) {
	case *repoconfigv1.HookRunsFor_AnyTarget:
		return true
	case *repoconfigv1.HookRunsFor_Main:
		return toMain
	}
	panic("lifecycleconfig: a checked entry names no scope")
}

// SlackThreadPlaceholder is the one placeholder a prompt hook may carry: the
// link of the Slack message the workspace was created for.
const SlackThreadPlaceholder = "{{slack_thread}}"

// EnvSlackThread is the environment variable a script hook reads the same
// link from, set only for a workspace with a Slack origin.
const EnvSlackThread = "AGENT_REPL_SLACK_THREAD"

// placeholderPattern is any `{{...}}` token in a prompt.
var placeholderPattern = regexp.MustCompile(`\{\{[^{}]*\}\}`)

// ErrNoSlackThread is a prompt that names the originating Slack thread for a
// workspace that has none.
var ErrNoSlackThread = errors.New("the prompt names " + SlackThreadPlaceholder + ", but the workspace was created for no Slack message")

// ExpandPrompt replaces TEXT's placeholder with SLACKTHREAD, the workspace's
// originating thread ("" when it has none). A prompt naming the thread for a
// workspace with none is ErrNoSlackThread: the hook cannot be what its author
// wrote, so it fails rather than reaching the agent with a hole in it.
func ExpandPrompt(text, slackThread string) (string, error) {
	if !strings.Contains(text, SlackThreadPlaceholder) {
		return text, nil
	}
	if slackThread == "" {
		return "", ErrNoSlackThread
	}
	return strings.ReplaceAll(text, SlackThreadPlaceholder, slackThread), nil
}
