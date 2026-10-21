// Package deployconfig reads a repository's `.agent-repl/deploy.yaml`: what a
// deploy of one of its workspaces does (repoconfig.v1.DeployConfig).
//
// THE FILE IS READ AT EVERY USE, never cached, so an edit takes effect on the
// next deploy. It is YAML converted to JSON and decoded with protojson with
// unknown fields refused, then checked for every field the schema calls
// REQUIRED: a file that decodes but leaves one unset is refused with the field
// named, never run with a default.
package deployconfig

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"go.yaml.in/yaml/v3"
	"google.golang.org/protobuf/encoding/protojson"
)

// Rel is the config's path relative to a repository's main checkout root, as
// an author writes it.
const Rel = ".agent-repl/deploy.yaml"

// Path is repoRoot's config file.
func Path(repoRoot string) string {
	return filepath.Join(repoRoot, ".agent-repl", "deploy.yaml")
}

// MissingError is a repository with no deploy config.
type MissingError struct {
	// Path is the file looked for.
	Path string
}

func (e *MissingError) Error() string {
	return fmt.Sprintf("deployconfig: %s does not exist; the repository states no deploy", e.Path)
}

// InvalidError is a deploy config that cannot be used.
type InvalidError struct {
	// Path is the file.
	Path string
	// Detail is why it was refused.
	Detail string
}

func (e *InvalidError) Error() string {
	return fmt.Sprintf("deployconfig: %s is not a usable deploy config: %s", e.Path, e.Detail)
}

// Files reads a repository's deploy config. It is an interface because the
// queue is unit-tested against configs that never touch a disk; OnDisk is the
// production value.
type Files interface {
	// Load answers repoRoot's deploy config, a *MissingError when the file is
	// absent, an *InvalidError when it is unusable, or the read's own error.
	Load(repoRoot string) (*repoconfigv1.DeployConfig, error)
}

// OnDisk is the production Files.
type OnDisk struct{}

// Load implements Files.
func (OnDisk) Load(repoRoot string) (*repoconfigv1.DeployConfig, error) {
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
func Parse(path string, data []byte) (*repoconfigv1.DeployConfig, error) {
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
	cfg := &repoconfigv1.DeployConfig{}
	if err := (protojson.UnmarshalOptions{DiscardUnknown: false}).Unmarshal(raw, cfg); err != nil {
		return nil, &InvalidError{Path: path, Detail: err.Error()}
	}
	if err := check(cfg); err != nil {
		return nil, &InvalidError{Path: path, Detail: err.Error()}
	}
	return cfg, nil
}

// check refuses a decoded config that leaves a REQUIRED field unset.
func check(cfg *repoconfigv1.DeployConfig) error {
	if cfg.GetPreDeploy() == nil {
		return errors.New("pre_deploy is required (an empty `pre_deploy: {}` states a deploy with no first stage)")
	}
	for i, entry := range cfg.GetPreDeploy().GetHooks() {
		if err := checkEntry(fmt.Sprintf("pre_deploy.hooks[%d]", i), entry); err != nil {
			return err
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
	if cfg.Deploy != nil {
		if err := checkEntry("deploy", cfg.GetDeploy()); err != nil {
			return err
		}
	}
	for i, entry := range cfg.GetPostDeploy() {
		if err := checkEntry(fmt.Sprintf("post_deploy[%d]", i), entry); err != nil {
			return err
		}
	}
	return nil
}

// checkEntry refuses a hook entry with no hook or no scope.
func checkEntry(at string, entry *repoconfigv1.DeployHookEntry) error {
	if err := CheckHook(at+".hook", entry.GetHook()); err != nil {
		return err
	}
	if entry.GetRunsFor().GetScope() == nil {
		return fmt.Errorf("%s.runs_for is required: `main: {}` or `any_target: {}`", at)
	}
	return nil
}

// CheckHook refuses a hook that is neither a usable script nor a usable
// prompt. A request's own hooks are checked by it too, so a hook given on the
// wire is held to the file's rules.
func CheckHook(at string, hook *repoconfigv1.DeployHook) error {
	switch h := hook.GetHook().(type) {
	case *repoconfigv1.DeployHook_Script:
		if strings.TrimSpace(h.Script.GetPath()) == "" {
			return fmt.Errorf("%s.script.path is required", at)
		}
		if filepath.IsAbs(h.Script.GetPath()) {
			return fmt.Errorf("%s.script.path %q must be relative to the repository's main checkout root", at, h.Script.GetPath())
		}
		if h.Script.GetTimeoutSeconds() < 1 {
			return fmt.Errorf("%s.script.timeout_seconds is required and at least 1", at)
		}
	case *repoconfigv1.DeployHook_Prompt:
		if strings.TrimSpace(h.Prompt.GetText()) == "" {
			return fmt.Errorf("%s.prompt.text is required and never blank", at)
		}
	default:
		return fmt.Errorf("%s is required: `script: {...}` or `prompt: {...}`", at)
	}
	return nil
}

// RunsFor reports whether a hook entry runs for a deploy whose target is
// (toMain) or is not the repository's main branch in its main worktree.
func RunsFor(entry *repoconfigv1.DeployHookEntry, toMain bool) bool {
	switch entry.GetRunsFor().GetScope().(type) {
	case *repoconfigv1.DeployHookRunsFor_AnyTarget:
		return true
	case *repoconfigv1.DeployHookRunsFor_Main:
		return toMain
	}
	panic("deployconfig: a checked entry names no scope")
}
