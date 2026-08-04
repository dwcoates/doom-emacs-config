package server

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"

	"claude-repld/internal/reload"
	"claude-repld/internal/stateroot"
	"claude-repld/internal/workspace/merge"
	"claude-repld/internal/workspace/postmerge"
)

// This file binds the two independent facts that follow a landed merge:
// process-level self-reload and resolution of the merged workspace's own
// postprocessing action.

// namedHook pairs a hook with the name its failures are logged under.
type namedHook struct {
	name string
	hook merge.PostMergeHook
}

// fanOutPostMergeHook runs every bound hook per merged outcome.
type fanOutPostMergeHook struct {
	logf  func(string, ...any)
	hooks []namedHook
}

var _ merge.PostMergeHook = (*fanOutPostMergeHook)(nil)

// AfterMerged runs every hook, in order, and never stops early.
//
// The errors are joined rather than reduced to the first: merge.Coordinator
// retains this return as a merge.PostMergeFailure, and a record naming only one
// of two failed hooks would understate what the merge left undone.
func (f *fanOutPostMergeHook) AfterMerged(ctx context.Context, req merge.Request) error {
	var errs []error
	for _, h := range f.hooks {
		if err := h.hook.AfterMerged(ctx, req); err != nil {
			f.logf("server: post-merge hook %s FAILED {ws=%s name=%s target=%s}: %v",
				h.name, req.Workspace, req.Name, req.TargetDir, err)
			errs = append(errs, fmt.Errorf("%s: %w", h.name, err))
		}
	}
	return errors.Join(errs...)
}

// buildPostMergeHook assembles process-level merge aftermath.
func buildPostMergeHook(logf func(string, ...any)) (merge.PostMergeHook, error) {
	hooks := []namedHook{}
	trigger, ok, err := buildSelfReloadTrigger(logf)
	if err != nil {
		return nil, err
	}
	if ok {
		hooks = append(hooks, namedHook{name: "self-reload", hook: trigger})
	}
	return &fanOutPostMergeHook{logf: logf, hooks: hooks}, nil
}

// buildSelfReloadTrigger assembles the self-merge redeploy trigger.
//
// The bool is false when THIS BINARY IS NOT DEPLOYED FROM AN AGENT-REPL
// CHECKOUT, which is a fact about the binary rather than a failure: a `go test`
// binary lives in the build cache and has no checkout to rebuild. It is recorded
// loudly so a daemon that silently never redeploys itself is never a mystery.
// Every other failure is a hard construction error.
func buildSelfReloadTrigger(logf func(string, ...any)) (*reload.Trigger, bool, error) {
	exe, err := os.Executable()
	if err != nil {
		return nil, false, fmt.Errorf("server: resolve own executable for the self-merge redeploy trigger: %w", err)
	}
	self, deployed, err := reload.ResolveSelf(context.Background(), exe)
	if err != nil {
		return nil, false, fmt.Errorf("server: resolve the checkout this daemon runs from: %w", err)
	}
	if !deployed {
		logf("server: self-merge redeploy DISABLED for this process: the running binary (%s) is not inside an agent-repl checkout, so there is nothing for it to rebuild", exe)
		return nil, false, nil
	}
	// The redeploy's log lives under the state root and NEVER inside the
	// checkout, because the checkout is precisely what the deploy is rewriting.
	root, err := stateroot.Root()
	if err != nil {
		return nil, false, fmt.Errorf("server: resolve state root for the self-merge redeploy log: %w", err)
	}
	launcher, err := reload.NewDetachedScript(reload.ScriptConfig{
		Script: self.ScriptPath(),
		Dir:    self.Root,
		LogDir: filepath.Join(root, "reload"),
		Logf:   logf,
	})
	if err != nil {
		return nil, false, fmt.Errorf("server: build the self-merge redeploy launcher: %w", err)
	}
	trigger, err := reload.New(reload.Config{Self: self, Launcher: launcher, Logf: logf})
	if err != nil {
		return nil, false, fmt.Errorf("server: build the self-merge redeploy trigger: %w", err)
	}
	logf("server: self-merge redeploy ARMED {checkout=%s script=%s}", self.Root, self.ScriptPath())
	return trigger, true, nil
}

// buildAfterActionSource binds the creation records that own the merged
// workspace's postprocessing action.
func buildAfterActionSource(cfg AgentShimConfig, logf func(string, ...any)) (merge.AfterActionSource, error) {
	postprocessing, ok := cfg.WorkspaceCreation.(postmerge.PostprocessingSource)
	if !ok {
		return nil, fmt.Errorf("server: the WorkspaceCreation bridge (%T) cannot resolve a workspace's postprocessing prompt, so a workspace created with one would merge and never run it", cfg.WorkspaceCreation)
	}
	source, err := postmerge.New(postmerge.Config{Logf: logf, Postprocessing: postprocessing})
	if err != nil {
		return nil, fmt.Errorf("server: build post-merge after-action source: %w", err)
	}
	return source, nil
}
