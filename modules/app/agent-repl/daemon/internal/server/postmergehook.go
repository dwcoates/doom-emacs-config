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

// This file binds merge.Coordinator's post-merge hook.
//
// BOTH OF ITS DEPENDENCIES ARE DERIVED FROM ALREADY-INJECTED ONES rather than
// injected separately, for the same reason mergeConflictResolver is (see
// agentshimwire.go). The hook prompts the PARENT workspace's session, and the
// parent's session must be on the SAME controller fleet that serves the user's
// prompts — a separately injected session source could be bound to a different
// fleet, so the phone-home would land in a session nobody is looking at. The
// postprocessing prompt comes from the SAME creation records the create
// commands write, so a second store binding could answer from a file no
// creation ever wrote to.
//
// Each derivation is a CHECKED assertion whose failure is a hard construction
// error naming exactly what is missing. It is never a silent downgrade to a
// disabled hook: a daemon whose merges quietly stop notifying parents is the
// state this whole package exists to end.

// TWO THINGS FOLLOW A MERGE, NOT ONE.
//
// Beside the parent handoff above, a merge into the stack's OWN checkout has to
// redeploy the running stack (internal/reload). The two are wholly independent
// — one prompts a session, the other spawns a build — so they are composed as a
// FAN-OUT rather than chained: each runs on every merged outcome regardless of
// whether the other failed, and each failure is surfaced on its own.
//
// Chaining them would make the notifier's failure silently cancel the redeploy,
// which is the state where a merge lands, the parent is not told, AND the
// daemon keeps running superseded code with nothing in the record explaining
// why the deploy never happened.

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
// of two failed handoffs would understate what the merge left undone.
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

// buildPostMergeHook assembles the post-merge fan-out from cfg's existing
// dependencies.
func buildPostMergeHook(cfg AgentShimConfig, logf func(string, ...any)) (merge.PostMergeHook, error) {
	notifier, err := buildPostMergeNotifier(cfg, logf)
	if err != nil {
		return nil, err
	}
	hooks := []namedHook{{name: "parent-handoff", hook: notifier}}
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

// buildPostMergeNotifier assembles the parent-handoff half of the fan-out.
func buildPostMergeNotifier(cfg AgentShimConfig, logf func(string, ...any)) (merge.PostMergeHook, error) {
	parents, ok := cfg.Prompts.(postmerge.ParentSession)
	if !ok {
		return nil, fmt.Errorf("server: the PromptRouter (%T) cannot answer whether a workspace has a live session, so the post-merge parent handoff has no way to avoid spawning a session for a workspace nobody has open", cfg.Prompts)
	}
	postprocessing, ok := cfg.WorkspaceCreation.(postmerge.PostprocessingSource)
	if !ok {
		return nil, fmt.Errorf("server: the WorkspaceCreation bridge (%T) cannot resolve a workspace's postprocessing prompt, so a workspace created with one would merge and never run it", cfg.WorkspaceCreation)
	}
	probe, err := postmerge.NewGitWorktreeProbe(logf)
	if err != nil {
		return nil, fmt.Errorf("server: build post-merge worktree probe: %w", err)
	}
	notifier, err := postmerge.New(postmerge.Config{
		Logf:           logf,
		Parents:        parents,
		Worktrees:      probe,
		Postprocessing: postprocessing,
	})
	if err != nil {
		return nil, fmt.Errorf("server: build post-merge notifier: %w", err)
	}
	return notifier, nil
}
