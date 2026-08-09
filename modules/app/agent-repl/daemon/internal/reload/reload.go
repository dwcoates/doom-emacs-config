// Package reload turns a merge of the agent-repl stack's OWN repository into a
// redeploy of the running stack.
//
// The stack develops itself. A workspace of ~/.config/doom merges its commits
// into that checkout, and until those commits are BUILT and the services
// BOUNCED, every running piece of the stack is executing superseded code while
// the tree on disk says otherwise. reload.Trigger closes that gap: it is a
// merge.PostMergeHook that recognizes a merge into its own checkout, works out
// what the landed commits touched, and hands bin/deploy-all.sh the job.
//
// WHAT THIS PACKAGE DOES NOT DO. It does not rebuild anything, does not stop or
// start any service, and does not restart the daemon. bin/deploy-all.sh already
// owns the ordered chain (proto, shim bundle, webapp, daemon, store, sidecar,
// daemon bounce, elisp hot-load) including the recorded store-before-sidecar
// ordering and the Emacs-not-running deferral. Reimplementing any of it here
// would give the system two deploy paths that drift.
//
// RECONNECTION IS ALSO NOT THIS PACKAGE'S JOB. The bounce is survivable because
// the machinery to survive it already exists: session shims outlive their
// daemon and reattach with sequence replay, Emacs rebinds on restart, and the
// webapp's websocket reconnects. This package's only obligation is to not
// bypass that machinery, which it satisfies by going through the same script a
// human deploy goes through.
package reload

import (
	"context"
	"fmt"
	"strings"
	"sync/atomic"

	"claude-repld/internal/dlog"
	"claude-repld/internal/workspace/merge"
)

// Config constructs a Trigger.
type Config struct {
	// Self identifies the checkout this daemon binary runs from. Required, and
	// injected rather than resolved here so every test states the identity it
	// means instead of inheriting the test binary's accidental location.
	Self SelfRepo
	// Launcher starts the decided redeploy. Required.
	Launcher Launcher
	// Logf receives this package's canonical log records. Required.
	Logf dlog.Logf
}

// Trigger is the self-merge half of the daemon's post-merge hook.
type Trigger struct {
	self     SelfRepo
	launcher Launcher
	logf     dlog.Logf

	// launched is the belt-and-braces reload latch. See AfterMerged for why a
	// loop is not expected in the first place, and why in-memory is the right
	// lifetime for the guard.
	launched atomic.Bool
}

var _ merge.PostMergeHook = (*Trigger)(nil)

// New validates cfg and returns the trigger. Every missing dependency is a hard
// construction error rather than a disabled trigger: a daemon that silently
// stops redeploying itself looks exactly like one whose deploys work, right up
// until somebody notices the running code is weeks old.
func New(cfg Config) (*Trigger, error) {
	if err := cfg.Self.validate(); err != nil {
		return nil, err
	}
	if cfg.Launcher == nil {
		return nil, fmt.Errorf("reload: Trigger needs a Launcher")
	}
	if cfg.Logf == nil {
		return nil, fmt.Errorf("reload: Trigger needs a Logf")
	}
	return &Trigger{self: cfg.Self, launcher: cfg.Launcher, logf: cfg.Logf}, nil
}

// AfterMerged implements merge.PostMergeHook.
//
// COST. This runs after EVERY merge the daemon performs, and almost all of them
// are for some other repository. The not-self path is therefore exactly one git
// invocation (a single rev-parse answering both halves of the identity) and no
// allocation beyond it, before returning nil.
//
// NO-LOOP ARGUMENT. A redeploy bounces the daemon, and the next boot drains the
// durable merge queue. It cannot re-run this merge:
//
//   - merge.Coordinator fires merge.PostMergeHook exactly once per `merged`
//     terminal outcome, and only AFTER the queue entry has been dropped. A
//     merge whose entry could not be dropped does not fire the hook at all.
//   - So by the time this function has ever been reached, the entry that would
//     have replayed is already gone, and boot-time Drain has nothing to replay.
//     The reload is downstream of the durable state, never upstream of it.
//
// The `launched` latch is belt-and-braces against a mechanism failing that
// argument, and it is deliberately IN-MEMORY: the thing it guards is "this
// process already spawned a deploy that is going to kill it", which is a fact
// about this process's lifetime and nothing longer. A durable latch would be
// strictly worse — it would survive the very bounce that makes it meaningless
// and would have to be cleared by whatever came back up, turning a two-line
// guard into another piece of durable state to get wrong.
//
// WHAT THE LATCH IS AND IS NOT. It guards against two deploys running AT ONCE,
// and against nothing else. It used to be held for the rest of the process's
// life once a launch succeeded, on the reasoning that a successful launch ends
// that life — but a launch is only the START of a deploy, and a deploy can end
// without bouncing anything (the script exits non-zero, or defers its bounce
// because no Emacs is running). Every such deploy permanently disabled
// self-redeploy for a daemon that then went on running superseded code with no
// further merge able to correct it. So the latch is RE-ARMED by
// {@link Trigger.rearm}, which the launcher calls from the reaper that already
// detects exactly this case. A deploy that DID bounce the daemon never calls
// it: the process is gone and its replacement boots with a fresh latch.
// AfterAction implements merge.PostMergeHook. The self-reload trigger delivers
// no prompt to any session — it redeploys a binary — so it reports none. It is
// an explicit empty answer rather than an absent method so a hook that DOES
// deliver a prompt can never be confused with one that does not.
func (t *Trigger) AfterAction(merge.Request) (string, error) { return "", nil }

func (t *Trigger) AfterMerged(ctx context.Context, req merge.Request) error {
	ident, err := IdentifyRepo(ctx, req.TargetDir)
	if err != nil {
		t.logf("reload: identify merge target FAILED {ws=%s name=%s dir=%s}: %v",
			req.Workspace, req.Name, req.TargetDir, err)
		return fmt.Errorf("reload: identify merge target %s: %w", req.TargetDir, err)
	}
	if ident.CommonDir != t.self.CommonDir {
		return nil
	}
	if ident.Toplevel != t.self.Root {
		// The same REPOSITORY, but a different worktree of it: a child
		// workspace merging into its parent workspace's worktree. The commits
		// landed somewhere this daemon's code was not built from, so rebuilding
		// from t.self.Root would deploy a tree that does not contain them.
		t.logf("reload: merge landed in a sibling worktree of this daemon's own repository, so no redeploy {ws=%s target=%s self=%s}",
			req.Workspace, ident.Toplevel, t.self.Root)
		return nil
	}
	if !t.launched.CompareAndSwap(false, true) {
		t.logf("reload: self-merge SKIPPED, a redeploy launched earlier in this daemon's lifetime is still the live one {ws=%s target=%s}",
			req.Workspace, ident.Toplevel)
		return nil
	}
	plan, ok, err := t.plan(ctx, req)
	if err != nil || !ok {
		// Nothing was launched, so the latch must not stay held against the
		// next merge.
		t.launched.Store(false)
		return err
	}
	if err := t.launcher.Launch(ctx, plan, func() { t.rearm(req.Workspace) }); err != nil {
		t.launched.Store(false)
		return fmt.Errorf("reload: launch redeploy for %s: %w", req.Workspace, err)
	}
	return nil
}

// rearm releases the latch for a deploy that ended without bouncing this
// daemon, so the next self-merge tries again instead of being skipped forever.
//
// It is idempotent by construction: the launcher calls it at most once per
// launch, and a redundant release only returns the latch to the state a process
// with no deploy in flight is already in.
func (t *Trigger) rearm(workspace string) {
	if !t.launched.CompareAndSwap(true, false) {
		t.logf("reload: redeploy exit reported with no launch latched {ws=%s}", workspace)
		return
	}
	t.logf("reload: the redeploy launched for %s ENDED without bouncing this daemon, so the reload latch is RE-ARMED; the next self-merge will try again", workspace)
}

// plan derives what the merge landed and what it touched. The bool reports
// whether a redeploy is warranted at all.
func (t *Trigger) plan(ctx context.Context, req merge.Request) (Plan, bool, error) {
	land, err := landedRange(ctx, req.TargetDir, req.SourceBranch)
	if err != nil {
		t.logf("reload: landed-range derivation FAILED {ws=%s branch=%s dir=%s}: %v",
			req.Workspace, req.SourceBranch, req.TargetDir, err)
		return Plan{}, false, err
	}
	if land.Count == 0 {
		t.logf("reload: self-merge added no commits to %s, so no redeploy {ws=%s branch=%s}",
			req.TargetDir, req.Workspace, req.SourceBranch)
		return Plan{}, false, nil
	}
	paths, err := changedPaths(ctx, req.TargetDir, land.Spec)
	if err != nil {
		t.logf("reload: changed-path query FAILED {ws=%s range=%s dir=%s}: %v",
			req.Workspace, land.Spec, req.TargetDir, err)
		return Plan{}, false, err
	}
	components := Classify(paths)
	if len(components) == 0 {
		t.logf("reload: self-merge touched nothing the running stack executes, so no redeploy {ws=%s range=%s commits=%d paths=%d}",
			req.Workspace, land.Spec, land.Count, len(paths))
		return Plan{}, false, nil
	}
	t.logf("reload: self-merge CLASSIFIED {ws=%s range=%s commits=%d paths=%d components=%s}",
		req.Workspace, land.Spec, land.Count, len(paths), strings.Join(Strings(components), ","))
	return Plan{Workspace: req.Workspace, Components: components, Range: land.Spec}, true, nil
}
