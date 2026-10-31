package merge

import (
	"context"
	"fmt"
	"path/filepath"
	"slices"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// This file is a repository's QUEUE DRAIN: the scripts its deploy config runs
// once its merge queue fully depletes (repoconfig.v1.DeployConfig.queue_drained),
// which for agent-repl push master to origin as a release.
//
// THE DEBT IS DURABLE. The store owes a repository its drain in the write that
// admits each of its merges (wsm's AdmitMerge), and clears it only once every
// script has succeeded. Neither a restart between a landing and the queue
// emptying (a landing's own rollout is one) nor a failed script loses it: the
// next time this daemon or its successor finds the queue empty, it runs them
// all again. The boot offers every registered repository's queue to its pump
// (Recover), so a successor that inherits an empty queue still finds the debt.
//
// NOTHING LANDS WHILE THEY RUN. They run on the admission pump, between its
// last empty look and its going idle, so this daemon admits nothing until they
// end (a kick meanwhile is remembered, and admitted after), and under the
// repository's kernel lock, so no other daemon admits either.

const opQueueDrained = "daemon.merge.queue_drained"

// settleDrained runs REPO's queue_drained scripts, in order, when its queue
// has fully depleted and owes them, and clears the debt once they have all
// succeeded. Every failure is recorded here, at ERROR, and leaves the debt
// standing for the queue's next emptying.
func (o *orchestrator) settleDrained(ctx context.Context, repo wsm.RepoKey) {
	root, scripts, due := o.drainDue(ctx, repo)
	if !due {
		return
	}
	defer o.releaseRepoLockIfIdle(ctx, repo)
	log := o.deps.Log.Global()
	fields := dlog.Context{"repo": string(repo), "repo_root": root, "scripts": len(scripts)}
	log.Info(opQueueDrained, "the repository's merge queue fully depleted; its queue_drained scripts run", fields)
	for i, script := range scripts {
		if !o.runDrainedScript(ctx, repo, root, i, script) {
			return
		}
	}
	// THE CLEAR IS AN ADMISSION STEP, so the shutdown drain never closes the
	// store under it. A daemon already exiting leaves the debt standing, and
	// its successor runs the scripts again.
	if !o.enterAdmission() {
		log.Info(opQueueDrained, "the daemon began its exit once the queue_drained scripts succeeded; the drain stays owed to its successor", fields)
		return
	}
	err := o.deps.DB.ClearQueueDrainOwed(ctx, repo)
	o.leaveAdmission()
	if err != nil {
		log.Error(opQueueDrained, "could not clear the repository's drain once its queue_drained scripts succeeded; the drain stays owed", withField(fields, "error", err.Error()))
		return
	}
	log.Info(opQueueDrained, "the repository's queue_drained scripts all succeeded", fields)
}

// drainDue answers whether REPO's queue_drained scripts run now: its main
// checkout root, the scripts, and true once the repository's kernel lock is
// held for them. They run only when the repository owes them, its queue is
// not paused, nothing is in it, and no run of it stands in this process. Its
// store reads are ONE admission step, as the pump's own are.
func (o *orchestrator) drainDue(ctx context.Context, repo wsm.RepoKey) (string, []*repoconfigv1.DeployHookScript, bool) {
	if !o.enterAdmission() {
		return "", nil, false
	}
	defer o.leaveAdmission()
	log := o.deps.Log.Global()
	fields := dlog.Context{"repo": string(repo)}
	owed, err := o.deps.DB.QueueDrainOwed(ctx, repo)
	if err != nil {
		log.Error(opQueueDrained, "could not read whether the repository owes its drain", withField(fields, "error", err.Error()))
		return "", nil, false
	}
	if !owed {
		return "", nil, false
	}
	// A PAUSED QUEUE ADVANCES NOTHING, its drain included: resuming it kicks
	// the pump, which finds the debt then.
	paused, err := o.deps.DB.MergeQueuePaused(ctx, repo)
	if err != nil {
		log.Error(opQueueDrained, "could not read whether the repository's queue is paused", withField(fields, "error", err.Error()))
		return "", nil, false
	}
	if paused {
		log.Debug(opQueueDrained, "the repository owes its drain, but its queue is paused; the drain runs once it resumes", fields)
		return "", nil, false
	}
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		log.Error(opQueueDrained, "could not read the repository's merge queue", withField(fields, "error", err.Error()))
		return "", nil, false
	}
	o.mu.Lock()
	busy := o.running[repo] != nil || len(o.chains[repo]) > 0
	o.mu.Unlock()
	if len(entries) > 0 || busy {
		return "", nil, false
	}
	root, err := o.repositoryRootOf(ctx, repo)
	if err != nil {
		log.Error(opQueueDrained, "could not resolve the repository whose queue owes its drain; the drain stays owed", withField(fields, "error", err.Error()))
		return "", nil, false
	}
	fields["repo_root"] = root
	cfg, err := o.deps.DeployConfig.Load(root)
	if err != nil {
		log.Error(opQueueDrained, "could not read the deploy config of the repository whose queue owes its drain; the drain stays owed", withField(fields, "error", err.Error()))
		return "", nil, false
	}
	scripts := cfg.GetQueueDrained()
	if len(scripts) == 0 {
		if err := o.deps.DB.ClearQueueDrainOwed(ctx, repo); err != nil {
			log.Error(opQueueDrained, "could not clear the drain of a repository whose config runs nothing then", withField(fields, "error", err.Error()))
			return "", nil, false
		}
		log.Debug(opQueueDrained, "the repository's merge queue fully depleted; its config runs nothing then", fields)
		return "", nil, false
	}
	taken, err := o.holdRepoLock(repo)
	if err != nil {
		log.Error(opQueueDrained, "could not take the repository's queue lock for its queue_drained scripts; the drain stays owed", withField(fields, "error", err.Error()))
		return "", nil, false
	}
	if !taken {
		log.Info(opQueueDrained, "another daemon holds the repository's merge queue; the drain is left to it", fields)
		return "", nil, false
	}
	return root, scripts, true
}

// runDrainedScript runs the I'th queue_drained script from the repository's
// main checkout root ROOT, with no deploy environment, and reports whether it
// succeeded. Its whole output is kept among the queue's logs, and a failure's
// record carries the log's path and how the script ended.
func (o *orchestrator) runDrainedScript(ctx context.Context, repo wsm.RepoKey, root string, i int, script *repoconfigv1.DeployHookScript) bool {
	log := o.deps.Log.Global()
	logPath := filepath.Join(LogsDir(o.deps.StateDir), fmt.Sprintf("queue_drained-%d-%d.log", o.deps.Now().UnixNano(), i))
	fields := dlog.Context{"repo": string(repo), "repo_root": root, "index": i, "script": script.GetPath(),
		"timeout_seconds": script.GetTimeoutSeconds(), "log": logPath}
	ran := o.runOwnerlessScript(ctx, root, script, nil, logPath)
	fields["exit_code"], fields["wall_ms"] = ran.run.ExitCode, ran.run.Wall.Milliseconds()
	failure := ran.failure(script)
	if failure == "" && ran.logErr != nil {
		failure = ran.logErr.Error()
	}
	if failure != "" {
		log.Error(opQueueDrained, "a queue_drained script failed; the scripts after it do not run, and the drain stays owed",
			withField(fields, "failure", failure))
		return false
	}
	log.Info(opQueueDrained, "a queue_drained script succeeded", fields)
	return true
}

// repositoryRootOf resolves the main checkout root of the registered
// repository whose queue is REPO.
func (o *orchestrator) repositoryRootOf(ctx context.Context, repo wsm.RepoKey) (string, error) {
	registered, err := o.registeredQueues(ctx, opQueueDrained)
	if err != nil {
		return "", fmt.Errorf("merge: reading the repository registry: %w", err)
	}
	for _, q := range registered {
		if q.key == repo {
			return o.deps.Git.MainWorktree(ctx, q.repository.Dir)
		}
	}
	return "", fmt.Errorf("merge: no registered repository's merge queue is %s", repo)
}

// offerOwedDrains offers this daemon's pump every registered repository that
// owes its drain and is not among OFFERED, the queues the caller already
// offered: a queue with no rows left is otherwise never looked at again until
// its next merge, and a debt a restart or a handover carried over would wait
// for it. The boot (Recover) and a successor's takeover (ReviewQueues) both
// ask it, under their own OP.
func (o *orchestrator) offerOwedDrains(ctx context.Context, op string, offered []wsm.RepoKey) error {
	registered, err := o.registeredQueues(ctx, op)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read the repository registry to offer the owed queue drains", dlog.Context{"error": err.Error()})
		return fmt.Errorf("merge: offering the owed queue drains: %w", err)
	}
	for _, q := range registered {
		if slices.Contains(offered, q.key) {
			continue
		}
		owed, err := o.deps.DB.QueueDrainOwed(ctx, q.key)
		if err != nil {
			o.deps.Log.Global().Error(op, "could not read whether a repository owes its queue drain", dlog.Context{
				"repo": string(q.key), "repository": q.repository.Dir, "error": err.Error()})
			return fmt.Errorf("merge: reading the queue drain of %s: %w", q.repository.Dir, err)
		}
		if !owed {
			continue
		}
		o.deps.Log.Global().Info(op, "a repository with an empty merge queue owes its queue drain; its queue is offered to the pump", dlog.Context{
			"repo": string(q.key), "repository": q.repository.Dir})
		o.kick(q.key)
	}
	return nil
}
