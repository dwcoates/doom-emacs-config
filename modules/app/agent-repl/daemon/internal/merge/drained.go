package merge

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"slices"
	"time"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// This file is a repository's QUEUE DRAIN: the scripts its deploy config runs
// once its merge queue fully depletes (repoconfig.v1.LifecycleConfig.queue_drained),
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
// A FAILED DRAIN IS OFFERED AGAIN ON ITS OWN. A script that failed (a release
// push while GitHub was unreachable, 2026-11-25) leaves the queue empty, and
// an empty queue is otherwise looked at again only once another merge lands:
// the debt then stood for eleven hours. So the failure arms a re-offer of the
// queue to its pump (retryDrain), after a wait that doubles with each failure
// in a row from DrainRetryFirst to DrainRetryMost, and the drain's success,
// or the debt's clearing any other way, disarms it.
//
// NOTHING LANDS WHILE THEY RUN. They run on the admission pump, between its
// last empty look and its going idle, so this daemon admits nothing until they
// end (a kick meanwhile is remembered, and admitted after), and under the
// repository's kernel lock, so no other daemon admits either.

const opQueueDrained = "daemon.merge.queue_drained"

// The waits before a failed drain's queue is offered to its pump again: the
// first after one failure, doubling with each failure in a row up to the
// most. The scripts that fail on their own are the ones that reach a remote
// (agent-repl's release push to GitHub), whose outages run from minutes to
// hours, so the first re-offer comes soon, and the most bounds a long
// outage's failure records to one an hour.
const (
	DrainRetryFirst = 5 * time.Minute
	DrainRetryMost  = time.Hour
)

// drainRetry is one repository's re-offer of its failed drain: how many of
// its drains failed in a row, and the stop of the timer armed for the next,
// nil once it fired.
type drainRetry struct {
	failures int
	stop     func() bool
}

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
			o.retryDrain(repo, fields)
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
	err := o.clearDrainOwed(ctx, repo)
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
func (o *orchestrator) drainDue(ctx context.Context, repo wsm.RepoKey) (string, []*repoconfigv1.HookScript, bool) {
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
	paused, err := o.queueHeld(ctx, repo)
	if err != nil {
		log.Error(opQueueDrained, "could not read whether the repository's queue is paused", withField(fields, "error", err.Error()))
		return "", nil, false
	}
	if paused {
		log.Debug(opQueueDrained, "the repository owes its drain, but its queue is held; the drain runs once it is taken on", fields)
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
	if errors.Is(err, errQueueUnregistered) {
		// A REPOSITORY FORGOTTEN WHILE ITS DRAIN WAS OWED owes nothing: its
		// last workspace went with its queue empty, and the debt, left, would
		// be asked after on every pump kick forever.
		if err := o.clearDrainOwed(ctx, repo); err != nil {
			log.Error(opQueueDrained, "could not clear the drain of a repository that is no longer registered", withField(fields, "error", err.Error()))
			return "", nil, false
		}
		log.Info(opQueueDrained, "the repository whose queue owed its drain is no longer registered; the drain is dropped", fields)
		return "", nil, false
	}
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
		if err := o.clearDrainOwed(ctx, repo); err != nil {
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

// clearDrainOwed records that REPO owes its drain no longer, and disarms the
// re-offer a failed drain armed: the repository's next failure waits from
// DrainRetryFirst again.
func (o *orchestrator) clearDrainOwed(ctx context.Context, repo wsm.RepoKey) error {
	if err := o.deps.DB.ClearQueueDrainOwed(ctx, repo); err != nil {
		return err
	}
	o.mu.Lock()
	defer o.mu.Unlock()
	if retry := o.drainRetries[repo]; retry != nil && retry.stop != nil {
		retry.stop()
	}
	delete(o.drainRetries, repo)
	return nil
}

// retryDrain arms the re-offer of REPO's queue to its pump once one of its
// queue_drained scripts failed, replacing any armed before: the wait is
// DrainRetryFirst after one failure in a row, doubled for each failure after
// it, never more than DrainRetryMost. An exiting daemon arms nothing, since
// its successor's boot offers every owed drain (offerOwedDrains).
func (o *orchestrator) retryDrain(repo wsm.RepoKey, fields dlog.Context) {
	o.mu.Lock()
	if o.draining {
		o.mu.Unlock()
		return
	}
	// EACH ARMING IS ITS OWN RECORD, so a replaced timer that fires anyway
	// finds a record other than its own standing, and kicks nothing.
	failures := 1
	if prev := o.drainRetries[repo]; prev != nil {
		if prev.stop != nil {
			prev.stop()
		}
		failures = prev.failures + 1
	}
	retry := &drainRetry{failures: failures}
	wait := drainRetryWait(failures)
	retry.stop = o.afterFunc(wait, func() { o.drainRetryFired(repo, retry) })
	o.drainRetries[repo] = retry
	o.mu.Unlock()
	o.deps.Log.Global().Info(opQueueDrained, "the failed drain's queue is offered to its pump again once its wait ends",
		withField(withField(fields, "failures", failures), "retry_in", wait.String()))
}

// drainRetryWait is the wait before a failed drain's re-offer after FAILURES
// failures in a row.
func drainRetryWait(failures int) time.Duration {
	wait := DrainRetryFirst
	for i := 1; i < failures && wait < DrainRetryMost; i++ {
		wait *= 2
	}
	return min(wait, DrainRetryMost)
}

// drainRetryFired offers REPO's queue to its pump once the wait RETRY armed
// ended, unless a later failure re-armed it, the debt was cleared, or the
// daemon began its exit meanwhile.
func (o *orchestrator) drainRetryFired(repo wsm.RepoKey, retry *drainRetry) {
	o.mu.Lock()
	current := o.drainRetries[repo] == retry && !o.draining
	if current {
		retry.stop = nil
	}
	failures := retry.failures
	o.mu.Unlock()
	if !current {
		return
	}
	o.deps.Log.Global().Info(opQueueDrained, "the failed drain's wait ended; its queue is offered to its pump again",
		dlog.Context{"repo": string(repo), "failures": failures})
	o.kick(repo)
}

// stopDrainRetriesLocked disarms every armed re-offer of a failed drain, for
// the daemon's exit. The caller holds mu.
func (o *orchestrator) stopDrainRetriesLocked() {
	for repo, retry := range o.drainRetries {
		if retry.stop != nil {
			retry.stop()
		}
		delete(o.drainRetries, repo)
	}
}

// runDrainedScript runs the I'th queue_drained script from the repository's
// main checkout root ROOT, with no deploy environment, and reports whether it
// succeeded. Its whole output is kept among the queue's logs, and a failure's
// record carries the log's path and how the script ended.
func (o *orchestrator) runDrainedScript(ctx context.Context, repo wsm.RepoKey, root string, i int, script *repoconfigv1.HookScript) bool {
	log := o.deps.Log.Global()
	logPath := filepath.Join(LogsDir(o.deps.StateDir), fmt.Sprintf("queue_drained-%d-%d.log", o.deps.Now().UnixNano(), i))
	fields := dlog.Context{"repo": string(repo), "repo_root": root, "index": i, "script": script.GetPath(),
		"timeout_seconds": script.GetTimeoutSeconds(), "log": logPath}
	ran := RunKeptScript(ctx, o.deps.HookRunner, root, script, nil, logPath)
	fields["exit_code"], fields["wall_ms"] = ran.Run.ExitCode, ran.Run.Wall.Milliseconds()
	failure := ran.Failure(script)
	if failure == "" && ran.LogErr != nil {
		failure = ran.LogErr.Error()
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
	return "", fmt.Errorf("%w: %s", errQueueUnregistered, repo)
}

// errQueueUnregistered is repositoryRootOf's answer for a queue no registered
// repository has: the repository was forgotten.
var errQueueUnregistered = errors.New("merge: no registered repository's merge queue is")

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
