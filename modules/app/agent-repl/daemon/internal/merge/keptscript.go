package merge

import (
	"context"
	"fmt"
	"path/filepath"
	"time"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/scriptrunner"
)

// This file is EVERY SCRIPT HOOK RUN OUTSIDE A DEPLOY RUN: a hold's release, a
// queue's drain, a finish-stage hook, and a creation-stage hook (the
// workspace verbs run those). A deploy run's own script hooks draw a tab as
// they write (deploy.go); these keep their whole output in one log among the
// queue's logs and are judged one way.

// KeptRun is how one script run outside a deploy run ended.
type KeptRun struct {
	// Run is the script's run: its exit, its timeout and its output.
	Run scriptrunner.Run
	// Err is why the script could not be run at all; nil when it ran.
	Err error
	// LogErr is why its log could not be written; nil when it was.
	LogErr error
}

// Failure is how the run failed, in the words a reader is told, or "" when
// the script exited 0. A log that could not be kept is not a failure here:
// a caller whose reader is promised the log decides that.
func (r KeptRun) Failure(script *repoconfigv1.HookScript) string {
	switch {
	case r.Err != nil:
		return r.Err.Error()
	case r.Run.TimedOut:
		return fmt.Sprintf("it ran past its %ds timeout", script.GetTimeoutSeconds())
	case r.Run.ExitCode != 0:
		return fmt.Sprintf("it exited %d", r.Run.ExitCode)
	}
	return ""
}

// RunKeptScript runs SCRIPT once from the repository's main checkout root
// REPOROOT, bounded by its timeout and with ENV beside the daemon's own, and
// keeps its whole output at logPath.
func RunKeptScript(ctx context.Context, runner HookScriptRunner, repoRoot string, script *repoconfigv1.HookScript, env []string, logPath string) KeptRun {
	run, err := runner.RunLinesWith(ctx, repoRoot, lifecycleconfig.ScriptArgv(repoRoot, script),
		scriptrunner.Options{Env: env, Timeout: time.Duration(script.GetTimeoutSeconds()) * time.Second}, nil)
	return KeptRun{Run: run, Err: err, LogErr: writeLog(logPath, run.Output)}
}

// LifecycleLogPath is where hook I of a creation or finish stage run AT keeps
// its output, among the queue's logs (LogsDir) so their retention is the
// queue's.
func LifecycleLogPath(stateDir string, stage lifecycleconfig.Stage, at time.Time, i int) string {
	if stage != lifecycleconfig.StageCreation && stage != lifecycleconfig.StageFinish {
		panic("merge: only a creation or finish hook keeps a lifecycle log; a deploy's hooks keep theirs by ledger")
	}
	return filepath.Join(LogsDir(stateDir), fmt.Sprintf("%s-%d-%d.log", stage.Key(), at.UnixNano(), i))
}
