package reload

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitexec"
)

// Plan is one decided redeploy: what the merge touched, and over which range.
type Plan struct {
	// Workspace is the merged workspace key, carried for the log record only.
	Workspace string
	// Components is the classified set, in componentOrder.
	Components []Component
	// Range is the `base..head` spec the classification was derived from.
	Range string
}

// Launcher starts a decided redeploy.
//
// It is an interface for one reason: the production implementation spawns a
// script that KILLS THIS DAEMON, and no unit test may ever come close to
// running it. Every test in this package injects a recording fake here.
type Launcher interface {
	Launch(ctx context.Context, plan Plan) error
}

// ScriptConfig constructs a DetachedScript.
type ScriptConfig struct {
	// Script is the absolute path of bin/deploy-all.sh. Required.
	Script string
	// Dir is the working directory the script runs in, which is the checkout it
	// will rebuild. Required.
	Dir string
	// LogDir is where the redeploy's output is written. Required. It lives
	// under the daemon's state root rather than anywhere inside the checkout,
	// because the checkout is what the script is busy rebuilding.
	LogDir string
	// Logf receives this package's canonical log records. Required.
	Logf dlog.Logf
	// Now supplies the log filename's timestamp. Optional; defaults to
	// time.Now.
	Now func() time.Time
}

// DetachedScript runs bin/deploy-all.sh out of this daemon's process group.
//
// THE DETACHMENT IS THE POINT. deploy-all.sh's step 5 restarts the very daemon
// that spawned it, so the child must outlive its parent's death:
//
//   - Setpgid puts the child in a NEW process group. A signal delivered to the
//     daemon's group (which is how a supervisor or a shell stops the daemon)
//     therefore never reaches the deploy.
//   - The command is built with exec.Command and NOT exec.CommandContext. The
//     only context available here is the merge coordinator's, which is
//     cancelled during the very shutdown the deploy causes; binding the child
//     to it would have the deploy kill itself halfway through.
//   - Output goes to a FILE, not to inherited pipes. Inherited pipes die with
//     the parent, and the deploy's log is the only record of what happened
//     across the bounce.
//   - Stdin is nil (/dev/null), so the orphaned child can never block on a
//     terminal read.
type DetachedScript struct {
	script string
	dir    string
	logDir string
	logf   dlog.Logf
	now    func() time.Time
}

var _ Launcher = (*DetachedScript)(nil)

// NewDetachedScript validates cfg. Every missing dependency is a hard
// construction error: a redeploy launcher that cannot log, or that does not
// know which script to run, would fail at the one moment nobody is watching.
func NewDetachedScript(cfg ScriptConfig) (*DetachedScript, error) {
	switch {
	case cfg.Script == "":
		return nil, fmt.Errorf("reload: DetachedScript needs a Script path")
	case cfg.Dir == "":
		return nil, fmt.Errorf("reload: DetachedScript needs a Dir")
	case cfg.LogDir == "":
		return nil, fmt.Errorf("reload: DetachedScript needs a LogDir")
	case cfg.Logf == nil:
		return nil, fmt.Errorf("reload: DetachedScript needs a Logf")
	}
	now := cfg.Now
	if now == nil {
		now = time.Now
	}
	return &DetachedScript{
		script: cfg.Script,
		dir:    cfg.Dir,
		logDir: cfg.LogDir,
		logf:   cfg.Logf,
		now:    now,
	}, nil
}

// args renders the script arguments for a plan. deploy-all.sh has no
// per-component selection flags — its only build-scoping switches are --force,
// --no-bounce and --no-daemon-bounce, none of which narrows WHAT is built — so
// every classified redeploy is a full ordered run. The classification's job is
// to decide whether to run at all, plus the one thing the script genuinely
// cannot work out for itself: which elisp range to hot-load.
func (s *DetachedScript) args(plan Plan) []string {
	if NeedsElispReload(plan.Components) {
		return []string{"--elisp", plan.Range}
	}
	return nil
}

// logPath is the file this redeploy's output is written to.
func (s *DetachedScript) logPath() string {
	return filepath.Join(s.logDir, "deploy-"+s.now().UTC().Format("20060102T150405.000Z")+".log")
}

// command builds the exec.Cmd without starting it. Split out from Launch so the
// detachment properties can be asserted on the built command instead of by
// spawning anything.
func (s *DetachedScript) command(plan Plan, out *os.File) *exec.Cmd {
	cmd := exec.Command(s.script, s.args(plan)...) //nolint:gosec // the script path is this daemon's own checkout, resolved at construction
	cmd.Dir = s.dir
	cmd.Stdin = nil
	cmd.Stdout = out
	cmd.Stderr = out
	// The daemon may itself have been launched from a git hook, and deploy-all.sh
	// runs git against its own checkout. The same repository bindings this
	// package strips from its probes are stripped from the deploy.
	cmd.Env = gitexec.StripEnv(os.Environ())
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	return cmd
}

// Launch starts the redeploy and returns as soon as it is running.
//
// It does NOT wait for the deploy: the deploy's final act is to kill this
// process. A goroutine reaps the child purely for observability, so a deploy
// that FAILS without bouncing the daemon still gets its exit status into the
// canonical log rather than vanishing.
func (s *DetachedScript) Launch(ctx context.Context, plan Plan) error {
	if err := os.MkdirAll(s.logDir, 0o755); err != nil {
		s.logf("reload: redeploy log dir FAILED {ws=%s dir=%s}: %v", plan.Workspace, s.logDir, err)
		return fmt.Errorf("reload: create redeploy log dir %s: %w", s.logDir, err)
	}
	path := s.logPath()
	out, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		s.logf("reload: redeploy log file FAILED {ws=%s path=%s}: %v", plan.Workspace, path, err)
		return fmt.Errorf("reload: open redeploy log %s: %w", path, err)
	}
	cmd := s.command(plan, out)
	// Logged BEFORE the spawn, because the spawn's whole purpose is to end this
	// process: a record written afterwards may never be written at all.
	s.logf("reload: LAUNCHING self-redeploy {ws=%s components=%s range=%s cmd=%s log=%s}",
		plan.Workspace, strings.Join(Strings(plan.Components), ","), plan.Range,
		strings.Join(append([]string{s.script}, s.args(plan)...), " "), path)
	if err := cmd.Start(); err != nil {
		closeErr := out.Close()
		s.logf("reload: self-redeploy LAUNCH FAILED {ws=%s cmd=%s log=%s close_err=%v}: %v",
			plan.Workspace, s.script, path, closeErr, err)
		return fmt.Errorf("reload: start %s: %w", s.script, err)
	}
	pid := cmd.Process.Pid
	s.logf("reload: self-redeploy running {ws=%s pid=%d log=%s}", plan.Workspace, pid, path)
	go s.reap(cmd, out, plan, pid, path)
	return nil
}

// reap waits for a deploy that did not manage to bounce the daemon and records
// how it ended. When the deploy succeeds this goroutine simply dies with the
// process, which is the expected outcome.
func (s *DetachedScript) reap(cmd *exec.Cmd, out *os.File, plan Plan, pid int, path string) {
	waitErr := cmd.Wait()
	closeErr := out.Close()
	if waitErr != nil {
		s.logf("reload: self-redeploy EXITED NON-ZERO without bouncing the daemon {ws=%s pid=%d log=%s close_err=%v}: %v",
			plan.Workspace, pid, path, closeErr, waitErr)
		return
	}
	s.logf("reload: self-redeploy exited 0 without bouncing the daemon {ws=%s pid=%d log=%s close_err=%v}",
		plan.Workspace, pid, path, closeErr)
}
