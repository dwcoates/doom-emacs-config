package shimclient

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"sync"
	"syscall"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/envc"
	"claude-repld/internal/ids"
)

// The spawn environment's own names, beside envc's four contracts. They are
// exported because the shim reads them by these exact spellings.
const (
	// EnvConfigDir is CLAUDE_CONFIG_DIR: the account root the shim's vendor
	// SDK reads its credentials and settings from.
	EnvConfigDir = "CLAUDE_CONFIG_DIR"
	// EnvShimBuildSHA is SHIM_BUILD_SHA: the bundle the shim was built from,
	// which the rollout's build-staleness check compares against.
	EnvShimBuildSHA = "SHIM_BUILD_SHA"
	// EnvSessionID is AGENT_REPL_SESSION_ID: the host session identity, for
	// LOG CORRELATION only. Session facts still travel exclusively in
	// StartSession.
	EnvSessionID = "AGENT_REPL_SESSION_ID"
)

// shimLogFD is the descriptor the shim writes its own log to: fd 3, the
// already-open sink the daemon's log surfaces own.
const shimLogFD = 3

// ActorStandDown is the kill attribution the supervisor's own sweep records.
// It is how the exit of an in-flight spawn is told from a crash: nothing else
// in the daemon knew that process existed, so nothing else could have named
// the actor.
const ActorStandDown = "shimclient.standdown"

// supervisor is the daemon's one shim supervisor.
type supervisor struct {
	surfaces  dlog.Surfaces
	back      backoff
	grace     time.Duration
	lockProbe func(workspaceDir string) (free bool, err error)

	// mu guards held.
	mu sync.Mutex
	// held is EVERY process this supervisor started and still owns, entered
	// the instant cmd.Start returns and left only when the process is gone or
	// has been handed to a successor.
	//
	// IT EXISTS BECAUSE NOTHING ELSE KNOWS. A spawn reaches the workspace
	// fleet's session map only after bring-up returns healthy AND the shim has
	// answered StartSession; for the whole window before that the process is
	// running, holding the workspace's kernel lock and ~95 MiB, and the
	// supervisor is its ONLY witness. An immediate shutdown that walked the
	// registered sessions alone therefore left it standing forever -- measured
	// over a 10s grace as "10.0xx s, 1 left" -- which is what
	// StandDownEverySpawn sweeps.
	held map[*client]struct{}
}

// hold enters a freshly started process in the supervisor's own registry and
// arms its release, so the registry empties itself on the process's death or
// its handover without anyone having to remember to.
func (s *supervisor) hold(c *client) {
	s.mu.Lock()
	if s.held == nil {
		s.held = make(map[*client]struct{})
	}
	s.held[c] = struct{}{}
	s.mu.Unlock()
	c.release = func() {
		s.mu.Lock()
		delete(s.held, c)
		s.mu.Unlock()
	}
}

// heldNow is a snapshot of the registry, taken so the sweep below never holds
// the supervisor's lock while it signals and waits.
func (s *supervisor) heldNow() []*client {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]*client, 0, len(s.held))
	for c := range s.held {
		out = append(out, c)
	}
	return out
}

// StandDownEverySpawn force-kills every process this supervisor started and
// still owns, and reports every kill that failed.
//
// IT IS THE `now` SHUTDOWN'S SWEEP, AND ONLY THAT. A BOUNCE hands its shims to
// a successor that adopts them, and the handover's per-workspace transfer says
// so by calling Client.Detach -- which leaves the process running and takes it
// OUT of the registry above. So a transferred shim is not in this set, and the
// bounce does not call this at all: `rollout.controller.Handover` exits through
// its own path and never reaches drain.ShutdownNow. The two cases are
// therefore distinguished twice over, by the caller and by the registry.
//
// EVERY WAIT IS BOUNDED TWICE. ctx bounds the whole sweep -- the drain gives it
// StandBound -- and each process additionally gets the supervisor's own kill
// grace, so one process whose reap never lands cannot starve its siblings of
// the remaining budget. The kill is FORCED: a graceful stand-down of a process
// that has not even finished coming up has nothing to be graceful about, and
// this daemon is already exiting.
//
// A failed kill is RETURNED, never swallowed: a leaked shim holds the
// workspace lock that refuses the next session, and the caller's record is the
// only thing that will ever say so.
func (s *supervisor) StandDownEverySpawn(ctx context.Context, reason string) error {
	held := s.heldNow()
	if len(held) == 0 {
		return nil
	}
	attr := KillAttribution{Actor: ActorStandDown, Reason: reason, Force: true}
	var errs []error
	for _, c := range held {
		c.log.Warn("daemon.shimclient.standdown", "a spawn this daemon never registered is being stood down", dlog.Context{
			"workspace_id": string(c.ws), "pid": c.PID(), "reason": reason,
		})
		if err := c.killWithin(ctx, s.grace, attr); err != nil {
			errs = append(errs, fmt.Errorf("shimclient: stand down the spawn for %q (pid %d): %w", c.ws, c.PID(), err))
		}
	}
	return errors.Join(errs...)
}

// Spawn starts a shim, dials it, and returns once WatchSession is connected
// and the first pushed diagnostics arm says healthy.
func (s *supervisor) Spawn(ctx context.Context, spec Spec) (Client, error) {
	if err := validateSpec(spec); err != nil {
		return nil, err
	}
	contracts := envc.Load()
	if !spec.Fake {
		if err := envc.NewVendorGuard(contracts).Check("shim-spawn"); err != nil {
			return nil, err
		}
	}

	log, err := s.surfaces.Workspace(spec.WorkspaceDir)
	if err != nil {
		return nil, fmt.Errorf("shimclient: resolve workspace log sink for %q: %w", spec.WorkspaceDir, err)
	}
	log = log.With(dlog.Context{"workspace_id": string(spec.WorkspaceID)})

	c := newClient(log, spec.WorkspaceID, spec.UDSPath, s.back, s.workspaceProbe(spec.WorkspaceDir))
	c.grace = s.grace
	c.stderr = newRing(stderrRingBytes)

	args := shimArgs(spec)
	cmd := exec.Command(spec.NodeBin, args...)
	cmd.Dir = spec.WorkspaceDir
	cmd.Env = spawnEnv(spec, contracts)
	cmd.Stdout = nil
	cmd.Stderr = c.stderr
	cmd.ExtraFiles = []*os.File{spec.LogSink}
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}

	log.Debug("daemon.shimclient.spawn", "spawning shim", dlog.Context{
		"node": spec.NodeBin, "argv": args, "cwd": spec.WorkspaceDir,
		"uds": spec.UDSPath, "store_socket": spec.StoreSocket, "fake": spec.Fake,
	})
	if err := cmd.Start(); err != nil {
		log.Error("daemon.shimclient.spawn", "spawn failed", dlog.Context{
			"node": spec.NodeBin, "error": err.Error(),
		})
		return nil, fmt.Errorf("shimclient: start %q: %w", spec.NodeBin, err)
	}

	c.mu.Lock()
	c.cmd = cmd
	c.pid = cmd.Process.Pid
	c.pgid = cmd.Process.Pid
	c.mu.Unlock()

	// THE REGISTRY IS ENTERED BEFORE THE REAPER IS STARTED, and before
	// bring-up, because from cmd.Start onward this process exists and the
	// supervisor is the only thing that knows it. Held first, released by the
	// reaper: the other order is a window in which a death deregisters
	// nothing.
	s.hold(c)

	log.Info("daemon.shimclient.spawn", "shim spawned", dlog.Context{
		"pid": cmd.Process.Pid, "uds": spec.UDSPath,
	})
	go c.reap()

	if err := c.bringUp(ctx); err != nil {
		c.abandonBringUp(err)
		return nil, err
	}
	return c, nil
}

// Adopt dials a shim that is already running — a crash boot's surviving
// process, or a handover's transferred one — and supervises it without
// spawning.
func (s *supervisor) Adopt(ctx context.Context, ws ids.WorkspaceID, workspaceDir, udsPath string) (Client, error) {
	switch {
	case strings.TrimSpace(string(ws)) == "":
		return nil, &SpecError{Field: "WorkspaceID", Reason: "is empty"}
	case strings.TrimSpace(workspaceDir) == "":
		return nil, &SpecError{Field: "WorkspaceDir", Reason: "is empty"}
	case strings.TrimSpace(udsPath) == "":
		return nil, &SpecError{Field: "UDSPath", Reason: "is empty"}
	}

	log, err := s.surfaces.Workspace(workspaceDir)
	if err != nil {
		return nil, fmt.Errorf("shimclient: resolve workspace log sink for %q: %w", workspaceDir, err)
	}
	log = log.With(dlog.Context{"workspace_id": string(ws)})

	c := newClient(log, ws, udsPath, s.back, s.workspaceProbe(workspaceDir))
	c.grace = s.grace
	c.stderr = newRing(stderrRingBytes)

	log.Info("daemon.shimclient.adopt", "adopting a running shim", dlog.Context{"uds": udsPath})
	if err := c.bringUp(ctx); err != nil {
		c.cancelMonitor()
		c.link.close()
		log.Error("daemon.shimclient.adopt", "adoption failed", dlog.Context{
			"uds": udsPath, "error": err.Error(),
		})
		return nil, err
	}
	// THE PID IS LEARNED HERE, from the socket's peer credential, so every
	// record this client writes names the process it is actually driving. The
	// KILL does not trust this number — it reads a fresh one at the instant it
	// signals, because a remembered pid can be recycled — so a platform that
	// cannot answer costs observability here and a loud refusal there, never a
	// silently unstoppable shim.
	if pid, pidErr := socketPeerPID(udsPath); pidErr != nil {
		log.Warn("daemon.shimclient.adopt", "could not learn the adopted shim's pid from its socket", dlog.Context{
			"uds": udsPath, "error": pidErr.Error(),
		})
	} else {
		c.mu.Lock()
		c.pid = pid
		c.mu.Unlock()
		log.Info("daemon.shimclient.adopt", "adopted a running shim", dlog.Context{"uds": udsPath, "pid": pid})
	}
	return c, nil
}

// workspaceProbe binds the injected lock probe to one workspace directory, or
// yields nil when no witness was supplied.
func (s *supervisor) workspaceProbe(workspaceDir string) func(ids.WorkspaceID) (bool, error) {
	if s.lockProbe == nil {
		return nil
	}
	return func(ids.WorkspaceID) (bool, error) { return s.lockProbe(workspaceDir) }
}

// abandonBringUp stops a spawned process whose bring-up failed, so a failed
// spawn never leaves an orphan holding the workspace lock.
func (c *client) abandonBringUp(cause error) {
	// THE ABANDONMENT IS RECORDED ON BOTH PATHS. A bring-up that failed
	// because the process had already died leaves nothing to stop, but it is
	// the same failure and the workspace's own log is where it belongs: the
	// exit record says the process is gone, and only this one says the
	// bring-up was given up on because of it.
	if c.exitedAlready() {
		c.log.Warn("daemon.shimclient.spawn", "bring-up failed; the spawned shim is already gone", dlog.Context{
			"pid": c.PID(), "error": cause.Error(),
		})
		c.cancelMonitor()
		return
	}
	c.log.Warn("daemon.shimclient.spawn", "bring-up failed; stopping the spawned shim", dlog.Context{
		"pid": c.PID(), "error": cause.Error(),
	})
	if err := c.Kill(KillAttribution{
		Actor:  "shimclient.bringup",
		Reason: "bring-up failed: " + cause.Error(),
	}); err != nil {
		c.log.Error("daemon.shimclient.spawn", "stopping the spawned shim failed", dlog.Context{
			"pid": c.PID(), "error": err.Error(),
		})
	}
	c.cancelMonitor()
}

// shimArgs is the shim's argv after the node binary, per the common spawn
// contract. The store socket is ALWAYS passed explicitly.
func shimArgs(spec Spec) []string {
	args := []string{
		spec.MainJS,
		"--listen", spec.UDSPath,
		"--store-socket", spec.StoreSocket,
		"--log-fd", fmt.Sprintf("%d", shimLogFD),
	}
	if spec.Fake {
		args = append(args, "--fake")
	}
	return args
}

// spawnEnv is the shim's environment: the daemon's OWN environment passed
// through, with the contracted variables set on top. Never an allowlist — a
// test-only channel and the user's own shell must both reach the child — and
// an inherited value of a contracted name is OVERRIDDEN, never duplicated.
func spawnEnv(spec Spec, contracts envc.Contracts) []string {
	overrides := [][2]string{
		{EnvConfigDir, spec.ConfigDir},
		{envc.EnvOwned, "1"},
		{EnvShimBuildSHA, spec.ShimBuildSHA},
	}
	if stateDir := stateDirFor(spec, contracts); stateDir != "" {
		overrides = append(overrides, [2]string{envc.EnvStateDir, stateDir})
	}
	if spec.SessionID != "" {
		overrides = append(overrides, [2]string{EnvSessionID, spec.SessionID})
	}
	if spec.ForbidVendor || contracts.ForbidVendorCalls() {
		overrides = append(overrides, [2]string{envc.EnvForbidVendorCalls, "1"})
	}

	overridden := make(map[string]bool, len(overrides))
	for _, kv := range overrides {
		overridden[kv[0]] = true
	}

	env := make([]string, 0, len(os.Environ())+len(overrides))
	for _, entry := range os.Environ() {
		name, _, ok := strings.Cut(entry, "=")
		if ok && overridden[name] {
			continue
		}
		env = append(env, entry)
	}
	for _, kv := range overrides {
		env = append(env, kv[0]+"="+kv[1])
	}
	return env
}

// stateDirFor is the state root the child must resolve: the spec's when the
// caller stated one, otherwise this daemon's own — the two must never diverge.
func stateDirFor(spec Spec, contracts envc.Contracts) string {
	if spec.StateDir != "" {
		return spec.StateDir
	}
	return contracts.StateDir()
}

// validateSpec is Spec's base function. The spawn contract has no optional
// parts, so an incomplete Spec is refused before a process exists.
func validateSpec(spec Spec) error {
	required := []struct {
		field string
		value string
	}{
		{"WorkspaceID", string(spec.WorkspaceID)},
		{"WorkspaceDir", spec.WorkspaceDir},
		{"UDSPath", spec.UDSPath},
		{"StoreSocket", spec.StoreSocket},
		{"ConfigDir", spec.ConfigDir},
		{"ShimBuildSHA", spec.ShimBuildSHA},
		{"NodeBin", spec.NodeBin},
		{"MainJS", spec.MainJS},
	}
	for _, r := range required {
		if strings.TrimSpace(r.value) == "" {
			return &SpecError{Field: r.field, Reason: "is empty"}
		}
	}
	if spec.LogSink == nil {
		return &SpecError{Field: "LogSink", Reason: "is nil; fd 3 must be the already-open shim log sink"}
	}
	return nil
}

// bringUp dials the shim until the link is connected AND the first pushed
// diagnostics arm says healthy. A process that dies ends this at once with its
// exit decoding and stderr ring — the correlation is a select on the death
// channel, never a timeout.
func (c *client) bringUp(parent context.Context) error {
	c.link.publish(LinkDialing)
	for attempt := 0; ; attempt++ {
		if err := c.deathOrContext(parent); err != nil {
			return err
		}
		stream, err := c.openSupervisedSession(parent)
		if err != nil {
			if stop := c.afterFailedDial(parent, err, attempt); stop != nil {
				return stop
			}
			continue
		}
		c.link.publish(LinkConnected)

		frames, errs := recvLoop(stream, c.monitorCtx.Done())
		err = c.awaitHealthy(parent, frames, errs)
		if err == nil {
			go c.monitor(stream, frames, errs)
			return nil
		}
		stream.Close()
		if stop := c.afterFailedDial(parent, err, attempt); stop != nil {
			return stop
		}
	}
}

// afterFailedDial decides whether a failed dial ends the loop, and waits out
// the backoff when it does not. A non-nil answer is the reason to stop.
func (c *client) afterFailedDial(ctx context.Context, cause error, attempt int) error {
	// A retried attempt is an ordinary branch of the ladder: the shim's socket
	// simply does not exist yet on the first attempt of every spawn. The reason
	// the ladder STOPS is returned from here and reported by the caller.
	c.log.Debug("daemon.shimclient.dial", "shim dial failed; retrying", dlog.Context{
		"uds": c.udsPath, "attempt": attempt, "error": cause.Error(),
	})
	if err := c.deathOrContext(ctx); err != nil {
		return err
	}
	if c.witnessAdoptedDeath(cause) {
		return c.deathError()
	}
	if err := c.back.wait(ctx, c.dead, attempt); err != nil {
		if err == errProcessDead {
			return c.deathError()
		}
		return err
	}
	return nil
}
