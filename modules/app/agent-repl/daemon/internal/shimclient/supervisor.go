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

// shimSpawnGateFD is the descriptor the shim waits on before it binds: fd 4,
// the read end of the spawn gate, second of cmd.ExtraFiles. See Spawn.
const shimSpawnGateFD = 4

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

	// mu guards held and standingDown, and it is held ACROSS a spawn's
	// cmd.Start so the latch and the registry cannot be raced: see Spawn.
	mu sync.Mutex
	// standingDown latches at the first sweep and never clears. The process is
	// exiting; there is no state after it in which a new spawn is wanted.
	standingDown bool
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
	s.holdLocked(c)
	s.mu.Unlock()
}

// holdLocked is hold's body, for the spawn that already holds the lock.
func (s *supervisor) holdLocked(c *client) {
	if s.held == nil {
		s.held = make(map[*client]struct{})
	}
	s.held[c] = struct{}{}
	c.release = func() {
		s.mu.Lock()
		delete(s.held, c)
		s.mu.Unlock()
	}
}

// BeginStandDown latches the supervisor's stand-down WITHOUT sweeping
// anything, and it is what an immediate shutdown calls FIRST -- before it
// walks the registered sessions, not only when it reaches the spawn sweep.
//
// THE LATCH IS THE SIGNAL EVERY CLIENT READS, and the walk is only one of the
// ways this daemon ends a shim. Latched by the sweep alone, every departure
// the walk itself caused landed while the latch was still false, so a client
// the walk did not reach -- an adopted survivor of a refused StartSession,
// which no session row names -- read this daemon's own teardown as a death:
// `daemon.shimclient.exit` ERROR "shim died" plus `daemon.shimclient.redial`
// WARN "redial stopped", measured eight times over realtest run
// 2026-09-13T18:31:58.
//
// It is IDEMPOTENT and it NEVER CLEARS: the process is exiting, and there is
// no state after it in which a new spawn is wanted. It answers whether this
// call was the one that latched it, so a caller can record the transition
// rather than every re-statement of it.
func (s *supervisor) BeginStandDown() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.standingDown {
		return false
	}
	s.standingDown = true
	return true
}

// StandingDown answers the latch. Every client this supervisor handed out
// reads it, and so does the workspace bring-up, which must not start a shim
// nothing will be left to stop.
func (s *supervisor) StandingDown() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.standingDown
}

// SpawnedFor answers whether this supervisor still owns a shim it STARTED for
// this workspace, and the pid of the first one it finds.
//
// IT IS THE ADOPTION'S OWN GUARD. A shim this daemon spawned and still
// supervises must never be adopted a second time by the same daemon: that is
// ONE process with TWO clients, which is the duplicate-client shape measured
// on 2026-09-13 (a `Fleet.Start` whose StartSession refused returned before
// the client was remembered, left its shim serving, and the next bring-up
// found "lock free, socket live" and adopted the very process the supervisor
// was still holding). The registry is the only place that knows, because a
// spawn reaches the fleet's session map only after StartSession answers.
//
// It reports the LIVE set and nothing else: a dead process leaves the registry
// on its exit decode, and a transferred one leaves it at `Detach`, so a
// successor daemon's adoption of a handed-over shim is not this case.
func (s *supervisor) SpawnedFor(ws ids.WorkspaceID) (int, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	for c := range s.held {
		if c.ws == ws {
			return c.PID(), true
		}
	}
	return 0, false
}

// heldNow is a snapshot of the registry. The sweep below takes its own under
// the same lock that latches it; this is the read every other caller uses.
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
	// THE LATCH AND THE SNAPSHOT ARE TAKEN TOGETHER, and that is what closes
	// the window a sweep on its own leaves open. A bring-up that had not yet
	// reached cmd.Start when the snapshot was taken would otherwise start its
	// shim just after this walked past, and nothing would ever stand it down:
	// measured on the Emacs e2e layer, a SubmitPrompt whose spawn was still
	// probing the workspace lock when the shutdown landed left a node shim
	// running with no daemon left to own it. Under one lock there are exactly
	// two cases and no third: a spawn that has started is in `held' and is
	// swept here, and a spawn that has not is refused with ErrStandingDown.
	s.mu.Lock()
	s.standingDown = true
	held := make([]*client, 0, len(s.held))
	for c := range s.held {
		held = append(held, c)
	}
	s.mu.Unlock()
	if len(held) == 0 {
		return nil
	}
	attr := KillAttribution{Actor: ActorStandDown, Reason: reason, Force: true}
	var errs []error
	for _, c := range held {
		// ONE OF THE TWO CASES THE LATCH ABOVE LEAVES, and the expected one:
		// a spawn in flight when the shutdown landed. The sweep exists
		// precisely to catch it, so catching it is the sweep succeeding. The
		// FAILED kill below is what is loud, because a leaked shim holds the
		// workspace lock that refuses the next session.
		c.log.Info("daemon.shimclient.standdown", "a spawn this daemon never registered is being stood down", dlog.Context{
			"workspace_id": string(c.ws), "pid": c.PID(), "reason": reason,
		})
		if err := c.killWithin(ctx, s.grace, attr); err != nil {
			errs = append(errs, fmt.Errorf("shimclient: stand down the spawn for %q (pid %d): %w", c.ws, c.PID(), err))
		}
	}
	return errors.Join(errs...)
}

// Spawn starts a shim, dials it, and returns once WatchSession is connected
// and the shim has pushed its first diagnostics arm, healthy or not.
func (s *supervisor) Spawn(ctx context.Context, spec Spec) (Client, error) {
	if err := validateSpec(spec); err != nil {
		return nil, err
	}
	contracts := envc.Load()
	// THE GUARD MEANS "NEVER TOUCH THE REAL VENDOR", NOT "NEVER SPAWN A SHIM".
	// A shim is not itself a vendor call: it is our own process, and it has a
	// fake mode in which the whole real shim runs over a scripted SDK. So a
	// guarded daemon does not refuse the spawn -- refusing made a workspace
	// impossible to create under the guard, which is a bigger hole than the
	// one it closed, because it left every guarded run unable to exercise the
	// verbs at all. It FORCES fake mode instead, which is strictly stronger
	// than the refusal was: the process that comes up cannot reach the vendor
	// no matter what the caller asked for, and the shim's own guard
	// (agent-shim/claude/shim/src/vendor-guard.ts) still throws if anything in
	// it ever tries.
	spec.Fake = fakeMode(spec, contracts)

	log, err := s.surfaces.Workspace(spec.WorkspaceDir)
	if err != nil {
		return nil, fmt.Errorf("shimclient: resolve workspace log sink for %q: %w", spec.WorkspaceDir, err)
	}
	log = log.With(dlog.Context{"workspace_id": string(spec.WorkspaceID)})

	c := newClient(log, spec.WorkspaceID, spec.UDSPath, s.back, s.workspaceProbe(spec.WorkspaceDir), s.StandingDown)
	c.grace = s.grace
	c.stderr = newRing(stderrRingBytes)

	args := shimArgs(spec)
	cmd := exec.Command(spec.NodeBin, args...)
	cmd.Dir = spec.WorkspaceDir
	cmd.Env = spawnEnv(spec, contracts)
	cmd.Stdout = nil
	cmd.Stderr = c.stderr
	// THE SPAWN GATE. The child inherits the read end of a pipe and binds
	// nothing until it reads one byte from it; that byte is written only once
	// Spec.Spawned has made the pid durable. A daemon killed in between closes
	// the write end with its death, the child reads EOF and exits unbound, so
	// a shim that ever binds is ALWAYS one whose pid is on record: the
	// successor can never meet a starting shim it has no record of.
	gate, opener, err := os.Pipe()
	if err != nil {
		log.Error("daemon.shimclient.spawn", "could not make the spawn gate", dlog.Context{"error": err.Error()})
		return nil, fmt.Errorf("shimclient: make the spawn gate: %w", err)
	}
	cmd.ExtraFiles = []*os.File{spec.LogSink, gate}
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}

	log.Debug("daemon.shimclient.spawn", "spawning shim", dlog.Context{
		"node": spec.NodeBin, "argv": args, "cwd": spec.WorkspaceDir,
		"uds": spec.UDSPath, "store_socket": spec.StoreSocket, "fake": spec.Fake,
	})
	// THE LATCH IS READ, THE PROCESS IS STARTED, AND THE REGISTRY IS ENTERED
	// UNDER ONE LOCK. Each of those alone is not enough:
	//
	//   - the LATCH, because a daemon that has begun standing down must not
	//     start a process nothing will be left to stop;
	//   - the REGISTRY BEFORE THE REAPER and before bring-up, because from
	//     cmd.Start onward this process exists and the supervisor is the only
	//     thing that knows it — the other order is a window in which a death
	//     deregisters nothing;
	//   - and the two TOGETHER, because a check that released the lock before
	//     starting would let the sweep's snapshot fall between them, which is
	//     precisely the leak this closes.
	//
	// The lock is held across a fork+exec, which is the only reason to accept
	// that cost: it is contended by nothing but other spawns and the sweep.
	s.mu.Lock()
	if s.standingDown {
		s.mu.Unlock()
		// IT IS INFO, NOT WARN. The refusal is the latch doing exactly what
		// it exists for, and the caller gets ErrStandingDown to act on; the
		// bring-up reads the latch before it ever asks (Fleet.bringUpClient),
		// so reaching here at all is the narrow race between that read and
		// this one -- an ordinary schedule, not a defect. Recorded as a
		// warning it cost realtest run 2026-09-13T18:32:16 a WARN plus the
		// two ERRORs its caller then raised, for a spawn the daemon was
		// right to refuse.
		log.Info("daemon.shimclient.spawn", "refused a spawn: this daemon is standing down", dlog.Context{
			"node": spec.NodeBin, "uds": spec.UDSPath,
		})
		closeGate(log, gate, opener)
		return nil, ErrStandingDown
	}
	startErr := cmd.Start()
	// The child holds its own copy of the read end; this process never reads.
	closeGate(log, gate, nil)
	if err := startErr; err != nil {
		closeGate(log, nil, opener)
		s.mu.Unlock()
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
	s.holdLocked(c)
	s.mu.Unlock()

	log.Info("daemon.shimclient.spawn", "shim spawned", dlog.Context{
		"pid": cmd.Process.Pid, "uds": spec.UDSPath,
	})
	// THE PID IS HANDED OUT BEFORE ANYTHING BLOCKS. The bring-up below is
	// where this call spends its ~110ms of Node startup, and a daemon that
	// dies inside it has left a shim nothing durable names. See Spec.Spawned.
	if spec.Spawned != nil {
		spec.Spawned(cmd.Process.Pid)
	}
	openGate(log, opener, cmd.Process.Pid)
	go c.reap()

	if err := c.bringUp(ctx); err != nil {
		c.abandonBringUp(ctx, err)
		return nil, err
	}
	return c, nil
}

// openGate lets the spawned shim go on to bind: one byte, then the write end
// is closed. A shim already gone has no reader left, which is recorded and is
// the reaper's to report.
func openGate(log dlog.Logger, opener *os.File, pid int) {
	if _, err := opener.Write([]byte{1}); err != nil {
		log.Info("daemon.shimclient.spawn", "the shim was gone before its spawn gate opened", dlog.Context{
			"pid": pid, "error": err.Error(),
		})
	}
	closeGate(log, nil, opener)
}

// closeGate closes whichever ends of the spawn gate it is handed.
func closeGate(log dlog.Logger, ends ...*os.File) {
	for _, end := range ends {
		if end == nil {
			continue
		}
		if err := end.Close(); err != nil {
			log.Error("daemon.shimclient.spawn", "could not close an end of the spawn gate", dlog.Context{"error": err.Error()})
		}
	}
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

	c := newClient(log, ws, udsPath, s.back, s.workspaceProbe(workspaceDir), s.StandingDown)
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
func (c *client) abandonBringUp(ctx context.Context, cause error) {
	// THE ABANDONMENT IS RECORDED ON BOTH PATHS. A bring-up that failed
	// because the process had already died leaves nothing to stop, but it is
	// the same failure and the workspace's own log is where it belongs: the
	// exit record says the process is gone, and only this one says the
	// bring-up was given up on because of it.
	if c.exitedAlready() {
		fields := dlog.Context{"pid": c.PID(), "error": cause.Error()}
		// THE SUPERVISOR'S OWN STAND-DOWN SWEEP killed it: the daemon is
		// leaving, and nothing about this shim failed.
		if errors.Is(cause, ErrStandingDown) {
			c.log.Info("daemon.shimclient.spawn", "the bring-up ended: this daemon's stand-down swept the spawned shim", fields)
		} else {
			c.log.Warn("daemon.shimclient.spawn", "bring-up failed; the spawned shim is already gone", fields)
		}
		c.cancelMonitor()
		return
	}
	c.log.Warn("daemon.shimclient.spawn", "bring-up failed; stopping the spawned shim", dlog.Context{
		"pid": c.PID(), "error": cause.Error(),
	})
	// THE ABANDONMENT GETS ITS OWN BUDGET, DETACHED FROM THE BRING-UP'S. The
	// commonest reason a bring-up fails is that ITS context ended, and a kill
	// handed that same dead context would SIGKILL a shim the grace would have
	// let leave cleanly. GracefulKillBound is exactly what a graceful stop can
	// cost, so this is the smallest bound that can still observe one.
	stop, cancelStop := context.WithTimeout(context.WithoutCancel(ctx), GracefulKillBound)
	defer cancelStop()
	if err := c.Kill(stop, KillAttribution{
		Actor:  "shimclient.bringup",
		Reason: "bring-up failed: " + cause.Error(),
	}); err != nil {
		c.log.Error("daemon.shimclient.spawn", "stopping the spawned shim failed", dlog.Context{
			"pid": c.PID(), "error": err.Error(),
		})
	}
	c.cancelMonitor()
}

// fakeMode is whether the spawned shim runs over the scripted SDK: because the
// caller asked for it, or because THIS daemon is under the vendor guard and a
// real-vendor shim is therefore not a thing it is allowed to start.
//
// It can only turn fake ON. Nothing here can make a spawn less fake than the
// caller asked for.
func fakeMode(spec Spec, contracts envc.Contracts) bool {
	return spec.Fake || spec.ForbidVendor || contracts.ForbidVendorCalls()
}

// shimArgs is the shim's argv after the node binary, per the common spawn
// contract. The store socket is ALWAYS passed explicitly.
func shimArgs(spec Spec) []string {
	args := []string{
		spec.MainJS,
		"--listen", spec.UDSPath,
		"--store-socket", spec.StoreSocket,
		"--log-fd", fmt.Sprintf("%d", shimLogFD),
		"--spawn-gate-fd", fmt.Sprintf("%d", shimSpawnGateFD),
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

// bringUp dials the shim until the link is connected AND the shim has pushed
// its first diagnostics arm. An UNHEALTHY arm is an answer and completes the
// bring-up: see awaitDiagnostics. A process that dies ends this at once with
// its exit decoding and stderr ring — the correlation is a select on the death
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
		c.connected()

		frames, errs := recvLoop(stream, c.monitorCtx.Done())
		err = c.awaitDiagnostics(parent, frames, errs)
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
