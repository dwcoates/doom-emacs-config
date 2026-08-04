package server

import (
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/shim"
)

// udsspawn.go — ONE UDS SHIM SPAWN, used by the daemon and by every harness
// that claims to exercise it.
//
// # What this replaces
//
// Three near-identical closures — main.go's and two e2e harnesses' — each
// resolved the canonical workspace, opened the shim log target, hand-appended
// `--log-fd 3` beside a hand-built one-element ExtraFiles slice, called
// shim.Spawn, drained the (deliberately empty) stdout event channel, reaped the
// child, and returned a stop closure. They drifted in exactly the ways
// duplicated procedure does: one tracked processes, one asserted on the log,
// one wired a spawn watch, and only one of them reaped inside the drain.
//
// A harness that spawns shims DIFFERENTLY from the daemon proves something
// about the harness. The differences that are genuinely per-caller — which
// logger receives the child's stderr, who is told a process appeared or exited,
// what extra argv a store socket needs — are hooks; everything else is one
// procedure here.

// SpawnedShim is what a spawn produced, handed to the caller's hooks. It names
// the log target as a BORROW (dlog.LogTarget), so a hook can identify the file
// the child writes to without being able to close a descriptor the manager and
// every other writer for that workspace share.
type SpawnedShim struct {
	SessionID string
	Workspace dlog.Workspace
	Proc      *shim.Proc
	LogTarget dlog.LogTarget
	Argv      []string
}

// UDSSpawnConfig assembles the one spawn procedure. Targets, Node, Script,
// ShimSocket, Logger and Event are required; a missing one is a construction
// error rather than a nil-deref at the first spawn.
type UDSSpawnConfig struct {
	// Targets owns the workspace log targets the child inherits.
	Targets *dlog.TargetManager
	// Node and Script locate the shim bundle; ShimSocket is the daemon socket
	// the shim dials back on.
	Node, Script, ShimSocket string
	// ForceFake runs every session against the offline scripted SDK.
	ForceFake bool
	// ExtraArgv is appended after the derived fd-3 pair, for the flags a
	// particular deployment adds (--claude-bin, --store-socket).
	ExtraArgv []string
	// ExtraEnv derives the child's environment additions from the create opts.
	// Nil adds none.
	ExtraEnv func(opts CreateOpts) []string
	// ArgvOverride, when set, replaces the computed argv wholesale. It exists
	// for the harness that deliberately spawns a process which is NOT a shim
	// (the wedged-shim case), and production leaves it nil.
	ArgvOverride func(sessionID string, argv []string) []string
	// Logger builds the per-process diagnostic boundary shim.Spawn writes the
	// child's stderr through. Required.
	Logger func(workspace dlog.Workspace, sessionID string) shim.Logger
	// Event records one spawn-lifecycle fact. Required: a spawn nobody records
	// is a process whose birth and death cannot be explained afterwards.
	Event func(level dlog.Level, workspace dlog.Workspace, sessionID, message string, context map[string]any)
	// Spawned is told about a process that started, before its stop func is
	// handed back. Nil-safe.
	Spawned func(SpawnedShim)
	// Exited is told about a process that has been reaped, with the wait error,
	// BEFORE the exit is recorded. It may add context to that record and may
	// replace its message — which is how the daemon reports the exit its spawn
	// watch classified as "died before it ever connected" without the exit
	// being recorded twice. A returned message marks the exit as a FAILURE.
	// Nil-safe.
	Exited func(SpawnedShim, error) (message string, context map[string]any)
	// Reaped is the reaper's LAST act, after the exit has been recorded. It is
	// separate from Exited because the two have opposite ordering needs: the
	// daemon's watch must hear about an exit BEFORE the log write (a bring-up
	// is blocked on it), while a harness publishing "this process is finished
	// with" must do so AFTER, or a waiter it releases can outrun the record.
	// Nil-safe.
	Reaped func(SpawnedShim, error)
	// Spawn is the launcher, injected so a test can drive this procedure
	// without a real process. Nil takes shim.Spawn.
	Spawn func(shim.Options) (*shim.Proc, error)
}

// NewUDSSpawner validates the wiring and returns the spawn func the
// ShimSpawner drives.
func NewUDSSpawner(cfg UDSSpawnConfig) (ShimSpawnFunc, error) {
	switch {
	case cfg.Targets == nil:
		return nil, fmt.Errorf("server: the UDS spawner needs a log target manager; without one a spawned shim has no descriptor to write its canonical records to")
	case cfg.Node == "":
		return nil, fmt.Errorf("server: the UDS spawner needs the node binary")
	case cfg.Script == "":
		return nil, fmt.Errorf("server: the UDS spawner needs the shim entrypoint")
	case cfg.ShimSocket == "":
		return nil, fmt.Errorf("server: the UDS spawner needs the daemon shim socket path; shims dial the daemon, so a spawn without one can never connect")
	case cfg.Logger == nil:
		return nil, fmt.Errorf("server: the UDS spawner needs a Logger factory")
	case cfg.Event == nil:
		return nil, fmt.Errorf("server: the UDS spawner needs an Event recorder; a spawn nobody records cannot be explained after the fact")
	}
	spawn := cfg.Spawn
	if spawn == nil {
		spawn = shim.Spawn
	}
	return func(sessionID string, opts CreateOpts) (ShimStopFunc, error) {
		return spawnUDSShim(cfg, spawn, sessionID, opts)
	}, nil
}

// spawnUDSShim is the procedure itself.
func spawnUDSShim(cfg UDSSpawnConfig, spawn func(shim.Options) (*shim.Proc, error), sessionID string, opts CreateOpts) (ShimStopFunc, error) {
	workspace, err := dlog.WorkspaceFromDirectory(opts.CWD)
	if err != nil {
		return nil, fmt.Errorf("server: resolve UDS shim workspace for %q: %w", opts.CWD, err)
	}
	canonicalOpts := opts
	canonicalOpts.CWD = workspace.Directory

	target, err := cfg.Targets.BorrowWorkspaceRuntime(workspace, dlog.RuntimeShim)
	if err != nil {
		return nil, fmt.Errorf("server: open shim log target for workspace %q: %w", workspace.Directory, err)
	}
	// THE FD CONTRACT IS DERIVED, not repeated: the ExtraFiles slice and the
	// --log-fd value come from the same computation, so they cannot disagree,
	// and a target that has been closed underneath us fails HERE rather than in
	// a child holding a dead descriptor.
	extraFiles, fdArgv, err := dlog.ChildLogBinding(target)
	if err != nil {
		return nil, fmt.Errorf("server: bind the shim log descriptor for workspace %q: %w", workspace.Directory, err)
	}

	argv := ShimUDSArgv(cfg.Node, cfg.Script, sessionID, cfg.ForceFake, canonicalOpts, cfg.ShimSocket)
	argv = append(argv, fdArgv...)
	argv = append(argv, cfg.ExtraArgv...)
	if cfg.ArgvOverride != nil {
		argv = cfg.ArgvOverride(sessionID, argv)
	}
	var extraEnv []string
	if cfg.ExtraEnv != nil {
		extraEnv = cfg.ExtraEnv(opts)
	}

	cfg.Event(dlog.LevelInfo, workspace, sessionID, "spawning UDS shim", map[string]any{
		"cwd": workspace.Directory, "model": opts.Model, "config_dir": opts.ConfigDir,
		"daemon_socket": cfg.ShimSocket, "log_target": target.Name(),
	})
	proc, spawnErr := spawn(shim.Options{
		Argv:       argv,
		Dir:        workspace.Directory,
		ExtraEnv:   extraEnv,
		ExtraFiles: extraFiles,
		Logger:     cfg.Logger(workspace, sessionID),
	})
	if spawnErr != nil {
		cfg.Event(dlog.LevelError, workspace, sessionID, "UDS shim spawn failed", map[string]any{
			"error": spawnErr.Error(), "cwd": workspace.Directory,
		})
		return nil, spawnErr
	}
	spawned := SpawnedShim{SessionID: sessionID, Workspace: workspace, Proc: proc, LogTarget: target, Argv: argv}
	if cfg.Spawned != nil {
		cfg.Spawned(spawned)
	}
	// pid AND pgid, always. The shim is spawned into its OWN process group
	// precisely so a signal aimed at the daemon's group cannot reach it, and a
	// claim like that is only worth anything if the log lets you check it:
	// pgid == pid means detached, anything else means still coupled.
	cfg.Event(dlog.LevelInfo, workspace, sessionID, "UDS shim spawned", map[string]any{
		"argv": argv, "cwd": workspace.Directory,
		"pid": proc.Pid(), "pgid": proc.Pgid(), "detached": proc.Pgid() == proc.Pid(),
	})
	// ONE REAPER PER PROCESS. In UDS mode the shim streams over its socket, not
	// stdout, so its stdout event channel stays empty — draining it keeps the
	// pump unblocked, and it closes when the child's stdout does. cmd.Wait is
	// not re-entrant, so this is the ONLY caller of it and every other waiter
	// observes the exit through the Exited hook.
	go func() {
		for range proc.Events() { //nolint:revive
		}
		werr := proc.Wait()
		context := map[string]any{
			"cwd":         workspace.Directory,
			"exit":        shim.ExitDescription(werr),
			"exit_code":   shim.ExitCode(werr),
			"stderr_tail": proc.StderrTail(),
		}
		level := dlog.LevelInfo
		if werr != nil {
			context["error"] = werr.Error()
			level = dlog.LevelError
		}
		message := "UDS shim exited"
		if cfg.Exited != nil {
			// The hook runs FIRST: it is what unblocks a bring-up still waiting
			// for this shim's connection, and it must not wait behind the log
			// write.
			hookMessage, hookContext := cfg.Exited(spawned, werr)
			for k, v := range hookContext {
				context[k] = v
			}
			if hookMessage != "" {
				message = hookMessage
				level = dlog.LevelError
			}
		}
		cfg.Event(level, workspace, sessionID, message, context)
		if cfg.Reaped != nil {
			cfg.Reaped(spawned, werr)
		}
	}()
	// The DELIBERATE-STOP record, and the counterpart to the spawn record
	// above: a shim death with one of these next to it was ordered, and a shim
	// death without one was not.
	return func(by ShimStop) error {
		cfg.Event(dlog.LevelInfo, workspace, sessionID, "stopping UDS shim (SIGTERM)", map[string]any{
			"cwd": workspace.Directory, "pid": proc.Pid(), "pgid": proc.Pgid(),
			"initiator": by.Initiator, "reason": by.Reason,
		})
		if err := proc.Terminate(by); err != nil {
			cfg.Event(dlog.LevelError, workspace, sessionID, "stopping UDS shim FAILED", map[string]any{
				"cwd": workspace.Directory, "pid": proc.Pid(),
				"initiator": by.Initiator, "reason": by.Reason, "error": err.Error(),
			})
			return err
		}
		return nil
	}, nil
}
