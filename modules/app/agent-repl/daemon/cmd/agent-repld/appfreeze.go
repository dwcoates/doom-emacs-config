package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"syscall"

	"agent-repld/internal/appfreeze"
	"agent-repld/internal/childproc"
	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
	"agent-repld/internal/persistentwifi"
	"agent-repld/internal/recordlock"
	"agent-repld/internal/runtimemetrics"
)

// thawGuardVerb runs the thaw guard (appfreeze.RunGuard): the process each
// daemon starts so its frozen applications never outlive every daemon.
const thawGuardVerb = "thaw-guard"

// freezeDir is the frozen set's directory under the state root.
func freezeDir(stateDir string) string { return filepath.Join(stateDir, "persistent-wifi") }

// The daemon keeps these for its whole life, so the kernel releases them only
// at its death: the shared hold on the frozen set's owners file and the write
// end of its guard's pipe. Neither is ever released by the daemon.
var (
	ownersHold *recordlock.Shared
	guardPipe  *os.File
)

// buildFreezer builds the application freezer for the quiet state. A REAL
// HOST holds the frozen set's ownership shared and starts its thaw guard
// before anything reads the set; a fake host (the harness's) does neither
// and signals no process, since its process table is a fake tool's.
func buildFreezer(cfg persistentwifi.Config, runner appfreeze.Runner, stateDir, selfExe string, metrics *runtimemetrics.Surface, log dlog.Logger) (*appfreeze.Freezer, error) {
	dir := freezeDir(stateDir)
	signal := appfreeze.Signaler(appfreeze.KillSignaler)
	if cfg.FakeLid != "" {
		signal = refuseSignal
	} else {
		if err := os.MkdirAll(dir, 0o700); err != nil {
			return nil, fmt.Errorf("agent-repld: make the frozen set's directory: %w", err)
		}
		owners, err := appfreeze.HoldOwnership(filepath.Join(dir, appfreeze.OwnersFile), appfreeze.GuardThawBound)
		if err != nil {
			return nil, fmt.Errorf("agent-repld: %w", err)
		}
		ownersHold = owners
		if err := startThawGuard(selfExe, stateDir, log); err != nil {
			return nil, err
		}
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return nil, fmt.Errorf("agent-repld: resolve the home directory for the freezer's protection: %w", err)
	}
	freezer, err := appfreeze.New(appfreeze.Deps{
		Apps:      cfg.FreezeApps,
		Processes: appfreeze.PSLister(runner, cfg.Tools.Ps),
		Signal:    signal,
		Store:     appfreeze.FileStore{Path: filepath.Join(dir, appfreeze.RecordFile)},
		Protection: appfreeze.Protection{
			Self:         os.Getpid(),
			PathPrefixes: []string{filepath.Dir(selfExe), filepath.Join(home, ".cache", "agent-repl")},
		},
		Log:       log,
		Telemetry: metrics,
	})
	if err != nil {
		return nil, fmt.Errorf("agent-repld: build the application freezer: %w", err)
	}
	return freezer, nil
}

// refuseSignal is a fake host's signaler: it signals nothing, so a harness
// that names applications to freeze never reaches a real process.
func refuseSignal(pid int, _ syscall.Signal) error {
	return fmt.Errorf("a fake host signals no process (pid %d)", pid)
}

// startThawGuard starts this daemon's thaw guard in a session of its own, so
// it outlives the daemon, handing it the read end of a pipe whose write end
// the daemon holds for life (processHolds): the pipe's end of file is the
// daemon's death, however it dies. A guard that ends while the daemon lives
// is recorded at ERROR.
func startThawGuard(selfExe, stateDir string, log dlog.Logger) error {
	const op = "daemon.appfreeze.guard"
	r, w, err := os.Pipe()
	if err != nil {
		return fmt.Errorf("agent-repld: make the thaw guard's pipe: %w", err)
	}
	cmd := childproc.Command(childproc.Stable, selfExe, thawGuardVerb, "-state-dir", stateDir)
	cmd.ExtraFiles = []*os.File{r}
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true}
	if err := cmd.Start(); err != nil {
		_ = r.Close()
		_ = w.Close()
		return fmt.Errorf("agent-repld: start the thaw guard: %w", err)
	}
	_ = r.Close()
	guardPipe = w
	log.Info(op, "the thaw guard is watching this daemon", dlog.Context{"guard_pid": cmd.Process.Pid})
	// A STANDING WATCH: the guard outlives the daemon by design, so the wait
	// ends only if the guard exits first, which is recorded.
	inflight.Standing(func() {
		err := cmd.Wait()
		cause := "exited"
		if err != nil {
			cause = err.Error()
		}
		log.Error(op, "the thaw guard ended while its daemon lives; a frozen application would not be thawed if this daemon died without a successor",
			dlog.Context{"guard_pid": cmd.Process.Pid, "cause": cause})
	})
	return nil
}

// runThawGuard is the thaw-guard verb: fd 3 is its daemon's pipe.
func runThawGuard(args []string, stderr io.Writer) int {
	fs := flag.NewFlagSet(thawGuardVerb, flag.ContinueOnError)
	fs.SetOutput(stderr)
	stateDir := fs.String("state-dir", "", "the daemon's state root")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if *stateDir == "" {
		fmt.Fprintln(stderr, "thaw-guard: -state-dir is required")
		return 2
	}
	surfaces, _, err := dlog.OpenSurfaces(filepath.Join(*stateDir, "logs", "thaw-guard.run.log"))
	if err != nil {
		fmt.Fprintf(stderr, "thaw-guard: open the log: %v\n", err)
		return 1
	}
	defer func() { _ = surfaces.Close() }()
	pipe := os.NewFile(3, "daemon-pipe")
	if pipe == nil {
		surfaces.Global().Error("daemon.appfreeze.guard", "the thaw guard was started without its daemon's pipe", nil)
		return 1
	}
	dir := freezeDir(*stateDir)
	err = appfreeze.RunGuard(appfreeze.GuardDeps{
		DaemonGone: appfreeze.PipeGone(pipe),
		TryOwn:     appfreeze.RecordTryOwn(filepath.Join(dir, appfreeze.OwnersFile)),
		Store:      appfreeze.FileStore{Path: filepath.Join(dir, appfreeze.RecordFile)},
		Signal:     appfreeze.KillSignaler,
		Log:        surfaces.Global(),
	})
	if err != nil {
		return 1
	}
	return 0
}
