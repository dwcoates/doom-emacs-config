// Command testrun runs the agent-repl test suites spread across the host's
// cores. bin/test-all.sh is its entry point; see that script and AGENTS.md.
//
//	testrun run --module DIR [--suites a,b] [--record --record-out PATH]
//	testrun finish-record --module DIR --record-out PATH
//	testrun cover-report -name N -module DIR -covdirs ROOT
//	testrun roster
package main

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"runtime"
	"strings"
	"syscall"

	"agentrepl/testrun/internal/cli"
	"agentrepl/testrun/internal/cover"
	"agentrepl/testrun/internal/history"
	"agentrepl/testrun/internal/ramdisk"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

func main() {
	log := &run.Log{Out: os.Stdout, Err: os.Stderr}
	if len(os.Args) < 2 {
		log.Errorf("usage: testrun run|finish-record|cover-report|roster ...")
		os.Exit(2)
	}
	switch os.Args[1] {
	case "run":
		os.Exit(runCmd(log, os.Args[2:]))
	case "finish-record":
		os.Exit(finishRecordCmd(log, os.Args[2:]))
	case "cover-report":
		os.Exit(coverCmd(log, os.Args[2:]))
	case "roster":
		fmt.Println(strings.Join(roster.Names(), "\n"))
	default:
		log.Errorf("unknown subcommand %q", os.Args[1])
		os.Exit(2)
	}
}

func runCmd(log *run.Log, argv []string) int {
	args, err := cli.ParseArgs(argv)
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}
	self, err := os.Executable()
	if err != nil {
		log.Errorf("resolve the testrun binary: %v", err)
		return 1
	}
	histPath, err := history.DefaultPath()
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}
	// THE RUN'S ROOT LIVES ON A RAM DISK on macOS (ramdisk), so the SQLite
	// files and everything else the suites write never reach the SSD the
	// owner's live store shares; a RAM disk that cannot be made falls back to
	// the disk, saying so.
	parent, prefix, release := runParent(log)
	code := runRooted(log, args, self, histPath, parent, prefix)
	if err := release(); err != nil {
		log.Errorf("%v", err)
		return 1
	}
	return code
}

// runRooted runs in a fresh temp root under parent and removes it after.
func runRooted(log *run.Log, args cli.Args, self, histPath, parent string, prefix []string) int {
	// THE RUN LIVES IN ITS OWN TEMP ROOT, and so does each unit under it
	// (run.OSExec). TMPDIR moves to the root, so even planning's
	// `go list`/`vitest list` write nothing in the user's temp directory.
	root, err := os.MkdirTemp(parent, "tr-")
	if err != nil {
		log.Errorf("create the run's temp root: %v", err)
		return 1
	}
	code := runIn(log, args, self, histPath, root, prefix)
	if err := os.RemoveAll(root); err != nil {
		log.Errorf("remove the run's temp root %s: %v", root, err)
		return 1
	}
	return code
}

// runParent reclaims every dead run's RAM disk, then makes this run's, and
// answers the directory the run's root goes in with the release to call after
// it. On a RAM disk it also exports ramdisk.EnvShortBase, the base the
// harnesses that need short, socket-safe roots make them in instead of /tmp.
func runParent(log *run.Log) (string, []string, func() error) {
	disk := func() error { return nil }
	if runtime.GOOS != "darwin" {
		return run.DefaultTmpParent, nil, disk
	}
	m := ramdisk.Default()
	reclaimed, err := m.Reclaim()
	for _, r := range reclaimed {
		log.Infof("reclaimed the RAM disk %s at %s left by dead run pid %d (attached %v, killed %d leftover processes)", r.Device, r.Mount, r.Pid, r.Attached, r.Killed)
	}
	if err != nil {
		log.Errorf("reclaiming a dead run's RAM disk failed: %v", err)
	}
	size, err := ramdisk.SizeFromEnv(os.Getenv)
	if err != nil {
		log.Errorf("RAM DISK UNAVAILABLE, this run's root falls back to the disk at %s: %v", run.DefaultTmpParent, err)
		return run.DefaultTmpParent, nil, disk
	}
	v, err := m.Acquire(size)
	if err != nil {
		log.Errorf("RAM DISK UNAVAILABLE, this run's root falls back to the disk at %s: %v", run.DefaultTmpParent, err)
		return run.DefaultTmpParent, nil, disk
	}
	if err := os.Setenv(ramdisk.EnvShortBase, v.Mount); err != nil {
		log.Errorf("export %s: %v", ramdisk.EnvShortBase, err)
	}
	log.Infof("the run's root is on a %d MiB RAM disk at %s (%s); units run at the %s I/O tier", size, v.Mount, v.Device, ramdisk.UnitIOTier)
	return v.Mount, ramdisk.UnitPrefix(), func() error {
		if err := v.Release(); err != nil {
			return fmt.Errorf("release the run's RAM disk: %w", err)
		}
		return nil
	}
}

func runIn(log *run.Log, args cli.Args, self, histPath, root string, prefix []string) int {
	if err := os.Setenv("TMPDIR", root); err != nil {
		log.Errorf("point TMPDIR at the run's temp root %s: %v", root, err)
		return 1
	}
	work, err := os.MkdirTemp(root, "work-")
	if err != nil {
		log.Errorf("create the run's scratch directory: %v", err)
		return 1
	}
	slots := cli.SlotsForHost(runtime.NumCPU())
	ctx, stop := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer stop()
	return cli.Run(ctx, cli.Deps{
		Log:         log,
		Exec:        run.OSExec{Log: log, Grace: run.KillGrace, TmpParent: root, Prefix: prefix},
		Clock:       run.WallClock{},
		Slots:       slots,
		HistoryPath: histPath,
		Build:       suites.Build,
		Git:         gitHead,
		Self:        self,
		Work:        work,
		Pid:         os.Getpid(),
	}, args)
}

func finishRecordCmd(log *run.Log, argv []string) int {
	a, err := cli.ParseFinishRecordArgs(argv)
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}
	moduleRoot, err := filepath.Abs(a.Module)
	if err != nil {
		log.Errorf("resolve the module root %s: %v", a.Module, err)
		return 1
	}
	return cli.FinishRecord(cli.Deps{Log: log, Git: gitHead}, moduleRoot, a.RecordOut)
}

func coverCmd(log *run.Log, argv []string) int {
	a, err := cli.ParseCoverArgs(argv)
	if err != nil {
		log.Errorf("%v", err)
		return 2
	}
	if err := cover.Report(cover.GoTool, os.Stdout, a.Name, a.Module, a.CovDirs); err != nil {
		log.Errorf("%v", err)
		return 1
	}
	return 0
}

func gitHead(repo string) (string, string, error) {
	branch, err := exec.Command("git", "-C", repo, "branch", "--show-current").Output()
	if err != nil {
		return "", "", fmt.Errorf("git branch --show-current: %w", err)
	}
	commit, err := exec.Command("git", "-C", repo, "rev-parse", "HEAD").Output()
	if err != nil {
		return "", "", fmt.Errorf("git rev-parse HEAD: %w", err)
	}
	return strings.TrimSpace(string(branch)), strings.TrimSpace(string(commit)), nil
}
