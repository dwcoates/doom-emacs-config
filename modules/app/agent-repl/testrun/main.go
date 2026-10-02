// Command testrun runs the agent-repl test suites spread across the host's
// cores. bin/test-all.sh is its entry point; see that script and AGENTS.md.
//
//	testrun run --module DIR [--suites a,b] [--record]
//	testrun cover-report -name N -module DIR -covdirs ROOT
//	testrun roster
package main

import (
	"context"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"os/signal"
	"runtime"
	"strings"
	"syscall"

	"agentrepl/testrun/internal/cli"
	"agentrepl/testrun/internal/cover"
	"agentrepl/testrun/internal/history"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

func main() {
	log := &run.Log{Out: os.Stdout, Err: os.Stderr}
	if len(os.Args) < 2 {
		log.Errorf("usage: testrun run|cover-report|roster ...")
		os.Exit(2)
	}
	switch os.Args[1] {
	case "run":
		os.Exit(runCmd(log, os.Args[2:]))
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
	work, err := os.MkdirTemp("", "agent-repl-testrun-")
	if err != nil {
		log.Errorf("create the run's scratch directory: %v", err)
		return 1
	}
	defer os.RemoveAll(work)
	slots := cli.SlotsForHost(runtime.NumCPU())
	ctx, stop := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer stop()
	return cli.Run(ctx, cli.Deps{
		Log:         log,
		Exec:        run.OSExec{Log: log, Grace: run.KillGrace},
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

func coverCmd(log *run.Log, argv []string) int {
	fs := flag.NewFlagSet("cover-report", flag.ContinueOnError)
	name := fs.String("name", "", "the module's suite name")
	module := fs.String("module", "", "the Go module directory")
	covRoot := fs.String("covdirs", "", "the directory holding one coverage directory per package unit")
	if err := fs.Parse(argv); err != nil {
		return 2
	}
	if *name == "" || *module == "" || *covRoot == "" {
		log.Errorf("cover-report needs -name, -module and -covdirs")
		return 2
	}
	if err := cover.Report(cover.GoTool, os.Stdout, *name, *module, *covRoot); err != nil {
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
