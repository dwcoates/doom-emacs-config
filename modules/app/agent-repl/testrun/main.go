// Command testrun runs the agent-repl test suites spread across the host's
// cores. bin/test-all.sh is its entry point; see that script and AGENTS.md.
//
//	testrun run --module DIR [--suites a,b] [--no-reuse] [--record --record-out PATH]
//	testrun plan --module DIR [--suites a,b] [--no-reuse] [--json]
//	testrun confine --profile P --keep A,B -- argv... (a keyed unit's launcher; see internal/reuse)
//	testrun warden --runner PID --since UNIX (started by the runner; see internal/life)
//	testrun finish-record --module DIR --record-out PATH
//	testrun cover-report -name N -module DIR -covdirs ROOT
//	testrun roster
//	testrun coverage-baseline -module DIR -work DIR -commit SHA -out FILE (a --coverage run's last unit)
//	testrun redundancy go -module DIR -pkg PKG -tests FILES -out DIR [...]
//	testrun redundancy report -in measurement.json [-universe U] [-overhead D]
package main

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"syscall"
	"time"

	"agentrepl/testrun/internal/cli"
	"agentrepl/testrun/internal/cover"
	"agentrepl/testrun/internal/gitinfo"
	"agentrepl/testrun/internal/history"
	"agentrepl/testrun/internal/life"
	"agentrepl/testrun/internal/ramdisk"
	"agentrepl/testrun/internal/redundancy"
	"agentrepl/testrun/internal/redundancy/gomeasure"
	"agentrepl/testrun/internal/reuse"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/reap"
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
		// `testrun run` is the SENTINEL; the runner it starts in a session of
		// its own runs `testrun run` again under life.EnvRole (internal/life).
		if os.Getenv(life.EnvRole) == life.RoleRunner {
			os.Exit(runnerCmd(os.Args[2:]))
		}
		os.Exit(sentinelCmd(log, os.Args[2:]))
	case "warden":
		os.Exit(wardenCmd(os.Args[2:]))
	case "finish-record":
		os.Exit(finishRecordCmd(log, os.Args[2:]))
	case "cover-report":
		os.Exit(coverCmd(log, os.Args[2:]))
	case "roster":
		fmt.Println(strings.Join(roster.Names(), "\n"))
	case "plan":
		os.Exit(planCmd(log, os.Args[2:]))
	case "confine":
		os.Exit(confineCmd(log, os.Args[2:]))
	case "redundancy":
		os.Exit(redundancyCmd(log, os.Args[2:]))
	case "coverage-baseline":
		os.Exit(coverageBaselineCmd(log, os.Args[2:]))
	default:
		log.Errorf("unknown subcommand %q", os.Args[1])
		os.Exit(2)
	}
}

// sentinelCmd starts the runner and stands in for it to the invoker.
func sentinelCmd(log *run.Log, argv []string) int {
	self, err := os.Executable()
	if err != nil {
		log.Errorf("resolve the testrun binary: %v", err)
		return 1
	}
	return life.Sentinel(self, append([]string{"run"}, argv...), os.Stdout, os.Stderr, log)
}

// runnerCmd runs the suites, leading the run's session.
func runnerCmd(argv []string) int {
	log := life.CutLog(os.Getenv(ramdisk.EnvRunLog))
	// The role is this process's alone: a unit that runs testrun itself
	// starts a sentinel of its own.
	if err := os.Unsetenv(life.EnvRole); err != nil {
		log.Errorf("unset %s: %v", life.EnvRole, err)
		return 1
	}
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
	runner, err := life.StartRunner(reap.OSTable{}, reap.OSReap(log), life.WardenCommand{Name: self, Args: []string{"warden"}}, log)
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}
	gone, err := life.Lifeline()
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	runner.CancelOnLifeline(gone, cancel)
	// THE RUN'S ROOT LIVES ON A RAM DISK on macOS (ramdisk), so the SQLite
	// files and everything else the suites write never reach the SSD the
	// owner's live store shares; a RAM disk that cannot be made falls back to
	// the disk, saying so.
	module, err := filepath.Abs(args.Module)
	if err != nil {
		log.Errorf("resolve the module root %s: %v", args.Module, err)
		return 1
	}
	parent, prefix, release := runParent(log, module, runner)
	histPath, err := history.DefaultPath(cli.RunConditions(prefix))
	if err != nil {
		log.Errorf("%v", err)
		return runner.Finish(1, gone, release)
	}
	code := runRooted(ctx, log, args, self, histPath, parent, prefix, runner)
	return runner.Finish(code, gone, release)
}

// wardenCmd watches the runner's lifeline and, if the runner dies without
// releasing its run, reaps and releases it (internal/life).
func wardenCmd(argv []string) int {
	log := life.CutLog(os.Getenv(ramdisk.EnvRunLog))
	a, err := cli.ParseWardenArgs(argv)
	if err != nil {
		log.Errorf("%v", err)
		return 2
	}
	m := ramdisk.Default(log)
	return life.Warden{
		Runner:   a.Runner,
		Since:    a.Since,
		Sentinel: a.Sentinel,
		Reap:     reap.OSReap(log),
		Release:  m.ReclaimRun,
		Log:      log,
	}.WatchLifeline()
}

// runRooted runs in a fresh temp root under parent and removes it after,
// once every process of the run is reaped.
func runRooted(ctx context.Context, log *run.Log, args cli.Args, self, histPath, parent string, prefix []string, runner *life.Runner) int {
	// THE RUN LIVES IN ITS OWN TEMP ROOT, and so does each unit under it
	// (run.OSExec). TMPDIR moves to the root, so even planning's
	// `go list`/`vitest list` write nothing in the user's temp directory.
	root, err := os.MkdirTemp(parent, "tr-")
	if err != nil {
		log.Errorf("create the run's temp root: %v", err)
		return 1
	}
	if err := runner.Root(root); err != nil {
		log.Errorf("%v", err)
		return 1
	}
	code := runIn(ctx, log, args, self, histPath, root, prefix)
	// NOTHING OF THE RUN OUTLIVES IT: a unit's leftover (a test daemon, its
	// shim) dies here, and a root is never removed from under one.
	if err := runner.ReapSession(); err != nil {
		return 1
	}
	if err := os.RemoveAll(root); err != nil {
		log.Errorf("remove the run's temp root %s: %v", root, err)
		return 1
	}
	return code
}

// runParent checks the disk manager, reclaims every dead run's RAM disk, then
// makes this run's, and answers the directory the run's root goes in with the
// release to call after it. On a RAM disk it also exports
// ramdisk.EnvShortBase, the base the harnesses that need short, socket-safe
// roots make them in instead of /tmp. A leaked RAM disk or a disk manager that
// does not answer puts the run on the disk and opens the one workspace that
// investigates it.
func runParent(log *run.Log, module string, runner *life.Runner) (string, []string, func() error) {
	disk := func() error { return nil }
	if runtime.GOOS != "darwin" {
		return run.DefaultTmpParent, nil, disk
	}
	m := ramdisk.Default(log)
	m.Own = runner.Members()
	m.Run = ramdisk.Timed(m.Run, func(call string, took time.Duration, err error) {
		if err != nil {
			log.Infof("ramdisk: %s failed after %s", call, took.Round(time.Millisecond))
			return
		}
		log.Infof("ramdisk: %s answered in %s", call, took.Round(time.Millisecond))
	})
	size, err := ramdisk.SizeFromEnv(os.Getenv)
	if err != nil {
		log.Errorf("RAM DISK UNAVAILABLE, this run's root falls back to the disk at %s: %v", run.DefaultTmpParent, err)
		return run.DefaultTmpParent, nil, disk
	}
	v, trouble, err := m.Setup(size)
	if trouble != nil {
		log.Errorf("RAM DISK REFUSED, this run's root falls back to the disk at %s: %s", run.DefaultTmpParent, trouble)
		investigate(log, m, module, trouble)
		// A PROBE THAT MISSED ITS BOUND IS WATCHED FOR THE WHOLE RUN, and
		// one still unanswered when the run ends is recorded as such.
		return run.DefaultTmpParent, nil, func() error {
			m.SettleProbes(trouble)
			return nil
		}
	}
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

// investigate opens the workspace that investigates the trouble, unless one
// is open already, saying which at INFO and any failure at ERROR.
func investigate(log *run.Log, m ramdisk.Manager, module string, trouble *ramdisk.Trouble) {
	home, err := os.UserHomeDir()
	if err != nil {
		log.Errorf("the RAM disk investigation workspace was not created: resolve the home directory: %v", err)
		return
	}
	dispatch := os.Getenv(ramdisk.EnvDispatch)
	if dispatch == "" {
		dispatch = filepath.Join(home, ".claude", "skills", "create-or-update-workspace", "run.sh")
	}
	inv := ramdisk.Investigation{
		Module:   module,
		Git:      ramdisk.RunBounded,
		Dispatch: ramdisk.ScriptDispatch(dispatch, ramdisk.DispatchBound),
		StateDir: filepath.Join(home, ".claude-emacs"),
		RunLog:   os.Getenv(ramdisk.EnvRunLog),
	}
	out, err := m.EnsureInvestigation(inv, trouble)
	if out.Unlocked {
		log.Infof("the check for an open RAM disk investigation workspace ran without the registry lock, which was not taken within %s", m.LockBound)
	}
	switch {
	case err != nil:
		log.Errorf("the RAM disk investigation workspace was not created: %v", err)
	case out.Existing != "":
		log.Infof("a RAM disk investigation workspace is already open (%s); no other is created", out.Existing)
	default:
		log.Infof("dispatched the RAM disk investigation workspace %q through %s", ramdisk.InvestigationName, dispatch)
	}
}

func runIn(ctx context.Context, log *run.Log, args cli.Args, self, histPath, root string, prefix []string) int {
	if err := os.Setenv("TMPDIR", root); err != nil {
		log.Errorf("point TMPDIR at the run's temp root %s: %v", root, err)
		return 1
	}
	for name, dir := range cli.RunArtifactsEnv(os.Getenv(ramdisk.EnvRunLog), os.Getenv) {
		if err := os.Setenv(name, dir); err != nil {
			log.Errorf("point %s at the run's artifacts %s: %v", name, dir, err)
			return 1
		}
		log.Infof("a failing test's whole logs are kept under %s (%s)", dir, name)
	}
	work, err := os.MkdirTemp(root, "work-")
	if err != nil {
		log.Errorf("create the run's scratch directory: %v", err)
		return 1
	}
	slots, err := cli.Slots(runtime.NumCPU())
	if err != nil {
		log.Errorf("size the run: %v", err)
		return 1
	}
	reuseDeps, err := reuse.HostDeps(args.Module, self, work, log)
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}
	ctx, stop := signal.NotifyContext(ctx, syscall.SIGINT, syscall.SIGTERM)
	defer stop()
	return cli.Run(ctx, cli.Deps{
		Reuse:       reuse.Runner{Deps: reuseDeps, Files: func() ([]string, error) { return reuse.ExecRepoFiles(reuseDeps.Repo) }},
		Log:         log,
		Exec:        run.OSExec{Log: log, Grace: run.KillGrace, TmpParent: root, Prefix: prefix},
		Clock:       run.WallClock{},
		Slots:       slots,
		HistoryPath: histPath,
		Build:       suites.Build,
		Git:         gitinfo.Head,
		Self:        self,
		Work:        work,
		Pid:         os.Getpid(),
		RunLog:      os.Getenv(ramdisk.EnvRunLog),
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
	return cli.FinishRecord(cli.Deps{Log: log, Git: gitinfo.Head}, moduleRoot, a.RecordOut)
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

// coverageBaselineCmd merges a finished --coverage run into each coverage
// system's all-tier statement coverage (internal/cover, baseline.go).
func coverageBaselineCmd(log *run.Log, argv []string) int {
	a, err := cover.ParseBaselineArgs(argv)
	if err != nil {
		log.Errorf("%v", err)
		return 2
	}
	goSystems, err := suites.SystemModules(suites.Layout{Module: a.Module})
	if err != nil {
		log.Errorf("coverage-baseline: %v", err)
		return 1
	}
	var vitest []string
	for _, name := range roster.CoverageSystems {
		if s, ok := roster.Lookup(name); ok && s.Kind == roster.Vitest {
			vitest = append(vitest, name)
		}
	}
	f, err := cover.RunBaseline(cover.GoTool, a, goSystems, vitest, time.Now())
	if err != nil {
		log.Errorf("coverage-baseline of %s at %s: %v", a.Work, a.Commit, err)
		return 1
	}
	names := make([]string, 0, len(f.Systems))
	for name := range f.Systems {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
		s := f.Systems[name]
		log.Infof("coverage-baseline: %s %.2f%% of %d statements (floor %.2f%%; %s)", name, s.Percent, s.Statements, s.Floor, s.Tiers)
	}
	log.Infof("coverage-baseline: wrote %s; it becomes testrun/coverage-baseline.json only by the owner's decision", a.Out)
	return 0
}

// redundancyCmd is `testrun redundancy go|report`.
func redundancyCmd(log *run.Log, argv []string) int {
	if len(argv) == 0 {
		log.Errorf("usage: testrun redundancy go|report ...")
		return 2
	}
	switch argv[0] {
	case "go":
		ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
		defer stop()
		return gomeasure.Command(ctx, log, argv[1:], gomeasure.OSExec)
	case "report":
		return redundancy.ReportCommand(log, argv[1:], os.Stdout)
	}
	log.Errorf("unknown redundancy subcommand %q", argv[0])
	return 2
}

// confineCmd is a keyed unit's launcher: it scrubs the environment and execs
// the unit's command under its suite's profile. It returns only on failure.
func confineCmd(log *run.Log, argv []string) int {
	c, err := reuse.ParseConfine(argv)
	if err != nil {
		log.Errorf("%v", err)
		return 2
	}
	log.Errorf("%v", reuse.Exec(c, os.Environ()))
	return 1
}

// planCmd prints what `run` with the same arguments would run and reuse,
// running nothing.
func planCmd(log *run.Log, argv []string) int {
	asJSON := false
	var rest []string
	for _, a := range argv {
		if a == "--json" {
			asJSON = true
			continue
		}
		rest = append(rest, a)
	}
	args, err := cli.ParseArgs(rest)
	if err != nil {
		log.Errorf("%v", err)
		return 2
	}
	self, err := os.Executable()
	if err != nil {
		log.Errorf("resolve the testrun binary: %v", err)
		return 1
	}
	work, err := os.MkdirTemp("", "testrun-plan-")
	if err != nil {
		log.Errorf("create the plan's scratch directory: %v", err)
		return 1
	}
	defer os.RemoveAll(work)
	// The plan's own notes go to stderr, so --json leaves stdout one document.
	quiet := &run.Log{Out: os.Stderr, Err: os.Stderr}
	d, err := reuse.HostDeps(args.Module, self, work, quiet)
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}
	var selected []roster.Suite
	for _, s := range roster.Suites {
		if args.Selects(s.Name) {
			selected = append(selected, s)
		}
	}
	r := reuse.Runner{Deps: d, Files: func() ([]string, error) { return reuse.ExecRepoFiles(d.Repo) }}
	verdicts, err := r.Decide(selected, args.ReuseMode())
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}
	unplaced, err := r.Unplaced(verdicts)
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}
	doc := reuse.BuildPlan(verdicts, unplaced, d.Probe() == nil)
	if asJSON {
		err = reuse.WritePlanJSON(os.Stdout, doc)
	} else {
		err = reuse.WritePlanText(os.Stdout, doc)
	}
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}
	return 0
}
