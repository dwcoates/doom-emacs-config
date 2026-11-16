package appfreeze

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"agentrepl/logging/procstat"
)

// Process is one running process as the process table lists it.
type Process struct {
	PID  int
	PPID int
	// Path is the process's executable, absolute.
	Path string
}

// Lister reads the process table. PSLister is the production one; a test
// hands a fixed table.
type Lister func(ctx context.Context) ([]Process, error)

// Runner runs one host tool and answers its output and exit code; the
// daemon's script runner is the production one (persistentwifi.Runner).
type Runner interface {
	Run(ctx context.Context, dir string, argv []string) (string, int, error)
}

// PSLister lists the process table with `ps -axo pid=,ppid=,comm=`, run at
// path through runner. macOS prints each process's whole executable path in
// `comm`, spaces included, so a line is two numbers and then the path.
func PSLister(runner Runner, path string) Lister {
	return func(ctx context.Context) ([]Process, error) {
		out, code, err := runner.Run(ctx, "/", []string{path, "-axo", "pid=,ppid=,comm="})
		if err != nil {
			return nil, fmt.Errorf("%s could not be run: %w", path, err)
		}
		if code != 0 {
			return nil, fmt.Errorf("%s exited %d: %s", path, code, strings.TrimSpace(out))
		}
		return parsePS(out)
	}
}

// parsePS reads ps's lines. A line that is not two numbers and a path is
// refused: the table is the one ps prints, so anything else is a ps this
// package does not know.
func parsePS(out string) ([]Process, error) {
	var procs []Process
	for line := range strings.SplitSeq(out, "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		fields := strings.Fields(line)
		if len(fields) < 3 {
			return nil, fmt.Errorf("ps printed %q, which is not a pid, a parent pid and a path", line)
		}
		pid, err := strconv.Atoi(fields[0])
		if err != nil {
			return nil, fmt.Errorf("ps printed %q, whose pid is not a number", line)
		}
		ppid, err := strconv.Atoi(fields[1])
		if err != nil {
			return nil, fmt.Errorf("ps printed %q, whose parent pid is not a number", line)
		}
		// The path is the rest of the line, spaces included.
		rest := strings.TrimSpace(strings.TrimPrefix(strings.TrimSpace(strings.TrimPrefix(line, fields[0])), fields[1]))
		procs = append(procs, Process{PID: pid, PPID: ppid, Path: rest})
	}
	return procs, nil
}

// bundleOf reports whether path lies inside an application bundle named
// app: some component of it is `<app>.app`. A helper in a framework inside
// the bundle is the bundle's (Chrome's renderers live four bundles deep), and
// a process of another bundle whose name merely starts the same ("Google
// Chrome for Testing") is not.
func bundleOf(path, app string) bool {
	return strings.Contains(path, "/"+app+".app/")
}

// executableOf is Linux's bundleOf, where an application is named by its
// executable: path's own name is app, or app names a directory path lies
// under (Chrome's helpers run from /opt/google/chrome/). A process of another
// executable whose name merely starts the same is not app's.
func executableOf(path, app string) bool {
	return filepath.Base(path) == app || strings.Contains(path, "/"+app+"/")
}

// AppOf answers whether the executable at path is app's.
type AppOf func(path, app string) bool

// AppMatcherFor answers goos's AppOf: an application is a bundle on macOS and
// an executable on Linux.
func AppMatcherFor(goos string) AppOf {
	if goos == "linux" {
		return executableOf
	}
	return bundleOf
}

// ProcLister lists the process table from a /proc tree (procstat.Tree), as
// Linux names a process's executable only there: `ps` prints its 15-byte
// command name. Another user's process hides its executable, and such a
// process is no application of this user's, so it is listed with no path; a
// kernel thread has none either.
func ProcLister(tree procstat.Tree) Lister {
	return func(context.Context) ([]Process, error) {
		pids, err := tree.PIDs()
		if err != nil {
			return nil, err
		}
		var procs []Process
		for _, pid := range pids {
			line, err := tree.Stat(pid)
			if procstat.Gone(err) {
				continue
			}
			if err != nil {
				return nil, err
			}
			ppid, err := line.Uint(procstat.FieldPPID)
			if err != nil {
				return nil, fmt.Errorf("pid %d: %w", pid, err)
			}
			exe, err := os.Readlink(tree.Path(pid, "exe"))
			switch {
			case err == nil:
				exe = strings.TrimSuffix(exe, " (deleted)")
			case procstat.Gone(err) || errors.Is(err, fs.ErrPermission):
				exe = ""
			default:
				return nil, fmt.Errorf("read the executable of pid %d: %w", pid, err)
			}
			procs = append(procs, Process{PID: pid, PPID: int(ppid), Path: exe})
		}
		return procs, nil
	}
}

// Protection says which processes are never frozen: the daemon itself and
// every ancestor of it below launchd (the editor that started it), every
// process whose executable lies under one of PathPrefixes (agent-repl's own
// binaries: the store, the sidecar, the shims), every process of a bundle
// named in neverFrozen (the editor and terminals), and every descendant of
// any of those, which is everything an agent session spawned.
type Protection struct {
	// Self is the daemon's own pid.
	Self int
	// PathPrefixes are the directories agent-repl's binaries run from.
	PathPrefixes []string
}

// protected answers the pids Protection covers in table.
func (p Protection) protected(table []Process, appOf AppOf) map[int]bool {
	byPID := map[int]Process{}
	children := map[int][]int{}
	for _, proc := range table {
		byPID[proc.PID] = proc
		children[proc.PPID] = append(children[proc.PPID], proc.PID)
	}
	roots := map[int]bool{}
	// The daemon and its ancestors, short of launchd (pid 1), whose
	// descendants are every process on the machine.
	for pid := p.Self; pid > 1; {
		roots[pid] = true
		proc, ok := byPID[pid]
		if !ok {
			break
		}
		pid = proc.PPID
	}
	for _, proc := range table {
		for _, prefix := range p.PathPrefixes {
			if prefix != "" && strings.HasPrefix(proc.Path, strings.TrimSuffix(prefix, "/")+"/") {
				roots[proc.PID] = true
			}
		}
		for _, app := range neverFrozen {
			if appOf(proc.Path, app) {
				roots[proc.PID] = true
			}
		}
	}
	out := map[int]bool{}
	var walk func(pid int)
	walk = func(pid int) {
		if out[pid] {
			return
		}
		out[pid] = true
		for _, child := range children[pid] {
			walk(child)
		}
	}
	for pid := range roots {
		walk(pid)
	}
	return out
}
