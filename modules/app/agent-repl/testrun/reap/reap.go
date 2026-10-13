// Package reap kills what a dead test run left running: the processes still
// running out of a directory the run owned (a RAM disk's mount, a harness's run
// root). The test runner's RAM disk reclaim and the daemon integration
// harness's run-root reclaim share it.
package reap

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"syscall"
	"unicode"
)

// Under SIGKILLs every process other than this one that runs out of dir,
// answering how many it killed. A process gone before its kill counts as
// killed; any other failed kill is answered, never dropped.
//
// A PROCESS RUNS OUT OF DIR WHEN ITS COMMAND LINE OR ITS ENVIRONMENT NAMES
// IT. The command line alone missed every process handed its directory only
// through TMPDIR: a dead run's ERT Emacs (`emacs -batch ... -l
// lisp/test-*.el`) outlived the reclaim of its RAM disk (2026-10-13,
// artr-28386 and artr-74298) and went on making its temp directories under
// the detached mount point, where nothing removed them. Every unit of a run
// inherits the run's TMPDIR, so the environment names each one.
func Under(dir string) (int, error) {
	dirs, err := spellings(dir)
	if err != nil {
		return 0, err
	}
	out, err := exec.Command("ps", "-axEo", "pid=,args=").Output()
	if err != nil {
		return 0, fmt.Errorf("list processes to reclaim %s: %w", dir, err)
	}
	killed := 0
	var errs []error
	for _, pid := range Matching(string(out), dirs, os.Getpid()) {
		if err := syscall.Kill(pid, syscall.SIGKILL); err != nil && !errors.Is(err, syscall.ESRCH) {
			errs = append(errs, fmt.Errorf("kill leftover pid %d under %s: %w", pid, dir, err))
			continue
		}
		killed++
	}
	return killed, errors.Join(errs...)
}

// spellings answers dir, and its spelling with its parent's symlinks
// resolved when that differs: a process may name /tmp/artr-7 as
// /private/tmp/artr-7.
func spellings(dir string) ([]string, error) {
	dir = filepath.Clean(dir)
	parent, err := filepath.EvalSymlinks(filepath.Dir(dir))
	if err != nil {
		return nil, fmt.Errorf("resolve the directory holding %s: %w", dir, err)
	}
	if resolved := filepath.Join(parent, filepath.Base(dir)); resolved != dir {
		return []string{dir, resolved}, nil
	}
	return []string{dir}, nil
}

// Matching answers the pids of the `ps -E -o pid=,args=` lines in ps (a
// command line and its environment) that name one of dirs or a path under
// it, leaving out self.
func Matching(ps string, dirs []string, self int) []int {
	var pids []int
	for _, line := range strings.Split(ps, "\n") {
		fields := strings.Fields(line)
		if len(fields) < 2 || !slices.ContainsFunc(dirs, func(dir string) bool { return names(line, dir) }) {
			continue
		}
		pid, err := strconv.Atoi(fields[0])
		if err != nil || pid == self {
			continue
		}
		pids = append(pids, pid)
	}
	return pids
}

// names reports whether line names dir itself or a path under it: dir
// followed by a slash, a space or the line's end, never a sibling dir is
// only the prefix of.
func names(line, dir string) bool {
	dir = strings.TrimSuffix(dir, "/")
	for rest := line; ; {
		i := strings.Index(rest, dir)
		if i < 0 {
			return false
		}
		rest = rest[i+len(dir):]
		if rest == "" || rest[0] == '/' || unicode.IsSpace(rune(rest[0])) {
			return true
		}
	}
}
