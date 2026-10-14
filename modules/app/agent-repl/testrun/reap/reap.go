// Package reap kills what a test run left running: every process of the run,
// frozen first so none can start another, then killed, then confirmed gone.
// It is the ONE reap of the test runner: a run's own release of its RAM disk,
// the runner reaping its session when its sentinel dies, the warden reaping a
// dead runner's session, the next run's reclaim of a dead run's disk, and the
// daemon integration harness's reclaim of a dead run root all go through Reap.
//
// WHY FREEZE FIRST. A dead run's processes were killed from ONE `ps` snapshot,
// one kill each, while the run's tree was still forking: a child forked
// between the snapshot and its parent's kill was never seen, survived the
// kill, and was left running out of a RAM disk the reclaim then
// force-detached. Its executable revoked, it spun for hours (2026-10-13 and
// 14: twenty processes of six dead runs on about ten cores, killed by hand). A
// stopped process cannot fork, so Reap stops every member it finds, pass after
// pass, until a pass finds no member it has not already stopped. Only then is
// every member killed, and only once every one has exited does Reap succeed.
package reap

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strings"
	"syscall"
	"time"
	"unicode"
)

// Proc is one process as the table lists it.
type Proc struct {
	Pid int
	// Sid is its session id.
	Sid int
	// State is ps's first state letter: 'T' stopped, 'Z' a zombie.
	State byte
	// Start is when it started, to the second.
	Start time.Time
	// Line is its command line followed by its environment.
	Line string
}

// command names the process for a log line: its executable's base name.
func (p Proc) command() string {
	fields := strings.Fields(p.Line)
	if len(fields) == 0 {
		return "?"
	}
	return filepath.Base(fields[0])
}

// Table is the host's process table.
type Table interface {
	// List answers every process.
	List() ([]Proc, error)
	// Signal sends sig to pid; a process already gone answers ESRCH.
	Signal(pid int, sig syscall.Signal) error
	// AwaitExit waits until every one of pids has exited or bound has
	// passed, and answers the pids still not exited.
	AwaitExit(pids []int, bound time.Duration) ([]int, error)
}

// Logger is the reaper's log: testrun's run.Log.
type Logger interface {
	Infof(format string, args ...any)
	Errorf(format string, args ...any)
}

// Members names a run's processes.
type Members struct {
	// What names the run in every log line.
	What string
	// Session is the run's session id: every process in it is a member, the
	// run's kernel-held membership, which no process can drop short of
	// leaving the session. Zero is no session.
	Session int
	// Since is when the session's leader started. A process of the session
	// started before it is not a member, and a LIVE leader that started at
	// another time means the id names another session now, so the session
	// is not used.
	Since time.Time
	// Dirs is the second net: a process whose command line or environment
	// names one of them, or a path under one, is a member. It catches the one
	// process known to leave a run's session, the daemon rollout's successor
	// (daemon/internal/rollout/spawn.go starts it with Setsid) in the
	// handover tests, which still runs out of the run's directories.
	Dirs []string
	// Except are never members: the reaper and the warden themselves.
	Except []int
}

// sessionUsable answers whether m.Session still names the run's session.
func (m Members) sessionUsable(procs []Proc) bool {
	if m.Session == 0 {
		return false
	}
	for _, p := range procs {
		if p.Pid == m.Session && p.State != 'Z' {
			return p.Start.Equal(m.Since)
		}
	}
	return true
}

func (m Members) member(p Proc, session bool) bool {
	if slices.Contains(m.Except, p.Pid) {
		return false
	}
	if session && p.Sid == m.Session && !p.Start.Before(m.Since) {
		return true
	}
	return slices.ContainsFunc(m.Dirs, func(dir string) bool { return names(p.Line, dir) })
}

// KillExitBound is how long the killed members have to exit. A SIGKILLed
// process exits as soon as it is scheduled; one that has not within the bound
// is stuck in the kernel (on a wedged disk, say), and the reap fails rather
// than let anything be detached under it.
//
// MEASURED (2026-10-14, load 6): 147 members of a forking loop all exited
// 2.4ms after their kill, one harness leftover 142us after. Like ListBound,
// the bound covers members at the run's nice 19 waiting for a CPU on a loaded
// host, whose slowest healthy wait the runs record is 471ms
// (ramdisk.InfoBound): just over twice that. Every reap logs its exit time,
// so the bound can be set again from the runs.
const KillExitBound = 1000 * time.Millisecond

// Result is what a reap did.
type Result struct {
	// Passes is how many freeze passes ran, the last finding nothing new.
	Passes int
	// Killed are the members it killed.
	Killed []Proc
}

// Reap freezes, kills and confirms gone every member of m.
//
// Each freeze pass lists the table and stops every member not yet stopped; the
// passes end once one finds none. Every stopped member is then SIGKILLed, and
// Reap waits for each to exit. A last listing confirms no member remains.
//
// Every pass, the kill and the confirmation are logged, each with how long its
// listing took (ListBound is set from those times); a failure is logged at
// ERROR and answered, and the caller then keeps whatever the members ran out
// of (a RAM disk is never detached under a member).
func Reap(t Table, m Members, log Logger) (Result, error) {
	var res Result
	fail := func(format string, args ...any) (Result, error) {
		err := fmt.Errorf("reap %s: "+format, append([]any{m.What}, args...)...)
		log.Errorf("%v", err)
		return res, err
	}
	stopped := map[int]Proc{}
	var errs []error
	for {
		listedAt := time.Now()
		procs, err := t.List()
		listTook := time.Since(listedAt)
		if err != nil {
			return fail("list the processes on freeze pass %d: %w", res.Passes+1, err)
		}
		res.Passes++
		session := m.sessionUsable(procs)
		if m.Session != 0 && !session && res.Passes == 1 {
			log.Infof("reap %s: session %d now names another session (its leader started at another time than %s), so only the directory net is used", m.What, m.Session, m.Since.Format(time.DateTime))
		}
		var fresh []Proc
		for _, p := range procs {
			if _, done := stopped[p.Pid]; done || p.State == 'Z' || !m.member(p, session) {
				continue
			}
			fresh = append(fresh, p)
		}
		if len(fresh) == 0 {
			log.Infof("reap %s: freeze pass %d found no member left running; %d member(s) stopped; the listing took %s", m.What, res.Passes, len(stopped), listTook.Round(time.Microsecond))
			break
		}
		for _, p := range fresh {
			stopped[p.Pid] = p
			if err := t.Signal(p.Pid, syscall.SIGSTOP); err != nil && !errors.Is(err, syscall.ESRCH) {
				errs = append(errs, fmt.Errorf("stop pid %d (%s): %w", p.Pid, p.command(), err))
			}
		}
		log.Infof("reap %s: freeze pass %d stopped %d process(es): %s; the listing took %s", m.What, res.Passes, len(fresh), describe(fresh), listTook.Round(time.Microsecond))
	}
	// A MEMBER THAT WOULD NOT STOP IS STILL KILLED WITH THE REST, and the
	// failure is answered once all of them are dealt with: a reap that gave up
	// here would leave every member it stopped frozen for good.
	pids := make([]int, 0, len(stopped))
	for pid, p := range stopped {
		if err := t.Signal(pid, syscall.SIGKILL); err != nil && !errors.Is(err, syscall.ESRCH) {
			errs = append(errs, fmt.Errorf("kill pid %d (%s): %w", pid, p.command(), err))
			continue
		}
		pids = append(pids, pid)
		res.Killed = append(res.Killed, p)
	}
	sort.Ints(pids)
	sort.Slice(res.Killed, func(i, j int) bool { return res.Killed[i].Pid < res.Killed[j].Pid })
	var exitTook time.Duration
	if len(pids) > 0 {
		killedAt := time.Now()
		left, err := t.AwaitExit(pids, KillExitBound)
		exitTook = time.Since(killedAt)
		switch {
		case err != nil:
			errs = append(errs, fmt.Errorf("wait for the %d killed member(s) to exit: %w", len(pids), err))
		case len(left) > 0:
			errs = append(errs, fmt.Errorf("%d killed member(s) had not exited within %s: pids %v", len(left), KillExitBound, left))
		}
	}
	confirmedAt := time.Now()
	procs, err := t.List()
	confirmTook := time.Since(confirmedAt)
	if err != nil {
		errs = append(errs, fmt.Errorf("list the processes to confirm no member remains: %w", err))
	} else {
		session := m.sessionUsable(procs)
		var remain []Proc
		for _, p := range procs {
			if p.State != 'Z' && m.member(p, session) {
				remain = append(remain, p)
			}
		}
		if len(remain) > 0 {
			errs = append(errs, fmt.Errorf("%d member(s) remain after the kill: %s", len(remain), describe(remain)))
		}
	}
	if len(errs) > 0 {
		return fail("%w", errors.Join(errs...))
	}
	if len(res.Killed) > 0 {
		log.Infof("reap %s: killed %d member(s), all exited in %s: %s; the confirming listing took %s", m.What, len(res.Killed), exitTook.Round(time.Microsecond), describe(res.Killed), confirmTook.Round(time.Microsecond))
	}
	return res, nil
}

// tmpdir answers the TMPDIR the process's environment names, or "".
func (p Proc) tmpdir() string {
	for _, field := range strings.Fields(p.Line) {
		if v, ok := strings.CutPrefix(field, "TMPDIR="); ok {
			return v
		}
	}
	return ""
}

// describe names processes "command[pid]", in pid order, each with the TMPDIR
// it was handed when it has one: a run gives every unit a root of its own, so
// the TMPDIR says which unit left the process.
func describe(procs []Proc) string {
	sorted := slices.Clone(procs)
	sort.Slice(sorted, func(i, j int) bool { return sorted[i].Pid < sorted[j].Pid })
	names := make([]string, len(sorted))
	for i, p := range sorted {
		names[i] = fmt.Sprintf("%s[%d]", p.command(), p.Pid)
		if dir := p.tmpdir(); dir != "" {
			names[i] += " (TMPDIR=" + dir + ")"
		}
	}
	return strings.Join(names, ", ")
}

// Under reaps every process running out of dir: whose command line or
// environment names it, or a path under it. It is the daemon integration
// harness's reclaim of a dead run root, which knows no session.
func Under(t Table, dir string, log Logger) (Result, error) {
	dirs, err := Spellings(dir)
	if err != nil {
		return Result{}, err
	}
	return Reap(t, Members{What: dir, Dirs: dirs, Except: []int{os.Getpid()}}, log)
}

// Spellings answers dir, and its spelling with its parent's symlinks
// resolved when that differs: a process may name /tmp/artr-7 as
// /private/tmp/artr-7.
func Spellings(dir string) ([]string, error) {
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
