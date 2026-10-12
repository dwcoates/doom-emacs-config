// Package profiling is the Go runtimes' RUNTIME CPU and heap profiling: the
// daemon, the store and the sidecar each start and stop it without a restart,
// into the one session directory bin/profile-agent-repl.sh opened.
//
// proto/vocab/profile-session.json is the contract every runtime (Go, the
// shim's TypeScript and Emacs) answers identically: where the root is, how a
// session is named, which signal reconciles, and how long a profile may run.
//
// OFF COSTS NOTHING. With no session open the only standing cost is one
// goroutine parked on a signal channel; nothing is polled and nothing samples.
package profiling

import (
	"errors"
	"fmt"
	"os"
	"os/signal"
	"path/filepath"
	"regexp"
	"runtime/pprof"
	"strings"
	"sync"
	"syscall"
	"time"
)

// RootEnvironment names the profiles root override.
const RootEnvironment = "AGENT_REPL_PROFILE_ROOT"

// DefaultRoot is the profiles root under the user's home.
const DefaultRoot = ".cache/agent-repl/profiles"

// ActiveFile is the file under the root that names the open session.
const ActiveFile = "active"

// Signal is the signal every Go and node runtime reconciles its profiling on.
// SIGUSR1 is node's inspector signal, so both use SIGUSR2.
var Signal os.Signal = syscall.SIGUSR2

// MaxDuration is how long a profile runs before the runtime stops it by
// itself.
const MaxDuration = 30 * time.Minute

// sessionPattern is a valid session id: one path component, no leading dot.
var sessionPattern = regexp.MustCompile(`^[A-Za-z0-9][A-Za-z0-9._-]{0,62}$`)

// ValidSession reports whether id names a session a runtime may write into.
func ValidSession(id string) bool { return sessionPattern.MatchString(id) }

// Root answers the profiles root: the override when set, else DefaultRoot
// under home.
func Root(getenv func(string) string, home string) (string, error) {
	if root := getenv(RootEnvironment); root != "" {
		return root, nil
	}
	if home == "" {
		return "", errors.New("profiling: no home directory to put the profiles root under")
	}
	return filepath.Join(home, DefaultRoot), nil
}

// EventKind names what a reconcile did.
type EventKind string

// The event kinds.
const (
	// Started is a profile that began writing into a session.
	Started EventKind = "started"
	// Stopped is a profile that ended and wrote its files.
	Stopped EventKind = "stopped"
	// Failed is a reconcile that could not do what the session asked.
	Failed EventKind = "failed"
)

// Event is one thing a reconcile did, for the runtime to record in its own
// log: Started and Stopped at info, Failed at error.
type Event struct {
	Kind    EventKind
	Session string
	// Reason says why a profile stopped: "session_closed", "session_changed",
	// "max_duration" or "exit".
	Reason string
	// Files are the profiles a stop wrote.
	Files []string
	Err   error
}

// Options configure a Profiler.
type Options struct {
	// Runtime names the files: <runtime>-<pid>.cpu.pprof and .heap.pprof.
	Runtime string
	// Root is the profiles root (see Root).
	Root string
	// Events receives every event. Required: a reconcile nobody records is
	// an invisible action.
	Events func(Event)
	// Max overrides MaxDuration; zero is MaxDuration.
	Max time.Duration
	// After schedules the max-duration stop; nil is time.AfterFunc.
	After func(time.Duration, func()) Timer
}

// Timer is the part of *time.Timer the max-duration stop needs.
type Timer interface{ Stop() bool }

// Profiler is one runtime's profiling state.
type Profiler struct {
	opts Options
	pid  int

	mu      sync.Mutex
	session string
	cpu     *os.File
	cpuPath string
	timer   Timer
	// generation counts the profiles started, so a max-duration stop armed
	// for one profile can never end a later one.
	generation int
}

// New builds a Profiler. It starts nothing.
func New(opts Options) (*Profiler, error) {
	switch {
	case opts.Runtime == "":
		return nil, errors.New("profiling: a profiler needs its runtime's name")
	case opts.Root == "":
		return nil, errors.New("profiling: a profiler needs the profiles root")
	case opts.Events == nil:
		return nil, errors.New("profiling: a profiler needs somewhere to record its events")
	}
	if opts.Max == 0 {
		opts.Max = MaxDuration
	}
	if opts.After == nil {
		opts.After = func(d time.Duration, f func()) Timer { return time.AfterFunc(d, f) }
	}
	return &Profiler{opts: opts, pid: os.Getpid()}, nil
}

// Session answers the session being profiled into, empty when none is.
func (p *Profiler) Session() string {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.session
}

// Reconcile makes the profiler match the root's active file: a valid session
// named there is profiled into, and no active file stops any profile.
func (p *Profiler) Reconcile() {
	want, err := p.activeSession()
	p.mu.Lock()
	defer p.mu.Unlock()
	if err != nil {
		p.opts.Events(Event{Kind: Failed, Session: p.session, Err: err})
		return
	}
	if want == p.session {
		return
	}
	if p.session != "" {
		reason := "session_closed"
		if want != "" {
			reason = "session_changed"
		}
		p.stopLocked(reason)
	}
	if want != "" {
		p.startLocked(want)
	}
}

// Stop ends a running profile and writes its files; it does nothing when no
// profile runs.
func (p *Profiler) Stop(reason string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	if p.session != "" {
		p.stopLocked(reason)
	}
}

// expire is the max-duration stop of the profile generation armed: a later
// profile is left running.
func (p *Profiler) expire(armed int) {
	p.mu.Lock()
	defer p.mu.Unlock()
	if p.session != "" && p.generation == armed {
		p.stopLocked("max_duration")
	}
}

// activeSession reads the active file: empty when there is none, an error
// when it names no valid session or cannot be read.
func (p *Profiler) activeSession() (string, error) {
	raw, err := os.ReadFile(filepath.Join(p.opts.Root, ActiveFile))
	if errors.Is(err, os.ErrNotExist) {
		return "", nil
	}
	if err != nil {
		return "", fmt.Errorf("profiling: read the active session: %w", err)
	}
	id := strings.TrimSpace(string(raw))
	if !ValidSession(id) {
		return "", fmt.Errorf("profiling: the active file names no valid session: %q", id)
	}
	return id, nil
}

// startLocked begins a CPU profile into session.
func (p *Profiler) startLocked(session string) {
	dir := filepath.Join(p.opts.Root, session)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		p.opts.Events(Event{Kind: Failed, Session: session, Err: fmt.Errorf("profiling: create the session directory: %w", err)})
		return
	}
	path := filepath.Join(dir, fmt.Sprintf("%s-%d.cpu.pprof", p.opts.Runtime, p.pid))
	f, err := os.Create(path)
	if err != nil {
		p.opts.Events(Event{Kind: Failed, Session: session, Err: fmt.Errorf("profiling: create the cpu profile: %w", err)})
		return
	}
	if err := pprof.StartCPUProfile(f); err != nil {
		closeErr := f.Close()
		p.opts.Events(Event{Kind: Failed, Session: session, Err: errors.Join(fmt.Errorf("profiling: start the cpu profile: %w", err), closeErr)})
		return
	}
	p.session, p.cpu, p.cpuPath = session, f, path
	p.generation++
	armed := p.generation
	p.timer = p.opts.After(p.opts.Max, func() { p.expire(armed) })
	p.opts.Events(Event{Kind: Started, Session: session, Files: []string{path}})
}

// stopLocked ends the CPU profile and writes the heap profile beside it.
func (p *Profiler) stopLocked(reason string) {
	session := p.session
	p.timer.Stop()
	pprof.StopCPUProfile()
	files := []string{p.cpuPath}
	err := p.cpu.Close()
	if err != nil {
		err = fmt.Errorf("profiling: close the cpu profile: %w", err)
	}
	heapPath := filepath.Join(filepath.Dir(p.cpuPath), fmt.Sprintf("%s-%d.heap.pprof", p.opts.Runtime, p.pid))
	if heapErr := writeHeap(heapPath); heapErr != nil {
		err = errors.Join(err, heapErr)
	} else {
		files = append(files, heapPath)
	}
	p.session, p.cpu, p.cpuPath, p.timer = "", nil, "", nil
	if err != nil {
		p.opts.Events(Event{Kind: Failed, Session: session, Reason: reason, Files: files, Err: err})
		return
	}
	p.opts.Events(Event{Kind: Stopped, Session: session, Reason: reason, Files: files})
}

// writeHeap writes the heap profile (in-use and allocated) to path.
func writeHeap(path string) error {
	f, err := os.Create(path)
	if err != nil {
		return fmt.Errorf("profiling: create the heap profile: %w", err)
	}
	if err := pprof.Lookup("heap").WriteTo(f, 0); err != nil {
		return errors.Join(fmt.Errorf("profiling: write the heap profile: %w", err), f.Close())
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("profiling: close the heap profile: %w", err)
	}
	return nil
}

// Watch reconciles p on every Signal until done closes, then stops any
// running profile with reason "exit". It returns once it has stopped
// listening; run it on its own goroutine.
func Watch(p *Profiler, done <-chan struct{}) {
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, Signal)
	defer signal.Stop(ch)
	watch(p, done, ch)
}

// watch is Watch's loop over an already-subscribed signal channel.
func watch(p *Profiler, done <-chan struct{}, ch <-chan os.Signal) {
	for {
		select {
		case <-ch:
			p.Reconcile()
		case <-done:
			p.Stop("exit")
			return
		}
	}
}
