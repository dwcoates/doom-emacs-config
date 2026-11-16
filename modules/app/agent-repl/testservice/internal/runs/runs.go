// Package runs is the test service's run registry: every run it starts, its
// output kept line by line on disk, and its end.
//
// A RUN OUTLIVES ITS WATCHERS, NEVER ITS SERVICE. The daemon starts a test
// gate's run here and watches it; a daemon deploy ends the watch and the new
// daemon watches the same run again by its id, from the line after its last,
// so the gate is never cut. The run's own process group lives exactly as
// long as this service: when the service stops, every run it was running is
// killed with it (Open settles each as lost), because nothing could read its
// output on any later life of the service.
//
// Each run is a directory under the registry's root, named by its id:
//
//	run.json    what was started, and the process group it leads
//	output.log  every line the run wrote, standard output and error combined
//	end.json    how it ended; written once, last
package runs

import (
	"bufio"
	"context"
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sync"
	"syscall"
	"time"

	"agentrepl/logging/childgroup"
	"agentrepl/logging/recordlog"
)

// Retention is how long an ended run's record is kept. A gate's merge
// resumes within minutes of a deploy; a day covers a queue held overnight.
const Retention = 24 * time.Hour

// ErrUnknownRun is a run id this registry holds no record of.
var ErrUnknownRun = errors.New("runs: no run of that id")

// Spec is what a run starts: ARGV in DIR, its environment the service's own
// scrubbed of every repository selector, plus ENV.
type Spec struct {
	Dir   string
	Argv  []string
	Env   []string
	Label string
}

// End is how a run ended.
type End struct {
	// Lost reports that the service stopped while the run ran; LostDetail
	// says so in one line. A lost run has no exit.
	Lost       bool   `json:"lost,omitempty"`
	LostDetail string `json:"lost_detail,omitempty"`
	// ExitCode is the command's exit code, -1 when a signal ended it.
	ExitCode int `json:"exit_code"`
	// Stopped reports that a Stop ended it.
	Stopped bool `json:"stopped,omitempty"`
	// CPU is the processor time the command and every descendant it waited
	// for spent; Wall how long the run took.
	CPU  time.Duration `json:"cpu"`
	Wall time.Duration `json:"wall"`
}

// Event is one thing a watcher is told: a line, or the run's end.
type Event struct {
	// Number and Text are a line, its place from one.
	Number uint64
	Text   string
	// End is set on the last event only.
	End *End
}

// record is run.json.
type record struct {
	Dir       string    `json:"dir"`
	Argv      []string  `json:"argv"`
	Label     string    `json:"label"`
	PGID      int       `json:"pgid"`
	StartedAt time.Time `json:"started_at"`
}

// Registry holds every run this service started and the records of those it
// ended.
type Registry struct {
	root  string
	log   *recordlog.Logger
	clock func() time.Time

	mu   sync.Mutex
	live map[string]*run
}

// run is one running run's live state.
type run struct {
	id   string
	pgid int

	mu sync.Mutex
	// written is how many lines output.log holds; changed is closed and
	// replaced on every line and at the end, waking every watcher.
	written uint64
	changed chan struct{}
	end     *End
	stopped bool
}

// Open opens the registry at ROOT. Every run a previous life of the service
// left running is killed and settled as lost, and every ended run past
// Retention is pruned.
func Open(root string, log *recordlog.Logger, clock func() time.Time) (*Registry, error) {
	const op = "tests.runs.open"
	if root == "" || log == nil || clock == nil {
		panic("runs: Open needs a root, a logger and a clock")
	}
	if err := os.MkdirAll(root, 0o755); err != nil {
		log.Error(op, "the run registry's root could not be made", recordlog.Context{"root": root, "cause": err.Error()})
		return nil, err
	}
	r := &Registry{root: root, log: log, clock: clock, live: map[string]*run{}}
	entries, err := os.ReadDir(root)
	if err != nil {
		log.Error(op, "the run registry's root could not be read", recordlog.Context{"root": root, "cause": err.Error()})
		return nil, err
	}
	for _, e := range entries {
		if e.IsDir() {
			r.settleLeftover(e.Name())
		}
	}
	return r, nil
}

// settleLeftover settles one run a previous life left: lost when it never
// ended, pruned when it ended past Retention.
func (r *Registry) settleLeftover(id string) {
	const op = "tests.runs.open"
	fields := recordlog.Context{"run": id}
	if info, err := os.Stat(r.endPath(id)); err == nil {
		if r.clock().Sub(info.ModTime()) > Retention {
			if err := os.RemoveAll(r.dir(id)); err != nil {
				r.log.Error(op, "an ended run past its retention could not be pruned", withCause(fields, err))
				return
			}
			r.log.Debug(op, "pruned an ended run past its retention", fields)
		}
		return
	}
	rec, err := r.readRecord(id)
	if err == nil && rec.PGID > 0 {
		if err := childgroup.KillGroup(rec.PGID); err != nil && !errors.Is(err, os.ErrProcessDone) {
			r.log.Error(op, "a run a previous life of the service left running could not be killed", withCause(fields, err))
		}
	}
	end := End{Lost: true, LostDetail: "the test service stopped while the run was running"}
	if err := writeJSON(r.endPath(id), end); err != nil {
		r.log.Error(op, "a run a previous life of the service left running could not be settled as lost", withCause(fields, err))
		return
	}
	r.log.Info(op, "settled a run a previous life of the service left running as lost; its process group was killed", fields)
}

// Start starts SPEC and answers its id. A run whose command cannot be
// spawned is an error, and leaves no record.
func (r *Registry) Start(spec Spec) (string, error) {
	const op = "tests.runs.start"
	id, err := mintID()
	if err != nil {
		r.log.Error(op, "a run id could not be minted", recordlog.Context{"label": spec.Label, "cause": err.Error()})
		return "", err
	}
	fields := recordlog.Context{"run": id, "label": spec.Label, "dir": spec.Dir}
	if err := os.MkdirAll(r.dir(id), 0o755); err != nil {
		r.log.Error(op, "the run's directory could not be made", withCause(fields, err))
		return "", &StorageError{err: err}
	}
	out, err := os.OpenFile(r.outputPath(id), os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		r.log.Error(op, "the run's output could not be opened", withCause(fields, err))
		_ = os.RemoveAll(r.dir(id))
		return "", &StorageError{err: err}
	}
	cmd := exec.Command(spec.Argv[0], spec.Argv[1:]...)
	cmd.Dir = spec.Dir
	cmd.Env = append(childgroup.ScrubRepositorySelectors(os.Environ()), spec.Env...)
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	pipe, err := cmd.StdoutPipe()
	if err != nil {
		_ = out.Close()
		_ = os.RemoveAll(r.dir(id))
		r.log.Error(op, "the run's output pipe could not be made", withCause(fields, err))
		return "", &StorageError{err: err}
	}
	cmd.Stderr = cmd.Stdout
	started := r.clock()
	if err := cmd.Start(); err != nil {
		_ = out.Close()
		_ = os.RemoveAll(r.dir(id))
		r.log.Warn(op, "the run's command could not be started", withCause(fields, err))
		return "", &UnstartableError{err: err}
	}
	rec := record{Dir: spec.Dir, Argv: spec.Argv, Label: spec.Label, PGID: cmd.Process.Pid, StartedAt: started}
	if err := writeJSON(r.recordPath(id), rec); err != nil {
		// A RUN WITH NO RECORD OF ITS GROUP could not be killed by the next
		// life of the service, so it is not left running.
		_ = childgroup.KillGroup(cmd.Process.Pid)
		_ = cmd.Wait()
		_ = out.Close()
		_ = os.RemoveAll(r.dir(id))
		r.log.Error(op, "the run's record could not be written; the run was killed", withCause(fields, err))
		return "", &StorageError{err: err}
	}
	live := &run{id: id, pgid: cmd.Process.Pid, changed: make(chan struct{})}
	r.mu.Lock()
	r.live[id] = live
	r.mu.Unlock()
	r.log.Info(op, "started a run", recordlog.Context{"run": id, "label": spec.Label, "dir": spec.Dir, "pgid": cmd.Process.Pid})
	go r.follow(live, cmd, pipe, out, started, fields)
	return id, nil
}

// follow copies the run's output into output.log line by line, waking every
// watcher on each, and records the run's end once it exits.
func (r *Registry) follow(live *run, cmd *exec.Cmd, pipe io.Reader, out *os.File, started time.Time, fields recordlog.Context) {
	const op = "tests.runs.follow"
	reader := bufio.NewReader(pipe)
	var writeErr error
	for {
		line, err := reader.ReadString('\n')
		if len(line) > 0 {
			if line[len(line)-1] != '\n' {
				line += "\n"
			}
			if _, werr := out.WriteString(line); werr != nil && writeErr == nil {
				writeErr = werr
				r.log.Error(op, "a line of the run could not be recorded; the run goes on, and its watchers miss what follows", withCause(fields, werr))
			}
			if writeErr == nil {
				live.wake(func() { live.written++ })
			}
		}
		if err != nil {
			if !errors.Is(err, io.EOF) {
				r.log.Error(op, "the run's output could not be read to its end", withCause(fields, err))
			}
			break
		}
	}
	waitErr := cmd.Wait()
	end := End{ExitCode: -1, Wall: r.clock().Sub(started)}
	if state := cmd.ProcessState; state != nil {
		end.ExitCode = state.ExitCode()
		end.CPU = state.UserTime() + state.SystemTime()
	}
	if waitErr != nil {
		var exit *exec.ExitError
		if !errors.As(waitErr, &exit) {
			r.log.Error(op, "the run's exit could not be read", withCause(fields, waitErr))
		}
	}
	if err := out.Close(); err != nil {
		r.log.Error(op, "the run's output could not be closed", withCause(fields, err))
	}
	live.mu.Lock()
	end.Stopped = live.stopped
	live.mu.Unlock()
	if err := writeJSON(r.endPath(live.id), end); err != nil {
		r.log.Error(op, "the run's end could not be recorded; its watchers are told it from memory, and a later life of the service reads it as lost", withCause(fields, err))
	}
	r.log.Info(op, "the run ended", recordlog.Context{"run": live.id, "label": fields["label"], "exit_code": end.ExitCode,
		"stopped": end.Stopped, "wall_seconds": end.Wall.Seconds(), "cpu_seconds": end.CPU.Seconds()})
	live.wake(func() { live.end = &end })
	r.mu.Lock()
	delete(r.live, live.id)
	r.mu.Unlock()
}

// wake applies CHANGE under the run's lock and wakes every watcher.
func (l *run) wake(change func()) {
	l.mu.Lock()
	change()
	old := l.changed
	l.changed = make(chan struct{})
	l.mu.Unlock()
	close(old)
}

// Stop kills run ID's process group. A run already ended is left as it
// ended.
func (r *Registry) Stop(id string) error {
	const op = "tests.runs.stop"
	r.mu.Lock()
	live, running := r.live[id]
	r.mu.Unlock()
	if !running {
		if _, err := os.Stat(r.endPath(id)); err == nil {
			r.log.Debug(op, "a stop reached a run that had already ended; it is left as it ended", recordlog.Context{"run": id})
			return nil
		}
		return ErrUnknownRun
	}
	live.mu.Lock()
	live.stopped = true
	live.mu.Unlock()
	if err := childgroup.KillGroup(live.pgid); err != nil && !errors.Is(err, os.ErrProcessDone) {
		r.log.Error(op, "the run's process group could not be killed", recordlog.Context{"run": id, "pgid": live.pgid, "cause": err.Error()})
		return err
	}
	r.log.Info(op, "stopped a run; its process group was killed", recordlog.Context{"run": id, "pgid": live.pgid})
	return nil
}

// Watch tells EMIT every line of run ID from line FROM on (one when zero),
// in order, then each new one as the run writes it, then the run's end. It
// returns when the end was told, when EMIT fails, or when ctx ends.
func (r *Registry) Watch(ctx context.Context, id string, from uint64, emit func(Event) error) error {
	if from == 0 {
		from = 1
	}
	r.mu.Lock()
	live, running := r.live[id]
	r.mu.Unlock()
	file, err := os.Open(r.outputPath(id))
	if errors.Is(err, os.ErrNotExist) {
		return ErrUnknownRun
	}
	if err != nil {
		return &StorageError{err: err}
	}
	defer file.Close()
	reader := bufio.NewReader(file)
	var number uint64
	// readTo tells every complete line through line LIMIT.
	readTo := func(limit uint64) error {
		for number < limit {
			line, err := reader.ReadString('\n')
			if err != nil {
				return &StorageError{err: fmt.Errorf("line %d of %d: %w", number+1, limit, err)}
			}
			number++
			if number >= from {
				if err := emit(Event{Number: number, Text: line[:len(line)-1]}); err != nil {
					return err
				}
			}
		}
		return nil
	}
	if !running {
		end, err := r.readEnd(id)
		if errors.Is(err, os.ErrNotExist) {
			// ENDED BETWEEN THE TWO READS: its end is written before it
			// leaves the live set, so a run neither live nor ended is the
			// end's write having failed. Its watchers were told from memory.
			return &StorageError{err: fmt.Errorf("run %s is neither running nor ended", id)}
		}
		if err != nil {
			return &StorageError{err: err}
		}
		for {
			line, err := reader.ReadString('\n')
			if errors.Is(err, io.EOF) {
				break
			}
			if err != nil {
				return &StorageError{err: err}
			}
			number++
			if number >= from {
				if err := emit(Event{Number: number, Text: line[:len(line)-1]}); err != nil {
					return err
				}
			}
		}
		return emit(Event{End: &end})
	}
	for {
		live.mu.Lock()
		written, end, changed := live.written, live.end, live.changed
		live.mu.Unlock()
		if err := readTo(written); err != nil {
			return err
		}
		if end != nil {
			return emit(Event{End: end})
		}
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-changed:
		}
	}
}

// StorageError is a run's record that could not be written or read.
type StorageError struct{ err error }

func (e *StorageError) Error() string { return "runs: storage: " + e.err.Error() }
func (e *StorageError) Unwrap() error { return e.err }

// UnstartableError is a run whose command could not be spawned.
type UnstartableError struct{ err error }

func (e *UnstartableError) Error() string { return "runs: unstartable: " + e.err.Error() }
func (e *UnstartableError) Unwrap() error { return e.err }

func (r *Registry) dir(id string) string        { return filepath.Join(r.root, id) }
func (r *Registry) recordPath(id string) string { return filepath.Join(r.root, id, "run.json") }
func (r *Registry) outputPath(id string) string { return filepath.Join(r.root, id, "output.log") }
func (r *Registry) endPath(id string) string    { return filepath.Join(r.root, id, "end.json") }

func (r *Registry) readRecord(id string) (record, error) {
	var rec record
	body, err := os.ReadFile(r.recordPath(id))
	if err != nil {
		return rec, err
	}
	return rec, json.Unmarshal(body, &rec)
}

func (r *Registry) readEnd(id string) (End, error) {
	var end End
	body, err := os.ReadFile(r.endPath(id))
	if err != nil {
		return end, err
	}
	return end, json.Unmarshal(body, &end)
}

// writeJSON writes V to PATH through a temporary file and a rename, so a
// reader never sees half of it.
func writeJSON(path string, v any) error {
	body, err := json.Marshal(v)
	if err != nil {
		return err
	}
	tmp := path + ".tmp"
	if err := os.WriteFile(tmp, body, 0o644); err != nil {
		return err
	}
	return os.Rename(tmp, path)
}

func mintID() (string, error) {
	var b [8]byte
	if _, err := rand.Read(b[:]); err != nil {
		return "", err
	}
	return hex.EncodeToString(b[:]), nil
}

func withCause(fields recordlog.Context, err error) recordlog.Context {
	out := make(recordlog.Context, len(fields)+1)
	for k, v := range fields {
		out[k] = v
	}
	out["cause"] = err.Error()
	return out
}
