package dlog

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"sync"
)

// RunLogBackups is how many previous run logs are retained beside the current
// one, as daemon.run.log.1 (the most recent) through .N (the oldest).
const RunLogBackups = 5

// runLog is the restart-scoped run log, which is also the daemon's global
// sink: the state root layout names no second global file, so a record with no
// conceptual workspace lands here.
//
// Opening it is a BOOT FATAL. A daemon whose own narrative has nowhere to go
// cannot report what it then does wrong, so openRunLog's error is returned
// from OpenSurfaces and the caller must treat it as fatal.
type runLog struct {
	path    string
	backups int

	mu     sync.Mutex
	f      *os.File
	size   int64
	poison error
}

// openRunLog rotates the previous run's file out of the way and opens a fresh
// one. The rotation is what makes the log restart-scoped: the current file
// always describes exactly this run.
func openRunLog(path string, backups int) (*runLog, error) {
	if path == "" {
		return nil, fmt.Errorf("run log path is empty")
	}
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, fmt.Errorf("create run log directory %q: %w", dir, err)
	}
	if err := rotateRunLog(path, backups); err != nil {
		return nil, err
	}
	f, err := os.OpenFile(path, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o644)
	if err != nil {
		return nil, fmt.Errorf("open run log %q: %w", path, err)
	}
	info, err := f.Stat()
	if err != nil {
		f.Close()
		return nil, fmt.Errorf("stat run log %q: %w", path, err)
	}
	return &runLog{path: path, backups: backups, f: f, size: info.Size()}, nil
}

// rotateRunLog shifts the retained backups down one slot and moves the
// current file into slot 1. The oldest slot is discarded. A missing file at
// any slot is not an error: the first boot on a fresh state root has none.
func rotateRunLog(path string, backups int) error {
	if backups < 1 {
		return removeIfPresent(path)
	}
	if err := removeIfPresent(backupPath(path, backups)); err != nil {
		return err
	}
	for i := backups - 1; i >= 1; i-- {
		from, to := backupPath(path, i), backupPath(path, i+1)
		if err := renameIfPresent(from, to); err != nil {
			return err
		}
	}
	return renameIfPresent(path, backupPath(path, 1))
}

// backupPath names the i-th retained backup.
func backupPath(path string, i int) string { return path + "." + strconv.Itoa(i) }

func removeIfPresent(path string) error {
	if err := os.Remove(path); err != nil && !os.IsNotExist(err) {
		return fmt.Errorf("remove run log backup %q: %w", path, err)
	}
	return nil
}

func renameIfPresent(from, to string) error {
	if _, err := os.Lstat(from); err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return fmt.Errorf("stat run log %q while rotating: %w", from, err)
	}
	if err := os.Rename(from, to); err != nil {
		return fmt.Errorf("rotate run log %q to %q: %w", from, to, err)
	}
	return nil
}

// write appends one JSONL line, enforcing the in-run cap first.
func (r *runLog) write(line []byte) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.poison != nil {
		return r.poison
	}
	if r.size+int64(len(line)) > CapBytes {
		if err := r.rollLocked(); err != nil {
			return err
		}
	}
	n, err := r.f.Write(line)
	r.size += int64(n)
	if err != nil {
		r.poison = fmt.Errorf("%w: append to run log %q: %w", ErrPoisoned, r.path, err)
		return r.poison
	}
	return nil
}

// rollLocked enforces the in-run cap by rotating and reopening rather than by
// truncating or refusing: the newest evidence is the evidence an operator
// wants, and the retained backups mean the older half is still on disk. A
// single very long run can therefore produce more than one file, which is why
// the backups exist.
func (r *runLog) rollLocked() error {
	if err := r.f.Close(); err != nil {
		r.poison = fmt.Errorf("%w: close run log %q at the cap: %w", ErrPoisoned, r.path, err)
		return r.poison
	}
	if err := rotateRunLog(r.path, r.backups); err != nil {
		r.poison = fmt.Errorf("%w: %w", ErrPoisoned, err)
		return r.poison
	}
	f, err := os.OpenFile(r.path, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o644)
	if err != nil {
		r.poison = fmt.Errorf("%w: reopen run log %q at the cap: %w", ErrPoisoned, r.path, err)
		return r.poison
	}
	r.f = f
	r.size = 0
	return nil
}

// close releases the run log's descriptor.
func (r *runLog) close() error {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.f == nil {
		return nil
	}
	err := r.f.Close()
	r.f = nil
	if r.poison == nil {
		r.poison = fmt.Errorf("%w: run log %q is closed", ErrPoisoned, r.path)
	}
	if err != nil {
		return fmt.Errorf("close run log %q: %w", r.path, err)
	}
	return nil
}
