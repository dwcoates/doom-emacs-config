// Package replog opens the daemon's on-disk log file, rotating the
// previous run's file aside and pruning old backups.
//
// Historically every log.Printf line went only to stderr, which Emacs
// captures in the ephemeral *claude-repld* buffer — so the evidence of a
// stall or drop died with the buffer (or the editor). Writing the same
// stream to disk under the shared state root makes daemon history
// readable without a live Emacs session.
//
// Rotation is restart-scoped, mirroring the Emacs module's own
// doom-agent-repl.log/.prev convention but keeping five generations: on
// every boot the previous run's file is renamed to
// claude-repld.log.<mtime stamp> and only the newest KeepBackups such
// backups survive.
package replog

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"time"
)

// FileName is the current run's log file under the state root. Backups
// carry a `.<stamp>` suffix derived from the rotated file's mtime.
const FileName = "claude-repld.log"

// KeepBackups is how many rotated log files survive a boot; older ones
// are deleted during rotation.
const KeepBackups = 5

// stampLayout formats a backup suffix. Second precision is enough to be
// meaningful; a same-second restart is disambiguated with a counter.
const stampLayout = "20060102-150405"

// Open rotates DIR's existing log file aside, prunes backups beyond
// KeepBackups, and opens a fresh FileName for appending. Prune failures
// are not fatal (the fresh file is what boot needs); they are returned
// as warnings for the caller to log once its sink is wired.
func Open(dir string) (f *os.File, warnings []string, err error) {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, nil, fmt.Errorf("replog: create log dir: %w", err)
	}
	current := filepath.Join(dir, FileName)
	if fi, statErr := os.Stat(current); statErr == nil {
		if renameErr := rotate(current, fi.ModTime().Format(stampLayout)); renameErr != nil {
			return nil, nil, fmt.Errorf("replog: rotate previous log: %w", renameErr)
		}
	} else if !os.IsNotExist(statErr) {
		return nil, nil, fmt.Errorf("replog: stat previous log: %w", statErr)
	}
	warnings = prune(dir)
	f, err = os.OpenFile(current, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		return nil, nil, fmt.Errorf("replog: open log file: %w", err)
	}
	return f, warnings, nil
}

// rotate renames CURRENT to CURRENT.<stamp>, appending -1, -2, … when a
// backup of that stamp already exists (two restarts inside one second).
func rotate(current, stamp string) error {
	dst := current + "." + stamp
	for n := 1; ; n++ {
		if _, err := os.Stat(dst); os.IsNotExist(err) {
			break
		}
		dst = fmt.Sprintf("%s.%s-%d", current, stamp, n)
	}
	return os.Rename(current, dst)
}

// CapBytes is the default in-run size cap a CappedWriter applies to the
// current log file: once a single run has written this many bytes to
// it, the file is rotated aside exactly like a fresh boot would, so a
// daemon whose run logs unexpectedly heavily (a busy loop, a chatty
// remote session) cannot grow the current log file without bound
// between restarts.
const CapBytes = 1 << 30 // 1 GiB

// CappedWriter wraps the current run's log file and rotates it aside,
// mid-run, once this run has written at least Cap bytes to it: closing
// the file, renaming it to a stamped backup with the same naming Open
// uses, pruning backups beyond KeepBackups, and reopening a fresh
// current file. Every transition happens under mu so a rotation can
// never interleave with (or be interleaved by) a concurrent Write.
type CappedWriter struct {
	mu      sync.Mutex
	dir     string
	file    *os.File
	written int64
	// Cap is the byte threshold that triggers a rotation. Exported so
	// tests can force a rotation with a tiny cap instead of writing a
	// gigabyte of log lines; production callers should go through
	// NewCappedWriter, which defaults it to CapBytes.
	Cap int64
}

// NewCappedWriter wraps f — the file at DIR/FileName as returned by
// Open — with a mid-run rotation cap. capBytes <= 0 falls back to
// CapBytes.
func NewCappedWriter(dir string, f *os.File, capBytes int64) *CappedWriter {
	if capBytes <= 0 {
		capBytes = CapBytes
	}
	return &CappedWriter{dir: dir, file: f, Cap: capBytes}
}

// Write appends p to the current log file, then rotates mid-run once
// this run's running total has crossed Cap. The bytes the caller handed
// in are always durably written before the cap check runs, so a
// rotation failure — reported to stderr, since the log sink itself is
// what just failed — can never turn into a lost log line, only a log
// file that keeps growing past its cap.
func (w *CappedWriter) Write(p []byte) (int, error) {
	w.mu.Lock()
	defer w.mu.Unlock()
	n, err := w.file.Write(p)
	w.written += int64(n)
	if err != nil {
		return n, err
	}
	if w.written >= w.Cap {
		if rotErr := w.rotateLocked(); rotErr != nil {
			fmt.Fprintf(os.Stderr, "replog: mid-run rotation: %v\n", rotErr)
		}
	}
	return n, nil
}

// rotateLocked performs the mid-run rotation described on CappedWriter.
// Called with mu held.
func (w *CappedWriter) rotateLocked() error {
	reached := w.written
	if err := w.file.Close(); err != nil {
		return fmt.Errorf("replog: close for mid-run rotation: %w", err)
	}
	current := filepath.Join(w.dir, FileName)
	if err := rotate(current, time.Now().Format(stampLayout)); err != nil {
		return fmt.Errorf("replog: mid-run rotate: %w", err)
	}
	warnings := prune(w.dir)
	f, err := os.OpenFile(current, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		return fmt.Errorf("replog: reopen after mid-run rotation: %w", err)
	}
	w.file = f
	w.written = 0
	fmt.Fprintf(f, "replog: mid-run rotation: previous file reached %d bytes (cap %d)\n", reached, w.Cap)
	for _, warn := range warnings {
		fmt.Fprintf(f, "replog: %s\n", warn)
	}
	return nil
}

// Close closes the current log file. Safe to call at shutdown even if a
// mid-run rotation has since replaced the *os.File Open originally
// returned.
func (w *CappedWriter) Close() error {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.file.Close()
}

// Name returns the current log file's path. Reads the live *os.File
// under mu rather than caching a path once, since a mid-run rotation
// keeps the name the same (FileName is always reopened at the same
// path) but replaces the underlying file.
func (w *CappedWriter) Name() string {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.file.Name()
}

// prune deletes all but the newest KeepBackups rotated files. The stamp
// layout sorts lexicographically in time order, so name order is age
// order. Failures are reported, never fatal.
func prune(dir string) (warnings []string) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return []string{fmt.Sprintf("replog: list log dir for pruning: %v", err)}
	}
	var backups []string
	for _, e := range entries {
		if !e.IsDir() && strings.HasPrefix(e.Name(), FileName+".") {
			backups = append(backups, e.Name())
		}
	}
	if len(backups) <= KeepBackups {
		return nil
	}
	sort.Strings(backups)
	for _, name := range backups[:len(backups)-KeepBackups] {
		if err := os.Remove(filepath.Join(dir, name)); err != nil {
			warnings = append(warnings, fmt.Sprintf("replog: prune old log %s: %v", name, err))
		}
	}
	return warnings
}
