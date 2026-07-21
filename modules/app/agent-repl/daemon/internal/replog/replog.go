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
