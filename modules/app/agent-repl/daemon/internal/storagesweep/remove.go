package storagesweep

import (
	"io/fs"
	"os"
	"path/filepath"
	"time"

	"agent-repld/internal/dlog"
)

// remove deletes path (a directory with everything under it, or a file) and
// records it, or under a dry run records what would go. A failure is recorded
// at ERROR and counted; whatever it could not delete stays.
func (s *Sweeper) remove(op, path string, reason Reason, report *Report) {
	bytes, err := sizeOf(path)
	if err != nil {
		report.Failures++
		s.deps.Log.Error(op, "an entry's size could not be read; it is kept", dlog.Context{"path": path, "reason": reason, "cause": err.Error()})
		return
	}
	if !report.DryRun {
		if err := os.RemoveAll(path); err != nil {
			report.Failures++
			s.deps.Log.Error(op, "an entry could not be deleted", dlog.Context{"path": path, "reason": reason, "cause": err.Error()})
			return
		}
	}
	report.Removed = append(report.Removed, Removal{Path: path, Reason: reason, Bytes: bytes})
	message := "deleted stored data nothing will read again"
	if report.DryRun {
		message = "would delete stored data nothing will read again"
	}
	s.deps.Log.Info(op, message, dlog.Context{"path": path, "reason": reason, "bytes": bytes, "dry_run": report.DryRun})
}

// sizeOf is what path holds on disk: every regular file under it.
func sizeOf(path string) (int64, error) {
	var total int64
	err := filepath.WalkDir(path, func(_ string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.Type().IsRegular() {
			info, err := d.Info()
			if err != nil {
				return err
			}
			total += info.Size()
		}
		return nil
	})
	return total, err
}

// lastWrite is the newest modification time of dir and of every entry under
// it.
func lastWrite(dir string) (time.Time, error) {
	var newest time.Time
	err := filepath.WalkDir(dir, func(_ string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		info, err := d.Info()
		if err != nil {
			return err
		}
		newest = later(newest, info.ModTime())
		return nil
	})
	return newest, err
}
