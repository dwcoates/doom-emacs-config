package storagesweep

import (
	"os"
	"path/filepath"
	"time"

	"agent-repld/internal/dlog"
)

// The aged rule's reasons.
const (
	ReasonAged Reason = "aged"

	KeepYoung           KeepReason = "young"
	KeepKeptByDirectory KeepReason = "kept_by_directory"
)

// sweepAged deletes every entry of one aged directory older than its age.
func (s *Sweeper) sweepAged(a AgedDir, now time.Time, report *Report) {
	entries, err := os.ReadDir(a.Dir)
	if os.IsNotExist(err) {
		s.deps.Log.Debug(opAged, "an aged directory does not exist; nothing to sweep", dlog.Context{"name": a.Name, "dir": a.Dir})
		return
	}
	if err != nil {
		report.Failures++
		s.deps.Log.Error(opAged, "an aged directory could not be listed; it is not swept", dlog.Context{
			"name": a.Name, "dir": a.Dir, "cause": err.Error(),
		})
		return
	}
	keep := map[string]bool{}
	if a.Keep != nil {
		if keep, err = a.Keep(); err != nil {
			report.Failures++
			s.deps.Log.Error(opAged, "an aged directory's kept entries could not be read; it is not swept", dlog.Context{
				"name": a.Name, "dir": a.Dir, "cause": err.Error(),
			})
			return
		}
	}
	cutoff := now.Add(-a.After)
	for _, entry := range entries {
		path := filepath.Join(a.Dir, entry.Name())
		if keep[entry.Name()] {
			report.Kept[KeepKeptByDirectory]++
			continue
		}
		info, err := entry.Info()
		if err != nil {
			report.Failures++
			s.deps.Log.Error(opAged, "an aged entry could not be read; it is kept", dlog.Context{"name": a.Name, "path": path, "cause": err.Error()})
			continue
		}
		if !info.ModTime().Before(cutoff) {
			report.Kept[KeepYoung]++
			continue
		}
		s.remove(opAged, path, ReasonAged, report)
	}
}
