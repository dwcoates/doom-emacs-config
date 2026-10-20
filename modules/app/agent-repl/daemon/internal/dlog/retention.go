package dlog

import (
	"errors"
	"regexp"
	"time"

	"agentrepl/logging"
)

// The daemon's half of the retention rule (agentrepl/logging retention.go):
// every log file the daemon writes is removed once its last write is
// logging.DefaultRetention old.
//
// ONE PASS, HOURLY, AGES EVERYTHING THE DAEMON WRITES under the state root's
// logs directory:
//
//   - every sink the daemon holds open, by its own sink (sink.expire);
//   - the run log and the stdio log, by their own RotatingFiles;
//   - every daemon-written family nobody holds -- a closed workspace's
//     targets, an older instance's generations -- by logging.PruneDirectory.
//
// Emacs's own files in the same directory (`emacs.central.log`, the
// `agent-repl-<id>-emacs-*.log` targets) are NEVER touched here: Emacs opens
// them per write and holds no lock, so the shared lock the prune relies on
// cannot speak for them, and Emacs ages them itself (lisp/core.el).
const (
	// retentionFirst is how long after boot the first pass runs: past the
	// boot's own sink opens, and soon enough that a daemon restarted often
	// still ages its directory.
	retentionFirst = time.Minute
	// retentionEvery is the cadence after the first pass.
	retentionEvery = logging.DefaultExpireInterval
)

// daemonLogFamily matches the base of every log family the daemon writes under
// the state root's logs directory: the run log, the stdio log, and the
// per-workspace targets createTarget mints (an older instance's 8-hex
// directory-hash names included).
var daemonLogFamily = regexp.MustCompile(`^(daemon\.run\.log|daemon\.stdio\.log|agent-repl-[0-9a-f]+-(daemon|shim|webapp|sidecar)-[A-Za-z0-9]+\.log)$`)

// isDaemonLogFamily answers whether base names a daemon-written log family.
func isDaemonLogFamily(base string) bool { return daemonLogFamily.MatchString(base) }

// retentionPass is what one pass did, summed for its one record.
type retentionPass struct {
	sinksRolled   int
	shimsMarked   int
	removed       int
	held          int
	kept          int
	families      int
	failures      int
	runLogRolled  bool
	stdioRolled   bool
	stdioAdopted  bool
	removedSample []string
}

// removedSampleSize bounds how many removed paths the pass record names.
const removedSampleSize = 10

func (p *retentionPass) noteRemoved(paths []string) {
	p.removed += len(paths)
	for _, path := range paths {
		if len(p.removedSample) < removedSampleSize {
			p.removedSample = append(p.removedSample, path)
		}
	}
}

// retainOnce runs one retention pass at now and records it.
func (s *surfaces) retainOnce(now time.Time) {
	var pass retentionPass
	s.retainSinks(now, &pass)

	if result, err := s.runLog.file.Expire(now); err != nil {
		pass.failures++
		s.Global().Error("daemon.dlog.retention_failed", "the run log could not be aged", Context{
			"log": s.runLog.path, "cause": err.Error(),
		})
	} else {
		pass.runLogRolled = result.RolledCurrent
		pass.noteRemoved(result.Removed)
	}

	s.mu.Lock()
	stdio := s.stdio
	s.mu.Unlock()
	if stdio != nil {
		pass.stdioAdopted = true
		retainStdio(s, stdio, now, &pass)
	}

	report, err := logging.PruneDirectory(s.logsDir, now, logging.DefaultRetention, isDaemonLogFamily)
	pass.families = report.Families
	pass.held = len(report.Held)
	pass.kept = report.Kept
	pass.noteRemoved(report.Removed)
	if err != nil {
		pass.failures++
		s.Global().Error("daemon.dlog.retention_failed", "the logs directory could not be fully pruned", Context{
			"logs_dir": s.logsDir, "removed": len(report.Removed), "cause": err.Error(),
		})
	}

	s.Global().Info("daemon.dlog.retention_pass", "aged every log the daemon writes", Context{
		"logs_dir":       s.logsDir,
		"retention":      logging.DefaultRetention.String(),
		"sinks_rolled":   pass.sinksRolled,
		"shims_marked":   pass.shimsMarked,
		"run_log_rolled": pass.runLogRolled,
		"stdio_adopted":  pass.stdioAdopted,
		"stdio_rolled":   pass.stdioRolled,
		"families":       pass.families,
		"held":           pass.held,
		"kept":           pass.kept,
		"removed":        pass.removed,
		"removed_sample": pass.removedSample,
		"failures":       pass.failures,
	})
}

// retainSinks ages every sink the daemon holds open.
func (s *surfaces) retainSinks(now time.Time, pass *retentionPass) {
	for _, e := range s.openSinks() {
		out, err := e.sk.expire(now)
		pass.noteRemoved(out.removed)
		if out.rolled {
			pass.sinksRolled++
		}
		if out.marked {
			pass.shimsMarked++
		}
		if err == nil {
			continue
		}
		pass.failures++
		if errors.Is(err, ErrPoisoned) {
			s.reportSinkFailure(e.ws, e.name, err)
			continue
		}
		s.WorkspaceOrCentral(e.ws.dir).Error("daemon.dlog.retention_failed", "a workspace log sink could not be aged", Context{
			"sink": e.name + ".log", "target": e.sk.target, "cause": err.Error(),
		})
	}
}

// retainStdio ages the stdio log the daemon took over at boot.
func retainStdio(s *surfaces, stdio *stdioLog, now time.Time, pass *retentionPass) {
	result, err := stdio.maintain(now)
	pass.stdioRolled = result.RolledCurrent
	pass.noteRemoved(result.Removed)
	if err != nil {
		pass.failures++
		s.Global().Error("daemon.dlog.retention_failed", "the stdio log could not be aged", Context{
			"log": stdio.path, "cause": err.Error(),
		})
	}
}
