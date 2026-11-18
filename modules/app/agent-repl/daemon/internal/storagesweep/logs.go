package storagesweep

import (
	"context"
	"os"
	"regexp"
	"time"

	"agent-repld/internal/dlog"
)

// The logs rule's reasons.
const (
	// ReasonUntouchedLogs: a daemon-written log file of a workspace nobody
	// touched for LogsAfter, that no writer holds.
	ReasonUntouchedLogs Reason = "untouched_logs"

	// KeepRecentlyTouched: a workspace with log files that somebody touched
	// inside LogsAfter.
	KeepRecentlyTouched KeepReason = "recently_touched"
	// KeepHeldLogs: a log file of an untouched workspace that a writer
	// outside the daemon still holds.
	KeepHeldLogs KeepReason = "held_logs"
)

// Logs is the part of the log surfaces the sweep drives (dlog.Surfaces).
type Logs interface {
	DiscardWorkspace(dir, id string, judgeOnly bool) (dlog.DiscardReport, error)
}

// workspaceLog reads the minted workspace id out of a per-workspace log
// file's name (dlog's createTarget: `agent-repl-<id>-<runtime>-<suffix>.log`
// and its generations).
var workspaceLog = regexp.MustCompile(`^agent-repl-([0-9a-f]+)-[a-z]+-[A-Za-z0-9]+\.log(\.[0-9]+)?$`)

// sweepLogs applies the logs rule: every daemon-written log file of a
// workspace nobody touched for LogsAfter is removed, with the canonical links
// that named it.
//
// THE WRITERS' OWN RETENTION DOES NOT REACH THESE FILES. It removes a file
// LogsRetention after its LAST WRITE (agentrepl/logging retention.go), and the
// daemon goes on writing a few records an hour into the log of a workspace
// nobody uses, so such a file's last write is always recent and it is kept at
// whatever size it reached (2026-11-11: 1.2 GB across 77 workspaces untouched
// for five days). The workspace's own record says nobody was there
// (wsm.Workspace.LastTouched), which is what this rule reads.
//
// A WORKSPACE THE REGISTRY DOES NOT KNOW IS LEFT TO THE WRITERS' RETENTION:
// nothing writes a forgotten workspace's log, so its last write ages it.
//
// The directory is listed first and only the workspaces that have a file in it
// are asked about, so a sweep costs one listing and one discard per workspace
// that has anything to remove.
func (s *Sweeper) sweepLogs(ctx context.Context, now time.Time, report *Report) {
	entries, err := os.ReadDir(s.deps.LogsDir)
	if os.IsNotExist(err) {
		s.deps.Log.Debug(opLogs, "there is no logs directory to sweep", dlog.Context{"dir": s.deps.LogsDir})
		return
	}
	if err != nil {
		report.Failures++
		s.deps.Log.Error(opLogs, "the logs directory could not be listed; no logs are swept", dlog.Context{
			"dir": s.deps.LogsDir, "cause": err.Error(),
		})
		return
	}
	logged := map[string]bool{}
	for _, entry := range entries {
		if m := workspaceLog.FindStringSubmatch(entry.Name()); m != nil {
			logged[m[1]] = true
		}
	}
	if len(logged) == 0 {
		return
	}
	workspaces, err := s.deps.Registry.ListWorkspaces(ctx)
	if err != nil {
		if ctx.Err() != nil {
			return
		}
		report.Failures++
		s.deps.Log.Error(opLogs, "the workspaces could not be read; no logs are swept", dlog.Context{"cause": err.Error()})
		return
	}
	cutoff := now.Add(-s.deps.LogsAfter)
	for _, ws := range workspaces {
		if ctx.Err() != nil {
			return
		}
		id := string(ws.ID)
		if !logged[id] {
			continue
		}
		touched := ws.LastTouched()
		if !touched.At.Before(cutoff) {
			report.Kept[KeepRecentlyTouched]++
			continue
		}
		fields := dlog.Context{
			"workspace": id, "workspace_name": ws.Name, "workspace_dir": ws.Dir, "closed": ws.Closed,
			"last_touched": touched.At.UTC().Format(time.RFC3339), "last_touched_signal": touched.Signal,
			"logs_after": s.deps.LogsAfter.String(), "dry_run": report.DryRun,
		}
		discarded, err := s.deps.Logs.DiscardWorkspace(ws.Dir, id, report.DryRun)
		var bytes int64
		for _, removed := range discarded.Removed {
			report.Removed = append(report.Removed, Removal{Path: removed.Path, Reason: ReasonUntouchedLogs, Bytes: removed.Bytes})
			bytes += removed.Bytes
		}
		report.Kept[KeepHeldLogs] += len(discarded.Held)
		fields["files"], fields["bytes"], fields["links"], fields["held"] = len(discarded.Removed), bytes, len(discarded.Links), len(discarded.Held)
		if err != nil {
			report.Failures++
			fields["cause"] = err.Error()
			s.deps.Log.Error(opLogs, "an untouched workspace's logs could not be fully removed", fields)
			continue
		}
		if len(discarded.Removed) == 0 {
			continue
		}
		message := "deleted the logs of a workspace nobody touched inside the window"
		if report.DryRun {
			message = "would delete the logs of a workspace nobody touched inside the window"
		}
		s.deps.Log.Info(opLogs, message, fields)
	}
}
