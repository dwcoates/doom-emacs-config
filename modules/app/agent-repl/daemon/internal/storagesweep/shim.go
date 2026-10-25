package storagesweep

import (
	"context"
	"os"
	"path/filepath"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/sessionlock"
	"agent-repld/internal/wsm"
)

// Reason is why an entry was deleted (agentrepl.v1.PruneStorageReason).
type Reason string

// KeepReason is why an entry was left alone
// (agentrepl.v1.PruneStorageKeepReason).
type KeepReason string

// The shim rule's reasons: why an entry was deleted or kept.
const (
	// ReasonNoWorkspace: no workspace the registry knows keys to the directory
	// (its workspace was forgotten), so nothing will read it again.
	ReasonNoWorkspace Reason = "no_workspace"
	// ReasonLanded: every workspace keyed to it closed by its own merge more
	// than LandedAfter ago; its worktree is gone, so the whole directory goes.
	ReasonLanded Reason = "landed"
	// ReasonClosedBackups: every workspace keyed to it closed more than ClosedAfter
	// ago, one of them by something other than its merge, so it may still be
	// reopened. Only its transcript backups go; its identity stays, so a
	// reopened workspace resumes as itself.
	ReasonClosedBackups Reason = "closed_backups"

	KeepOpenWorkspace  KeepReason = "open_workspace"
	KeepRecentlyClosed KeepReason = "recently_closed"
	KeepNotADirectory  KeepReason = "not_a_directory"
	KeepNoBackups      KeepReason = "no_backups"
)

// The shim's own layout (agent-shim/claude/shim/src/engine/backup.ts).
const backupsDir = "backups"

// scope is how much of a shim directory a verdict deletes.
type scope int

const (
	scopeBackups scope = iota
	scopeWhole
)

// verdict is what the rows keyed to one directory allow.
type verdict struct {
	// keep is set when any keyed workspace is open.
	keep bool
	// deadline is the latest instant any keyed workspace allows deletion at.
	deadline time.Time
	// scope is the least any keyed workspace allows deleting.
	scope scope
	// undated is set when a keyed workspace closed with no close record, so
	// its deadline counts from the directory's own last write.
	undated bool
}

// sweepShims applies the shim rule. The directory is listed BEFORE the
// registry is read: the shim makes a workspace's directory only after the
// registry holds the workspace's row, so a directory listed first always
// finds its row in the read that follows, and a workspace created mid-sweep
// is never mistaken for a forgotten one.
func (s *Sweeper) sweepShims(ctx context.Context, now time.Time, report *Report) error {
	entries, err := os.ReadDir(s.deps.ShimDir)
	if os.IsNotExist(err) {
		s.deps.Log.Debug(opShim, "there is no shim state directory to sweep", dlog.Context{"dir": s.deps.ShimDir})
		return nil
	}
	if err != nil {
		report.Failures++
		s.deps.Log.Error(opShim, "the shim state directory could not be listed; no shim state is swept", dlog.Context{
			"dir": s.deps.ShimDir, "cause": err.Error(),
		})
		return nil
	}
	workspaces, err := s.deps.Registry.ListWorkspaces(ctx)
	if err != nil {
		if ctx.Err() != nil {
			return ctx.Err()
		}
		report.Failures++
		s.deps.Log.Error(opShim, "the workspaces could not be read; no shim state is swept", dlog.Context{"cause": err.Error()})
		return err
	}
	verdicts, err := s.verdicts(workspaces)
	if err != nil {
		report.Failures++
		s.deps.Log.Error(opShim, "a workspace's shim key could not be derived; no shim state is swept", dlog.Context{"cause": err.Error()})
		return nil
	}
	for _, entry := range entries {
		if ctx.Err() != nil {
			return ctx.Err()
		}
		s.judgeShim(entry, verdicts, now, report)
	}
	return nil
}

// verdicts folds every workspace row into its shim key's verdict. A key
// shared by several rows (a directory reused by a later workspace) is kept
// while any of them is open, deleted no earlier than the latest any allows,
// and no more than the least any allows.
func (s *Sweeper) verdicts(workspaces []wsm.Workspace) (map[string]verdict, error) {
	out := map[string]verdict{}
	for _, ws := range workspaces {
		key, err := sessionlock.WorkspaceKey(ws.Dir)
		if err != nil {
			return nil, err
		}
		v, seen := out[key]
		if !seen {
			v = verdict{scope: scopeWhole}
		}
		switch {
		case !ws.Closed:
			v.keep = true
		case ws.Closure == nil:
			v.undated = true
			v.scope = scopeBackups
		case ws.Closure.By == wsm.CloseByMerge:
			v.deadline = later(v.deadline, ws.Closure.At.Add(s.deps.LandedAfter))
		default:
			v.deadline = later(v.deadline, ws.Closure.At.Add(s.deps.ClosedAfter))
			v.scope = scopeBackups
		}
		out[key] = v
	}
	return out, nil
}

// judgeShim deletes what one shim directory's verdict allows.
func (s *Sweeper) judgeShim(entry os.DirEntry, verdicts map[string]verdict, now time.Time, report *Report) {
	dir := filepath.Join(s.deps.ShimDir, entry.Name())
	if !entry.IsDir() {
		report.Kept[KeepNotADirectory]++
		return
	}
	v, known := verdicts[entry.Name()]
	switch {
	case !known:
		s.remove(opShim, dir, ReasonNoWorkspace, report)
		return
	case v.keep:
		report.Kept[KeepOpenWorkspace]++
		return
	}
	deadline := v.deadline
	if v.undated {
		last, err := lastWrite(dir)
		if err != nil {
			report.Failures++
			s.deps.Log.Error(opShim, "a shim directory's last write could not be read; it is kept", dlog.Context{"dir": dir, "cause": err.Error()})
			return
		}
		deadline = later(deadline, last.Add(s.deps.ClosedAfter))
	}
	if now.Before(deadline) {
		report.Kept[KeepRecentlyClosed]++
		return
	}
	if v.scope == scopeWhole {
		s.remove(opShim, dir, ReasonLanded, report)
		return
	}
	backups := filepath.Join(dir, backupsDir)
	if _, err := os.Lstat(backups); os.IsNotExist(err) {
		report.Kept[KeepNoBackups]++
		return
	}
	s.remove(opShim, backups, ReasonClosedBackups, report)
}

// later is the later of two instants.
func later(a, b time.Time) time.Time {
	if b.After(a) {
		return b
	}
	return a
}
