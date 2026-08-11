package server

import (
	"fmt"

	"claude-repld/internal/inflight"
)

// inflightmanifest.go — THE BOUNCE'S BEFORE AND AFTER, PER WORKSPACE.
//
// bounceledger writes down which shim PID each session is handed over with and
// judges it at the next boot. This is the same discipline over WORK: which
// turns, tasks and queries were in flight when the bounce started, and what
// became of each one.
//
// The two halves are written by two different daemon processes — the outgoing
// one writes START, the incoming one writes END — which is exactly why the sink
// is an append-only plain file in the workspace rather than the daemon log,
// whose canonical symlink is re-pointed by the very restart being described.

// InFlightManifestSource is what the manifest needs from the fleet: the
// authoritative in-flight set for a workspace. Satisfied by
// *sessioncontroller.Manager.
type InFlightManifestSource interface {
	InFlight(workspace string) inflight.Set
}

// InFlightTerminalSource answers whether one work item's terminal result was
// actually recorded. Satisfied by *ssm.Manager (WorkItemTerminated).
//
// Its ERROR is load-bearing: a plane this daemon records no terminal for must
// return one, so the judgement is UNKNOWN rather than a fabricated INTERRUPTED.
type InFlightTerminalSource interface {
	WorkItemTerminated(workspace, kind, id string) (bool, string, error)
}

// RecordBounceStart writes each workspace's pre-bounce manifest.
//
// A write failure is REPORTED, never swallowed, and it does not stop the
// bounce: refusing to shut down because a manifest could not be written would
// trade an accounting gap for an unstoppable daemon. What it must never do is
// pass silently — a bounce with no manifest reads as a bounce that interrupted
// nothing, and that is the exact false clean bill this mechanism exists to
// deny.
func RecordBounceStart(logf func(string, ...any), workspaces map[string]string, source InFlightManifestSource, bounceID, cause string) {
	if source == nil {
		logf("server: bounce in-flight manifest SKIPPED bounce_id=%s cause=%q — no in-flight source is wired, so this bounce makes NO claim about what work it interrupted",
			bounceID, cause)
		return
	}
	for sessionID, workspace := range workspaces {
		set := source.InFlight(workspace)
		rec := inflight.Record{
			Phase:     inflight.PhaseStart,
			BounceID:  bounceID,
			Workspace: workspace,
			SessionID: sessionID,
			Cause:     cause,
			Known:     set.Known(),
			Unknown:   set.Reason(),
			Items:     set.Items(),
		}
		if err := inflight.Append(workspace, rec); err != nil {
			logf("server: bounce in-flight manifest WRITE FAILED bounce_id=%s session=%s ws=%q: %v — this workspace's pre-bounce work is unrecorded, so nothing can be judged about it afterwards",
				bounceID, sessionID, workspace, err)
			continue
		}
		logf("server: bounce in-flight manifest START bounce_id=%s session=%s ws=%q cause=%q — %s",
			bounceID, sessionID, workspace, cause, set.Summary())
	}
}

// ReportBounceEnd closes out a workspace's manifest: it reads the newest START
// record, reconciles it against what is in flight NOW, and appends the END
// record with a per-item disposition.
//
// It returns the tally so a caller can act on a non-zero INTERRUPTED count.
func ReportBounceEnd(logf func(string, ...any), workspace string, source InFlightManifestSource, terminals InFlightTerminalSource) (map[string]int, error) {
	if source == nil || terminals == nil {
		return nil, fmt.Errorf("server: closing a bounce manifest for %q needs both an in-flight source and a terminal source; without the second a vanished item cannot be told from a finished one", workspace)
	}
	records, err := inflight.Read(workspace)
	if err != nil {
		return nil, fmt.Errorf("server: reading the bounce manifest for %q: %w", workspace, err)
	}
	start, found := newestStart(records)
	if !found {
		// NOT AN ERROR AND NOT A CLEAN BILL. A workspace whose predecessor never
		// wrote a START has nothing to be judged against, and saying so is the
		// whole point of distinguishing it from a judged-and-empty bounce.
		logf("server: bounce in-flight manifest END SKIPPED ws=%q — no predecessor START record, so this boot makes no claim about what the last bounce did to this workspace's work", workspace)
		return nil, nil
	}
	before := rebuildSet(workspace, start)
	after := source.InFlight(workspace)
	judgements, reconcileErr := inflight.Reconcile(before, after, func(item inflight.Item) (bool, string, error) {
		return terminals.WorkItemTerminated(workspace, string(item.Kind), item.ID)
	})
	if reconcileErr != nil {
		logf("server: bounce in-flight manifest END UNJUDGEABLE ws=%q bounce_id=%s: %v",
			workspace, start.BounceID, reconcileErr)
		return nil, reconcileErr
	}
	tally := inflight.Tally(judgements)
	rec := inflight.Record{
		Phase:      inflight.PhaseEnd,
		BounceID:   start.BounceID,
		Workspace:  workspace,
		SessionID:  start.SessionID,
		Cause:      start.Cause,
		Known:      after.Known(),
		Unknown:    after.Reason(),
		Items:      after.Items(),
		Judgements: judgements,
		Tally:      tally,
	}
	if err := inflight.Append(workspace, rec); err != nil {
		return tally, fmt.Errorf("server: writing the bounce manifest END for %q: %w", workspace, err)
	}
	for _, j := range judgements {
		logf("server: bounce work accounting ws=%q bounce_id=%s item=%s disposition=%s — %s",
			workspace, start.BounceID, j.Item, j.Disposition, j.Reason)
	}
	logf("server: bounce work accounting SUMMARY ws=%q bounce_id=%s items=%d preserved=%d completed=%d interrupted=%d unknown=%d — a bounce is scored by what happened to each named work item, never by a count of processes",
		workspace, start.BounceID, len(judgements),
		tally[inflight.DispositionPreserved], tally[inflight.DispositionCompleted],
		tally[inflight.DispositionInterrupted], tally[inflight.DispositionUnknown])
	if tally[inflight.DispositionInterrupted] > 0 {
		logf("server: bounce work accounting WORK LOST ws=%q bounce_id=%s interrupted=%d of %d — work that was live before this bounce is gone and never recorded a terminal result; NOBODY DECIDED THAT",
			workspace, start.BounceID, tally[inflight.DispositionInterrupted], len(judgements))
	}
	return tally, nil
}

// newestStart returns the last START record with no END for the same bounce id.
// Scanning backwards is what makes the file's append-only history usable: a
// workspace accumulates many bounces and only the newest unclosed one is open.
func newestStart(records []inflight.Record) (inflight.Record, bool) {
	closed := map[string]bool{}
	for _, rec := range records {
		if rec.Phase == inflight.PhaseEnd {
			closed[rec.BounceID] = true
		}
	}
	for i := len(records) - 1; i >= 0; i-- {
		if records[i].Phase == inflight.PhaseStart && !closed[records[i].BounceID] {
			return records[i], true
		}
	}
	return inflight.Record{}, false
}

// rebuildSet reconstitutes the pre-bounce set from its persisted record.
//
// A record whose `known` is false rebuilds as UNANSWERED, carrying its own
// stated reason — so a bounce that could not observe its own pre-state cannot
// be laundered into a judgeable empty one by a round trip through the file.
func rebuildSet(workspace string, rec inflight.Record) inflight.Set {
	if !rec.Known {
		return inflight.Unanswered(workspace, rec.Unknown)
	}
	set, err := inflight.Answered(workspace, rec.Items...)
	if err != nil {
		return inflight.Unanswered(workspace, fmt.Sprintf("the persisted pre-bounce record could not be rebuilt: %v", err))
	}
	return set
}
