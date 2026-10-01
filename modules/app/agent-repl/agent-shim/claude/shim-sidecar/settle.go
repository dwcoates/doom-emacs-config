package main

// settle.go — A SETTLED RUN IS NEVER TRACKED AGAIN, AND NEVER CONCLUDED LOST,
// BY THIS PROCESS OR ANY LATER ONE.
//
// THE DEFECT THIS EXISTS FOR (2026-09-30). A background shell's spool carried
// `EXIT=0`; the sidecar read it, wrote the run's terminal, and untracked it.
// The sidecar was then restarted. The new process resumed the spool at its
// committed cursor — PAST the marker it had already converted — so nothing it
// read said the run had ended, and it tracked the quiet spool afresh. Thirty
// minutes later it concluded the finished run LOST, and that LOST terminal
// superseded the real one in the store. Every way a run settles (its own
// terminator, a transcript's notification, a person's stop, an earlier LOST)
// lived only in the old process's memory.
//
// THE SETTLE IS DURABLE IN EXACTLY ONE PLACE, the run's `detached_work` row, so
// that is what decides. A watched detached run is not observed by the LOST
// policy until the store (GetRunSettlements) has said it has not ended; a run
// the record holds as ended is settled instead. A sweep's conclusions are put
// to the same question before any LOST terminal is minted, so a run another
// plane ended while it was tracked is settled rather than restated. Inside one
// process every settle goes through settleRun into the tracker's settled set,
// and observing a settled run panics.
//
// NO TIMER AND NO RETRY LOOP OF ITS OWN. A store that cannot answer leaves the
// run untracked in trackPending, and the next ordinary pass of watchTargets
// asks again.

import (
	"os"
	"sort"
	"time"

	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/stale"
)

// trackDetached queues the LOST policy's clock for a file that IS a detached
// run. A transcript is not one: it is an agent's own record, and its silence is
// not a conclusion about anything. The clock starts once the store has said the
// run has not already settled (resolvePendingTracking).
func (s *sidecar) trackDetached(target discover.Target, now time.Time) {
	if target.TaskID == "" || target.SessionID != "" {
		return
	}
	if s.tracker.Settled(target.Path) {
		// The run settled earlier in this process and its watcher was rebuilt
		// (a vanished file that came back, say). It is never tracked again.
		s.log.With(logging.Context{Operation: "lost-policy", Path: target.Path, TaskID: target.TaskID}).
			LogVerbose("this run already settled in this process; its rebuilt watcher does not track it again")
		return
	}
	work := stale.Work{
		Path:          target.Path,
		TaskID:        target.TaskID,
		Kind:          target.Kind,
		OwnerAgentID:  s.owners.agentFor(target.TaskID),
		RunActivityID: s.owners.activityFor(target.TaskID),
	}
	if work.RunActivityID == "" {
		// A DETACHED FILE IS WATCHED ONLY ONCE ITS SPAWNING CALL CLAIMED IT
		// (resolveTarget), and that call IS the run the store is asked about. A
		// watched detached file with no run broke that invariant, and tracking
		// it blind is how a settled run gets concluded LOST.
		panic("sidecar: a watched detached run names no spawning call, so whether it settled cannot be asked: " + target.Path)
	}
	if info, err := os.Stat(target.Path); err == nil {
		work.LastActivityMs = info.ModTime().UnixMilli()
	}
	s.trackPending[target.Path] = work
}

// tracking reports whether a run is owed a conclusion: tracked by the LOST
// policy, or waiting for the store to say whether it already settled.
func (s *sidecar) tracking(path string) bool {
	if _, pending := s.trackPending[path]; pending {
		return true
	}
	return s.tracker.Open(path)
}

// settleRun is THE ONE WAY a run settles in this process: its own terminal
// made durable, a transcript's conclusion, a person's stop, or a LOST terminal
// made durable. The run leaves both the pending queue and the tracker and joins
// the tracker's settled set, so it is never tracked again. It answers whether
// the run was being tracked or awaiting its settlement.
func (s *sidecar) settleRun(path string) bool {
	_, pending := s.trackPending[path]
	delete(s.trackPending, path)
	open := s.tracker.Settle(path)
	return open || pending
}

// askSettlements asks the store which of runs the record holds as ended,
// answering each one's end instant. A run absent from the answer has not
// settled. An error is returned as is; storeclient owns its causal record.
func (s *sidecar) askSettlements(runs []string) (map[string]int64, error) {
	ctx, cancel := s.rpcContext()
	defer cancel()
	settled, err := s.store.RunSettlements(ctx, runs)
	if err != nil {
		return nil, err
	}
	endedAt := make(map[string]int64, len(settled))
	for _, run := range settled {
		endedAt[run.GetRunId()] = run.GetEndedAtMs()
	}
	return endedAt, nil
}

// resolvePendingTracking asks the store, in one read, whether each run awaiting
// its settlement already ended, and either settles it (SettledByRecord) or
// starts its LOST clock (Observe).
//
// A STORE THAT CANNOT ANSWER LEAVES EVERY ONE OF THEM UNTRACKED, stated once at
// ERROR, and the next ordinary pass asks again. Guessing "not settled" is the
// defect this file exists to remove, and guessing "settled" would leave a
// live run unconcludable.
func (s *sidecar) resolvePendingTracking(now time.Time) {
	if len(s.trackPending) == 0 {
		return
	}
	paths := make([]string, 0, len(s.trackPending))
	unique := map[string]bool{}
	var runs []string
	for path, work := range s.trackPending {
		paths = append(paths, path)
		if !unique[work.RunActivityID] {
			unique[work.RunActivityID] = true
			runs = append(runs, work.RunActivityID)
		}
	}
	sort.Strings(paths)
	sort.Strings(runs)
	endedAt, err := s.askSettlements(runs)
	if err != nil {
		if s.interrupted(err) {
			s.log.With(logging.Context{Operation: "run-settlements", Repeat: logging.Repeat(len(paths))}).
				LogVerbose("shutdown interrupted the settlement read; %d detached run(s) stay untracked and the next boot asks again", len(paths))
			return
		}
		s.log.With(logging.Context{Operation: "run-settlements", Level: "error", Repeat: logging.Repeat(len(paths))}).Log(
			"%d detached run(s) are not tracked: the store could not say whether they already settled, and tracking a settled run would conclude a finished run LOST; the next cycle asks again: %v",
			len(paths), err)
		return
	}
	for _, path := range paths {
		work := s.trackPending[path]
		delete(s.trackPending, path)
		if ms, settled := endedAt[work.RunActivityID]; settled {
			s.tracker.SettledByRecord(work, ms)
			continue
		}
		s.tracker.Observe(work, now.UnixMilli())
	}
}

// concludeLost turns a sweep's conclusions into the runs' LOST terminals, but
// only for runs the record does not already hold as ended, and settles each
// run once its terminal is durable.
//
// THE RECORD IS ASKED BEFORE ANYTHING IS MINTED. A run tracked here can still
// be ended by another plane (the shim's terminal, a notification it wrote)
// while its file goes quiet, and a LOST over it would supersede that ending —
// the store refuses one as an invariant violation. Such a run is settled, not
// concluded. A store that cannot answer mints nothing: production is suspended
// like any store failure, and the next cycle re-derives the conclusions.
//
// A CONCLUSION SETTLES ONLY ONCE ITS TERMINAL IS DURABLE. A write that failed
// leaves the run unsettled, so the next cycle restates it.
func (s *sidecar) concludeLost(what string, conclusions []stale.Lost) {
	if len(conclusions) == 0 {
		return
	}
	unique := map[string]bool{}
	var runs []string
	for _, lost := range conclusions {
		if lost.RunActivityID != "" && !unique[lost.RunActivityID] {
			unique[lost.RunActivityID] = true
			runs = append(runs, lost.RunActivityID)
		}
	}
	sort.Strings(runs)
	endedAt := map[string]int64{}
	if len(runs) > 0 {
		answered, err := s.askSettlements(runs)
		if err != nil {
			if !s.interrupted(err) {
				s.log.With(logging.Context{Operation: "lost-terminal", Level: "error", Repeat: logging.Repeat(len(conclusions))}).Log(
					"%d %s conclusion(s) minted no LOST terminal: the store could not say whether their runs already settled, and a LOST over a settled run would supersede its real terminal; production is suspended and the next cycle re-derives them: %v",
					len(conclusions), what, err)
			}
			s.noteStoreErr("run-settlements", err)
			return
		}
		endedAt = answered
	}
	var minting []stale.Lost
	for _, lost := range conclusions {
		ms, settled := endedAt[lost.RunActivityID]
		if !settled {
			minting = append(minting, lost)
			continue
		}
		s.settleRun(lost.Path)
		s.log.With(logging.Context{
			Operation: "lost-terminal-settled", Path: lost.Path, TaskID: lost.TaskID,
			AgentID: lost.OwnerAgentID, ActivityID: lost.RunActivityID, Reason: string(lost.Reason),
		}).Log("no LOST terminal for this run: the record already holds it as ended (ended_at_ms=%d), so it settled rather than went unseen (reason=%s)", ms, lost.Reason)
	}
	if !s.emit(what, s.lostEntries(minting)) {
		return
	}
	for _, lost := range minting {
		s.settleRun(lost.Path)
	}
}
