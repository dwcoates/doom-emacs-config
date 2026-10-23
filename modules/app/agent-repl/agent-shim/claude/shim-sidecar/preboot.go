package main

// preboot.go — A SHELL LAUNCHED BEFORE THIS BOOT, WHOSE SPOOL IS GONE, IS LOST.
//
// THE DEFECT THIS EXISTS FOR (2026-10-23). Three background shells were
// running when the host restarted. A shell's terminal is this reader's, read
// off its spool, and the boot sweep concludes LOST every run whose spool
// predates the boot -- but only runs it TRACKS, and it tracks only spools it
// finds. The spools lived under /tmp, which the restart emptied, so the
// launches replayed from the transcript named no file anyone could watch. No
// terminal was ever written; the shim, for which a shell is live until this
// reader writes its end, re-announced all three on every watch, and the merge
// of the workspace that ran them waited forever for them to end.
//
// A PROCESS CANNOT OUTLIVE THE KERNEL THAT RAN IT. A shell launched before the
// machine's boot instant is ended, whatever its record says, and a spool that
// does not exist will never say how. So such a run is concluded LOST with the
// boot sweep's own reason (swept_up), keyed on its spawning call. The store is
// asked first, as for every LOST conclusion (concludeLost): a run the record
// already holds as ended is left as it is, never restated.
//
// A MISSING NAMED PATH IS NOT PROOF THERE IS NO SPOOL. A spool is matched to
// its run by task id as well as by the path its launch named, so a run is held
// until a FULL SCAN that began after it was held has completed, and is then
// concluded only if discovery found no spool for its task id anywhere: watched,
// held unclaimed, or dormant. One that has a spool is the boot sweep's.
//
// TWO SOURCES FEED THE SAME HOLD. A launch this process reads (TaskSpawned)
// is one; the other is the RECORD, asked once per process after the first
// full scan (GetLiveShellRunsAnnouncedBefore): a reboot leaves every launch read
// before it past each transcript's committed cursor, so a run from before the
// boot is usually on no line this process will ever read, and the record is
// the only place that still names it. Held runs are keyed by their spawning
// call, the one identity both sources share.
//
// HELD, THEN CONCLUDED IN ONE BATCH PER SWEEP. A restarted reader replays every
// transcript's launches, thousands of them, nearly all long settled; asking the
// store one run at a time would be a store call per historical launch.

import (
	"errors"
	"io/fs"
	"os"
	"sort"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/handler"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/stale"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// preBootHold is one held run, and the count of completed full scans when it
// was held: only a scan completed after that can say no spool exists for it.
type preBootHold struct {
	work    stale.Work
	scansAt uint64
	// launch is the transcript that stated the launch: the run is concluded
	// only once that transcript has no unread bytes, since its later lines may
	// carry the run's own end (a notification, a stop).
	launch handler.LaunchSite
}

// preBootBatch bounds one settlement question, so a reader replaying a long
// history asks the store in pieces it can answer within its deadline.
const preBootBatch = 500

// holdPreBoot holds a detached shell's launch for concludePreBoot when the run
// started before the machine's boot and its spool can never be read.
func (s *sidecar) holdPreBoot(taskID, run, owner, output string, backgrounded bool, launch handler.LaunchSite) {
	launchedAtMs := launch.AtMs
	if backgrounded || launchedAtMs <= 0 || run == "" {
		return
	}
	if kind, _ := discover.TaskKind(taskID); kind != tail.KindShellSpool {
		return
	}
	boot := s.bootTimeMs()
	if boot <= 0 || launchedAtMs >= boot {
		return
	}
	bound := s.log.With(logging.Context{Operation: "preboot-run", TaskID: taskID, ActivityID: run, Path: output})
	if _, watched := s.spoolForTask(taskID); watched {
		return
	}
	if output != "" {
		_, err := os.Stat(output)
		if err == nil {
			// Its spool survived the boot: it is read, and the boot sweep
			// concludes it from the file.
			return
		}
		if !errors.Is(err, fs.ErrNotExist) {
			bound.With(logging.Context{Level: "warn"}).Log(
				"whether this pre-boot shell's spool exists could not be established, so it is not concluded LOST now: %v", err)
			return
		}
	}
	s.preBoot[run] = preBootHold{
		work:    stale.Work{TaskID: taskID, Kind: tail.KindShellSpool, OwnerAgentID: owner, RunActivityID: run, LastActivityMs: launchedAtMs},
		scansAt: s.fullScans,
		launch:  launch,
	}
	bound.LogVerbose("held: this shell was launched before the machine's boot and its named spool is gone; it is concluded LOST once a full scan finds no spool for it either")
}

// concludePreBoot concludes every held pre-boot shell a full scan has since
// found no spool for: a run the store already holds as ended is dropped, every
// other one gets its LOST terminal. A held run discovery did find a spool for
// is released to that spool's reader.
func (s *sidecar) concludePreBoot() {
	if len(s.preBoot) == 0 {
		return
	}
	spooled := s.spooledRuns()
	var runs []string
	for run, hold := range s.preBoot {
		if spooled[run] {
			delete(s.preBoot, run)
			s.log.With(logging.Context{Operation: "preboot-run", TaskID: hold.work.TaskID, ActivityID: run}).
				LogVerbose("released: discovery found this pre-boot shell's spool, so its reader concludes it")
			continue
		}
		if s.fullScans > hold.scansAt && (hold.launch.Transcript == "" || !s.unseenBytes(hold.launch.Transcript)) {
			runs = append(runs, run)
		}
	}
	sort.Strings(runs)
	for start := 0; start < len(runs); start += preBootBatch {
		if !s.concludePreBootBatch(runs[start:min(start+preBootBatch, len(runs))]) {
			return
		}
	}
}

// spooledRuns is every run discovery holds a spool for, watched, held
// unclaimed, or dormant, by its spawning call where the launch that names the
// spool's task is known.
func (s *sidecar) spooledRuns() map[string]bool {
	out := map[string]bool{}
	add := func(task string) {
		if task == "" {
			return
		}
		if run := s.owners.activityFor(task); run != "" {
			out[run] = true
		}
	}
	for _, w := range s.watchers {
		add(w.target.TaskID)
	}
	for task := range s.unclaimedShells {
		add(task)
	}
	for _, target := range s.dormant {
		add(target.TaskID)
	}
	return out
}

// concludePreBootBatch concludes one batch of held runs, answering false when
// the store could not answer, which leaves the rest held for the next sweep.
func (s *sidecar) concludePreBootBatch(runs []string) bool {
	ended, err := s.askSettlements(runs)
	if err != nil {
		if !s.interrupted(err) {
			s.log.With(logging.Context{Operation: "preboot-run", Level: "error", Repeat: logging.Repeat(len(runs))}).Log(
				"%d pre-boot shell(s) stay held: the store could not say whether their runs already settled, and a LOST over a settled run would supersede its real terminal; the next sweep asks again: %v",
				len(runs), err)
		}
		s.noteStoreErr("run-settlements", err)
		return false
	}
	var entries []*storev1.StoreEntry
	var minted []string
	settled := 0
	for _, run := range runs {
		hold := s.preBoot[run]
		task := hold.work.TaskID
		_, wasEnded := ended[run]
		ends := wasEnded
		if task != "" {
			_, concluded := s.concluded[task]
			_, notified := s.notified[task]
			_, stopped := s.stopped[task]
			ends = ends || concluded || notified || stopped
		}
		if ends {
			// Its end is the record's, or another fact's that writes it.
			delete(s.preBoot, run)
			settled++
			continue
		}
		lost := handler.NewRunOutput(s.launchLog(hold.launch)).Lost(task, run, hold.work.OwnerAgentID, string(stale.ReasonSweptUp), true)
		if len(lost) == 0 {
			// seamTerminal stated why it refused; the run stays held.
			continue
		}
		entries = append(entries, lost...)
		minted = append(minted, run)
	}
	if len(entries) > 0 {
		skips, err := s.storeWrite("pre-boot LOST terminals", &storev1.EntryBatch{Entries: entries})
		if err != nil {
			if !s.interrupted(err) {
				s.log.With(logging.Context{Operation: "preboot-run", Level: "error", Repeat: logging.Repeat(len(minted))}).Log(
					"%d pre-boot shell LOST terminal(s) were not committed; they stay held for the next sweep: %v", len(minted), err)
			}
			return false
		}
		s.warnUnexpectedSkips("pre-boot LOST terminals", skips)
		for _, run := range minted {
			delete(s.preBoot, run)
		}
	}
	s.log.With(logging.Context{Operation: "preboot-run"}).Log(
		"pre-boot shells concluded: %d LOST terminal(s) written, %d already ended on record", len(minted), settled)
	return true
}

// askRecordForPreBoot asks the record, once per process and only after the
// first full scan, for every live shell announced before the machine's boot,
// and holds each one not already held. Held at the current scan count, each
// waits one more full scan, so a spool discovery finds is its reader's first.
// A store that cannot answer is asked again at the next sweep.
func (s *sidecar) askRecordForPreBoot() {
	if s.preBootAsked || s.fullScans == 0 {
		return
	}
	boot := s.bootTimeMs()
	if boot <= 0 {
		// BootSweep already said the boot time is unknown, loudly.
		s.preBootAsked = true
		return
	}
	ctx, cancel := s.rpcContext()
	defer cancel()
	runs, err := s.store.LiveShellRunsAnnouncedBefore(ctx, boot)
	if err != nil {
		if !s.interrupted(err) {
			s.log.With(logging.Context{Operation: "preboot-run", Level: "error"}).Log(
				"the record could not name the live shells from before the boot (%d); they stay open until the next sweep asks again: %v", boot, err)
		}
		s.noteStoreErr("live-shell-runs", err)
		return
	}
	s.preBootAsked = true
	held := 0
	for _, r := range runs {
		run := r.GetRun().GetValue()
		if run == "" {
			// The store answers no run without its origin unit; one here broke
			// that, and nothing could key its terminal.
			s.log.With(logging.Context{Operation: "preboot-run", Level: "error", TaskID: r.GetDetached().GetWork().GetValue()}).
				Log("the record named a live pre-boot shell with no spawning call; no terminal can be keyed to it")
			continue
		}
		if _, already := s.preBoot[run]; already {
			continue
		}
		s.preBoot[run] = preBootHold{
			work: stale.Work{Kind: tail.KindShellSpool, OwnerAgentID: r.GetDetached().GetOwner().GetValue(),
				RunActivityID: run, LastActivityMs: r.GetAnnouncedAtMs()},
			scansAt: s.fullScans,
		}
		held++
	}
	s.log.With(logging.Context{Operation: "preboot-run"}).Log(
		"the record names %d live shell(s) announced before the boot; %d newly held to be concluded LOST once a full scan finds no spool for them", len(runs), held)
}

// launchLog is the log the run's LOST terminal is stated on: the transcript the
// launch was read from, and how far it had been read when the run was
// concluded, so the row it writes traces back to a file and a position.
func (s *sidecar) launchLog(launch handler.LaunchSite) *logging.Bound {
	ctx := logging.Context{Path: launch.Transcript, FileID: launch.FileID}
	if w, ok := s.watchers[launch.Transcript]; ok && w.tailer != nil {
		if seen, sized := w.tailer.LastSize(); sized {
			ctx.Offset = &seen
		}
	}
	return s.log.With(ctx)
}

// releasePreBoot drops a held run the moment discovery watches a spool for it:
// the run is that spool's reader's from then on.
func (s *sidecar) releasePreBoot(taskID, run string) {
	if _, held := s.preBoot[run]; !held {
		return
	}
	delete(s.preBoot, run)
	s.log.With(logging.Context{Operation: "preboot-run", TaskID: taskID, ActivityID: run}).
		LogVerbose("released: a spool for this pre-boot shell is now watched, so its reader concludes it")
}
