// seam.go is the whole boundary between the READER (this package, internal/tail,
// internal/discover, internal/storeclient) and the CONVERTER (internal/convert,
// internal/handler).
//
// The reader decides WHICH FILES exist, WHERE it has read to, WHO owns a spool
// and WHEN it stopped seeing a run. The converter decides WHAT a record means.
// Everything that crosses between them crosses here, through tail.Handler and
// the two optional interfaces below.
//
// THE OPTIONAL INTERFACES ARE ADOPTED BY ADDING A METHOD, and they take plain
// function and string arguments rather than named types, so neither package has
// to import the other. A handler that has not adopted one is not a silent
// degradation: the reader states exactly what it could not hand over, at error
// level, every time it had something to hand.
package main

import (
	"fmt"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/handler"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/stale"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// taskObserverSink is implemented by a handler whose converter accepts the
// reader's spawn observations (see Observer in owner.go). The converter reads a
// spawning call's tool result and reports the task it opened; the reader is what
// turns that into a spool's owner.
type taskObserverSink interface {
	SetTaskObserver(func(taskID, toolUseID, agentID, outputPath string, backgrounded bool, workspaceDir, workspaceID, claudeSessionID string))
}

// lostTerminalSink is implemented by a handler that can mint a detached run's
// terminal from the reader's LOST conclusion. The reader states HOW it stopped
// seeing the run; spelling that as the run's terminal frame is conversion.
// `catchup` says the conclusion is startup backlog the policy already rolled
// into one summary, so the terminal's own record follows the same
// classification instead of restating it as a fresh warning (convert.lostCtx).
type lostTerminalSink interface {
	LostTerminal(taskID, runActivityID, ownerAgentID, reason string, catchup bool) []*storev1.StoreEntry
}

// cancelTerminalSink is implemented by a handler that can spell a person's stop
// as the run's terminal. Only the spool's reader can: the terminal owes the
// output the run produced, and those bytes exist nowhere else.
type cancelTerminalSink interface {
	CancelTerminal(taskID, run, ownerAgentID string, settledAtMs int64) []*storev1.StoreEntry
}

// taskStopSink is implemented by a handler whose converter reports the TaskStop
// results it reads. The transcript states the fact; the reader routes it to the
// spool that owns the run.
type taskStopSink interface {
	SetTaskStopObserver(func(taskID string))
}

// taskConclusionSink is implemented by a handler whose converter reports the
// backgrounded agent runs it SETTLED itself (a task notification, an agent
// TaskStop). The reader turns that into "this run can never be concluded LOST".
type taskConclusionSink interface {
	SetTaskConclusionObserver(func(taskID string))
}

// shellConclusionSink is implemented by a handler whose converter reports the
// vendor's task notification for a DETACHED SHELL run. The transcript states
// the fact; the reader writes the run's one terminal from the run's spool.
type shellConclusionSink interface {
	SetShellConclusionObserver(func(taskID, status string, atMs int64))
}

// notifiedTerminalSink is implemented by a handler that can spell the vendor's
// task notification as a shell run's terminal. Only the spool's reader can: the
// terminal owes the output the run produced, and those bytes exist nowhere else.
type notifiedTerminalSink interface {
	NotifiedTerminal(taskID, run, ownerAgentID, status string, settledAtMs int64) []*storev1.StoreEntry
}

// terminalReadSink is implemented by a handler that can tell the reader it READ
// a detached run's own terminal off the file (a spool's EXIT marker). The reader
// is what turns that into "this run can no longer be concluded LOST".
type terminalReadSink interface {
	SetTerminalObserver(func(path, run string))
}

// workflowSpoolKind is the vendor_specific kind a w* spool's bytes land under
// (R-S4). It is a DECLARED disposition, not a classification failure, so the
// day workflow ingestion lands every one of these rows is findable by it.
const workflowSpoolKind = "spool/workflow"

// newHandler builds the converter for one file kind and hands it the reader's
// observations.
func (s *sidecar) newHandler(kind tail.Kind, log *logging.Bound) tail.Handler {
	handlerLog := log.With(logging.Context{Component: "handler"})
	var built tail.Handler
	switch kind {
	case tail.KindSessionTranscript:
		built = handler.NewSessionTranscriptHandler(handlerLog)
	case tail.KindAgentTranscript:
		built = handler.NewAgentTranscriptHandler(handlerLog)
	case tail.KindWorkflowJournal:
		built = handler.NewWorkflowJournalHandler(handlerLog)
	case tail.KindShellSpool:
		built = handler.NewShellOutputHandler(handlerLog)
	case tail.KindWorkflowSpool:
		// R-S4: workflow is KICKED this wave. The spool is discovered and
		// cursor-tailed like any other file, and its bytes land as DECLARED
		// residue rather than being converted or dropped.
		built = newDeclaredResidueHandler(workflowSpoolKind, handlerLog)
	default:
		panic(fmt.Sprintf("sidecar: unsupported tail kind %d", kind))
	}
	s.plumbObserver(kind, built, handlerLog)
	s.plumbTaskStops(kind, built, handlerLog)
	s.plumbTaskConclusions(kind, built, handlerLog)
	s.plumbShellConclusions(kind, built, handlerLog)
	s.plumbTerminals(kind, built, handlerLog)
	return built
}

// transcriptFact describes one fact only a TRANSCRIPT converter reports to the
// reader, for plumbTranscriptFact: what it is called in the log, which method
// adopts it, and what goes wrong downstream when a transcript converter does not.
type transcriptFact struct {
	// operation is the plumbing record's operation.
	operation string
	// noun names the fact, plural ("task stops").
	noun string
	// onlyWhy says why only a transcript carries it.
	onlyWhy string
	// method is the Set*Observer method a converter adopts it by.
	method string
	// consequence is what a transcript converter's silence costs.
	consequence string
}

// plumbTranscriptFact hands a converter one of the reader's transcript-fact
// callbacks through ADOPT, which answers whether the converter took it.
//
// ONLY A TRANSCRIPT CARRIES THESE FACTS, so a spool's or a journal's converter
// declining one is ordinary and recorded at verbose, and a TRANSCRIPT converter
// declining one is a defect stated at error with what it costs.
func (s *sidecar) plumbTranscriptFact(kind tail.Kind, fact transcriptFact, adopt func() bool, log *logging.Bound) {
	if !adopt() {
		if kind != tail.KindSessionTranscript && kind != tail.KindAgentTranscript {
			log.With(logging.Context{Operation: fact.operation}).LogVerbose(
				"the %s converter reports no %s; %s", kind, fact.noun, fact.onlyWhy)
			return
		}
		log.With(logging.Context{Operation: fact.operation, Level: "error"}).Log(
			"the %s converter reports no %s (it implements no %s): %s", kind, fact.noun, fact.method, fact.consequence)
		return
	}
	log.With(logging.Context{Operation: fact.operation}).LogVerbose("%s plumbed for kind=%s", fact.noun, kind)
}

// plumbTaskStops hands the converter the reader's task-stop callback.
//
// ONLY A TRANSCRIPT CARRIES A TaskStop RESULT, for the same reason only a
// transcript carries a launch: both are tool results. A transcript converter
// that reports none leaves every cancelled run to be concluded LOST instead,
// which is the one thing the carve-out exists to prevent — so its silence is a
// defect, and a spool's is not.
func (s *sidecar) plumbTaskStops(kind tail.Kind, built tail.Handler, log *logging.Bound) {
	s.plumbTranscriptFact(kind, transcriptFact{
		operation:   "plumb-task-stop",
		noun:        "task stops",
		onlyWhy:     "only a transcript carries the tool results a stop is stated in",
		method:      "SetTaskStopObserver",
		consequence: "a run a person stopped will be concluded LOST instead of cancelled",
	}, func() bool {
		sink, ok := built.(taskStopSink)
		if ok {
			sink.SetTaskStopObserver(s.TaskStopped)
		}
		return ok
	}, log)
}

// plumbTaskConclusions hands the converter the reader's run-concluded callback.
//
// ONLY A TRANSCRIPT SETTLES A BACKGROUNDED AGENT RUN, so a transcript converter
// that reports none leaves every notified run to be overwritten LOST — a defect.
func (s *sidecar) plumbTaskConclusions(kind tail.Kind, built tail.Handler, log *logging.Bound) {
	s.plumbTranscriptFact(kind, transcriptFact{
		operation:   "plumb-task-conclusion",
		noun:        "run conclusions",
		onlyWhy:     "only a transcript settles a backgrounded agent run",
		method:      "SetTaskConclusionObserver",
		consequence: "a notified agent run can later be concluded LOST over its terminal",
	}, func() bool {
		sink, ok := built.(taskConclusionSink)
		if ok {
			sink.SetTaskConclusionObserver(s.TaskConcluded)
		}
		return ok
	}, log)
}

// plumbShellConclusions hands the converter the reader's shell-conclusion
// callback.
//
// THE SIDECAR IS THE ONLY WRITER OF A SHELL RUN'S TERMINAL, and a run whose
// spool carries no terminator ends only on its notification — so a transcript
// converter that reports none leaves such a run open until a silence window
// concludes it LOST, a defect.
func (s *sidecar) plumbShellConclusions(kind tail.Kind, built tail.Handler, log *logging.Bound) {
	s.plumbTranscriptFact(kind, transcriptFact{
		operation:   "plumb-shell-conclusion",
		noun:        "shell conclusions",
		onlyWhy:     "only a transcript carries a shell run's task notification",
		method:      "SetShellConclusionObserver",
		consequence: "a shell run whose spool carries no terminator stays open until it is concluded LOST",
	}, func() bool {
		sink, ok := built.(shellConclusionSink)
		if ok {
			sink.SetShellConclusionObserver(s.ShellConcluded)
		}
		return ok
	}, log)
}

// plumbTerminals hands the converter the reader's terminal-read callback.
//
// ONLY A DETACHED RUN HAS A TERMINAL TO READ. A transcript's silence concludes
// nothing, so a converter for one is not expected to report terminals and its
// silence here is recorded at verbose rather than as a defect.
func (s *sidecar) plumbTerminals(kind tail.Kind, built tail.Handler, log *logging.Bound) {
	sink, ok := built.(terminalReadSink)
	if !ok {
		log.With(logging.Context{Operation: "plumb-terminal-observer"}).LogVerbose(
			"the %s converter reports no run terminals; only a detached run has one to read", kind)
		return
	}
	sink.SetTerminalObserver(s.RunSettled)
	log.With(logging.Context{Operation: "plumb-terminal-observer"}).LogVerbose("terminal observations plumbed for kind=%s", kind)
}

// plumbObserver hands the converter the reader's spawn-observation callback.
//
// ONLY A TRANSCRIPT CAN REPORT A LAUNCH. A launch is stated in a TOOL RESULT,
// and a spool or a journal carries none — so their converters are not expected
// to report one and their silence is ordinary. A TRANSCRIPT converter that
// reports none is a different matter entirely: it is the only source the reader
// has, and without it every spool waits out its hold and goes to residue.
func (s *sidecar) plumbObserver(kind tail.Kind, built tail.Handler, log *logging.Bound) {
	sink, ok := built.(taskObserverSink)
	if !ok {
		if kind != tail.KindSessionTranscript && kind != tail.KindAgentTranscript {
			log.With(logging.Context{Operation: "plumb-observer"}).LogVerbose(
				"the %s converter reports no spawn observations; only a transcript carries the tool results a launch is stated in", kind)
			return
		}
		// The reader cannot learn which call spawned a task from any other
		// source, so a converter that reports none leaves every spool unclaimed
		// until its bounded wait expires and its bytes go to residue.
		log.With(logging.Context{Operation: "plumb-observer", Level: "error"}).Log(
			"the %s converter reports no spawn observations (it implements no SetTaskObserver): every spool it could have claimed stays unowned until its hold expires", kind)
		return
	}
	sink.SetTaskObserver(s.TaskSpawned)
	log.With(logging.Context{Operation: "plumb-observer"}).LogVerbose("spawn observations plumbed for kind=%s", kind)
}

// lostEntries turns the LOST policy's conclusions into the runs' terminals.
//
// THE REASON GOES ON THE WIRE AND IN THE LOG, and it is the same reason in
// both. conversation.v1's DetachedLost carries the three arms this reader
// concludes with (file_vanished / went_silent / swept_up), reached through
// AgentBashInterrupted.cause.lost, so HOW we stopped seeing a run is a fact a
// consumer can draw. The `reason` key on the record below is what joins that
// terminal back to the sweep that concluded it.
//
// EACH OUTCOME IS ITS OWN OPERATION. A conclusion that MINTED a terminal and one
// that refused because the file names no run are different branches, and a
// reader that can only tell them apart by the sentence they wrote cannot filter
// for "LOST runs left open" at all. `lost-terminal` is the minted one;
// `lost-terminal-unwatched`, `lost-terminal-residue` and
// `lost-terminal-unsupported` are the three that mint nothing.
func (s *sidecar) lostEntries(conclusions []stale.Lost) []*storev1.StoreEntry {
	var out []*storev1.StoreEntry
	for _, lost := range conclusions {
		// THE REASON RIDES THE BOUND CONTEXT, so EVERY branch below carries it.
		// It used to be interpolated into each branch's prose instead, which
		// left three of the four outcomes — a file no longer watched, a residue
		// spool naming no run, a converter with no LostTerminal — findable only
		// by substring, and a reader filtering on `reason` saw a conclusion
		// reached for a residue spool as no conclusion at all.
		bound := s.log.With(logging.Context{
			Operation: "lost-terminal", Path: lost.Path, TaskID: lost.TaskID,
			AgentID: lost.OwnerAgentID, ActivityID: lost.RunActivityID,
			Reason: string(lost.Reason),
		})
		// A run concluded LOST because its FILE IS GONE is never read again, so
		// its tailer goes as soon as its terminal has been stated (or refused).
		// A run that merely went quiet keeps its tailer: the file is still
		// there, and anything appended to it later must still land.
		if lost.Reason == stale.ReasonFileVanished {
			defer func(path string) {
				delete(s.watchers, path)
				s.log.With(logging.Context{Operation: "lost-terminal-tailer-dropped", Path: path}).
					LogVerbose("the vanished file's tailer is dropped now that its terminal has been stated")
			}(lost.Path)
		}
		// A RUN THE STORE ALREADY HOLDS A TERMINAL FOR IS NEVER RESTATED LOST.
		// Its conclusion settles the tracker, so reaching here means a path
		// escaped that settle; the LOST write would supersede the real terminal.
		if _, concluded := s.concluded[lost.TaskID]; concluded && lost.TaskID != "" {
			bound.With(logging.Context{Operation: "lost-terminal-refused", Level: "error"}).Log(
				"LOST refused: the run already concluded (task notification or stop) and a LOST terminal would supersede it (reason=%s)", lost.Reason)
			s.tracker.Settle(lost.Path)
			continue
		}
		if lost.Kind == tail.KindResidueSpool {
			// A RESIDUE SPOOL NAMES NO RUN. Its task-id prefix failed
			// classification, so no unit was ever opened for it and there is
			// nothing a terminal could settle. Such a spool is never read
			// (held.go), so it is never watched and never tracked — which is why
			// this is decided BEFORE the watcher lookup: a conclusion reaching
			// here for one is stated as what it is, never as a run whose
			// converter went missing, and never invents a run that never existed.
			bound.With(logging.Context{Operation: "lost-terminal-residue"}).Log("the LOST file was residue and named no run, so there is no unit to settle (reason=%s)", lost.Reason)
			continue
		}
		w, watched := s.watchers[lost.Path]
		if !watched {
			bound.With(logging.Context{Operation: "lost-terminal-unwatched", Level: "warn"}).Log(
				"no terminal for the LOST run: its file is no longer watched, so no converter is left to spell one (reason=%s)", lost.Reason)
			continue
		}
		sink, ok := w.tailer.Handler().(lostTerminalSink)
		if !ok {
			bound.With(logging.Context{Operation: "lost-terminal-unsupported", Level: "error"}).Log(
				"no terminal for the LOST run: the %s converter implements no LostTerminal, so the run stays open in every reader downstream (reason=%s)",
				lost.Kind, lost.Reason)
			continue
		}
		entries := sink.LostTerminal(lost.TaskID, lost.RunActivityID, lost.OwnerAgentID, string(lost.Reason), lost.Catchup)
		// THE REASON RIDES A DEDICATED KEY. It is the arm now set on the wire,
		// and a record that only interpolated it into a sentence could not be
		// filtered or joined against the terminal it explains.
		bound.Log("LOST terminal minted reason=%s entries=%d", lost.Reason, len(entries))
		out = append(out, entries...)
	}
	return out
}
