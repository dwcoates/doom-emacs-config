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
	SetTaskObserver(func(taskID, toolUseID, agentID, outputPath string))
}

// lostTerminalSink is implemented by a handler that can mint a detached run's
// terminal from the reader's LOST conclusion. The reader states HOW it stopped
// seeing the run; spelling that as the run's terminal frame is conversion.
type lostTerminalSink interface {
	LostTerminal(taskID, runActivityID, ownerAgentID, reason string) []*storev1.StoreEntry
}

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
	case tail.KindResidueSpool:
		built = newResidueHandler("the spool's task id carries no a/b/w kind prefix, so no conversion could be selected for it", handlerLog)
	default:
		panic(fmt.Sprintf("sidecar: unsupported tail kind %d", kind))
	}
	s.plumbObserver(kind, built, handlerLog)
	return built
}

// plumbObserver hands the converter the reader's spawn-observation callback.
func (s *sidecar) plumbObserver(kind tail.Kind, built tail.Handler, log *logging.Bound) {
	sink, ok := built.(taskObserverSink)
	if !ok {
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
// The reason is carried into the LOG rather than onto the wire: store.v1 and
// conversation.v1 have no DetachedLost message this wave, so HOW we stopped
// seeing the run survives only in the record this writes. That is a contract
// gap, and it is stated here rather than papered over by picking an arm that
// means something else.
func (s *sidecar) lostEntries(conclusions []stale.Lost) []*storev1.StoreEntry {
	var out []*storev1.StoreEntry
	for _, lost := range conclusions {
		bound := s.log.With(logging.Context{
			Operation: "lost-terminal", Path: lost.Path, TaskID: lost.TaskID,
			AgentID: lost.OwnerAgentID, ActivityID: lost.RunActivityID,
		})
		// A run concluded LOST because its FILE IS GONE is never read again, so
		// its tailer goes as soon as its terminal has been stated (or refused).
		// A run that merely went quiet keeps its tailer: the file is still
		// there, and anything appended to it later must still land.
		if lost.Reason == stale.ReasonFileVanished {
			defer func(path string) {
				delete(s.watchers, path)
				s.log.With(logging.Context{Operation: "lost-terminal", Path: path}).
					LogVerbose("the vanished file's tailer is dropped now that its terminal has been stated")
			}(lost.Path)
		}
		w, watched := s.watchers[lost.Path]
		if !watched {
			bound.With(logging.Context{Level: "warn"}).Log(
				"no terminal for the LOST run: its file is no longer watched, so no converter is left to spell one (reason=%s)", lost.Reason)
			continue
		}
		sink, ok := w.tailer.Handler().(lostTerminalSink)
		if !ok {
			bound.With(logging.Context{Level: "error"}).Log(
				"no terminal for the LOST run: the %s converter implements no LostTerminal, so the run stays open in every reader downstream (reason=%s)",
				lost.Kind, lost.Reason)
			continue
		}
		entries := sink.LostTerminal(lost.TaskID, lost.RunActivityID, lost.OwnerAgentID, string(lost.Reason))
		bound.Log("LOST terminal minted reason=%s entries=%d", lost.Reason, len(entries))
		out = append(out, entries...)
	}
	return out
}
