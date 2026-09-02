package main

import (
	"context"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/sessionwatcher"
)

// THE LIFECYCLE SINK.
//
// The session watcher routes three facts at the daemon's own machinery rather
// than at a view, and they belong to two different components: the prompt
// queue drains on a turn's end, and the workspace verbs raise a host
// notification. The sink is the two of them side by side, and it is here
// because it is nothing but the two of them side by side.
type lifecycleSink struct {
	queue promptqueue.Queue
	verbs *verbsForwarder
	log   dlog.Logger
}

// OnTurnEnded pops the queue and releases a hold-for-turn-end.
func (s *lifecycleSink) OnTurnEnded(ws ids.WorkspaceID, turn ids.TurnID, how sessionwatcher.TurnClose) {
	s.queue.OnTurnEnded(ws, turn, how)
}

// OnLiveWorkChanged has no consumer beyond the watcher itself: FREENESS is
// answered inside the watcher, from the same set, and the roster hears the set
// on its own sink. It is recorded rather than dropped silently, so a future
// consumer is added here rather than discovered to be missing.
func (s *lifecycleSink) OnLiveWorkChanged(ws ids.WorkspaceID, live sessionwatcher.LiveWorkSet) {
	s.log.Debug("daemon.cmd.lifecycle", "the live-work set changed", dlog.Context{
		"workspace": string(ws),
		"agents":    len(live.Agents),
		"shells":    len(live.Shells),
		"monitors":  len(live.Monitors),
	})
}

// OnNotification raises the host notification and the roster's attention
// marker through the verbs, which own both.
func (s *lifecycleSink) OnNotification(ws ids.WorkspaceID, note sessionwatcher.HostNotification) {
	verbs, ok := s.verbs.verbs()
	if !ok {
		s.log.Error("daemon.cmd.lifecycle", "a notification arrived before the workspace verbs existed", dlog.Context{
			"workspace": string(ws), "kind": string(note.Kind),
		})
		return
	}
	if err := verbs.Notify(context.Background(), ws, note); err != nil {
		s.log.Error("daemon.cmd.lifecycle", "the host notification could not be raised", dlog.Context{
			"workspace": string(ws), "kind": string(note.Kind), "cause": err.Error(),
		})
	}
}
