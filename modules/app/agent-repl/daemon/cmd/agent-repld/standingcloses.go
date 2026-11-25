package main

import (
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/holds"
	"agent-repld/internal/wsm"
)

// holdsTellingStandingCloses is the feed's held section as the prompt queue
// states its holds to it, telling the standing closes each workspace whose
// held prompts were stated anew: a prompt sent or dropped may clear the last
// thing a standing close request waits on. The queue states every change of a
// workspace's holds through these two calls, so no change is missed.
type holdsTellingStandingCloses struct {
	holds.Sink
	changed func(ws ids.WorkspaceID)
}

// SetHeldPrompts implements holds.Sink.
func (h holdsTellingStandingCloses) SetHeldPrompts(ws ids.WorkspaceID, held []wsm.HeldPrompt) {
	h.Sink.SetHeldPrompts(ws, held)
	h.changed(ws)
}

// FoldHeldPrompts implements holds.Sink.
func (h holdsTellingStandingCloses) FoldHeldPrompts(ws ids.WorkspaceID, held []wsm.HeldPrompt, fold holds.Fold) {
	h.Sink.FoldHeldPrompts(ws, held, fold)
	h.changed(ws)
}
