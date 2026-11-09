package footer

import (
	"strings"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE VENDOR'S SILENCE.
//
// A turn whose vendor has said nothing past the silence threshold reads the
// `waiting` step (workingStep), and the activity cell under that step is the
// vendor-silence line (silenceActivity): what was last heard, and what that
// means for the response the turn waits on. The line's instant is the moment
// the vendor was last heard from, so the age the client ticks IS the time
// since the last API response.

// lastHeard is the vendor's last word in the turn in flight, as the waiting
// step's line names it.
type lastHeard struct {
	// stalled reports a frame that was part of a response still streaming, so
	// the response stalled; false is a frame that closed out an exchange (or
	// the turn's opening), so the next response has not started.
	stalled bool
	// text is the line's words for the frame.
	text string
	// cause names the edge for the record: the frame's item arm and phase arm,
	// or the turn's opening.
	cause string
}

// heardTurnOpened is the turn's opening: the prompt went out, and nothing of
// the response has come back.
var heardTurnOpened = lastHeard{text: "the prompt went out", cause: "turn_opened"}

// heardOf words one classified frame as the vendor's last word. Thinking and
// reply text are a response streaming; so is any item still running. An item
// that landed, or one with no lifecycle, closed out an exchange. A subagent's
// frame says so, so it is never read as the main agent's.
func heardOf(f classifiedFrame, subagent bool) lastHeard {
	label := strings.ReplaceAll(string(f.arm), "_", " ")
	if label == "" {
		label = "an activity frame"
	}
	h := lastHeard{cause: string(f.arm) + "." + string(f.phaseArm)}
	switch {
	case f.arm == "thinking":
		h.stalled, h.text = true, "thinking"
	case f.arm == "response":
		h.stalled, h.text = true, "reply text"
	case f.phase == phaseRunning:
		h.stalled, h.text = true, label+" started"
	case f.phase == phaseLanded:
		h.text = label + " finished"
	default:
		h.text = label
	}
	if subagent {
		h.text = "a subagent's " + h.text
	}
	return h
}

// heardFromVendor records that the vendor was last heard from now, saying
// HEARD, and arms the one timer that republishes the strip once the
// vendor-silence threshold passes with nothing more, when the `thinking` step
// reads `waiting` (vendorSilent). A silence the strip had drawn ends here, and
// its end is recorded. Nothing on the wire ticks: the timer's only effect is
// one more whole-view publication.
func (r *resolver) heardFromVendor(ws ids.WorkspaceID, s *wsState, heard lastHeard) {
	r.endSilence(ws, s, heard.cause)
	s.heardAt = r.opts.clock.Now()
	s.heard = heard
	s.silence = r.opts.clock.AfterFunc(r.opts.vendorSilence, func() { r.vendorSilenceElapsed(ws) })
}

// endSilence stops the silence timer and, when the strip had turned to
// `waiting`, records the silence's end: what ended it and how long it lasted.
// It runs wherever the silence can end — the vendor heard from, the turn's
// end — so every end is recorded once.
func (r *resolver) endSilence(ws ids.WorkspaceID, s *wsState, cause string) {
	if s.silence != nil {
		s.silence.Stop()
		s.silence = nil
	}
	if !s.silenceDrawn {
		return
	}
	s.silenceDrawn = false
	r.logOf(ws, s).Info("daemon.footer.vendor_silence_ended",
		"the vendor's silence ended; the waiting step gave way",
		dlog.Context{
			"cause":           cause,
			"silent_ms":       r.opts.clock.Now().Sub(s.heardAt).Milliseconds(),
			"heard_at":        s.heardAt,
			"last_heard":      s.heard.text,
			"last_heard_edge": s.heard.cause,
			"stalled":         s.heard.stalled,
		})
}

// vendorSilenceElapsed republishes the strip once the vendor has been silent
// past the threshold, and records the step turning to `waiting` with what
// was last heard.
func (r *resolver) vendorSilenceElapsed(ws ids.WorkspaceID) {
	r.mutate(ws, "daemon.footer.vendor_silence",
		"the vendor-silence threshold passed; the footer republished its working step",
		nil, func(s *wsState) {
			s.silence = nil
			if s.turn != nil && r.vendorSilent(s) {
				s.silenceDrawn = true
				r.logOf(ws, s).Info("daemon.footer.vendor_silence",
					"nothing has come back from the vendor past the silence threshold; the thinking step reads waiting",
					dlog.Context{
						"heard_at":        s.heardAt,
						"silence_ms":      r.opts.vendorSilence.Milliseconds(),
						"last_heard":      s.heard.text,
						"last_heard_edge": s.heard.cause,
						"stalled":         s.heard.stalled,
					})
			}
		})
}

// vendorSilent reports whether the vendor has been silent past the threshold.
func (r *resolver) vendorSilent(s *wsState) bool {
	return !r.opts.clock.Now().Before(s.heardAt.Add(r.opts.vendorSilence))
}

// silenceActivity is the activity cell under the `waiting` step: the
// vendor-silence line, standing since the vendor was last heard from.
func silenceActivity(s *wsState) *frontendv1.FooterStatusWorkingActivity {
	heard := &frontendv1.FooterStatusActivityVendorSilenceLastHeard{Text: s.heard.text}
	line := &frontendv1.FooterStatusActivityVendorSilence{}
	if s.heard.stalled {
		line.Response = &frontendv1.FooterStatusActivityVendorSilence_Stalled{
			Stalled: &frontendv1.FooterStatusActivityVendorSilenceStalled{LastHeard: heard}}
	} else {
		line.Response = &frontendv1.FooterStatusActivityVendorSilence_NotStarted{
			NotStarted: &frontendv1.FooterStatusActivityVendorSilenceNotStarted{LastHeard: heard}}
	}
	return &frontendv1.FooterStatusWorkingActivity{Tier: &frontendv1.FooterStatusWorkingActivity_Salient{
		Salient: &frontendv1.FooterStatusWorkingSalient{
			At:   stamp(s.heardAt),
			Kind: &frontendv1.FooterStatusWorkingSalient_VendorSilence{VendorSilence: line},
		}}}
}
