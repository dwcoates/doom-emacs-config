package topbar

import (
	"fmt"
	"sort"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// warningStrip resolves everything the topbar has to warn about right now,
// NEWEST FIRST and capped.
//
// AN EMPTY LIST IS AN ANSWER: it is the daemon saying nothing is wrong, and the
// client draws no indicator at all rather than a quiet control over an empty
// list.
func (r *resolver) warningStrip(s *wsState) *frontendv1.TopbarWarningStrip {
	collected := make([]warning, 0, 8)
	if w, ok := r.sessionlessWarning(s); ok {
		collected = append(collected, w)
	}
	if w, ok := r.accountingWarning(s); ok {
		collected = append(collected, w)
	}
	collected = append(collected, r.unmodeledWarnings(s)...)
	collected = append(collected, r.detachedUnmodeledWarnings(s)...)
	collected = append(collected, r.faultWarnings(s)...)
	collected = append(collected, r.windowWarnings(s)...)
	collected = append(collected, r.raisedWarnings(s)...)

	sort.SliceStable(collected, func(i, j int) bool { return collected[i].seq > collected[j].seq })
	if len(collected) > r.opts.warningCap {
		collected = collected[:r.opts.warningCap]
	}

	out := &frontendv1.TopbarWarningStrip{}
	for _, w := range collected {
		out.Warnings = append(out.Warnings, w.detail())
	}
	return out
}

// sessionlessWarning is the workspace's session-less state, as a LINE.
//
// THE FACT MUST NOT BE LOST. The whole-view `hibernated` and `cold_gate`
// states were retired by the FIXED SCHEMA ruling (2026-09-13), and this is
// where the fact they carried went: the strip is one shape, so a state that
// used to replace it is now a sentence in the list of things a reader should
// know. The same sentence heads the context chip's hover, because it is one
// fact drawn in two places.
//
// IT CARRIES NO OVERLAY. There is nothing further to reveal and, for the cold
// gate, nothing to answer HERE: the feed's gate card is the one place a gate
// is answered, and a second place to answer one question is exactly what the
// strip must not become.
//
// SEQ 0, SO IT SORTS LAST. The dropdown draws the newest observation first,
// and a standing state is the OLDEST thing in the list — it was true before
// anything that is wrong right now went wrong.
func (r *resolver) sessionlessWarning(s *wsState) (warning, bool) {
	line := sessionlessReason(s)
	if line == "" {
		return warning{}, false
	}
	return warning{
		kind: warnSessionless, key: "sessionless", seq: 0, line: line,
		detail: func() *frontendv1.TopbarWarning {
			return &frontendv1.TopbarWarning{
				Line: &frontendv1.TopbarWarningLine{Text: truncate(line, DefaultLineWidth)},
			}
		},
	}, true
}

// accountingWarning is the SESSION-level reconciliation's own verdict. It is
// this component's OWN copy of the fact the footer's verdict line draws:
// duplicate, don't share — the footer reconciles a TURN and this reconciles the
// SESSION, and the two are different facts that happen to read alike.
//
// It RETRACTS on its own: once every settled response's usage has been
// observed and nothing contradicts, there is nothing to warn about.
func (r *resolver) accountingWarning(s *wsState) (warning, bool) {
	// THE RECONCILIATION IS PER API RESPONSE, NEVER PER UNIT. Usage rides
	// exactly one unit per API response, so comparing the settled-response set
	// against the usage-carrying set compares two different key spaces: an
	// ordinary prose turn, whose usage rode the thinking unit that opened the
	// response, would warn that its one response is missing usage.
	var lines []string
	missing := s.responses.Unaccounted()
	switch {
	case len(s.contradictions) > 0:
		lines = append([]string(nil), s.contradictions...)
		sort.Strings(lines)
	case missing > 0:
		lines = []string{fmt.Sprintf("%s missing usage", plural(missing, "response"))}
	default:
		return warning{}, false
	}

	if s.accountingSeq == 0 {
		s.accountingSeq = s.nextSeq()
	}
	line := "the session's token accounting does not reconcile"
	seq := s.accountingSeq
	return warning{
		kind: warnAccounting, key: "accounting", seq: seq, line: line,
		detail: func() *frontendv1.TopbarWarning {
			detail := &frontendv1.TopbarAccountingWarningDetail{}
			for _, text := range lines {
				detail.Lines = append(detail.Lines,
					&frontendv1.TopbarWarningDetailLine{Text: truncate(text, DefaultLineWidth)})
			}
			return &frontendv1.TopbarWarning{
				Line:   &frontendv1.TopbarWarningLine{Text: line},
				Detail: &frontendv1.TopbarWarning_Accounting{Accounting: detail},
			}
		},
	}, true
}

// unmodeledWarnings are the distinct unmodeled tools the session has run, ONE
// PER DISTINCT NAME.
//
// NOT DRAWN AS A FAILURE: the tool very likely ran fine, and the only thing
// wrong is that this contract cannot describe it. They are never retracted,
// because the blind spot does not close when the call ends.
func (r *resolver) unmodeledWarnings(s *wsState) []warning {
	out := make([]warning, 0, len(s.unmodeled))
	for _, call := range s.unmodeled {
		call := call
		line := "an unmodeled tool ran: " + call.toolName
		out = append(out, warning{
			kind: warnUnmodeledTool, key: "unmodeled:" + call.toolName, seq: call.seq, line: line,
			detail: func() *frontendv1.TopbarWarning {
				detail := &frontendv1.TopbarUnmodeledToolWarningDetail{
					ToolName: &frontendv1.TopbarUnmodeledToolName{Text: call.toolName},
				}
				for _, text := range call.argumentLines {
					detail.ArgumentLines = append(detail.ArgumentLines,
						&frontendv1.TopbarWarningDetailLine{Text: text})
				}
				return &frontendv1.TopbarWarning{
					Line:   &frontendv1.TopbarWarningLine{Text: truncate(line, DefaultLineWidth)},
					Detail: &frontendv1.TopbarWarning_UnmodeledTool{UnmodeledTool: detail},
				}
			},
		})
	}
	return out
}

// detachedUnmodeledWarnings are the live unmodeled items running DETACHED —
// one warning per live item. The footer's chips deliberately do not carry them,
// so this is the only place they can be seen.
func (r *resolver) detachedUnmodeledWarnings(s *wsState) []warning {
	out := make([]warning, 0, len(s.detachedUnmodeled))
	for i, item := range s.detachedUnmodeled {
		item := item
		line := "unmodeled work is running detached: " + item.ToolName
		out = append(out, warning{
			kind: warnDetachedUnmodeled,
			key:  fmt.Sprintf("detached:%s:%d", item.ToolName, item.StartedAt.UnixMilli()),
			seq:  s.detachedSeq + i,
			line: line,
			detail: func() *frontendv1.TopbarWarning {
				return &frontendv1.TopbarWarning{
					Line: &frontendv1.TopbarWarningLine{Text: truncate(line, DefaultLineWidth)},
					Detail: &frontendv1.TopbarWarning_DetachedUnmodeled{
						DetachedUnmodeled: &frontendv1.TopbarDetachedUnmodeledWarningDetail{
							ToolName:    &frontendv1.TopbarUnmodeledToolName{Text: item.ToolName},
							StartedAtMs: item.StartedAt.UnixMilli(),
						},
					},
				}
			},
		})
	}
	return out
}

// faultWarnings are the shim's standing faults, one warning each. They are
// RETRACTED by the next healthy diagnostics push: the arm is the verdict, and a
// healthy verdict means no fault stands.
func (r *resolver) faultWarnings(s *wsState) []warning {
	out := make([]warning, 0, len(s.faults))
	for _, fault := range s.faults {
		fault := fault
		out = append(out, warning{
			kind: warnSessionFault, key: "fault:" + fault.component + ":" + fault.line,
			seq: fault.seq, line: fault.line,
			detail: func() *frontendv1.TopbarWarning {
				return &frontendv1.TopbarWarning{
					Line: &frontendv1.TopbarWarningLine{Text: truncate(fault.line, DefaultLineWidth)},
					Detail: &frontendv1.TopbarWarning_SessionFault{
						SessionFault: &frontendv1.TopbarSessionFaultWarningDetail{
							Component: &frontendv1.TopbarWarningComponent{Text: fault.component},
							Detail:    &frontendv1.TopbarWarningDetailLine{Text: fault.detail},
						},
					},
				}
			},
		})
	}
	return out
}

// windowWarnings are the degraded windows, one per window. A window is
// EVIDENCE THAT WHAT IS ON SCREEN MAY HAVE HOLES from that span, so a closed
// one is kept rather than forgotten.
func (r *resolver) windowWarnings(s *wsState) []warning {
	out := make([]warning, 0, len(s.windows))
	for _, window := range s.windows {
		window := window
		line := window.component + " was degraded"
		if window.closed && window.dropped > 0 {
			line = fmt.Sprintf("%s was degraded and dropped %d observations",
				window.component, window.dropped)
		} else if !window.closed {
			line = window.component + " is degraded right now"
		}
		out = append(out, warning{
			kind: warnDegradedWindow,
			key:  fmt.Sprintf("window:%s:%d", window.component, window.beganAtMs),
			seq:  window.seq, line: line,
			detail: func() *frontendv1.TopbarWarning {
				detail := &frontendv1.TopbarDegradedWindowWarningDetail{
					Component: &frontendv1.TopbarWarningComponent{Text: window.component},
					Reason:    &frontendv1.TopbarWarningDetailLine{Text: window.reason},
					BeganAtMs: window.beganAtMs,
				}
				if window.closed {
					detail.Extent = &frontendv1.TopbarDegradedWindowWarningDetail_Closed{
						Closed: &frontendv1.TopbarDegradedWindowClosed{
							EndedAtMs:    window.endedAtMs,
							DroppedCount: window.dropped,
						},
					}
				} else {
					detail.Extent = &frontendv1.TopbarDegradedWindowWarningDetail_Open{
						Open: &frontendv1.TopbarDegradedWindowOpen{},
					}
				}
				return &frontendv1.TopbarWarning{
					Line:   &frontendv1.TopbarWarningLine{Text: truncate(line, DefaultLineWidth)},
					Detail: &frontendv1.TopbarWarning_DegradedWindow{DegradedWindow: detail},
				}
			},
		})
	}
	return out
}

// raisedWarnings are the conditions the daemon raised about its own
// resolution, one line each and no overlay: the raising site's own record
// carries the full context, and the line is what puts it in front of the
// reader.
func (r *resolver) raisedWarnings(s *wsState) []warning {
	out := make([]warning, 0, len(s.raised))
	for key, record := range s.raised {
		record := record
		out = append(out, warning{
			kind: warnRaised, key: "raised:" + key, seq: record.seq, line: record.line,
			detail: func() *frontendv1.TopbarWarning {
				return &frontendv1.TopbarWarning{
					Line: &frontendv1.TopbarWarningLine{Text: truncate(record.line, DefaultLineWidth)},
				}
			},
		})
	}
	return out
}

// applyDiagnostics folds one health push into the fault and window records.
// The faults are REPLACED wholesale, which is what makes a healthy verdict a
// retraction; the windows are merged, because the shim reports every window it
// has recorded since it started and a closed one is still evidence.
func (r *resolver) applyDiagnostics(s *wsState, diagnostics *conversationv1.SessionDiagnostics) {
	seen := map[string]*faultRecord{}
	if unhealthy, ok := diagnostics.GetHealth().(*conversationv1.SessionDiagnostics_Unhealthy); ok {
		for _, fault := range unhealthy.Unhealthy.GetFaults() {
			component, line := respellFault(fault)
			key := "fault:" + component + ":" + line
			seq := s.nextSeq()
			if previous, held := s.faults[key]; held {
				seq = previous.seq
			}
			detail := fault.GetDetail()
			if detail == "" {
				detail = line
			}
			seen[key] = &faultRecord{component: component, detail: detail, line: line, seq: seq}
		}
	}
	s.faults = seen

	for _, window := range diagnostics.GetDegradedWindows() {
		key := fmt.Sprintf("window:%s:%d", window.GetComponent(), window.GetBeganAtMs())
		seq := s.nextSeq()
		if previous, held := s.windows[key]; held {
			seq = previous.seq
		}
		record := &windowRecord{
			component: window.GetComponent(),
			reason:    window.GetReason(),
			beganAtMs: window.GetBeganAtMs(),
			seq:       seq,
		}
		if closed, ok := window.GetExtent().(*conversationv1.SessionDegradedWindow_Closed); ok {
			record.closed = true
			record.endedAtMs = closed.Closed.GetEndedAtMs()
			record.dropped = int64(closed.Closed.GetDroppedCount())
		}
		s.windows[key] = record
	}
}

// respellFault turns a SessionFault's typed KIND into the component and the
// sentence the overlay draws. The shim's own component and detail win where it
// stated them; the kind supplies both where it did not, so a fault is never
// drawn as a blank.
func respellFault(fault *conversationv1.SessionFault) (component, line string) {
	kindComponent, kindLine := faultKindWords(fault)
	component = fault.GetComponent()
	if component == "" {
		component = kindComponent
	}
	return component, kindLine
}

// faultKindWords is the respelling table for SessionFault.kind.
func faultKindWords(fault *conversationv1.SessionFault) (component, line string) {
	switch fault.GetKind().(type) {
	case *conversationv1.SessionFault_StoreUnreachable:
		return "store client", "the store cannot be reached; writes are buffering"
	case *conversationv1.SessionFault_ConverterDefect:
		return "converter", "the shim met a record it should model and could not"
	case *conversationv1.SessionFault_LogSinkPoisoned:
		return "log sink", "the shim's log sink is poisoned; records are being lost"
	case *conversationv1.SessionFault_KeepaliveFailed:
		return "keep-alive", "a keep-alive submission failed; the prompt cache may lapse"
	case *conversationv1.SessionFault_VendorQueryFailed:
		return "vendor query", "a vendor query failed outside the stream's own terminals"
	default:
		return "the session", "the shim reported a fault it did not classify"
	}
}
