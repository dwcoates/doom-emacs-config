package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// THE COLD-CONTEXT GATE. DATA, not prose: the daemon relays the shim's
// SessionCold facts and the CLIENT owns the wording, the formatting and the
// ticking — a deliberate departure from the daemon-formats precedent, because
// the gate's facts are raw counts and instants rather than resolved
// presentation.
//
// The gate is ONE row per workspace: while STANDING it owns the composer, and
// once resolved it stays in history as the trace of the choice. Both states
// therefore key onto the same FeedId.

// coldGateRowID is the gate's identity: one per workspace, so the resolved
// trace replaces the standing gate rather than sitting beside it.
func (r *resolver) coldGateRowID(s *wsState) *frontendv1.FeedId {
	return r.rowID(s.id, feedid.Feed{Root: true}, feedid.RowKey{
		Kind: feedid.KindColdGate, ID: string(s.id),
	})
}

// RaiseColdGate draws the STANDING gate from the shim's cold facts and the
// remediations the daemon is willing to offer.
//
// scopes are the compaction types the answer verb will accept: the served
// values ARE the contract, so a client can only echo one back.
func (r *resolver) RaiseColdGate(ws ids.WorkspaceID, cold *conversationv1.SessionCold, summarizers []*conversationv1.AgentModel, scopes []conversationv1.SessionCompactScope) *frontendv1.FeedId {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)

	models := make([]*frontendv1.FeedColdGateModelOption, 0, len(summarizers))
	for _, model := range summarizers {
		models = append(models, &frontendv1.FeedColdGateModelOption{Model: model})
	}
	standing := &frontendv1.FeedColdGateStanding{
		// RAW counts and instants: the client formats and ticks.
		ContextTokens: &frontendv1.FeedColdGateContextTokens{Tokens: int64(cold.GetContextTokens())},
		LastRequest:   &frontendv1.FeedColdGateLastRequest{AtMs: cold.GetLastRequestAtMs()},
		Model:         &frontendv1.FeedColdGateModel{Model: cold.GetRequestedModel()},
		Compact: &frontendv1.FeedColdGateCompactMenu{
			Models: models,
			Scopes: scopes,
		},
	}

	id := r.coldGateRowID(s)
	row := &frontendv1.FeedRow{
		Id: id,
		Row: &frontendv1.FeedRow_ColdGate{ColdGate: &frontendv1.FeedColdGate{
			State: &frontendv1.FeedColdGate_Standing{Standing: standing},
		}},
	}
	r.logger(ws).Debug("daemon.feed.cold_gate_standing",
		"the cold-context gate was raised and owns the composer",
		dlog.Context{
			"row": id.GetValue(), "context_tokens": cold.GetContextTokens(),
			"reason": coldReasonWord(cold), "scopes": len(scopes), "summarizers": len(models),
		})
	r.upsert(s, placement{feed: feedid.Feed{Root: true}}, row, true)
	return id
}

// ResolveColdGate replaces the standing gate with the TRACE of what was
// chosen. The row stays in history: a reader scrolling back sees the decision,
// not a gate that vanished.
func (r *resolver) ResolveColdGate(ws ids.WorkspaceID, remediation *conversationv1.SessionColdRemediation) *frontendv1.FeedId {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)

	resolved := &frontendv1.FeedColdGateResolved{AtMs: r.deps.Now().UnixMilli()}
	switch choice := remediation.GetRemediation().(type) {
	case *conversationv1.SessionColdRemediation_Pay:
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "ResolveColdGate", "branch": "case *conversationv1.SessionColdRemediation_Pay"})
		resolved.Choice = &frontendv1.FeedColdGateResolved_Pay{Pay: &frontendv1.FeedColdGateResolvedPay{}}
	case *conversationv1.SessionColdRemediation_Clear:
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "ResolveColdGate", "branch": "case *conversationv1.SessionColdRemediation_Clear"})
		resolved.Choice = &frontendv1.FeedColdGateResolved_Clear{Clear: &frontendv1.FeedColdGateResolvedClear{}}
	case *conversationv1.SessionColdRemediation_Compact:
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "ResolveColdGate", "branch": "case *conversationv1.SessionColdRemediation_Compact"})
		compact := &frontendv1.FeedColdGateResolvedCompact{}
		// The trace names the summarizer AND what it summarized: a compaction
		// of the prompts alone bought something different from a compaction of
		// everything, and the trace is where that is recoverable.
		applyResolvedCompact(compact, choice.Compact.GetModel(), choice.Compact.GetScope())
		resolved.Choice = &frontendv1.FeedColdGateResolved_Compact{Compact: compact}
	default:
		r.logger(ws).Warn("daemon.feed.cold_gate_unset_remediation",
			"a cold-gate resolution carried no remediation; the gate was left standing",
			dlog.Context{})
		return nil
	}

	id := r.coldGateRowID(s)
	row := &frontendv1.FeedRow{
		Id: id,
		Row: &frontendv1.FeedRow_ColdGate{ColdGate: &frontendv1.FeedColdGate{
			State: &frontendv1.FeedColdGate_Resolved{Resolved: resolved},
		}},
	}
	r.logger(ws).Debug("daemon.feed.cold_gate_resolved",
		"the cold-context gate was resolved and stays as the trace of the choice",
		dlog.Context{"row": id.GetValue(), "choice": remediationWord(remediation)})
	r.upsert(s, placement{feed: feedid.Feed{Root: true}}, row, true)
	return id
}

// coldReasonWord names why the cache would not serve, for a log record.
func coldReasonWord(cold *conversationv1.SessionCold) string {
	switch cold.GetReason().(type) {
	case *conversationv1.SessionCold_Lapsed:
		return "lapsed"
	case *conversationv1.SessionCold_ModelSwitch:
		return "model_switch"
	}
	return "unset"
}

// remediationWord names the chosen remediation for a log record.
func remediationWord(remediation *conversationv1.SessionColdRemediation) string {
	switch remediation.GetRemediation().(type) {
	case *conversationv1.SessionColdRemediation_Pay:
		return "pay"
	case *conversationv1.SessionColdRemediation_Clear:
		return "clear"
	case *conversationv1.SessionColdRemediation_Compact:
		return "compact"
	}
	return "unset"
}
