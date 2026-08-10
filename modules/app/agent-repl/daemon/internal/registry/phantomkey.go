package registry

import (
	"path/filepath"
	"sort"
	"time"

	"claude-repld/internal/errclass"
)

// ---------------------------------------------------------------------------
// Phantom workspace-key consolidation at boot
// ---------------------------------------------------------------------------
//
// A record's CWD is its workspace key, and the daemon used to accept whatever
// spelling a command frame carried. A trailing separator therefore minted a
// PHANTOM record — a second workspace, with its own session, for a directory
// that already had one. Ingress canonicalizes the key now
// (internal/frontend/workspacekey.go), so no new phantom can appear, but rows
// written before that still sit in the store, and a phantom is not inert: its
// session enumerates as live, runs keep-alive machinery, squats the
// workspace's locks, and answers webapp pages that cached its session id.
//
// Boot therefore CONSOLIDATES them, once, idempotently:
//
//   - No canonical twin — the phantom IS the workspace under a bad spelling,
//     so its key is rewritten to the canonical one. A pure rename: no record
//     is created, retired, or dropped, and the session keeps its id.
//
//   - A canonical twin EXISTS — the phantom is a duplicate workspace, and the
//     canonical record is the one the workspace actually belongs to. The
//     phantom is RETIRED through the registry's existing retirement shape
//     (Terminal + DeathReason), never hard-deleted: the row is evidence that a
//     conversation ran, and every live enumeration in the daemon already
//     excludes a terminal record, so retirement is what takes its session out
//     of keep-alive and bring-up.
//
// CANONICAL RECORDS ARE NEVER TOUCHED, in either branch.
//
// The retirement is stamped RESOLVED at the same instant it is written. A
// supersede death is window-shaped — its card stays open until a successor is
// genuinely up — but there is no bring-up here to wait for: the successor is
// the canonical record, which already exists at the moment of the decision. An
// unresolved stamp would reopen a death card at every boot for a workspace
// that never lost anything.

// phantomRetirementReason is the death reason a retired phantom carries. A
// phantom is retired for exactly the reason a supersede exists: a newer,
// canonical record holds this workspace.
const phantomRetirementReason = errclass.DeathReasonSuperseded

// noncanonicalRecords returns the records whose workspace key is not its own
// canonical spelling, sorted by session id.
func noncanonicalRecords(records []Record) []Record {
	var out []Record
	for _, rec := range records {
		if rec.CWD == "" {
			continue
		}
		if filepath.Clean(rec.CWD) != rec.CWD {
			out = append(out, rec)
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i].SessionID < out[j].SessionID })
	return out
}

// phantomDecision is one consolidation decision, logged after the transaction
// that performed it commits.
type phantomDecision struct {
	sessionID string
	phantom   string
	canonical string
	action    string
}

// consolidationSummary counts what one consolidation pass did.
type consolidationSummary struct {
	consolidated int // phantom keys rewritten to canonical
	retired      int // phantom records retired behind a canonical twin
	untouched    int // noncanonical records deliberately left as they were
}

// sortedRecords returns state's records ordered by session id, so a pass over
// two phantoms of the same workspace resolves deterministically.
func sortedRecords(state *registryState) []Record {
	out := make([]Record, 0, len(state.records))
	for _, rec := range state.records {
		out = append(out, rec)
	}
	sort.Slice(out, func(i, j int) bool { return out[i].SessionID < out[j].SessionID })
	return out
}

// consolidatePhantomWorkspaceKeys rewrites or retires every noncanonical
// record in state. It is a registryState mutation, run inside Prepare's
// transaction; decisions are returned rather than logged so nothing is
// announced that a rolled-back transaction did not actually do.
func (r *Registry) consolidatePhantomWorkspaceKeys(state *registryState) ([]phantomDecision, consolidationSummary) {
	var (
		decisions []phantomDecision
		summary   consolidationSummary
	)
	records := sortedRecords(state)

	// The canonical keys already claimed by a LIVE record. A retired twin does
	// not hold a workspace, so it must not block a rename that would otherwise
	// give the workspace its clean spelling back.
	claimed := make(map[string]bool, len(records))
	for _, rec := range records {
		if rec.CWD == "" || rec.Terminal {
			continue
		}
		if filepath.Clean(rec.CWD) == rec.CWD {
			claimed[rec.CWD] = true
		}
	}

	for _, rec := range records {
		if rec.CWD == "" {
			continue
		}
		canonical := filepath.Clean(rec.CWD)
		if canonical == rec.CWD {
			continue // canonical: never touched
		}
		switch {
		case rec.Terminal:
			// Already retired — its key is history, and rewriting it would
			// move a dead conversation between workspaces for no gain.
			summary.untouched++
			decisions = append(decisions, phantomDecision{
				sessionID: rec.SessionID, phantom: rec.CWD, canonical: canonical, action: "untouched_terminal",
			})
		case claimed[canonical]:
			rec.Terminal = true
			rec.DeathReason = phantomRetirementReason
			now := r.now().UTC()
			rec.TerminalAt = now.Format(time.RFC3339Nano)
			rec.DeathResolvedAtMs = now.UnixMilli()
			state.records[rec.SessionID] = rec
			summary.retired++
			decisions = append(decisions, phantomDecision{
				sessionID: rec.SessionID, phantom: rec.CWD, canonical: canonical, action: "retired",
			})
		default:
			phantom := rec.CWD
			rec.CWD = canonical
			state.records[rec.SessionID] = rec
			claimed[canonical] = true
			summary.consolidated++
			decisions = append(decisions, phantomDecision{
				sessionID: rec.SessionID, phantom: phantom, canonical: canonical, action: "renamed",
			})
		}
	}
	return decisions, summary
}

// logPhantomConsolidation reports each decision and the pass summary. Silent
// when the store carries no noncanonical key at all.
func (r *Registry) logPhantomConsolidation(decisions []phantomDecision, summary consolidationSummary) {
	if len(decisions) == 0 {
		return
	}
	for _, d := range decisions {
		r.logf("registry: phantom workspace key session_id=%s cwd=%q canonical=%q action=%s",
			d.sessionID, d.phantom, d.canonical, d.action)
	}
	r.logf("registry: phantom workspace key consolidation consolidated=%d retired=%d untouched=%d",
		summary.consolidated, summary.retired, summary.untouched)
}
