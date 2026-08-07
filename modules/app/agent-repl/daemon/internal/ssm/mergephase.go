package ssm

import (
	"fmt"
	"strings"

	"claude-repld/internal/workspace/merge"
)

// This file answers ONE question over the append-only log: which workspaces
// currently rest on a given merge phase.
//
// It exists for the boot-time sweep of `merge_enqueuing`. That phase is the
// command handler's "I have this" mark, emitted before anything durable is
// written, so it is the one merge state with NO queue entry behind it. A daemon
// that dies between the mark and the durable enqueue leaves a workspace pinned
// on a phase nothing will ever advance, and the only party that can notice is
// the next boot — which needs to read the phase back out of the log to do it.
//
// It reads the LATEST merge row per workspace, not "any row ever". A workspace
// that enqueued, merged, and enqueued again has a merge_enqueuing row in its
// history forever; only its newest merge row says where it rests now.

// mergeAxisStates are the tokens that constitute the merge axis. It is the
// same set resolveQuery's latest_merge CTE selects on, and the two MUST stay
// together: a token in one and not the other would make "the latest merge row"
// mean two different things in the resolver and in this sweep.
var mergeAxisStates = []string{
	sigMergeEnqueuing,
	sigMerging,
	sigMergeBeforeAction,
	sigMergeAfterAction,
	sigMergeQueued,
	sigMergeConflict,
	sigMergeFailed,
	sigMerged,
	sigMergeNone,
}

// mergeAxisPlaceholders renders one SQL bind placeholder per merge-axis token.
// Every query that selects over the axis builds its IN list from here, so a new
// token can never leave a hand-written placeholder count behind — the two would
// disagree silently until some workspace resolved the wrong phase.
func mergeAxisPlaceholders() string {
	return strings.TrimSuffix(strings.Repeat("?,", len(mergeAxisStates)), ",")
}

// WorkspacesAtMergePhase reports every workspace whose LATEST merge-axis row
// is phase, in stable workspace order.
//
// An unknown phase is a hard error rather than an empty result: a caller
// sweeping on a token nothing writes would find nothing and report a clean
// system, which is the silent failure this refuses.
func (m *Manager) WorkspacesAtMergePhase(phase merge.Phase) ([]string, error) {
	token, err := mergeToken(string(phase))
	if err != nil {
		m.warn("ssm: WorkspacesAtMergePhase REFUSED unknown phase %q: %v", phase, err)
		return nil, err
	}

	m.mu.Lock()
	defer m.mu.Unlock()

	query := `
WITH merge_rows AS (
  SELECT workspace, state, at FROM workspace_state
  WHERE state IN (` + mergeAxisPlaceholders() + `)
)
SELECT workspace FROM merge_rows r
WHERE r.state = ?
  AND r.at = (SELECT MAX(at) FROM merge_rows n WHERE n.workspace = r.workspace)
ORDER BY workspace`
	args := make([]any, 0, len(mergeAxisStates)+1)
	for _, s := range mergeAxisStates {
		args = append(args, s)
	}
	args = append(args, token)

	rows, err := m.db.Query(query, args...)
	if err != nil {
		m.warn("ssm: WorkspacesAtMergePhase query FAILED {phase=%s}: %v", token, err)
		return nil, fmt.Errorf("ssm: list workspaces at merge phase %q: %w", token, err)
	}
	defer rows.Close()
	var out []string
	for rows.Next() {
		var ws string
		if err := rows.Scan(&ws); err != nil {
			m.warn("ssm: WorkspacesAtMergePhase scan FAILED {phase=%s}: %v", token, err)
			return nil, fmt.Errorf("ssm: scan workspace at merge phase %q: %w", token, err)
		}
		out = append(out, ws)
	}
	if err := rows.Err(); err != nil {
		m.warn("ssm: WorkspacesAtMergePhase iterate FAILED {phase=%s}: %v", token, err)
		return nil, fmt.Errorf("ssm: iterate workspaces at merge phase %q: %w", token, err)
	}
	return out, nil
}
