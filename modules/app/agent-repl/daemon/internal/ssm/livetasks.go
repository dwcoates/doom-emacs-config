package ssm

import (
	"fmt"
)

// livetasks.go — THE LIVE BACKGROUND-TASK SET, BY IDENTITY.
//
// # Why this exists beside live_task_count
//
// resolve.go already folds the same rows into `live_task_count`, and that count
// is what WorkspaceState publishes, what the progress footer renders, and what
// the scheduled-shutdown drain reports its holds with. A count is the right
// shape for a badge and the WRONG shape for a bounce decision: it cannot tell
// "the same two tasks are still running" from "one died and a new one started",
// which is the error class that let a total shim-fleet loss pass for a clean
// restart on 2026-08-10 with an equal process count on both sides.
//
// So the identities are read here, from the SAME rows the count is folded from,
// and the two can never disagree about which tasks exist — only about how much
// they say.
//
// # ANONYMOUS STARTS ARE AN ANSWER OF "I CANNOT SAY"
//
// The count arithmetic tolerates `task_id IS NULL` rows: a legacy or malformed
// start with no identity still contributes 1. A SET cannot tolerate them,
// because an item with no identity could never be recognised on the far side of
// a bounce. Dropping them would UNDERSTATE what is running, which is the one
// direction that gets somebody's work killed — so they are counted separately
// and returned, and the caller that builds the in-flight set turns a non-zero
// count into an UNKNOWN rather than into a smaller set.

// liveTaskIDsQuery names every task with an observed start and no observed end,
// plus the number of anonymous live starts that have no identity to name.
//
// It mirrors resolve.go's live_task_count arithmetic exactly: identified live
// tasks are STARTS EXCEPT ENDS (an end with no matching start is an anomaly for
// the ingestion edge to repair, never a negative contribution here), and the
// anonymous leg is a floored starts-minus-ends difference.
const liveTaskIDsQuery = `
WITH rows AS (
  SELECT state, task_id FROM workspace_state WHERE workspace = ?
),
live AS (
  SELECT DISTINCT task_id AS task_key
  FROM rows WHERE state = 'task_started' AND task_id IS NOT NULL
  EXCEPT
  SELECT DISTINCT task_id AS task_key
  FROM rows WHERE state = 'task_ended' AND task_id IS NOT NULL
)
SELECT
  (SELECT COALESCE(GROUP_CONCAT(task_key, char(10)), '') FROM (SELECT task_key FROM live ORDER BY task_key)),
  MAX(
    (SELECT COUNT(*) FROM rows WHERE state = 'task_started' AND task_id IS NULL)
    - (SELECT COUNT(*) FROM rows WHERE state = 'task_ended' AND task_id IS NULL),
    0)
`

// LiveTaskIDs reports the workspace's live background tasks BY IDENTITY, and
// the number of live tasks whose start carried no identity at all.
//
// A workspace with nothing running answers with no ids, no anonymous starts and
// no error: holding nothing is an answer. A non-zero anonymous count is NOT an
// error either — the rows are real work — but it makes the identified list an
// incomplete account of what is running, and the caller must treat it as such.
func (m *Manager) LiveTaskIDs(workspace string) (ids []string, anonymous int64, err error) {
	if workspace == "" {
		err := fmt.Errorf("ssm: reading live task identities requires a workspace")
		m.logf("ssm: live task set decision=reject_validation operation=live_task_ids workspace=%q error=%v", workspace, err)
		return nil, 0, err
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	var joined string
	if scanErr := m.db.QueryRow(liveTaskIDsQuery, workspace).Scan(&joined, &anonymous); scanErr != nil {
		readErr := fmt.Errorf("ssm: read live task identities for workspace %q: %w", workspace, scanErr)
		m.logf("ssm: live task set decision=unreadable operation=live_task_ids workspace=%s error=%v", workspace, readErr)
		return nil, 0, readErr
	}
	ids = splitTaskIDs(joined)
	if anonymous > 0 {
		m.logf("ssm: live task set ANONYMOUS STARTS workspace=%s identified=%d anonymous=%d — these tasks are running and cannot be named, so the identified list is an incomplete account of this workspace's work",
			workspace, len(ids), anonymous)
	}
	return ids, anonymous, nil
}

// splitTaskIDs unpacks the newline-joined identities. A task id containing a
// newline would be indistinguishable from two ids, so the joiner uses the one
// character the id vocabulary (a uuid or a tool_use_id) cannot contain; an
// empty segment is dropped rather than admitted as an unidentified member.
func splitTaskIDs(joined string) []string {
	if joined == "" {
		return nil
	}
	var out []string
	start := 0
	for i := 0; i <= len(joined); i++ {
		if i == len(joined) || joined[i] == '\n' {
			if seg := joined[start:i]; seg != "" {
				out = append(out, seg)
			}
			start = i + 1
		}
	}
	return out
}
