package ssm

import (
	"database/sql"
	"fmt"
)

// This file answers ONE question: what happens to the merge axis when a merged
// workspace is REOPENED.
//
// `merged` outranks every session-status row (rank 3, above the whole color
// ladder), and the merge axis has no natural closing edge — nothing supersedes
// a merge row except another merge row. That combination made `merged` STICKY:
// a workspace that merged, and was then reopened and brought back up, kept
// resolving `merged` forever. Its live session ran, its turns started and
// ended, and every one of those rows lost to a merge that finished days ago.
// Observed live: a reopened workspace pushed `merged` for the rest of its life.
//
// THE MERGED FACT IS NOT WHAT IS CLEARED. `workspace_merged` (merged.go) is
// set-once and untouched here, and `WorkspaceState.merged_at_ms` keeps
// reporting the instant the merge landed. What is cleared is the AXIS — the
// claim that a merge is what this workspace is currently doing — which stopped
// being true the moment somebody opened it again.
//
// THE EDGE IS THE BRING-UP, and it is the only edge that could be. Reopening a
// workspace is exactly a new controller generation entering `connecting`; there
// is no other event that means "a human wants to use this workspace again". A
// timer, a heuristic over turn rows, or a frontend-issued clear would each be a
// second authority on the same question.

// supersedeMergedAxisOnReopen appends `merge_none` when workspace's newest
// merge-axis row is `merged`, retiring the axis so a reopened workspace resolves
// off its live session again.
//
// It runs inside the caller's connectivity transaction, so the bring-up edge and
// the axis retirement land together or not at all: a reopen that recorded the
// generation but not the retirement would leave the workspace merged-forever
// with nothing to try again.
//
// It reports whether it appended, purely so the caller can log the branch it
// took. A workspace resting on any OTHER merge token is left alone — a queued or
// conflicted merge is genuinely still in flight, and a bring-up is often exactly
// what the merge itself asked for.
func supersedeMergedAxisOnReopen(tx rowQueryExecer, workspace string, at int64) (bool, error) {
	token, found, err := newestMergeToken(tx, workspace)
	if err != nil {
		return false, err
	}
	if !found || token != sigMerged {
		return false, nil
	}
	if err := appendRow(tx, workspace, "", sigMergeNone, causeMergeReopened, sql.NullInt64{}, at, ""); err != nil {
		return false, err
	}
	return true, nil
}

// newestMergeToken reads the workspace's newest merge-axis token. It selects
// over the SAME token set resolveQuery's latest_merge CTE does, which is what
// makes "the newest merge row" mean one thing in the resolver and here.
func newestMergeToken(tx rowQueryExecer, workspace string) (string, bool, error) {
	query := `
SELECT state FROM workspace_state
WHERE workspace = ? AND state IN (` + mergeAxisPlaceholders() + `)
ORDER BY at DESC LIMIT 1`
	args := make([]any, 0, len(mergeAxisStates)+1)
	args = append(args, workspace)
	for _, s := range mergeAxisStates {
		args = append(args, s)
	}
	var token string
	err := tx.QueryRow(query, args...).Scan(&token)
	if err == sql.ErrNoRows {
		return "", false, nil
	}
	if err != nil {
		return "", false, fmt.Errorf("ssm: read newest merge token for workspace %q: %w", workspace, err)
	}
	return token, true, nil
}

// rowQueryExecer is the slice of *sql.Tx (and *sql.DB) this file needs, so the
// retirement can run inside the caller's transaction.
type rowQueryExecer interface {
	rowExecer
	QueryRow(query string, args ...any) *sql.Row
}
