package wsm

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"agent-repld/internal/dlog"
)

// THE GITHUB FAILURE NOTICES ARE RETIRED (owner ruling, 2026-11-10): a read
// the daemon could not make is told to no workspace (prwatch). Notices an
// older build composed of nothing but such sentences, and submitted, may
// still wait undelivered; delivered, or carried into the next notice, they
// would tell a workspace what no build tells any more. The layout-119 rewrite
// drops each one whose every sentence is a retired failure sentence, and
// leaves any notice that also carries real activity as it is.

// retiredFailurePrefixes begin every sentence the retired failure notice
// composed (prwatch's failureSentence, removed).
var retiredFailurePrefixes = []string{
	"The daemon can no longer read this ",
	"The daemon's reads of this ",
}

// onlyRetiredFailures reports whether SENTENCES are all retired failure
// sentences, and there is at least one.
func onlyRetiredFailures(sentences []string) bool {
	if len(sentences) == 0 {
		return false
	}
	for _, sentence := range sentences {
		retired := false
		for _, prefix := range retiredFailurePrefixes {
			if strings.HasPrefix(sentence, prefix) {
				retired = true
				break
			}
		}
		if !retired {
			return false
		}
	}
	return true
}

// retireGithubFailureNotices is the layout-119 rewrite: every standing
// notice, of a pull request or a GitHub merge queue, made only of retired
// failure sentences is dropped, its held prompt tombstoned and its untold
// row deleted, so it is neither delivered nor carried.
func retireGithubFailureNotices(ctx context.Context, tx *sql.Tx, log dlog.Logger) error {
	now := nanos(time.Now())
	dropped := 0
	for _, table := range []string{"pull_request_untold_notices", "github_merge_queue_untold_notices"} {
		rows, err := tx.QueryContext(ctx, `SELECT n.rowid, n.turn_id, n.sentences FROM `+table+` AS n
			JOIN held_prompts AS h ON h.turn_id = n.turn_id WHERE h.tombstone_kind IS NULL`)
		if err != nil {
			return err
		}
		type retiring struct {
			rowid int64
			turn  string
		}
		var retire []retiring
		for rows.Next() {
			var (
				rowid     int64
				turn, raw string
			)
			if err := rows.Scan(&rowid, &turn, &raw); err != nil {
				rows.Close()
				return err
			}
			var sentences []string
			if err := json.Unmarshal([]byte(raw), &sentences); err != nil {
				rows.Close()
				return fmt.Errorf("wsm: %s row %d holds unreadable sentences: %w", table, rowid, err)
			}
			if onlyRetiredFailures(sentences) {
				retire = append(retire, retiring{rowid: rowid, turn: turn})
			}
		}
		if err := rows.Close(); err != nil {
			return err
		}
		for _, r := range retire {
			if _, err := tx.ExecContext(ctx, `UPDATE held_prompts SET tombstone_kind = 'dropped', tombstone_at = ? WHERE turn_id = ?`, now, r.turn); err != nil {
				return err
			}
			if _, err := tx.ExecContext(ctx, `DELETE FROM `+table+` WHERE rowid = ?`, r.rowid); err != nil {
				return err
			}
		}
		dropped += len(retire)
	}
	log.Info("daemon.wsm.open", "dropped the waiting notices made only of retired GitHub failure sentences", dlog.Context{"dropped": dropped})
	return nil
}
