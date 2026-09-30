package wsm

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"
	"time"

	"claude-repld/internal/dlog"
)

// THE WORKSPACE STATE DATABASE IS THE USER'S DATA, and it is carried FORWARD,
// never thrown away.
//
// This file exists because the opposite rule shipped and cost a person their
// whole workspace state: a purely additive layout change (one new table) took
// the layout stamp from 3 to 4, and the deployed daemon then refused every
// database written by the build before it — so the daemon exited at boot and
// nothing served their editor at all. A layout change that can be EXPRESSED as
// a migration is applied; only a layout this build genuinely cannot interpret
// is refused, and a refusal never rewrites the file it refused.
//
// A DOWNGRADE IS NOT A MIGRATION. A file stamped NEWER than this build was
// written by a build that knew things this one does not, so it is refused
// rather than "migrated" backwards into a shape that silently drops whatever
// the newer build recorded.

// migration is ONE step forward: the statements that turn a file stamped with
// To-1 into a file stamped with To. Steps are dense and ordered, so the list
// itself is the only place a layout version is introduced.
type migration struct {
	// To is the layout version the file carries once this step has committed.
	To int
	// Name says what the step does, for the record the open path writes.
	Name string
	// DDL is the whole step, run as one script inside the step's transaction.
	DDL string
}

// migrations is the ordered list of every step this build can apply, ascending
// by To. The last entry's To IS LayoutVersion; a build that bumps
// LayoutVersion without appending its step here refuses every older file,
// which is the defect this list exists to prevent.
//
// EACH STEP REUSES THE FRESH-FILE DDL rather than restating it, so a migrated
// file and a created one cannot drift into two different shapes.
var migrations = []migration{
	{To: 4, Name: "ported_prompts", DDL: portedPromptsDDL},
	{To: 5, Name: "host_session_identity_backfill", DDL: hostSessionIdentityBackfillDDL},
	{To: 6, Name: "creation_jobs_drop_one_shot_finish", DDL: creationJobsDropOneShotFinishDDL},
	{To: 7, Name: "sessions_selected_config_dir", DDL: sessionsSelectedConfigDirDDL},
	{To: 8, Name: "workspaces_spawned_shim_pid", DDL: spawnedShimPidDDL},
	{To: 9, Name: "workspaces_last_activity_at", DDL: lastActivityAtDDL},
	{To: 10, Name: "feed_text_scale", DDL: feedTextScaleDDL},
	{To: 11, Name: "idempotency_keys_accepted_at", DDL: acceptedAtDDL},
	{To: 12, Name: "held_prompts_delivery", DDL: heldPromptsDeliveryDDL},
	{To: 13, Name: "workspaces_turn_result", DDL: turnResultDDL},
	{To: 14, Name: "held_prompts_act_and_coalesced", DDL: heldPromptsActDDL},
}

// heldPromptsActDDL adds a held entry's SESSION ACT (a model or
// permission-mode change queued in order with the prompts around it) and
// whether later prompts were COALESCED into it. Every existing row is a
// prompt (NULL act) and was never coalesced (0), which is exactly right: no
// build before this one held an act durably or coalesced a prompt.
//
// Like the other column-add steps this ALTER cannot reuse the fresh-file DDL:
// the fresh-file table declares the columns inline in schema.go.
const heldPromptsActDDL = `
ALTER TABLE held_prompts ADD COLUMN act_kind TEXT;
ALTER TABLE held_prompts ADD COLUMN act_value TEXT;
ALTER TABLE held_prompts ADD COLUMN coalesced INTEGER NOT NULL DEFAULT 0;
`

// turnResultDDL adds the roster's last TURN RESULT: the turn-end arm the last
// turn resolved to and whether the user has seen it. The roster held both only
// in memory, so every daemon that did not see a workspace's turn end -- a
// successor after a deploy's handover, a restart -- drew every row `ready`,
// FULL, whatever it had been (owner report, 2026-09-29). Every existing row is
// NULL and unread, which reads as "no result stands": the row is drawn from
// its live state until its next turn ends.
//
// Like the other column-add steps this ALTER cannot reuse the fresh-file DDL:
// the fresh-file table declares the columns inline in schema.go.
const turnResultDDL = `
ALTER TABLE workspaces ADD COLUMN result_end TEXT;
ALTER TABLE workspaces ADD COLUMN result_read INTEGER NOT NULL DEFAULT 0;
`

// heldPromptsDeliveryDDL adds how a held prompt asked to be delivered
// (agentrepl.v1 SubmitPromptDelivery, as wsm.Delivery: 0 ordinary, 1
// deferred). Every existing row is 0, the ordinary
// delivery, by the column default -- exactly right: no build before this one
// could hold a prompt any other way.
//
// Like the other column-add steps this ALTER cannot reuse the fresh-file DDL:
// the fresh-file table declares the column inline in schema.go.
const heldPromptsDeliveryDDL = `
ALTER TABLE held_prompts ADD COLUMN delivery INTEGER NOT NULL DEFAULT 0;
`

// acceptedAtDDL adds when the prompt queue ACCEPTED an idempotency claim's
// submission. A claim was once final the instant it was written, BEFORE the
// queue took the submission, so a submission whose queue call hung, errored or
// died with its process left a key that refused every retry as a duplicate of
// a turn that was never delivered -- and the client, told "the earlier
// submission stands", dropped the prompt.
//
// THE BACKFILL STAMPS A CLAIM ONLY ON DURABLE EVIDENCE THE QUEUE TOOK IT: a
// turn row (written on the delivery path) or a held-prompt row (a hold, which
// is never deleted, only tombstoned) under the claimed turn. A claim with
// neither never reached the queue's first durable write, so it stays NULL and
// a retry of it is re-driven, which is exactly the rule this build enforces.
//
// Like the other column-add steps this ALTER cannot reuse the fresh-file DDL:
// the fresh-file table declares the column inline in schema.go.
const acceptedAtDDL = `
ALTER TABLE idempotency_keys ADD COLUMN accepted_at INTEGER;
UPDATE idempotency_keys SET accepted_at = claimed_at
WHERE turn_id IN (SELECT id FROM turns) OR turn_id IN (SELECT turn_id FROM held_prompts);
`

// lastActivityAtDDL adds when a workspace LAST DID REAL WORK — the roster
// when-column's new source, stamped inside the turn writes at the genuine
// activity edge. Every existing row is NULL, which reads exactly as "this
// workspace has never taken a turn": the when-column falls back to the
// creation time for such a row, and the first turn it takes stamps a real
// instant. It is a WORKSPACE fact, not a session one — a registered workspace
// that has never had a session still has a creation time and can still take a
// turn — so it is a workspace column beside last_selected_at and merged_at.
//
// Like the other column-add steps this ALTER cannot reuse the fresh-file DDL:
// the fresh-file table declares the column inline in schema.go. One meaning,
// two spellings that must not drift, which is why both name the same column
// with the same type.
const lastActivityAtDDL = `
ALTER TABLE workspaces ADD COLUMN last_activity_at INTEGER;
`

// spawnedShimPidDDL adds the pid of the shim a daemon SPAWNED for a workspace.
//
// It is a WORKSPACE fact and not a session one, which is the whole reason it
// is a new column rather than a second writer of sessions.shim_pid: a
// registered workspace that has never had a session has NO session row at all,
// and the pid must be durable from the instant of the fork -- long before any
// StartSession could file one. Every existing row is NULL, which reads exactly
// as "no daemon has a spawn outstanding for this workspace", the same thing a
// cleared value means.
const spawnedShimPidDDL = `
ALTER TABLE workspaces ADD COLUMN spawned_shim_pid INTEGER;
`

// sessionsSelectedConfigDirDDL adds the root the USER CHOSE for a workspace.
// Every existing row is stamped ” by the column default, which is exactly
// right: nobody had chosen anything before this build, so every one of them
// keeps following the path routing.
const sessionsSelectedConfigDirDDL = `
ALTER TABLE sessions ADD COLUMN selected_config_dir TEXT NOT NULL DEFAULT '';
`

// creationJobsDropOneShotFinishDDL retires the creation job's recorded ONE-SHOT
// FINISH ACTION. A one-shot no longer has one (owner ruling, 2026-09-12): what
// happens on completion is the repository's own directive, appended to the
// commission and carried out by the agent, so the daemon performs no finish and
// has nothing to record or spend.
//
// Like the layout-5 step this one introduces NO SHAPE, so there is no
// fresh-file DDL for it to reuse — the fresh-file table simply no longer
// declares the column. Dropping it is lossless in the only sense that matters:
// nothing left in this build reads the value, and a recorded finish this build
// would decline to take is worse kept than dropped.

// hostSessionIdentityBackfillDDL heals the DURABLE RESIDUE of a build that
// filed a session row before minting its host identity: the creation path
// recorded the spawn facts the fleet reads at bring-up, and left
// host_session_id empty until a session actually came up. A workspace whose
// bring-up never succeeded kept that row forever, and the host view is
// WITHHELD for a row with no identity — so every compose for that workspace
// recorded the missing-identity ERROR again, for the life of the file.
//
// Minting the identity where the session is created stops new rows from
// arriving in that state; it does nothing for the ones already filed. This
// step is the one-shot catch-up over that backlog: it is summarized ONCE by
// the migration record the open path writes, and anything that arises AFTER
// it is a genuine defect that keeps the compose ERROR.
//
// The id is minted in SQL as sixteen lowercase hex characters, which is
// exactly the shape NewHostSessionID mints (eight random bytes, hex-encoded).
// randomblob is evaluated per row, so no two rows are healed to the same id.
//
// Unlike the layout-4 step this one introduces NO SHAPE, so there is no
// fresh-file DDL for it to reuse: a file created by this build has no session
// rows at all, and every row it later writes goes through PutSession, which
// refuses one with no identity.
const hostSessionIdentityBackfillDDL = `
UPDATE sessions SET host_session_id = lower(hex(randomblob(8)))
WHERE host_session_id IS NULL OR host_session_id = '';
`

const creationJobsDropOneShotFinishDDL = `
ALTER TABLE creation_jobs DROP COLUMN one_shot_finish;
`

// planMigrations answers the steps that carry a file stamped with from up to
// this build's LayoutVersion, and whether such a chain exists at all. A gap in
// the list — a file so old that no step starts where it stands — is NOT a
// migration, and the caller refuses it rather than applying a partial chain.
func planMigrations(from int) ([]migration, bool) {
	if from >= LayoutVersion {
		return nil, false
	}
	want := from + 1
	var plan []migration
	for _, m := range migrations {
		if m.To < want {
			continue
		}
		if m.To != want {
			// A hole in the chain: this build cannot get from `from` to here.
			return nil, false
		}
		plan = append(plan, m)
		want++
		if m.To == LayoutVersion {
			break
		}
	}
	if want != LayoutVersion+1 {
		return nil, false
	}
	return plan, true
}

// migrateForward carries an existing file from its stamped layout up to this
// build's, one transaction per step, and records which steps ran.
//
// The file is COPIED ASIDE first, so a migration that fails halfway through a
// step leaves both the original file (rolled back, untouched) and a copy of it
// as it stood before any step ran. Every refusal here names that copy.
func (s *store) migrateForward(ctx context.Context, from int) error {
	const op = "daemon.wsm.open"
	plan, ok := planMigrations(from)
	if !ok {
		refusal := &LayoutError{
			Path:   s.path,
			File:   from,
			Binary: LayoutVersion,
			Reason: "no migration in this build leads from it to this build's layout",
		}
		s.log.Error(op, "refused a state database this build cannot migrate", dlog.Context{
			"path": s.path, "file_layout": from, "binary_layout": LayoutVersion, "error": refusal.Error(),
		})
		return refusal
	}
	backup, err := s.copyAsideBeforeMigrating(ctx, from)
	if err != nil {
		s.log.Error(op, "the state database could not be copied aside before migrating", dlog.Context{
			"path": s.path, "file_layout": from, "error": err.Error(),
		})
		return err
	}
	applied := make([]string, 0, len(plan))
	for _, m := range plan {
		if err := s.applyMigration(ctx, m); err != nil {
			refusal := &MigrationError{Path: s.path, From: from, To: m.To, Backup: backup, Err: err}
			s.log.Error(op, "a state database migration was rolled back", dlog.Context{
				"path": s.path, "file_layout": from, "step_layout": m.To,
				"migration": m.Name, "backup": backup, "error": refusal.Error(),
			})
			return refusal
		}
		applied = append(applied, fmt.Sprintf("%d:%s", m.To, m.Name))
	}
	s.log.Info(op, "migrated the state database forward", dlog.Context{
		"path": s.path, "file_layout": from, "binary_layout": LayoutVersion,
		"migrations": strings.Join(applied, ","), "backup": backup,
	})
	return nil
}

// applyMigration runs one step's DDL and its version stamp inside ONE
// transaction, so a step that fails partway leaves the file exactly as it was:
// a half-applied step that still carried the OLD stamp would be re-applied on
// the next boot, and one that carried the NEW stamp would hide missing tables
// behind a version that claims they exist.
func (s *store) applyMigration(ctx context.Context, m migration) error {
	tx, err := s.handle.BeginTx(ctx, nil)
	if err != nil {
		return fmt.Errorf("wsm: begin migration to layout %d on %q: %w", m.To, s.path, err)
	}
	defer s.endTx(tx, "daemon.wsm.open", dlog.Context{"path": s.path, "step_layout": m.To, "migration": m.Name})
	if _, err := tx.ExecContext(ctx, m.DDL); err != nil {
		return fmt.Errorf("wsm: apply migration %q to layout %d in %q: %w", m.Name, m.To, s.path, err)
	}
	res, err := tx.ExecContext(ctx, `UPDATE layout SET version = ? WHERE id = 1`, m.To)
	if err != nil {
		return fmt.Errorf("wsm: stamp layout %d in %q: %w", m.To, s.path, err)
	}
	rows, err := res.RowsAffected()
	if err != nil {
		return fmt.Errorf("wsm: stamp layout %d in %q: rows affected: %w", m.To, s.path, err)
	}
	if rows != 1 {
		return fmt.Errorf("wsm: stamp layout %d in %q touched %d rows, want exactly 1", m.To, s.path, rows)
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("wsm: commit migration to layout %d on %q: %w", m.To, s.path, err)
	}
	return nil
}

// copyAsideBeforeMigrating writes a consistent snapshot of the file as it
// stands, named for the layout it carries, and answers where it went.
//
// `VACUUM INTO` is the copy rather than a byte-for-byte file copy because this
// database runs in WAL mode: the committed state lives across the file AND its
// write-ahead log, so copying the main file alone can produce a snapshot that
// is missing the most recent commits.
//
// A copy from an EARLIER failed attempt stands rather than being overwritten:
// the source is unchanged between attempts (a failed migration rolls back), so
// the standing copy already holds the same content, and the user's file is
// never deleted by a retry.
func (s *store) copyAsideBeforeMigrating(ctx context.Context, from int) (string, error) {
	path := fmt.Sprintf("%s.layout%d.bak-%s", s.path, from, time.Now().UTC().Format("2006-01-02"))
	switch _, err := os.Stat(path); {
	case err == nil:
		s.log.Warn("daemon.wsm.open", "a pre-migration copy from an earlier attempt already stands", dlog.Context{
			"path": s.path, "backup": path, "file_layout": from,
		})
		return path, nil
	case !errors.Is(err, os.ErrNotExist):
		return "", fmt.Errorf("wsm: inspect the pre-migration copy %q: %w", path, err)
	}
	if _, err := s.handle.ExecContext(ctx, `VACUUM INTO `+quoteSQLText(path)); err != nil {
		return "", fmt.Errorf("wsm: copy %q aside as %q before migrating: %w", s.path, path, err)
	}
	return path, nil
}

// quoteSQLText renders a string as a SQL literal. It exists because `VACUUM
// INTO` names its destination in the statement itself and takes no bound
// parameter.
func quoteSQLText(s string) string {
	return "'" + strings.ReplaceAll(s, "'", "''") + "'"
}
