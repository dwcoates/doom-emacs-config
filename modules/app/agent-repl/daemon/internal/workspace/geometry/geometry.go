// Package geometry owns THE daemon's workspace -> merge-geometry map.
//
// A merge needs three coordinates, and until the cutover Emacs computed all
// three and rode them on every merge_workspace command. Two owners of one map
// is how a merge landed against a target the daemon had never heard of, so the
// map now has exactly one owner: the daemon records the coordinates at the
// moment it creates the worktree (when it knows all three as facts, not as
// guesses), and resolves them from that record when a merge arrives.
//
// The three coordinates:
//
//   - SourceBranch — the branch whose commits are cherry-picked.
//   - SourceDir    — the workspace's own worktree.
//   - TargetDir    — the worktree the commits land in.
//
// The record lives in THE shared state store (internal/statedb), beside the
// session registry and the session-state manager's log, because it is durable
// daemon-owned identity of exactly their kind: it must survive the daemon
// bounce a self-merge causes, and it must be readable in the same transaction
// discipline as everything else keyed by workspace.
//
// A workspace with no record is NEVER guessed at. The merge is refused with an
// explanation naming the workspace, because a merge run against a fabricated
// target writes commits into a repository nobody asked for.
package geometry

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"path/filepath"
	"strings"

	"claude-repld/internal/dlog"
)

// Origin records HOW a geometry record was obtained. It is diagnostic, and it
// is also the precedence rule: a fact observed at creation outranks one derived
// after the fact from the git state a pre-cutover workspace happens to be in.
type Origin string

const (
	// OriginCreated: recorded by the daemon at the moment it created the
	// worktree. Authoritative.
	OriginCreated Origin = "created"
	// OriginBackfilled: derived at boot from git facts for a workspace that
	// predates the cutover. Correct as far as git can tell, but it cannot know
	// which worktree the workspace was SPAWNED from — only which worktree is
	// the repository's main one.
	OriginBackfilled Origin = "backfilled"
)

// Record is one workspace's complete merge geometry.
type Record struct {
	// Workspace is the daemon's workspace KEY — the session cwd, the same
	// string merge.Request.Workspace and every SSM row are keyed on.
	Workspace string
	// SourceBranch is the branch whose commits are cherry-picked.
	SourceBranch string
	// SourceDir is the workspace's own worktree.
	SourceDir string
	// TargetDir is the worktree the cherry-pick lands in.
	TargetDir string
	// THE `before_ws_merge` PROMPT IS NOT HERE, deliberately. It is a field of
	// the create Request, and the merge reads it back from the creation records
	// through create.BeforeWSMergePromptFor — the ONE accessor over the ONE store
	// that fact is written into. This record used to carry a second copy of it,
	// written here and read nowhere, which is precisely how a writer and a reader
	// came to name two different columns. Do not reintroduce it.
	//
	// Origin is how the record was obtained; see Origin.
	Origin Origin
}

func (r Record) validate() error {
	switch {
	case r.Workspace == "":
		return fmt.Errorf("geometry: record Workspace is required")
	case r.SourceBranch == "":
		return fmt.Errorf("geometry: record %s SourceBranch is required", r.Workspace)
	case r.SourceDir == "":
		return fmt.Errorf("geometry: record %s SourceDir is required", r.Workspace)
	case r.TargetDir == "":
		return fmt.Errorf("geometry: record %s TargetDir is required", r.Workspace)
	case r.Origin != OriginCreated && r.Origin != OriginBackfilled:
		return fmt.Errorf("geometry: record %s has unknown origin %q", r.Workspace, r.Origin)
	case Key(r.SourceDir) == Key(r.TargetDir):
		// A cherry-pick from a worktree into itself is not a merge; it is a
		// no-op that reports success. Refuse to record one at all.
		return fmt.Errorf("geometry: record %s has SourceDir == TargetDir (%s); a workspace cannot merge into itself", r.Workspace, r.SourceDir)
	}
	return nil
}

// Key normalizes a workspace path into the store's key spelling. Every write
// and every lookup goes through it, so "/x/y" and "/x//y/." are one key rather
// than two records nobody can reconcile.
func Key(path string) string {
	if path == "" {
		return ""
	}
	return filepath.Clean(path)
}

// Store is the durable workspace -> geometry map, backed by the shared state
// database.
type Store struct {
	db   *sql.DB
	logf dlog.Logf
}

// Open migrates the geometry table onto the shared state store and returns the
// store. It touches only its OWN table, exactly as the registry and the SSM do
// on the same database.
func Open(db *sql.DB, logf dlog.Logf) (*Store, error) {
	if db == nil {
		return nil, fmt.Errorf("geometry: store needs a state database")
	}
	if logf == nil {
		// A geometry decision that cannot be logged makes an unexplainable
		// merge refusal undiagnosable from the shared log.
		return nil, fmt.Errorf("geometry: store needs a Logf")
	}
	if _, err := db.Exec(`
		CREATE TABLE IF NOT EXISTS workspace_merge_geometry (
			workspace     TEXT PRIMARY KEY,
			source_branch TEXT NOT NULL,
			source_dir    TEXT NOT NULL,
			target_dir    TEXT NOT NULL,
			origin        TEXT NOT NULL
		);
	`); err != nil {
		return nil, fmt.Errorf("geometry: create schema: %w", err)
	}
	// `before_action` is a RETIRED column. It once held a second copy of the
	// workspace's `before_ws_merge` prompt; the merge now reads that prompt only
	// from the creation records (create.BeforeWSMergePromptFor), so nothing writes
	// or reads this column any more.
	//
	// The migration stays because the history stays: databases in the field
	// already have the column, and it is NOT NULL, so every daemon that opens one
	// must find it — dropping the ALTER would leave a fresh database and an
	// existing one with two different table shapes. It is additive and idempotent
	// (a duplicate-column error is the migration having already run, which is the
	// only reading of it), and the NOT NULL DEFAULT '' is what lets the writes
	// below omit the column entirely.
	if _, err := db.Exec(`ALTER TABLE workspace_merge_geometry ADD COLUMN before_action TEXT NOT NULL DEFAULT ''`); err != nil &&
		!strings.Contains(err.Error(), "duplicate column name") {
		return nil, fmt.Errorf("geometry: add retired before_action column: %w", err)
	}
	return &Store{db: db, logf: logf}, nil
}

// ErrGeometryConflict marks a refusal to overwrite an existing record with a
// DERIVED one that disagrees with it. The derived answer is the weaker fact, so
// it loses — loudly, never by silently keeping either.
var ErrGeometryConflict = errors.New("geometry: derived geometry disagrees with the recorded geometry")

// Record durably records one workspace's geometry.
//
// It is idempotent by design: workspace creation checkpoints its worktree stage
// before and after this call, so a resumed job re-records the same three facts.
//
// Precedence when a record already exists and DISAGREES:
//
//   - an OriginCreated record replaces it, loudly. The daemon just built that
//     worktree; whatever was there is stale.
//   - an OriginBackfilled record is refused with ErrGeometryConflict. A guess
//     derived from today's git state must never silently displace the fact
//     recorded when the worktree was made.
func (s *Store) Record(ctx context.Context, rec Record) error {
	rec.Workspace = Key(rec.Workspace)
	rec.SourceDir = Key(rec.SourceDir)
	rec.TargetDir = Key(rec.TargetDir)
	if err := rec.validate(); err != nil {
		s.logf("geometry: record REFUSED {workspace=%s branch=%q source=%s target=%s origin=%s}: %v",
			rec.Workspace, rec.SourceBranch, rec.SourceDir, rec.TargetDir, rec.Origin, err)
		return err
	}
	existing, found, err := s.Lookup(ctx, rec.Workspace)
	if err != nil {
		return err
	}
	if found {
		same := existing.SourceBranch == rec.SourceBranch && existing.SourceDir == rec.SourceDir && existing.TargetDir == rec.TargetDir
		if same {
			if existing.Origin == rec.Origin {
				s.logf("geometry: record UNCHANGED {workspace=%s branch=%q source=%s target=%s origin=%s}",
					rec.Workspace, rec.SourceBranch, rec.SourceDir, rec.TargetDir, rec.Origin)
				return nil
			}
			s.logf("geometry: record ORIGIN UPGRADED {workspace=%s branch=%q was=%s now=%s}",
				rec.Workspace, rec.SourceBranch, existing.Origin, rec.Origin)
		} else if rec.Origin == OriginCreated {
			s.logf("geometry: record REPLACED {workspace=%s old_branch=%q old_source=%s old_target=%s old_origin=%s new_branch=%q new_source=%s new_target=%s}",
				rec.Workspace, existing.SourceBranch, existing.SourceDir, existing.TargetDir, existing.Origin,
				rec.SourceBranch, rec.SourceDir, rec.TargetDir)
		} else {
			s.logf("geometry: record CONFLICT {workspace=%s recorded_branch=%q recorded_source=%s recorded_target=%s recorded_origin=%s derived_branch=%q derived_source=%s derived_target=%s}",
				rec.Workspace, existing.SourceBranch, existing.SourceDir, existing.TargetDir, existing.Origin,
				rec.SourceBranch, rec.SourceDir, rec.TargetDir)
			return fmt.Errorf("%w: workspace %s is recorded as branch=%q source=%s target=%s (origin %s) but was derived as branch=%q source=%s target=%s",
				ErrGeometryConflict, rec.Workspace, existing.SourceBranch, existing.SourceDir, existing.TargetDir, existing.Origin,
				rec.SourceBranch, rec.SourceDir, rec.TargetDir)
		}
	}
	if _, err := s.db.ExecContext(ctx, `
		INSERT INTO workspace_merge_geometry(workspace, source_branch, source_dir, target_dir, origin)
		VALUES (?, ?, ?, ?, ?)
		ON CONFLICT(workspace) DO UPDATE SET
			source_branch = excluded.source_branch,
			source_dir    = excluded.source_dir,
			target_dir    = excluded.target_dir,
			origin        = excluded.origin
	`, rec.Workspace, rec.SourceBranch, rec.SourceDir, rec.TargetDir, string(rec.Origin)); err != nil {
		s.logf("geometry: record WRITE FAILED {workspace=%s origin=%s}: %v", rec.Workspace, rec.Origin, err)
		return fmt.Errorf("geometry: record %s: %w", rec.Workspace, err)
	}
	s.logf("geometry: RECORDED {workspace=%s branch=%q source=%s target=%s origin=%s}",
		rec.Workspace, rec.SourceBranch, rec.SourceDir, rec.TargetDir, rec.Origin)
	return nil
}

// Lookup returns the workspace's recorded geometry. A missing record is
// reported as found=false and NEVER as a synthesized answer: the caller's
// correct move is to refuse the merge with an explanation.
func (s *Store) Lookup(ctx context.Context, workspace string) (Record, bool, error) {
	key := Key(workspace)
	if key == "" {
		return Record{}, false, fmt.Errorf("geometry: lookup needs a workspace key")
	}
	rec := Record{Workspace: key}
	var origin string
	err := s.db.QueryRowContext(ctx, `
		SELECT source_branch, source_dir, target_dir, origin
		FROM workspace_merge_geometry WHERE workspace = ?
	`, key).Scan(&rec.SourceBranch, &rec.SourceDir, &rec.TargetDir, &origin)
	if errors.Is(err, sql.ErrNoRows) {
		return Record{}, false, nil
	}
	if err != nil {
		s.logf("geometry: lookup FAILED {workspace=%s}: %v", key, err)
		return Record{}, false, fmt.Errorf("geometry: lookup %s: %w", key, err)
	}
	rec.Origin = Origin(origin)
	return rec, true, nil
}
