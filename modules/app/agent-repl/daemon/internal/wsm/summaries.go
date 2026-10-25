package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
)

// THE WORKSPACE SUMMARY the topbar draws (internal/titlesynth), kept so a
// daemon restart does not drop every workspace back to its bare name until its
// session comes up and a new summary is distilled.
//
// THE SUMMARY AND ITS CADENCE ARE ONE RECORD. The synthesizer decides whether a
// digest is due a model call from the digest it last summarized (its hash, its
// prompt count, its boundary). Keeping the title without that state would make
// every restart a "first title" and spend a model call per workspace on a
// summary that was already standing; keeping it in the same row makes the two
// unable to disagree.
//
// A /clear KEEPS THE TITLE AND FORGETS THE DIGEST (ResetWorkspaceSummaryDigest):
// the title stands until the next distillation replaces it, and a NULL digest
// is what makes that next trigger due at once, across a restart as within one.

// workspaceSummariesDDL is the layout-56 addition. A new table, so the step is
// additive: the build before it never names it. The fresh-file schema appends
// this same text (schema.go), so a migrated file and a created one cannot
// differ.
const workspaceSummariesDDL = `
CREATE TABLE workspace_summaries (
  workspace_id    TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  title           TEXT NOT NULL,
  -- The digest the title was distilled from. All three are NULL together once
  -- a /clear has reset the cadence, and set together otherwise.
  digest_hash     TEXT,
  digest_prompts  INTEGER,
  digest_boundary INTEGER
);
`

// WorkspaceSummary is one workspace's synthesized summary and the digest it
// was distilled from.
type WorkspaceSummary struct {
	// Title is the summary the topbar draws. Never empty: an empty summary is
	// a retraction, which deletes the row.
	Title string
	// Digest is the digest Title was distilled from, or nil once a /clear has
	// reset the cadence and the next trigger is due a new summary at once.
	Digest *SummaryDigest
}

// SummaryDigest is the cadence state of one distillation.
type SummaryDigest struct {
	// Hash fingerprints the digest's boundary and prompts.
	Hash string
	// Prompts is how many prompts the digest held.
	Prompts int
	// Boundary is the context boundary the digest was measured from.
	Boundary shimv1.TitleDigestBoundary
}

// validSummaryBoundary reports whether b is a boundary a digest is measured
// from. UNSPECIFIED is a malformed frame, and anything else is no boundary the
// shim states.
func validSummaryBoundary(b shimv1.TitleDigestBoundary) bool {
	return b == shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE ||
		b == shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_CLEAR
}

// validateSummary refuses a summary no distillation produces.
func validateSummary(sum WorkspaceSummary) error {
	if sum.Title == "" {
		return errors.New("a summary has a title; an empty one is a retraction")
	}
	if d := sum.Digest; d != nil {
		if d.Hash == "" || d.Prompts < 0 || !validSummaryBoundary(d.Boundary) {
			return fmt.Errorf("a summary's digest has a hash, a non-negative prompt count and a known boundary, got %+v", *d)
		}
	}
	return nil
}

// PutWorkspaceSummary records a workspace's summary, replacing any it had. An
// unknown workspace is ErrNotFound: a summary distilled for a workspace
// forgotten mid-flight has nothing to belong to. A summary no distillation
// produces is a defect in the caller and is refused rather than stored.
func (s *store) PutWorkspaceSummary(ctx context.Context, id WorkspaceID, sum WorkspaceSummary) error {
	const op = "daemon.wsm.put_workspace_summary"
	fields := dlog.Context{"workspace": string(id), "title": sum.Title, "reset": sum.Digest == nil}
	if err := validateSummary(sum); err != nil {
		err = fmt.Errorf("wsm: workspace summary of %s: %w", id, err)
		s.log.Error(op, "refused a malformed workspace summary", withError(fields, err))
		return err
	}
	var (
		hash     sql.NullString
		prompts  sql.NullInt64
		boundary sql.NullInt64
	)
	if d := sum.Digest; d != nil {
		hash = sql.NullString{String: d.Hash, Valid: true}
		prompts = sql.NullInt64{Int64: int64(d.Prompts), Valid: true}
		boundary = sql.NullInt64{Int64: int64(d.Boundary), Valid: true}
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var existing string
		if err := scanWorkspaceRow(ctx, tx, id, "id", &existing); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx,
			`INSERT INTO workspace_summaries (workspace_id, title, digest_hash, digest_prompts, digest_boundary) VALUES (?, ?, ?, ?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET
			   title = excluded.title, digest_hash = excluded.digest_hash,
			   digest_prompts = excluded.digest_prompts, digest_boundary = excluded.digest_boundary`,
			id, sum.Title, hash, prompts, boundary)
		return err
	})
}

// ResetWorkspaceSummaryDigest forgets the digest a workspace's summary was
// distilled from, keeping the title: a /clear leaves the summary standing
// until the next distillation, which the reset makes due at once. A workspace
// with no summary has no digest to forget, which is not an error.
func (s *store) ResetWorkspaceSummaryDigest(ctx context.Context, id WorkspaceID) error {
	return s.write(ctx, "daemon.wsm.reset_workspace_summary_digest", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`UPDATE workspace_summaries SET digest_hash = NULL, digest_prompts = NULL, digest_boundary = NULL WHERE workspace_id = ?`, id)
		return err
	})
}

// DropWorkspaceSummary deletes a workspace's summary: the conversation it
// described is gone. A workspace with no summary is not an error.
func (s *store) DropWorkspaceSummary(ctx context.Context, id WorkspaceID) error {
	return s.write(ctx, "daemon.wsm.drop_workspace_summary", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `DELETE FROM workspace_summaries WHERE workspace_id = ?`, id)
		return err
	})
}

// WorkspaceSummaryOf loads a workspace's summary; the bool is false when none
// stands. A stored row no distillation produces is a DecodeError.
func (s *store) WorkspaceSummaryOf(ctx context.Context, id WorkspaceID) (WorkspaceSummary, bool, error) {
	var (
		out   WorkspaceSummary
		found bool
	)
	err := s.read(ctx, "daemon.wsm.workspace_summary", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var (
			title    string
			hash     sql.NullString
			prompts  sql.NullInt64
			boundary sql.NullInt64
		)
		err := s.q(ctx).QueryRowContext(ctx,
			`SELECT title, digest_hash, digest_prompts, digest_boundary FROM workspace_summaries WHERE workspace_id = ?`, id).
			Scan(&title, &hash, &prompts, &boundary)
		if errors.Is(err, sql.ErrNoRows) {
			out, found = WorkspaceSummary{}, false
			return nil
		}
		if err != nil {
			return err
		}
		sum, err := decodeSummary(title, hash, prompts, boundary)
		if err != nil {
			return &DecodeError{Table: "workspace_summaries", Row: string(id), Err: err}
		}
		out, found = sum, true
		return nil
	})
	if err != nil {
		return WorkspaceSummary{}, false, err
	}
	return out, found, nil
}

// decodeSummary reads one stored row: the digest columns are all NULL or all
// set, and the whole record passes the same validation a put does.
func decodeSummary(title string, hash sql.NullString, prompts, boundary sql.NullInt64) (WorkspaceSummary, error) {
	sum := WorkspaceSummary{Title: title}
	switch {
	case !hash.Valid && !prompts.Valid && !boundary.Valid:
	case hash.Valid && prompts.Valid && boundary.Valid:
		if boundary.Int64 < 0 || boundary.Int64 > int64(^uint32(0)>>1) {
			return WorkspaceSummary{}, fmt.Errorf("a stored boundary is an enum value, got %d", boundary.Int64)
		}
		sum.Digest = &SummaryDigest{
			Hash:     hash.String,
			Prompts:  int(prompts.Int64),
			Boundary: shimv1.TitleDigestBoundary(boundary.Int64),
		}
	default:
		return WorkspaceSummary{}, errors.New("a stored digest is all NULL or all set")
	}
	if err := validateSummary(sum); err != nil {
		return WorkspaceSummary{}, err
	}
	return sum, nil
}
