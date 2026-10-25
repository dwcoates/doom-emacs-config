package wsm

import (
	"context"
	"database/sql"
	"errors"

	"agent-repld/internal/dlog"
)

// sidebarViewDDL is the layout-23 addition (docs/protobuf-design/
// sidebar-folds-global.md): the sidebar's DURABLE view state, which the owner
// ruled is no workspace's own (2026-10-06) — every workspace's page draws the
// same sidebar. A repository's fold already lives on its row
// (repositoriesFoldedDDL); this adds a task's fold on its row and the two
// view facts that belong to no row: the recently-merged band's fold and which
// grouping is shown.
//
// Every existing task is expanded (0), which is how every task section drew
// by default. sidebar_view is a SINGLETON like feed_text_scale: the CHECK
// (id = 1) makes "one view" structural, and an ABSENT row is the view nobody
// has changed, which is DefaultSidebarView.
//
// Like the other column-add steps it is an ALTER, applied after the tasks
// table on a fresh file and as its own step on a migrated one.
const sidebarViewDDL = `
ALTER TABLE tasks ADD COLUMN folded INTEGER NOT NULL DEFAULT 0;

CREATE TABLE sidebar_view (
  id            INTEGER PRIMARY KEY CHECK (id = 1),
  merged_folded INTEGER NOT NULL,
  grouping      TEXT NOT NULL
);
`

// maturityFoldsDDL is the layout-29 addition: each maturity stage's section
// fold, for the stage grouping retired on 2026-10-15. Nothing reads or writes
// these columns any more (retiredGroupingColumn).
//
// Like the other column-add steps it is an ALTER, applied after sidebar_view
// on a fresh file and as its own step on a migrated one.
const maturityFoldsDDL = `
ALTER TABLE sidebar_view ADD COLUMN backlog_folded INTEGER NOT NULL DEFAULT 0;
ALTER TABLE sidebar_view ADD COLUMN in_progress_folded INTEGER NOT NULL DEFAULT 0;
ALTER TABLE sidebar_view ADD COLUMN merging_folded INTEGER NOT NULL DEFAULT 0;
`

// hiddenWorkspacesDDL is the layout-32 addition: whether each workspace sits
// in the roster's hidden section (frontend.v1.WorkspaceRoster.hidden), and
// that section's fold on the sidebar_view singleton beside the merged band's.
// Every existing workspace is shown (0) and the section is expanded (0) until
// someone changes either.
//
// Like the other column-add steps it is an ALTER, applied after sidebar_view
// on a fresh file and as its own step on a migrated one.
const hiddenWorkspacesDDL = `
ALTER TABLE workspaces ADD COLUMN hidden INTEGER NOT NULL DEFAULT 0;
ALTER TABLE sidebar_view ADD COLUMN hidden_folded INTEGER NOT NULL DEFAULT 0;
`

// openFoldedDDL is the layout-46 addition: the open-workspaces section's fold
// (frontend.v1.WorkspaceRoster.open) on the sidebar_view singleton beside the
// hidden section's. The section is expanded (0) until someone folds it.
//
// Like the other column-add steps it is an ALTER, applied after sidebar_view
// on a fresh file and as its own step on a migrated one.
const openFoldedDDL = `
ALTER TABLE sidebar_view ADD COLUMN open_folded INTEGER NOT NULL DEFAULT 0;
`

// hiddenAndDweltAtDDL is the layout-43 addition (owner requests, 2026-10-13):
//
//   - workspaces.hidden_at, when the workspace was moved into the hidden
//     section, set exactly while `hidden` is: the hidden band orders by it,
//     newest first, and its rows' when-column counts from it. A row already
//     hidden when the column lands has no recorded instant, so the step
//     stamps it with the migration's own instant, the earliest moment this
//     build can vouch for: from here on `hidden` and `hidden_at` stand and
//     fall together.
//   - workspaces.dwelt_at, the last instant the user LEFT the workspace after
//     looking at it for five seconds, which hiding the workspace the user
//     stood on once landed by. RETIRED 2026-10-14: hiding now lands on the
//     tab bar's neighbour, and no build reads or writes the column; a later
//     breaking step drops it (MigrationKind's two deploys).
//
// IT IS ADDITIVE: the previous build names neither column.
const hiddenAndDweltAtDDL = `
ALTER TABLE workspaces ADD COLUMN hidden_at INTEGER;
ALTER TABLE workspaces ADD COLUMN dwelt_at INTEGER;
UPDATE workspaces SET hidden_at = CAST(strftime('%s', 'now') AS INTEGER) * 1000000000 WHERE hidden = 1;
`

// viewedWorkspacesDDL is the layout-40 addition, kept as shipped because
// deployed files carry it: when each workspace's last turn result was read
// (workspaces.result_read_at) and the fold of the roster's viewed band
// (sidebar_view.viewed_folded). THE BAND WAS RETIRED: viewed workspaces stay
// in their grouping, so this build neither reads nor writes either column.
// Dropping them is a breaking step (the layout-40 build names them), so it
// ships as its own later deploy, as MigrationKind describes.
const viewedWorkspacesDDL = `
ALTER TABLE workspaces ADD COLUMN result_read_at INTEGER;
ALTER TABLE sidebar_view ADD COLUMN viewed_folded INTEGER NOT NULL DEFAULT 0;
`

// retiredGroupingColumn is what every sidebar_view row writes to its
// `grouping` column. The column is NOT NULL with no default, and it is read by
// nothing since the task and stage groupings were deleted (owner ruling,
// 2026-10-15): the rail draws one grouping, by repository. Dropping the
// column, with the stage folds' and tasks.folded, is a breaking step, so it
// ships as its own later migration (MigrationKind).
const retiredGroupingColumn = "repository"

// SidebarView is the sidebar's durable view state that belongs to no row.
type SidebarView struct {
	// MergedFolded is whether the recently-merged band is collapsed.
	MergedFolded bool
	// HiddenFolded is whether the hidden-workspaces section is collapsed.
	HiddenFolded bool
	// OpenFolded is whether the open-workspaces section is collapsed.
	OpenFolded bool
}

// DefaultSidebarView is the view nobody has changed: the recently-merged band
// EXPANDED, so a just-merged workspace is in sight until the user folds the
// band away (owner ruling 2026-10-08).
var DefaultSidebarView = SidebarView{MergedFolded: false}

// SetMergedSectionFolded records whether the recently-merged band is
// collapsed, leaving the rest of the view as it stands.
func (s *store) SetMergedSectionFolded(ctx context.Context, folded bool) error {
	return s.updateSidebarView(ctx, "daemon.wsm.set_merged_section_folded", dlog.Context{"folded": folded},
		`UPDATE sidebar_view SET merged_folded = ? WHERE id = 1`, folded)
}

// SetHiddenSectionFolded records whether the hidden-workspaces section is
// collapsed, leaving the rest of the view as it stands.
func (s *store) SetHiddenSectionFolded(ctx context.Context, folded bool) error {
	return s.updateSidebarView(ctx, "daemon.wsm.set_hidden_section_folded", dlog.Context{"folded": folded},
		`UPDATE sidebar_view SET hidden_folded = ? WHERE id = 1`, folded)
}

// SetOpenSectionFolded records whether the open-workspaces section is
// collapsed, leaving the rest of the view as it stands.
func (s *store) SetOpenSectionFolded(ctx context.Context, folded bool) error {
	return s.updateSidebarView(ctx, "daemon.wsm.set_open_section_folded", dlog.Context{"folded": folded},
		`UPDATE sidebar_view SET open_folded = ? WHERE id = 1`, folded)
}

// updateSidebarView writes one column of the singleton, first minting the row
// at DefaultSidebarView when nobody has changed the view yet, in the same
// transaction, so the columns this write leaves alone keep their defaults.
func (s *store) updateSidebarView(ctx context.Context, op string, fields dlog.Context, update string, value any) error {
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if _, err := tx.ExecContext(ctx,
			`INSERT INTO sidebar_view (id, merged_folded, grouping) VALUES (1, ?, ?) ON CONFLICT(id) DO NOTHING`,
			DefaultSidebarView.MergedFolded, retiredGroupingColumn); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx, update, value)
		return err
	})
}

// SidebarView loads the sidebar's view state, or DefaultSidebarView when
// nobody has changed it. An absent row is that legitimate "never changed"
// case and is NOT an error.
func (s *store) SidebarView(ctx context.Context) (SidebarView, error) {
	view := DefaultSidebarView
	err := s.read(ctx, "daemon.wsm.sidebar_view", dlog.Context{}, func(ctx context.Context) error {
		var read SidebarView
		err := s.q(ctx).QueryRowContext(ctx, `SELECT merged_folded, hidden_folded, open_folded FROM sidebar_view WHERE id = 1`).
			Scan(&read.MergedFolded, &read.HiddenFolded, &read.OpenFolded)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		view = read
		return nil
	})
	return view, err
}
