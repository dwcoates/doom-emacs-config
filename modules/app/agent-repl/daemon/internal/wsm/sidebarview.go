package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

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
// fold (frontend.v1.RosterMaturitySection.fold). A stage belongs to no row,
// so its fold lives on the sidebar_view singleton beside the merged band's.
// Every stage is expanded (0) until someone folds it.
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

// Grouping is which of the sidebar's resolved views is shown.
type Grouping string

// The groupings, spelled as the sidebar_view row stores them.
const (
	GroupingRepository Grouping = "repository"
	GroupingTask       Grouping = "task"
	GroupingMaturity   Grouping = "maturity"
)

// Valid reports whether g is one of the groupings.
func (g Grouping) Valid() bool {
	return g == GroupingRepository || g == GroupingTask || g == GroupingMaturity
}

// MaturityStage is one section of the maturity grouping
// (workspace.v1.MaturityStage): how far along a workspace's work is.
type MaturityStage string

// The stages, in the order work moves through them.
const (
	MaturityBacklog    MaturityStage = "backlog"
	MaturityInProgress MaturityStage = "in_progress"
	MaturityMerging    MaturityStage = "merging"
)

// MaturityStages is every stage, in the order the grouping draws them.
var MaturityStages = []MaturityStage{MaturityBacklog, MaturityInProgress, MaturityMerging}

// foldColumn is the sidebar_view column that holds the stage's fold, or false
// when the stage is none of the three.
func (m MaturityStage) foldColumn() (string, bool) {
	switch m {
	case MaturityBacklog:
		return "backlog_folded", true
	case MaturityInProgress:
		return "in_progress_folded", true
	case MaturityMerging:
		return "merging_folded", true
	default:
		return "", false
	}
}

// MaturityFolds is whether each maturity stage's section is collapsed.
type MaturityFolds struct {
	Backlog    bool
	InProgress bool
	Merging    bool
}

// Folded reports whether STAGE's section is collapsed. A stage that is none
// of the three is a caller defect: the stages are this package's own
// constants, so there is no unknown stage to answer for.
func (f MaturityFolds) Folded(stage MaturityStage) bool {
	return *f.slot(stage)
}

// With answers the folds with STAGE's set to FOLDED.
func (f MaturityFolds) With(stage MaturityStage, folded bool) MaturityFolds {
	*f.slot(stage) = folded
	return f
}

// slot is STAGE's field, panicking on a stage that is none of the three.
func (f *MaturityFolds) slot(stage MaturityStage) *bool {
	switch stage {
	case MaturityBacklog:
		return &f.Backlog
	case MaturityInProgress:
		return &f.InProgress
	case MaturityMerging:
		return &f.Merging
	default:
		panic(fmt.Sprintf("wsm: maturity stage %q is none of backlog, in_progress, merging", stage))
	}
}

// SidebarView is the sidebar's durable view state that belongs to no row.
type SidebarView struct {
	// MergedFolded is whether the recently-merged band is collapsed.
	MergedFolded bool
	// MaturityFolded is whether each maturity stage's section is collapsed.
	MaturityFolded MaturityFolds
	// HiddenFolded is whether the hidden-workspaces section is collapsed.
	HiddenFolded bool
	// OpenFolded is whether the open-workspaces section is collapsed.
	OpenFolded bool
	// Grouping is the grouping every page shows.
	Grouping Grouping
}

// DefaultSidebarView is the view nobody has changed: the recently-merged band
// EXPANDED, so a just-merged workspace is in sight until the user folds the
// band away (owner ruling 2026-10-08), and the repository grouping shown.
var DefaultSidebarView = SidebarView{MergedFolded: false, Grouping: GroupingRepository}

// SetTaskFolded records whether a task's roster section is collapsed. An
// unknown task is refused (ErrNotFound), never a write that touched nothing.
func (s *store) SetTaskFolded(ctx context.Context, id TaskID, folded bool) error {
	return s.write(ctx, "daemon.wsm.set_task_folded", dlog.Context{"task": string(id), "folded": folded},
		func(ctx context.Context, tx *sql.Tx) error {
			res, err := tx.ExecContext(ctx, `UPDATE tasks SET folded = ? WHERE id = ?`, folded, id)
			if err != nil {
				return err
			}
			return requireOneRow(res, fmt.Sprintf("wsm: task %s", id))
		})
}

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

// SetMaturityFolded records whether one maturity stage's section is
// collapsed, leaving the rest of the view as it stands. A stage that is none
// of the three is a caller defect and is refused loudly rather than persisted.
func (s *store) SetMaturityFolded(ctx context.Context, stage MaturityStage, folded bool) error {
	const op = "daemon.wsm.set_maturity_folded"
	fields := dlog.Context{"stage": string(stage), "folded": folded}
	column, ok := stage.foldColumn()
	if !ok {
		err := fmt.Errorf("wsm: invalid maturity stage %q", stage)
		s.log.Error(op, "refused a maturity stage that is none of backlog, in_progress, merging", withError(fields, err))
		return err
	}
	return s.updateSidebarView(ctx, op, fields, `UPDATE sidebar_view SET `+column+` = ? WHERE id = 1`, folded)
}

// SetGrouping records which grouping every page shows, leaving the rest of the
// view as it stands. A grouping that is none of the three is a caller defect
// and is refused loudly rather than persisted.
func (s *store) SetGrouping(ctx context.Context, grouping Grouping) error {
	const op = "daemon.wsm.set_grouping"
	fields := dlog.Context{"grouping": string(grouping)}
	if !grouping.Valid() {
		err := fmt.Errorf("wsm: invalid grouping %q", grouping)
		s.log.Error(op, "refused a grouping that is none of repository, task, maturity", withError(fields, err))
		return err
	}
	return s.updateSidebarView(ctx, op, fields, `UPDATE sidebar_view SET grouping = ? WHERE id = 1`, string(grouping))
}

// updateSidebarView writes one column of the singleton, first minting the row
// at DefaultSidebarView when nobody has changed the view yet, in the same
// transaction, so the columns this write leaves alone keep their defaults.
func (s *store) updateSidebarView(ctx context.Context, op string, fields dlog.Context, update string, value any) error {
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if _, err := tx.ExecContext(ctx,
			`INSERT INTO sidebar_view (id, merged_folded, grouping) VALUES (1, ?, ?) ON CONFLICT(id) DO NOTHING`,
			DefaultSidebarView.MergedFolded, string(DefaultSidebarView.Grouping)); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx, update, value)
		return err
	})
}

// SidebarView loads the sidebar's view state, or DefaultSidebarView when
// nobody has changed it. An absent row is that legitimate "never changed"
// case and is NOT an error; a stored grouping that is none of the three is a
// corrupt record, surfaced as a DecodeError.
func (s *store) SidebarView(ctx context.Context) (SidebarView, error) {
	view := DefaultSidebarView
	err := s.read(ctx, "daemon.wsm.sidebar_view", dlog.Context{}, func(ctx context.Context) error {
		var (
			folded   bool
			hidden   bool
			open     bool
			maturity MaturityFolds
			grouping string
		)
		// Each stage's column is read straight into its field, through the
		// same two mappings the write uses, so a stage cannot be read from a
		// column other than the one it is written to.
		columns := "merged_folded, grouping, hidden_folded, open_folded"
		dest := []any{&folded, &grouping, &hidden, &open}
		for _, stage := range MaturityStages {
			column, _ := stage.foldColumn()
			columns += ", " + column
			dest = append(dest, maturity.slot(stage))
		}
		err := s.db().QueryRowContext(ctx, `SELECT `+columns+` FROM sidebar_view WHERE id = 1`).Scan(dest...)
		if errors.Is(err, sql.ErrNoRows) {
			view = DefaultSidebarView
			return nil
		}
		if err != nil {
			return err
		}
		if !Grouping(grouping).Valid() {
			return &DecodeError{Table: "sidebar_view", Row: "1", Field: "grouping", Err: fmt.Errorf("a stored grouping is repository, task or maturity, not %q", grouping)}
		}
		view = SidebarView{MergedFolded: folded, MaturityFolded: maturity, HiddenFolded: hidden, OpenFolded: open, Grouping: Grouping(grouping)}
		return nil
	})
	return view, err
}
