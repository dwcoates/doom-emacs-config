package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
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

// Grouping is which of the sidebar's two resolved views is shown.
type Grouping string

// The two groupings, spelled as the sidebar_view row stores them.
const (
	GroupingRepository Grouping = "repository"
	GroupingTask       Grouping = "task"
)

// valid reports whether g is one of the two groupings.
func (g Grouping) valid() bool {
	return g == GroupingRepository || g == GroupingTask
}

// SidebarView is the sidebar's durable view state that belongs to no row.
type SidebarView struct {
	// MergedFolded is whether the recently-merged band is collapsed.
	MergedFolded bool
	// Grouping is the grouping every page shows.
	Grouping Grouping
}

// DefaultSidebarView is the view nobody has changed: the recently-merged band
// COLLAPSED, because settled history should not spend rail height until it is
// asked for, and the repository grouping shown.
var DefaultSidebarView = SidebarView{MergedFolded: true, Grouping: GroupingRepository}

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

// SetGrouping records which grouping every page shows, leaving the rest of the
// view as it stands. A grouping that is neither of the two is a caller defect
// and is refused loudly rather than persisted.
func (s *store) SetGrouping(ctx context.Context, grouping Grouping) error {
	const op = "daemon.wsm.set_grouping"
	fields := dlog.Context{"grouping": string(grouping)}
	if !grouping.valid() {
		err := fmt.Errorf("wsm: invalid grouping %q", grouping)
		s.log.Error(op, "refused a grouping that is neither repository nor task", withError(fields, err))
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
// case and is NOT an error; a stored grouping that is neither of the two is a
// corrupt record, surfaced as a DecodeError.
func (s *store) SidebarView(ctx context.Context) (SidebarView, error) {
	view := DefaultSidebarView
	err := s.read(ctx, "daemon.wsm.sidebar_view", dlog.Context{}, func(ctx context.Context) error {
		var (
			folded   bool
			grouping string
		)
		err := s.db().QueryRowContext(ctx, `SELECT merged_folded, grouping FROM sidebar_view WHERE id = 1`).Scan(&folded, &grouping)
		if errors.Is(err, sql.ErrNoRows) {
			view = DefaultSidebarView
			return nil
		}
		if err != nil {
			return err
		}
		if !Grouping(grouping).valid() {
			return &DecodeError{Table: "sidebar_view", Row: "1", Field: "grouping", Err: fmt.Errorf("a stored grouping is repository or task, not %q", grouping)}
		}
		view = SidebarView{MergedFolded: folded, Grouping: Grouping(grouping)}
		return nil
	})
	return view, err
}
