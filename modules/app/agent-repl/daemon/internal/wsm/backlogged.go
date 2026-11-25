package wsm

// THE BACKLOG IS NAMED BACKLOG IN THE FILE. The sidebar section the user sees
// labelled "Backlog" was stored under "hidden" columns, which read as "not
// visible", a different fact. Layout 210 adds the backlog's columns beside
// the hidden ones and layout 211 retires the hidden ones, the additive and
// contract pair a rename ships as (MigrationKind):
//
//   - workspaces.hidden, hidden_at and hidden_with become backlogged,
//     backlogged_at and backlogged_with (whether the workspace is in the
//     backlog, when it was moved there, and the root it was backlogged with),
//     and the index workspaces_by_hidden_with becomes
//     workspaces_by_backlogged_with;
//   - sidebar_view.hidden_folded and hidden_lines become
//     backlog_section_folded and backlog_section_lines (the section's fold
//     and its height). They are not backlog_folded: that name belonged to a
//     retired section's column (retiredColumnsDDL), whose retirement a test
//     pins.

// backloggedDDL is the layout-210 addition: the backlog's columns, filled
// from the hidden ones, and the triggers that keep each pair equal while a
// build that still writes the hidden columns serves beside this one.
//
// IT IS ADDITIVE: the previous build names none of the new columns, and a
// handover's incumbent keeps writing the hidden ones. The triggers carry
// every such write into the backlog's columns, and this build's writes back
// into the hidden ones, so the two daemons serving side by side read one
// backlog. A pair already equal fires nothing (each trigger's WHEN), so the
// two directions never chase each other.
const backloggedDDL = `
ALTER TABLE workspaces ADD COLUMN backlogged INTEGER NOT NULL DEFAULT 0;
ALTER TABLE workspaces ADD COLUMN backlogged_at INTEGER;
ALTER TABLE workspaces ADD COLUMN backlogged_with TEXT;
UPDATE workspaces SET backlogged = hidden, backlogged_at = hidden_at, backlogged_with = hidden_with;
CREATE INDEX workspaces_by_backlogged_with ON workspaces(backlogged_with) WHERE backlogged_with IS NOT NULL;
ALTER TABLE sidebar_view ADD COLUMN backlog_section_folded INTEGER NOT NULL DEFAULT 0;
ALTER TABLE sidebar_view ADD COLUMN backlog_section_lines INTEGER;
UPDATE sidebar_view SET backlog_section_folded = hidden_folded, backlog_section_lines = hidden_lines;
CREATE TRIGGER workspaces_hidden_writes_backlogged AFTER UPDATE OF hidden, hidden_at, hidden_with ON workspaces
WHEN NEW.hidden IS NOT NEW.backlogged OR NEW.hidden_at IS NOT NEW.backlogged_at OR NEW.hidden_with IS NOT NEW.backlogged_with
BEGIN
  UPDATE workspaces SET backlogged = NEW.hidden, backlogged_at = NEW.hidden_at, backlogged_with = NEW.hidden_with WHERE id = NEW.id;
END;
CREATE TRIGGER workspaces_backlogged_writes_hidden AFTER UPDATE OF backlogged, backlogged_at, backlogged_with ON workspaces
WHEN NEW.hidden IS NOT NEW.backlogged OR NEW.hidden_at IS NOT NEW.backlogged_at OR NEW.hidden_with IS NOT NEW.backlogged_with
BEGIN
  UPDATE workspaces SET hidden = NEW.backlogged, hidden_at = NEW.backlogged_at, hidden_with = NEW.backlogged_with WHERE id = NEW.id;
END;
CREATE TRIGGER workspaces_hidden_insert_writes_backlogged AFTER INSERT ON workspaces
WHEN NEW.hidden <> 0 AND NEW.backlogged = 0
BEGIN
  UPDATE workspaces SET backlogged = NEW.hidden, backlogged_at = NEW.hidden_at, backlogged_with = NEW.hidden_with WHERE id = NEW.id;
END;
CREATE TRIGGER workspaces_backlogged_insert_writes_hidden AFTER INSERT ON workspaces
WHEN NEW.backlogged <> 0 AND NEW.hidden = 0
BEGIN
  UPDATE workspaces SET hidden = NEW.backlogged, hidden_at = NEW.backlogged_at, hidden_with = NEW.backlogged_with WHERE id = NEW.id;
END;
CREATE TRIGGER sidebar_view_hidden_writes_backlog AFTER UPDATE OF hidden_folded, hidden_lines ON sidebar_view
WHEN NEW.hidden_folded IS NOT NEW.backlog_section_folded OR NEW.hidden_lines IS NOT NEW.backlog_section_lines
BEGIN
  UPDATE sidebar_view SET backlog_section_folded = NEW.hidden_folded, backlog_section_lines = NEW.hidden_lines WHERE id = NEW.id;
END;
CREATE TRIGGER sidebar_view_backlog_writes_hidden AFTER UPDATE OF backlog_section_folded, backlog_section_lines ON sidebar_view
WHEN NEW.hidden_folded IS NOT NEW.backlog_section_folded OR NEW.hidden_lines IS NOT NEW.backlog_section_lines
BEGIN
  UPDATE sidebar_view SET hidden_folded = NEW.backlog_section_folded, hidden_lines = NEW.backlog_section_lines WHERE id = NEW.id;
END;
`

// retiredHiddenColumnsDDL is the layout-211 contract: the hidden columns,
// their index and the triggers that mirrored them (backloggedDDL). No build
// past layout 210 reads or writes them, and the contract runs only once no
// predecessor shares the file, so nothing writes them any more when they go.
const retiredHiddenColumnsDDL = `
DROP TRIGGER workspaces_hidden_writes_backlogged;
DROP TRIGGER workspaces_backlogged_writes_hidden;
DROP TRIGGER workspaces_hidden_insert_writes_backlogged;
DROP TRIGGER workspaces_backlogged_insert_writes_hidden;
DROP TRIGGER sidebar_view_hidden_writes_backlog;
DROP TRIGGER sidebar_view_backlog_writes_hidden;
DROP INDEX workspaces_by_hidden_with;
ALTER TABLE workspaces DROP COLUMN hidden;
ALTER TABLE workspaces DROP COLUMN hidden_at;
ALTER TABLE workspaces DROP COLUMN hidden_with;
ALTER TABLE sidebar_view DROP COLUMN hidden_folded;
ALTER TABLE sidebar_view DROP COLUMN hidden_lines;
`
