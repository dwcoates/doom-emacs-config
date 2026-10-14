package wsm

import (
	"database/sql"
	"fmt"
	"strings"

	"agent-repld/internal/semver"
)

// This file is the durable half of a MERGE'S VERSION INCREMENT
// (agentrepl.v1.MergeWorkspaceVersionIncrement): how a merge onto agent-repl's
// master moves the version, and the changelog line its tag carries. It rides
// the merge's queue row (MergeSource.Version), and an async request's
// hand-off carries it onto the merge workspace's own row, because the version
// is computed when the merge LANDS.

// VersionIncrement is one merge's move of agent-repl's version.
type VersionIncrement struct {
	// Part is the number the merge moves.
	Part semver.Part
	// ChangelogLine is the tag's annotation: one non-blank line.
	ChangelogLine string
}

// Validate refuses an increment that moves no declared part or whose
// changelog line is not exactly one non-blank line. It is the one rule every
// path that builds an increment (the rpc, the command file, the queue row)
// holds it to.
func (v VersionIncrement) Validate() error {
	if !v.Part.Valid() {
		return fmt.Errorf("a version increment moves minor or patch, not %s", v.Part)
	}
	if strings.TrimSpace(v.ChangelogLine) == "" || strings.ContainsAny(v.ChangelogLine, "\r\n") {
		return fmt.Errorf("a version increment carries one non-blank changelog line, not %q", v.ChangelogLine)
	}
	return nil
}

// versionIncrementDDL is the layout-49 addition. It adds:
//
//   - merge_queue.source_version_part: the part a merge's increment moves
//     (semver.Part); NULL on a merge that carries none, which is every
//     existing row.
//   - merge_queue.source_changelog_line: the increment's changelog line, set
//     exactly when source_version_part is.
//
// IT IS ADDITIVE: the previous build's INSERTs name neither column, so a row
// it writes takes NULL, a merge with no increment, which is exactly what that
// build asked for. The fresh-file schema appends this same text (schema.go),
// so a migrated file and a created one cannot differ.
const versionIncrementDDL = `
ALTER TABLE merge_queue ADD COLUMN source_version_part INTEGER;
ALTER TABLE merge_queue ADD COLUMN source_changelog_line TEXT;
`

// versionPart and versionLine spell an increment as its two columns: NULL and
// NULL for none.
func versionPart(v *VersionIncrement) any {
	if v == nil {
		return nil
	}
	return int64(v.Part)
}

func versionLine(v *VersionIncrement) any {
	if v == nil {
		return nil
	}
	return v.ChangelogLine
}

// scanVersion reads the two columns back. Exactly both or neither is set: a
// row with one alone was not written by insertRequest, and is refused rather
// than read as no increment.
func scanVersion(part sql.NullInt64, line sql.NullString) (*VersionIncrement, error) {
	if part.Valid != line.Valid {
		return nil, fmt.Errorf("a version increment's part and changelog line are set together, not part %v and line %v", part.Valid, line.Valid)
	}
	if !part.Valid {
		return nil, nil
	}
	return &VersionIncrement{Part: semver.Part(part.Int64), ChangelogLine: line.String}, nil
}
