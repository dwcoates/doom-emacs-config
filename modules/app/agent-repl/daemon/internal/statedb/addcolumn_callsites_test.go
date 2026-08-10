package statedb

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestNoPackageHandRollsAnAdditiveColumnMigration is the guard on the
// extraction itself.
//
// Four packages-worth of hand-rolled `ALTER TABLE ... ADD COLUMN` were
// consolidated onto AddColumnIfMissing, and the failure mode of any such
// consolidation is a FIFTH site written by hand later: it compiles, it passes
// its own tests, and it silently reintroduces the non-idempotent migration the
// helper exists to prevent. Nothing in the type system can catch that, so the
// call sites are asserted to share the extracted shape here.
//
// addcolumn.go is the one legitimate holder of the statement.
func TestNoPackageHandRollsAnAdditiveColumnMigration(t *testing.T) {
	// Arrange — walk the daemon's whole internal tree, not just this package:
	// the duplication this replaced spanned two of them.
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("resolve daemon root: %v", err)
	}
	const statement = "ALTER TABLE"
	// The helper that owns the statement, and this guard, which must name the
	// statement in order to look for it.
	exempt := map[string]bool{
		filepath.Join(root, "internal", "statedb", "addcolumn.go"):                true,
		filepath.Join(root, "internal", "statedb", "addcolumn_callsites_test.go"): true,
	}

	// Act.
	var offenders []string
	walkErr := filepath.WalkDir(root, func(path string, entry fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if entry.IsDir() || !strings.HasSuffix(path, ".go") || exempt[path] {
			return nil
		}
		body, readErr := os.ReadFile(path)
		if readErr != nil {
			return readErr
		}
		for _, line := range strings.Split(string(body), "\n") {
			// PROSE ABOUT the statement is not the statement. Several of these
			// files explain in a comment why the migration is additive, and a
			// guard that could not tell an explanation from an execution would
			// be one nobody could keep green.
			trimmed := strings.TrimSpace(line)
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "--") {
				continue
			}
			if !strings.Contains(line, statement) {
				continue
			}
			rel, relErr := filepath.Rel(root, path)
			if relErr != nil {
				rel = path
			}
			offenders = append(offenders, rel)
			break
		}
		return nil
	})

	// Assert.
	if walkErr != nil {
		t.Fatalf("walk daemon sources: %v", walkErr)
	}
	if len(offenders) != 0 {
		t.Fatalf("hand-rolled %q found in %v; additive column migrations go through statedb.AddColumnIfMissing",
			statement, offenders)
	}
}
