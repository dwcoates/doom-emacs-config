package statedb

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// THE PLACEHOLDER LIST IS DERIVED, NEVER WRITTEN OUT.
//
// Three packages built their own IN-list widths — two by repeating "?," and one
// by appending "?" per argument in a loop — and each was one number stated
// twice, beside the arguments it had to agree with. These tests pin the
// extracted helper's own behavior, and that nobody re-rolls it.

func TestPlaceholdersRendersOnePerArgument(t *testing.T) {
	// Arrange, Act.
	got := Placeholders(3)

	// Assert.
	if got != "?,?,?" {
		t.Fatalf("Placeholders(3) = %q, want %q", got, "?,?,?")
	}
}

func TestPlaceholdersRendersOneWithNoTrailingComma(t *testing.T) {
	// Arrange — the boundary the hand-rolled repeat kept getting wrong: a
	// trailing comma is a syntax error at execution, far from its cause.
	// Act.
	got := Placeholders(1)

	// Assert.
	if got != "?" {
		t.Fatalf("Placeholders(1) = %q, want %q", got, "?")
	}
}

func TestPlaceholdersRendersNothingForNoArguments(t *testing.T) {
	// Arrange — callers guard the empty case themselves; the helper's job is
	// to state honestly that there is nothing to match.
	// Act.
	got := Placeholders(0)

	// Assert.
	if got != "" {
		t.Fatalf("Placeholders(0) = %q, want the empty list", got)
	}
}

// TestNoPackageHandRollsAPlaceholderList is the guard on the extraction itself.
//
// The failure mode of any consolidation is a later site written by hand: it
// compiles, it passes its own tests, and it puts the count back where it can
// silently disagree with the arguments. Nothing in the type system catches
// that, so the call sites are asserted to share the extracted shape here.
func TestNoPackageHandRollsAPlaceholderList(t *testing.T) {
	// Arrange — walk the daemon's whole internal tree: the duplication this
	// replaced spanned two packages.
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("resolve daemon root: %v", err)
	}
	const statement = `strings.Repeat("?,"`
	exempt := map[string]bool{
		filepath.Join(root, "internal", "statedb", "placeholders.go"):      true,
		filepath.Join(root, "internal", "statedb", "placeholders_test.go"): true,
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
			// PROSE ABOUT the construction is not the construction, exactly as
			// the additive-migration guard beside this one reasons.
			trimmed := strings.TrimSpace(line)
			if strings.HasPrefix(trimmed, "//") {
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
		t.Fatalf("hand-rolled placeholder list found in %v; SQL bind lists go through statedb.Placeholders", offenders)
	}
}
