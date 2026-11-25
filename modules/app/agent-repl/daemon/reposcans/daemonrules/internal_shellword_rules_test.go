package daemonrules

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalShellwordDir is the package these rules hold, under the daemon module's root.
const internalShellwordDir = "internal/shellword"

// TestNoProductionSourceHandRollsTheQuoting fails on any daemon source outside
// this package that spells the quote-escape idiom itself, so every caller goes
// through Quote rather than a copy that can drift.
func TestNoProductionSourceHandRollsTheQuoting(t *testing.T) {
	sourcescan.InPackage(t, internalShellwordDir)
	// Arrange.
	root := filepath.Join("..", "..")
	self, err := filepath.Abs(".")
	if err != nil {
		t.Fatalf("resolve this package: %v", err)
	}
	var offenders []string

	// Act.
	walkErr := filepath.WalkDir(root, func(path string, entry fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if entry.IsDir() {
			abs, absErr := filepath.Abs(path)
			if absErr != nil {
				return absErr
			}
			if abs == self || entry.Name() == "node_modules" {
				return filepath.SkipDir
			}
			return nil
		}
		if filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		source, readErr := os.ReadFile(path)
		if readErr != nil {
			return readErr
		}
		if strings.Contains(string(source), `'\''`) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert.
	if walkErr != nil {
		t.Fatalf("walk the daemon sources: %v", walkErr)
	}
	if len(offenders) > 0 {
		t.Fatalf("these sources quote for sh by hand; call shellword.Quote instead: %v", offenders)
	}
}
