package reposcans

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

// holderSpellings are what speaking the socket holder's control protocol by
// hand looks like: passing a descriptor, or writing one of its requests or
// statuses as a literal, or comparing a grant's verb to one.
var holderSpellings = regexp.MustCompile(`UnixRights|ParseUnixRights|"return "|"accept "|StatusRefused byte|StatusOK byte|Verb == "(take|probe|return|accept)"`)

// TestOnlyTheSocketHolderPackageSpeaksItsProtocol pins that the holder's
// requests (take, probe, return, accept) and its status bytes are defined
// once, in agentrepl/logging/sockethold, which the holder program and the
// daemon both import: no other Go file passes a descriptor or spells a
// request or a status itself.
func TestOnlyTheSocketHolderPackageSpeaksItsProtocol(t *testing.T) {
	// Arrange
	root := moduleRootDir(t)
	home := filepath.Join(root, "agent-shim", "logging", "go", "sockethold")
	scans, err := filepath.Abs(".")
	if err != nil {
		t.Fatal(err)
	}
	var offenders []string

	// Act
	err = filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if path == home || path == scans || d.Name() == "node_modules" || d.Name() == ".git" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if holderSpellings.Match(body) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk agent-repl: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these files speak the socket holder's protocol themselves; use agentrepl/logging/sockethold: %v", offenders)
	}
}
