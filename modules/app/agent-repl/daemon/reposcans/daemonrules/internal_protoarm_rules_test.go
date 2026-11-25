package daemonrules

import (
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalProtoarmDir is the package these rules hold, under the daemon module's root.
const internalProtoarmDir = "internal/protoarm"

// TestNoSiteResolvesANamedOneofByHand holds every production site in the
// daemon to the one shape: an arm of a oneof named in the source is read
// through Set or Name, never by handing WhichOneof a ByName lookup.
func TestNoSiteResolvesANamedOneofByHand(t *testing.T) {
	sourcescan.InPackage(t, internalProtoarmDir)
	// Arrange
	root := filepath.Join("..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for i, line := range strings.Split(string(body), "\n") {
			if strings.Contains(line, "WhichOneof(") && strings.Contains(line, "Oneofs().ByName(") {
				offenders = append(offenders, path+":"+strconv.Itoa(i+1))
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("named oneofs resolved by hand, want protoarm.Set or protoarm.Name: %v", offenders)
	}
}

// TestNoSiteHandRollsARequiredOneofPanic holds every production site that
// treats an unset oneof as a bug to Must, so each one names the message, the
// oneof and the operation alike: a Set or Name whose unset branch panics by
// hand is a divergent copy of it.
func TestNoSiteHandRollsARequiredOneofPanic(t *testing.T) {
	sourcescan.InPackage(t, internalProtoarmDir)
	// Arrange
	root := filepath.Join("..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		lines := strings.Split(string(body), "\n")
		for i := 0; i+2 < len(lines); i++ {
			resolves := strings.Contains(lines[i], "protoarm.Name(") || strings.Contains(lines[i], "protoarm.Set(")
			if resolves && strings.TrimSpace(lines[i+1]) == "if !ok {" && strings.Contains(lines[i+2], "panic(") {
				offenders = append(offenders, path+":"+strconv.Itoa(i+1))
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("required oneofs checked by hand, want protoarm.Must: %v", offenders)
	}
}
