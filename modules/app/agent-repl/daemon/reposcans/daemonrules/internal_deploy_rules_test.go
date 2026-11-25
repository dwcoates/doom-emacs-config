package daemonrules

import (
	"os"
	"regexp"
	"slices"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalDeployDir is the package these rules hold, under the daemon module's root.
const internalDeployDir = "internal/deploy"

// TestDeployNamesTheLockHolderOnlyThroughTheSharedConstant guards the
// extraction: deploy.go spells the lock holder's binary name only as
// sessionlock.HolderBinary, so the name it installs and the name the traffic
// measurement recognizes cannot drift apart.
func TestDeployNamesTheLockHolderOnlyThroughTheSharedConstant(t *testing.T) {
	sourcescan.InPackage(t, internalDeployDir)
	// Arrange.
	src, err := os.ReadFile("deploy.go")
	if err != nil {
		t.Fatalf("read deploy.go: %v", err)
	}

	// Act.
	literal := strings.Count(string(src), `"shim-lock"`)
	shared := strings.Count(string(src), "sessionlock.HolderBinary")

	// Assert.
	if literal != 0 || shared == 0 {
		t.Fatalf("deploy.go spells the lock holder %d time(s) as a literal and %d through sessionlock.HolderBinary; want only the constant", literal, shared)
	}
}

func TestEveryLiveAbsenceIsReadThroughPathStands(t *testing.T) {
	sourcescan.InPackage(t, internalDeployDir)
	// Act
	sites := sourcescan.Sites(t, internalDeployDir, "os.Lstat(")

	// Assert: pathStands' own read is the only one.
	if !slices.Equal(sites, []string{"deploy.go"}) {
		t.Fatalf("os.Lstat sites = %v, want only pathStands' in deploy.go", sites)
	}
}

// EVERY QUESTION TO THE STAGED BINARY GOES THROUGH askBinary: a question
// spelled with its own exec would drift from the bound and the error shape.
func TestTheBinaryQuestionsShareAskBinary(t *testing.T) {
	sourcescan.InPackage(t, internalDeployDir)
	// Arrange
	src, err := os.ReadFile("layout.go")
	if err != nil {
		t.Fatalf("read layout.go: %v", err)
	}

	// Act
	execs := strings.Count(string(src), "childproc.CommandContext(")

	// Assert
	if execs != 1 {
		t.Fatalf("layout.go runs %d processes by hand, want the one inside askBinary", execs)
	}
}

// TestEverySingleServiceRestartGoesThroughTheHelpers pins the shared shape: in
// a deploy and in its rollback, the only service restarts called directly are
// the store's (which restarts the sidecar with it and so has two outcomes) and
// a socket holder's (one per holder, restarted before anything else moves,
// with no phase of its own on the update line); every other goes through
// restartOnto or rollbackRestart.
func TestEverySingleServiceRestartGoesThroughTheHelpers(t *testing.T) {
	sourcescan.InPackage(t, internalDeployDir)
	direct := regexp.MustCompile(`Services\.Restart(\w+)\(ctx[,)]`)
	for _, file := range []string{"deploy.go", "rollback.go"} {
		// Arrange
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}

		// Act
		calls := direct.FindAllStringSubmatch(string(src), -1)

		// Assert
		for _, call := range calls {
			if call[1] != "Store" && call[1] != "Holder" {
				t.Errorf("%s calls Services.Restart%s directly; a single service restarts through restartOnto or rollbackRestart", file, call[1])
			}
		}
	}
}
