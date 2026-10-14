package harness

import (
	"agentrepl/testrun/roster"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func TestMockedGitLinkNamesTheGitclientSeam(t *testing.T) {
	// Act.
	got := MockedGitLink("/tmp/x/git")

	// Assert.
	if got != "-X agent-repld/internal/gitclient.gitBinary=/tmp/x/git" {
		t.Fatalf("MockedGitLink = %q", got)
	}
}

// TestTheMockedGitLinkReachesTheSeam builds a daemon-module program with the
// link and reads the seam back, so a renamed or moved variable fails here
// rather than leaving test daemons on the host's git: -X on a variable that
// does not exist is silently ignored by the linker.
func TestTheMockedGitLinkReachesTheSeam(t *testing.T) {
	// Arrange: a throwaway main package inside the daemon module that prints
	// the seam through the package's own test-only accessor. IT EXISTS ONLY IN
	// THE BUILD'S OVERLAY: a directory written into the module tree is walked
	// by every source-scanning test running beside this one, which then failed
	// on it vanishing (TestTurnCloseIsNamedOnlyByItsString, 2026-10-14).
	module, err := moduleRoot()
	if err != nil {
		t.Fatalf("module root: %v", err)
	}
	scratch := t.TempDir()
	src := `package main

import (
	"fmt"

	"agent-repld/internal/gitclient"
)

func main() { fmt.Print(gitclient.Binary()) }
`
	mainGo := filepath.Join(scratch, "main.go")
	if err := os.WriteFile(mainGo, []byte(src), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	overlay, err := json.Marshal(map[string]map[string]string{"Replace": {filepath.Join(module, "seamprobe", "main.go"): mainGo}})
	if err != nil {
		t.Fatalf("encode the overlay: %v", err)
	}
	overlayFile := filepath.Join(scratch, "overlay.json")
	if err := os.WriteFile(overlayFile, overlay, 0o644); err != nil {
		t.Fatalf("write the overlay: %v", err)
	}
	bin := filepath.Join(scratch, "probe")

	// Act.
	build := exec.Command("go", "build", "-buildvcs=false", "-overlay="+overlayFile, "-ldflags="+MockedGitLink("/mocked/git"), "-o", bin, "./seamprobe")
	build.Dir = module
	if out, err := build.CombinedOutput(); err != nil {
		t.Fatalf("go build: %v\n%s", err, out)
	}
	out, err := exec.Command(bin).Output()

	// Assert.
	if err != nil || strings.TrimSpace(string(out)) != "/mocked/git" {
		t.Fatalf("the linked seam reads %q (%v), want /mocked/git", out, err)
	}
}

// TestTheRunsDaemonProgramLinksTheMockedGitAsThisHarnessDoes pins the one link
// a test run builds the daemon with (roster.Programs) to MockedGitLink, so a
// daemon built once for the run runs the mocked git exactly as one this
// harness builds itself.
func TestTheRunsDaemonProgramLinksTheMockedGitAsThisHarnessDoes(t *testing.T) {
	// Arrange.
	program, ok := roster.LookupProgram("agent-repld")
	if !ok || len(program.Links) != 1 {
		t.Fatalf("roster program agent-repld = %+v, %v; want one link", program, ok)
	}
	link := program.Links[0]

	// Act.
	got := "-X " + link.Var + "=" + filepath.Join("/programs", link.Program)

	// Assert.
	if want := MockedGitLink(filepath.Join("/programs", "git")); got != want || link.Program != "git" {
		t.Fatalf("the run links %q, the harness %q", got, want)
	}
}
