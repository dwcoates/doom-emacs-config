package harness

import (
	"agentrepl/testrun/roster"
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
	// the seam through the package's own test-only accessor.
	module, err := moduleRoot()
	if err != nil {
		t.Fatalf("module root: %v", err)
	}
	prog, err := os.MkdirTemp(module, "seamprobe-")
	if err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(prog) })
	src := `package main

import (
	"fmt"

	"agent-repld/internal/gitclient"
)

func main() { fmt.Print(gitclient.Binary()) }
`
	if err := os.WriteFile(filepath.Join(prog, "main.go"), []byte(src), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	bin := filepath.Join(t.TempDir(), "probe")

	// Act.
	build := exec.Command("go", "build", "-buildvcs=false", "-ldflags="+MockedGitLink("/mocked/git"), "-o", bin, "./"+filepath.Base(prog))
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
