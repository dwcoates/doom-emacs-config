//go:build playtest

package e2e

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"
)

// OWNER 1 of PLAYTEST-PLAN.md's partition: A1-A3 -- cold start, adopting an
// already-answering daemon, and a build failure.
//
// A.1 is here. A.2 and A.3 are unwritten.

// TestPlaytestColdStartAndFirstTab is plan A.1 and A.4: an editor with
// nothing in it, a daemon Emacs spawned itself, and the first workspace tab.
//
// The two captures are the tab bar, which is the plan's own visual subject
// for section A and a surface no Connect-dialing test can see at all.
func TestPlaytestColdStartAndFirstTab(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "01-cold-start",
		"Plan A.1 and A.4. A cold Emacs spawns its own daemon, then registers a repository "+
			"from a directory and gains its first workspace tab.")
	p, e := s.Book, s.E

	if !decodeBool(e.Eval(`(and agent-repl--frontend-daemon-process
                                (process-live-p agent-repl--frontend-daemon-process))`)) {
		t.Fatal("the launcher reports no live daemon process after EnsureDaemon")
	}
	if _, err := os.Stat(filepath.Join(e.StateDir, "daemon.addr")); err != nil {
		t.Fatalf("the daemon published no address under the state root Emacs handed it: %v", err)
	}
	p.capture("cold-editor", "nothing registered yet",
		"`agent-repl--frontend-daemon-process` is live and the daemon published `daemon.addr` under the state root",
		"An Emacs frame filling the whole screen, booted through the image's real Doom. "+
			"The tab bar carries NO workspace tab: nothing is registered yet.")

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	if got := s.tabNames(); len(got) != 1 || got[0] != name {
		t.Fatalf("the tab bar draws %v, want exactly [%s]", got, name)
	}
	// A WORKSPACE NOTHING HAS BEEN WIRED TO IS `:none`, AND THAT IS THE
	// POINT OF THIS PICTURE. Registering mints an identity; it does not bring
	// a session up, which the first submit does. Per the module's color rule
	// `:none` is TEAL -- "nothing is wired, and nothing is wrong" -- and it
	// is emphatically not the blue of a broken link.
	s.awaitArm(t, name, "the new workspace's tab arm to be published", playtestUnwiredArm)
	s.captureArm(t, "first-tab", name,
		"`agent-repl-add-project-workspace` (`SPC TAB C-n`) on a scripted fake-git worktree",
		playtestUnwiredArm,
		fmt.Sprintf("The tab bar must carry EXACTLY ONE workspace tab and it must be named %q: "+
			"`agent-repl--ws-tabline-names` says the module knows about exactly that one.", name))
}

// TestPlaytestSwitchBetweenWorkspaces is plan A.7: a second workspace, the
// selection moving between the two, and the tab bar following it.
