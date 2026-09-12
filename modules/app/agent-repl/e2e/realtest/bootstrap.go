//go:build realtest

package realtest

import (
	"fmt"
)

// REALTEST 4 BRINGS ITS OWN THIRD WORKSPACE.
//
// The test needs three open workspaces, because with two tabs `s-}` and `s-{`
// land on the SAME tab and a reversed direction cannot be told from a correct
// one (realtest_4_switch_between_workspaces_test.go says so at
// rt4MinimumWorkspaces). It used to REFUSE when the registry held fewer and
// tell the owner to open some, which on the 2026-09-12 run meant realtest 4
// could not run at all: the registry held two.
//
// A realtest that cannot run because of the shape of the owner's registry is a
// realtest that measures the owner's registry. So it bootstraps instead, using
// the same scratch-repository substrate realtests 5 through 8 already use: a
// repository of its own under the run directory, registered through the
// register command, closed and deleted on the way out through `t.Cleanup` —
// including on failure. The residue registering leaves is the one the substrate
// already reports (wsActCleanupRegistered), and it is reported the same way.
//
// It registers rather than creates: registering a directory mints an open
// workspace with a tab and no worktree, no branch and no shim conversation,
// which is exactly and only what a third tab to navigate to has to be.

// rt4BootstrapCount is how many workspaces the run must bootstrap, given how
// many the registry already holds open.
func rt4BootstrapCount(open int) int {
	if open >= rt4MinimumWorkspaces {
		return 0
	}
	return rt4MinimumWorkspaces - open
}

// rt4BootstrapRepoName names the scratch repository for the bootstrap
// workspace numbered `index` (counting from 0).
//
// The names are distinct because more than one may be needed — a registry
// holding one open workspace needs two — and two repositories at one path
// would be one repository registered twice.
func rt4BootstrapRepoName(index int) string {
	return fmt.Sprintf("rt4-bootstrap-%d", index+1)
}

// rt4BootstrapGuardMessage is what the run says when registering the scratch
// repository did not produce a workspace.
//
// IT NAMES THE VENDOR GUARD BY NAME. A register that fails under the guard and
// a register that fails because the daemon refused the directory look identical
// from here, and the first is a known, separately-owned condition on this
// branch. Naming it is what stops the next reader from re-diagnosing it.
func rt4BootstrapGuardMessage(dir string, cause string) string {
	return fmt.Sprintf("realtest 4 needs %d open workspaces and could not bootstrap the ones the registry "+
		"lacks: registering the scratch repository %s produced no workspace (%s).\n"+
		"FIRST SUSPECT THE VENDOR GUARD: a create or register that reaches the shim's `createRealQuery` throws "+
		"under the guard that forbids the vendor for a realtest run, and a run whose Emacs lacks "+
		"AGENT_REPL_FAKE_SHIMS=1 will fail here and nowhere earlier. Check the register command's own record "+
		"(`elisp.commands.add-project-not-registered`) in the log before looking anywhere else.",
		rt4MinimumWorkspaces, dir, cause)
}
