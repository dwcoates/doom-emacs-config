package harness

import (
	"context"
	"os"
	"path/filepath"
	"slices"
	"testing"

	"agent-repld/internal/kernelevent"
)

// awaitChange answers true once check holds, checking now and again at every
// change the kernel reports under paths, or false once wait ends first. It is
// the harness's ONE wait on a file another process writes -- a log record, an
// address file, a socket -- and it never re-reads on a timer.
//
// PATHS IS ASKED AGAIN EACH ROUND, so a file that a change made (a log target
// rotated in, a workspace's sink linked) is followed from the next round on.
// Each round's watch is taken before its check, so a change in between is
// reported, and a round whose paths changed while its watch was taken is taken
// again before it checks. A watch that fails fails t: a wait that can no longer see changes
// would otherwise only ever end at its bound.
func awaitChange(t testing.TB, wait context.Context, check func() bool, paths func() []string) bool {
	t.Helper()
	for {
		followed := paths()
		changes, stop := kernelevent.Follow(wait, followed...)
		if !slices.Equal(paths(), followed) {
			// WHAT TO FOLLOW CHANGED WHILE THE WATCH WAS TAKEN, so the watch
			// may have missed the change that moved it: a sink's link renamed
			// into place after paths was asked and before its directory was
			// watched leaves its target unfollowed, and the target's first
			// write was never reported (2026-11-15 20:19,
			// TestWebappLayer/WebappLayerRestartHandover in a merge gate). The
			// round is taken again, so every check runs under a watch of what
			// paths answers after that watch was in place.
			stop()
			if wait.Err() != nil {
				return false
			}
			continue
		}
		if check() {
			stop()
			return true
		}
		select {
		case err := <-changes:
			stop()
			if err != nil && wait.Err() == nil {
				t.Fatalf("harness: watch %v: %v", paths(), err)
			}
		case <-wait.Done():
			stop()
			return false
		}
	}
}

// followFiles answers what a wait on FILES follows: each file (its writes),
// and every directory from its own up to the first that exists (its making,
// its rotation and its replacement). A file that is a symbolic link is
// followed through its target too: the target, and every directory from the
// target's own up to the first that exists.
//
// A LINK WHOSE TARGET IS NOT WRITTEN YET IS DANGLING, and a watch cannot open
// it, so only the link's own directory was followed -- which the target's
// making, in another directory, never changes. A workspace's sink is such a
// link (`.claude/emacs/webapp.log` into the logs directory): a wait taken
// before the page's first record never saw it written, and ran out its bound
// with the record standing (2026-11-15 06:31,
// TestWebappLayer/WebappLayerRestartHandover in a full run).
func followFiles(files ...string) func() []string {
	return func() []string {
		var out []string
		seen := map[string]bool{}
		add := func(p string) {
			if !seen[p] {
				seen[p] = true
				out = append(out, p)
			}
		}
		addWithDirs := func(f string) {
			add(f)
			for dir := filepath.Dir(f); ; dir = filepath.Dir(dir) {
				add(dir)
				if _, err := os.Stat(dir); err == nil || dir == filepath.Dir(dir) {
					break
				}
			}
		}
		for _, f := range files {
			addWithDirs(f)
			target, err := os.Readlink(f)
			if err != nil {
				continue
			}
			if !filepath.IsAbs(target) {
				target = filepath.Join(filepath.Dir(f), target)
			}
			addWithDirs(target)
		}
		return out
	}
}
