package harness

import (
	"context"
	"os"
	"path/filepath"
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
// reported. A watch that fails fails t: a wait that can no longer see changes
// would otherwise only ever end at its bound.
func awaitChange(t testing.TB, wait context.Context, check func() bool, paths func() []string) bool {
	t.Helper()
	for {
		changes, stop := kernelevent.Follow(wait, paths()...)
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
// its rotation and its replacement).
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
		for _, f := range files {
			add(f)
			for dir := filepath.Dir(f); ; dir = filepath.Dir(dir) {
				add(dir)
				if _, err := os.Stat(dir); err == nil || dir == filepath.Dir(dir) {
					break
				}
			}
		}
		return out
	}
}
