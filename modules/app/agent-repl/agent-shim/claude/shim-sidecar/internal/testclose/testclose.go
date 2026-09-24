// Package testclose holds the sidecar's test helpers for tearing down a
// subject's resources: a close or a removal is part of what a test observes,
// so a failure there fails the test rather than being discarded.
package testclose

import (
	"io"
	"os"
	"testing"
)

// OrFail closes c and fails the test if the close fails. A subject's own close
// is part of what it observes: a stream, body, listener or file that will not
// close cleanly is a fault the subject would otherwise hide.
func OrFail(t testing.TB, c io.Closer) {
	t.Helper()
	if err := c.Close(); err != nil {
		t.Errorf("closing %T: %v", c, err)
	}
}

// RemoveOrFail removes path and fails the test if the removal fails for any
// reason other than the path never having existed — a socket or fixture a
// subject never got around to creating is not a teardown fault.
func RemoveOrFail(t testing.TB, path string) {
	t.Helper()
	if err := os.Remove(path); err != nil && !os.IsNotExist(err) {
		t.Errorf("removing %s: %v", path, err)
	}
}
