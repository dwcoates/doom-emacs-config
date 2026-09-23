// Package testclose holds the store's one test helper for closing a subject's
// resources: a close is part of what a test observes, so a failed one fails
// the test rather than being discarded.
package testclose

import (
	"io"
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
