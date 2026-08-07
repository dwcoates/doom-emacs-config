package server

import (
	"errors"
	"fmt"
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/sessioncontroller"
)

// closingTokenUsage stands in for the statedb-backed token ledger: a read after
// Close is the "sql: database is closed" the SessionView push used to take, and
// the read's no-fallback contract turns that into a panic.
type closingTokenUsage struct {
	mu             sync.Mutex
	closed         bool
	readAfterClose bool
}

func (c *closingTokenUsage) List(string) ([]*frontendv1.TokenUtilization, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.closed {
		c.readAfterClose = true
		return nil, errors.New("sql: database is closed")
	}
	return nil, nil
}

func (c *closingTokenUsage) Close() {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.closed = true
}

func (c *closingTokenUsage) readPastClose() bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.readAfterClose
}

// TestShutdownAllEndsSessionViewPushesBeforeTheStoresClose pins the teardown
// ordering every owner of a daemon store relies on. A SessionView push is
// driven by the SHIMS through the registrar's PushView hook, from goroutines
// the server neither owns nor can join, so during teardown one of them reached
// the token ledger after the state database had been closed. The iterations are
// the point: a race that is merely unlikely passes once.
func TestShutdownAllEndsSessionViewPushesBeforeTheStoresClose(t *testing.T) {
	const iterations = 20
	const pushers = 4
	for i := range iterations {
		t.Run(fmt.Sprintf("iteration_%d", i), func(t *testing.T) {
			// Arrange — a session to push, and a ledger that can be closed under it.
			usage := &closingTokenUsage{}
			h := newHarnessWith(t, Config{TokenUsage: usage})
			id := createSession(t, h, `{"cwd":"/w"}`)
			var pushing sync.WaitGroup
			var running sync.WaitGroup
			pushing.Add(pushers)
			running.Add(pushers)
			stop := make(chan struct{})
			for range pushers {
				go func() {
					defer running.Done()
					h.srv.RepushSessionView(id)
					pushing.Done()
					for {
						select {
						case <-stop:
							return
						default:
							h.srv.RepushSessionView(id)
						}
					}
				}()
			}
			// Every pusher is live before teardown begins, so the shutdown
			// genuinely races in-flight pushes rather than outrunning their start.
			pushing.Wait()

			// Act — the production order: shut the server down, then close the store.
			h.srv.ShutdownAll(false, sessioncontroller.StopCauseDaemonShutdown())
			usage.Close()
			close(stop)
			running.Wait()

			// Assert.
			if usage.readPastClose() {
				t.Fatal("a SessionView push read the token ledger after it was closed; ShutdownAll must join and close the push path")
			}
		})
	}
}
