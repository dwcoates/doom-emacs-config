package main

import (
	"fmt"
	"strings"
	"sync"
	"time"

	"claude-repld/internal/dlog"
)

// teardownClock times the daemon's shutdown, step by step, and records it in
// ONE INFO record just before "the daemon process ended".
//
// WHY: an outgoing daemon's exit is what a replacement waits on -- the boot
// claim is released only when the process ends -- and the steps between the
// serving context's cancel and the end wrote nothing. On 2026-10-06 an
// outgoing daemon's last record and its exit were 1.07s apart under load, a
// replacement sat in that gap with nothing said, and an e2e reconnect bound
// was spent there with no record naming which step held it.
type teardownClock struct {
	log   dlog.Logger
	mu    sync.Mutex
	began time.Time
	steps []teardownStep
}

type teardownStep struct {
	name string
	took time.Duration
}

func newTeardownClock(log dlog.Logger) *teardownClock { return &teardownClock{log: log} }

// begin marks the moment the daemon stopped serving; the first call wins.
func (c *teardownClock) begin(at time.Time) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.began.IsZero() {
		c.began = at
	}
}

// mark records a step that has already happened, timed from start.
func (c *teardownClock) mark(name string, start time.Time) {
	took := time.Since(start)
	c.mu.Lock()
	defer c.mu.Unlock()
	c.steps = append(c.steps, teardownStep{name: name, took: took})
}

// markSinceBegin records the step that ended now and began when the daemon
// stopped serving: the listener draining its connections. Before any begin
// (a Serve that returned on its own) it is timed as nothing.
func (c *teardownClock) markSinceBegin(name string) {
	c.mu.Lock()
	began := c.began
	c.mu.Unlock()
	if began.IsZero() {
		began = time.Now()
	}
	c.mark(name, began)
}

// step runs fn and records how long it took under name.
func (c *teardownClock) step(name string, fn func()) {
	start := time.Now()
	fn()
	c.mark(name, start)
}

// report writes the one record. A boot that never served still reports the
// steps it unwound, with no begin.
func (c *teardownClock) report() {
	c.mu.Lock()
	defer c.mu.Unlock()
	parts := make([]string, 0, len(c.steps))
	for _, s := range c.steps {
		parts = append(parts, fmt.Sprintf("%s=%dms", s.name, s.took.Milliseconds()))
	}
	fields := dlog.Context{"steps": strings.Join(parts, " ")}
	if !c.began.IsZero() {
		fields["since_serving_stopped_ms"] = time.Since(c.began).Milliseconds()
	}
	c.log.Info("daemon.cmd.teardown", "the teardown's steps, in the order they ran", fields)
}
