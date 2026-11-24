package life

import (
	"context"
	"fmt"
	"sort"
	"strings"
	"sync"
	"time"
)

// JoinBound bounds Background.Stop's wait for the run's loops to return
// after their cancellation. A loop returns at the end of the step it is in,
// and the slowest is the CPU governor's: its host-power, host-CPU and
// marked-process reads took p50 47ms, p99 219ms, max 244ms over 100 steps
// (measured 2026-11-24 on a sixteen-core Mac in Low Power mode at a load
// average of 9).
const JoinBound = 600 * time.Millisecond

// slowestStep is the slowest governor step JoinBound was measured from.
const slowestStep = 244 * time.Millisecond

// Background runs the run's own background loops (the CPU governor, the
// units' CPU sampler) and joins them before the run reaps its session.
//
// A LOOP CANCELLED BUT NOT JOINED STARTS ONE MORE STEP. Its select picks at
// random among ready cases, so a tick that arrives with the cancellation
// runs one more step, and the governor's step runs `ioreg` and `pmset`. Run
// 79559 (2026-11-24) passed every unit, then its reap found `ioreg` started
// after the kill and failed the run. Stop returns only once every loop has,
// so no loop's command can start after it.
type Background struct {
	ctx    context.Context
	cancel context.CancelFunc
	bound  time.Duration
	after  func(time.Duration) <-chan time.Time
	log    Logger

	wg      sync.WaitGroup
	mu      sync.Mutex
	running map[string]int
}

// NewBackground answers a Background whose loops run under a context of
// PARENT's, and whose Stop waits for them up to BOUND as AFTER measures it.
func NewBackground(parent context.Context, bound time.Duration, after func(time.Duration) <-chan time.Time, log Logger) *Background {
	ctx, cancel := context.WithCancel(parent)
	return &Background{ctx: ctx, cancel: cancel, bound: bound, after: after, log: log, running: map[string]int{}}
}

// Go runs the loop NAME, LOOP, in a goroutine of its own until its context
// is done.
func (b *Background) Go(name string, loop func(context.Context)) {
	b.mu.Lock()
	b.running[name]++
	b.mu.Unlock()
	b.wg.Add(1)
	go func() {
		defer b.wg.Done()
		defer func() {
			b.mu.Lock()
			defer b.mu.Unlock()
			b.running[name]--
			if b.running[name] == 0 {
				delete(b.running, name)
			}
		}()
		loop(b.ctx)
	}()
}

// Stop cancels every loop and waits for each to return. A loop still running
// at the bound is logged by name, and Stop answers an error without waiting
// further, so the run fails: the reap that follows kills what the loop runs.
func (b *Background) Stop() error {
	b.cancel()
	joined := make(chan struct{})
	go func() {
		b.wg.Wait()
		close(joined)
	}()
	select {
	case <-joined:
		return nil
	case <-b.after(b.bound):
		err := fmt.Errorf("%s still running %s after their cancellation, past the join bound (the slowest step measured returns within %s)", b.stillRunning(), b.bound, slowestStep)
		b.log.Errorf("join the run's background loops before its reap: %v; the reap goes ahead without them and the run fails", err)
		return err
	}
}

func (b *Background) stillRunning() string {
	b.mu.Lock()
	defer b.mu.Unlock()
	names := make([]string, 0, len(b.running))
	for name := range b.running {
		names = append(names, name)
	}
	sort.Strings(names)
	return strings.Join(names, ", ")
}
