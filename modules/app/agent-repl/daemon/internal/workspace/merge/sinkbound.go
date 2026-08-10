package merge

import "time"

// sinkbound.go — THE ONE BOUND EVERY SINK CALL IN THIS PACKAGE TAKES.
//
// The pipeline writes to two sinks, StateSink and StatusSink, and BOTH are the
// SSM at stitch. The SSM's append serializes on a process-wide mutex shared with
// every other daemon subsystem, so a write from here waits on work this package
// neither owns nor can see.
//
// Neither call was bounded, and the observed production shape was a drain
// blocked in one of them: the merge had landed, the commits were on the target,
// and the queue head was held until the daemon was bounced. Two call sites with
// the same failure mode is one bound, extracted here, rather than two timeouts
// that could drift apart — the emitters differ in what they LOG, never in how
// long they are willing to wait.

// sinkPublishBound is how long ONE sink call may take before the pipeline stops
// waiting on it.
//
// IT IS A FAILURE BOUND, NOT A TUNED DELAY. A merge run must reach an observable
// update within two minutes, and this bound is a quarter of that so a call that
// expires still leaves its run's terminal handling time to be seen. Expiring
// records nothing and reports an error, which is the SAFE direction: an
// unpublished terminal keeps its durable queue entry and is settled under the
// replay budget, so the word is owed rather than lost.
const sinkPublishBound = 30 * time.Second

// callSinkBounded runs one sink call under bound and reports the sink's own
// error, or that the bound expired. A non-positive bound takes
// sinkPublishBound.
//
// THE CALL IS NOT ABANDONED, IT IS ONLY STOPPED BEING WAITED ON. There is no way
// to cancel a call already inside the SSM's mutex, and pretending otherwise
// would be a lie about what the timeout did. The goroutine survives until the
// sink returns and its result lands in a BUFFERED channel, so it cannot park
// forever on a send nobody is receiving any more.
//
// A call that expired is REPORTED AS FAILED even if the sink later succeeds.
// That is the honest reading: the pipeline stopped knowing, and the paths it
// feeds are built to say a word twice rather than never.
func callSinkBounded(bound time.Duration, call func() error) (err error, expired bool) {
	bound = boundOr(bound)
	done := make(chan error, 1)
	go func() { done <- call() }()
	timer := time.NewTimer(bound)
	defer timer.Stop()
	select {
	case err := <-done:
		return err, false
	case <-timer.C:
		return nil, true
	}
}

// boundOr resolves an emitter's configured bound to the one actually taken, so
// the log line naming the bound and the timer enforcing it cannot disagree.
func boundOr(bound time.Duration) time.Duration {
	if bound <= 0 {
		return sinkPublishBound
	}
	return bound
}
