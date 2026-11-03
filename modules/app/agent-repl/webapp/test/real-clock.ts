/**
 * THE REAL CLOCK, captured before anything fakes it.
 *
 * Every page a suite mounts runs on fake timers, so `Date.now()` on it is
 * whatever the page's fake clock says. A test that bounds something by REAL
 * time (a budget against a real daemon, a measured hop) reads this instead.
 *
 * `performance.now()` rather than `Date.now()`: jsdom resolves the former to
 * sub-millisecond floats, and the integration harness never fakes it. The
 * function is bound at module evaluation, which ESM guarantees happens before
 * any test body runs and so before any mount calls `vi.useFakeTimers`.
 */
export const realNow: () => number = globalThis.performance.now.bind(globalThis.performance);

/**
 * THE REAL `setImmediate`, captured at the same moment and for the same
 * reason: a mount fakes the page's, and a yield to real I/O, or a test's own
 * real event-loop turn, must not depend on whether a fake clock is installed.
 */
export const realSetImmediate: typeof globalThis.setImmediate = globalThis.setImmediate;
