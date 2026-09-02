/**
 * test/integration-support/redrain.ts — the safety net under every fs.watch.
 *
 * # Why this exists
 *
 * Every wait in this harness is an EVENT: the kernel says a file grew or
 * appeared, we re-read it, and any predicate that now matches settles. That is
 * still how these waits are SHAPED — the level is checked first, the watcher is
 * installed second, and the level is re-checked third, so nothing that already
 * happened is ever waited on.
 *
 * What the shape cannot cover is a kernel event that is never delivered at all.
 * On macOS `fs.watch` rides FSEvents, which coalesces and — for a file appended
 * to through a descriptor another process inherited — can drop the notification
 * outright. When that happens the wait does not fail, it HANGS, and the test
 * reports as a 60 s timeout somewhere unrelated to its subject. That is exactly
 * the ~29-failure flake bucket this file removes.
 *
 * So: the watcher remains the fast path and settles in microseconds; this
 * bounded re-drain runs alongside it and re-checks the same level condition on
 * a coarse interval, purely so a LOST event costs a few milliseconds instead of
 * the whole test budget. It is a backstop for a missing notification, never the
 * mechanism by which a wait is expected to succeed — which is why the interval
 * is coarse, is unref'd (it can never hold the process open), and is torn down
 * the moment nothing is waiting.
 */

/** How often a pending wait re-checks its level condition. */
const REDRAIN_INTERVAL_MS = 20;

/**
 * A re-check that runs while — and only while — someone is waiting.
 *
 * `start` is idempotent, so every waiter may call it; `stop` is called once the
 * last waiter has settled.
 */
export class ReDrain {
  private timer: NodeJS.Timeout | null = null;

  constructor(private readonly check: () => void) {}

  /** Begin re-checking, if not already. */
  start(): void {
    if (this.timer !== null) return;
    this.timer = setInterval(() => {
      this.check();
    }, REDRAIN_INTERVAL_MS);
    this.timer.unref();
  }

  /** Stop re-checking. */
  stop(): void {
    if (this.timer === null) return;
    clearInterval(this.timer);
    this.timer = null;
  }
}
