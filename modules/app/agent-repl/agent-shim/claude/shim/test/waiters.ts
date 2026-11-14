/**
 * test/waiters.ts — the one shape of a test's wait on an event, never a poll.
 *
 * A fake that a test waits on owns one `Waiters` and calls `notify` at each
 * event that may change what a waiter waits for (a push, a write, a read, an
 * arrival). Each wait is judged at once and again at every notify, so the
 * event is what ends it (the agent-repl-tests skill, "Synchronization:
 * events, never time"). A source that can end calls `end`, which rejects
 * every pending wait at once, since nothing can satisfy it any more.
 */

interface Waiter {
  readonly what: string;
  readonly check: () => boolean;
  readonly resolve: () => void;
  readonly reject: (err: Error) => void;
}

export class Waiters {
  private readonly pending = new Set<Waiter>();
  private ended: string | undefined;

  /** SOURCE names what the waits are on, for a wait its end rejects. */
  constructor(private readonly source: string) {}

  /**
   * Resolves once CHECK holds: at once if it does, else at the notify after
   * which it does. WHAT names the state for the failure. It rejects at once
   * when the source has ended or ends, and when CHECK throws: a check that
   * throws is a broken test, not a state not reached yet.
   */
  until(what: string, check: () => boolean): Promise<void> {
    if (check()) return Promise.resolve();
    if (this.ended !== undefined) return Promise.reject(this.endedError(what, this.ended));
    return new Promise<void>((resolve, reject) => {
      this.pending.add({ what, check, resolve, reject });
    });
  }

  /** Judges every pending wait again, after an event. */
  notify(): void {
    for (const waiter of [...this.pending]) {
      let held: boolean;
      try {
        held = waiter.check();
      } catch (err) {
        this.pending.delete(waiter);
        waiter.reject(new Error(`waiting for ${waiter.what}: the check threw: ${String(err)}`));
        continue;
      }
      if (!held) continue;
      this.pending.delete(waiter);
      waiter.resolve();
    }
  }

  /** Ends the source, HOW naming why: every pending and later wait rejects. */
  end(how: string): void {
    this.ended = how;
    for (const waiter of [...this.pending]) {
      this.pending.delete(waiter);
      waiter.reject(this.endedError(waiter.what, how));
    }
  }

  private endedError(what: string, how: string): Error {
    return new Error(`waiting for ${what}: ${this.source} ended first (${how})`);
  }
}
