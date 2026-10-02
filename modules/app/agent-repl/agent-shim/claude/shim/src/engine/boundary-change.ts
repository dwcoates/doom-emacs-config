/**
 * engine/boundary-change.ts — a session change that waits for the running
 * turn to end, and the caller still waiting on it.
 *
 * SetSessionModel and SetSessionEffort share the rule: a turn runs on ONE
 * model at ONE effort throughout, so a change asked for mid-turn lands at the
 * turn boundary and its call resolves only then. A later change during the
 * same turn REPLACES the earlier one, whose caller is answered rather than
 * left holding a promise nothing will settle; a stand-down answers whatever
 * is waiting. This is the one spelling of that slot.
 */

/** The waiting change: what was asked for, and the call to answer. */
export interface WaitingChange<V, R> {
  readonly value: V;
  readonly resolve: (response: R) => void;
}

/** One boundary-deferred change slot. */
export interface BoundaryChange<V, R> {
  /** Wait for VALUE, answering any earlier waiter with SUPERSEDED first. */
  wait(value: V, superseded: R): Promise<R>;
  /** Take the waiting change, emptying the slot; undefined when none waits. */
  take(): WaitingChange<V, R> | undefined;
}

/** A fresh, empty slot. */
export function boundaryChange<V, R>(): BoundaryChange<V, R> {
  let waiting: WaitingChange<V, R> | undefined;
  return {
    wait(value: V, superseded: R): Promise<R> {
      waiting?.resolve(superseded);
      return new Promise<R>((resolve) => {
        waiting = { value, resolve };
      });
    },
    take(): WaitingChange<V, R> | undefined {
      const taken = waiting;
      waiting = undefined;
      return taken;
    },
  };
}
