/**
 * test/until-pushed.ts — a wait on the engine's pushes, never a poll.
 *
 * Everything a client of the engine can see changes through
 * `SessionPushes.push`: an update, and every fault opening or closing (the
 * diagnostics are pushed on each). So a test waiting for such a state judges
 * it now and again after every push, and the push is the event (the
 * agent-repl-tests skill, "Synchronization: events, never time").
 */
import type { SessionPushes } from "../src/engine/pushes.js";

type Waiter = () => void;

/** The waiters of each hub whose push has been wrapped. */
const waiting = new WeakMap<SessionPushes, Set<Waiter>>();

function waitersOf(pushes: SessionPushes): Set<Waiter> {
  const known = waiting.get(pushes);
  if (known !== undefined) return known;
  const waiters = new Set<Waiter>();
  waiting.set(pushes, waiters);
  const push = pushes.push.bind(pushes);
  pushes.push = (update) => {
    const accepted = push(update);
    for (const waiter of [...waiters]) waiter();
    return accepted;
  };
  return waiters;
}

/**
 * Resolves once CHECK holds, judged at once and after every push PUSHES
 * makes. WHAT names the state for the test that times out waiting.
 */
export function untilPushed(
  pushes: SessionPushes,
  what: string,
  check: () => boolean,
): Promise<void> {
  if (check()) return Promise.resolve();
  const waiters = waitersOf(pushes);
  return new Promise<void>((resolve, reject) => {
    const waiter = (): void => {
      let held: boolean;
      try {
        held = check();
      } catch (err) {
        // A check that THROWS is a broken test, not a state not reached yet:
        // the wait ends on it at once.
        waiters.delete(waiter);
        reject(
          new Error(`waiting for ${what}: the check threw: ${String(err)}`),
        );
        return;
      }
      if (!held) return;
      waiters.delete(waiter);
      resolve();
    };
    waiters.add(waiter);
  });
}

/**
 * Whether ASSERTION's `expect`s hold: false on an assertion failure, which is
 * the state not reached yet. Any other error is rethrown (untilPushed then
 * ends the wait on it).
 */
export function holds(assertion: () => void): boolean {
  try {
    assertion();
    return true;
  } catch (err) {
    if (err instanceof Error && err.name === "AssertionError") return false;
    throw err;
  }
}
