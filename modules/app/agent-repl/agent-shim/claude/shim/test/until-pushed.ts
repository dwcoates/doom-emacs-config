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
import { Waiters } from "./waiters.js";

/** The waits on each hub whose push has been wrapped. */
const waiting = new WeakMap<SessionPushes, Waiters>();

function waitersOf(pushes: SessionPushes): Waiters {
  const known = waiting.get(pushes);
  if (known !== undefined) return known;
  const waiters = new Waiters("the hub");
  waiting.set(pushes, waiters);
  const push = pushes.push.bind(pushes);
  pushes.push = (update) => {
    const accepted = push(update);
    waiters.notify();
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
  return waitersOf(pushes).until(what, check);
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
