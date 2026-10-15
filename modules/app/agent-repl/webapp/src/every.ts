/**
 * every — run a callback every interval on a one-shot timer, rescheduled after
 * each run, so a run that overruns never stacks a second one behind it.
 */

/** Run FN once after MS; answers the cancel. */
export type After = (fn: () => void, ms: number) => () => void;

/** The page's own one-shot timer. */
export const pageAfter: After = (fn, ms) => {
  const handle = setTimeout(fn, ms);
  return () => clearTimeout(handle);
};

/** Run FN every MS through AFTER, the first time MS from now; answers the stop. */
export function every(after: After, ms: number, fn: () => void): () => void {
  let cancel = (): void => undefined;
  const schedule = (): void => {
    cancel = after(() => {
      fn();
      schedule();
    }, ms);
  };
  schedule();
  return () => cancel();
}
