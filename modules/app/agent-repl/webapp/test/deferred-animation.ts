/**
 * deferred-animation — A WEB ANIMATION WHOSE `finished` SETTLES WHEN THE TEST
 * SAYS. jsdom runs no animations (animations.ts answers one already
 * finished), so a suite that asserts on what happens before and after an
 * animation the app started ends hands `animate` this one and settles it.
 */

/** An animation, and the test's two ways of settling its `finished`. */
export interface DeferredAnimation {
  /** The animation `animate` answers: only `finished` is real. */
  readonly animation: Animation;
  /** Let the animation run out, and wait for its `finished` handlers. */
  finish(): Promise<void>;
  /** Cancel the animation, as the engine does, and wait for its handlers. */
  cancel(): Promise<void>;
}

/** An animation whose `finished` is pending until `finish` or `cancel`. */
export function deferredAnimation(): DeferredAnimation {
  let resolve: () => void = () => undefined;
  let reject: (cause: unknown) => void = () => undefined;
  const finished = new Promise<void>((res, rej) => {
    resolve = res;
    reject = rej;
  });
  // A settled `finished` has run its handlers once both turns have passed.
  const settled = async (): Promise<void> => {
    await finished.catch(() => undefined);
    await Promise.resolve();
  };
  return {
    animation: { finished } as unknown as Animation,
    finish: async () => {
      resolve();
      await settled();
    },
    cancel: async () => {
      reject(new DOMException("cancelled", "AbortError"));
      await settled();
    },
  };
}
