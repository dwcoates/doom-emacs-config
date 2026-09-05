/**
 * test/next-push.ts — the next push off a standing stream, typed and asserted.
 *
 * `AsyncIterator<T>`'s SECOND type parameter — what the iterator returns when
 * it ENDS — defaults to `any`, so `(await it.next()).value` is `any` and every
 * field a test reads off a push is unchecked. That is not theoretical: it hid
 * an assertion here reading `.id` off a `oneof` arm that does not carry one.
 *
 * This narrows on `done` instead, which is the honest question anyway: a test
 * awaiting a push wants the push, and a stream that ENDED where a push was
 * expected is a failure with its own sentence rather than an `undefined` that
 * turns into a confusing comparison three lines later.
 */

/** The next value a standing stream yields; refuses a stream that ended. */
export async function nextPush<T>(it: AsyncIterator<T>): Promise<T> {
  const result = await it.next();
  if (result.done === true) {
    throw new Error("the stream ended where a push was expected");
  }
  return result.value;
}
