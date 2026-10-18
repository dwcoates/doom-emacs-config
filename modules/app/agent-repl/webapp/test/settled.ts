/**
 * Whether PROMISE has settled once the current macrotask's work is done: every
 * microtask it was waiting on has run by then.
 */
export async function settledByNextTask(promise: Promise<unknown>): Promise<boolean> {
  let settled = false;
  void promise.then(
    () => (settled = true),
    () => (settled = true),
  );
  await new Promise((resolve) => setTimeout(resolve, 0));
  return settled;
}
