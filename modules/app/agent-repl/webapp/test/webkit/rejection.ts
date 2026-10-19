/**
 * The error a promise a webkit suite's hook awaits was REJECTED with, or null
 * when it resolved. A hook records a refusal this way so the test body reads
 * it (vitest.webkit.config.ts: a test only reads what its hook produced).
 */
export function rejectionOf(promise: Promise<unknown>): Promise<unknown> {
  return promise.then(
    () => null,
    (err: unknown) => err,
  );
}
