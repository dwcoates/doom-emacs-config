/**
 * engine/settleable.ts — a promise and the one function that settles it, held
 * together.
 *
 * The session engine waits on facts that arrive by a different path than the
 * one waiting: the shim's own turn leaving the send slot, a stopped turn's
 * result being folded, a restarted query's stream ending. Each is a promise
 * minted where the wait starts and settled where the fact lands; this is the
 * one spelling of that pair.
 */

/** A pending promise and the function that resolves it. */
export interface Settleable<T> {
  readonly promise: Promise<T>;
  readonly resolve: (value: T) => void;
}

/** A fresh, unsettled {@link Settleable}. Resolving it again is a no-op. */
export function settleable<T = void>(): Settleable<T> {
  let resolve: (value: T) => void = () => undefined;
  const promise = new Promise<T>((settle) => {
    resolve = settle;
  });
  return { promise, resolve };
}
