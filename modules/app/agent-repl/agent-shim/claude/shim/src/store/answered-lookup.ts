/**
 * store/answered-lookup.ts — THE SHAPE EVERY STORE LOOKUP SHARES: one request
 * on the read retry schedule, whose every outcome comes back as an answer.
 *
 * The lookups (store/locator.ts, store/detached-work.ts, store/peer-delivery.ts)
 * differ only in what they ask and how they read a success. What each repeats
 * is here, once:
 *
 *   - a thrown transport error is the kind the engine switches on
 *     (`sendLookup`);
 *   - the failure arm is a refused request (a shim defect, never retried) or
 *     the read failure it names (`lookupRefused`);
 *   - whatever is left after the retry schedule is the `failed` answer, never a
 *     throw, so the caller that owns the consequence writes the one record for
 *     it (`answeredLookup`).
 */
import { PersistenceError } from "./persistence.js";
import { readFailure, transportFailure } from "./reader.js";
import { readWithRetry, type ReadRetryOptions } from "./retry.js";

/** What is left of a lookup after the retry schedule: the store could not answer. */
export interface FailedLookup {
  readonly kind: "failed";
  readonly detail: string;
}

/** A lookup response's failure arm, as every lookup endpoint spells it. */
export interface LookupFailure {
  readonly detail: string;
  readonly kind:
    | { readonly case: "invalidRequest"; readonly value: { readonly field: string } }
    | { readonly case: "storageFailure" }
    | { readonly case: undefined };
}

/** Send one lookup request; a thrown transport error is the kind the engine switches on. */
export async function sendLookup<R>(send: () => Promise<R>): Promise<R> {
  try {
    return await send();
  } catch (error) {
    throw transportFailure(error);
  }
}

/**
 * A lookup's failure arm, thrown. A REQUEST THIS PROCESS BUILT, REFUSED, is a
 * shim defect and is never retried; anything else is the read failure it names.
 */
export function lookupRefused(failure: LookupFailure): never {
  if (failure.kind.case === "invalidRequest") {
    throw new PersistenceError(
      "invalid_request",
      `the store refused the lookup as malformed (field ${failure.kind.value.field}): ${failure.detail}`,
    );
  }
  throw readFailure(failure);
}

/**
 * Run ATTEMPT on the read retry schedule. Whatever is left after it is the
 * `failed` answer, never a throw.
 */
export async function answeredLookup<A>(
  rpc: string,
  attempt: () => Promise<A>,
  options: ReadRetryOptions,
): Promise<A | FailedLookup> {
  try {
    return await readWithRetry(rpc, attempt, options);
  } catch (error) {
    return { kind: "failed", detail: error instanceof Error ? error.message : String(error) };
  }
}
