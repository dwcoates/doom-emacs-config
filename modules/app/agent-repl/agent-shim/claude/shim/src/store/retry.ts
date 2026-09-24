/**
 * store/retry.ts — the READ half's retry schedule.
 *
 * THE WRITE HALF ALWAYS HAD ONE; THE READ HALF NEVER DID, and that asymmetry
 * was a defect rather than a design. A batched write that met a busy database
 * replayed from the bounded buffer and landed; a READ that met the same busy
 * database — `SQLITE_BUSY` on `begin read transaction`, which is one writer
 * holding the file for a few milliseconds — failed once, raised a
 * `store_unreachable` fault, and was never attempted again. A shim's whole
 * bring-up was refused on a contention window narrower than one backoff step.
 *
 * WHICH FAILURES ARE RETRIED, AND WHICH ARE ANSWERS. Only
 * `store_unavailable` — the arm the store's `storage_failure` and every
 * transport error map onto — is a condition another attempt could change.
 * `unknown_agent` and `stale_pointer` are the store ANSWERING: it read the
 * request and said what is true, and replaying the identical request replays
 * the identical answer. Retrying those would turn a correct refusal into a
 * multi-second stall.
 *
 * THE SCHEDULE IS THE WRITER'S. One policy for the record plane, so a store
 * outage looks the same whichever half of it met the store first.
 */
import { bindLog } from "../log.js";
import {
  DEFAULT_RETRY_POLICY,
  PersistenceError,
  type PersistenceRetryPolicy,
} from "./persistence.js";

const LOGGER = bindLog({ component: "shim-store-retry", operation: "shim.store.retry" });

/** The backoff, taken without holding the process open for it. */
const defaultSleep = (ms: number): Promise<void> =>
  new Promise((resolve) => {
    const timer = setTimeout(resolve, ms);
    timer.unref?.();
  });

/**
 * Whether another attempt could answer differently.
 *
 * THE ARM DECIDES, never the driver's text: `readFailure` has already turned
 * the store's own arm into a {@link PersistenceError} kind, and
 * `store_unavailable` is the only one that names a condition rather than an
 * answer.
 */
export function isRetryableRead(error: unknown): boolean {
  return error instanceof PersistenceError && error.kind === "store_unavailable";
}

/** What a retried read needs beyond the read itself. */
export interface ReadRetryOptions {
  /**
   * The schedule. Defaults to {@link DEFAULT_RETRY_POLICY}. A read GIVES UP at
   * `maxAttempts`, so the write half's held-batch cadence is no part of it.
   */
  readonly retry?: Pick<PersistenceRetryPolicy, "backoffMs" | "maxAttempts">;
  /** How a backoff is taken, injected so a suite does not wait in real time. */
  readonly sleep?: (ms: number) => Promise<void>;
}

/**
 * Run one store read on the retry schedule.
 *
 * The LAST failure is thrown exactly as it stood, so the caller's typed arm and
 * the driver's own text both survive the retries: a caller that must raise a
 * fault still raises it, and now only after the store really has stopped
 * answering.
 */
export async function readWithRetry<T>(
  what: string,
  read: () => Promise<T>,
  options: ReadRetryOptions = {},
): Promise<T> {
  const policy = options.retry ?? DEFAULT_RETRY_POLICY;
  const sleep = options.sleep ?? defaultSleep;
  for (let attempt = 1; ; attempt += 1) {
    try {
      const answer = await read();
      if (attempt > 1) {
        LOGGER.info(
          { read: what, attempts: attempt },
          "the store answered a read that had been failing; the retry schedule absorbed the outage",
        );
      }
      return answer;
    } catch (error) {
      if (!isRetryableRead(error) || attempt >= policy.maxAttempts) throw error;
      const backoff = policy.backoffMs[Math.min(attempt - 1, policy.backoffMs.length - 1)] ?? 0;
      LOGGER.debug(
        {
          read: what,
          attempt,
          max_attempts: policy.maxAttempts,
          backoff_ms: backoff,
          detail: error instanceof Error ? error.message : String(error),
        },
        "the store could not answer a read; replaying it on the retry schedule",
      );
      await sleep(backoff);
    }
  }
}
