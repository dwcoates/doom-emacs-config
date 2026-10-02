/**
 * Why a vendor start failed, LABELED where the shim saw it fail.
 *
 * THE SHIM IS THE SOURCE OF TRUTH FOR RETRYABILITY. The daemon owns the retry
 * loop and never re-derives the label from the refusal's `detail`
 * (`endpoint_start_session.proto`, `StartSessionVendorStartFailed`), so every
 * place that settles a pending start as failed says, at that place, whether
 * asking again can help. The catch that answers `vendor_start_failed` only
 * READS the label; it never guesses one.
 */
import { classifyVendorApiFailure, type VendorApiFailureKind } from "../convert/terminals.js";
import { bindLog } from "../log.js";
import type { VendorStartRetry } from "../service/failures.js";

const LOGGER = bindLog({ component: "shim-engine-start-failure", operation: "shim.engine.start_failure" });

/** Which bound a failed start tripped, by name and size. */
export interface VendorStartBound {
  /**
   * `live_signal`: the one control round-trip a start settles on.
   * `init_silence`: the last-resort bound on a child that answers nothing.
   */
  readonly name: "live_signal" | "init_silence";
  readonly ms: number;
}

/** A failed vendor start, carrying its retry label and the bound it tripped. */
export class VendorStartError extends Error {
  readonly retry: VendorStartRetry;
  readonly bound: VendorStartBound | undefined;

  constructor(message: string, retry: VendorStartRetry, bound?: VendorStartBound) {
    super(message);
    this.name = "VendorStartError";
    this.retry = retry;
    this.bound = bound;
  }
}

/**
 * A control round-trip that did not answer inside its bound.
 *
 * Its own class so a reader can tell "the bound tripped" from "the vendor
 * answered with a failure" without reading the sentence.
 */
export class BoundExceededError extends Error {
  readonly call: string;
  readonly boundMs: number;

  constructor(call: string, boundMs: number) {
    super(`the vendor did not answer ${call} within ${boundMs}ms`);
    this.name = "BoundExceededError";
    this.call = call;
    this.boundMs = boundMs;
  }
}

/** An opening error result's reading: the diagnostic kind and the label. */
export interface OpeningErrorVerdict {
  readonly kind: VendorApiFailureKind;
  readonly retry: VendorStartRetry;
}

/**
 * HTTP statuses that name a condition which passes: a request timeout, rate
 * limiting, and every server-side failure (529 is the vendor's "overloaded").
 */
function transientStatus(status: number): boolean {
  return status === 408 || status === 429 || (status >= 500 && status <= 599);
}

/**
 * The vendor's words for a transient failure, read only when it stated NO
 * status: an overloaded or rate-limited API, a server error, a timeout, or a
 * network failure the errno patterns of {@link classifyVendorApiFailure} do not
 * name (`fetch failed` is the SDK's own sentence for one).
 */
const TRANSIENT_WORDS =
  /overloaded|rate[\s_-]?limit|too many requests|server error|service unavailable|bad gateway|gateway timeout|timed?[\s_-]?out|fetch failed|network/;

/**
 * Whether an error result that ended the session's opening can pass.
 *
 * A TRANSIENT STATUS WINS OVER THE TEXT. `classifyVendorApiFailure` reads loose
 * text patterns ("resource", "auth"), and a 503 whose sentence happens to say
 * "resource" is still a server that will come back. Otherwise the diagnostic
 * kind decides: unreachable is a network failure and RETRYABLE; a rejected
 * credential and a missing model are REJECTED; every other API error is
 * RETRYABLE only when it reads as overloaded / server / rate-limit / network,
 * and REJECTED otherwise — which is where a refused resume lands.
 */
export function openingErrorVerdict(status: number | undefined, text: string): OpeningErrorVerdict {
  const kind = classifyVendorApiFailure(status, undefined, text);
  if (status !== undefined && transientStatus(status)) return { kind, retry: "retryable" };
  switch (kind) {
    case "shim.vendor.unreachable":
      return { kind, retry: "retryable" };
    case "shim.vendor.auth_rejected":
    case "shim.vendor.model_missing":
      return { kind, retry: "rejected" };
    case "shim.vendor.api_error":
      return {
        kind,
        retry: status === undefined && TRANSIENT_WORDS.test(text.toLowerCase()) ? "retryable" : "rejected",
      };
  }
}

/** The label a failed start answers with, and the bound it tripped if any. */
export interface StartFailureLabel {
  readonly retry: VendorStartRetry;
  readonly bound: VendorStartBound | undefined;
}

/**
 * The retry label a failed start carries, read off the error that failed it.
 *
 * EVERY SETTLE SITE LABELS ITS OWN FAILURE, so an unlabeled error here is a
 * shim defect: something threw inside the start that nobody classified. It is
 * said loudly and answered REJECTED, because silently retrying a failure nobody
 * understood is how a deterministic fault becomes a retry storm.
 */
export function startFailureLabel(err: unknown): StartFailureLabel {
  if (err instanceof VendorStartError) return { retry: err.retry, bound: err.bound };
  // warn: a defect because every path that fails a start labels its error, and this one did not
  LOGGER.warn(
    { cause: err instanceof Error ? err.message : String(err) },
    "a failed start reached the refusal with no retry label; answering it as rejected",
  );
  return { retry: "rejected", bound: undefined };
}
