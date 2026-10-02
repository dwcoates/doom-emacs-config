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
 * The words for a NETWORK failure, read only when the vendor stated NO status:
 * the errno names {@link classifyVendorApiFailure} also reads, and the sentences
 * it does not (`fetch failed`, the API client's `Connection error.`, a timeout).
 */
const NETWORK_WORDS =
  /fetch failed|network|connection (?:error|refused|reset|closed)|timed?[\s_-]?out|socket hang up|\b(?:enotfound|eai_again|econnrefused|econnreset|etimedout|enetunreach|ehostunreach)\b/;

/**
 * The words for any OTHER transient failure, read only when the vendor stated
 * no status: an overloaded or rate-limited API, or a server error.
 */
const TRANSIENT_WORDS =
  /overloaded|rate[\s_-]?limit|too many requests|server error|service unavailable|bad gateway|gateway timeout/;

/**
 * Whether an error result that ended the session's opening can pass.
 *
 * WITH A STATUS, THE STATUS DECIDES FIRST: a transient one (408, 429, 5xx) is
 * RETRYABLE whatever the sentence says, because `classifyVendorApiFailure`
 * reads loose patterns ("resource", "auth") and a 503 that says "resource" is
 * still a server that will come back; an explicit 401/403 is REJECTED.
 *
 * WITH NO STATUS, NETWORK WORDING WINS OVER AUTH WORDING (orchestrator ruling,
 * 2026-10-02): "OAuth token refresh failed: fetch failed" is an outage that
 * happened to strike a token refresh, not a rejected credential.
 *
 * Otherwise the diagnostic kind decides: a rejected credential and a missing
 * model are REJECTED; every other API error is RETRYABLE only when it reads as
 * overloaded / server / rate-limit, and REJECTED otherwise — which is where a
 * refused resume lands.
 */
export function openingErrorVerdict(status: number | undefined, text: string): OpeningErrorVerdict {
  const kind = classifyVendorApiFailure(status, undefined, text);
  if (status !== undefined && transientStatus(status)) return { kind, retry: "retryable" };
  const said = text.toLowerCase();
  if (status === undefined && NETWORK_WORDS.test(said)) return { kind, retry: "retryable" };
  switch (kind) {
    case "shim.vendor.unreachable":
      return { kind, retry: "retryable" };
    case "shim.vendor.auth_rejected":
    case "shim.vendor.model_missing":
      return { kind, retry: "rejected" };
    case "shim.vendor.api_error":
      return { kind, retry: status === undefined && TRANSIENT_WORDS.test(said) ? "retryable" : "rejected" };
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
