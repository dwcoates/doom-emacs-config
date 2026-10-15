/**
 * engine/vendor-error-retry-prompt.ts — the prompt that asks the main agent to
 * continue a turn the vendor ended on an API error after its own retries ran
 * out (engine/vendor-error-retry.ts).
 *
 * DEPENDENCY-FREE ON PURPOSE, as the network-resume prompt is: a mocked vendor
 * may read it back to answer it, and must not pull the engine in to do so.
 */

/**
 * The marker the retry prompt opens with.
 *
 * An HTML comment, as the keep-alive's and the network resume's are, so a
 * markdown surface draws nothing for it while a reader of the transcript can
 * still tell whose prompt it was.
 */
export const VENDOR_ERROR_RETRY_MARKER = "<!--agent-repl:vendor-error-retry-->";

/** The prompt for the chain's ATTEMPT-th retry (1-based). */
export function vendorErrorRetryPrompt(attempt: number): string {
  return [
    VENDOR_ERROR_RETRY_MARKER,
    `agent-repl: your last turn ended on a vendor API error after the vendor's own retries ran out. This is automatic retry ${String(attempt)}.`,
    "Continue exactly where you left off, with the same brief and the same finish.",
  ].join("\n");
}

/** Whether a prompt is the shim's own vendor-error retry prompt. */
export function isVendorErrorRetryPrompt(text: string): boolean {
  return text.trimStart().startsWith(VENDOR_ERROR_RETRY_MARKER);
}
