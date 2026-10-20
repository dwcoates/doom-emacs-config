/**
 * engine/shim-prompt.ts — the shape of a prompt the SHIM authors and delivers
 * as the main agent's next turn (engine/session.ts `deliverShimPrompt`): the
 * network resume's (engine/network-resume.ts) and the vendor-error retry's
 * (engine/vendor-error-retry.ts). Each caller names itself with a
 * {@link ShimPromptSource}; the delivery itself is one function, so the two
 * cannot drift in how they take the send slot.
 */

/** What delivering one shim-authored prompt came to. */
export type ShimPromptDelivery =
  /** The prompt opened the shim turn `turnId`. */
  | { readonly kind: "delivered"; readonly turnId: string }
  /** The main agent is working; the prompt waits for it to be idle. */
  | { readonly kind: "busy"; readonly detail: string }
  /** The session can never take the prompt (no query, a dead session). */
  | { readonly kind: "unavailable"; readonly detail: string };

/** Which shim-authored prompt is being delivered, for its adopted row and its records. */
export interface ShimPromptSource {
  /** Why the shim opened the turn: the adopted prompt record's `cause`. */
  readonly cause: string;
  /** The adopted prompt record's `first_message`. */
  readonly firstMessage: string;
  /** The prompt's name on the delivery records (`prompt`). */
  readonly label: string;
}

/** The network resume's source. */
export const NETWORK_RESUME_SOURCE: ShimPromptSource = {
  cause: "the shim asked the main agent to resume network-killed agents",
  firstMessage: "network_resume",
  label: "network-resume",
};

/** The vendor-error retry's source. */
export const VENDOR_ERROR_RETRY_SOURCE: ShimPromptSource = {
  cause: "the shim asked the main agent to continue a turn the vendor failed",
  firstMessage: "vendor_error_retry",
  label: "vendor-error-retry",
};
