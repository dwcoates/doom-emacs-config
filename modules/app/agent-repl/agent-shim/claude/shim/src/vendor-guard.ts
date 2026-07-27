/**
 * The single chokepoint through which the real Claude Agent SDK enters this
 * process.
 *
 * WHY IT EXISTS: a test that reaches the real SDK spends the user's tokens and
 * makes the suite non-hermetic and non-deterministic. Every test harness in
 * this repo therefore sets AGENT_REPL_FORBID_VENDOR_CALLS, and this module
 * turns that variable into a STRUCTURAL impossibility rather than a convention:
 * there is exactly one place that can load the SDK, and with the variable set
 * that place throws.
 *
 * The failure is LOUD by construction: {@link assertVendorCallsAllowed} throws,
 * the throw propagates out of `main()`, and the entrypoint's existing fatal
 * handler prints it to stderr and exits nonzero. It is never a silent no-op and
 * never falls back to a fake — a test that expected offline behavior must pass
 * `--fake`, and a test that reached here has a real bug.
 */

/** The environment variable that forbids real vendor calls. */
export const FORBID_VENDOR_CALLS_ENV = "AGENT_REPL_FORBID_VENDOR_CALLS";

/**
 * Thrown by {@link assertVendorCallsAllowed}. A distinct class so a unit test
 * can assert the guard fired rather than pattern-matching prose, and so the
 * fatal handler's stack print names it.
 */
export class VendorCallsForbiddenError extends Error {
  constructor(site: string) {
    super(
      `${FORBID_VENDOR_CALLS_ENV} is set: the shim is running in test mode and real ` +
        `Claude Agent SDK calls are forbidden (blocked at: ${site}). ` +
        `Run the shim with --fake for offline behavior; unset ${FORBID_VENDOR_CALLS_ENV} ` +
        `only outside tests.`,
    );
    this.name = "VendorCallsForbiddenError";
  }
}

/**
 * Refuse to proceed when the forbid variable is set to any non-empty value.
 *
 * `site` names the call path being blocked, so a failure says WHICH vendor
 * entry a test tripped instead of just that one did.
 */
export function assertVendorCallsAllowed(site: string): void {
  const flag = process.env[FORBID_VENDOR_CALLS_ENV];
  if (flag !== undefined && flag !== "") {
    throw new VendorCallsForbiddenError(site);
  }
}

/**
 * Load the real Claude Agent SDK. THE ONLY dynamic import of the vendor SDK in
 * this codebase — every other call site must route through here so the guard
 * cannot be bypassed by adding a second import.
 */
export async function importRealSDK(
  site: string,
): Promise<typeof import("@anthropic-ai/claude-agent-sdk")> {
  assertVendorCallsAllowed(site);
  return import("@anthropic-ai/claude-agent-sdk");
}
