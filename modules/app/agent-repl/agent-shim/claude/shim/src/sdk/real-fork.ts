/**
 * The vendor's own session fork, for ForkSession (engine/fork-session.ts).
 *
 * Routes through {@link importRealSDK}, the single vendor chokepoint: under
 * AGENT_REPL_FORBID_VENDOR_CALLS this throws rather than reaching the vendor.
 * The fork reads and writes files only and makes no model call, but it is still
 * the vendor's code, so it enters the process the one way the vendor may.
 *
 * The vendor reads the parent from, and writes the fork into, the account root
 * this process runs under (`CLAUDE_CONFIG_DIR`), beside the parent's transcript.
 */
import type { VendorForkSpec } from "../engine/fork-session.js";
import { importRealSDK } from "../vendor-guard.js";

/** Fork `spec`'s parent through the real SDK; answers the fork's vendor session id. */
export async function forkRealSession(spec: VendorForkSpec): Promise<string> {
  const sdk = await importRealSDK("forkRealSession");
  const forked = await sdk.forkSession(spec.parentSessionId, {
    dir: spec.parentDir,
    ...(spec.upToMessageId === undefined ? {} : { upToMessageId: spec.upToMessageId }),
  });
  return forked.sessionId;
}
