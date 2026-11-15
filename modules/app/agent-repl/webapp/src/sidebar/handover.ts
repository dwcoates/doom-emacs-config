/**
 * handover — WHICH DAEMON SERVES A WORKSPACE WHILE A HANDOVER MOVES IT, as the
 * web view host's sidebar reads it off a refusal.
 *
 * A handover moves workspaces from one daemon to its successor one at a time,
 * and Emacs points the host at the successor (lisp/webhost.el, `setDaemon`) on
 * its own schedule. So a sidebar call about one workspace can reach either end
 * of the handover before that workspace has moved, and both ends name the one
 * that serves it:
 *
 *   - `not_yet_adopted{address}`: the host already dials the successor, and
 *     the predecessor named by ADDRESS (when any) still serves the workspace.
 *   - `transferring_away{address}`: the host still dials the predecessor, and
 *     the successor at ADDRESS now serves the workspace.
 *
 * NEITHER IS A MOVE OF THE SIDEBAR. A workspace page's lifecycle draws
 * `transferring_away` as the page's terminal "this workspace moved" notice
 * (rpc/moved.ts), because that page belongs to the one workspace. The sidebar
 * belongs to no workspace and mounts no lifecycle: it asks the daemon the arm
 * names, and otherwise treats the arm as the lifecycle state it is.
 */
import type { DaemonContext } from "../rpc/context.js";
import { MalformedView } from "../rpc/malformed.js";
import type { UnaryContext } from "../rpc/unary.js";

/** The two arms by which a daemon says a handover moves the workspace. */
export const HANDOVER_ARMS = ["notYetAdopted", "transferringAway"] as const;

/** One of the handover arms. */
export type HandoverArm = (typeof HANDOVER_ARMS)[number];

/** Whether ARM is one of the handover arms. */
export function isHandoverArm(arm: string): arm is HandoverArm {
  return (HANDOVER_ARMS as readonly string[]).includes(arm);
}

/**
 * Whether ARM stands as the handover state on CTX's document rather than as
 * that document's own refusal: `not_yet_adopted` always does, and
 * `transferring_away` does only on a document that can dial the successor
 * (the web view host's, which has `clientAt`). A workspace page belongs to
 * the one workspace, so there `transferring_away` is its terminal "this
 * workspace moved" notice (rpc/moved.ts), never a quiet state.
 */
export function standsAsHandover(arm: string, ctx: DaemonContext): arm is HandoverArm {
  return arm === "notYetAdopted" || (arm === "transferringAway" && ctx.clientAt !== undefined);
}

/**
 * The address of the daemon a handover arm names as serving the workspace,
 * or null when CAUSE is no handover arm or names none.
 *
 * `transferring_away` always names its successor, so an arm with no address
 * is a malformed view and throws; `not_yet_adopted` names a predecessor only
 * while one still serves the workspace. PATH is the cause's wire path, for
 * the malformed view's record.
 */
export function handoverAddress(path: string, cause: { case?: string; value?: unknown }): string | null {
  if (cause.case === undefined || !isHandoverArm(cause.case)) return null;
  const address = (cause.value as { address?: unknown } | undefined)?.address;
  if (cause.case === "transferringAway" && (typeof address !== "string" || address === "")) {
    throw new MalformedView(`${path}.transferringAway.address`, `expected the successor's address, got ${JSON.stringify(address)}`);
  }
  return typeof address === "string" && address !== "" ? address : null;
}

/**
 * The context a call is asked again on, at the daemon at ADDRESS, or null on
 * a document that cannot dial another daemon (only the web view host's
 * context has `clientAt`). It goes quiet with CTX.
 */
export function contextAt(ctx: DaemonContext, address: string): UnaryContext | null {
  if (ctx.clientAt === undefined) return null;
  return { client: ctx.clientAt(address), failures: ctx.failures, isQuiesced: () => ctx.isQuiesced() };
}
