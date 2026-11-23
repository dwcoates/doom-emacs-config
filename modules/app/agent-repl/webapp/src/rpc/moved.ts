/**
 * moved — the page-wide "this workspace moved" signal, and the one registration
 * that lets a refusal reach the mounted banner.
 *
 * WHY IT IS ITS OWN MODULE. `transferring_away{address}` arrives two ways: as
 * the `transferred` push on `WatchWebWorkspace`, and as a refusal answering ANY
 * per-workspace rpc. Both mean the same thing, so both must raise the same
 * page-wide notice — which the LIFECYCLE draws. If the shared refusal hook
 * (`src/rpc/refuse.ts`) imported the lifecycle directly, the rpc layer would
 * import a component that imports the rpc layer: a cycle, and a layering
 * inversion besides. So the signal lives here, at the bottom, with no imports
 * of its own beyond the logger: `startLifecycle` REGISTERS its handler, and the
 * refusal hook RAISES the signal by name.
 *
 * There is at most one page, so there is at most one handler.
 */
import { ConnectError } from "@connectrpc/connect";

import { createLogger } from "../log.js";

const log = createLogger("webapp.rpc.moved");

let moveHandler: ((address: string) => void) | null = null;

/**
 * Install HANDLER as the page's move handler. Returns its uninstaller, which
 * removes it only if it is still the installed one — so a late dispose can
 * never unregister a successor's handler.
 */
export function registerWorkspaceMoved(handler: (address: string) => void): () => void {
  moveHandler = handler;
  return () => {
    if (moveHandler === handler) moveHandler = null;
  };
}

/**
 * The workspace has moved to ADDRESS — from the push, or from any control's
 * `transferring_away` refusal.
 *
 * Answers whether a handler was installed. A `false` is logged at warn and NOT
 * swallowed: it means the page learned the workspace moved and had nowhere to
 * say so, which the reader must not be left to infer from a screen that quietly
 * stops updating.
 */
export function workspaceMoved(address: string): boolean {
  if (moveHandler === null) {
    log.warn("the workspace moved to {address} but no lifecycle is mounted to say so", {
      operation: "lifecycle.move-unhandled",
      context: {},
    }, { address });
    return false;
  }
  moveHandler(address);
  return true;
}

/** The daemon's metadata naming the successor a `transferring_away` close was handed to. */
export const SUCCESSOR_HEADER = "Agent-Repl-Successor";

/**
 * The successor's address an error names, or undefined for any other error.
 * The daemon sets it on every `transferring_away` refusal answered as an
 * error, a stream's close or a verb's alike (daemon internal/server
 * SuccessorHeader), and a page subscription's failure carries it the same way
 * (page-streams.ts). A handover is planned, never a fault: every layer that
 * meets an error asks this one reader first, so a stream, a unary call and a
 * page subscription all raise the move rather than file an outage.
 */
export function successorOf(err: unknown): string | undefined {
  if (!(err instanceof ConnectError)) return undefined;
  const address = err.metadata.get(SUCCESSOR_HEADER);
  return address === null || address === "" ? undefined : address;
}
