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
import { log } from "../log.js";

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
    log("warn", `the workspace moved to ${address} but no lifecycle is mounted to say so`, {
      operation: "lifecycle.move-unhandled",
      context: { address },
    });
    return false;
  }
  moveHandler(address);
  return true;
}
