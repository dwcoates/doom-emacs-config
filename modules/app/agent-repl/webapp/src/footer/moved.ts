/**
 * moved — "workspace moved to 127.0.0.1:8123", the page's terminal line, drawn
 * in the strip's activity cell.
 *
 * THE MOVE IS READ IN THE FOOTER (owner request, 2026-11-25), not in a banner
 * over the feed: when the daemon serving this workspace runs another webapp
 * build, the lifecycle goes quiet until the host reloads the view
 * (lifecycle.ts `goQuiet`), and the footer's activity cell says where the
 * workspace went. The address is drawn because it is the ONE fact the reader
 * may need: if the host does not reload the view, this is what tells them (or
 * whoever debugs it) where the workspace actually went.
 *
 * IT IS TERMINAL, SO IT STANDS OVER EVERY OTHER LINE: the daemon's tiers, the
 * client's verdict and the hovered bubble's stamp alike. Nothing on the page
 * updates after it, so any line it covered would be stale.
 *
 * It is the page's own line, not a daemon tier: the cell wears
 * `data-tier="moved"`, and nothing about it is ever pushed. The lifecycle is
 * mounted before the footer, so the move is held here (`createWorkspaceMove`)
 * and the footer reads it on every draw.
 */
import { createListeners } from "../listeners.js";
import { createLogger } from "../log.js";
import { activityCell, replaceActivityCell } from "./activity-cell.js";
import { textLine } from "./parts.js";

const log = createLogger("webapp.footer.moved");

/** The tier the moved line's cell wears, beside the daemon's three. */
export const MOVED_TIER = "moved";

/** The kind the moved line's cell wears as its `data-arm`. */
export const MOVED_ARM = "workspace-moved";

/** Where this page's workspace moved, as the lifecycle last heard it. */
export interface WorkspaceMove {
  /** The successor's address, or null while the workspace has not moved away. */
  current(): string | null;
  /** Record that the workspace moved to ADDRESS and this page went quiet. */
  moved(address: string): void;
  /** Be told every move; answers the unsubscribe. */
  subscribe(listener: () => void): () => void;
}

/** No move, until the lifecycle records one. */
export function createWorkspaceMove(): WorkspaceMove {
  let address: string | null = null;
  const listeners = createListeners();
  return {
    current: () => address,
    moved(to) {
      log.info("the workspace moved to {address}; drawing it as the footer's activity line", {
        operation: "footer.moved",
        context: { previous: address },
      }, { address: to });
      address = to;
      listeners.notify();
    },
    subscribe(listener) {
      return listeners.add(listener);
    },
  };
}

/** The activity cell saying the workspace moved to ADDRESS. */
export function drawMovedActivity(address: string): HTMLElement {
  const text = `workspace moved to ${address}`;
  const cell = activityCell();
  cell.setAttribute("data-tier", MOVED_TIER);
  cell.setAttribute("data-arm", MOVED_ARM);
  cell.setAttribute("data-moved", address);
  cell.appendChild(textLine("footer-activity-moved", text));
  cell.title = text;
  return cell;
}

/**
 * Put the moved line for ADDRESS in STRIP's activity cell, which every strip
 * draws exactly once. A strip with none, or with more than one, is a defect.
 */
export function overlayMoved(strip: HTMLElement, address: string): void {
  replaceActivityCell(strip, drawMovedActivity(address), "footer.moved.no-cell", { address });
}
