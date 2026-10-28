/**
 * overlay — A BUBBLE EXPANDED OVER THE WHOLE WEBAPP (owner request,
 * 2026-10-28).
 *
 * A double click on a prompt or response bubble in the feed draws that
 * bubble's body large in a fixed overlay, so a table the bubble's width wraps
 * to pieces reads at the width of the window. The overlay holds a COPY of the
 * body as it stood at the click: the feed underneath is never moved, and the
 * overlay does not follow the bubble as it streams.
 *
 * IT COVERS THE SIDEBAR TOO, though the webhost draws the sidebar over every
 * page. The page cannot draw past its own iframe, so it states, on its root,
 * that an overlay stands (`BUBBLE_OVERLAY_ATTRIBUTE`); the host link
 * (host/embed.ts) reports that to the webhost, which lifts the page's iframe
 * above its sidebar while it does.
 *
 * Escape closes it, pressed here or in Emacs's composer (the host relays the
 * composer's escape as `dismissDigest`, which `closeStandingBubbleOverlay`
 * answers). A click on the scrim or on the close control closes it too.
 */
import { log } from "../log.js";

/** The attribute the page's root wears while an overlay stands. */
export const BUBBLE_OVERLAY_ATTRIBUTE = "data-bubble-overlay";

/** The classes and marks the overlay draws. */
export const BUBBLE_OVERLAY_HOST_CLASS = "expand-overlay";
export const BUBBLE_OVERLAY_PANEL_CLASS = "expand-pane";
export const BUBBLE_OVERLAY_CLOSE_ATTRIBUTE = "data-bubble-overlay-close";

/** The selector of a bubble the overlay can expand. */
const BUBBLE_SELECTOR = ".bubble[data-role]";
const BODY_SELECTOR = ".bubble-body";

/** What the page holds of the overlay. */
export interface BubbleOverlayHandle {
  /** Take the overlay down, if one stands. Answers whether one did. */
  readonly close: (cause: string) => boolean;
  readonly dispose: () => void;
}

/** Mount the overlay: a double click inside FEEDSCROLL expands the bubble under it. */
export function mountBubbleOverlay(doc: Document, feedScroll: HTMLElement): BubbleOverlayHandle {
  log.debug("mounting the bubble overlay", { operation: "bubble-overlay.mount" });
  const host = doc.createElement("div");
  host.className = BUBBLE_OVERLAY_HOST_CLASS;
  host.setAttribute("data-component", "bubble-overlay");
  host.hidden = true;
  doc.body.append(host);

  const close = (cause: string): boolean => {
    if (host.hidden) return false;
    host.hidden = true;
    host.replaceChildren();
    doc.documentElement.removeAttribute(BUBBLE_OVERLAY_ATTRIBUTE);
    log.info(`the bubble overlay closes on ${cause}`, { operation: "bubble-overlay.close", context: { cause } });
    return true;
  };

  const open = (bubble: Element, body: Element): void => {
    const role = bubble.getAttribute("data-role") ?? "";
    const panel = doc.createElement("div");
    panel.className = BUBBLE_OVERLAY_PANEL_CLASS;
    panel.setAttribute("role", "dialog");
    panel.setAttribute("data-role", role);
    panel.append(body.cloneNode(true));
    const control = doc.createElement("button");
    control.type = "button";
    control.className = "expand-close";
    control.setAttribute(BUBBLE_OVERLAY_CLOSE_ATTRIBUTE, "");
    control.setAttribute("aria-label", "close");
    control.textContent = "close";
    control.addEventListener("click", () => close("the close control"));
    host.replaceChildren(panel, control);
    host.hidden = false;
    doc.documentElement.setAttribute(BUBBLE_OVERLAY_ATTRIBUTE, role);
    log.info("a bubble is expanded over the webapp", { operation: "bubble-overlay.open", context: { role } });
  };

  const onDoubleClick = (event: MouseEvent): void => {
    if (!(event.target instanceof Element)) return;
    const bubble = event.target.closest(BUBBLE_SELECTOR);
    if (bubble === null || !feedScroll.contains(bubble)) return;
    const body = bubble.querySelector(BODY_SELECTOR);
    if (body === null) {
      log.debug("a double click on a bubble with no body expands nothing", { operation: "bubble-overlay.no-body" });
      return;
    }
    open(bubble, body);
  };
  const onHostClick = (event: MouseEvent): void => {
    if (event.target === host) close("a click on the scrim");
  };
  const onKeydown = (event: KeyboardEvent): void => {
    if (event.key !== "Escape" || host.hidden) return;
    event.preventDefault();
    close("Escape");
  };
  feedScroll.addEventListener("dblclick", onDoubleClick);
  host.addEventListener("click", onHostClick);
  doc.addEventListener("keydown", onKeydown);

  return {
    close,
    dispose(): void {
      feedScroll.removeEventListener("dblclick", onDoubleClick);
      host.removeEventListener("click", onHostClick);
      doc.removeEventListener("keydown", onKeydown);
      close("dispose");
      host.remove();
    },
  };
}

/**
 * Close the page's standing overlay, as Emacs's escape asks of it. Answers
 * whether one stood. The one dismissal path is the overlay's own close control.
 */
export function closeStandingBubbleOverlay(doc: Document): boolean {
  const control = doc.querySelector<HTMLElement>(`[${BUBBLE_OVERLAY_CLOSE_ATTRIBUTE}]`);
  if (control === null) return false;
  control.click();
  return true;
}
