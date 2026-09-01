/**
 * The topbar's reveal layer: one open reveal at a time, always below the strip.
 *
 * WHY A LAYER RATHER THAN A CHILD OF EACH CONTROL. Every push replaces the
 * strip WHOLE — that is the contract for every component in this app — so a
 * reveal parented to its button would be destroyed by the next topbar push,
 * which arrives whenever a token count moves. The layer is a sibling of the
 * strip that survives redraws, and the strip re-anchors whatever is open by
 * NAME after each one.
 *
 * ONE AT A TIME, because these are menus: two open reveals would overlap under
 * a strip only a few hundred pixels wide, and the reader has no way to say
 * which one they meant.
 *
 * IT CLOSES ON A CLICK OUTSIDE AND ON ESCAPE, the two gestures every dropdown
 * in every application answers to. The click listener is on the document
 * because the point is to catch clicks the layer never sees.
 */
import { stopTicking } from "../feed/ticking.js";
import { log } from "../log.js";
import { clampReveal, type Rect } from "./clamp.js";

/** What a reveal draws, built fresh each time it opens. */
export type RevealBody = () => HTMLElement;

export interface RevealLayer {
  /** The open reveal's name, or null. */
  current(): string | null;
  /** Open NAME under ANCHOR. Replaces whatever was open. */
  open(name: string, anchor: HTMLElement, body: RevealBody): void;
  /** Open NAME, or close it if it is already the open one. */
  toggle(name: string, anchor: HTMLElement, body: RevealBody): void;
  /** Close whatever is open. */
  close(): void;
  dispose(): void;
}

/** How the layer reads geometry; replaced in tests, where jsdom reports zeros. */
export interface RevealGeometry {
  rectOf(element: HTMLElement): Rect;
  viewport(): { width: number; height: number };
}

const DOM_GEOMETRY: RevealGeometry = {
  rectOf: (element) => element.getBoundingClientRect(),
  viewport: () => ({ width: window.innerWidth, height: window.innerHeight }),
};

/**
 * Mount the layer inside HOST (the topbar's own host, which is the reveal's
 * positioning context).
 */
export function mountRevealLayer(
  host: HTMLElement,
  geometry: RevealGeometry = DOM_GEOMETRY,
): RevealLayer {
  const layer = document.createElement("div");
  layer.className = "topbar-reveal-layer";
  host.append(layer);

  let openName: string | null = null;

  const close = (): void => {
    if (openName === null) return;
    log("debug", `closing the ${openName} reveal`, {
      operation: "topbar.reveal-close",
      context: { reveal: openName },
    });
    openName = null;
    // A reveal's rows may hold clock subscriptions (a detached tool's age, a
    // degraded window's "since"); dropped without this they would tick against
    // detached elements for the life of the page.
    stopTicking(layer);
    layer.replaceChildren();
  };

  const onDocumentClick = (event: MouseEvent): void => {
    if (openName === null) return;
    const target = event.target;
    if (target instanceof Node && layer.contains(target)) return;
    // A click on the anchor itself reaches the anchor's own toggle handler,
    // which runs BEFORE this one bubbles here; closing unconditionally would
    // make a second click on the button close-then-reopen forever.
    if (target instanceof Element && target.closest("[data-reveal-anchor]") !== null) return;
    close();
  };

  const onKeydown = (event: KeyboardEvent): void => {
    if (event.key !== "Escape") return;
    close();
  };

  document.addEventListener("click", onDocumentClick);
  document.addEventListener("keydown", onKeydown);

  const open = (name: string, anchor: HTMLElement, body: RevealBody): void => {
    log("debug", `opening the ${name} reveal`, {
      operation: "topbar.reveal-open",
      context: { reveal: name },
    });
    openName = name;
    stopTicking(layer);
    const panel = document.createElement("div");
    panel.className = "topbar-reveal";
    panel.setAttribute("data-reveal", name);
    panel.append(body());
    layer.replaceChildren(panel);

    // Measured AFTER the panel is in the document, because its width is what
    // decides whether it has to slide left.
    const placement = clampReveal(
      geometry.rectOf(anchor),
      geometry.rectOf(panel),
      geometry.viewport(),
    );
    const hostRect = geometry.rectOf(host);
    // The layer is positioned inside the topbar's own box, so the viewport
    // coordinates the clamp works in are translated back into it.
    panel.style.left = `${placement.left - hostRect.left}px`;
    panel.style.top = `${placement.top - hostRect.top}px`;
    panel.style.maxHeight = `${placement.maxHeight}px`;
  };

  return {
    current: () => openName,
    open,
    toggle(name, anchor, body): void {
      if (openName === name) {
        close();
        return;
      }
      open(name, anchor, body);
    },
    close,
    dispose(): void {
      document.removeEventListener("click", onDocumentClick);
      document.removeEventListener("keydown", onKeydown);
      openName = null;
      stopTicking(layer);
      layer.remove();
    },
  };
}
