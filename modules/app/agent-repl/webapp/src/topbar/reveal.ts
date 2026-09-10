/**
 * The topbar's reveal layer: one open reveal at a time, always below the strip.
 *
 * WHY A LAYER RATHER THAN A CHILD OF EACH CONTROL. Every push replaces the
 * strip WHOLE — that is the contract for every component in this app — so a
 * reveal parented to its button would be destroyed by the next topbar push,
 * which arrives whenever a token count moves. The layer is a sibling of the
 * strip that survives redraws.
 *
 * AND WHY IT REMEMBERS BY NAME. A reveal the reader opened must survive the
 * pushes that arrive while they are reading it, but its CONTENT must be the new
 * push's — a stale model list is a list of models that may no longer be
 * offered. So each control REGISTERS its reveal as it is drawn (name, the
 * anchor's name, a body builder closed over THIS push's view), and `refresh()`
 * re-opens whatever was open from the entry the new draw just registered. A
 * reveal whose control is gone from the new view — the warning chip, when the
 * warnings cleared — closes, because there is nothing left for it to belong to.
 *
 * ONE AT A TIME, because these are menus: two open reveals would overlap under
 * a strip only a few hundred pixels wide, and the reader has no way to say
 * which one they meant.
 *
 * IT CLOSES ON A CLICK OUTSIDE AND ON ESCAPE, the two gestures every dropdown
 * answers to. The click listener is on the document because the point is to
 * catch clicks the layer never sees.
 */
import { stopTicking } from "../feed/ticking.js";
import { log } from "../log.js";
import { clampReveal, type Rect } from "./clamp.js";

/** What a reveal draws, built fresh from the current view each time. */
export type RevealBody = () => HTMLElement;

/** The attribute a reveal's anchor carries, so the layer can find it again. */
export const ANCHOR_ATTRIBUTE = "data-reveal-anchor";

export interface RevealLayer {
  /** The open reveal's name, or null. */
  current(): string | null;
  /**
   * Record how NAME draws, without opening it. Every control calls this as it
   * is drawn, so a redraw can re-open whatever the reader had open.
   */
  register(name: string, anchorName: string, body: RevealBody): void;
  /** Record and open NAME, replacing whatever was open. */
  open(name: string, anchorName: string, body: RevealBody): void;
  /** Open NAME, or close it if it is already the open one. */
  toggle(name: string, anchorName: string, body: RevealBody): void;
  /** Re-open whatever is open, from the entries the latest draw registered. */
  refresh(): void;
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
 * positioning context and where the anchors live).
 */
export function mountRevealLayer(
  host: HTMLElement,
  geometry: RevealGeometry = DOM_GEOMETRY,
): RevealLayer {
  const layer = document.createElement("div");
  layer.className = "topbar-reveal-layer";
  host.append(layer);

  let openName: string | null = null;
  const entries = new Map<string, { anchorName: string; body: RevealBody }>();

  const clear = (): void => {
    // A reveal's rows may hold clock subscriptions (a detached tool's age, a
    // degraded window's "since"); dropped without this they would tick against
    // detached elements for the life of the page.
    stopTicking(layer);
    layer.replaceChildren();
  };

  const close = (): void => {
    if (openName === null) return;
    log.debug(`closing the ${openName} reveal`, {
      operation: "topbar.reveal-close",
      context: { reveal: openName },
    });
    openName = null;
    clear();
  };

  const anchorFor = (anchorName: string): HTMLElement | null =>
    host.querySelector<HTMLElement>(`[${ANCHOR_ATTRIBUTE}="${anchorName}"]`);

  /** Draw NAME's registered body under its anchor. */
  const render = (name: string): void => {
    const entry = entries.get(name);
    if (entry === undefined) {
      close();
      return;
    }
    const anchor = anchorFor(entry.anchorName);
    if (anchor === null) {
      // The control this reveal belongs to is gone from the new view. Closing
      // is the honest outcome: a menu floating under a strip that no longer
      // has the button it came from is a menu about nothing.
      log.info(`the ${name} reveal closed: its anchor is no longer drawn`, {
        operation: "topbar.reveal-anchor-gone",
        context: { reveal: name, anchor: entry.anchorName },
      });
      close();
      return;
    }
    openName = name;
    clear();
    const panel = document.createElement("div");
    panel.className = "topbar-reveal";
    panel.setAttribute("data-reveal", name);
    panel.append(entry.body());
    layer.replaceChildren(panel);

    // Measured AFTER the panel is in the document, because its own width is
    // what decides whether it has to slide left to stay on screen.
    const placement = clampReveal(
      geometry.rectOf(anchor),
      geometry.rectOf(panel),
      geometry.viewport(),
    );
    const hostRect = geometry.rectOf(host);
    // The layer sits inside the topbar's own box, so the viewport coordinates
    // the clamp works in are translated back into it.
    panel.style.left = `${placement.left - hostRect.left}px`;
    panel.style.top = `${placement.top - hostRect.top}px`;
    panel.style.maxHeight = `${placement.maxHeight}px`;
  };

  const onDocumentClick = (event: MouseEvent): void => {
    if (openName === null) return;
    const target = event.target;
    if (target instanceof Node && layer.contains(target)) return;
    // A click on an anchor reaches that anchor's own toggle handler, which runs
    // before this one bubbles here; closing unconditionally would make a second
    // click on the button close-then-reopen forever.
    if (target instanceof Element && target.closest(`[${ANCHOR_ATTRIBUTE}]`) !== null) return;
    close();
  };

  const onKeydown = (event: KeyboardEvent): void => {
    if (event.key !== "Escape") return;
    close();
  };

  document.addEventListener("click", onDocumentClick);
  document.addEventListener("keydown", onKeydown);

  return {
    current: () => openName,

    register(name, anchorName, body): void {
      entries.set(name, { anchorName, body });
    },

    open(name, anchorName, body): void {
      log.debug(`opening the ${name} reveal`, {
        operation: "topbar.reveal-open",
        context: { reveal: name },
      });
      entries.set(name, { anchorName, body });
      render(name);
    },

    toggle(name, anchorName, body): void {
      if (openName === name) {
        close();
        return;
      }
      entries.set(name, { anchorName, body });
      log.debug(`opening the ${name} reveal`, {
        operation: "topbar.reveal-open",
        context: { reveal: name },
      });
      render(name);
    },

    refresh(): void {
      if (openName === null) return;
      render(openName);
    },

    close,

    dispose(): void {
      document.removeEventListener("click", onDocumentClick);
      document.removeEventListener("keydown", onKeydown);
      openName = null;
      entries.clear();
      stopTicking(layer);
      layer.remove();
    },
  };
}
