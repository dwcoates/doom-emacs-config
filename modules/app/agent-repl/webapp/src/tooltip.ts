/**
 * tooltip — the webapp's delayed hover tooltip: a few words naming what an
 * element is for, shown once the pointer has rested on it for
 * `TOOLTIP_DELAY_MS` (owner request, 2026-10-23: the footer's chips).
 *
 * WHY NOT `title`. The native tooltip's delay belongs to the platform, not to
 * the page, and it is dropped whenever the element under the pointer is
 * replaced. The footer redraws its whole strip on every push, so a chip is a
 * new element many times a second while work runs, and a native tooltip (or a
 * timer armed on one chip element) would rarely live long enough to show.
 *
 * SO THE HOVER IS OWNED BY THE HOST, AND NAMED BY KEY. An element opts in with
 * `armTooltip`, which stamps a stable `data-tooltip-key` and its text. One
 * controller per host (`mountTooltips`) listens on the host, which outlives
 * every redraw, and remembers WHICH KEY the pointer rests on rather than which
 * element: a redraw that replaces the element under the pointer leaves the key,
 * the pending timer and a shown tooltip standing. After each redraw the host's
 * owner calls `refresh`, which re-reads the text from the element now carrying
 * the key and re-anchors the tooltip to it, or hides it when the key is gone.
 *
 * The tooltip is drawn on `document.body`, fixed-positioned above its anchor,
 * because the footer dock clips its own overflow.
 */
import { log } from "./log.js";
import { queryAll } from "./dom.js";

/** How long the pointer rests on an element before its tooltip shows. */
export const TOOLTIP_DELAY_MS = 1000;

/** The attribute naming an element's tooltip across redraws. */
export const TOOLTIP_KEY_ATTR = "data-tooltip-key";

/** The attribute carrying an element's tooltip text. */
export const TOOLTIP_TEXT_ATTR = "data-tooltip";

/** The gap between the tooltip and the top of its anchor, in px. */
const TOOLTIP_GAP_PX = 6;

/** Give EL the tooltip TEXT, named KEY so it survives EL being redrawn. */
export function armTooltip(el: HTMLElement, key: string, text: string): void {
  el.setAttribute(TOOLTIP_KEY_ATTR, key);
  el.setAttribute(TOOLTIP_TEXT_ATTR, text);
}

/** What the host's owner holds: the redraw hook, and the teardown. */
export interface Tooltips {
  /** Re-anchor (or hide) the tooltip after the host's content was redrawn. */
  refresh(): void;
  /** Remove the listeners, the pending timer and any shown tooltip. */
  dispose(): void;
}

/** Watch HOST for pointer rests on its armed elements, and show their tooltips. */
export function mountTooltips(host: HTMLElement): Tooltips {
  let hovered: string | null = null;
  let timer: ReturnType<typeof setTimeout> | null = null;
  let tip: HTMLElement | null = null;

  const anchorOf = (key: string): HTMLElement | undefined =>
    queryAll<HTMLElement>(host, `[${TOOLTIP_KEY_ATTR}]`).find((el) => el.getAttribute(TOOLTIP_KEY_ATTR) === key);

  const cancel = (): void => {
    if (timer === null) return;
    clearTimeout(timer);
    timer = null;
  };

  const hide = (): void => {
    cancel();
    tip?.remove();
    tip = null;
  };

  const place = (anchor: HTMLElement, shown: HTMLElement): void => {
    shown.textContent = anchor.getAttribute(TOOLTIP_TEXT_ATTR);
    const rect = anchor.getBoundingClientRect();
    shown.style.left = `${rect.left + rect.width / 2}px`;
    shown.style.top = `${rect.top - TOOLTIP_GAP_PX}px`;
  };

  const show = (key: string): void => {
    timer = null;
    const anchor = anchorOf(key);
    // The armed element went away under a resting pointer (its chip lapsed):
    // there is nothing left to explain.
    if (anchor === undefined) {
      hovered = null;
      return;
    }
    tip = document.createElement("div");
    tip.className = "app-tooltip";
    tip.setAttribute("role", "tooltip");
    tip.setAttribute(TOOLTIP_KEY_ATTR, key);
    place(anchor, tip);
    document.body.appendChild(tip);
    log.debug("showed a tooltip", {
      operation: "tooltip.show",
      context: { key, text: tip.textContent },
    });
  };

  const onOver = (event: MouseEvent): void => {
    const target = event.target instanceof Element ? event.target.closest(`[${TOOLTIP_KEY_ATTR}]`) : null;
    const key = target !== null && host.contains(target) ? target.getAttribute(TOOLTIP_KEY_ATTR) : null;
    if (key === hovered) return;
    hide();
    hovered = key;
    if (key !== null) timer = setTimeout(() => show(key), TOOLTIP_DELAY_MS);
  };

  const onLeave = (): void => {
    hide();
    hovered = null;
  };

  // A press acts on the element; its explanation has done its job. The key
  // stays hovered, so the tooltip does not come back until the pointer moves on.
  const onDown = (): void => hide();

  host.addEventListener("mouseover", onOver);
  host.addEventListener("mouseleave", onLeave);
  host.addEventListener("mousedown", onDown);

  return {
    refresh(): void {
      if (tip === null || hovered === null) return;
      const anchor = anchorOf(hovered);
      if (anchor === undefined) {
        onLeave();
        return;
      }
      place(anchor, tip);
    },
    dispose(): void {
      host.removeEventListener("mouseover", onOver);
      host.removeEventListener("mouseleave", onLeave);
      host.removeEventListener("mousedown", onDown);
      onLeave();
    },
  };
}
