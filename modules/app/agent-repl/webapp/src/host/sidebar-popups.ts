/**
 * sidebar-popups — THE SIDEBAR'S HOVER POPUPS ARE DRAWN OVER THE FEED (owner
 * request, 2026-10-31).
 *
 * A row's detail panel opens to the right of the sidebar, over the feed, its
 * top on the row's own line. The sidebar is a web view of its own
 * (sidebar-page.ts), which cannot draw past its window's edge, so the host,
 * the web view in the window right of it, draws the panel: the sidebar keeps
 * its own panel hidden and hands the host a copy of it, and the row's place.
 *
 * ONE CHANNEL, NO ROUND TRIP. The two documents share the host origin, so a
 * `BroadcastChannel` reaches from one to the other directly (probed in the
 * running Emacs, 2026-10-31: a message posted in one web view arrived in the
 * other). The sidebar posts `show` with the panel's markup and the row line's
 * top and bottom, and `hide` when it closes; the host answers `pointer` as the
 * pointer enters and leaves its copy, which the sidebar replays on its own
 * hidden panel as `mouseenter`/`mouseleave`, so the row's one hover owner
 * (sidebar/row-hover.ts) keeps the panel open while the pointer is on it.
 *
 * WHERE IT LANDS. A row's height in the sidebar is its height in the host
 * plus how far below the host's top the sidebar's window starts, which Emacs
 * measures from both windows and tells the host (`Host.setSidebarOffset`;
 * zero while both run from the frame's top). The host places the copy with
 * `clampBeside` against an anchor at its own left edge: its top on the row's
 * line, sliding up only as far as the feed window's bottom edge demands, and
 * scrolling inside itself past that. A row beside the composer, below the
 * feed's window, gets its panel standing on the feed's bottom edge.
 *
 * THE PANEL MEETS THE SIDEBAR. The host's web view starts at the seam with
 * the sidebar's (Emacs measures the two windows a one-pixel divider apart), so
 * the anchor's right edge is the host's left edge and the panel's left edge is
 * zero (`besideFloor`): no margin stands between a panel and its row.
 *
 * The markup is the sidebar's own drawing of the panel, from the same bundle,
 * and is drawn into the host verbatim: nothing in it comes from outside the
 * webapp's own DOM.
 */
import { pageMutationObserver, queryAll } from "../dom.js";
import { log } from "../log.js";
import { scrollKept } from "../scroll.js";
import { clampBeside, type Rect } from "../topbar/clamp.js";

/** The channel both documents open, on the host origin. */
export const SIDEBAR_POPUPS_CHANNEL = "agent-repl.sidebar-popups";

/** The id the host's popup layer wears: the rail's own, so one stylesheet draws a panel in either. */
export const POPUP_LAYER_ID = "ws-sidebar";

/** The class the host's popup layer wears beside the rail's id. */
export const POPUP_LAYER_CLASS = "sidebar-popups";

/** sidebar -> host: show the panel KEY, drawn as HTML, beside the row line spanning TOP to BOTTOM. */
export interface ShowPopupMessage {
  readonly kind: "show";
  readonly key: string;
  readonly html: string;
  readonly top: number;
  readonly bottom: number;
}

/** sidebar -> host: the panel KEY closed. */
export interface HidePopupMessage {
  readonly kind: "hide";
  readonly key: string;
}

/** host -> sidebar: the pointer entered (INSIDE) or left the host's copy of panel KEY. */
export interface PopupPointerMessage {
  readonly kind: "pointer";
  readonly key: string;
  readonly inside: boolean;
}

export type SidebarPopupMessage = ShowPopupMessage | HidePopupMessage;

/** The part of a BroadcastChannel either side uses, injectable for tests. */
export interface PopupChannel {
  postMessage(message: unknown): void;
  addEventListener(type: "message", listener: (event: MessageEvent) => void): void;
  removeEventListener(type: "message", listener: (event: MessageEvent) => void): void;
  close(): void;
}

/** The channel on this document's origin. */
export function openPopupChannel(): PopupChannel {
  return new BroadcastChannel(SIDEBAR_POPUPS_CHANNEL);
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null;
}

/** DATA as a sidebar popup message, or null for anything else. */
export function parseSidebarPopupMessage(data: unknown): SidebarPopupMessage | null {
  if (!isRecord(data) || typeof data.key !== "string" || data.key === "") return null;
  if (data.kind === "hide") return { kind: "hide", key: data.key };
  if (
    data.kind === "show" &&
    typeof data.html === "string" &&
    typeof data.top === "number" &&
    Number.isFinite(data.top) &&
    typeof data.bottom === "number" &&
    Number.isFinite(data.bottom)
  ) {
    return { kind: "show", key: data.key, html: data.html, top: data.top, bottom: data.bottom };
  }
  return null;
}

/** DATA as a host pointer message, or null for anything else. */
export function parsePopupPointerMessage(data: unknown): PopupPointerMessage | null {
  if (!isRecord(data) || data.kind !== "pointer" || typeof data.key !== "string" || data.key === "") return null;
  return typeof data.inside === "boolean" ? { kind: "pointer", key: data.key, inside: data.inside } : null;
}

/** The anchor a panel is placed beside in the host: the row's line, at the host's own left edge. */
export function hostAnchor(top: number, bottom: number): Rect {
  return { left: 0, right: 0, top, bottom, width: 0, height: Math.max(0, bottom - top) };
}

// ---------------------------------------------------------------------------
// The host's side: the layer the copies are drawn in.
// ---------------------------------------------------------------------------

/** The host's popup layer. */
export interface PopupLayer {
  /** The key of the panel drawn now, or null. */
  shown(): string | null;
  dispose(): void;
}

/** What the layer needs from its environment, injectable for tests. */
export interface PopupLayerEnv {
  readonly doc: Document;
  readonly win: Window;
  readonly channel: PopupChannel;
  /** An element's laid-out box; `getBoundingClientRect` when omitted. */
  readonly measure?: (el: Element) => Rect;
  /**
   * Subscribe to how far below the host's top the sidebar's window starts
   * (`Host.onSidebarOffset`, which Emacs measures); answers the stop. Zero, the
   * two windows' tops level, when omitted.
   */
  readonly onOffset?: (listener: (pixels: number) => void) => () => void;
}

/** Draw the sidebar's popups over the host, from what CHANNEL carries. */
export function installPopupLayer(env: PopupLayerEnv): PopupLayer {
  const { doc, win, channel } = env;
  const measure = env.measure ?? ((el: Element) => el.getBoundingClientRect());
  const layer = doc.createElement("nav");
  layer.id = POPUP_LAYER_ID;
  layer.className = POPUP_LAYER_CLASS;
  doc.body.appendChild(layer);
  let key: string | null = null;
  /** The last panel shown, so a changed offset places it again. */
  let last: ShowPopupMessage | null = null;
  let offset = 0;

  const panel = (): HTMLElement | null => layer.querySelector<HTMLElement>(".detail");
  const point = (inside: boolean) => (): void => {
    if (key === null) return;
    const message: PopupPointerMessage = { kind: "pointer", key, inside };
    channel.postMessage(message);
  };
  const entered = point(true);
  const left = point(false);

  /** The scroll offsets of the panel and every scroller in it, in drawn order. */
  const scrollTops = (): number[] => {
    const drawn = panel();
    return drawn === null ? [] : [drawn, ...queryAll<HTMLElement>(drawn, "*")].map((el) => el.scrollTop);
  };
  const restoreScrollTops = (tops: readonly number[]): void => {
    const drawn = panel();
    if (drawn === null) return;
    [drawn, ...queryAll<HTMLElement>(drawn, "*")].forEach((el, i) => {
      const top = tops[i];
      if (top !== undefined && top > 0) scrollKept(el, top);
    });
  };

  const clear = (): void => {
    const drawn = panel();
    drawn?.removeEventListener("mouseenter", entered);
    drawn?.removeEventListener("mouseleave", left);
    layer.replaceChildren();
    key = null;
  };

  const show = (message: ShowPopupMessage): void => {
    const wasKey = key;
    // AN UPDATE OF THE PANEL ON SCREEN KEEPS WHERE THE READER SCROLLED IT: the
    // memory block refreshes while the panel is open, and a redraw that reset
    // a scrolled children list would snatch it back to the top.
    const scrolls = wasKey === message.key ? scrollTops() : null;
    clear();
    // THE ROW'S OWN CLASSES around the copy, so the stylesheet's rules for an
    // open row's panel (`#ws-sidebar .ws.open > .detail`) draw it here too.
    const row = doc.createElement("div");
    row.className = "ws open";
    row.innerHTML = message.html;
    layer.appendChild(row);
    const drawn = panel();
    if (drawn === null) {
      log.error("the sidebar handed the webhost a popup with no panel in it", {
        operation: "webhost.sidebar-popup.no-panel",
        context: { key: message.key },
      });
      layer.replaceChildren();
      return;
    }
    key = message.key;
    drawn.style.maxHeight = "";
    last = message;
    // THE ROW'S HEIGHT IN THE HOST is its height in the sidebar plus how far
    // below the host's top the sidebar's window starts, which Emacs measures.
    const placement = clampBeside(hostAnchor(message.top + offset, message.bottom + offset), measure(drawn), {
      width: win.innerWidth,
      height: win.innerHeight,
    });
    drawn.style.left = `${placement.left}px`;
    drawn.style.top = `${placement.top}px`;
    drawn.style.maxHeight = `${placement.maxHeight}px`;
    drawn.addEventListener("mouseenter", entered);
    drawn.addEventListener("mouseleave", left);
    if (scrolls !== null) restoreScrollTops(scrolls);
    if (wasKey !== message.key) {
      log.debug("the webhost drew a sidebar popup over the feed", {
        operation: "webhost.sidebar-popup.show",
        verbosity: "verbose",
        context: { key: message.key, top: placement.top, max_height: placement.maxHeight },
      });
    }
  };

  const onMessage = (event: MessageEvent): void => {
    const message = parseSidebarPopupMessage(event.data);
    if (message === null) {
      log.warn("the webhost could not read a sidebar popup message", {
        operation: "webhost.sidebar-popup.unreadable",
        context: { data: JSON.stringify(event.data) },
      });
      return;
    }
    if (message.kind === "show") show(message);
    else if (message.key === key) {
      clear();
      last = null;
    }
  };
  channel.addEventListener("message", onMessage);
  const stopOffset = (env.onOffset ?? (() => () => undefined))((pixels) => {
    offset = pixels;
    if (last !== null && key === last.key) show(last);
  });

  return {
    shown: () => key,
    dispose() {
      stopOffset();
      channel.removeEventListener("message", onMessage);
      clear();
      layer.remove();
    },
  };
}

// ---------------------------------------------------------------------------
// The sidebar's side: the mirror that hands its panels over.
// ---------------------------------------------------------------------------

/** The sidebar's handing-over of its open row panels. */
export interface PopupMirror {
  /** Panel DETAIL of row KEY opened beside its line LINE: draw it over the feed, and keep it current. */
  show(key: string, detail: HTMLElement, line: HTMLElement): void;
  /** Row KEY's panel moved with its line (a scroll, a resize): place it again. */
  place(key: string): void;
  /** Row KEY's panel closed. */
  hide(key: string): void;
  dispose(): void;
}

/** What the mirror needs from its environment, injectable for tests. */
export interface PopupMirrorEnv {
  readonly channel: PopupChannel;
  /** Watch EL's subtree, calling FN on every change; the page's own mutation observer when omitted. Answers the stop. */
  readonly watch?: (el: Element, fn: () => void) => () => void;
}

function observeSubtree(el: Element, fn: () => void): () => void {
  // THE PAGE'S OWN OBSERVER (dom.ts), disconnected as the page hides.
  const observer = pageMutationObserver(el.ownerDocument, fn);
  observer.observe(el, { subtree: true, childList: true, characterData: true, attributes: true });
  return () => observer.disconnect();
}

/** Hand the sidebar's open panels to the host over CHANNEL. */
export function createPopupMirror(env: PopupMirrorEnv): PopupMirror {
  const { channel } = env;
  const watch = env.watch ?? observeSubtree;
  let open: { key: string; detail: HTMLElement; line: HTMLElement; stop: () => void } | null = null;

  const post = (): void => {
    if (open === null) return;
    const box = open.line.getBoundingClientRect();
    const message: ShowPopupMessage = {
      kind: "show",
      key: open.key,
      html: open.detail.outerHTML,
      top: box.top,
      bottom: box.bottom,
    };
    channel.postMessage(message);
  };

  const close = (): void => {
    if (open === null) return;
    open.stop();
    const message: HidePopupMessage = { kind: "hide", key: open.key };
    channel.postMessage(message);
    open = null;
  };

  // THE POINTER ON THE HOST'S COPY IS THE POINTER ON THIS PANEL: replayed as
  // the panel's own enter and leave, which the row's hover owner listens to.
  const onMessage = (event: MessageEvent): void => {
    const message = parsePopupPointerMessage(event.data);
    if (message === null || open === null || message.key !== open.key) return;
    open.detail.dispatchEvent(new MouseEvent(message.inside ? "mouseenter" : "mouseleave"));
  };
  channel.addEventListener("message", onMessage);

  return {
    show(key, detail, line) {
      if (open !== null && open.key === key && open.detail === detail) {
        post();
        return;
      }
      close();
      open = { key, detail, line, stop: watch(detail, post) };
      post();
    },
    place(key) {
      if (open !== null && open.key === key) post();
    },
    hide(key) {
      if (open !== null && open.key === key) close();
    },
    dispose() {
      close();
      channel.removeEventListener("message", onMessage);
    },
  };
}
