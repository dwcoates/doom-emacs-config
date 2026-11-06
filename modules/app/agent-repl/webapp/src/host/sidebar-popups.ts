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
 * A PRESS ANYWHERE BUT ON THE PANEL CLOSES IT (owner request, 2026-11-02),
 * as a click away from it in the rail does: a press in the host's own
 * document outside the layer, or inside a workspace page (`pressed`,
 * host/protocol.ts, which the host relays through `Host.onPagePressed`),
 * answers `dismiss`, and the sidebar lets go of every popup the way it does
 * when the user leaves the rail (sidebar/transients.ts), which hides the copy.
 *
 * A PRESS ON THE COPY IS A PRESS ON THE PANEL (owner request, 2026-11-06:
 * the row's "⋯" verb menu is drawn here, as its hover panel is). The host
 * replays every `click` and `input` on its copy to the sidebar (`click`,
 * `input`), which replays it on its own hidden panel, so the panel's controls
 * run from the one place that holds their requests and draws their answers;
 * the answer changes the panel, which the sidebar posts again. Each `show`
 * carries a revision and each gesture the revision of the copy it landed on,
 * so a gesture on a copy the sidebar has since redrawn is dropped rather than
 * landing on whatever control now holds its place. The text the reader typed
 * into the copy is the host's: a redraw puts it back, and posts it again
 * under the new revision, so the sidebar's panel holds what the copy shows.
 *
 * THE FOCUS THE PRESS TOOK. A press on the copy hands the host's web view the
 * focus, which the sidebar sees as its own `blur`; it lets go of nothing for
 * it while the host says the pointer is on the copy (`pointer`). The host
 * says so as the pointer arrives, ahead of any press there, but the notice
 * and the `blur` reach the sidebar by different paths, so it is the pointer's
 * own travel that orders them, not the channel (org/todo.md). Once the host
 * holds the focus, its own `blur` answers `dismiss` (`DISMISS_HOST_BLUR`),
 * which the sidebar heeds only while it has not taken the focus back itself:
 * a press back in the rail is the rail's to judge (`dropdowns.ts`).
 *
 * The markup is the sidebar's own drawing of the panel, from the same bundle,
 * and is drawn into the host verbatim: nothing in it comes from outside the
 * webapp's own DOM.
 */
import { pageMutationObserver, queryAll } from "../dom.js";
import { createListeners } from "../listeners.js";
import { log } from "../log.js";
import { scrollKept } from "../scroll.js";
import type { Rect } from "../topbar/clamp.js";
import { placeBeside, viewportOf } from "../placement.js";
import { detailsEntry, placeDetailsPanel, wireDetailsEntry } from "../sidebar/detail-more.js";

/** The channel both documents open, on the host origin. */
export const SIDEBAR_POPUPS_CHANNEL = "agent-repl.sidebar-popups";

/** The id the host's popup layer wears: the rail's own, so one stylesheet draws a panel in either. */
export const POPUP_LAYER_ID = "ws-sidebar";

/**
 * What the rail hands the host as a popup: a detail panel (a row's, the
 * version's memory hover) or a row's verb menu. Markup holding neither is no
 * popup.
 */
export const POPUP_PANEL_SELECTOR = ".detail, .sb-menu";

/** The class the host's popup layer wears beside the rail's id. */
export const POPUP_LAYER_CLASS = "sidebar-popups";

/** sidebar -> host: show the panel KEY at REVISION, drawn as HTML, beside the row line spanning TOP to BOTTOM. */
export interface ShowPopupMessage {
  readonly kind: "show";
  readonly key: string;
  readonly revision: number;
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

/** host -> sidebar: the reader clicked the element at PATH in the host's copy of panel KEY at REVISION. */
export interface PopupClickMessage {
  readonly kind: "click";
  readonly key: string;
  readonly revision: number;
  readonly path: readonly number[];
}

/** host -> sidebar: the text field at PATH in the host's copy of panel KEY at REVISION now holds VALUE. */
export interface PopupInputMessage {
  readonly kind: "input";
  readonly key: string;
  readonly revision: number;
  readonly path: readonly number[];
  readonly value: string;
}

/** host -> sidebar: the reader pressed somewhere outside the host's copy of panel KEY, WHY. */
export interface DismissPopupMessage {
  readonly kind: "dismiss";
  readonly key: string;
  readonly why: string;
}

export type SidebarPopupMessage = ShowPopupMessage | HidePopupMessage;

/** Every message the host sends the sidebar. */
export type HostPopupMessage = PopupPointerMessage | PopupClickMessage | PopupInputMessage | DismissPopupMessage;

/** Why the host dismissed a popup, for a press in its own document. */
export const DISMISS_HOST_PRESS = "a press in the webhost outside the popup";
/** Why the host dismissed a popup, for a press inside a workspace page. */
export const DISMISS_PAGE_PRESS = "a press in a workspace page";
/** Why the host dismissed a popup, for its own web view losing the focus a press on the copy gave it. */
export const DISMISS_HOST_BLUR = "the webhost lost the focus";

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

function isRevision(value: unknown): value is number {
  return typeof value === "number" && Number.isInteger(value) && value > 0;
}

function isPath(value: unknown): value is number[] {
  return Array.isArray(value) && value.every((i) => typeof i === "number" && Number.isInteger(i) && i >= 0);
}

/**
 * Where NODE stands under ROOT: the index of each element, among its parent's
 * element children, on the way down from ROOT; null when NODE is not under
 * ROOT. The copy is the panel's markup verbatim, so a path read in one names
 * the same element in the other.
 */
export function elementPath(root: Element, node: Node | null): number[] | null {
  const path: number[] = [];
  let at: Node | null = node instanceof Element ? node : (node?.parentElement ?? null);
  while (at !== null && at !== root) {
    const parent: Element | null = at.parentElement;
    if (parent === null) return null;
    path.unshift(Array.prototype.indexOf.call(parent.children, at));
    at = parent;
  }
  return at === root ? path : null;
}

/** The element PATH names under ROOT (`elementPath`), or null when the drawing holds none there. */
export function elementAt(root: Element, path: readonly number[]): Element | null {
  let at: Element | null = root;
  for (const index of path) {
    at = at.children.item(index);
    if (at === null) return null;
  }
  return at;
}

/** DATA as a sidebar popup message, or null for anything else. */
export function parseSidebarPopupMessage(data: unknown): SidebarPopupMessage | null {
  if (!isRecord(data) || typeof data.key !== "string" || data.key === "") return null;
  if (data.kind === "hide") return { kind: "hide", key: data.key };
  if (
    data.kind === "show" &&
    isRevision(data.revision) &&
    typeof data.html === "string" &&
    typeof data.top === "number" &&
    Number.isFinite(data.top) &&
    typeof data.bottom === "number" &&
    Number.isFinite(data.bottom)
  ) {
    return { kind: "show", key: data.key, revision: data.revision, html: data.html, top: data.top, bottom: data.bottom };
  }
  return null;
}

/** DATA as a host-to-sidebar message, or null for anything else. */
export function parseHostPopupMessage(data: unknown): HostPopupMessage | null {
  if (!isRecord(data) || typeof data.key !== "string" || data.key === "") return null;
  if (data.kind === "pointer" && typeof data.inside === "boolean") {
    return { kind: "pointer", key: data.key, inside: data.inside };
  }
  if (data.kind === "click" && isRevision(data.revision) && isPath(data.path)) {
    return { kind: "click", key: data.key, revision: data.revision, path: data.path };
  }
  if (data.kind === "input" && isRevision(data.revision) && isPath(data.path) && typeof data.value === "string") {
    return { kind: "input", key: data.key, revision: data.revision, path: data.path, value: data.value };
  }
  if (data.kind === "dismiss" && typeof data.why === "string" && data.why !== "") {
    return { kind: "dismiss", key: data.key, why: data.why };
  }
  return null;
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
  /**
   * Subscribe to every press inside a workspace page (`Host.onPagePressed`);
   * answers the stop. No page presses when omitted.
   */
  readonly onPagePressed?: (listener: () => void) => () => void;
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

  /** The panel drawn now, whichever of the rail's popups it is. */
  const panel = (): HTMLElement | null => layer.querySelector<HTMLElement>(`:scope > .ws > :is(${POPUP_PANEL_SELECTOR})`);
  const point = (inside: boolean) => (): void => {
    if (key === null) return;
    const message: PopupPointerMessage = { kind: "pointer", key, inside };
    channel.postMessage(message);
  };
  const entered = point(true);
  const left = point(false);

  /** Ask the sidebar to let go of the panel drawn now, for WHY; nothing when none is. */
  const dismiss = (why: string): void => {
    if (key === null) return;
    const message: DismissPopupMessage = { kind: "dismiss", key, why };
    channel.postMessage(message);
  };
  const onPointerDown = (event: Event): void => {
    if (event.target instanceof Node && layer.contains(event.target)) return;
    dismiss(DISMISS_HOST_PRESS);
  };
  const onBlur = (): void => {
    dismiss(DISMISS_HOST_BLUR);
  };

  /** The revision of the copy drawn now. */
  let revision = 0;
  /** Post a click on the copy to the sidebar; nothing for a click outside it. */
  const onClick = (event: Event): void => {
    const drawn = panel();
    const path = drawn === null || key === null ? null : elementPath(drawn, event.target as Node | null);
    if (path === null || key === null) return;
    const message: PopupClickMessage = { kind: "click", key, revision, path };
    channel.postMessage(message);
  };
  const postInput = (field: HTMLInputElement | HTMLTextAreaElement): void => {
    const drawn = panel();
    const path = drawn === null ? null : elementPath(drawn, field);
    if (path === null || key === null) return;
    const message: PopupInputMessage = { kind: "input", key, revision, path, value: field.value };
    channel.postMessage(message);
  };
  const onInput = (event: Event): void => {
    const field = event.target;
    if (field instanceof HTMLInputElement || field instanceof HTMLTextAreaElement) postInput(field);
  };

  /** Every text field in the copy, in drawn order. */
  const fields = (): Array<HTMLInputElement | HTMLTextAreaElement> => {
    const drawn = panel();
    return drawn === null ? [] : queryAll<HTMLInputElement | HTMLTextAreaElement>(drawn, "input, textarea");
  };
  /** What the reader typed into the copy, and where the caret stood. */
  interface KeptField {
    readonly value: string;
    readonly focused: boolean;
    readonly start: number | null;
    readonly end: number | null;
  }
  const keepFields = (): KeptField[] =>
    fields().map((field) => ({
      value: field.value,
      focused: doc.activeElement === field,
      start: field.selectionStart,
      end: field.selectionEnd,
    }));
  const restoreFields = (kept: readonly KeptField[]): void => {
    fields().forEach((field, i) => {
      const was = kept[i];
      if (was === undefined) return;
      if (was.value !== "" && field.value !== was.value) {
        field.value = was.value;
        // THE SIDEBAR'S PANEL HOLDS WHAT THE COPY SHOWS, under the revision
        // just drawn: a keystroke posted under the last one may have been
        // dropped as stale.
        postInput(field);
      }
      if (was.focused) {
        field.focus();
        if (was.start !== null && was.end !== null) field.setSelectionRange(was.start, was.end);
      }
    });
  };

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
    const kept = wasKey === message.key ? keepFields() : null;
    // ITS DETAILS POPUP STAYS OPEN ACROSS AN UPDATE TOO (sidebar/detail-more.ts):
    // the copy opens it here, the sidebar's own panel never does, so every
    // redraw would otherwise shut it under the pointer.
    const drawnBefore = panel();
    const detailsOpen =
      wasKey === message.key && drawnBefore !== null && detailsEntry(drawnBefore)?.classList.contains("open") === true;
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
    revision = message.revision;
    last = message;
    // THE ROW'S HEIGHT IN THE HOST is its height in the sidebar plus how far
    // below the host's top the sidebar's window starts, which Emacs measures.
    const placement = placeBeside(drawn, hostAnchor(message.top + offset, message.bottom + offset), viewportOf(win), measure);
    drawn.addEventListener("mouseenter", entered);
    drawn.addEventListener("mouseleave", left);
    // THE DETAILS POPUP STANDS BESIDE THIS COPY as the copy stands beside the
    // row: placed by the same `clampBeside`, against this copy's right edge.
    const placeDetails = (): void => {
      placeDetailsPanel(drawn, measure, viewportOf(win));
    };
    wireDetailsEntry(drawn, placeDetails);
    const entry = detailsEntry(drawn);
    if (detailsOpen && entry !== null && !entry.hidden) {
      entry.classList.add("open");
      placeDetails();
    }
    if (scrolls !== null) restoreScrollTops(scrolls);
    if (kept !== null) restoreFields(kept);
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
  doc.addEventListener("pointerdown", onPointerDown, { capture: true });
  win.addEventListener("blur", onBlur);
  layer.addEventListener("click", onClick);
  layer.addEventListener("input", onInput);
  const stopPagePresses = (env.onPagePressed ?? (() => () => undefined))(() => {
    dismiss(DISMISS_PAGE_PRESS);
  });
  const stopOffset = (env.onOffset ?? (() => () => undefined))((pixels) => {
    offset = pixels;
    if (last !== null && key === last.key) show(last);
  });

  return {
    shown: () => key,
    dispose() {
      stopOffset();
      stopPagePresses();
      doc.removeEventListener("pointerdown", onPointerDown, { capture: true });
      win.removeEventListener("blur", onBlur);
      layer.removeEventListener("click", onClick);
      layer.removeEventListener("input", onInput);
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
  /** Tell LISTENER, WHY, every time the host asks to let go of the open panel; answers the stop. */
  onDismissed(listener: (why: string) => void): () => void;
  /** Whether the host last said the pointer is on its copy of the open panel. */
  pointerOnCopy(): boolean;
  dispose(): void;
}

/** What the mirror needs from its environment, injectable for tests. */
export interface PopupMirrorEnv {
  readonly channel: PopupChannel;
  /** Watch EL's subtree, calling FN on every change; the page's own mutation observer when omitted. Answers the stop. */
  readonly watch?: (el: Element, fn: () => void) => () => void;
  /** Whether this page holds the focus; the document's own `hasFocus` when omitted. */
  readonly hasFocus?: () => boolean;
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
  const hasFocus = env.hasFocus ?? ((): boolean => document.hasFocus());
  let open: { key: string; detail: HTMLElement; line: HTMLElement; stop: () => void; onCopy: boolean } | null = null;
  /** The revision of the last panel posted, which every gesture on the host's copy names. */
  let revision = 0;

  const post = (): void => {
    if (open === null) return;
    const box = open.line.getBoundingClientRect();
    revision += 1;
    const message: ShowPopupMessage = {
      kind: "show",
      key: open.key,
      revision,
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

  const dismissals = createListeners<[string]>();

  // THE POINTER ON THE HOST'S COPY IS THE POINTER ON THIS PANEL: replayed as
  // the panel's own enter and leave, which the row's hover owner listens to.
  // A PRESS OUTSIDE THE COPY is the user leaving the popup: its listeners
  // (sidebar/transients.ts) let go of it.
  /** Replay the gesture MESSAGE names on the open panel's own element. */
  const replay = (detail: HTMLElement, message: PopupClickMessage | PopupInputMessage): void => {
    if (message.revision !== revision) {
      log.info("dropped a gesture on a copy of a sidebar popup the sidebar has since redrawn", {
        operation: "sidebar.popup-mirror.stale-gesture",
        context: { key: message.key, gesture: message.kind, revision: message.revision, current: revision },
      });
      return;
    }
    // THE SAME REVISION IS THE SAME MARKUP: a path that names nothing here,
    // or names the wrong kind of element, is a copy that is not this panel's.
    const el = elementAt(detail, message.path);
    const fits =
      message.kind === "click"
        ? el instanceof HTMLElement
        : el instanceof HTMLInputElement || el instanceof HTMLTextAreaElement;
    if (!fits) {
      log.error("a gesture on the webhost's copy of a sidebar popup names no such element in the panel", {
        operation: "sidebar.popup-mirror.gesture-unresolved",
        context: { key: message.key, gesture: message.kind, path: message.path.join("/"), found: el?.tagName ?? null },
      });
      throw new Error(`agent-repl: a ${message.kind} on the webhost's copy of '${message.key}' names no such element`);
    }
    log.debug("replaying a gesture on the webhost's copy of a sidebar popup", {
      operation: "sidebar.popup-mirror.gesture",
      context: { key: message.key, gesture: message.kind, path: message.path.join("/") },
    });
    if (message.kind === "click") {
      (el as HTMLElement).click();
      return;
    }
    const field = el as HTMLInputElement | HTMLTextAreaElement;
    field.value = message.value;
    field.dispatchEvent(new Event("input", { bubbles: true }));
  };

  const onMessage = (event: MessageEvent): void => {
    const message = parseHostPopupMessage(event.data);
    if (message === null || open === null || message.key !== open.key) return;
    switch (message.kind) {
      case "dismiss":
        // THE RAIL TOOK THE FOCUS BACK: the press that blurred the host is
        // the rail's own, which its dropdowns judge.
        if (message.why === DISMISS_HOST_BLUR && hasFocus()) {
          log.debug("the webhost lost the focus to the rail; the rail judges the press", {
            operation: "sidebar.popup-mirror.host-blur-to-rail",
            context: { key: message.key },
          });
          return;
        }
        dismissals.notify(message.why);
        return;
      case "pointer":
        open.onCopy = message.inside;
        open.detail.dispatchEvent(new MouseEvent(message.inside ? "mouseenter" : "mouseleave"));
        return;
      case "click":
      case "input":
        replay(open.detail, message);
        return;
    }
  };
  channel.addEventListener("message", onMessage);

  return {
    show(key, detail, line) {
      if (open !== null && open.key === key && open.detail === detail) {
        post();
        return;
      }
      close();
      open = { key, detail, line, stop: watch(detail, post), onCopy: false };
      post();
    },
    place(key) {
      if (open !== null && open.key === key) post();
    },
    hide(key) {
      if (open !== null && open.key === key) close();
    },
    onDismissed(listener) {
      return dismissals.add(listener);
    },
    pointerOnCopy() {
      return open?.onCopy ?? false;
    },
    dispose() {
      close();
      dismissals.clear();
      channel.removeEventListener("message", onMessage);
    },
  };
}
