// @vitest-environment jsdom
// The sidebar's hover popups, drawn over the feed by the host. See
// src/host/sidebar-popups.ts.
import { afterEach, describe, expect, it, onTestFinished } from "vitest";

import {
  POPUP_LAYER_CLASS,
  createPopupMirror,
  hostAnchor,
  installPopupLayer,
  parsePopupPointerMessage,
  parseSidebarPopupMessage,
  type PopupLayer,
  type PopupMirror,
} from "../../src/host/sidebar-popups.js";
import type { Rect } from "../../src/topbar/clamp.js";
import { REVEAL_MARGIN_PX } from "../../src/topbar/clamp.js";
import { captureLogRecords } from "../log-capture.js";
import { fakePopupChannel } from "./popup-channel.js";

/** A box of HEIGHT at the top-left, as a measured panel. */
function box(height: number, width = 200): Rect {
  return { left: 0, top: 0, right: width, bottom: height, width, height };
}

/**
 * Give every element a scrollTop that keeps what is set (jsdom's is always
 * zero), for the test it is called in; answers the restore.
 */
function keepScrollTops(): () => void {
  const original = Object.getOwnPropertyDescriptor(Element.prototype, "scrollTop");
  const kept = new WeakMap<Element, number>();
  Object.defineProperty(Element.prototype, "scrollTop", {
    configurable: true,
    get(this: Element) {
      return kept.get(this) ?? 0;
    },
    set(this: Element, value: number) {
      kept.set(this, value);
    },
  });
  return () => {
    if (original !== undefined) Object.defineProperty(Element.prototype, "scrollTop", original);
  };
}

const PANEL = '<div class="detail"><dl><dt>branch</dt><dd>main</dd></dl><div class="children"><ul><li>a</li></ul></div></div>';

let layer: PopupLayer | null = null;
let mirror: PopupMirror | null = null;

afterEach(() => {
  layer?.dispose();
  layer = null;
  mirror?.dispose();
  mirror = null;
  document.body.replaceChildren();
});

describe("parseSidebarPopupMessage", () => {
  it.each([
    ["a show", { kind: "show", key: "ws-a", html: PANEL, top: 10, bottom: 30 }],
    ["a hide", { kind: "hide", key: "ws-a" }],
  ])("reads %s", (_name, data) => {
    expect(parseSidebarPopupMessage(data)).toEqual(data);
  });

  it.each([
    ["a show with no key", { kind: "show", key: "", html: PANEL, top: 10, bottom: 30 }],
    ["a show with no markup", { kind: "show", key: "ws-a", top: 10, bottom: 30 }],
    ["a show with a top that is no number", { kind: "show", key: "ws-a", html: PANEL, top: "10", bottom: 30 }],
    ["a show with a non-finite bottom", { kind: "show", key: "ws-a", html: PANEL, top: 10, bottom: Infinity }],
    ["a kind it does not know", { kind: "move", key: "ws-a" }],
    ["no record at all", "show"],
  ])("refuses %s", (_name, data) => {
    expect(parseSidebarPopupMessage(data)).toBeNull();
  });
});

describe("parsePopupPointerMessage", () => {
  it("reads a pointer message", () => {
    expect(parsePopupPointerMessage({ kind: "pointer", key: "ws-a", inside: true })).toEqual({
      kind: "pointer",
      key: "ws-a",
      inside: true,
    });
  });

  it("refuses a pointer message whose inside is no boolean", () => {
    expect(parsePopupPointerMessage({ kind: "pointer", key: "ws-a", inside: 1 })).toBeNull();
  });

  it("refuses a sidebar message", () => {
    expect(parsePopupPointerMessage({ kind: "hide", key: "ws-a" })).toBeNull();
  });
});

describe("hostAnchor", () => {
  it("stands the row's line at the host's own left edge", () => {
    expect(hostAnchor(40, 62)).toEqual({ left: 0, right: 0, top: 40, bottom: 62, width: 0, height: 22 });
  });
});

describe("the host's popup layer", () => {
  /** Install the layer over a fake channel, its panel measured at HEIGHT, with the offset told through OFFSET. */
  function install(height = 100) {
    const fake = fakePopupChannel();
    let tellOffset: ((pixels: number) => void) | null = null;
    layer = installPopupLayer({
      doc: document,
      win: window,
      channel: fake.channel,
      measure: () => box(height),
      onOffset: (listener) => {
        tellOffset = listener;
        return () => {
          tellOffset = null;
        };
      },
    });
    const drawn = (): HTMLElement | null => document.querySelector<HTMLElement>(`.${POPUP_LAYER_CLASS} .detail`);
    return { ...fake, drawn, offset: (pixels: number) => tellOffset?.(pixels) };
  }

  it("lays its layer in the document, wearing the rail's id", () => {
    install();
    expect(document.querySelector(`#ws-sidebar.${POPUP_LAYER_CLASS}`)).not.toBeNull();
  });

  it("draws a shown panel inside an open row, so the rail's rules draw it", () => {
    const { deliver, drawn } = install();
    deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
    expect(drawn()?.parentElement?.className).toBe("ws open");
  });

  it("places a shown panel at the host's left edge, meeting the sidebar with no gap, its top on the row's line", () => {
    const { deliver, drawn } = install();
    deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
    expect([drawn()?.style.left, drawn()?.style.top]).toEqual(["0px", "40px"]);
  });

  it("slides a panel up to stand on the window's bottom edge for a row below it", () => {
    const { deliver, drawn } = install(100);
    deliver({ kind: "show", key: "ws-a", html: PANEL, top: window.innerHeight + 50, bottom: window.innerHeight + 72 });
    expect(drawn()?.style.top).toBe(`${window.innerHeight - REVEAL_MARGIN_PX - 100}px`);
  });

  it("caps a panel taller than the window so it scrolls inside itself", () => {
    const { deliver, drawn } = install(window.innerHeight * 2);
    deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
    expect(drawn()?.style.maxHeight).toBe(`${window.innerHeight - 2 * REVEAL_MARGIN_PX}px`);
  });

  it("places a panel at the row's height plus the sidebar's offset", () => {
    const { deliver, drawn, offset } = install();
    offset(12);
    deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
    expect(drawn()?.style.top).toBe("52px");
  });

  it("places the panel on screen again when the offset changes", () => {
    const { deliver, drawn, offset } = install();
    deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
    offset(-10);
    expect(drawn()?.style.top).toBe("30px");
  });

  it("keeps a scrolled panel where the reader scrolled it when the same panel is shown again", () => {
    onTestFinished(keepScrollTops());
    const { deliver, drawn } = install();
    deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
    const list = drawn()?.querySelector("ul");
    if (list) list.scrollTop = 37;
    deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
    expect(drawn()?.querySelector("ul")?.scrollTop).toBe(37);
  });

  it("draws another row's panel from its top, not where the last was scrolled", () => {
    onTestFinished(keepScrollTops());
    const { deliver, drawn } = install();
    deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
    const list = drawn()?.querySelector("ul");
    if (list) list.scrollTop = 37;
    deliver({ kind: "show", key: "ws-b", html: PANEL, top: 40, bottom: 62 });
    expect(drawn()?.querySelector("ul")?.scrollTop).toBe(0);
  });

  it("draws one panel at a time", () => {
    const { deliver } = install();
    deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
    deliver({ kind: "show", key: "ws-b", html: PANEL, top: 80, bottom: 102 });
    expect([document.querySelectorAll(`.${POPUP_LAYER_CLASS} .detail`).length, layer?.shown()]).toEqual([1, "ws-b"]);
  });

  it("takes a hidden panel off", () => {
    const { deliver, drawn } = install();
    deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
    deliver({ kind: "hide", key: "ws-a" });
    expect([drawn(), layer?.shown()]).toEqual([null, null]);
  });

  it("leaves the panel on screen when another row's panel is hidden", () => {
    const { deliver } = install();
    deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
    deliver({ kind: "hide", key: "ws-b" });
    expect(layer?.shown()).toBe("ws-a");
  });

  it.each([
    ["enters", "mouseenter", true],
    ["leaves", "mouseleave", false],
  ] as const)("tells the sidebar when the pointer %s its panel", (_name, event, inside) => {
    const { deliver, drawn, posted } = install();
    deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
    drawn()?.dispatchEvent(new MouseEvent(event));
    expect(posted).toEqual([{ kind: "pointer", key: "ws-a", inside }]);
  });

  it("records a message it cannot read at warn", () => {
    const capture = captureLogRecords();
    const { deliver } = install();
    deliver({ kind: "show", key: "ws-a" });
    capture.logger.flush();
    const record = capture.sent.find((r) => r.operation === "webhost.sidebar-popup.unreadable");
    expect(record?.level.case).toBe("warn");
  });

  it("records a shown popup with no panel in it at error, and draws nothing", () => {
    const capture = captureLogRecords();
    const { deliver, drawn } = install();
    deliver({ kind: "show", key: "ws-a", html: "<p>no panel</p>", top: 40, bottom: 62 });
    capture.logger.flush();
    const record = capture.sent.find((r) => r.operation === "webhost.sidebar-popup.no-panel");
    expect([record?.level.case, drawn(), layer?.shown()]).toEqual(["error", null, null]);
  });

  it("stops listening and removes its layer when disposed", () => {
    const { listeners } = install();
    layer?.dispose();
    layer = null;
    expect([listeners.size, document.querySelector(`.${POPUP_LAYER_CLASS}`)]).toEqual([0, null]);
  });
});

describe("the sidebar's popup mirror", () => {
  /** A row line at TOP to TOP+22 and its panel, in the document; the mirror's watch is triggered by hand. */
  function setUp(top = 40) {
    const fake = fakePopupChannel();
    const line = document.createElement("div");
    line.getBoundingClientRect = () => DOMRect.fromRect({ x: 0, y: top, width: 300, height: 22 });
    const detail = document.createElement("div");
    detail.className = "detail";
    detail.textContent = "branch main";
    document.body.append(line, detail);
    const watched = { fire: null as (() => void) | null, stopped: 0 };
    mirror = createPopupMirror({
      channel: fake.channel,
      watch: (_el, fn) => {
        watched.fire = fn;
        return () => {
          watched.stopped += 1;
        };
      },
    });
    return { ...fake, line, detail, watched };
  }

  it("hands the host the panel's markup and its row line's span", () => {
    const { posted, line, detail } = setUp(40);
    mirror?.show("ws-a", detail, line);
    expect(posted).toEqual([{ kind: "show", key: "ws-a", html: detail.outerHTML, top: 40, bottom: 62 }]);
  });

  it("hands the host the panel again whenever it changes", () => {
    const { posted, line, detail, watched } = setUp();
    mirror?.show("ws-a", detail, line);
    detail.textContent = "branch other";
    watched.fire?.();
    expect((posted[1] as { html: string }).html).toContain("branch other");
  });

  it("hands the host the panel's new place when its line moves", () => {
    const { posted, line, detail } = setUp(40);
    mirror?.show("ws-a", detail, line);
    line.getBoundingClientRect = () => DOMRect.fromRect({ x: 0, y: 90, width: 300, height: 22 });
    mirror?.place("ws-a");
    expect((posted[1] as { top: number }).top).toBe(90);
  });

  it("tells the host the panel closed, and stops watching it", () => {
    const { posted, line, detail, watched } = setUp();
    mirror?.show("ws-a", detail, line);
    mirror?.hide("ws-a");
    expect([posted.at(-1), watched.stopped]).toEqual([{ kind: "hide", key: "ws-a" }, 1]);
  });

  it("closes the last panel when another row's opens", () => {
    const { posted, line, detail } = setUp();
    mirror?.show("ws-a", detail, line);
    mirror?.show("ws-b", detail, line);
    expect(posted.map((m) => (m as { kind: string; key: string }).kind + ":" + (m as { key: string }).key)).toEqual([
      "show:ws-a",
      "hide:ws-a",
      "show:ws-b",
    ]);
  });

  it("ignores a hide of a panel that is not the one open", () => {
    const { posted, line, detail } = setUp();
    mirror?.show("ws-a", detail, line);
    mirror?.hide("ws-b");
    expect(posted).toHaveLength(1);
  });

  it.each([
    ["enters", true, "mouseenter"],
    ["leaves", false, "mouseleave"],
  ] as const)("replays the pointer that %s the host's copy on its own panel", (_name, inside, event) => {
    const { deliver, line, detail } = setUp();
    const seen: string[] = [];
    detail.addEventListener(event, () => seen.push(event));
    mirror?.show("ws-a", detail, line);
    deliver({ kind: "pointer", key: "ws-a", inside });
    expect(seen).toEqual([event]);
  });

  it("replays nothing for another row's panel", () => {
    const { deliver, line, detail } = setUp();
    const seen: string[] = [];
    detail.addEventListener("mouseenter", () => seen.push("enter"));
    mirror?.show("ws-a", detail, line);
    deliver({ kind: "pointer", key: "ws-b", inside: true });
    expect(seen).toEqual([]);
  });
});
