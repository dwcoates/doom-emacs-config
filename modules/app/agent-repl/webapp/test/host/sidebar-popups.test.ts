// @vitest-environment jsdom
// The sidebar's hover popups, drawn over the feed by the host. See
// src/host/sidebar-popups.ts.
import { afterEach, describe, expect, it, onTestFinished } from "vitest";

import {
  DISMISS_HOST_PRESS,
  DISMISS_PAGE_PRESS,
  POPUP_LAYER_CLASS,
  createPopupMirror,
  hostAnchor,
  installPopupLayer,
  parseHostPopupMessage,
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

describe("parseHostPopupMessage", () => {
  it.each([
    ["a pointer message", { kind: "pointer", key: "ws-a", inside: true }],
    ["a dismiss", { kind: "dismiss", key: "ws-a", why: "a press in a workspace page" }],
  ])("reads %s", (_name, data) => {
    expect(parseHostPopupMessage(data)).toEqual(data);
  });

  it.each([
    ["a pointer message whose inside is no boolean", { kind: "pointer", key: "ws-a", inside: 1 }],
    ["a dismiss with no why", { kind: "dismiss", key: "ws-a" }],
    ["a dismiss with an empty why", { kind: "dismiss", key: "ws-a", why: "" }],
    ["a dismiss with no key", { kind: "dismiss", key: "", why: "a press" }],
    ["a sidebar message", { kind: "hide", key: "ws-a" }],
  ])("refuses %s", (_name, data) => {
    expect(parseHostPopupMessage(data)).toBeNull();
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
    let tellPagePressed: (() => void) | null = null;
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
      onPagePressed: (listener) => {
        tellPagePressed = listener;
        return () => {
          tellPagePressed = null;
        };
      },
    });
    const drawn = (): HTMLElement | null => document.querySelector<HTMLElement>(`.${POPUP_LAYER_CLASS} .detail`);
    return {
      ...fake,
      drawn,
      offset: (pixels: number) => tellOffset?.(pixels),
      pagePressed: () => tellPagePressed?.(),
      pagePressSubscribed: () => tellPagePressed !== null,
    };
  }

  /** Press the pointer down on TARGET. */
  const press = (target: EventTarget): void => {
    target.dispatchEvent(new MouseEvent("pointerdown", { bubbles: true }));
  };

  describe("a press outside the panel", () => {
    it("asks the sidebar to dismiss the panel for a press in the host's own document", () => {
      // ARRANGE
      const { deliver, posted } = install();
      deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
      // ACT
      press(document.body);
      // ASSERT
      expect(posted).toEqual([{ kind: "dismiss", key: "ws-a", why: DISMISS_HOST_PRESS }]);
    });

    it("asks the sidebar to dismiss the panel for a press inside a workspace page", () => {
      // ARRANGE
      const { deliver, posted, pagePressed } = install();
      deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
      // ACT
      pagePressed();
      // ASSERT
      expect(posted).toEqual([{ kind: "dismiss", key: "ws-a", why: DISMISS_PAGE_PRESS }]);
    });

    it("asks nothing for a press on the panel itself", () => {
      // ARRANGE
      const { deliver, posted, drawn } = install();
      deliver({ kind: "show", key: "ws-a", html: PANEL, top: 40, bottom: 62 });
      // ACT
      press(drawn()!.querySelector("dd")!);
      // ASSERT
      expect(posted).toEqual([]);
    });

    it.each([
      ["in the host's own document", "host"],
      ["inside a workspace page", "page"],
    ] as const)("asks nothing for a press %s with no panel drawn", (_name, where) => {
      // ARRANGE
      const { posted, pagePressed } = install();
      // ACT
      if (where === "host") press(document.body);
      else pagePressed();
      // ASSERT
      expect(posted).toEqual([]);
    });
  });

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

  describe("the details popup", () => {
    const WITH_DETAILS =
      '<div class="detail"><dl><dt>branch</dt><dd>main</dd></dl>' +
      '<div class="detail-more" tabindex="0"><span class="detail-more-label">details</span>' +
      '<div class="detail-sub"><div class="memory"><dl><dt>agent</dt><dd>98 MB</dd></dl></div></div></div></div>';
    const entry = (): HTMLElement | null => document.querySelector<HTMLElement>(`.${POPUP_LAYER_CLASS} .detail-more`);
    const sub = (): HTMLElement | null => document.querySelector<HTMLElement>(`.${POPUP_LAYER_CLASS} .detail-sub`);
    const hoverEntry = (): void => {
      entry()?.querySelector(".detail-more-label")?.dispatchEvent(new MouseEvent("mouseover", { bubbles: true }));
    };

    it("opens beside the copy, meeting its right edge with no gap", () => {
      // ARRANGE: every box measures 200 wide from the host's left edge.
      const { deliver } = install();
      deliver({ kind: "show", key: "ws-a", html: WITH_DETAILS, top: 40, bottom: 62 });
      // ACT
      hoverEntry();
      // ASSERT
      expect([entry()?.classList.contains("open"), sub()?.style.left]).toEqual([true, "200px"]);
    });

    it("stays open when the same panel is shown again", () => {
      // ARRANGE
      const { deliver } = install();
      deliver({ kind: "show", key: "ws-a", html: WITH_DETAILS, top: 40, bottom: 62 });
      hoverEntry();
      // ACT
      deliver({ kind: "show", key: "ws-a", html: WITH_DETAILS, top: 40, bottom: 62 });
      // ASSERT
      expect([entry()?.classList.contains("open"), sub()?.style.left]).toEqual([true, "200px"]);
    });

    it("is shut on another row's panel", () => {
      // ARRANGE
      const { deliver } = install();
      deliver({ kind: "show", key: "ws-a", html: WITH_DETAILS, top: 40, bottom: 62 });
      hoverEntry();
      // ACT
      deliver({ kind: "show", key: "ws-b", html: WITH_DETAILS, top: 80, bottom: 102 });
      // ASSERT
      expect(entry()?.classList.contains("open")).toBe(false);
    });

    it("is not reopened behind an entry the update hid", () => {
      // ARRANGE
      const { deliver } = install();
      deliver({ kind: "show", key: "ws-a", html: WITH_DETAILS, top: 40, bottom: 62 });
      hoverEntry();
      // ACT
      deliver({ kind: "show", key: "ws-a", html: WITH_DETAILS.replace('tabindex="0"', 'tabindex="0" hidden=""'), top: 40, bottom: 62 });
      // ASSERT
      expect(entry()?.classList.contains("open")).toBe(false);
    });
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

  it("stops hearing presses when disposed", () => {
    // ARRANGE
    const { posted, pagePressSubscribed } = install();
    // ACT
    layer?.dispose();
    layer = null;
    press(document.body);
    // ASSERT
    expect([posted, pagePressSubscribed()]).toEqual([[], false]);
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

  it("tells its listeners why the host dismissed the open panel", () => {
    // ARRANGE
    const { deliver, line, detail } = setUp();
    const whys: string[] = [];
    mirror?.onDismissed((why) => whys.push(why));
    mirror?.show("ws-a", detail, line);
    // ACT
    deliver({ kind: "dismiss", key: "ws-a", why: DISMISS_PAGE_PRESS });
    // ASSERT
    expect(whys).toEqual([DISMISS_PAGE_PRESS]);
  });

  it.each([
    ["another panel's", "ws-b", true],
    ["a closed panel's", "ws-a", false],
  ] as const)("tells nothing for %s dismiss", (_name, key, opened) => {
    // ARRANGE
    const { deliver, line, detail } = setUp();
    const whys: string[] = [];
    mirror?.onDismissed((why) => whys.push(why));
    if (opened) mirror?.show("ws-a", detail, line);
    // ACT
    deliver({ kind: "dismiss", key, why: DISMISS_PAGE_PRESS });
    // ASSERT
    expect(whys).toEqual([]);
  });

  it("tells nothing once the listener stopped", () => {
    // ARRANGE
    const { deliver, line, detail } = setUp();
    const whys: string[] = [];
    const stop = mirror!.onDismissed((why) => whys.push(why));
    mirror?.show("ws-a", detail, line);
    // ACT
    stop();
    deliver({ kind: "dismiss", key: "ws-a", why: DISMISS_PAGE_PRESS });
    // ASSERT
    expect(whys).toEqual([]);
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
