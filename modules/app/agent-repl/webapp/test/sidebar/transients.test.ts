// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createDropdowns, type Dropdowns } from "../../src/sidebar/dropdowns.js";
import { createRowHover, type RowHover } from "../../src/sidebar/row-hover.js";
import { HOVER_SUSPENDED_CLASS, installTransients, type Transients } from "../../src/sidebar/transients.js";
import { log } from "../../src/log.js";
import { createListeners } from "../../src/listeners.js";
import { FOCUS_LEFT_BLUR, FOCUS_LEFT_EMACS, FOCUS_LEFT_HIDDEN } from "../../src/focus-left.js";
import { DISMISS_PAGE_PRESS } from "../../src/host/sidebar-popups.js";
import stylesheet from "../../src/styles.css?raw";
import { rulesOf } from "../stylesheet.js";

describe("the rail's transients", () => {
  let host: HTMLElement;
  let dropdowns: Dropdowns;
  let rowHover: RowHover;
  let transients: Transients;
  let closes: string[];
  /** The page's focus-left signal, fired by hand. */
  let focusLeft: ReturnType<typeof createListeners<[string]>>;
  /** The popup mirror's dismissals, fired by hand. */
  let dismissals: ReturnType<typeof createListeners<[string]>>;
  /** Whether the popup mirror says the pointer is on the host's copy. */
  let onCopy: boolean;

  beforeEach(() => {
    host = document.createElement("nav");
    document.body.appendChild(host);
    dropdowns = createDropdowns();
    rowHover = createRowHover(dropdowns, new Set());
    focusLeft = createListeners<[string]>();
    dismissals = createListeners<[string]>();
    onCopy = false;
    transients = installTransients(host, {
      dropdowns,
      rowHover,
      focusLeft: { onLeft: (listener) => focusLeft.add(listener) },
      popupMirror: { onDismissed: (listener) => dismissals.add(listener), pointerOnCopy: () => onCopy },
    });
    closes = [];
  });

  /** The focus leaves the page, WHY. */
  const leave = (why: string = FOCUS_LEFT_BLUR): void => {
    focusLeft.notify(why);
  };

  afterEach(() => {
    transients.dispose();
    rowHover.dispose();
    vi.restoreAllMocks();
    document.body.replaceChildren();
  });

  /** Open a dropdown in the rail that records why it closed. */
  function openDropdown(): void {
    dropdowns.opened({
      kind: "test",
      element: document.createElement("div"),
      openers: [],
      close: (why) => closes.push(why),
    });
  }

  /** Move the pointer to (X, Y) over the rail. */
  function move(x: number, y: number): void {
    host.dispatchEvent(new MouseEvent("mousemove", { bubbles: true, clientX: x, clientY: y }));
  }

  const suspended = (): boolean => host.classList.contains(HOVER_SUSPENDED_CLASS);

  describe("the page losing focus to the popup over the feed", () => {
    it("closes no dropdown while the pointer is on the host's copy", () => {
      // ARRANGE
      openDropdown();
      onCopy = true;
      // ACT
      leave();
      // ASSERT
      expect(closes).toEqual([]);
    });

    it("leaves the hover highlight answering", () => {
      // ARRANGE
      onCopy = true;
      // ACT
      leave();
      // ASSERT
      expect(suspended()).toBe(false);
    });

    it("records that it let go of nothing", () => {
      // ARRANGE
      const debug = vi.spyOn(log, "debug");
      onCopy = true;
      // ACT
      leave();
      // ASSERT
      expect(debug).toHaveBeenCalledWith(expect.any(String), {
        operation: "sidebar.transients.focus-to-popup",
        context: { why: FOCUS_LEFT_BLUR },
      });
    });

    it("still closes every dropdown for Emacs moving its focus", () => {
      // ARRANGE
      openDropdown();
      onCopy = true;
      // ACT
      leave(FOCUS_LEFT_EMACS);
      // ASSERT
      expect(closes).toEqual([FOCUS_LEFT_EMACS]);
    });
  });

  describe("the page losing focus", () => {
    it("closes every open dropdown", () => {
      // ARRANGE
      openDropdown();
      // ACT
      leave();
      // ASSERT
      expect(closes).toEqual(["the page lost focus"]);
    });

    it("closes the row panels", () => {
      // ARRANGE
      const dismiss = vi.spyOn(rowHover, "dismiss");
      // ACT
      leave();
      // ASSERT
      expect(dismiss).toHaveBeenCalledWith("the page lost focus");
    });

    it("suspends the hover highlight", () => {
      // ACT
      leave();
      // ASSERT
      expect(suspended()).toBe(true);
    });

    it("logs the dismissal with why", () => {
      // ARRANGE
      const debug = vi.spyOn(log, "debug");
      // ACT
      leave();
      // ASSERT
      expect(debug).toHaveBeenCalledWith("the user left the rail; closing everything the pointer raised in it", {
        operation: "sidebar.transients.dismiss",
        context: { why: "the page lost focus" },
      });
    });
  });

  describe("the focus leaving the page for another reason", () => {
    it.each([
      ["the page hidden", FOCUS_LEFT_HIDDEN],
      ["Emacs moving its focus, such as to its composer", FOCUS_LEFT_EMACS],
    ])("dismisses for %s", (_name, why) => {
      // ARRANGE
      openDropdown();
      // ACT
      leave(why);
      // ASSERT
      expect([closes, suspended()]).toEqual([[why], true]);
    });
  });

  describe("the webhost dismissing a popup over the feed", () => {
    it("closes every open dropdown", () => {
      // ARRANGE
      openDropdown();
      // ACT
      dismissals.notify(DISMISS_PAGE_PRESS);
      // ASSERT
      expect([closes, suspended()]).toEqual([[DISMISS_PAGE_PRESS], true]);
    });

    it("closes the row panels", () => {
      // ARRANGE
      const dismiss = vi.spyOn(rowHover, "dismiss");
      // ACT
      dismissals.notify(DISMISS_PAGE_PRESS);
      // ASSERT
      expect(dismiss).toHaveBeenCalledWith(DISMISS_PAGE_PRESS);
    });
  });

  describe("the selection", () => {
    it("dismisses nothing on the first selection", () => {
      // ARRANGE
      openDropdown();
      // ACT
      transients.selected("ws-1");
      // ASSERT
      expect(closes).toEqual([]);
    });

    it("dismisses nothing when the same workspace is selected again", () => {
      // ARRANGE
      transients.selected("ws-1");
      openDropdown();
      // ACT
      transients.selected("ws-1");
      // ASSERT
      expect(closes).toEqual([]);
    });

    it("dismisses when the selection moves to another workspace", () => {
      // ARRANGE
      transients.selected("ws-1");
      openDropdown();
      // ACT
      transients.selected("ws-2");
      // ASSERT
      expect([closes, suspended()]).toEqual([["the selection moved"], true]);
    });

    it("dismisses when the selection moves to none", () => {
      // ARRANGE
      transients.selected("ws-1");
      openDropdown();
      // ACT
      transients.selected(null);
      // ASSERT
      expect(closes).toEqual(["the selection moved"]);
    });
  });

  describe("the hover highlight's suspension", () => {
    it("lifts on a pointer move away from where it was suspended", () => {
      // ARRANGE
      move(10, 10);
      leave();
      // ACT
      move(11, 10);
      // ASSERT
      expect(suspended()).toBe(false);
    });

    it("stays for a move WebKit synthesizes where the pointer rests", () => {
      // ARRANGE
      move(10, 10);
      leave();
      // ACT
      move(10, 10);
      // ASSERT
      expect(suspended()).toBe(true);
    });

    it("lifts on any move when the pointer was never seen in the rail", () => {
      // ARRANGE
      leave();
      // ACT
      move(10, 10);
      // ASSERT
      expect(suspended()).toBe(false);
    });

    it("lifts on a key pressed in the rail", () => {
      // ARRANGE
      move(10, 10);
      leave();
      // ACT
      host.dispatchEvent(new KeyboardEvent("keydown", { bubbles: true, key: "Tab" }));
      // ASSERT
      expect(suspended()).toBe(false);
    });

    it("lets the row panels answer again when it lifts", () => {
      // ARRANGE
      leave();
      const resume = vi.spyOn(rowHover, "resume");
      // ACT
      move(10, 10);
      // ASSERT
      expect(resume).toHaveBeenCalledTimes(1);
    });

    it("logs the lift with what lifted it", () => {
      // ARRANGE
      leave();
      const debug = vi.spyOn(log, "debug");
      // ACT
      move(10, 10);
      // ASSERT
      expect(debug).toHaveBeenCalledWith("the user is back in the rail; hover answers again", {
        operation: "sidebar.transients.resume",
        context: { via: "pointer" },
      });
    });

    it("resumes nothing on a move while nothing is suspended", () => {
      // ARRANGE
      const resume = vi.spyOn(rowHover, "resume");
      // ACT
      move(10, 10);
      // ASSERT
      expect(resume).not.toHaveBeenCalled();
    });

    it("still tells the row gesture where the pointer is on every move", () => {
      // ARRANGE
      const pointerAt = vi.spyOn(rowHover, "pointerAt");
      // ACT
      move(10, 10);
      // ASSERT
      expect(pointerAt).toHaveBeenCalledWith(host);
    });
  });

  describe("the pointer leaving the rail", () => {
    it("suspends the hover highlight", () => {
      // ACT
      host.dispatchEvent(new MouseEvent("mouseleave"));
      // ASSERT
      expect(suspended()).toBe(true);
    });

    it("closes no dropdown", () => {
      // ARRANGE
      openDropdown();
      // ACT
      host.dispatchEvent(new MouseEvent("mouseleave"));
      // ASSERT
      expect(closes).toEqual([]);
    });

    it("tells the row gesture the pointer is gone", () => {
      // ARRANGE
      const pointerAt = vi.spyOn(rowHover, "pointerAt");
      // ACT
      host.dispatchEvent(new MouseEvent("mouseleave"));
      // ASSERT
      expect(pointerAt).toHaveBeenCalledWith(null);
    });
  });

  it("stops listening once disposed", () => {
    // ARRANGE
    openDropdown();
    transients.dispose();
    // ACT
    leave();
    dismissals.notify(DISMISS_PAGE_PRESS);
    host.dispatchEvent(new MouseEvent("mouseleave"));
    // ASSERT
    expect([closes, suspended()]).toEqual([[], false]);
  });
});

describe("the rail's hover highlights in the stylesheet", () => {
  const railHoverSelectors = rulesOf(stylesheet)
    .flatMap((rule) => rule.selectors)
    .filter((selector) => selector.startsWith("#ws-sidebar") && selector.includes(":hover"));

  it("has hover highlights in the rail to gate", () => {
    expect(railHoverSelectors.length).toBeGreaterThan(0);
  });

  it("draws every one only while the rail is not suspended", () => {
    expect(
      railHoverSelectors.filter((selector) => !selector.startsWith(`#ws-sidebar:not(.${HOVER_SUSPENDED_CLASS}) `)),
    ).toEqual([]);
  });
});
