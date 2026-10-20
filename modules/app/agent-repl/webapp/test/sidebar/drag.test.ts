// @vitest-environment jsdom
// The drag's click guard. See src/sidebar/drag.ts.
import { afterEach, describe, expect, it, vi } from "vitest";
import { listAt, swallowTheNextClick } from "../../src/sidebar/drag.js";
import { settleOwedDragClick } from "./harness.js";
import { boxRect } from "../dom-rect.js";

/** A second window, in an iframe of this page. */
function otherWindow(): Window {
  const frame = document.createElement("iframe");
  document.body.appendChild(frame);
  const win = frame.contentWindow;
  if (win === null) throw new Error("the iframe has no window");
  return win;
}

/** Whether a click on WIN's body reaches a listener there. */
function clickReaches(win: Window): boolean {
  const reached = vi.fn();
  win.document.body.addEventListener("click", reached);
  const { MouseEvent: WindowMouseEvent } = win as Window & typeof globalThis;
  win.document.body.dispatchEvent(new WindowMouseEvent("click", { bubbles: true }));
  win.document.body.removeEventListener("click", reached);
  return reached.mock.calls.length > 0;
}

afterEach(() => {
  settleOwedDragClick();
  document.body.replaceChildren();
});

describe("the click guard", () => {
  it("swallows the one click a drag leaves behind, then lets clicks through", () => {
    // Act
    swallowTheNextClick(window);

    // Assert
    expect([clickReaches(window), clickReaches(window)]).toEqual([false, true]);
  });

  it("is taken down by the next press, so a release with no click cannot swallow a later one", () => {
    // Arrange
    swallowTheNextClick(window);

    // Act
    window.dispatchEvent(new PointerEvent("pointerdown"));

    // Assert
    expect(clickReaches(window)).toBe(true);
  });

  it("guards only the window the drag happened in", () => {
    // Arrange
    const other = otherWindow();

    // Act
    swallowTheNextClick(window);

    // Assert: the other window's click goes through; this one's is swallowed.
    expect([clickReaches(other), clickReaches(window)]).toEqual([true, false]);
  });

  it("owes one click for one drag, whatever drags came before", () => {
    // Arrange: an earlier drag's guard, taken down by the next drag's press.
    swallowTheNextClick(window);
    window.dispatchEvent(new PointerEvent("pointerdown"));

    // Act
    swallowTheNextClick(window);

    // Assert
    expect([clickReaches(window), clickReaches(window)]).toEqual([false, true]);
  });
});


/** A list element whose box spans TOP to BOTTOM. */
function spanning(top: number, bottom: number): HTMLElement {
  const el = document.createElement("div");
  Object.defineProperty(el, "getBoundingClientRect", {
    value: () => boxRect({ top, height: bottom - top, width: 100 }),
  });
  return el;
}

describe("listAt", () => {
  const own = (list: HTMLElement): HTMLElement => list;

  it("lands in the innermost of the zones the pointer is over", () => {
    // Arrange -- a section's list (0 to 100) holding a subsection (40 to 80).
    const outer = spanning(0, 100);
    const inner = spanning(40, 80);

    // Act / Assert
    expect([listAt([outer, inner], 50, own), listAt([outer, inner], 20, own)]).toEqual([inner, outer]);
  });

  it("lands in a list through the zone it claims", () => {
    // Arrange -- the subsection's rows (60 to 80) claim the whole subsection (40 to 80).
    const outer = spanning(0, 100);
    const rows = spanning(60, 80);
    const subsection = spanning(40, 80);

    // Act
    const got = listAt([outer, rows], 45, (list) => (list === rows ? subsection : list));

    // Assert
    expect(got).toBe(rows);
  });

  it("lands in the nearest zone when the pointer is over none, the earlier on a tie", () => {
    // Arrange
    const first = spanning(0, 50);
    const second = spanning(0, 50);
    const far = spanning(100, 150);

    // Act / Assert
    expect([listAt([first, second, far], 70, own), listAt([first, far], 90, own)]).toEqual([first, far]);
  });

  it("refuses a drag with no list to land in", () => {
    // Act / Assert
    expect(() => listAt([], 10, own)).toThrow(/invariant/);
  });
});
