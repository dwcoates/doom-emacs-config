// @vitest-environment jsdom
// The drag's click guard. See src/sidebar/drag.ts.
import { afterEach, describe, expect, it, vi } from "vitest";
import { swallowTheNextClick } from "../../src/sidebar/drag.js";
import { settleOwedDragClick } from "./harness.js";

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

