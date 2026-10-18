// @vitest-environment jsdom
// The one way a test dispatches a page transition. See test/page-transition.ts.
import { describe, expect, it } from "vitest";
import { codeUnder } from "./files-under.js";
import { dispatchPageTransition } from "./page-transition.js";

describe("dispatchPageTransition", () => {
  it.each([
    ["pagehide", false],
    ["pagehide", true],
    ["pageshow", false],
    ["pageshow", true],
  ] as const)("dispatches a %s with persisted %s on the document's own window", (type, persisted) => {
    // Arrange
    const win = document.defaultView!;
    const seen: Array<[string, boolean]> = [];
    const listener = (event: Event): void => {
      seen.push([event.type, (event as PageTransitionEvent).persisted]);
    };
    win.addEventListener(type, listener);
    // Act
    dispatchPageTransition(type, persisted);
    win.removeEventListener(type, listener);
    // Assert
    expect(seen).toEqual([[type, persisted]]);
  });
});

describe("the call sites", () => {
  it("dispatch a page transition only through dispatchPageTransition", () => {
    // Arrange / Act
    const rolled = [...codeUnder(process.cwd() + "/test")]
      .filter(([file, code]) => file !== "page-transition.ts" && /new\s+(?:[\w$]+\.)*PageTransitionEvent\(/.test(code))
      .map(([file]) => file);
    // Assert
    expect(rolled).toEqual([]);
  });
});
