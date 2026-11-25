/**
 * THE DOCKS ARE BUILT ON installCardDock (src/feed/card-dock.ts): each dock's
 * source docks through the shared helper and moves no card itself.
 */
import { describe, expect, it } from "vitest";

// The docks built on the helper, read as text so the suite can hold them to it.
import gateDockSource from "../../src/feed/gate-dock.ts?raw";
import askTraySource from "../../src/feed/ask-tray.ts?raw";

describe("the docks built on installCardDock", () => {
  it.each([
    ["gate-dock.ts", gateDockSource],
    ["ask-tray.ts", askTraySource],
  ])("%s docks through the shared helper and moves no card itself", (_name, source) => {
    // Arrange / Act
    const usesHelper = source.includes("installCardDock(");
    const movesByHand = /\.(replaceChildren|insertBefore)\(/.test(source) || /pageMutationObserver/.test(source);

    // Assert
    expect([usesHelper, movesByHand]).toEqual([true, false]);
  });
});
