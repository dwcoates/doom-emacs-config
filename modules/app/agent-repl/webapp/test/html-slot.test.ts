// @vitest-environment jsdom
/**
 * html-slot — the guard that keeps a chrome repaint from eating a click.
 *
 * The whole point is node SURVIVAL: a button the user is mid-press on must
 * still be the same node when the mouseup lands, or the browser fires no
 * click. So these assert on element identity across paints, not on markup.
 * One edge per test.
 */
import { describe, expect, it } from "vitest";

import { HtmlSlot } from "../src/html-slot.js";

const CARD = '<div class="card"><button id="go">Go</button></div>';

/** A detached slot element, so each test owns its own DOM. */
function slotEl(): HTMLElement {
  return document.createElement("div");
}

describe("HtmlSlot", () => {
  it("writes the markup on the first paint", () => {
    // Arrange
    const el = slotEl();
    const slot = new HtmlSlot(el);
    // Act
    const wrote = slot.paint(CARD);
    // Assert
    expect(wrote).toBe(true);
    expect(el.querySelector("#go")).not.toBeNull();
  });

  it("does NOT rewrite the slot when the markup is unchanged", () => {
    // Arrange — the defect this exists for: an unrelated frame repainting a
    // standing card destroys the pressed button, and the click never fires.
    const el = slotEl();
    const slot = new HtmlSlot(el);
    slot.paint(CARD);
    const button = el.querySelector("#go");
    // Act
    const wrote = slot.paint(CARD);
    // Assert — the very same node, not an equal one.
    expect(wrote).toBe(false);
    expect(el.querySelector("#go")).toBe(button);
  });

  it("rewrites the slot when the markup changed", () => {
    // Arrange
    const el = slotEl();
    const slot = new HtmlSlot(el);
    slot.paint(CARD);
    // Act
    const wrote = slot.paint('<div class="card"><button id="stop">Stop</button></div>');
    // Assert — a guard that skipped a real change would freeze the chrome.
    expect(wrote).toBe(true);
    expect(el.querySelector("#go")).toBeNull();
    expect(el.querySelector("#stop")).not.toBeNull();
  });

  it("paints again after the same markup returns, so a cleared slot refills", () => {
    // Arrange — up, down, up: the third paint must land even though its
    // markup equals the first.
    const el = slotEl();
    const slot = new HtmlSlot(el);
    slot.paint(CARD);
    slot.paint("");
    // Act
    const wrote = slot.paint(CARD);
    // Assert
    expect(wrote).toBe(true);
    expect(el.querySelector("#go")).not.toBeNull();
  });
});
