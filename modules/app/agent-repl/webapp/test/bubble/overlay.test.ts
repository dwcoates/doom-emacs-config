// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import {
  BUBBLE_OVERLAY_ATTRIBUTE,
  BUBBLE_OVERLAY_HOST_CLASS,
  BUBBLE_OVERLAY_PANEL_CLASS,
  closeStandingBubbleOverlay,
  mountBubbleOverlay,
  type BubbleOverlayHandle,
} from "../../src/bubble/overlay.js";

let feed: HTMLElement;
let overlay: BubbleOverlayHandle;

const bubble = (role: string, withBody = true): HTMLElement => {
  const el = document.createElement("div");
  el.className = "bubble";
  el.setAttribute("data-role", role);
  el.innerHTML = withBody ? `<div class="bubble-body"><table><tr><td>cell</td></tr></table></div>` : "<span>x</span>";
  feed.append(el);
  return el;
};
const host = (): HTMLElement => document.querySelector<HTMLElement>(`.${BUBBLE_OVERLAY_HOST_CLASS}`) as HTMLElement;
const dbl = (el: Element): void => void el.dispatchEvent(new MouseEvent("dblclick", { bubbles: true }));
const esc = (): void => void document.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape", bubbles: true }));

beforeEach(() => {
  document.body.replaceChildren();
  document.documentElement.removeAttribute(BUBBLE_OVERLAY_ATTRIBUTE);
  feed = document.createElement("div");
  document.body.append(feed);
  overlay = mountBubbleOverlay(document, feed);
});
afterEach(() => overlay.dispose());

describe("mountBubbleOverlay", () => {
  it("is hidden until a bubble is double clicked", () => {
    expect([host().hidden, document.documentElement.hasAttribute(BUBBLE_OVERLAY_ATTRIBUTE)]).toEqual([true, false]);
  });

  it.each(["response", "prompt"])("expands a %s bubble's body into the panel", (role) => {
    const b = bubble(role);
    dbl(b.querySelector("td") as Element);
    const panel = host().querySelector(`.${BUBBLE_OVERLAY_PANEL_CLASS}`);
    expect([host().hidden, panel?.getAttribute("data-role"), panel?.querySelector("td")?.textContent]).toEqual([
      false,
      role,
      "cell",
    ]);
  });

  it("marks the page's root with the bubble's role while open", () => {
    dbl(bubble("response"));
    expect(document.documentElement.getAttribute(BUBBLE_OVERLAY_ATTRIBUTE)).toBe("response");
  });

  it("draws a copy and leaves the bubble in the feed", () => {
    const b = bubble("response");
    dbl(b);
    expect(b.querySelector(".bubble-body")).not.toBeNull();
  });

  it("ignores a double click outside any bubble", () => {
    dbl(feed);
    expect(host().hidden).toBe(true);
  });

  it("ignores a double click on a bubble with no body", () => {
    dbl(bubble("response", false));
    expect(host().hidden).toBe(true);
  });

  it("closes on Escape", () => {
    dbl(bubble("response"));
    esc();
    expect([host().hidden, document.documentElement.hasAttribute(BUBBLE_OVERLAY_ATTRIBUTE)]).toEqual([true, false]);
  });

  it("closes on a click on the scrim but not on the panel", () => {
    dbl(bubble("response"));
    (host().querySelector(`.${BUBBLE_OVERLAY_PANEL_CLASS}`) as HTMLElement).click();
    const stayed = host().hidden;
    host().click();
    expect([stayed, host().hidden]).toEqual([false, true]);
  });

  it("closes on its close control", () => {
    dbl(bubble("response"));
    (host().querySelector("[data-bubble-overlay-close]") as HTMLElement).click();
    expect(host().hidden).toBe(true);
  });

  it("empties the panel when closed", () => {
    dbl(bubble("response"));
    esc();
    expect(host().childElementCount).toBe(0);
  });

  it("Escape does nothing while none stands", () => {
    esc();
    expect(host().hidden).toBe(true);
  });
});

describe("closeStandingBubbleOverlay", () => {
  it("closes a standing overlay and says so", () => {
    dbl(bubble("response"));
    expect([closeStandingBubbleOverlay(document), host().hidden]).toEqual([true, true]);
  });

  it("answers false when none stands", () => {
    expect(closeStandingBubbleOverlay(document)).toBe(false);
  });
});
