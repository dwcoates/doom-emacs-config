// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { FRAME_TONE_ATTRIBUTE, applyFrameTone } from "../../src/feed/bubble-frame.js";

/** A head stating TONE for its frame, or none. */
function head(tone: string | null): HTMLElement {
  const el = document.createElement("div");
  if (tone !== null) el.setAttribute(FRAME_TONE_ATTRIBUTE, tone);
  return el;
}

describe("applyFrameTone", () => {
  it("repeats the head's frame tone on the bubble", () => {
    const bubble = document.createElement("div");
    applyFrameTone(bubble, head("green"));
    expect(bubble.getAttribute(FRAME_TONE_ATTRIBUTE)).toBe("green");
  });

  it("clears the bubble's frame tone once the head states none", () => {
    const bubble = document.createElement("div");
    applyFrameTone(bubble, head("green"));
    applyFrameTone(bubble, head(null));
    expect(bubble.hasAttribute(FRAME_TONE_ATTRIBUTE)).toBe(false);
  });
});
