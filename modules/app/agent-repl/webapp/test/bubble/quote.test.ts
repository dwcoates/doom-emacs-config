// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { BUBBLE_QUOTE_CLASS, quoteSlot } from "../../src/bubble/quote.js";
import { BUBBLE_EXPANDED_ONLY_CLASS } from "../../src/bubble/expanded-only.js";
import { MARKDOWN_SLOT_ATTRIBUTE, createBubbleBody, paintBody } from "../../src/bubble/body.js";

describe("quoteSlot", () => {
  it("wears the drawing kind's class, the quote class, and the expanded-only mark", () => {
    // Act
    const slot = quoteSlot("queued-quote", "the quote");
    // Assert
    expect([...slot.classList]).toEqual(["queued-quote", BUBBLE_QUOTE_CLASS, BUBBLE_EXPANDED_ONLY_CLASS]);
  });

  it("is a markdown slot, so the bubble's body pipeline paints it", () => {
    // Act
    const slot = quoteSlot("prompt-block", "the quote");
    // Assert
    expect(slot.hasAttribute(MARKDOWN_SLOT_ATTRIBUTE)).toBe(true);
  });

  it("draws a fenced quote as a code block", () => {
    // Arrange
    const slot = quoteSlot("prompt-block", "⟢ Replying:\n\n````\nconst x = `y`;\n````\n\n⟢ My message:\n");
    // Act
    paintBody(createBubbleBody(), [slot]);
    // Assert
    expect(slot.querySelector("pre code")?.textContent).toBe("const x = `y`;");
  });
});
