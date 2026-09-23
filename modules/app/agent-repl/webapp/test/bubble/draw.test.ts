// @vitest-environment jsdom
/**
 * drawBubble — the one bubble every blue and purple kind is built through.
 *
 * Each role, variant and cap is drawn from a spec and read back off the element
 * the stylesheet keys on; the nesting (strip above the box, corner inside it,
 * footer after it) is the structure every kind now shares.
 */
import { describe, expect, it } from "vitest";
import {
  BUBBLE_CAP_ATTRIBUTE,
  BUBBLE_CAP_LINES,
  BUBBLE_ROLE_ATTRIBUTE,
  BUBBLE_STRIP_CLASS,
  BUBBLE_VARIANTS,
  BUBBLE_VARIANT_ATTRIBUTE,
  drawBubble,
  type BubbleCapLines,
  type BubbleSpec,
  type PromptVariant,
  type ResponseVariant,
} from "../../src/bubble/draw.js";
import { BUBBLE_BODY_TAG } from "../../src/bubble/body.js";
import { BUBBLE_SCROLL_CLASS } from "../../src/feed/bubble-scroll.js";
import { PROMPT_WAVE_ATTRIBUTE, PROMPT_WAVE_WORKING } from "../../src/breathing.js";

/** A plain element of CLASSNAME holding TEXT. */
function el(className: string, text = ""): HTMLElement {
  const node = document.createElement("span");
  node.className = className;
  node.textContent = text;
  return node;
}

/** The smallest spec of VARIANT, with OVERRIDES. */
function spec(variant: PromptVariant | ResponseVariant, overrides: Partial<BubbleSpec> = {}): BubbleSpec {
  const role = BUBBLE_VARIANTS[variant];
  const base =
    role === "prompt"
      ? { role, variant: variant as PromptVariant, working: false, content: [], capLines: "feed" as const }
      : { role, variant: variant as ResponseVariant, content: [], capLines: "feed" as const };
  return { ...base, ...overrides } as BubbleSpec;
}

describe("drawBubble: role and variant", () => {
  const VARIANTS = Object.keys(BUBBLE_VARIANTS) as Array<keyof typeof BUBBLE_VARIANTS>;

  it.each(VARIANTS)("stamps the %s variant's role on the bubble", (variant) => {
    // Arrange / Act
    const { bubble } = drawBubble(spec(variant));
    // Assert
    expect(bubble.getAttribute(BUBBLE_ROLE_ATTRIBUTE)).toBe(BUBBLE_VARIANTS[variant]);
  });

  it.each(VARIANTS)("stamps the %s variant itself on the bubble", (variant) => {
    // Arrange / Act
    const { bubble } = drawBubble(spec(variant));
    // Assert
    expect(bubble.getAttribute(BUBBLE_VARIANT_ATTRIBUTE)).toBe(variant);
  });

  it("wears the bubble class and the kind's hook classes", () => {
    // Arrange / Act
    const { bubble } = drawBubble(spec("response", { hooks: ["assistant", "final-response"] }));
    // Assert
    expect([...bubble.classList]).toEqual(expect.arrayContaining(["bubble", "assistant", "final-response"]));
  });

  it("carries the kind's state arm", () => {
    // Arrange / Act
    const { bubble } = drawBubble(spec("response", { state: "success" }));
    // Assert
    expect(bubble.getAttribute("data-state")).toBe("success");
  });

  it("carries no state when the kind has none", () => {
    // Arrange / Act
    const { bubble } = drawBubble(spec("peer"));
    // Assert
    expect(bubble.hasAttribute("data-state")).toBe(false);
  });
});

describe("drawBubble: the collapsed line limit", () => {
  it.each(BUBBLE_CAP_LINES.map((cap) => [String(cap), cap] as [string, BubbleCapLines]))(
    "stamps the %s cap for the stylesheet to read",
    (label, cap) => {
      // Arrange / Act
      const { bubble } = drawBubble(spec("response", { capLines: cap }));
      // Assert
      expect(bubble.getAttribute(BUBBLE_CAP_ATTRIBUTE)).toBe(label);
    },
  );
});

describe("drawBubble: the working wave", () => {
  it("waves a working prompt", () => {
    // Arrange / Act
    const { bubble } = drawBubble({ ...spec("user"), working: true } as BubbleSpec);
    // Assert
    expect(bubble.getAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(PROMPT_WAVE_WORKING);
  });

  it("rests a prompt whose turn is not working", () => {
    // Arrange / Act
    const { bubble } = drawBubble(spec("user"));
    // Assert
    expect(bubble.hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(false);
  });

  it("stamps the wave's phase on every prompt, waving or not", () => {
    // Arrange / Act
    const { bubble } = drawBubble(spec("held"));
    // Assert
    expect(bubble.style.animationDelay).toMatch(/^-\d+ms$/);
  });

  it("never stamps a wave or its phase on a response", () => {
    // Arrange / Act
    const { bubble } = drawBubble(spec("response"));
    // Assert
    expect(bubble.hasAttribute(PROMPT_WAVE_ATTRIBUTE) || bubble.hasAttribute("style")).toBe(false);
  });
});

describe("drawBubble: the structure every kind shares", () => {
  it("hangs the body in the one scroll box", () => {
    // Arrange / Act
    const { bubble, body } = drawBubble(spec("user"));
    // Assert
    expect(body.parentElement).toBe(bubble.querySelector(`:scope > .${BUBBLE_SCROLL_CLASS}`));
  });

  it("builds the body as the one bubble-body element", () => {
    // Arrange / Act
    const { body } = drawBubble(spec("agentic"));
    // Assert
    expect(body.tagName.toLowerCase()).toBe(BUBBLE_BODY_TAG);
  });

  it("puts the content in the body, in order", () => {
    // Arrange
    const content = [el("first"), el("second")];
    // Act
    const { body } = drawBubble(spec("user", { content }));
    // Assert
    expect([...body.children]).toEqual(content);
  });

  it("puts the header strip above the scroll box, in order", () => {
    // Arrange
    const strip = [el("address"), el("delivery")];
    // Act
    const { bubble } = drawBubble(spec("agent", { strip }));
    // Assert
    expect([...bubble.children].map((c) => c.className)).toEqual([
      `address ${BUBBLE_STRIP_CLASS}`,
      `delivery ${BUBBLE_STRIP_CLASS}`,
      BUBBLE_SCROLL_CLASS,
    ]);
  });

  it("floats the corner inside the scroll box, before the body", () => {
    // Arrange
    const corner = el("usage-corner");
    // Act
    const { body } = drawBubble(spec("response", { corner }));
    // Assert
    expect(corner.nextElementSibling).toBe(body);
  });

  it("puts the footer after the scroll box", () => {
    // Arrange
    const footer = el("response-cut-short-marker");
    // Act
    const { bubble } = drawBubble(spec("response", { footer: [footer] }));
    // Assert
    expect(bubble.lastElementChild).toBe(footer);
  });

  it("draws no strip element when the kind has none", () => {
    // Arrange / Act
    const { bubble } = drawBubble(spec("user"));
    // Assert
    expect(bubble.querySelector(`.${BUBBLE_STRIP_CLASS}`)).toBeNull();
  });
});
