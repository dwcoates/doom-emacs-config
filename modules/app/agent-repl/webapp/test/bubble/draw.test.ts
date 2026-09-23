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
  SAYS_ATTRIBUTE,
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

describe("drawBubble: a redraw given its previous draw updates it in place", () => {
  it("returns the previous bubble itself", () => {
    // Arrange
    const first = drawBubble(spec("user")).bubble;
    // Act
    const again = drawBubble(spec("user"), first).bubble;
    // Assert
    expect(again).toBe(first);
  });

  it("keeps the scroll box and the body", () => {
    // Arrange
    const first = drawBubble(spec("response"));
    // Act
    const again = drawBubble(spec("response", { state: "success" }), first.bubble);
    // Assert
    expect(again.body).toBe(first.body);
  });

  it("draws a fresh bubble over a previous draw of the other role", () => {
    // Arrange
    const prompt = drawBubble(spec("user")).bubble;
    // Act
    const response = drawBubble(spec("response"), prompt).bubble;
    // Assert
    expect(response).not.toBe(prompt);
  });

  it("draws a fresh bubble over an element this module did not draw", () => {
    // Arrange
    const stranger = document.createElement("div");
    stranger.className = "bubble";
    stranger.setAttribute(BUBBLE_ROLE_ATTRIBUTE, "prompt");
    // Act
    const drawn = drawBubble(spec("user"), stranger).bubble;
    // Assert
    expect(drawn).not.toBe(stranger);
  });

  it("takes back a hook class the new spec no longer carries", () => {
    // Arrange
    const first = drawBubble(spec("response", { hooks: ["final-response"] })).bubble;
    // Act
    drawBubble(spec("response"), first);
    // Assert
    expect(first.classList.contains("final-response")).toBe(false);
  });

  it("leaves a class no spec gave it, the controller's own", () => {
    // Arrange
    const first = drawBubble(spec("response", { hooks: ["final-response"] })).bubble;
    first.classList.add("response-selected");
    // Act
    drawBubble(spec("response", { hooks: ["final-response"] }), first);
    // Assert
    expect(first.classList.contains("response-selected")).toBe(true);
  });

  it("drops a state the new spec does not carry", () => {
    // Arrange
    const first = drawBubble(spec("response", { state: "update" })).bubble;
    // Act
    drawBubble(spec("response"), first);
    // Assert
    expect(first.hasAttribute("data-state")).toBe(false);
  });

  it("keeps the wave's phase when only the working flag moved", () => {
    // Arrange
    const first = drawBubble(spec("user")).bubble;
    const phase = first.style.animationDelay;
    // Act
    drawBubble({ ...spec("user"), working: true } as BubbleSpec, first);
    // Assert
    expect([first.style.animationDelay, first.getAttribute(PROMPT_WAVE_ATTRIBUTE)]).toEqual([
      phase,
      PROMPT_WAVE_WORKING,
    ]);
  });

  /** A chrome element of CLASSNAME that states SAYS. */
  function said(className: string, says: string): HTMLElement {
    const node = el(className, says);
    node.setAttribute(SAYS_ATTRIBUTE, says);
    return node;
  }

  it("keeps a strip element that states the same thing", () => {
    // Arrange
    const first = drawBubble(spec("response", { strip: [said("notice", "Interrupted")] })).bubble;
    const notice = first.querySelector(".notice");
    // Act
    drawBubble(spec("response", { strip: [said("notice", "Interrupted")] }), first);
    // Assert
    expect(first.querySelector(".notice")).toBe(notice);
  });

  it("never keeps an element that states nothing, however equal it looks", () => {
    // Arrange — chrome like a held prompt's actions, whose listeners belong to their push.
    const first = drawBubble(spec("held", { footer: [el("queued-actions", "Release")] })).bubble;
    const actions = first.querySelector(".queued-actions");
    // Act
    drawBubble(spec("held", { footer: [el("queued-actions", "Release")] }), first);
    // Assert
    expect(first.querySelector(".queued-actions")).not.toBe(actions);
  });

  it("replaces a strip element whose words changed", () => {
    // Arrange
    const first = drawBubble(spec("agent", { strip: [el("address", "→ Explore")] })).bubble;
    // Act
    drawBubble(spec("agent", { strip: [el("address", "→ Plan")] }), first);
    // Assert
    expect(first.querySelector(".address")?.textContent).toBe("→ Plan");
  });

  it("keeps a corner whose stated words did not change, whatever its clock shows", () => {
    // Arrange
    const corner = el("usage-corner", "1.2k");
    corner.setAttribute(SAYS_ATTRIBUTE, "1.2k|0");
    const first = drawBubble(spec("response", { corner })).bubble;
    const again = el("usage-corner", "1.2k (ticked)");
    again.setAttribute(SAYS_ATTRIBUTE, "1.2k|0");
    // Act
    drawBubble(spec("response", { corner: again }), first);
    // Assert
    expect(first.querySelector(".usage-corner")).toBe(corner);
  });

  it("drops a footer element the new spec does not draw", () => {
    // Arrange
    const first = drawBubble(spec("response", { footer: [el("response-cut-short-marker")] })).bubble;
    // Act
    drawBubble(spec("response"), first);
    // Assert
    expect(first.querySelector(".response-cut-short-marker")).toBeNull();
  });

  it("keeps the strip above the box and the footer below it", () => {
    // Arrange
    const first = drawBubble(spec("agent", { strip: [el("address", "a")] })).bubble;
    // Act
    drawBubble(spec("agent", { strip: [el("address", "b")], footer: [el("delivery")] }), first);
    // Assert
    expect([...first.children].map((c) => c.classList[0])).toEqual(["address", BUBBLE_SCROLL_CLASS, "delivery"]);
  });
});
