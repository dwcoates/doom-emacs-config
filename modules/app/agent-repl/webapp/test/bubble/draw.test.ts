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
  BUBBLE_EXPAND_ONLY_CLASS,
  BUBBLE_MORE_ATTRIBUTE,
  BUBBLE_MORE_ELLIPSIS,
  BUBBLE_MORE_FADE,
  BUBBLE_ROLE_ATTRIBUTE,
  BUBBLE_STRIP_CLASS,
  BUBBLE_VARIANTS,
  BUBBLE_UNCAPPED,
  BUBBLE_VARIANT_ATTRIBUTE,
  ELLIPSIS_CAP_LINES,
  SAYS_ATTRIBUTE,
  drawBubble,
  isCapped,
  type BubbleCapLines,
  type BubbleSpec,
  type PromptVariant,
  type ResponseVariant,
} from "../../src/bubble/draw.js";
import { BUBBLE_BODY_TAG } from "../../src/bubble/body.js";
import { BUBBLE_BOX_CLASS, BUBBLE_SCROLL_CLASS } from "../../src/feed/bubble-scroll.js";
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

describe("drawBubble: the more signal", () => {
  /** The smallest spec that chooses the one-line ellipsis. */
  const ellipsis = (): BubbleSpec =>
    spec("held", { capLines: ELLIPSIS_CAP_LINES, more: BUBBLE_MORE_ELLIPSIS });

  it("stamps the shared fade on a capped bubble that chooses none", () => {
    // Arrange / Act
    const { bubble } = drawBubble(spec("user"));
    // Assert
    expect(bubble.getAttribute(BUBBLE_MORE_ATTRIBUTE)).toBe(BUBBLE_MORE_FADE);
  });

  it("stamps the ellipsis on a bubble that chooses it", () => {
    // Arrange / Act
    const { bubble } = drawBubble(ellipsis());
    // Assert
    expect(bubble.getAttribute(BUBBLE_MORE_ATTRIBUTE)).toBe(BUBBLE_MORE_ELLIPSIS);
  });

  it("draws the ellipsis at the one-line cap", () => {
    // Arrange / Act
    const { bubble } = drawBubble(ellipsis());
    // Assert
    expect(bubble.getAttribute(BUBBLE_CAP_ATTRIBUTE)).toBe("1");
  });

  it("states no more signal on an uncapped bubble", () => {
    // Arrange / Act
    const { bubble } = drawBubble(spec("response", { capLines: BUBBLE_UNCAPPED }));
    // Assert
    expect(bubble.hasAttribute(BUBBLE_MORE_ATTRIBUTE)).toBe(false);
  });

  it("cannot be given the ellipsis at the shared feed cap, which no clamp can state", () => {
    // Arrange / Act — the spec types refuse it; `npm run typecheck` holds the line.
    // @ts-expect-error the ellipsis is drawn at the one-line cap only
    const refused: BubbleSpec = { role: "response", variant: "thinking", content: [], capLines: "feed", more: BUBBLE_MORE_ELLIPSIS };
    // Assert
    expect(refused.capLines).toBe("feed");
  });

  it("switches the signal in place on a redraw, keeping the scroll box", () => {
    // Arrange — a thinking bubble arriving under the fade, then landing.
    const first = drawBubble(spec("thinking"));
    const box = first.body.parentElement;
    // Act
    const again = drawBubble(
      spec("thinking", { capLines: ELLIPSIS_CAP_LINES, more: BUBBLE_MORE_ELLIPSIS }),
      first.bubble,
    );
    // Assert
    expect([again.bubble === first.bubble, again.body.parentElement === box, again.bubble.getAttribute(BUBBLE_MORE_ATTRIBUTE)]).toEqual([
      true,
      true,
      BUBBLE_MORE_ELLIPSIS,
    ]);
  });
});

describe("drawBubble: the uncapped mode", () => {
  /** The smallest uncapped response spec. */
  const uncapped = (): BubbleSpec => spec("response", { capLines: BUBBLE_UNCAPPED });

  it("stamps the uncapped mode for the stylesheet to read", () => {
    // Arrange / Act
    const { bubble } = drawBubble(uncapped());
    // Assert
    expect(bubble.getAttribute(BUBBLE_CAP_ATTRIBUTE)).toBe(BUBBLE_UNCAPPED);
  });

  it("hangs the body in a box that is not a scroll box", () => {
    // Arrange / Act
    const { body } = drawBubble(uncapped());
    // Assert
    expect(body.parentElement?.className).toBe(BUBBLE_BOX_CLASS);
  });

  it("hangs a capped bubble's body in the scroll box", () => {
    // Arrange / Act
    const { body } = drawBubble(spec("thinking", { capLines: 1 }));
    // Assert
    expect(body.parentElement?.classList.contains(BUBBLE_SCROLL_CLASS)).toBe(true);
  });

  it("updates an uncapped bubble in place on an uncapped redraw", () => {
    // Arrange
    const first = drawBubble(uncapped()).bubble;
    // Act
    const again = drawBubble(uncapped(), first).bubble;
    // Assert
    expect(again).toBe(first);
  });

  it("builds a fresh bubble when a capped row is redrawn uncapped", () => {
    // Arrange
    const first = drawBubble(spec("response")).bubble;
    // Act
    const again = drawBubble(uncapped(), first).bubble;
    // Assert
    expect(again).not.toBe(first);
  });

  it("builds a fresh bubble when an uncapped row is redrawn capped", () => {
    // Arrange
    const first = drawBubble(uncapped()).bubble;
    // Act
    const again = drawBubble(spec("response"), first).bubble;
    // Assert
    expect(again).not.toBe(first);
  });

  it("cannot be given expand-only chrome, having no open state to show it in", () => {
    // Arrange / Act — the spec types refuse it; `npm run typecheck` holds the line.
    // @ts-expect-error an uncapped bubble carries no expand-only chrome
    const refused: BubbleSpec = {
      role: "response",
      variant: "response",
      content: [],
      capLines: BUBBLE_UNCAPPED,
      expandOnly: [el("details")],
    };
    // Assert
    expect(refused.capLines).toBe(BUBBLE_UNCAPPED);
  });

  it("answers the capped values as capped", () => {
    // Arrange / Act / Assert
    expect(BUBBLE_CAP_LINES.every((cap) => isCapped(cap))).toBe(true);
  });

  it("answers the uncapped mode as not capped", () => {
    // Arrange / Act / Assert
    expect(isCapped(BUBBLE_UNCAPPED)).toBe(false);
  });
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
      `${BUBBLE_SCROLL_CLASS} ${BUBBLE_BOX_CLASS}`,
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

  it("puts the expand-only chrome after the scroll box, before the footer", () => {
    // Arrange
    const details = el("details");
    const footer = el("response-cut-short-marker");
    // Act
    const { bubble } = drawBubble(spec("held", { expandOnly: [details], footer: [footer] }));
    // Assert
    expect([...bubble.children].map((c) => c.classList[0])).toEqual([
      BUBBLE_SCROLL_CLASS,
      "details",
      "response-cut-short-marker",
    ]);
  });

  it("marks every expand-only element as expand-only", () => {
    // Arrange
    const expandOnly = [el("details"), el("actions")];
    // Act
    drawBubble(spec("held", { expandOnly }));
    // Assert
    expect(expandOnly.map((node) => node.classList.contains(BUBBLE_EXPAND_ONLY_CLASS))).toEqual([true, true]);
  });

  it("leaves the always-shown footer unmarked", () => {
    // Arrange
    const footer = el("response-cut-short-marker");
    // Act
    drawBubble(spec("response", { footer: [footer] }));
    // Assert
    expect(footer.classList.contains(BUBBLE_EXPAND_ONLY_CLASS)).toBe(false);
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
    first.classList.add("entry-selected");
    // Act
    drawBubble(spec("response", { hooks: ["final-response"] }), first);
    // Assert
    expect(first.classList.contains("entry-selected")).toBe(true);
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
    const first = drawBubble(spec("held", { footer: [el("queued-actions", "Send now")] })).bubble;
    const actions = first.querySelector(".queued-actions");
    // Act
    drawBubble(spec("held", { footer: [el("queued-actions", "Send now")] }), first);
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

  it("takes the expand-only mark off a kept element that moved to the footer", () => {
    // Arrange
    const said = (): HTMLElement => {
      const node = el("marker", "same");
      node.setAttribute(SAYS_ATTRIBUTE, "same");
      return node;
    };
    const first = drawBubble(spec("held", { expandOnly: [said()] })).bubble;
    const kept = first.querySelector(".marker");
    // Act
    drawBubble(spec("held", { footer: [said()] }), first);
    // Assert
    expect([first.querySelector(".marker") === kept, kept?.classList.contains(BUBBLE_EXPAND_ONLY_CLASS)]).toEqual([
      true,
      false,
    ]);
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
